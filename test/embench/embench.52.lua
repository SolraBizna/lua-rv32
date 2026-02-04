#!/usr/bin/env lua5.2

if #arg ~= 1 then
    print("usage: embench.lua")
    os.exit(1)
end

local inelfpath = arg[1]

local uspath
if arg[0] then
    uspath = arg[0]:gsub("/[^/]*$","/")
end
if not uspath or uspath:sub(-1,-1) ~= "/" then
    uspath = "./" -- :S
end

rv32trace = not not os.getenv("RV32TRACE")
local rv32 = dofile(uspath.."../../lib/rv32.lua")

if not string.unpack then
    function string.unpack(source, input, pos)
        pos = pos or 1
        assert(source:sub(1,1) == "<")
        local t = {}
        for n=2,#source,2 do
            assert(source:sub(n,n) == "I")
            local count = source:sub(n+1,n+1)
            assert(count == "1" or count == "2" or count == "4")
            count = tonumber(count)
            local bits = 0
            local mult = 1
            for m=1,count do
                bits = bits + input:byte(pos) * mult
                mult = mult * 256
                pos = pos + 1
            end
            t[#t+1] = bits
        end
        return table.unpack(t)
    end
end

local elf = assert(io.open(inelfpath, "rb"))

local ram = {}

local header = assert(elf:read(52))
assert(#header == 52)
assert(header:sub(1,4) == "\x7FELF", "Not an ELF")
assert(header:sub(5,5) == "\x01", "Not a 32-bit ELF")
assert(header:sub(6,6) == "\x01", "Not a two's complement little-endian ELF")
assert(header:sub(7,7) == "\x01", "Not a version 1 ELF")
-- ignoring 8 and 9 because the ABI is often set wrong
-- ignoring 10-16 because they're reserved and should be ignored
assert(header:sub(17,18) == "\x02\x00", "Not an executable ELF")
assert(header:sub(19,20) == "\xF3\x00", "Not a RISC-V ELF")
local e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx = ("<I4I4I4I4I4I2I2I2I2I2I2"):unpack(header, 21)
assert(e_ehsize >= 52, "main header in ELF too small")
assert(e_phentsize >= 32, "program headers in ELF too small")
assert(e_shentsize >= 40, "section headers in ELF too small")

for n=1,e_phnum do
    assert(elf:seek("set", e_phoff + e_phentsize * (n-1)))
    header = assert(elf:read(32))
    local p_type, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_flags, p_align = ("<I4I4I4I4I4I4I4I4"):unpack(header)
    if p_type ~= 1 then
        -- ignore segments other than PT_LOAD
        goto continue
    end
    assert(p_vaddr == p_paddr, "ELF seems to assume virtual memory")
    assert(p_filesz <= p_memsz, "ELF has a program header larger on disk than in memory?")
    assert(p_vaddr % 4 == 0, "section not aligned to 4-byte boundary")
    assert(elf:seek("set", p_offset))
    local disk_size = p_filesz
    if disk_size % 4 ~= 0 then
        -- read extra bytes... :/
        disk_size = disk_size + (4 - disk_size % 4)
    end
    local data = assert(elf:read(disk_size))
    assert(#data == disk_size, "short read! (try again?)")
    for n=1,#data,4 do
        local offset = (n-1)
        ram[bit32.band(p_paddr+offset,0xFFFFFFFF)] = ("<I4"):unpack(data, n)
    end
    for offset=#data,p_memsz,4 do
        ram[bit32.band(p_paddr+offset,0xFFFFFFFF)] = 0
    end
    ::continue::
end

local symtab_header, strtab_header
for n=1,e_shnum do
    assert(elf:seek("set", e_shoff + e_shentsize * (n-1)))
    header = assert(elf:read(40))
    local sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize = ("<I4I4I4I4I4I4I4I4I4I4"):unpack(header)
    if sh_type == 2 then
        if symtab_header then
            error("ELF contains multiple symbol tables!")
        end
        symtab_header = {
            sh_name=sh_name,
            sh_type=sh_type,
            sh_flags=sh_flags,
            sh_addr=sh_addr,
            sh_offset=sh_offset,
            sh_size=sh_size,
            sh_link=sh_link,
            sh_info=sh_info,
            sh_addralign=sh_addralign,
            sh_entsize=sh_entsize,
        }
    elseif sh_type == 3 and strtab_header == nil then
        -- if there is more than one string table, we only care about the first
        -- one
        strtab_header = {
            sh_name=sh_name,
            sh_type=sh_type,
            sh_flags=sh_flags,
            sh_addr=sh_addr,
            sh_offset=sh_offset,
            sh_size=sh_size,
            sh_link=sh_link,
            sh_info=sh_info,
            sh_addralign=sh_addralign,
            sh_entsize=sh_entsize,
        }
    end
end

local start_instruction_count, start_clock
local stop_instruction_count, stop_clock

function rv32.read_word(cpu, addr)
    if bit32.band(addr,3) == 0 then
        if addr >= 0x00080000 then
            error(("out of range read address %08X"):format(addr))
        end
        return ram[addr] or 0
    else
        cpu:exception(rv32.EXC_MISALIGNED_LOAD, addr)
    end
end
function rv32.write_word(cpu, addr, value, mask)
    if bit32.band(addr,3) == 0 then
        if addr == 0xFFFFFF00 then
            assert(not start_instruction_count, "start_cycle writen more than once!")
            start_instruction_count, start_clock = cpu.instret_lo, os.clock()
        elseif addr == 0xFFFFFF04 then
            assert(not stop_instruction_count, "stop_cycle writen more than once!")
            stop_instruction_count, stop_clock = cpu.instret_lo, os.clock()
        else
            if addr >= 0x00080000 then
                error(("out of range write address %08X"):format(addr))
            end
            ram[addr] = bit32.bor(bit32.band((ram[addr] or 0),bit32.bnot(mask)),bit32.band(value,mask))
        end
    else
        cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
    end
end

local cpu = rv32.new()
-- initialize stack pointer
cpu.regs[2] = 0x0007FE00
-- and program counter
cpu.pc = e_entry

cpu.m_enabled = true
cpu.a_enabled = true
cpu.c_enabled = true
cpu.zicsr_enabled = false
cpu.strict_alignment = true

function cpu.execute_ecall(cpu)
    if not start_instruction_count or not stop_instruction_count then
        error("ECALL but benchmark not both started and finished!")
    end
    print(("exit status = %i"):format(cpu.regs[10]))
    print(("start cycles = %i"):format(start_instruction_count))
    print(("stop cycles = %i"):format(stop_instruction_count))
    local nanos = math.floor((stop_clock-start_clock)*1000000000)
    print(("realtime ns = %i"):format(nanos))
    print(("effective MIPS = %.3f"):format((stop_instruction_count - start_instruction_count) * 1000 / nanos))
    os.exit(0)
end

local iterations = 0
repeat
    cpu:run(100)
    iterations = iterations + 1
until iterations >= 10000000

error("infinite loop suspected")
