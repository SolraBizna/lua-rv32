#!/usr/bin/env lua5.3

if #arg ~= 3 then
    print("usage: riscof-dut.lua input.elf output.signature.output rv32i[m][a][c]")
    os.exit(1)
end

local inelfpath = arg[1]
local outsigpath = arg[2]
local isa = arg[3]
-- create it now, in case of error
local sigout = assert(io.open(outsigpath, "wb"))

local uspath
if arg[0] then
    uspath = arg[0]:gsub("/[^/]*$","/")
end
if not uspath or uspath:sub(-1,-1) ~= "/" then
    uspath = "./" -- :S
end

local rv32 = dofile(uspath.."../../lib/rv32.53.trace.lua")

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
        ram[(p_paddr+offset)&0xFFFFFFFF] = ("<I4"):unpack(data, n)
    end
    for offset=#data,p_memsz,4 do
        ram[(p_paddr+offset)&0xFFFFFFFF] = 0
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

assert(symtab_header, "no symbol table, how will we extract the signature?")
assert(strtab_header, "no string table, how will we interpret the symbols?")
assert(elf:seek("set", strtab_header.sh_offset))
local raw_strtab = assert(elf:read(strtab_header.sh_size))
local strtab = {}
for offset,string in raw_strtab:gmatch("()([^\0]*)\0?") do
    for n=1,#string+1 do
        strtab[offset+n-2] = string:sub(n,-1)
    end
end
local symbols = {}
assert(elf:seek("set", symtab_header.sh_offset))
assert(symtab_header.sh_entsize == 16, "symbols not 16 bytes? CONFUSING")
for n = 0, symtab_header.sh_size-1, 16 do
    local buf = assert(elf:read(16))
    assert(#buf == 16)
    local st_name, st_value, st_size, st_info, st_other, st_shndx = ("<I4I4I4I1I1I2"):unpack(buf)
    local symbol_name = strtab[st_name]
    assert(symbol_name, "symbol name not in string table")
    symbols[symbol_name] = st_value
end


function rv32.read_word(cpu, addr)
    if addr & 3 == 0 then
        if addr < 0x80000000 then
            error(("out of range read address %08X"):format(addr))
        end
        return ram[addr] or 0
    else
        local shift = (addr & 3) << 3
        addr = addr & ~3
        return ((cpu:read_word(addr) >> shift) | (cpu:read_word((addr+4)&0xFFFFFFFF) << (32-shift))) & 0xFFFFFFFF
    end
end
function rv32.write_word(cpu, addr, value, mask)
    if addr & 3 == 0 then
        if addr < 0x80000000 then
            error(("out of range write address %08X"):format(addr))
        end
        ram[addr] = ((ram[addr] or 0) & ~mask) | (value & mask)
        print(("WRITE %08X ← %08X (mask %08X)"):format(addr, value, mask))
    else
        assert(mask == 0xFFFFFFFF, "unaligned word write with non-word granularity!")
        local shift = (addr & 3) << 3
        addr = addr & ~3
        cpu:write_word(addr, value << shift, 0xFFFFFFFF << shift)
        cpu:write_word((addr+4)&0xFFFFFFFF, value >> (32-shift), 0xFFFFFFFF >> (32-shift))
    end
end

local cpu = rv32.new()
-- initialize stack pointer
cpu.regs[2] = 0x81000000
-- and program counter
cpu.pc = e_entry

cpu.m_enabled = isa:match("m")
cpu.a_enabled = isa:match("a")
cpu.c_enabled = isa:match("c")
cpu.zicsr_enabled = true

local CSR_MSTATUS = 0x300
local CSR_MISA = 0x301
local CSR_MTVEC = 0x305
local CSR_MCAUSE = 0x342
local CSR_MEPC = 0x341
local CSR_MTVAL = 0x343
local CSR_MSCRATCH = 0x340

cpu.mstatus = 1 << 17
cpu.mtvec = 0
cpu.mcause = 0
cpu.mepc = 0
cpu.mtval = 0
cpu.mscratch = 0
cpu.misa = 0x40000100
if cpu.m_enabled then cpu.misa = cpu.misa | (1<<12) end
if cpu.a_enabled then cpu.misa = cpu.misa | (1<<0) end
if cpu.c_enabled then cpu.misa = cpu.misa | (1<<2) end

function rv32.read_csr(cpu, csr)
    print(("reading csr %03X"):format(csr))
    if csr == CSR_MSTATUS then
        return cpu.mstatus
    elseif csr == CSR_MISA then
        return cpu.misa
    elseif csr == CSR_MTVEC then
        return cpu.mtvec
    elseif csr == CSR_MCAUSE then
        return cpu.mcause
    elseif csr == CSR_MEPC then
        return cpu.mepc
    elseif csr == CSR_MTVAL then
        return cpu.mtval
    elseif csr == CSR_MSCRATCH then
        return cpu.mscratch
    end
end

function rv32.write_csr(cpu, csr, value)
    print(("writing csr %03X with %08X"):format(csr, value))
    if csr == CSR_MSTATUS then
        error("Attempted to write mstatus")
    elseif csr == CSR_MISA then
        error("Attempted to write misa")
    elseif csr == CSR_MTVEC then
        cpu.mtvec = value & 0xFFFFFFFC
    elseif csr == CSR_MCAUSE then
        cpu.mcause = value
    elseif csr == CSR_MEPC then
        cpu.mepc = value
    elseif csr == CSR_MTVAL then
        cpu.mtval = value
    elseif csr == CSR_MSCRATCH then
        cpu.mscratch = value
    else
        return false
    end
end

local old_exception = cpu.exception
local exception_count = 0
function rv32.exception(cpu, cause, tval)
    print("EXCEPTION! cause="..cpu.CAUSE_STRINGS[cause])
    cpu.mepc = cpu.pc
    cpu.pc = cpu.mtvec
    cpu.mcause = cause
    cpu.mtval = tval
    cpu.had_exception = true
    exception_count = exception_count + 1
    if exception_count > 100 then
        error("giving up after 100 exceptions")
    end
end

local function is_selfjump()
    local instruction = cpu:read_word(cpu.pc, 0xFFFFFFFF)
    return instruction == 0x0000006F or (instruction & 0xFFFF) == 0xA001
end

local iterations = 0
repeat
    cpu:run(100)
    iterations = iterations + 1
until is_selfjump() or iterations >= 100000

if not is_selfjump() then
    error("infinite loop suspected")
end

for addr=symbols.begin_signature,symbols.end_signature-1,4 do
    assert(sigout:write(("%08x\n"):format(ram[addr] or 0)))
end
sigout:close()
