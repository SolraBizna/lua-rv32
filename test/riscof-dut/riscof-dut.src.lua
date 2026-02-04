%lua(5.2){#!/usr/bin/env lua5.2}%lua(5.3){#!/usr/bin/env lua5.3}

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

rv32trace = not not os.getenv("RV32TRACE")
local rv32 = dofile(uspath.."../../lib/rv32.lua")
local rv32_elf = dofile(uspath.."../../lib/rv32_elf.lua")
local elf = assert(io.open(inelfpath, "rb"))
local ram = {}
local symbols = {}
local e_entry = assert(rv32_elf.load(elf, {
    write_word=function(addr,value)
        ram[addr] = value
    end,
    learn_symbol=function(name,value)
        symbols[name] = value
    end,
}))

function rv32.read_word(cpu, addr)
    if bit32.band(addr,3) == 0 then
        if addr < 0x80000000 then
            error(("out of range read address %08X"):format(addr))
        end
        return ram[addr] or 0
    else
        local shift = bit32.lshift(bit32.band(addr,3),3)
        addr = bit32.band(addr, 0xFFFFFFFC)
        return bit32.bor(bit32.rshift(cpu:read_word(addr), shift), (bit32.lshift(cpu:read_word(bit32.band(addr+4,0xFFFFFFFF)),(32-shift))))
    end
end
function rv32.write_word(cpu, addr, value, mask)
    if bit32.band(addr,3) == 0 then
        if addr < 0x80000000 then
            error(("out of range write address %08X"):format(addr))
        end
        ram[addr] = bit32.bor(bit32.band((ram[addr] or 0), bit32.bnot(mask)), bit32.band(value, mask))
        if rv32trace then
            print(("WRITE %08X ← %08X (mask %08X)"):format(addr, value, mask))
        end
    else
        assert(mask == 0xFFFFFFFF, "unaligned word write with non-word granularity!")
        local shift = bit32.lshift(bit32.band(addr, 3), 3)
        addr = bit32.band(addr, 0xFFFFFFFC)
        cpu:write_word(addr, bit32.lshift(value, shift), bit32.lshift(0xFFFFFFFF, shift))
        cpu:write_word(bit32.band(addr+4,0xFFFFFFFF), bit32.rshift(value, (32-shift)), bit32.rshift(0xFFFFFFFF, (32-shift)))
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

cpu.mstatus = bit32.lshift(1, 17)
cpu.mtvec = 0
cpu.mcause = 0
cpu.mepc = 0
cpu.mtval = 0
cpu.mscratch = 0
cpu.misa = 0x40000100
if cpu.m_enabled then cpu.misa = bit32.bor(cpu.misa, bit32.lshift(1,12)) end
if cpu.a_enabled then cpu.misa = bit32.bor(cpu.misa, bit32.lshift(1,0)) end
if cpu.c_enabled then cpu.misa = bit32.bor(cpu.misa, bit32.lshift(1,2)) end

function rv32.read_csr(cpu, csr)
    if rv32trace then
        print(("reading csr %03X"):format(csr))
    end
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
    if rv32trace then
        print(("writing csr %03X with %08X"):format(csr, value))
    end
    if csr == CSR_MSTATUS then
        error("Attempted to write mstatus")
    elseif csr == CSR_MISA then
        error("Attempted to write misa")
    elseif csr == CSR_MTVEC then
        cpu.mtvec = bit32.band(value, 0xFFFFFFFC)
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
    return instruction == 0x0000006F or bit32.band(instruction, 0xFFFF) == 0xA001
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
