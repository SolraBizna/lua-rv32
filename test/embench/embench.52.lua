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
local rv32_elf = dofile(uspath.."../../lib/rv32_elf.lua")
local elf = assert(io.open(inelfpath, "rb"))
local ram = {}
local e_entry = assert(rv32_elf.load(elf, {
    write_word=function(addr, value)
        ram[addr] = value
    end,
}))

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
    print(("clock() ns = %i"):format(nanos))
    print(("effective MIPS = %.3f"):format((stop_instruction_count - start_instruction_count) * 1000 / nanos))
    os.exit(0)
end

local iterations = 0
repeat
    cpu:run(100)
    iterations = iterations + 1
until iterations >= 10000000

error("infinite loop suspected")
