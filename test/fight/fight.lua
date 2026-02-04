#!/usr/bin/env lua5.4

local f = assert(io.open("riscv-fight.bin","rb"))
local binary = assert(f:read("*a"))
f:close()
if #binary % 4 ~= 0 then
    binary = binary .. ("\0"):rep(4 - #binary % 4)
end
local ram = {}
for pos=1,#binary,4 do
    local addr = pos-1
    ram[addr] = ("<I4"):unpack(binary, pos)
end

local rv32 = dofile("../../lib/rv32.53.lua")
-- gross
function rv32.read_word(cpu, addr)
    if addr == 0xFFFFFFFC then
        repeat
            local ch = assert(io.read(1))
            if ch ~= "\n" then
                return ch:byte()
            end
        until false
    elseif addr & 3 == 0 then
        if addr > 0x1000000 then
            error("out of range read address")
        end
        return ram[addr] or 0
    else
        local shift = (addr & 3) << 3
        addr = addr & ~3
        return ((cpu:read_word(addr) >> shift) | (cpu:read_word((addr+4)&0xFFFFFFFF) << (32-shift))) & 0xFFFFFFFF
    end
end
function rv32.write_word(cpu, addr, value, mask)
    --print(("%08X := %08X & %08X"):format(addr, value, mask))
    if addr == 0xFFFFFFFC then
        io.write(string.char(value))
        io.flush()
    elseif addr & 3 == 0 then
        if addr > 0x1000000 then
            error("out of range write address")
        end
        ram[addr] = ((ram[addr] or 0) & ~mask) | (value & mask)
    else
        assert(mask == 0xFFFFFFFF, "unaligned word write with non-word granularity!")
        local shift = (addr & 3) << 3
        addr = addr & ~3
        cpu:write_word(addr, value << shift, 0xFFFFFFFF << shift)
        cpu:write_word((addr+4)&0xFFFFFFFF, value >> (32-shift), 0xFFFFFFFF >> (32-shift))
    end
end

local cpu = rv32.new()
while true do
    cpu:run(100)
end
