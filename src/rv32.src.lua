local rv32 = {
    REG_ZERO = 0,
    REG_RA = 1,
    REG_SP = 2,
    REG_GP = 3,
    REG_TP = 4,
    REG_T0 = 5,
    REG_T1 = 6,
    REG_T2 = 7,
    REG_S0 = 8,
    REG_FP = 8, -- yes, this register has two names in the ABI
    REG_S1 = 9,
    REG_A0 = 10,
    REG_A1 = 11,
    REG_A2 = 12,
    REG_A3 = 13,
    REG_A4 = 14,
    REG_A5 = 15,
    REG_A6 = 16,
    REG_A7 = 17,
    REG_S2 = 18,
    REG_S3 = 19,
    REG_S4 = 20,
    REG_S5 = 21,
    REG_S6 = 22,
    REG_S7 = 23,
    REG_S8 = 24,
    REG_S9 = 25,
    REG_S10 = 26,
    REG_S11 = 27,
    REG_T3 = 28,
    REG_T4 = 29,
    REG_T5 = 30,
    REG_T6 = 31,
    EXC_MISALIGNED_PC = 0,
    EXC_INSTRUCTION_ACCESS_FAULT = 1,
    EXC_ILLEGAL_INSTRUCTION = 2,
    EXC_BREAKPOINT = 3,
    EXC_MISALIGNED_LOAD = 4,
    EXC_LOAD_ACCESS_FAULT = 5,
    EXC_MISALIGNED_STORE = 6,
    EXC_STORE_ACCESS_FAULT = 7,
    EXC_ECALL_FROM_UMODE = 8,
    EXC_ECALL_FROM_SMODE = 9,
    EXC_ECALL_FROM_MMODE = 11,
    EXC_INSTRUCTION_PAGE_FAULT = 12,
    EXC_LOAD_PAGE_FAULT = 13,
    EXC_STORE_PAGE_FAULT = 15,
    CAUSE_STRINGS = {
        [0]="misaligned PC",
        "instruction access fault",
        "illegal instruction",
        "breakpoint",
        "misaligned load",
        "load access fault",
        "misaligned store/amo",
        "store/amo access fault",
        "ecall (from U-mode)",
        "ecall (from S-mode)",
        "ecall (from M-mode)",
        "instruction access page fault",
        "load page fault",
        nil,
        "store page fault",
    },
}

local function ympa(wat)
    return function()
        error("you must provide a "..wat.." function")
    end
end
rv32.read_word = ympa("read_word")
rv32.write_word = ympa("write_word")
rv32.read_csr = ympa("read_csr")
rv32.write_csr = ympa("write_csr")
rv32.exception = function(cpu, cause, tval)
    cpu.had_exception = true
    error(([[

The emulated RISC-V CPU encountered an exception, and no exception handler was provided by the embedding application.

Exception type: %s (%i)
Trap value (address/instruction): %08X
pc:%08X ra:%08X sp:%08X gp:%08X tp:%08X
t0:%08X t1:%08X t2:%08X s0:%08X s1:%08X
a0:%08X a1:%08X a2:%08X a3:%08X a4:%08X
a5:%08X a6:%08X a7:%08X s2:%08X s3:%08X
s4:%08X s5:%08X s6:%08X s7:%08X s8:%08X
s9:%08X sA:%08X sB:%08X t3:%08X t4:%08X
t5:%08X t6:%08X
]]):format(rv32.CAUSE_STRINGS[cause] or "unknown!", cause, tval, cpu.pc or 0xDEADBEEF, table.unpack(cpu.regs)))

end

%trace(){
if rv32trace == nil or rv32trace == true then
    if (package.config:sub(1,2) == "/\n" and os.getenv("TERM")) or os.getenv("RV32_FORCE_ANSI") then
        function rv32trace(cpu, val)
            if val == true then
                io.write("\x1B[1m") -- enable bold
            elseif val == false then
                io.write("\x1B[0m") -- clear styling
            else
                print(val)
            end
        end
    else
        function rv32trace(cpu, val)
            if val == true or val == false then
                -- do nothing, no styling available
            else
                print(val)
            end
        end
    end
elseif rv32trace == false then
    (warn or print)("`rv32trace` was set to false, but a tracing version of `lua-rv32` was loaded! No tracing will take place, BUT THE PERFORMANCE PRICE OF TRACING WILL STILL BE PAID!")
    function rv32trace() end
end
}
%notrace(){
if rv32trace then
    (warn or print)("`rv32trace` was set to a non-nil value, but a non-tracing version of `lua-rv32` was loaded! Tracing will NOT take place!")
end
}

function rv32.new()
    local ret = {
        regs={[0]=0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
        pc=0,
        budget=0,
        m_enabled=true,
        a_enabled=true,
        c_enabled=true,
        zicsr_enabled=false,
        had_exception=nil, -- \:|
        strict_alignment=nil, -- |:|
        reserved_address=nil, -- /:|
        instret_lo=0,
        instret_hi=0,
    }
    setmetatable(ret, {__index=rv32})
    return ret
end

function rv32.read_byte(cpu, addr)
    local shift = $lshift($band(addr,3),3)
    return $band($rshift(cpu:read_word($band(addr,$bnot(3))),shift),255)
end

function rv32.write_byte(cpu, addr, byte)
    local shift = $lshift($band(addr,3),3)
    byte = $bor(byte,$lshift(byte,16))
    byte = $bor(byte,$lshift(byte,8))
    cpu:write_word($band(addr,$bnot(3)), byte, $lshift(0xFF, shift))
end

function rv32.read_halfword(cpu, addr, exception_type)
    if cpu.strict_alignment and $btest(addr,1) then
        return cpu:exception(rv32.EXC_MISALIGNED_LOAD, addr)
    elseif $band(addr,3) == 3 then
        return $bor(cpu:read_byte(addr), $lshift(cpu:read_byte($band(addr+1,0xFFFFFFFF)), 8))
    else
        local word = cpu:read_word($band(addr,$bnot(3)), exception_type)
        local shift = $lshift($band(addr,3),3)
        return $band($rshift(word,shift),0xFFFF)
    end
end

function rv32.write_halfword(cpu, addr, value)
    if cpu.strict_alignment and $btest(addr,1) then
        return cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
    elseif $band(addr,3) == 3 then
        cpu:write_byte(addr, $band(value,255))
        cpu:write_byte(addr+1, $rshift(value,8))
    else
        local shift = $lshift($band(addr,3),3)
        cpu:write_word($band(addr,$bnot(3)), $lshift(value,shift), $lshift(0xFFFF,shift))
    end
end

function rv32.run(cpu, num_cycles)
    if num_cycles then
        cpu.budget = cpu.budget + num_cycles
    else
        cpu.budget = 1
    end
    while cpu.budget > 0 do
        local cost = 1
        local valid_instruction = false
        local orig_instruction
        local old_pc = cpu.pc
        local new_pc = nil
        do
        cpu.had_exception = nil
        local instruction
        -----FETCH-----
        if not cpu.c_enabled then
            if $btest(old_pc,3) then
                error("PC got a misaligned value with C disabled! THIS IS A BUG IN THE EMBEDDING PROGRAM!")
            end
            -- C disabled: fetch an aligned instruction word
            orig_instruction = cpu:read_word(old_pc, 1)
            if cpu.had_exception then
                goto abort_instruction
            end
            instruction = orig_instruction
            new_pc = old_pc + 4
            %trace() {
                rv32trace(cpu,true)
                if $band(orig_instruction,3) == 3 then
                    rv32trace(cpu,("EXECUTE @ %08X: %08X"):format(old_pc, orig_instruction))
                else
                    rv32trace(cpu,("EXECUTE @ %08X: %04X"):format(old_pc, $band(orig_instruction,0xFFFF)))
                end
                rv32trace(cpu,false)
            }
        else
            if $btest(old_pc,2) then
                -- C enabled, PC half-aligned; start by reading one halfword...
                orig_instruction = cpu:read_halfword(old_pc, 1)
                if cpu.had_exception then
                    goto abort_instruction
                end
                if $band(orig_instruction,3) == 3 then
                    -- ..and if it's the lower half of a 32-bit instruction,
                    -- read the other half.
                    cost = cost + 1
                    orig_instruction = $bor(orig_instruction, $lshift(cpu:read_halfword(old_pc+2,true),16))
                    if cpu.had_exception then
                        goto abort_instruction
                    end
                end
            else
                -- C enabled, PC full-aligned. Always read a whole word.
                orig_instruction = cpu:read_word(old_pc, 1)
                if cpu.had_exception then
                    goto abort_instruction
                end
            end
            %trace() {
                rv32trace(cpu,true)
                if $band(orig_instruction,3) == 3 then
                    rv32trace(cpu,("EXECUTE @ %08X: %08X"):format(old_pc, orig_instruction))
                else
                    rv32trace(cpu,("EXECUTE @ %08X: %04X"):format(old_pc, $band(orig_instruction,0xFFFF)))
                end
                rv32trace(cpu,false)
            }
            if $band(orig_instruction,3) == 3 then
                -- 32-bit instruction
                new_pc = old_pc + 4
                instruction = orig_instruction
            else
                -- 16-bit instruction... try to either decode it into its
                -- equivalent 32-bit instruction, OR execute it directly if
                -- that would be awkward.
                new_pc = old_pc + 2
                orig_instruction = $band(orig_instruction, 0xFFFF)
                -- Extract a scrambled operation code...
                local bitsy = $disassemble(orig_instruction, 15..13->2, 1..0->0)
                %select(bitsy){
                    0 => {
                        -- C.ADDI4SPN (add unsigned immediate to stack pointer)
                        local offset = $disassemble(orig_instruction, 12..11->4, 10..7->6, 6..6->2, 5..5->3)
                        if offset == 0 then
                            -- ADDI4SPN with zero offset is nonsense and
                            -- therefore illegal
                            goto instruction_legality_determined
                        end
                        local rd = $extract(orig_instruction, 2, 3)+8
                        %trace(){
                            rv32trace(cpu,("C.ADDI4SPN rd:%i=%08X offset:%i"):format(rd, cpu.regs[rd], offset))
                        }
                        cpu.regs[rd] = $band(cpu.regs[2]+offset, $bnot(0))
                        %trace(){
                            rv32trace(cpu,"r%i := %08X", rd, cpu.regs[rd])
                        }
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    8 => {
                        -- C.LW
                        local offset = $disassemble(orig_instruction, 12..10->3, 6..6->2, 5..5->6)
                        local rs1 = $extract(orig_instruction, 7, 3)+8
                        local rd = $extract(orig_instruction, 2, 3)+8
                        %trace(){
                            rv32trace(cpu,("C.LW rd:%i rs1:%i offset:%i"):format(rd, rs1, offset))
                        }
                        instruction = $assemble(0x2003, rs1->15, rd->7, offset->20)
                    }
                    24 => {
                        -- C.SW
                        local offset = $disassemble(orig_instruction, 12..10->3, 6..6->2, 5..5->6)
                        local rs1 = $extract(orig_instruction, 7, 3)+8
                        local rs2 = $extract(orig_instruction, 2, 3)+8
                        %trace(){
                            rv32trace(cpu,("C.SW rs1:%i rs2:%i offset:%i"):format(rs1, rs2, offset))
                        }
                        instruction = $assemble(0x2023, rs1->15, rs2->20, $rshift(offset,5)->25, $band(offset,31)->7)
                    }
                    1 => {
                        -- C.ADDI
                        local rd = $extract(orig_instruction, 7, 5)
                        local imm = $disassemble(orig_instruction, ~12..12->5, 6..2->0)
                        %trace(){
                            rv32trace(cpu,("C.ADDI rd:%i=%08X imm:%08X"):format(rd, cpu.regs[rd], imm))
                        }
                        if rd ~= 0 then
                            cpu.regs[rd] = $band(cpu.regs[rd]+imm, $bnot(0))
                            %trace(){
                                rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                            }
                        end
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    5 => {
                        -- C.JAL
                        -- to quote the original comment from rrv32:
                        -- "what the"
                        local imm = $disassemble(orig_instruction, ~12..12->11, 11..11->4, 10..9->8, 8..8->10, 7..7->6, 6..6->7, 5..3->1, 2..2->5)
                        cpu.regs[1] = new_pc
                        local target = $band(old_pc + imm, $bnot(0))
                        %trace(){
                            rv32trace(cpu,("C.JAL imm:%i"):format(imm))
                            rv32trace(cpu,("pc := %08X"):format(target))
                        }
                        -- we're in C mode so the PC can't misalign
                        new_pc = target
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    9 => {
                        -- C.LI
                        local imm = $disassemble(orig_instruction, ~12..12->5, 6..2->0)
                        local rd = $extract(orig_instruction, 7, 5)
                        %trace(){
                            rv32trace(cpu,("C.LI rd:%i imm:%i"):format(rd, imm))
                        }
                        if rd ~= 0 then
                            cpu.regs[rd] = imm
                            %trace(){
                                rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                            }
                        end
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    13 => {
                        local rd = $extract(orig_instruction, 7, 5)
                        if rd == 2 then
                            -- C.ADDI16SP!!!
                            local imm = $disassemble(orig_instruction, ~12..12->9, 6..6->4, 5..5->6, 4..3->7, 2..2->5)
                            %trace(){
                                rv32trace(cpu,("C.ADDI16SP imm:%i"):format(imm))
                                rv32trace(cpu,("r2 == %08X"):format(cpu.regs[2]))
                            }
                            cpu.regs[2] = $band(cpu.regs[2] + imm, $bnot(0))
                            %trace(){
                                rv32trace(cpu,("r2 := %08X"):format(cpu.regs[2]))
                            }
                            valid_instruction = true
                            goto instruction_legality_determined
                        else
                            -- C.LUI
                            local imm = $disassemble(orig_instruction, ~12..12->17, 6..2->12)
                            if imm == 0 then
                                -- bad instruction!
                                goto instruction_legality_determined
                            end
                            %trace(){
                                rv32trace(cpu,("C.LUI rd:%i imm:%i"):format(rd, imm))
                            }
                            if rd ~= 0 then
                                cpu.regs[rd] = imm
                                %trace(){
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                }
                            end
                            valid_instruction = true
                            goto instruction_legality_determined
                        end
                    }
                    17 => {
                        -- some arithmetic operations
                        local funct2 = $extract(orig_instruction, 10, 2)
                        local rd = $extract(orig_instruction, 7, 3)+8
                        %select(funct2){
                            0 => {
                                -- SRLI
                                if $btest(orig_instruction, 0x1000) then
                                    -- illegal bit pattern
                                    goto instruction_legality_determined
                                end
                                local amt = $extract(orig_instruction, 2, 5)
                                %trace(){
                                    rv32trace(cpu,("C.SRLI rd:%i=%08X shamt:%i"):format(rd, cpu.regs[rd], amt))
                                }
                                cpu.regs[rd] = $rshift(cpu.regs[rd], amt)
                                %trace() {
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                }
                                valid_instruction = true
                                goto instruction_legality_determined
                            }
                            1 => {
                                -- SRAI
                                if $btest(orig_instruction, 0x1000) then
                                    -- illegal bit pattern
                                    goto instruction_legality_determined
                                end
                                local amt = $extract(orig_instruction, 2, 5)
                                %trace(){
                                    rv32trace(cpu,("C.SRAI rd:%i=%08X shamt:%i"):format(rd, cpu.regs[rd], amt))
                                }
                                cpu.regs[rd] = $arshift(cpu.regs[rd], amt)
                                %trace() {
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                }
                                valid_instruction = true
                                goto instruction_legality_determined
                            }
                            2 => {
                                -- ANDI
                                local imm = $disassemble(orig_instruction, ~12..12->5, 6..2->0)
                                %trace(){
                                    rv32trace(cpu,("C.ANDI rd:%i=%08X imm=%08X"):format(rd, cpu.regs[rd], imm))
                                }
                                cpu.regs[rd] = $band(cpu.regs[rd], imm)
                                %trace(){
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                }
                                valid_instruction = true
                                goto instruction_legality_determined
                            }
                            3 => {
                                -- and the rest~
                                local op = $extract(orig_instruction, 5, 2)
                                local rs2 = $extract(orig_instruction, 2, 3)+8
                                %trace(){
                                    rv32trace(cpu,("C.OP rd:%i=%08X rs2:%i=%08X op:%i"):format(rd, cpu.regs[rd], rs2, cpu.regs[rs2], op))
                                }
                                %select(op){
                                    0 => {
                                        -- SUB
                                        cpu.regs[rd] = $band(cpu.regs[rd]-cpu.regs[rs2],0xFFFFFFFF)
                                    }
                                    1 => {
                                        -- XOR
                                        cpu.regs[rd] = $bxor(cpu.regs[rd],cpu.regs[rs2])
                                    }
                                    2 => {
                                        -- OR
                                        cpu.regs[rd] = $band($bor(cpu.regs[rd],cpu.regs[rs2]),0xFFFFFFFF)
                                    }
                                    3 => {
                                        -- AND
                                        cpu.regs[rd] = $band(cpu.regs[rd],cpu.regs[rs2])
                                    }
                                }
                                %trace(){
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                }
                                valid_instruction = true
                                goto instruction_legality_determined
                            }
                        }
                    }
                    21 => {
                        -- C.J
                        -- oh, ye gods!
                        local imm = $disassemble(orig_instruction, ~12..12->11, 11..11->4, 10..9->8, 8..8->10, 7..7->6, 6..6->7, 5..3->1, 2..2->5)
                        local target = $band(old_pc + imm, $bnot(1))
                        %trace(){
                            rv32trace(cpu,("C.J imm=%08X"):format(imm))
                            rv32trace(cpu,("pc := %08X"):format(target))
                        }
                        -- we're in C mode so the PC can't misalign
                        new_pc = target
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    25 => {
                        -- C.BEQZ
                        local rs1 = $extract(orig_instruction, 7, 3)+8
                        %trace(){
                            local target = $band(old_pc + $disassemble(orig_instruction, ~12..12->8, 11..10->3, 6..5->6, 4..3->1, 2..2->5), $bnot(1))
                            rv32trace(cpu,("C.BEQZ rs1=%i:%08X target=%08X"):format(rs1, cpu.regs[rs1], target))
                        }
                        if cpu.regs[rs1] == 0 then
                            local offset = $disassemble(orig_instruction, ~12..12->8, 11..10->3, 6..5->6, 4..3->1, 2..2->5)
                            new_pc = $band(old_pc + offset, $bnot(1))
                            -- we're in C mode so the PC can't misalign
                        end
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    29 => {
                        -- C.BNEZ
                        local rs1 = $extract(orig_instruction, 7, 3)+8
                        %trace(){
                            local target = $band(old_pc + $disassemble(orig_instruction, ~12..12->8, 11..10->3, 6..5->6, 4..3->1, 2..2->5), $bnot(1))
                            rv32trace(cpu,("C.BNEZ rs1=%i:%08X target=%08X"):format(rs1, cpu.regs[rs1], target))
                        }
                        if cpu.regs[rs1] ~= 0 then
                            local offset = $disassemble(orig_instruction, ~12..12->8, 11..10->3, 6..5->6, 4..3->1, 2..2->5)
                            new_pc = $band(old_pc + offset, $bnot(1))
                            -- we're in C mode so the PC can't misalign
                        end
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    2 => {
                        -- C.SLLI
                        if $btest(orig_instruction, 0x1000) then
                            -- illegal bit pattern
                            goto instruction_legality_determined
                        end
                        local amt = $extract(orig_instruction, 2, 5)
                        local rd = $extract(orig_instruction, 7, 5)
                        %trace(){
                            rv32trace(cpu,("C.SLLI rd:%i=%08X amt=%i"):format(rd, cpu.regs[rd], amt))
                        }
                        cpu.regs[rd] = $lshift(cpu.regs[rd], amt)
                        %trace(){
                            rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                        }
                        valid_instruction = true
                        goto instruction_legality_determined
                    }
                    10 => {
                        -- C.LWSP
                        local rd = $extract(orig_instruction, 7, 5)
                        if rd == 0 then
                            goto instruction_legality_determined
                        end
                        local offset = $disassemble(orig_instruction, 12..12->5, 6..4->2, 3..2->6)
                        instruction = $assemble(0x2003, offset->20, rd->7, 2->15)
                        %trace(){
                            rv32trace(cpu,("C.LWSP rd=%i offset=%i"):format(rd, offset))
                        }
                    }
                    26 => {
                        -- C.SWSP
                        local rd = $extract(orig_instruction, 2, 5)
                        local offset = $disassemble(orig_instruction, 12..9->2, 8..7->6)
                        instruction = $assemble(0x2023, rd->20, 2->15, $rshift(offset,5)->25, $band(offset,31)->7)
                        %trace(){
                            rv32trace(cpu,("C.SWSP rd=%i offset=%i"):format(rd, offset))
                        }
                    }
                    18 => {
                        -- nightmare
                        local twelve = $extract(orig_instruction, 12, 1)
                        local rs1 = $extract(orig_instruction, 7, 5)
                        local rs2 = $extract(orig_instruction, 2, 5)
                        if twelve == 0 then
                            if rs2 == 0 then
                                -- C.JR
                                if rs1 == 0 then
                                    goto instruction_legality_determined
                                end
                                %trace(){
                                    rv32trace(cpu,("C.JR rs1=%i:%08X"):format(rs1, cpu.regs[rs1]))
                                }
                                new_pc = $band(cpu.regs[rs1],$bnot(1))
                                -- we're in C mode so the PC can't misalign
                            else
                                -- C.MV
                                -- rs2 cannot be zero
                                %trace(){
                                    rv32trace(cpu,("C.MV rd=%i rs2=%i:%08X"):format(rs1, rs2, cpu.regs[rs2]))
                                }
                                if rs1 ~= 0 then
                                    cpu.regs[rs1] = cpu.regs[rs2]
                                    %trace(){
                                        rv32trace(cpu,("r%i := %08X"):format(rs1, cpu.regs[rs1]))
                                    }
                                end
                            end
                            valid_instruction = true
                            goto instruction_legality_determined
                        elseif rs2 == 0 then
                            if rs1 == 0 then
                                -- C.EBREAK
                                instruction = 0x8003B
                                %trace(){
                                    rv32trace(cpu,"C.EBREAK")
                                }
                            else
                                -- C.JALR
                                local destination = cpu.regs[rs1]
                                %trace(){
                                    rv32trace(cpu,("C.JALR rs1:%i=%08X"):format(rs1, destination))
                                }
                                cpu.regs[1] = new_pc
                                new_pc = $band(destination,$bnot(1))
                                -- we're in C mode so the PC can't misalign
                                valid_instruction = true
                                goto instruction_legality_determined
                            end
                        else
                            -- C.ADD
                            %trace(){
                                rv32trace(cpu,("C.ADD rs1:%i=%08X rs2:%i=%08X"):format(rs1, cpu.regs[rs1], rs2, cpu.regs[rs2]))
                            }
                            instruction = $assemble(0x33, rs1->7, rs1->15, rs2->20)
                        end
                    }
                }
                if not instruction then
                    goto instruction_legality_determined
                end
            end
        end
        -- if we get here in C mode, we had a full-size instruction OR we
        -- decompressed a half-size instruction into a full-size one
        if $band(instruction,3) ~= 3 then
            cpu:exception(rv32.EXC_ILLEGAL_INSTRUCTION, orig_instruction)
            goto abort_instruction
        end
        -----EXECUTE-----
        -- (some 16-bit instructions will have been executed above)
        local opcode = $extract(instruction, 2, 5)
        %select(opcode){
            0 => {
-- load
cpu.reserved_address = nil
cost = cost + 1
local rs1 = $rs1(instruction)
local imm = $immI(instruction)
local funct3 = $funct3(instruction)
local addr = $band(cpu.regs[rs1] + imm, 0xFFFFFFFF)
%trace() {
rv32trace(cpu,("LOAD rs1:%i=%08X imm:%08X funct3:%i addr:%08X"):format(rs1, cpu.regs[rs1], $band(imm,0xFFFFFFFF), funct3, addr))
}
local result
%select(funct3){
    0 => {
        result = cpu:read_byte(addr)
        if cpu.had_exception then goto abort_instruction end
        result = $sex8(result)
    }
    1 => {
        if $band(addr,3) == 3 then
            cost = cost + 1
        end
        result = cpu:read_halfword(addr)
        if cpu.had_exception then goto abort_instruction end
        result = $sex16(result)
    }
    2 => {
        if $btest(addr,3) then
            cost = cost + 1
        end
        result = cpu:read_word(addr)
        if cpu.had_exception then goto abort_instruction end
    }
    4 => {
        result = cpu:read_byte(addr)
        if cpu.had_exception then goto abort_instruction end
    }
    5 => {
        if $band(addr,3) == 3 then
            cost = cost + 1
        end
        result = cpu:read_halfword(addr)
        if cpu.had_exception then goto abort_instruction end
    }
}
if cpu.had_exception then
    goto abort_instruction
end
if result ~= nil then
    local rd = $rd(instruction)
    %trace() {
        rv32trace(cpu,("r%i := %08X"):format(rd, result))
    }
    if rd ~= 0 then cpu.regs[rd] = result end
    valid_instruction = true
end
            }
            -- 1 => load (floating point)
            -- 2 => custom-0 (see bottom)
            3 => {
local funct3 = $funct3(instruction)
-- misc-mem
%select(funct3){
    0 => {
        -- FENCE (implemented as no-op)
        valid_instruction = true
    }
    1 => {
        -- IFENCE.I (implemented as no-op)
        valid_instruction = true
    }
}
            }
            4 => {
-- op (immediate)
local rs1 = $rs1(instruction)
local a = cpu.regs[rs1]
local b = $immI(instruction)
local result
local funct3 = $funct3(instruction)
local rd = $rd(instruction)
%trace(){
rv32trace(cpu,("OP-I rd:%i rs1:%i=%08X imm:%08X funct3:%i"):format(rd, rs1, a, $band(b,0xFFFFFFFF), funct3))
}
%select(funct3){
    0 => {
        -- ADDI
        result = $band(a+b,0xFFFFFFFF)
    }
    1 => {
        -- SLLI
        if $funct7(instruction) == 0 then
            result = $lshift(a,$band(b,31))
        end
    }
    2 => {
        -- SLTI
        result = $sex32(a) < $sex32(b) and 1 or 0
    }
    3 => {
        -- SLTIU
        result = a < b and 1 or 0
    }
    4 => {
        -- XORI
        result = $bxor(a,b)
    }
    5 => {
        -- SRLI/SRAI
        local funct7 = $funct7(instruction)
        if funct7 == 0x20 then
            -- SRAI
            result = $arshift(a,$band(b,31))
        elseif funct7 == 0x00 then
            -- SRLI
            result = $rshift(a,$band(b,31))
        end
    }
    6 => {
        -- ORI
        result = $bor(a, b)
    }
    7 => {
        -- ANDI
        result = $band(a, b)
    }
}
if result ~= nil then
    %trace(){
        rv32trace(cpu,("r%i := %08X"):format(rd, result))
    }
    if rd ~= 0 then cpu.regs[rd] = result end
    valid_instruction = true
end
            }
            5 => {
-- auipc = add upper immediate to pc
local rd = $rd(instruction)
local ui = $band(instruction,$bnot(0xFFF))
%trace(){
rv32trace(cpu,("AUIPC r%i := pc + %08X"):format(rd, $band(ui,0xFFFFFFFF)))
}
if rd ~= 0 then
    cpu.regs[rd] = $band(old_pc + ui, 0xFFFFFFFF)
end
valid_instruction = true
            }
            -- 6 => op-imm32 (64-bit only)
            -- 7 => 48-bit instruction
            8 => {
-- store
cpu.reserved_address = nil
cost = cost + 1
local rs1 = $rs1(instruction)
local rs2 = $rs2(instruction)
local value = cpu.regs[rs2]
local imm = $immS(instruction)
local funct3 = $funct3(instruction)
local addr = $band(cpu.regs[rs1] + imm, 0xFFFFFFFF)
%trace() {
rv32trace(cpu,("STORE rs1:%i=%08X rs2:%i=%08X imm:%08X funct3:%i addr:%08X"):format(rs1, cpu.regs[rs1], rs2, cpu.regs[rs2], $band(imm,0xFFFFFFFF), funct3, addr))
}
%select(funct3){
    0 => {
        cpu:write_byte(addr, $band(value,255))
        if cpu.had_exception then goto abort_instruction end
        valid_instruction = true
    }
    1 => {
        if $band(addr,3) == 3 then
            cost = cost + 1
        end
        cpu:write_halfword(addr, $band(value,65535))
        if cpu.had_exception then goto abort_instruction end
        valid_instruction = true
    }
    2 => {
        if $btest(addr,3) then
            cost = cost + 1
        end
        cpu:write_word(addr, value, 0xFFFFFFFF)
        if cpu.had_exception then goto abort_instruction end
        valid_instruction = true
    }
}
if cpu.had_exception then
    goto abort_instruction
end
            }
            -- 9 => store (floating point)
            -- 10 => custom-1 (see bottom)
            11 => {
                if not cpu.a_enabled then goto bad_amo end do
                -- aq/rl ignored because we don't have caches or snooping...
                local funct5 = $funct5(instruction)
                local funct3 = $funct3(instruction)
                local rs1 = $rs1(instruction)
                local rs2 = $rs2(instruction)
                local rd = $rd(instruction)
                if funct3 ~= 2 then
                    goto bad_amo
                end
                local addr = cpu.regs[rs1]
                if $btest(addr,3) then
                    cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
                    goto abort_instruction
                end
                if funct5 ~= 3 then
                    -- no AMO operation other than SC will ever preserve the
                    -- reservation
                    cpu.reserved_address = nil
                end
                local amo_op
                %select(funct5){
                    -- these are in a weird order but, ok
                    2 => {
                        -- LR.W - load reserved word
                        if rs2 ~= 0 then goto bad_amo end
                        local value = cpu:read_word(addr)
                        if cpu.had_exception then
                            goto abort_instruction
                        end
                        if rd ~= 0 then
                            cpu.regs[rd] = value
                        end
                        cpu.reserved_address = addr
                        valid_instruction = true
                    }
                    3 => {
                        -- SC.W - store conditional word
                        if addr == cpu.reserved_address then
                            local value = cpu.regs[rs2]
                            cpu:write_word(addr, value, 0xFFFFFFFF)
                            if cpu.had_exception then goto abort_instruction end
                            if rd ~= 0 then
                                cpu.regs[rd] = 0
                            end
                        else
                            if rd ~= 0 then
                                cpu.regs[rd] = 1 -- unspecified failure
                            end
                        end
                        valid_instruction = true
                    }
                    1 => { amo_op = function(mem,reg) return reg end }
                    0 => { amo_op = function(mem,reg) return $band(mem+reg,0xFFFFFFFF) end }
                    4 => {
                        %lua(5.2) { amo_op = bit32.bxor }
                        %lua(5.3) { amo_op = function(mem,reg) return mem~reg end }
                    }
                    12 => {
                        %lua(5.2) { amo_op = bit32.band }
                        %lua(5.3) { amo_op = function(mem,reg) return mem&reg end }
                    }
                    8 => {
                        %lua(5.2) { amo_op = bit32.bor }
                        %lua(5.3) { amo_op = function(mem,reg) return mem|reg end }
                    }
                    16 => {
                        amo_op = function(mem,reg)
                            mem = $sex32(mem)
                            reg = $sex32(reg)
                            return $band(math.min(mem, reg),0xFFFFFFFF)
                        end
                    }
                    20 => {
                        amo_op = function(mem,reg)
                            mem = $sex32(mem)
                            reg = $sex32(reg)
                            return $band(math.max(mem, reg),0xFFFFFFFF)
                        end
                    }
                    24 => { amo_op = math.min }
                    28 => { amo_op = math.max }
                }
                if amo_op then
                    local mem_value = cpu:read_word(addr, 7)
                    if cpu.had_exception then
                        goto abort_instruction
                    end
                    local reg_value = cpu.regs[rs2]
                    local write_value = amo_op(mem_value, reg_value)
                    cpu:write_word(addr, write_value, 0xFFFFFFFF)
                    if cpu.had_exception then
                        goto abort_instruction
                    end
                    if rd ~= 0 then
                        cpu.regs[rd] = mem_value
                    end
                    valid_instruction = true
                end
                if valid_instruction then
                    cost = cost + 3
                end
                end ::bad_amo::
            }
            12 => {
-- op (register-to-register)
local rs1 = $rs1(instruction)
local a = cpu.regs[rs1]
local rs2 = $rs2(instruction)
local b = cpu.regs[rs2]
local result
local funct3 = $funct3(instruction)
local funct7 = $funct7(instruction)
local rd = $rd(instruction)
%trace(){
rv32trace(cpu,("OP rd:%i rs1:%i=%08X rs2:%i=%08X funct3:%i funct7:%i"):format(rd, rs1, a, rs2, b, funct3, funct7))
}
local funct37 = $bor($lshift(funct7,3),funct3)
%select(funct37){
    0 => {
        -- ADD
        result = $band(a+b,0xFFFFFFFF)
    }
    256 => {
        -- SUB
        result = $band(a-b,0xFFFFFFFF)
    }
    1 => {
        -- SLL
        result = $lshift(a,$band(b,31))
    }
    2 => {
        -- SLT
        result = $sex32(a) < $sex32(b) and 1 or 0
    }
    3 => {
        -- SLTU
        result = a < b and 1 or 0
    }
    4 => {
        -- XOR
        result = $bxor(a,b)
    }
    5 => {
        -- SRL
        result = $rshift(a,$band(b,31))
    }
    261 => {
        -- SRA
        result = $arshift(a,$band(b,31))
    }
    6 => {
        -- OR
        result = $bor(a, b)
    }
    7 => {
        -- AND
        result = $band(a, b)
    }
    8 => {
        -- MUL
        if cpu.m_enabled then
            cost = cost + 3
            %lua(5.2) {
                local low_result = ($band(a,0xFFFF)*b) % 0x100000000
                local high_result = ($band(a,0xFFFF0000)*b) % 0x100000000
                result = low_result + high_result
            }
            %lua(5.3){
                result = (a*b)&0xFFFFFFFF
            }
        end
    }
    9 => {
        -- MULH
        if cpu.m_enabled then
            %lua(5.2) {
                result = 0
                local sign = 1
                a = $sex32(a)
                b = $sex32(b)
                if a < 0 then
                    a = -a
                    sign = -sign
                end
                if b < 0 then
                    b = -b
                    sign = -sign
                end
                if a > b then
                    b,a = a,b
                end
                local mult = 0x1p-33
                while a > 0 do
                    cost = cost + 1
                    result = result * 0.5
                    if bit32.btest(a,1) then
                        result = result + b
                    end
                    if sign < 0 then
                        result = math.ceil(result)
                    else
                        result = math.floor(result)
                    end
                    a = bit32.rshift(a,1)
                    mult = mult * 2
                end
                if sign < 0 then
                    result = math.ceil(result * mult) * -1
                else
                    result = math.floor(result * mult)
                end
                result = $band(result,$bnot(0))
            }
            %lua(5.3){
                cost = cost + 3
                result = (a*b)&0xFFFFFFFF
                a = $sex32(a)
                b = $sex32(b)
                result = ((a*b)>>32)&0xFFFFFFFF
            }
        end
    }
    10 => {
        -- MULHSU
        if cpu.m_enabled then
            %lua(5.2) {
                result = 0
                a = $sex32(a)
                local mult = 0x1p-33
                while b > 0 do
                    cost = cost + 1
                    result = result * 0.5
                    if bit32.btest(b,1) then
                        result = result + a
                    end
                    result = math.floor(result)
                    b = bit32.rshift(b,1)
                    mult = mult * 2
                end
                result = math.floor(result * mult)
                result = $band(result,$bnot(0))
            }
            %lua(5.3) {
                cost = cost + 3
                a = $sex32(a)
                result = ((a*b)>>32)&0xFFFFFFFF
            }
        end
    }
    11 => {
        -- MULHU
        if cpu.m_enabled then
            %lua(5.2) {
                if a > b then
                    b,a = a,b
                end
                result = 0
                local mult = 0x1p-33
                while a > 0 do
                    cost = cost + 1
                    result = result * 0.5
                    if bit32.btest(a,1) then
                        result = result + b
                    end
                    result = math.floor(result)
                    a = bit32.rshift(a,1)
                    mult = mult * 2
                end
                result = math.floor(result * mult)
            }
            %lua(5.3){
                cost = cost + 3
                result = (a*b)>>32
            }
        end
    }
    12 => {
        -- DIV
        if cpu.m_enabled then
            cost = cost + 5
            if b == 0 then
                result = 0xFFFFFFFF
            elseif a == 0x80000000 and b == 0xFFFFFFFF then
                result = 0x80000000
            else
                result = ($sex32(a) / $sex32(b))
                if result < 0 then
                    result = math.ceil(result)
                else
                    result = math.floor(result)
                end
                result = $band(result, 0xFFFFFFFF)
            end
        end
    }
    13 => {
        -- DIVU
        if cpu.m_enabled then
            cost = cost + 5
            %lua(5.2) {
                if b == 0 then
                    result = 0xFFFFFFFF
                else
                    result = $band(math.floor(a/b), 0xFFFFFFFF)
                end
            }
            %lua(5.3) {
                if b == 0 then
                    result = 0xFFFFFFFF
                else
                    result = a // b
                end
            }
        end
    }
    14 => {
        -- REM
        if cpu.m_enabled then
            cost = cost + 5
            if b == 0 then
                result = a
            elseif a == 0x80000000 and b == 0xFFFFFFFF then
                result = 0
            else
                a = $sex32(a)
                b = $sex32(b)
                local quotient = (a / b)
                if quotient < 0 then
                    quotient = math.ceil(quotient)
                else
                    quotient = math.floor(quotient)
                end
                result = a - (quotient * b)
                result = $band(result, 0xFFFFFFFF)
            end
        end
    }
    15 => {
        -- REMU
        if cpu.m_enabled then
            cost = cost + 5
            %lua(5.2) {
                if b == 0 then
                    result = a
                else
                    result = $band(a%b, 0xFFFFFFFF)
                end
            }
            %lua(5.3) {
                if b == 0 then
                    result = a
                else
                    result = a % b
                end
            }
        end
    }
}
if result ~= nil then
    %trace(){
        rv32trace(cpu,("r%i := %08X"):format(rd, result))
    }
    if rd ~= 0 then cpu.regs[rd] = result end
    valid_instruction = true
end
            }
            13 => {
-- lui = load upper immediate
local rd = $rd(instruction)
local ui = $band(instruction,$bnot(0xFFF))
%trace(){
rv32trace(cpu,("LUI r%i := %08X"):format(rd, $band(ui,0xFFFFFFFF)))
}
if rd ~= 0 then
    cpu.regs[rd] = ui
end
valid_instruction = true
            }
            -- 14 => op-32 (64-bit only)
            -- 15 => 64-bit instruction
            -- 16 => madd (floating point)
            -- 17 => msub (floating point)
            -- 18 => nmadd (floating point)
            -- 19 => nmsub (floating point)
            -- 20 => op (floating point)
            -- 21 => reserved
            -- 22 => custom-2 (see bottom)
            -- 23 => 48-bit instruction
            24 => {
-- various forms of branch
local rs1 = $rs1(instruction)
local rs2 = $rs2(instruction)
local a = cpu.regs[rs1]
local b = cpu.regs[rs2]
local funct3 = $funct3(instruction)
local should_branch
%trace(){
rv32trace(cpu,("BRANCH rs1:%i=%08X rs2:%i=%08X funct3:%i imm:%08X"):format(rs1, a, rs2, b, funct3, $immB(instruction)))
}
%select(funct3){
    0 => { should_branch = a == b }
    1 => { should_branch = a ~= b }
    4 => { should_branch = $sex32(a) < $sex32(b) }
    5 => { should_branch = $sex32(a) >= $sex32(b) }
    6 => { should_branch = a < b }
    7 => { should_branch = a >= b }
}
valid_instruction = should_branch ~= nil
if should_branch then
    local imm = $immB(instruction)
    local target = $band(old_pc + imm, $bnot(1))
    if not cpu.c_enabled and $btest(target,2) then
        cpu:exception(rv32.EXC_MISALIGNED_PC, target)
        goto abort_instruction
    else
        new_pc = target
    end
end
            }
            25 => {
-- jalr = jump (and link) to register
local rd = $rd(instruction)
local rs1 = $rs1(instruction)
local imm = $immI(instruction)
%trace(){
rv32trace(cpu,("JALR r%i := %08X, rs1:%i=%08X, imm=%08X"):format(rd, cpu.pc, rs1, cpu.regs[rs1], imm))
}
local target=$band(cpu.regs[rs1]+imm,$bnot(1))
if rd ~= 0 then
    cpu.regs[rd] = new_pc
end
if not cpu.c_enabled and $btest(target,2) then
    cpu:exception(rv32.EXC_MISALIGNED_PC, target)
    goto abort_instruction
else
    new_pc = target
end
valid_instruction = true
            }
            -- 26 => reserved
            27 => {
-- jal = jump and link
local old_new_pc = new_pc
local rd = $rd(instruction)
local imm = $immJ(instruction)%trace(){
rv32trace(cpu,("JAL r%i := %08X, imm=%08X"):format(rd, cpu.pc, imm))
}
local target = $band(old_pc + imm, $bnot(1))
if not cpu.c_enabled and $btest(target,2) then
    cpu:exception(rv32.EXC_MISALIGNED_PC, target)
    goto abort_instruction
else
    new_pc = target
end
if rd ~= 0 then
    cpu.regs[rd] = old_new_pc
end
valid_instruction = true
            }
            28 => {
                -- system
                local funct3 = $funct3(instruction)
                if funct3 == 0 or funct3 == 4 then
                    -- note: normally ecall/ebreak should not retire, but if
                    -- the caller wishes it, we let them retire
                    if instruction == 0x00000073 then
                        if cpu.execute_ecall then
                            cost = cost + (cpu:execute_ecall() or 0)
                            if cpu.had_exception then
                                goto abort_instruction
                            end
                            valid_instruction = true
                        end
                    elseif instruction == 0x00100073 then
                        if cpu.execute_ebreak then
                            cost = cost + (cpu:execute_ebreak() or 0)
                            if cpu.had_exception then
                                goto abort_instruction
                            end
                            valid_instruction = true
                        end
                    end
                    goto instruction_legality_determined
                end
                if not cpu.zicsr_enabled then
                    goto instruction_legality_determined
                end
                -- CSRRW/CSRRS/CSRRC or one of the I variants
                local rd = $rd(instruction)
                local rs1 = $rs1(instruction)
                local csr = $extract(instruction, 20, 12)
                %trace(){
                    local name,rs1name,rs1format
                    %select(funct3){
                        1 => { name,rs1name,rs1format = "CSRRW","rs1","%i=%08X" }
                        2 => { name,rs1name,rs1format = "CSRRS","rs1","%i=%08X" }
                        3 => { name,rs1name,rs1format = "CSRRC","rs1","%i=%08X" }
                        5 => { name,rs1name,rs1format = "CSRRWI","imm","%i" }
                        6 => { name,rs1name,rs1format = "CSRRSI","imm","%i" }
                        7 => { name,rs1name,rs1format = "CSRRCI","imm","%i" }
                    }
                    rv32trace(cpu,("%s csr:%i rd:%i %s:"..rs1format):format(name, csr, rd, rs1name, rs1, cpu.regs[rs1]))
                }
                local wvalue
                if funct3 >= 4 then
                    -- I variant
                    wvalue = rs1
                else
                    -- Non-I variant
                    wvalue = cpu.regs[rs1]
                end
                local funct2 = $band(funct3,3)
                local rvalue
                if rd ~= 0 or funct2 ~= 1 then
                    rvalue = cpu:read_csr(csr)
                    if cpu.had_exception then
                        goto abort_instruction
                    end
                    if rvalue == nil then
                        goto instruction_legality_determined
                    end
                end
                %select(funct2){
                    1 => {
                        -- Read & Write
                        valid_instruction = cpu:write_csr(csr, wvalue) ~= false
                    }
                    2 => {
                        -- Read & Set
                        if rs1 == 0 then
                            -- no write required
                            valid_instruction = true
                        else
                            valid_instruction = cpu:write_csr(csr, $bor(rvalue, wvalue)) ~= false
                        end
                    }
                    3 => {
                        -- Read & Clear
                        if rs1 == 0 then
                            -- no write required
                            valid_instruction = true
                        else
                            valid_instruction = cpu:write_csr(csr, $band(rvalue, $bnot(wvalue))) ~= false
                        end
                    }
                }
                if cpu.had_exception then
                    goto abort_instruction
                end
                if rd ~= 0 and valid_instruction then
                    cpu.regs[rd] = rvalue
                    %trace(){
                        rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                    }
                end
            }
            -- 29 => reserved
            -- 30 => custom-3 (see bottom)
            -- 31 => 80-bit (or more) instruction
            2 => {
                if cpu.execute_custom0 then
                    cost = cost + cpu:execute_custom0(instruction) or 0
                    if cpu.had_exception then goto abort_instruction end
                    valid_instruction = true
                end
            }
            10 => {
                if cpu.execute_custom1 then
                    cost = cost + cpu:execute_custom1(instruction) or 0
                    if cpu.had_exception then goto abort_instruction end
                    valid_instruction = true
                end
            }
            22 => {
                if cpu.execute_custom2 then
                    cost = cost + cpu:execute_custom2(instruction) or 0
                    if cpu.had_exception then goto abort_instruction end
                    valid_instruction = true
                end
            }
            30 => {
                if cpu.execute_custom3 then
                    cost = cost + cpu:execute_custom3(instruction) or 0
                    if cpu.had_exception then goto abort_instruction end
                    valid_instruction = true
                end
            }
        }
    end
    ::instruction_legality_determined::
    if not valid_instruction then
        cpu:exception(rv32.EXC_ILLEGAL_INSTRUCTION, orig_instruction)
    else
        cpu.pc = new_pc
        cpu.instret_lo = cpu.instret_lo + 1
        if cpu.instret_lo >= 0x100000000 then
            cpu.instret_lo = 0 -- :)
            cpu.instret_hi = (cpu.instret_hi + 1) % 0x100000000
        end
    end
    ::abort_instruction::
        cpu.budget = cpu.budget - cost
    end
end

return rv32
