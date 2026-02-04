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
   local shift = bit32.lshift(bit32.band(addr,3),3)
   return bit32.band(bit32.rshift(cpu:read_word(bit32.band(addr,4294967292)),shift),255)
end

function rv32.write_byte(cpu, addr, byte)
   local shift = bit32.lshift(bit32.band(addr,3),3)
   byte = bit32.bor(byte,bit32.lshift(byte,16))
   byte = bit32.bor(byte,bit32.lshift(byte,8))
   cpu:write_word(bit32.band(addr,4294967292), byte, bit32.lshift(0xFF,shift))
end

function rv32.read_halfword(cpu, addr, exception_type)
   if cpu.strict_alignment and bit32.btest(addr,1) then
      return cpu:exception(rv32.EXC_MISALIGNED_LOAD)
   elseif bit32.band(addr,3) == 3 then
      return bit32.bor(cpu:read_byte(addr),bit32.lshift(cpu:read_byte(bit32.band(addr+1,0xFFFFFFFF)),8))
   else
      local word = cpu:read_word(bit32.band(addr,4294967292), exception_type)
      local shift = bit32.lshift(bit32.band(addr,3),3)
      return bit32.band(bit32.rshift(word,shift),0xFFFF)
   end
end

function rv32.write_halfword(cpu, addr, value)
   if cpu.strict_alignment and bit32.btest(addr,1) then
      return cpu:exception(rv32.EXC_MISALIGNED_STORE)
   elseif bit32.band(addr,3) == 3 then
      cpu:write_byte(addr, bit32.band(value,255))
      cpu:write_byte(addr+1, bit32.rshift(value,8))
   else
      local shift = bit32.lshift(bit32.band(addr,3),3)
      cpu:write_word(bit32.band(addr,4294967292), bit32.lshift(value,shift), bit32.lshift(0xFFFF,shift))
   end
end

function rv32.run(cpu, num_cycles)
   if num_cycles then
      cpu.budget = cpu.budget + num_cycles
   else
      cpu.budget = 1
   end
   repeat
      local cost = 1
      local valid_instruction = false
      local orig_instruction
      local old_pc = cpu.pc
      local new_pc = nil
      do
         cpu.had_exception = nil
         local instruction
         if not cpu.c_enabled then
            if bit32.btest(old_pc,3) then
               error("PC got a misaligned value with C disabled! THIS IS A BUG IN THE EMBEDDING PROGRAM!")
            end
            orig_instruction = cpu:read_word(old_pc, 1)
            if cpu.had_exception then
               goto continue
            end
            instruction = orig_instruction
            new_pc = old_pc + 4
            rv32trace(cpu,true)
            if bit32.band(orig_instruction,3) == 3 then
               rv32trace(cpu,("EXECUTE @ %08X: %08X"):format(old_pc, orig_instruction))
            else
               rv32trace(cpu,("EXECUTE @ %08X: %04X"):format(old_pc, bit32.band(orig_instruction,0xFFFF)))
            end
            rv32trace(cpu,false)
         else
            if bit32.btest(old_pc,2) then
               orig_instruction = cpu:read_halfword(old_pc, 1)
               if cpu.had_exception then
                  goto continue
               end
               if bit32.band(orig_instruction,3) == 3 then
                  cost = cost + 1
                  orig_instruction = bit32.bor(orig_instruction,bit32.lshift(cpu:read_halfword(old_pc+2,true),16))
                  if cpu.had_exception then
                     goto continue
                  end
               end
            else
               orig_instruction = cpu:read_word(old_pc, 1)
               if cpu.had_exception then
                  goto continue
               end
            end
            rv32trace(cpu,true)
            if bit32.band(orig_instruction,3) == 3 then
               rv32trace(cpu,("EXECUTE @ %08X: %08X"):format(old_pc, orig_instruction))
            else
               rv32trace(cpu,("EXECUTE @ %08X: %04X"):format(old_pc, bit32.band(orig_instruction,0xFFFF)))
            end
            rv32trace(cpu,false)
            if bit32.band(orig_instruction,3) == 3 then
               new_pc = old_pc + 4
               instruction = orig_instruction
            else
               new_pc = old_pc + 2
               orig_instruction = bit32.band(orig_instruction,0xFFFF)
               local bitsy = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,13),7),2),bit32.band(bit32.rshift(orig_instruction,0),3))
               --begin machine generated code (sorry)
               if bitsy <= 10 then
                  if bitsy <= 2 then
                     if bitsy <= 0 then
                        if bitsy == 0 then
                           -- C.ADDI4SPN (add unsigned immediate to stack pointer)
                           local offset = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,11),3),4),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,7),15),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),2),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),1),3))
                           if offset == 0 then
                              goto instruction_legality_determined
                           end
                           local rd = bit32.band(bit32.rshift(orig_instruction,2),7)+8
                           rv32trace(cpu,("C.ADDI4SPN rd:%i=%08X offset:%i"):format(rd, cpu.regs[rd], offset))
                           cpu.regs[rd] = bit32.band(cpu.regs[2]+offset,4294967295)
                           rv32trace(cpu,"r%i := %08X", rd, cpu.regs[rd])
                           valid_instruction = true
                           goto instruction_legality_determined
                        end
                     else
                        if bitsy <= 1 then
                           -- C.ADDI
                           local rd = bit32.band(bit32.rshift(orig_instruction,7),31)
                           local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFFE0 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),5),bit32.band(bit32.rshift(orig_instruction,2),31))
                           rv32trace(cpu,("C.ADDI rd:%i=%08X imm:%08X"):format(rd, cpu.regs[rd], imm))
                           if rd ~= 0 then
                              cpu.regs[rd] = bit32.band(cpu.regs[rd]+imm,4294967295)
                              rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        else
                           -- C.SLLI
                           if bit32.btest(orig_instruction,0x1000) then
                              goto instruction_legality_determined
                           end
                           local amt = bit32.band(bit32.rshift(orig_instruction,2),31)
                           local rd = bit32.band(bit32.rshift(orig_instruction,7),31)
                           rv32trace(cpu,("C.SLLI rd:%i=%08X amt=%i"):format(rd, cpu.regs[rd], amt))
                           cpu.regs[rd] = bit32.lshift(cpu.regs[rd],amt)
                           rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                           valid_instruction = true
                           goto instruction_legality_determined
                        end
                     end
                  else
                     if bitsy <= 8 then
                        if bitsy <= 5 then
                           if bitsy == 5 then
                              -- C.JAL
                              -- to quote the original comment from rrv32:
                              -- "what the"
                              local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFF800 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),11),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,11),1),4),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,9),3),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,8),1),10),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,7),1),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),7),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),7),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5))
                              cpu.regs[1] = new_pc
                              local target = bit32.band(old_pc + imm,4294967295)
                              rv32trace(cpu,("C.JAL imm:%i"):format(imm))
                              rv32trace(cpu,("pc := %08X"):format(target))
                              -- we're in C mode so the PC can't misalign
                              new_pc = target
                              valid_instruction = true
                              goto instruction_legality_determined
                           end
                        else
                           if bitsy == 8 then
                              -- C.LW
                              local offset = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),7),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),2),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),1),6))
                              local rs1 = bit32.band(bit32.rshift(orig_instruction,7),7)+8
                              local rd = bit32.band(bit32.rshift(orig_instruction,2),7)+8
                              rv32trace(cpu,("C.LW rd:%i rs1:%i offset:%i"):format(rd, rs1, offset))
                              instruction = bit32.bor(0x2003,bit32.lshift(rs1,15),bit32.lshift(rd,7),bit32.lshift(offset,20))
                           end
                        end
                     else
                        if bitsy <= 9 then
                           -- C.LI
                           local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFFE0 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),5),bit32.band(bit32.rshift(orig_instruction,2),31))
                           local rd = bit32.band(bit32.rshift(orig_instruction,7),31)
                           rv32trace(cpu,("C.LI rd:%i imm:%i"):format(rd, imm))
                           if rd ~= 0 then
                              cpu.regs[rd] = imm
                              rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        else
                           -- C.LWSP
                           local rd = bit32.band(bit32.rshift(orig_instruction,7),31)
                           if rd == 0 then
                              goto instruction_legality_determined
                           end
                           local offset = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),5),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,4),7),2),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),3),6))
                           instruction = bit32.bor(0x2003,bit32.lshift(offset,20),bit32.lshift(rd,7),bit32.lshift(2,15))
                           rv32trace(cpu,("C.LWSP rd=%i offset=%i"):format(rd, offset))
                        end
                     end
                  end
               else
                  if bitsy <= 21 then
                     if bitsy <= 17 then
                        if bitsy <= 13 then
                           if bitsy == 13 then
                              local rd = bit32.band(bit32.rshift(orig_instruction,7),31)
                              if rd == 2 then
                                 -- C.ADDI16SP!!!
                                 local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFE00 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),9),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),4),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),1),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),3),7),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5))
                                 rv32trace(cpu,("C.ADDI16SP imm:%i"):format(imm))
                                 rv32trace(cpu,("r2 == %08X"):format(cpu.regs[2]))
                                 cpu.regs[2] = bit32.band(cpu.regs[2] + imm,4294967295)
                                 rv32trace(cpu,("r2 := %08X"):format(cpu.regs[2]))
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              else
                                 -- C.LUI
                                 local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFE0000 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),17),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),31),12))
                                 if imm == 0 then
                                    -- bad instruction!
                                    goto instruction_legality_determined
                                 end
                                 rv32trace(cpu,("C.LUI rd:%i imm:%i"):format(rd, imm))
                                 if rd ~= 0 then
                                    cpu.regs[rd] = imm
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                 end
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              end
                           end
                        else
                           if bitsy == 17 then
                              -- some arithmetic operations
                              local funct2 = bit32.band(bit32.rshift(orig_instruction,10),3)
                              local rd = bit32.band(bit32.rshift(orig_instruction,7),7)+8
                              --begin machine generated code (sorry)
                              if funct2 <= 1 then
                                 if funct2 <= 0 then
                                    if funct2 == 0 then
                                       -- SRLI
                                       if bit32.btest(orig_instruction,0x1000) then
                                          goto instruction_legality_determined
                                       end
                                       local amt = bit32.band(bit32.rshift(orig_instruction,2),31)
                                       rv32trace(cpu,("C.SRLI rd:%i=%08X shamt:%i"):format(rd, cpu.regs[rd], amt))
                                       cpu.regs[rd] = bit32.rshift(cpu.regs[rd],amt)
                                       rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                       valid_instruction = true
                                       goto instruction_legality_determined
                                    end
                                 else
                                    -- SRAI
                                    if bit32.btest(orig_instruction,0x1000) then
                                       goto instruction_legality_determined
                                    end
                                    local amt = bit32.band(bit32.rshift(orig_instruction,2),31)
                                    rv32trace(cpu,("C.SRAI rd:%i=%08X shamt:%i"):format(rd, cpu.regs[rd], amt))
                                    cpu.regs[rd] = bit32.arshift(cpu.regs[rd],amt)
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                    valid_instruction = true
                                    goto instruction_legality_determined
                                 end
                              else
                                 if funct2 <= 2 then
                                    -- ANDI
                                    local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFFE0 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),5),bit32.band(bit32.rshift(orig_instruction,2),31))
                                    rv32trace(cpu,("C.ANDI rd:%i=%08X imm=%08X"):format(rd, cpu.regs[rd], imm))
                                    cpu.regs[rd] = bit32.band(cpu.regs[rd],imm)
                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                    valid_instruction = true
                                    goto instruction_legality_determined
                                 else
                                    -- and the rest~
                                    local op = bit32.band(bit32.rshift(orig_instruction,5),3)
                                    local rs2 = bit32.band(bit32.rshift(orig_instruction,2),7)+8
                                    rv32trace(cpu,("C.OP rd:%i=%08X rs2:%i=%08X op:%i"):format(rd, cpu.regs[rd], rs2, cpu.regs[rs2], op))
                                    --begin machine generated code (sorry)
                                    if op <= 1 then
                                       if op <= 0 then
                                          if op == 0 then
                                             -- SUB
                                             cpu.regs[rd] = bit32.band(cpu.regs[rd]-cpu.regs[rs2],0xFFFFFFFF)
                                          end
                                       else
                                          -- XOR
                                          cpu.regs[rd] = bit32.bxor(cpu.regs[rd],cpu.regs[rs2])
                                       end
                                    else
                                       if op <= 2 then
                                          -- OR
                                          cpu.regs[rd] = bit32.band(bit32.bor(cpu.regs[rd],cpu.regs[rs2]),0xFFFFFFFF)
                                       else
                                          -- AND
                                          cpu.regs[rd] = bit32.band(cpu.regs[rd],cpu.regs[rs2])
                                       end
                                    end
                                    --end machine generated code


                                    rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                                    valid_instruction = true
                                    goto instruction_legality_determined
                                 end
                              end
                              --end machine generated code


                           end
                        end
                     else
                        if bitsy <= 18 then
                           -- nightmare
                           local twelve = bit32.band(bit32.rshift(orig_instruction,12),1)
                           local rs1 = bit32.band(bit32.rshift(orig_instruction,7),31)
                           local rs2 = bit32.band(bit32.rshift(orig_instruction,2),31)
                           if twelve == 0 then
                              if rs2 == 0 then
                                 -- C.JR
                                 if rs1 == 0 then
                                    goto instruction_legality_determined
                                 end
                                 rv32trace(cpu,("C.JR rs1=%i:%08X"):format(rs1, cpu.regs[rs1]))
                                 new_pc = bit32.band(cpu.regs[rs1],4294967294)
                                 -- we're in C mode so the PC can't misalign
                              else
                                 -- C.MV
                                 -- rs2 cannot be zero
                                 rv32trace(cpu,("C.MV rd=%i rs2=%i:%08X"):format(rs1, rs2, cpu.regs[rs2]))
                                 if rs1 ~= 0 then
                                    cpu.regs[rs1] = cpu.regs[rs2]
                                    rv32trace(cpu,("r%i := %08X"):format(rs1, cpu.regs[rs1]))
                                 end
                              end
                              valid_instruction = true
                              goto instruction_legality_determined
                           elseif rs2 == 0 then
                              if rs1 == 0 then
                                 -- C.EBREAK
                                 instruction = 0x8003B
                                 rv32trace(cpu,"C.EBREAK")
                              else
                                 -- C.JALR
                                 local destination = cpu.regs[rs1]
                                 rv32trace(cpu,("C.JALR rs1:%i=%08X"):format(rs1, destination))
                                 cpu.regs[1] = new_pc
                                 new_pc = bit32.band(destination,4294967294)
                                 -- we're in C mode so the PC can't misalign
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              end
                           else
                              -- C.ADD
                              rv32trace(cpu,("C.ADD rs1:%i=%08X rs2:%i=%08X"):format(rs1, cpu.regs[rs1], rs2, cpu.regs[rs2]))
                              instruction = bit32.bor(0x33,bit32.lshift(rs1,7),bit32.lshift(rs1,15),bit32.lshift(rs2,20))
                           end
                        else
                           if bitsy == 21 then
                              -- C.J
                              local imm = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFF800 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),11),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,11),1),4),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,9),3),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,8),1),10),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,7),1),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),7),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),7),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5))
                              local target = bit32.band(old_pc + imm,4294967294)
                              rv32trace(cpu,("C.J imm=%08X"):format(imm))
                              rv32trace(cpu,("pc := %08X"):format(target))
                              -- we're in C mode so the PC can't misalign
                              new_pc = target
                              valid_instruction = true
                              goto instruction_legality_determined
                           end
                        end
                     end
                  else
                     if bitsy <= 25 then
                        if bitsy <= 24 then
                           if bitsy == 24 then
                              -- C.SW
                              local offset = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),7),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,6),1),2),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),1),6))
                              local rs1 = bit32.band(bit32.rshift(orig_instruction,7),7)+8
                              local rs2 = bit32.band(bit32.rshift(orig_instruction,2),7)+8
                              rv32trace(cpu,("C.SW rs1:%i rs2:%i offset:%i"):format(rs1, rs2, offset))
                              instruction = bit32.bor(0x2023,bit32.lshift(rs1,15),bit32.lshift(rs2,20),bit32.lshift(bit32.rshift(offset,5),25),bit32.lshift(bit32.band(offset,31),7))
                           end
                        else
                           -- C.BEQZ
                           local rs1 = bit32.band(bit32.rshift(orig_instruction,7),7)+8
                           local target = bit32.band(old_pc + bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFF00 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),3),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),3),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),3),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5)),4294967294)
                           rv32trace(cpu,("C.BEQZ rs1=%i:%08X target=%08X"):format(rs1, cpu.regs[rs1], target))
                           if cpu.regs[rs1] == 0 then
                              local offset = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFF00 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),3),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),3),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),3),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5))
                              new_pc = bit32.band(old_pc + offset,4294967294)
                              -- we're in C mode so the PC can't misalign
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        end
                     else
                        if bitsy <= 26 then
                           -- C.SWSP
                           local rd = bit32.band(bit32.rshift(orig_instruction,2),31)
                           local offset = bit32.bor(bit32.lshift(bit32.band(bit32.rshift(orig_instruction,9),15),2),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,7),3),6))
                           instruction = bit32.bor(0x2023,bit32.lshift(rd,20),bit32.lshift(2,15),bit32.lshift(bit32.rshift(offset,5),25),bit32.lshift(bit32.band(offset,31),7))
                           rv32trace(cpu,("C.SWSP rd=%i offset=%i"):format(rd, offset))
                        else
                           if bitsy == 29 then
                              -- C.BNEZ
                              local rs1 = bit32.band(bit32.rshift(orig_instruction,7),7)+8
                              local target = bit32.band(old_pc + bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFF00 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),3),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),3),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),3),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5)),4294967294)
                              rv32trace(cpu,("C.BNEZ rs1=%i:%08X target=%08X"):format(rs1, cpu.regs[rs1], target))
                              if cpu.regs[rs1] ~= 0 then
                                 local offset = bit32.bor(bit32.btest(orig_instruction,4096) and 0xFFFFFF00 or 0,bit32.lshift(bit32.band(bit32.rshift(orig_instruction,12),1),8),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,10),3),3),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,5),3),6),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,3),3),1),bit32.lshift(bit32.band(bit32.rshift(orig_instruction,2),1),5))
                                 new_pc = bit32.band(old_pc + offset,4294967294)
                                 -- we're in C mode so the PC can't misalign
                              end
                              valid_instruction = true
                              goto instruction_legality_determined
                           end
                        end
                     end
                  end
               end
               --end machine generated code

               if not instruction then
                  goto instruction_legality_determined
               end
            end
         end
         -- if we get here with a C instruction, it failed to decompress
         -- this check should only be reachable outside of C mode
         if bit32.band(instruction,3) ~= 3 then
            cpu:exception(rv32.EXC_ILLEGAL_INSTRUCTION, orig_instruction)
            goto continue
         end
         local opcode = bit32.band(bit32.rshift(instruction,2),31)
         --rv32trace(cpu,opcode)
         --begin machine generated code (sorry)
         if opcode <= 11 then
            if opcode <= 4 then
               if opcode <= 2 then
                  if opcode <= 0 then
                     if opcode == 0 then
                        -- load
                        cpu.reserved_address = nil
                        cost = cost + 1
                        local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                        local imm = bit32.arshift(instruction,20)
                        local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                        local addr = bit32.band(cpu.regs[rs1] + imm,0xFFFFFFFF)
                        rv32trace(cpu,("LOAD rs1:%i=%08X imm:%08X funct3:%i addr:%08X"):format(rs1, cpu.regs[rs1], bit32.band(imm,0xFFFFFFFF), funct3, addr))
                        local result
                        --begin machine generated code (sorry)
                        if funct3 <= 1 then
                           if funct3 <= 0 then
                              if funct3 == 0 then
                                 result = cpu:read_byte(addr)
                                 if cpu.had_exception then goto continue end
                                 result = bit32.bor(result,bit32.btest(result,0x80) and 0xFFFFFF00 or 0)
                              end
                           else
                              if bit32.band(addr,3) == 3 then
                                 cost = cost + 1
                              end
                              result = cpu:read_halfword(addr)
                              if cpu.had_exception then goto continue end
                              result = bit32.bor(result,bit32.btest(result,0x8000) and 0xFFFF0000 or 0)
                           end
                        else
                           if funct3 <= 2 then
                              if bit32.btest(addr,3) then
                                 cost = cost + 1
                              end
                              result = cpu:read_word(addr)
                              if cpu.had_exception then goto continue end
                           else
                              if funct3 <= 4 then
                                 if funct3 == 4 then
                                    result = cpu:read_byte(addr)
                                    if cpu.had_exception then goto continue end
                                 end
                              else
                                 if bit32.band(addr,3) == 3 then
                                    cost = cost + 1
                                 end
                                 result = cpu:read_halfword(addr)
                                 if cpu.had_exception then goto continue end
                              end
                           end
                        end
                        --end machine generated code

                        if cpu.had_exception then
                           goto continue
                        end
                        if result ~= nil then
                           local rd = bit32.band(bit32.rshift(instruction,7),31)
                           rv32trace(cpu,("r%i := %08X"):format(rd, result))
                           if rd ~= 0 then cpu.regs[rd] = result end
                           valid_instruction = true
                        end
                     end
                  else
                     if opcode == 2 then
                        if cpu.execute_custom0 then
                           cost = cost + cpu:execute_custom0(instruction) or 0
                           if cpu.had_exception then goto continue end
                           valid_instruction = true
                        end
                     end
                  end
               else
                  if opcode <= 3 then
                     local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                     -- misc-mem
                     --begin machine generated code (sorry)
                     if funct3 <= 0 then
                        if funct3 == 0 then
                           -- FENCE (implemented as no-op)
                           valid_instruction = true
                        end
                     else

                        -- IFENCE.I (implemented as no-op)
                        valid_instruction = true
                     end
                     --end machine generated code


                  else
                     -- op (immediate)
                     local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                     local a = cpu.regs[rs1]
                     local b = bit32.arshift(instruction,20)
                     local result
                     local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     rv32trace(cpu,("OP-I rd:%i rs1:%i=%08X imm:%08X funct3:%i"):format(rd, rs1, a, bit32.band(b,0xFFFFFFFF), funct3))
                     --begin machine generated code (sorry)
                     if funct3 <= 3 then
                        if funct3 <= 1 then
                           if funct3 <= 0 then
                              if funct3 == 0 then
                                 -- ADDI
                                 result = bit32.band(a+b,0xFFFFFFFF)
                              end
                           else
                              -- SLLI
                              if bit32.band(bit32.rshift(instruction,25),127) == 0 then
                                 result = bit32.lshift(a,bit32.band(b,31))
                              end
                           end
                        else
                           if funct3 <= 2 then
                              -- SLTI
                              result = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0)) < ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0)) and 1 or 0
                           else
                              -- SLTIU
                              result = a < b and 1 or 0
                           end
                        end
                     else
                        if funct3 <= 5 then
                           if funct3 <= 4 then
                              -- XORI
                              result = bit32.bxor(a,b)
                           else
                              -- SRLI/SRAI
                              local funct7 = bit32.band(bit32.rshift(instruction,25),127)
                              if funct7 == 0x20 then
                                 -- SRAI
                                 result = bit32.arshift(a,bit32.band(b,31))
                              elseif funct7 == 0x00 then
                                 -- SRLI
                                 result = bit32.rshift(a,bit32.band(b,31))
                              end
                           end
                        else
                           if funct3 <= 6 then
                              -- ORI
                              result = bit32.bor(a,b)
                           else
                              -- ANDI
                              result = bit32.band(a,b)
                           end
                        end
                     end
                     --end machine generated code

                     if result ~= nil then
                        rv32trace(cpu,("r%i := %08X"):format(rd, result))
                        if rd ~= 0 then cpu.regs[rd] = result end
                        valid_instruction = true
                     end
                  end
               end
            else
               if opcode <= 8 then
                  if opcode <= 5 then
                     -- auipc = add upper immediate to pc
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     local ui = bit32.band(instruction,bit32.bnot(0xFFF))
                     rv32trace(cpu,("AUIPC r%i := pc + %08X"):format(rd, bit32.band(ui,0xFFFFFFFF)))
                     if rd ~= 0 then
                        cpu.regs[rd] = bit32.band(old_pc + ui,0xFFFFFFFF)
                     end
                     valid_instruction = true
                  else
                     if opcode == 8 then
                        -- store
                        cpu.reserved_address = nil
                        cost = cost + 1
                        local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                        local rs2 = bit32.band(bit32.rshift(instruction,20),31)
                        local value = cpu.regs[rs2]
                        local imm = bit32.bor(bit32.band(bit32.arshift(instruction,20),0xFFFFFFE0),bit32.band(bit32.rshift(instruction,7),31))
                        local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                        local addr = bit32.band(cpu.regs[rs1] + imm,0xFFFFFFFF)
                        rv32trace(cpu,("STORE rs1:%i=%08X rs2:%i=%08X imm:%08X funct3:%i addr:%08X"):format(rs1, cpu.regs[rs1], rs2, cpu.regs[rs2], bit32.band(imm,0xFFFFFFFF), funct3, addr))
                        --begin machine generated code (sorry)
                        if funct3 <= 0 then
                           if funct3 == 0 then
                              cpu:write_byte(addr, bit32.band(value,255))
                              if cpu.had_exception then goto continue end
                              valid_instruction = true
                           end
                        else
                           if funct3 <= 1 then
                              if bit32.band(addr,3) == 3 then
                                 cost = cost + 1
                              end
                              cpu:write_halfword(addr, bit32.band(value,65535))
                              if cpu.had_exception then goto continue end
                              valid_instruction = true
                           else
                              if bit32.btest(addr,3) then
                                 cost = cost + 1
                              end
                              cpu:write_word(addr, value, 0xFFFFFFFF)
                              if cpu.had_exception then goto continue end
                              valid_instruction = true
                           end
                        end
                        --end machine generated code

                        if cpu.had_exception then
                           goto continue
                        end
                     end
                  end
               else
                  if opcode <= 10 then
                     if opcode == 10 then
                        if cpu.execute_custom1 then
                           cost = cost + cpu:execute_custom1(instruction) or 0
                           if cpu.had_exception then goto continue end
                           valid_instruction = true
                        end
                     end
                  else
                     if not cpu.a_enabled then goto bad_amo end do
                        -- aq/rl ignored because we don't have caches or snooping...
                        local funct5 = bit32.band(bit32.rshift(instruction,27),31)
                        local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                        local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                        local rs2 = bit32.band(bit32.rshift(instruction,20),31)
                        local rd = bit32.band(bit32.rshift(instruction,7),31)
                        if funct3 ~= 2 then
                           goto bad_amo
                        end
                        local addr = cpu.regs[rs1]
                        if bit32.btest(addr,3) then
                           cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
                           goto continue
                        end
                        if funct5 ~= 3 then
                           -- no AMO operation other than SC will ever preserve the
                           -- reservation
                           cpu.reserved_address = nil
                        end
                        local amo_op
                        --begin machine generated code (sorry)
                        if funct5 <= 4 then
                           if funct5 <= 1 then
                              if funct5 <= 0 then
                                 if funct5 == 0 then
                                    amo_op = function(mem,reg) return bit32.band(mem+reg,0xFFFFFFFF) end
                                 end
                              else
                                 amo_op = function(mem,reg) return reg end
                              end
                           else
                              if funct5 <= 2 then
                                 -- LR.W - load reserved word
                                 if rs2 ~= 0 then goto bad_amo end
                                 local value = cpu:read_word(addr)
                                 if cpu.had_exception then
                                    goto continue
                                 end
                                 if rd ~= 0 then
                                    cpu.regs[rd] = value
                                 end
                                 cpu.reserved_address = addr
                                 valid_instruction = true
                              else
                                 if funct5 <= 3 then
                                    -- SC.W - store conditional word
                                    if addr == cpu.reserved_address then
                                       local value = cpu.regs[rs2]
                                       cpu:write_word(addr, value, 0xFFFFFFFF)
                                       if cpu.had_exception then goto continue end
                                       if rd ~= 0 then
                                          cpu.regs[rd] = 0
                                       end
                                    else
                                       if rd ~= 0 then
                                          cpu.regs[rd] = 1 -- unspecified failure
                                       end
                                    end
                                    valid_instruction = true
                                 else
                                    amo_op = bit32.bxor
                                 end
                              end
                           end
                        else
                           if funct5 <= 16 then
                              if funct5 <= 8 then
                                 if funct5 == 8 then
                                    amo_op = bit32.bor
                                 end
                              else
                                 if funct5 <= 12 then
                                    if funct5 == 12 then
                                       amo_op = bit32.band
                                    end
                                 else
                                    if funct5 == 16 then
                                       amo_op = function(mem,reg)
                                          mem = ((mem) - (bit32.btest(mem,0x80000000) and 0x100000000 or 0))
                                          reg = ((reg) - (bit32.btest(reg,0x80000000) and 0x100000000 or 0))
                                          return bit32.band(math.min(mem, reg),0xFFFFFFFF)
                                       end
                                    end
                                 end
                              end
                           else
                              if funct5 <= 20 then
                                 if funct5 == 20 then
                                    amo_op = function(mem,reg)
                                       mem = ((mem) - (bit32.btest(mem,0x80000000) and 0x100000000 or 0))
                                       reg = ((reg) - (bit32.btest(reg,0x80000000) and 0x100000000 or 0))
                                       return bit32.band(math.max(mem, reg),0xFFFFFFFF)
                                    end
                                 end
                              else
                                 if funct5 <= 24 then
                                    if funct5 == 24 then
                                       amo_op = math.min
                                    end
                                 else
                                    if funct5 == 28 then
                                       amo_op = math.max
                                    end
                                 end
                              end
                           end
                        end
                        --end machine generated code

                        if amo_op then
                           local mem_value = cpu:read_word(addr, 7)
                           if cpu.had_exception then
                              goto continue
                           end
                           local reg_value = cpu.regs[rs2]
                           local write_value = amo_op(mem_value, reg_value)
                           cpu:write_word(addr, write_value, 0xFFFFFFFF)
                           if cpu.had_exception then
                              goto continue
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
                  end
               end
            end
         else
            if opcode <= 24 then
               if opcode <= 13 then
                  if opcode <= 12 then
                     -- op (register-to-register)
                     local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                     local a = cpu.regs[rs1]
                     local rs2 = bit32.band(bit32.rshift(instruction,20),31)
                     local b = cpu.regs[rs2]
                     local result
                     local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                     local funct7 = bit32.band(bit32.rshift(instruction,25),127)
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     rv32trace(cpu,("OP rd:%i rs1:%i=%08X rs2:%i=%08X funct3:%i funct7:%i"):format(rd, rs1, a, rs2, b, funct3, funct7))
                     local funct37 = bit32.bor(bit32.lshift(funct7,3),funct3)
                     --begin machine generated code (sorry)
                     if funct37 <= 8 then
                        if funct37 <= 3 then
                           if funct37 <= 1 then
                              if funct37 <= 0 then
                                 if funct37 == 0 then
                                    -- ADD
                                    result = bit32.band(a+b,0xFFFFFFFF)
                                 end
                              else
                                 -- SLL
                                 result = bit32.lshift(a,bit32.band(b,31))
                              end
                           else
                              if funct37 <= 2 then
                                 -- SLT
                                 result = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0)) < ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0)) and 1 or 0
                              else
                                 -- SLTU
                                 result = a < b and 1 or 0
                              end
                           end
                        else
                           if funct37 <= 5 then
                              if funct37 <= 4 then
                                 -- XOR
                                 result = bit32.bxor(a,b)
                              else
                                 -- SRL
                                 result = bit32.rshift(a,bit32.band(b,31))
                              end
                           else
                              if funct37 <= 6 then
                                 -- OR
                                 result = bit32.bor(a,b)
                              else
                                 if funct37 <= 7 then
                                    -- AND
                                    result = bit32.band(a,b)
                                 else
                                    -- MUL
                                    if cpu.m_enabled then
                                       cost = cost + 3
                                       local low_result = (bit32.band(a,0xFFFF)*b) % 0x100000000
                                       local high_result = (bit32.band(a,0xFFFF0000)*b) % 0x100000000
                                       result = low_result + high_result
                                    end
                                 end
                              end
                           end
                        end
                     else
                        if funct37 <= 12 then
                           if funct37 <= 10 then
                              if funct37 <= 9 then
                                 -- MULH
                                 if cpu.m_enabled then
                                    result = 0
                                    local sign = 1
                                    a = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0))
                                    b = ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0))
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
                                    result = bit32.band(result,4294967295)
                                 end
                              else
                                 -- MULHSU
                                 if cpu.m_enabled then
                                    result = 0
                                    a = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0))
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
                                    result = bit32.band(result,4294967295)
                                 end
                              end
                           else
                              if funct37 <= 11 then
                                 -- MULHU
                                 if cpu.m_enabled then
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
                                 end
                              else
                                 -- DIV
                                 if cpu.m_enabled then
                                    cost = cost + 5
                                    if b == 0 then
                                       result = 0xFFFFFFFF
                                    elseif a == 0x80000000 and b == 0xFFFFFFFF then
                                       result = 0x80000000
                                    else
                                       result = (((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0)) / ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0)))
                                       if result < 0 then
                                          result = math.ceil(result)
                                       else
                                          result = math.floor(result)
                                       end
                                       result = bit32.band(result,0xFFFFFFFF)
                                    end
                                 end
                              end
                           end
                        else
                           if funct37 <= 14 then
                              if funct37 <= 13 then
                                 -- DIVU
                                 if cpu.m_enabled then
                                    cost = cost + 5
                                    if b == 0 then
                                       result = 0xFFFFFFFF
                                    else
                                       result = bit32.band(math.floor(a/b),0xFFFFFFFF)
                                    end
                                 end
                              else
                                 -- REM
                                 if cpu.m_enabled then
                                    cost = cost + 5
                                    if b == 0 then
                                       result = a
                                    elseif a == 0x80000000 and b == 0xFFFFFFFF then
                                       result = 0
                                    else
                                       a = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0))
                                       b = ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0))
                                       local quotient = (a / b)
                                       if quotient < 0 then
                                          quotient = math.ceil(quotient)
                                       else
                                          quotient = math.floor(quotient)
                                       end
                                       result = a - (quotient * b)
                                       result = bit32.band(result,0xFFFFFFFF)
                                    end
                                 end
                              end
                           else
                              if funct37 <= 15 then
                                 -- REMU
                                 if cpu.m_enabled then
                                    cost = cost + 5
                                    if b == 0 then
                                       result = a
                                    else
                                       result = bit32.band(a%b,0xFFFFFFFF)
                                    end
                                 end
                              else
                                 if funct37 <= 256 then
                                    if funct37 == 256 then
                                       -- SUB
                                       result = bit32.band(a-b,0xFFFFFFFF)
                                    end
                                 else
                                    if funct37 == 261 then
                                       -- SRA
                                       result = bit32.arshift(a,bit32.band(b,31))
                                    end
                                 end
                              end
                           end
                        end
                     end
                     --end machine generated code

                     if result ~= nil then
                        rv32trace(cpu,("r%i := %08X"):format(rd, result))
                        if rd ~= 0 then cpu.regs[rd] = result end
                        valid_instruction = true
                     end
                  else
                     -- lui = load upper immediate
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     local ui = bit32.band(instruction,bit32.bnot(0xFFF))
                     rv32trace(cpu,("LUI r%i := %08X"):format(rd, bit32.band(ui,0xFFFFFFFF)))
                     if rd ~= 0 then
                        cpu.regs[rd] = ui
                     end
                     valid_instruction = true
                  end
               else
                  if opcode <= 22 then
                     if opcode == 22 then
                        if cpu.execute_custom2 then
                           cost = cost + cpu:execute_custom2(instruction) or 0
                           if cpu.had_exception then goto continue end
                           valid_instruction = true
                        end
                     end
                  else
                     if opcode == 24 then
                        -- various forms of branch
                        local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                        local rs2 = bit32.band(bit32.rshift(instruction,20),31)
                        local a = cpu.regs[rs1]
                        local b = cpu.regs[rs2]
                        local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                        local should_branch
                        rv32trace(cpu,("BRANCH rs1:%i=%08X rs2:%i=%08X funct3:%i imm:%08X"):format(rs1, a, rs2, b, funct3, bit32.bor(bit32.band(bit32.arshift(instruction,19),0xFFFFF000),bit32.lshift(bit32.band(bit32.rshift(instruction,7),1),11),bit32.lshift(bit32.band(bit32.rshift(instruction,25),63),5),bit32.lshift(bit32.band(bit32.rshift(instruction,8),15),1))))
                        --begin machine generated code (sorry)
                        if funct3 <= 4 then
                           if funct3 <= 0 then
                              if funct3 == 0 then
                                 should_branch = a == b
                              end
                           else
                              if funct3 <= 1 then
                                 should_branch = a ~= b
                              else
                                 if funct3 == 4 then
                                    should_branch = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0)) < ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0))
                                 end
                              end
                           end
                        else
                           if funct3 <= 5 then
                              should_branch = ((a) - (bit32.btest(a,0x80000000) and 0x100000000 or 0)) >= ((b) - (bit32.btest(b,0x80000000) and 0x100000000 or 0))
                           else
                              if funct3 <= 6 then
                                 should_branch = a < b
                              else
                                 should_branch = a >= b
                              end
                           end
                        end
                        --end machine generated code

                        valid_instruction = should_branch ~= nil
                        if should_branch then
                           local imm = bit32.bor(bit32.band(bit32.arshift(instruction,19),0xFFFFF000),bit32.lshift(bit32.band(bit32.rshift(instruction,7),1),11),bit32.lshift(bit32.band(bit32.rshift(instruction,25),63),5),bit32.lshift(bit32.band(bit32.rshift(instruction,8),15),1))
                           local target = bit32.band(old_pc + imm,4294967294)
                           if not cpu.c_enabled and bit32.btest(target,2) then
                              cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                              goto continue
                           else
                              new_pc = target
                           end
                        end
                     end
                  end
               end
            else
               if opcode <= 27 then
                  if opcode <= 25 then
                     -- jalr = jump (and link) to register
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                     local imm = bit32.arshift(instruction,20)
                     rv32trace(cpu,("JALR r%i := %08X, rs1:%i=%08X, imm=%08X"):format(rd, cpu.pc, rs1, cpu.regs[rs1], imm))
                     local target=bit32.band(cpu.regs[rs1]+imm,4294967294)
                     if rd ~= 0 then
                        cpu.regs[rd] = new_pc
                     end
                     if not cpu.c_enabled and bit32.btest(target,2) then
                        cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                        goto continue
                     else
                        new_pc = target
                     end
                     valid_instruction = true
                  else
                     if opcode == 27 then
                        -- jal = jump and link
                        local rd = bit32.band(bit32.rshift(instruction,7),31)
                        local imm = bit32.bor(bit32.band(bit32.arshift(instruction,11),0xFFF00000),bit32.band(instruction,0xFF000),bit32.lshift(bit32.band(bit32.rshift(instruction,20),1),11),bit32.lshift(bit32.band(bit32.rshift(instruction,21),1023),1))
                        rv32trace(cpu,("JAL r%i := %08X, imm=%08X"):format(rd, cpu.pc, imm))
                        if rd ~= 0 then
                           cpu.regs[rd] = new_pc
                        end
                        local target = bit32.band(old_pc + imm,4294967294)
                        if not cpu.c_enabled and bit32.btest(target,2) then
                           cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                           goto continue
                        else
                           new_pc = target
                        end
                        valid_instruction = true
                     end
                  end
               else
                  if opcode <= 28 then
                     -- system
                     local funct3 = bit32.band(bit32.rshift(instruction,12),7)
                     if funct3 == 0 or funct3 == 4 then
                        -- note: normally ecall/ebreak should not retire, but if
                        -- the caller wishes it, we let them retire
                        if instruction == 0x00000073 then
                           if cpu.execute_ecall then
                              cost = cost + (cpu:execute_ecall() or 0)
                              if cpu.had_exception then
                                 goto continue
                              end
                              valid_instruction = true
                           end
                        elseif instruction == 0x00100073 then
                           if cpu.execute_ebreak then
                              cost = cost + (cpu:execute_ebreak() or 0)
                              if cpu.had_exception then
                                 goto continue
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
                     local rd = bit32.band(bit32.rshift(instruction,7),31)
                     local rs1 = bit32.band(bit32.rshift(instruction,15),31)
                     local csr = bit32.band(bit32.rshift(instruction,20),4095)
                     local name,rs1name,rs1format
                     --begin machine generated code (sorry)
                     if funct3 <= 3 then
                        if funct3 <= 1 then
                           if funct3 == 1 then
                              name,rs1name,rs1format = "CSRRW","rs1","%i=%08X"
                           end
                        else
                           if funct3 <= 2 then
                              name,rs1name,rs1format = "CSRRS","rs1","%i=%08X"
                           else
                              name,rs1name,rs1format = "CSRRC","rs1","%i=%08X"
                           end
                        end
                     else
                        if funct3 <= 5 then
                           if funct3 == 5 then
                              name,rs1name,rs1format = "CSRRWI","imm","%i"
                           end
                        else
                           if funct3 <= 6 then
                              name,rs1name,rs1format = "CSRRSI","imm","%i"
                           else
                              name,rs1name,rs1format = "CSRRCI","imm","%i"
                           end
                        end
                     end
                     --end machine generated code

                     rv32trace(cpu,("%s csr:%i rd:%i %s:"..rs1format):format(name, csr, rd, rs1name, rs1, cpu.regs[rs1]))
                     local wvalue
                     if funct3 >= 4 then
                        -- I variant
                        wvalue = rs1
                     else
                        -- Non-I variant
                        wvalue = cpu.regs[rs1]
                     end
                     local funct2 = bit32.band(funct3,3)
                     local rvalue
                     if rd ~= 0 or funct2 ~= 1 then
                        rvalue = cpu:read_csr(csr)
                        if cpu.had_exception then
                           goto continue
                        end
                        if rvalue == nil then
                           goto instruction_legality_determined
                        end
                     end
                     --begin machine generated code (sorry)
                     if funct2 <= 1 then
                        if funct2 == 1 then
                           -- Read & Write
                           valid_instruction = cpu:write_csr(csr, wvalue) ~= false
                        end
                     else
                        if funct2 <= 2 then
                           -- Read & Set
                           if rs1 == 0 then
                              -- no write required
                              valid_instruction = true
                           else
                              valid_instruction = cpu:write_csr(csr, bit32.bor(rvalue,wvalue)) ~= false
                           end
                        else
                           -- Read & Clear
                           if rs1 == 0 then
                              -- no write required
                              valid_instruction = true
                           else
                              valid_instruction = cpu:write_csr(csr, bit32.band(rvalue,bit32.bnot(wvalue))) ~= false
                           end
                        end
                     end
                     --end machine generated code

                     if cpu.had_exception then
                        goto continue
                     end
                     if rd ~= 0 and valid_instruction then
                        cpu.regs[rd] = rvalue
                        rv32trace(cpu,("r%i := %08X"):format(rd, cpu.regs[rd]))
                     end
                  else
                     if opcode == 30 then
                        if cpu.execute_custom3 then
                           cost = cost + cpu:execute_custom3(instruction) or 0
                           if cpu.had_exception then goto continue end
                           valid_instruction = true
                        end
                     end
                  end
               end
            end
         end
         --end machine generated code

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
      ::continue::
      cpu.budget = cpu.budget - cost
   until cpu.budget <= 0
end

return rv32
