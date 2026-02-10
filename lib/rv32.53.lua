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



if rv32trace then
   (warn or print)("`rv32trace` was set to a non-nil value, but a non-tracing version of `lua-rv32` was loaded! Tracing will NOT take place!")
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
   local shift = (((((addr) & (3))) << (3)) & 0xFFFFFFFF)
   return ((((cpu:read_word(((addr) & (4294967292)))) >> (shift))) & (255))
end

function rv32.write_byte(cpu, addr, byte)
   local shift = (((((addr) & (3))) << (3)) & 0xFFFFFFFF)
   byte = ((byte)|((((byte) << (16)) & 0xFFFFFFFF)))
   byte = ((byte)|((((byte) << (8)) & 0xFFFFFFFF)))
   cpu:write_word(((addr) & (4294967292)), byte, (((0xFF) << (shift)) & 0xFFFFFFFF))
end

function rv32.read_halfword(cpu, addr, exception_type)
   if cpu.strict_alignment and (((addr) & (1)) ~= 0) then
      return cpu:exception(rv32.EXC_MISALIGNED_LOAD, addr)
   elseif ((addr) & (3)) == 3 then
      return ((cpu:read_byte(addr))|((((cpu:read_byte(((addr+1) & (0xFFFFFFFF)))) << (8)) & 0xFFFFFFFF)))
   else
      local word = cpu:read_word(((addr) & (4294967292)), exception_type)
      local shift = (((((addr) & (3))) << (3)) & 0xFFFFFFFF)
      return ((((word) >> (shift))) & (0xFFFF))
   end
end

function rv32.write_halfword(cpu, addr, value)
   if cpu.strict_alignment and (((addr) & (1)) ~= 0) then
      return cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
   elseif ((addr) & (3)) == 3 then
      cpu:write_byte(addr, ((value) & (255)))
      cpu:write_byte(addr+1, ((value) >> (8)))
   else
      local shift = (((((addr) & (3))) << (3)) & 0xFFFFFFFF)
      cpu:write_word(((addr) & (4294967292)), (((value) << (shift)) & 0xFFFFFFFF), (((0xFFFF) << (shift)) & 0xFFFFFFFF))
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
            if (((old_pc) & (3)) ~= 0) then
               error("PC got a misaligned value with C disabled! THIS IS A BUG IN THE EMBEDDING PROGRAM!")
            end
            -- C disabled: fetch an aligned instruction word
            orig_instruction = cpu:read_word(old_pc, 1)
            if cpu.had_exception then
               goto abort_instruction
            end
            instruction = orig_instruction
            new_pc = old_pc + 4
         else
            if (((old_pc) & (2)) ~= 0) then
               -- C enabled, PC half-aligned; start by reading one halfword...
               orig_instruction = cpu:read_halfword(old_pc, 1)
               if cpu.had_exception then
                  goto abort_instruction
               end
               if ((orig_instruction) & (3)) == 3 then
                  -- ..and if it's the lower half of a 32-bit instruction,
                  -- read the other half.
                  cost = cost + 1
                  orig_instruction = ((orig_instruction)|((((cpu:read_halfword(old_pc+2,true)) << (16)) & 0xFFFFFFFF)))
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
            if ((orig_instruction) & (3)) == 3 then
               -- 32-bit instruction
               new_pc = old_pc + 4
               instruction = orig_instruction
            else
               -- 16-bit instruction... try to either decode it into its
               -- equivalent 32-bit instruction, OR execute it directly if
               -- that would be awkward.
               new_pc = old_pc + 2
               orig_instruction = ((orig_instruction) & (0xFFFF))
               -- Extract a scrambled operation code...
               local bitsy = (((((((((orig_instruction) >> (13))) & (7))) << (2)) & 0xFFFFFFFF))|(((((orig_instruction) >> (0))) & (3))))
               --begin machine generated code (sorry)
               if bitsy <= 10 then
                  if bitsy <= 2 then
                     if bitsy <= 0 then
                        if bitsy == 0 then
                           -- C.ADDI4SPN (add unsigned immediate to stack pointer)
                           local offset = (((((((((orig_instruction) >> (11))) & (3))) << (4)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (7))) & (15))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (2)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (1))) << (3)) & 0xFFFFFFFF)))
                           if offset == 0 then
                              -- ADDI4SPN with zero offset is nonsense and
                              -- therefore illegal
                              goto instruction_legality_determined
                           end
                           local rd = ((((orig_instruction) >> (2))) & (7))+8
                           cpu.regs[rd] = ((cpu.regs[2]+offset) & (4294967295))
                           valid_instruction = true
                           goto instruction_legality_determined
                        end
                     else
                        if bitsy <= 1 then
                           -- C.ADDI
                           local rd = ((((orig_instruction) >> (7))) & (31))
                           local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFFE0 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (5)) & 0xFFFFFFFF))|(((((orig_instruction) >> (2))) & (31))))
                           if rd ~= 0 then
                              cpu.regs[rd] = ((cpu.regs[rd]+imm) & (4294967295))
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        else
                           -- C.SLLI
                           if (((orig_instruction) & (0x1000)) ~= 0) then
                              -- illegal bit pattern
                              goto instruction_legality_determined
                           end
                           local amt = ((((orig_instruction) >> (2))) & (31))
                           local rd = ((((orig_instruction) >> (7))) & (31))
                           cpu.regs[rd] = (((cpu.regs[rd]) << (amt)) & 0xFFFFFFFF)
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
                              local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFF800 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (11)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (11))) & (1))) << (4)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (9))) & (3))) << (8)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (8))) & (1))) << (10)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (7))) & (1))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (7)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (3))) & (7))) << (1)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (1))) << (5)) & 0xFFFFFFFF)))
                              cpu.regs[1] = new_pc
                              local target = ((old_pc + imm) & (4294967295))
                              -- we're in C mode so the PC can't misalign
                              new_pc = target
                              valid_instruction = true
                              goto instruction_legality_determined
                           end
                        else
                           if bitsy == 8 then
                              -- C.LW
                              local offset = (((((((((orig_instruction) >> (10))) & (7))) << (3)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (2)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (1))) << (6)) & 0xFFFFFFFF)))
                              local rs1 = ((((orig_instruction) >> (7))) & (7))+8
                              local rd = ((((orig_instruction) >> (2))) & (7))+8
                              instruction = ((0x2003)|((((rs1) << (15)) & 0xFFFFFFFF))|((((rd) << (7)) & 0xFFFFFFFF))|((((offset) << (20)) & 0xFFFFFFFF)))
                           end
                        end
                     else
                        if bitsy <= 9 then
                           -- C.LI
                           local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFFE0 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (5)) & 0xFFFFFFFF))|(((((orig_instruction) >> (2))) & (31))))
                           local rd = ((((orig_instruction) >> (7))) & (31))
                           if rd ~= 0 then
                              cpu.regs[rd] = imm
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        else
                           -- C.LWSP
                           local rd = ((((orig_instruction) >> (7))) & (31))
                           if rd == 0 then
                              goto instruction_legality_determined
                           end
                           local offset = (((((((((orig_instruction) >> (12))) & (1))) << (5)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (4))) & (7))) << (2)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (3))) << (6)) & 0xFFFFFFFF)))
                           instruction = ((0x2003)|((((offset) << (20)) & 0xFFFFFFFF))|((((rd) << (7)) & 0xFFFFFFFF))|((((2) << (15)) & 0xFFFFFFFF)))
                        end
                     end
                  end
               else
                  if bitsy <= 21 then
                     if bitsy <= 17 then
                        if bitsy <= 13 then
                           if bitsy == 13 then
                              local rd = ((((orig_instruction) >> (7))) & (31))
                              if rd == 2 then
                                 -- C.ADDI16SP!!!
                                 local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFE00 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (9)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (4)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (1))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (3))) & (3))) << (7)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (1))) << (5)) & 0xFFFFFFFF)))
                                 cpu.regs[2] = ((cpu.regs[2] + imm) & (4294967295))
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              else
                                 -- C.LUI
                                 local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFE0000 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (17)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (31))) << (12)) & 0xFFFFFFFF)))
                                 if imm == 0 then
                                    -- bad instruction!
                                    goto instruction_legality_determined
                                 end
                                 if rd ~= 0 then
                                    cpu.regs[rd] = imm
                                 end
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              end
                           end
                        else
                           if bitsy == 17 then
                              -- some arithmetic operations
                              local funct2 = ((((orig_instruction) >> (10))) & (3))
                              local rd = ((((orig_instruction) >> (7))) & (7))+8
                              --begin machine generated code (sorry)
                              if funct2 <= 1 then
                                 if funct2 <= 0 then
                                    if funct2 == 0 then
                                       -- SRLI
                                       if (((orig_instruction) & (0x1000)) ~= 0) then
                                          -- illegal bit pattern
                                          goto instruction_legality_determined
                                       end
                                       local amt = ((((orig_instruction) >> (2))) & (31))
                                       cpu.regs[rd] = ((cpu.regs[rd]) >> (amt))
                                       valid_instruction = true
                                       goto instruction_legality_determined
                                    end
                                 else
                                    -- SRAI
                                    if (((orig_instruction) & (0x1000)) ~= 0) then
                                       -- illegal bit pattern
                                       goto instruction_legality_determined
                                    end
                                    local amt = ((((orig_instruction) >> (2))) & (31))
                                    cpu.regs[rd] = ((((cpu.regs[rd]) >> (amt)) | ((((cpu.regs[rd]) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(amt))) or 0)) & 0xFFFFFFFF)
                                    valid_instruction = true
                                    goto instruction_legality_determined
                                 end
                              else
                                 if funct2 <= 2 then
                                    -- ANDI
                                    local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFFE0 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (5)) & 0xFFFFFFFF))|(((((orig_instruction) >> (2))) & (31))))
                                    cpu.regs[rd] = ((cpu.regs[rd]) & (imm))
                                    valid_instruction = true
                                    goto instruction_legality_determined
                                 else
                                    -- and the rest~
                                    local op = ((((orig_instruction) >> (5))) & (3))
                                    local rs2 = ((((orig_instruction) >> (2))) & (7))+8
                                    --begin machine generated code (sorry)
                                    if op <= 1 then
                                       if op <= 0 then
                                          if op == 0 then
                                             -- SUB
                                             cpu.regs[rd] = ((cpu.regs[rd]-cpu.regs[rs2]) & (0xFFFFFFFF))
                                          end
                                       else
                                          -- XOR
                                          cpu.regs[rd] = ((cpu.regs[rd]) ~ (cpu.regs[rs2]))
                                       end
                                    else
                                       if op <= 2 then
                                          -- OR
                                          cpu.regs[rd] = ((((cpu.regs[rd])|(cpu.regs[rs2]))) & (0xFFFFFFFF))
                                       else
                                          -- AND
                                          cpu.regs[rd] = ((cpu.regs[rd]) & (cpu.regs[rs2]))
                                       end
                                    end
                                    --end machine generated code


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
                           local twelve = ((((orig_instruction) >> (12))) & (1))
                           local rs1 = ((((orig_instruction) >> (7))) & (31))
                           local rs2 = ((((orig_instruction) >> (2))) & (31))
                           if twelve == 0 then
                              if rs2 == 0 then
                                 -- C.JR
                                 if rs1 == 0 then
                                    goto instruction_legality_determined
                                 end
                                 new_pc = ((cpu.regs[rs1]) & (4294967294))
                                 -- we're in C mode so the PC can't misalign
                              else
                                 -- C.MV
                                 -- rs2 cannot be zero
                                 if rs1 ~= 0 then
                                    cpu.regs[rs1] = cpu.regs[rs2]
                                 end
                              end
                              valid_instruction = true
                              goto instruction_legality_determined
                           elseif rs2 == 0 then
                              if rs1 == 0 then
                                 -- C.EBREAK
                                 instruction = 0x8003B
                              else
                                 -- C.JALR
                                 local destination = cpu.regs[rs1]
                                 cpu.regs[1] = new_pc
                                 new_pc = ((destination) & (4294967294))
                                 -- we're in C mode so the PC can't misalign
                                 valid_instruction = true
                                 goto instruction_legality_determined
                              end
                           else
                              -- C.ADD
                              instruction = ((0x33)|((((rs1) << (7)) & 0xFFFFFFFF))|((((rs1) << (15)) & 0xFFFFFFFF))|((((rs2) << (20)) & 0xFFFFFFFF)))
                           end
                        else
                           if bitsy == 21 then
                              -- C.J
                              -- oh, ye gods!
                              local imm = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFF800 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (11)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (11))) & (1))) << (4)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (9))) & (3))) << (8)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (8))) & (1))) << (10)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (7))) & (1))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (7)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (3))) & (7))) << (1)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (1))) << (5)) & 0xFFFFFFFF)))
                              local target = ((old_pc + imm) & (4294967294))
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
                              local offset = (((((((((orig_instruction) >> (10))) & (7))) << (3)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (6))) & (1))) << (2)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (1))) << (6)) & 0xFFFFFFFF)))
                              local rs1 = ((((orig_instruction) >> (7))) & (7))+8
                              local rs2 = ((((orig_instruction) >> (2))) & (7))+8
                              instruction = ((0x2023)|((((rs1) << (15)) & 0xFFFFFFFF))|((((rs2) << (20)) & 0xFFFFFFFF))|((((((offset) >> (5))) << (25)) & 0xFFFFFFFF))|((((((offset) & (31))) << (7)) & 0xFFFFFFFF)))
                           end
                        else
                           -- C.BEQZ
                           local rs1 = ((((orig_instruction) >> (7))) & (7))+8
                           if cpu.regs[rs1] == 0 then
                              local offset = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFF00 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (8)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (10))) & (3))) << (3)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (3))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (3))) & (3))) << (1)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (1))) << (5)) & 0xFFFFFFFF)))
                              new_pc = ((old_pc + offset) & (4294967294))
                              -- we're in C mode so the PC can't misalign
                           end
                           valid_instruction = true
                           goto instruction_legality_determined
                        end
                     else
                        if bitsy <= 26 then
                           -- C.SWSP
                           local rd = ((((orig_instruction) >> (2))) & (31))
                           local offset = (((((((((orig_instruction) >> (9))) & (15))) << (2)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (7))) & (3))) << (6)) & 0xFFFFFFFF)))
                           instruction = ((0x2023)|((((rd) << (20)) & 0xFFFFFFFF))|((((2) << (15)) & 0xFFFFFFFF))|((((((offset) >> (5))) << (25)) & 0xFFFFFFFF))|((((((offset) & (31))) << (7)) & 0xFFFFFFFF)))
                        else
                           if bitsy == 29 then
                              -- C.BNEZ
                              local rs1 = ((((orig_instruction) >> (7))) & (7))+8
                              if cpu.regs[rs1] ~= 0 then
                                 local offset = (((((orig_instruction) & (4096)) ~= 0) and 0xFFFFFF00 or 0)|((((((((orig_instruction) >> (12))) & (1))) << (8)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (10))) & (3))) << (3)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (5))) & (3))) << (6)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (3))) & (3))) << (1)) & 0xFFFFFFFF))|((((((((orig_instruction) >> (2))) & (1))) << (5)) & 0xFFFFFFFF)))
                                 new_pc = ((old_pc + offset) & (4294967294))
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
         -- if we get here in C mode, we had a full-size instruction OR we
         -- decompressed a half-size instruction into a full-size one
         if ((instruction) & (3)) ~= 3 then
            cpu:exception(rv32.EXC_ILLEGAL_INSTRUCTION, orig_instruction)
            goto abort_instruction
         end
         -----EXECUTE-----
         -- (some 16-bit instructions will have been executed above)
         local opcode = ((((instruction) >> (2))) & (31))
         --begin machine generated code (sorry)
         if opcode <= 11 then
            if opcode <= 4 then
               if opcode <= 2 then
                  if opcode <= 0 then
                     if opcode == 0 then
                        -- load
                        cpu.reserved_address = nil
                        cost = cost + 1
                        local rs1 = ((((instruction) >> (15))) & (31))
                        local imm = ((((instruction) >> (20)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(20))) or 0)) & 0xFFFFFFFF)
                        local funct3 = ((((instruction) >> (12))) & (7))
                        local addr = ((cpu.regs[rs1] + imm) & (0xFFFFFFFF))
                        local result
                        --begin machine generated code (sorry)
                        if funct3 <= 1 then
                           if funct3 <= 0 then
                              if funct3 == 0 then
                                 result = cpu:read_byte(addr)
                                 if cpu.had_exception then goto abort_instruction end
                                 result = ((result)|((((result) & (0x80)) ~= 0) and 0xFFFFFF00 or 0))
                              end
                           else
                              if ((addr) & (3)) == 3 then
                                 cost = cost + 1
                              end
                              result = cpu:read_halfword(addr)
                              if cpu.had_exception then goto abort_instruction end
                              result = ((result)|((((result) & (0x8000)) ~= 0) and 0xFFFF0000 or 0))
                           end
                        else
                           if funct3 <= 2 then
                              if (((addr) & (3)) ~= 0) then
                                 cost = cost + 1
                              end
                              result = cpu:read_word(addr)
                              if cpu.had_exception then goto abort_instruction end
                           else
                              if funct3 <= 4 then
                                 if funct3 == 4 then
                                    result = cpu:read_byte(addr)
                                    if cpu.had_exception then goto abort_instruction end
                                 end
                              else
                                 if ((addr) & (3)) == 3 then
                                    cost = cost + 1
                                 end
                                 result = cpu:read_halfword(addr)
                                 if cpu.had_exception then goto abort_instruction end
                              end
                           end
                        end
                        --end machine generated code

                        if cpu.had_exception then
                           goto abort_instruction
                        end
                        if result ~= nil then
                           local rd = ((((instruction) >> (7))) & (31))
                           if rd ~= 0 then cpu.regs[rd] = result end
                           valid_instruction = true
                        end
                     end
                  else
                     if opcode == 2 then
                        if cpu.execute_custom0 then
                           cost = cost + cpu:execute_custom0(instruction) or 0
                           if cpu.had_exception then goto abort_instruction end
                           valid_instruction = true
                        end
                     end
                  end
               else
                  if opcode <= 3 then
                     local funct3 = ((((instruction) >> (12))) & (7))
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
                     local rs1 = ((((instruction) >> (15))) & (31))
                     local a = cpu.regs[rs1]
                     local b = ((((instruction) >> (20)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(20))) or 0)) & 0xFFFFFFFF)
                     local result
                     local funct3 = ((((instruction) >> (12))) & (7))
                     local rd = ((((instruction) >> (7))) & (31))
                     --begin machine generated code (sorry)
                     if funct3 <= 3 then
                        if funct3 <= 1 then
                           if funct3 <= 0 then
                              if funct3 == 0 then
                                 -- ADDI
                                 result = ((a+b) & (0xFFFFFFFF))
                              end
                           else
                              -- SLLI
                              if ((((instruction) >> (25))) & (127)) == 0 then
                                 result = (((a) << (((b) & (31)))) & 0xFFFFFFFF)
                              end
                           end
                        else
                           if funct3 <= 2 then
                              -- SLTI
                              result = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0)) < ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0)) and 1 or 0
                           else
                              -- SLTIU
                              result = a < b and 1 or 0
                           end
                        end
                     else
                        if funct3 <= 5 then
                           if funct3 <= 4 then
                              -- XORI
                              result = ((a) ~ (b))
                           else
                              -- SRLI/SRAI
                              local funct7 = ((((instruction) >> (25))) & (127))
                              if funct7 == 0x20 then
                                 -- SRAI
                                 result = ((((a) >> (((b) & (31)))) | ((((a) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(((b) & (31))))) or 0)) & 0xFFFFFFFF)
                              elseif funct7 == 0x00 then
                                 -- SRLI
                                 result = ((a) >> (((b) & (31))))
                              end
                           end
                        else
                           if funct3 <= 6 then
                              -- ORI
                              result = ((a)|(b))
                           else
                              -- ANDI
                              result = ((a) & (b))
                           end
                        end
                     end
                     --end machine generated code

                     if result ~= nil then
                        if rd ~= 0 then cpu.regs[rd] = result end
                        valid_instruction = true
                     end
                  end
               end
            else
               if opcode <= 8 then
                  if opcode <= 5 then
                     -- auipc = add upper immediate to pc
                     local rd = ((((instruction) >> (7))) & (31))
                     local ui = ((instruction) & (((0xFFF) ~ 0xFFFFFFFF)))
                     if rd ~= 0 then
                        cpu.regs[rd] = ((old_pc + ui) & (0xFFFFFFFF))
                     end
                     valid_instruction = true
                  else
                     if opcode == 8 then
                        -- store
                        cpu.reserved_address = nil
                        cost = cost + 1
                        local rs1 = ((((instruction) >> (15))) & (31))
                        local rs2 = ((((instruction) >> (20))) & (31))
                        local value = cpu.regs[rs2]
                        local imm = ((((((((instruction) >> (20)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(20))) or 0)) & 0xFFFFFFFF)) & (0xFFFFFFE0)))|(((((instruction) >> (7))) & (31))))
                        local funct3 = ((((instruction) >> (12))) & (7))
                        local addr = ((cpu.regs[rs1] + imm) & (0xFFFFFFFF))
                        --begin machine generated code (sorry)
                        if funct3 <= 0 then
                           if funct3 == 0 then
                              cpu:write_byte(addr, ((value) & (255)))
                              if cpu.had_exception then goto abort_instruction end
                              valid_instruction = true
                           end
                        else
                           if funct3 <= 1 then
                              if ((addr) & (3)) == 3 then
                                 cost = cost + 1
                              end
                              cpu:write_halfword(addr, ((value) & (65535)))
                              if cpu.had_exception then goto abort_instruction end
                              valid_instruction = true
                           else
                              if (((addr) & (3)) ~= 0) then
                                 cost = cost + 1
                              end
                              cpu:write_word(addr, value, 0xFFFFFFFF)
                              if cpu.had_exception then goto abort_instruction end
                              valid_instruction = true
                           end
                        end
                        --end machine generated code

                        if cpu.had_exception then
                           goto abort_instruction
                        end
                     end
                  end
               else
                  if opcode <= 10 then
                     if opcode == 10 then
                        if cpu.execute_custom1 then
                           cost = cost + cpu:execute_custom1(instruction) or 0
                           if cpu.had_exception then goto abort_instruction end
                           valid_instruction = true
                        end
                     end
                  else
                     if not cpu.a_enabled then goto bad_amo end do
                        -- aq/rl ignored because we don't have caches or snooping...
                        local funct5 = ((((instruction) >> (27))) & (31))
                        local funct3 = ((((instruction) >> (12))) & (7))
                        local rs1 = ((((instruction) >> (15))) & (31))
                        local rs2 = ((((instruction) >> (20))) & (31))
                        local rd = ((((instruction) >> (7))) & (31))
                        if funct3 ~= 2 then
                           goto bad_amo
                        end
                        local addr = cpu.regs[rs1]
                        if (((addr) & (3)) ~= 0) then
                           cpu:exception(rv32.EXC_MISALIGNED_STORE, addr)
                           goto abort_instruction
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
                                    amo_op = function(mem,reg) return ((mem+reg) & (0xFFFFFFFF)) end
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
                                    goto abort_instruction
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
                                 else
                                    amo_op = function(mem,reg) return mem~reg end
                                 end
                              end
                           end
                        else
                           if funct5 <= 16 then
                              if funct5 <= 8 then
                                 if funct5 == 8 then
                                    amo_op = function(mem,reg) return mem|reg end
                                 end
                              else
                                 if funct5 <= 12 then
                                    if funct5 == 12 then
                                       amo_op = function(mem,reg) return mem&reg end
                                    end
                                 else
                                    if funct5 == 16 then
                                       amo_op = function(mem,reg)
                                          mem = ((mem) - ((((mem) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                          reg = ((reg) - ((((reg) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                          return ((math.min(mem, reg)) & (0xFFFFFFFF))
                                       end
                                    end
                                 end
                              end
                           else
                              if funct5 <= 20 then
                                 if funct5 == 20 then
                                    amo_op = function(mem,reg)
                                       mem = ((mem) - ((((mem) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                       reg = ((reg) - ((((reg) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                       return ((math.max(mem, reg)) & (0xFFFFFFFF))
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
                  end
               end
            end
         else
            if opcode <= 24 then
               if opcode <= 13 then
                  if opcode <= 12 then
                     -- op (register-to-register)
                     local rs1 = ((((instruction) >> (15))) & (31))
                     local a = cpu.regs[rs1]
                     local rs2 = ((((instruction) >> (20))) & (31))
                     local b = cpu.regs[rs2]
                     local result
                     local funct3 = ((((instruction) >> (12))) & (7))
                     local funct7 = ((((instruction) >> (25))) & (127))
                     local rd = ((((instruction) >> (7))) & (31))
                     local funct37 = (((((funct7) << (3)) & 0xFFFFFFFF))|(funct3))
                     --begin machine generated code (sorry)
                     if funct37 <= 8 then
                        if funct37 <= 3 then
                           if funct37 <= 1 then
                              if funct37 <= 0 then
                                 if funct37 == 0 then
                                    -- ADD
                                    result = ((a+b) & (0xFFFFFFFF))
                                 end
                              else
                                 -- SLL
                                 result = (((a) << (((b) & (31)))) & 0xFFFFFFFF)
                              end
                           else
                              if funct37 <= 2 then
                                 -- SLT
                                 result = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0)) < ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0)) and 1 or 0
                              else
                                 -- SLTU
                                 result = a < b and 1 or 0
                              end
                           end
                        else
                           if funct37 <= 5 then
                              if funct37 <= 4 then
                                 -- XOR
                                 result = ((a) ~ (b))
                              else
                                 -- SRL
                                 result = ((a) >> (((b) & (31))))
                              end
                           else
                              if funct37 <= 6 then
                                 -- OR
                                 result = ((a)|(b))
                              else
                                 if funct37 <= 7 then
                                    -- AND
                                    result = ((a) & (b))
                                 else
                                    -- MUL
                                    if cpu.m_enabled then
                                       cost = cost + 3
                                       result = (a*b)&0xFFFFFFFF
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
                                    cost = cost + 3
                                    result = (a*b)&0xFFFFFFFF
                                    a = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                    b = ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                    result = ((a*b)>>32)&0xFFFFFFFF
                                 end
                              else
                                 -- MULHSU
                                 if cpu.m_enabled then
                                    cost = cost + 3
                                    a = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                    result = ((a*b)>>32)&0xFFFFFFFF
                                 end
                              end
                           else
                              if funct37 <= 11 then
                                 -- MULHU
                                 if cpu.m_enabled then
                                    cost = cost + 3
                                    result = (a*b)>>32
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
                                       result = (((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0)) / ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0)))
                                       if result < 0 then
                                          result = math.ceil(result)
                                       else
                                          result = math.floor(result)
                                       end
                                       result = ((result) & (0xFFFFFFFF))
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
                                       result = a // b
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
                                       a = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                       b = ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                       local quotient = (a / b)
                                       if quotient < 0 then
                                          quotient = math.ceil(quotient)
                                       else
                                          quotient = math.floor(quotient)
                                       end
                                       result = a - (quotient * b)
                                       result = ((result) & (0xFFFFFFFF))
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
                                       result = a % b
                                    end
                                 end
                              else
                                 if funct37 <= 256 then
                                    if funct37 == 256 then
                                       -- SUB
                                       result = ((a-b) & (0xFFFFFFFF))
                                    end
                                 else
                                    if funct37 == 261 then
                                       -- SRA
                                       result = ((((a) >> (((b) & (31)))) | ((((a) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(((b) & (31))))) or 0)) & 0xFFFFFFFF)
                                    end
                                 end
                              end
                           end
                        end
                     end
                     --end machine generated code

                     if result ~= nil then
                        if rd ~= 0 then cpu.regs[rd] = result end
                        valid_instruction = true
                     end
                  else
                     -- lui = load upper immediate
                     local rd = ((((instruction) >> (7))) & (31))
                     local ui = ((instruction) & (((0xFFF) ~ 0xFFFFFFFF)))
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
                           if cpu.had_exception then goto abort_instruction end
                           valid_instruction = true
                        end
                     end
                  else
                     if opcode == 24 then
                        -- various forms of branch
                        local rs1 = ((((instruction) >> (15))) & (31))
                        local rs2 = ((((instruction) >> (20))) & (31))
                        local a = cpu.regs[rs1]
                        local b = cpu.regs[rs2]
                        local funct3 = ((((instruction) >> (12))) & (7))
                        local should_branch
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
                                    should_branch = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0)) < ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0))
                                 end
                              end
                           end
                        else
                           if funct3 <= 5 then
                              should_branch = ((a) - ((((a) & (0x80000000)) ~= 0) and 0x100000000 or 0)) >= ((b) - ((((b) & (0x80000000)) ~= 0) and 0x100000000 or 0))
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
                           local imm = ((((((((instruction) >> (19)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(19))) or 0)) & 0xFFFFFFFF)) & (0xFFFFF000)))|((((((((instruction) >> (7))) & (1))) << (11)) & 0xFFFFFFFF))|((((((((instruction) >> (25))) & (63))) << (5)) & 0xFFFFFFFF))|((((((((instruction) >> (8))) & (15))) << (1)) & 0xFFFFFFFF)))
                           local target = ((old_pc + imm) & (4294967294))
                           if not cpu.c_enabled and (((target) & (2)) ~= 0) then
                              cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                              goto abort_instruction
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
                     local rd = ((((instruction) >> (7))) & (31))
                     local rs1 = ((((instruction) >> (15))) & (31))
                     local imm = ((((instruction) >> (20)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(20))) or 0)) & 0xFFFFFFFF)
                     local target=((cpu.regs[rs1]+imm) & (4294967294))
                     if rd ~= 0 then
                        cpu.regs[rd] = new_pc
                     end
                     if not cpu.c_enabled and (((target) & (2)) ~= 0) then
                        cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                        goto abort_instruction
                     else
                        new_pc = target
                     end
                     valid_instruction = true
                  else
                     if opcode == 27 then
                        -- jal = jump and link
                        local old_new_pc = new_pc
                        local rd = ((((instruction) >> (7))) & (31))
                        local imm = ((((((((instruction) >> (11)) | ((((instruction) & (0x80000000)) ~= 0) and (0xFFFFFFFF << (32-(11))) or 0)) & 0xFFFFFFFF)) & (0xFFF00000)))|(((instruction) & (0xFF000)))|((((((((instruction) >> (20))) & (1))) << (11)) & 0xFFFFFFFF))|((((((((instruction) >> (21))) & (1023))) << (1)) & 0xFFFFFFFF)))
                        local target = ((old_pc + imm) & (4294967294))
                        if not cpu.c_enabled and (((target) & (2)) ~= 0) then
                           cpu:exception(rv32.EXC_MISALIGNED_PC, target)
                           goto abort_instruction
                        else
                           new_pc = target
                        end
                        if rd ~= 0 then
                           cpu.regs[rd] = old_new_pc
                        end
                        valid_instruction = true
                     end
                  end
               else
                  if opcode <= 28 then
                     -- system
                     local funct3 = ((((instruction) >> (12))) & (7))
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
                     local rd = ((((instruction) >> (7))) & (31))
                     local rs1 = ((((instruction) >> (15))) & (31))
                     local csr = ((((instruction) >> (20))) & (4095))
                     local wvalue
                     if funct3 >= 4 then
                        -- I variant
                        wvalue = rs1
                     else
                        -- Non-I variant
                        wvalue = cpu.regs[rs1]
                     end
                     local funct2 = ((funct3) & (3))
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
                              valid_instruction = cpu:write_csr(csr, ((rvalue)|(wvalue))) ~= false
                           end
                        else
                           -- Read & Clear
                           if rs1 == 0 then
                              -- no write required
                              valid_instruction = true
                           else
                              valid_instruction = cpu:write_csr(csr, ((rvalue) & (((wvalue) ~ 0xFFFFFFFF)))) ~= false
                           end
                        end
                     end
                     --end machine generated code

                     if cpu.had_exception then
                        goto abort_instruction
                     end
                     if rd ~= 0 and valid_instruction then
                        cpu.regs[rd] = rvalue
                     end
                  else
                     if opcode == 30 then
                        if cpu.execute_custom3 then
                           cost = cost + cpu:execute_custom3(instruction) or 0
                           if cpu.had_exception then goto abort_instruction end
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
      ::abort_instruction::
      cpu.budget = cpu.budget - cost
   end
end

return rv32
