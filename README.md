# Not again!

RISC-V is a powerful, elegant CPU instruction set architecture that is also an open standard. `lua-rv32` is a pure Lua implementation of this architecture. You can use it to create a 32-bit RISC-V emulator.

`lua-rv32` comes in two versions, one for Lua 5.2, one for Lua 5.3 and later. They are equivalent, except that the Lua 5.2 version is a lot slower.

# Completeness

`lua-rv32` supports all of RV32IMAC_Zicsr_Zifence. To unpack that one piece at a time:

- RV32: 32-bit version of the RISC-V ISA. 32-bit address space, 32-bit general purpose registers, 32-bit ALU, 32-bit data bus.
- I: Version of the basic instruction set that has 31 general purpose registers. (As opposed to E, which has only 15.)
- M: Multiplication and division instructions.
- A: Atomic memory operations.
- C: Support for compressing certain common instructions into 16 bits, significantly reducing program size. (As opposed to every instruction taking up an entire 32 bits.)
- Zicsr: Instructions for reading and writing hardware "control and status registers".
- Zifence: An instruction that informs the CPU's instruction cache that some instructions stored in the instruction cache may no longer be valid (because the memory underlying them has changed).

`lua-rv32` does not implement anything outside of the purview of these standards. In particular, it only implements components of the RISC-V *unprivileged* specification. While it does not contain any implementation of the RISC-V *privileged* specification, it is carefully designed such that your embedding program can provide a full implementation of the privileged specification—or any other paradigm you care to imagine that fills the same role.

`lua-rv32` accounts, very roughly, for the effective execution time of a given instruction:

- Normal instructions: 1 cycle
- Load/store: 2 cycles if aligned, 3 if misaligned
- AMO\*: 4 cycles
- MUL: 4 cycles
- MULH/MULHS/MULHSU: 4 cycles on Lua 5.3+, variably up to 33 cycles on Lua 5.2
- DIV/DIVU/REM/REMU: 6 cycles
- Any 32-bit instruction not aligned to a four-byte boundary: +1 cycle

These are balanced so that a degenerate program ought not to be able to unreasonably inflate the execution time of a single `run` call. (Multiply and divide are given a little bit of extra cost to reflect a common attribute of real low-end CPU designs.)

# Usage

Put `rv32.lua` in your module search path. This is a somewhat bulky bundled version that will adapt to whatever Lua version it is loaded into, and will also optionally enable/disable verbose tracing according to the presence of a global named `rv32trace` at load time. (See [TRACING.md](TRACING.md) for more information on verbose tracing.)

```lua
local rv32 = require "rv32"
-- or
local rv32 = dofile "path/to/rv32.lua"
```

**ALTERNATIVELY:** Execute the correct module for your Lua version and tracing needs:

```lua
local rv32 = dofile "path/to/rv32.52.lua" -- Lua 5.2, normal
local rv32 = dofile "path/to/rv32.52.trace.lua" -- Lua 5.2, tracing
local rv32 = dofile "path/to/rv32.53.lua" -- Lua 5.3+, normal
local rv32 = dofile "path/to/rv32.53.trace.lua" -- Lua 5.3+, tracing
```

Either way, the next step is to create a new CPU:
```lua
local cpu = rv32.new()
```

You must, at minimum, implement `read_word` and `write_word` for this CPU. (See "Methods" for more information, or [`test/fight/fight.lua`](test/fight/fight.lua) for a very simple example.) Load a valid RISC-V program (see [`LOADING.md`](LOADING.md)), and run!

## Fields

An RV32 CPU has the following fields, which you may modify within the indicated constraints:

- `regs`: A 31-element array of registers, each of which must be a valid unsigned 32-bit integer. For performance reasons, key [0] also exists and must always have a value of 0. You can index this table either with the raw register number (0–31) or using ABI names via the `REG_*` constants in `rv32` (e.g. `rv32.REG_SP`).
- `pc`: Outside of `run`, the address of the next instruction to execute. During a call to `run`, the address of the executing instruction. Must be a valid unsigned 32-bit integer, and MUST NOT have the lowest bit set.
- `budget`: How many cycles are left in the current `run` call. Will end up with a negative value any time a `run` call finishes with an instruction that is longer than the remaining budget. The negative cycles will be paid for by the next `run` call, keeping the overall throughput consistent. You can freely manipulate this to affect the behavior of a `run` in progress.
- `m_enabled` (default true): Whether to allow M instructions (multiplication/division)
- `a_enabled` (default true): Whether to allow A instructions (atomic memory operations)
- `c_enabled` (default true): Whether to allow C instructions (compact forms of popular instructions)
- `zicsr_enabled` (default false): Whether to allow `CSR*` instructions (control and status register read/write—this needs additional help from you, see relevant section)
- `strict_alignment` (default nil): If true, misaligned halfword reads and writes will throw an exception. (You are responsible for handling fullword reads and writes, so it's up to you to throw the appropriate exception if you want strict alignment.)
- `had_exception`: `true` if an exception has occurred during the execution of this instruction. Set to `nil` every time an instruction is fetched. You must set this to `true` inside your `exception` method (if you provide one).
- `reserved_address`: Used to implement the reserved load / conditional store instructions. Set it to nil any time you mess with memory.
- `instret_lo`: Low 32-bits of the number of instructions that have executed since the CPU was created. Suitable for feeding the `minstret` CSR.
- `instret_hi`: High 32-bits of the number of instructions that have executed since the CPU was created. Suitable for feeding the `minstreth` CSR.

You may set additional fields as you see fit, as long as they don't conflict with another field or method name.

## Methods 

These are divided between methods *you* implement and methods *we* provide.

### Your methods

You must provide at least `read_word` and `write_word` in order for the emulator to basically work. You can provide methods in one of three ways:

- By setting them as fields of the `cpu` object directly (complicating serialization)
- By setting them as fields of the `rv32` module (gross)
- By making your own metatable that sits in between the `cpu` object and the `rv32` module

With that in mind, here are the methods you must provide:

- `read_word(cpu, address, fault_code)`: Read one 32-bit word from memory. `address` is a 32-bit unsigned integer. This MUST return a valid 32-bit integer OR throw an exception/error. `fault_code` will be one of `rv32.EXC_INSTRUCTION_ACCESS_FAULT` (if an instruction is being fetched) or `rv32.EXC_STORE_ACCESS_FAULT` (if an AMO is being performed). If it is non-nil, you should use it as the exception code for an invalid access. If it is nil, use `rv32.EXC_LOAD_ACCESS_FAULT`.
- `write_word(cpu, address, value, mask)`: Write one 32-bit word to memory. `address` and `value` are both 32-bit unsigned integers. `mask` is a four byte integer, where an all-ones byte indicates a byte of memory that should change and an all-zeroes byte indicates a byte that should be preserved.

If you want support for misaligned memory reads/writes, you must implement it yourself in `read_word`/`write_word`. If you don't want it, set `cpu.strict_alignment = true`, and you must call `cpu:exception(rv32.EXC_MISALIGNED_*, addr)` on any misaligned word read/write.

The default exception handler throws a rather verbose Lua error that shows what exception happened, what the "trap value" was, and the current state of all CPU registers. If you want your own exception handling behavior, you can override the following method:

- `exception(cpu, cause, tval)`: Process an exception. `cause` is one of the `rv32.EXC_*` constants. (A human-readable English description of the cause can be obtained from `rv32.CAUSE_STRINGS`.) **You must set `cpu.had_exception=true` inside this function or things will break badly.** The standard privileged model, in M mode, will store the current `pc` in CSR `mepc`, set the `pc` to CSR `mtvec`, store `cause` in CSR `mcause`, store `tval` in CSR `mtval`, and disable interrupts at the very least. See the RISC-V Privileged specification for (way) more details.

If you want Zicsr support, you must also provide:

- `read_csr(cpu, csr)`: Return the current value of the given CSR if the program should be able to read it. Return `nil` if the CSR is unknown or the read should fail.
- `write_csr(cpu, csr, new_value)`: Write the given value to the given CSR if the program should be able to write it. Return `false` (NOT `nil`) if the CSR is unknown or the write should *entirely* fail. (With most defined CSRs, bits that "do not exist" or cannot be written will simply not change.)

We also provide some hooks for custom instructions. If any of the following methods are non-nil, they will be called for the appropriate instruction:

- `execute_custom0(cpu, instruction)`
- `execute_custom1(cpu, instruction)`
- `execute_custom2(cpu, instruction)`
- `execute_custom3(cpu, instruction)`
- `ecall(cpu)`
- `ebreak(cpu)`

For custom-[0-3], all but the lowest 7 bits of the instruction word are available for your purposes as custom parameters, data, etc. This is not the case for `ECALL`/`EBREAK`, which have exactly one valid bit pattern.

### Our methods

Once you have provided all relevant methods for your execution environment, you may call any of the provided methods:

- `rv32.new()`: Returns a brand-new CPU, initialized to a sane default state, with `{__index=rv32}` as its metatable.
- `cpu:run([budget])`: Adds the given number of cycles to the budget and runs the CPU until the budget runs out. If budget is nil or unspecified, **sets the budget to 1** and then runs one instruction.
- `cpu:read_byte(addr)`: Reads an unsigned 8-bit byte from the given memory address.
- `cpu:write_byte(addr, value)`: Writes an unsigned 8-bit byte to the given memory address.
- `cpu:read_halfword(addr, fault_code)`: Reads an unsigned 16-bit halfword from the given memory address. `fault_code` is passed to `read_word` internally, and you should not need it.
- `cpu:write_halfword(addr, value)`: Writes an unsigned 16-bit halfword to the given memory address.

And that's it.

# Compliance

`lua-rv32` complies with all the applicable RISC-V Foundation compliance tests I was able to find. Notable exception: the misaligned branch tests express an exception signature when run with `lua-rv32` but not with the official SAIL model. I'm pretty sure they're *supposed* to express an exception signature. I'm not sure why they don't on SAIL.

There is a lot more testing that can be done to a RISC-V implementation that these tests will not cover. Performing such testing is outside my scope and budget. I know there are no *really big* bugs. There are probably small ones here and there.

# Performance

On a Ryzen 9 9950X3D clocking in at over 4GHz, running the Embench™ benchmark suite, the Lua 5.2 version manages 0.96–1.34MIPS and the Lua 5.3+ version manages 3.55–4.66MIPS. That means that a mere 100kIPS processor would use up a significant chunk of one core of my beefy workstation, and 10-50kIPS is probably more reasonable if this is going to be part of a singlethreaded game loop.

Well, what did you expect? Lua is an interpreted language, and emulating a CPU is a very complicated task. 10kIPS is still thousands of times faster than any human.

"But what about LuaJIT?" Yes, LuaJIT would probably be a lot faster; except that its bit manipulation works with *signed* 32-bit values, and *the entire architecture of the emulator* is structured around *unsigned* 32-bit values, so we'll never know.

# Why though?!

I wanted to make a RISC-V CPU mod for Factorio, and I took it too far. Hey, at least I don't seem to hate programming any more? Who knows how long *that* will last.

# Building

Use the prebuilt artifacts in `lib`. Seriously.

If you have Lua 5.2 installed as `lua5.2` and `luac5.2`, and Lua 5.3 installed as `lua5.3` and `luac5.3`, and `emacs` installed with working `lua-mode` (e.g. from `package.el`), and a Bourne-compatible shell, you can run `./build.sh` to rebuild all the versions from the common source file. Which—

## WAIT WHAT IS THIS DEMAC GARBAGE, WHAT HAVE YOU DONE

Okay, so Lua 5.2 and 5.3+ have wildly different methods of doing bit manipulation and integer math, but abstracting those out with function calls would add too much overhead to the core emulator loop. So this emulator isn't actually *written* in any one dialect of Lua, instead it uses a very ad-hoc and fragile intermediate macro system termed "demac" to automatically generate a tuned implementation for each target version of Lua. It also let me have the tracing code optionally exist without it adding any overhead when not needed.

It's ugly, but effective.

Don't look at the select macro...

# Legalese

`lua-rv32` is copyright 2026, Solra Bizna, and licensed under either of:

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or
   <http://www.apache.org/licenses/LICENSE-2.0>)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the `lua-rv32` module by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
