This program uses `lua-rv32` to emulate a simple machine designed only to run programs in the official RISC-V compliance test suite. It requires some nontrivial changes to the test suite tree in order to run, and may not function with the latest version of the repository.

Like `lua-rv32`, this program has versions for both Lua 5.2 and 5.3+. This facilitates testing of both versions of the emulator.

This is *not* the preferred way to test this emulator, but is presented for completeness.
