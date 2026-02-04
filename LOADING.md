Technically, all you need to do to load a program is to prefill memory with your program (maybe by calling `cpu:write_word` a bunch?), set `cpu.pc` to the entry point address, and go for broke. However, chances are, your program isn't already in the form of a linearly-loadable slab with a predetermined entry point.

If you can get your program in the form of a `.elf` file, you're in luck. The `rv32_elf` module is here to help you.

```lua
-- Load the ELF loader module
local rv32_elf = require "rv32_elf"
-- Open the ELF file for reading (in binary mode, if on Windows)
local elf = assert(io.open("my_program.elf","rb"))
-- Load it!
cpu.pc = assert(rv32_elf.load(elf, {
    write_word=function(...)
        -- Just blindly accepting any and all loading :)
        cpu:write_word(...)
    end,
}))
-- don't forget to close the file when you're done
elf:close()
```

The first parameter must be a file-like object; it must support `read` and `seek` and behave like a standard Lua file. The second parameter must be a table containing at least one of the following:

- `function write_word(addr, value, mask)`: Load a 32-bit word to a 32-bit aligned *physical* address. Mask is always 0xFFFFFFFF. (As seen above, you can forward this directly to `cpu:write_word`—note that this `write_word` does not have a self argument, so you can't just pass `cpu.write_word` directly, you have to *forward* it!)
- `function load_segment(virtual_addr, physical_addr, memory_size, data)`: Load an entire segment of the program to the given physical/virtual address. If `memory_size` is larger than `#data` then extra bytes should be filled in with zeroes. You are responsible for interpreting virtual/physical addressing, wrapping the address if necessary, breaking `data` up into 32-bit words if necessary, etc. (If both `load_segment` and `write_word` are provided, `load_segment` is used.)
- `function learn_symbol(symbol_name, symbol_value, symbol_size)`: If any symbols are defined in the ELF, this function is called with the name, value, and size of each symbol. (Advanced users who want access to the other data in the section table entry will find extra parameters after the documented ones...)

The segments are only read if `write_word` or `load_segment` is present. The symbol table is only read if `learn_symbol` is present.

