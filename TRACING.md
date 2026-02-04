Verbose tracing produces a *huge* amount of information about what the emulated CPU is doing. It slows down emulation *enormously*. In addition, it's probably only useful for debugging the emulator core itself. Nevertheless, the capability is documented here for completeness.

To enable tracing, set the `rv32trace` global to a truthy value *before* loading the `rv32` module for the first time. It can either be a function (see below) or simply `true` to trace using Lua's built-in `print` function.

```lua
rv32trace = true
local rv32 = require "rv32"
```

**After the module has been loaded for the first time**, you are in one of two states:

- Tracing was not loaded. Setting `rv32trace` will have no effect.
- Tracing was loaded. You may only set `rv32trace` to function values. (Setting it to `function() end` will disable tracing, but the full overhead will still be incurred.)

If `rv32trace` was set to true, it is replaced with a function implementing tracing via the built-in `print` function.

```lua
function rv32trace(value)
    if value == true then
        -- Subsequent messages should be emphasized. (They deal with an instruction fetch, which should draw the eye.)
    elseif value == false then
        -- Subsequent messages should no longer be emphasized. (We are done talking about an instruction fetch.)
    else
        -- value is a message to display, which may contain multiple lines
    end
end
```

Since the versions of the emulator have identical internal state, you could turn tracing on/off by switching which of `rv32.5x.trace` and `rv32.5x` is in the `rv32` global and the metatable and ... that sounds like a recipe for insanity. Just don't use tracing unless you're debugging the core.
