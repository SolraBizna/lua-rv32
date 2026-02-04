#!/usr/bin/env lua
-- -*- lua -*-

local version

local s,e = pcall(function()
    return assert(load("return 1 << 3","test53+"))()
end)
if s and e == 8 then
    version = "53"
elseif bit32 then
    version = "52"
end

if version == "52" then
    return load(@"rv32_elf.52.lua")()
elseif version == "53" then
    return load(@"rv32_elf.53.lua")()
else
    error("Your version of Lua is too old to use `rv32_elf`. Lua 5.2 or later is required.")
end
