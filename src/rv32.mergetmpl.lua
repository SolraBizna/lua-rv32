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
    if _G.rv32trace then
        return load(@"rv32.52.trace.lua")()
    else
        return load(@"rv32.52.lua")()
    end
elseif version == "53" then
    if _G.rv32trace then
        return load(@"rv32.53.trace.lua")()
    else
        return load(@"rv32.53.lua")()
    end
else
    error("Your version of Lua is too old to use `rv32`. Lua 5.2 or later is required.")
end
