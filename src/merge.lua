#!/usr/bin/env lua5.4
-- -*- lua -*-

if #arg ~= 2 then
    print("usage: merge input.tmpl.lua output.lua")
    os.exit(1)
end

local inpath = arg[1]
local outpath = arg[2]

local f = assert(io.open(inpath,"rb"))
local a = f:read("*a")
f:close()

a = a:gsub("@\"([^\"]+)\"", function(path)
    local f = assert(io.open(path,"rb"))
    local a = assert(f:read("*a"))
    f:close()
    return ("%q,\"@%s\""):format(a,path)
end)
f = assert(io.open(outpath,"wb"))
assert(f:write(a))
f:close()
