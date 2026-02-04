#!/usr/bin/env lua5.4

if #arg ~= 3 then
    error("usage: demac.lua input.lua macros.lua output.lua")
end

local inpath = arg[1]
local macrospath = arg[2]
local outpath = arg[3]

local f = assert(io.open(inpath,"rb"))
local text = assert(f:read("*a"))
f:close()

local function protect_parens(x)
    return x:gsub("%b()", function(q)
        return q:gsub(",","⋄")
    end):gsub("[()]", {
        ["("]="«",
        [")"]="»",
    })
end

local function unprotect_parens(x)
    return x:gsub("«", "("):gsub("»", ")"):gsub("⋄",",")
end

local macros
function demac(text)
    return text:gsub("%%([_a-zA-Z][_a-zA-Z0-9]*) *(%b()) *(%b{})",
    function(macroname, args, block)
        args = args:sub(2,-2)
        block = block:sub(2,-2)
        local macro = macros.block[macroname]
        if not macros.block[macroname] then
            error(("unknown block macro %q"):format(macroname))
        end
        local argt = {}
        args = protect_parens(args)
        for arg in args:gmatch("[^,]+") do
            arg = arg:gsub("^ +",""):gsub(" +$","")
            if arg ~= "" then
                argt[#argt+1] = demac(unprotect_parens(arg))
            end
        end
        return demac(macro(block, table.unpack(argt)))
    end):gsub("%$([_a-zA-Z][_a-zA-Z0-9]*) *(%b())",
    function(macroname, args)
        args = args:sub(2,-2)
        local macro = macros.arg[macroname]
        if not macros.arg[macroname] then
            error(("unknown arg macro %q"):format(macroname))
        end
        local argt = {}
        args = protect_parens(args)
        for arg in args:gmatch("[^,]+") do
            arg = arg:gsub("^ +",""):gsub(" +$","")
            if arg ~= "" then
                argt[#argt+1] = demac(unprotect_parens(arg))
            end
        end
        return demac(macro(table.unpack(argt)))
    end)
end
macros = dofile(macrospath)

text = demac(text)
-- okay, but now it looks really terrible!
-- part 1: remove blank lines in indented areas
local t = {}
local prev_indented = false
for line in text:gmatch("[^\n]*") do
    line = line:gsub("[ \t]+$","")
    if line == "" and prev_indented then
        goto continue
    end
    prev_indented = line:match("^[ \t]")
    t[#t+1] = line
    ::continue::
end
-- part 2 is up to regen.sh

local f = assert(io.open(outpath, "wb"))
assert(f:write(table.concat(t,"\n")))
f:close()
