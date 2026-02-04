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
    return load("local rv32_elf = {}\
\
local string_unpack = string.unpack\
if not string_unpack then\
   string_unpack = function(source, input, pos)\
      pos = pos or 1\
      assert(source:sub(1,1) == \"<\")\
      local t = {}\
      for n=2,#source,2 do\
         assert(source:sub(n,n) == \"I\")\
         local count = source:sub(n+1,n+1)\
         assert(count == \"1\" or count == \"2\" or count == \"4\")\
         count = tonumber(count)\
         local bits = 0\
         local mult = 1\
         for m=1,count do\
            bits = bits + input:byte(pos) * mult\
            mult = mult * 256\
            pos = pos + 1\
         end\
         t[#t+1] = bits\
      end\
      return table.unpack(t)\
   end\
end\
\
function rv32_elf.load(elf, params)\
   if not params or (not params.write_word and not params.load_segment and not params.learn_symbol) then\
      error(\"you must provide either write_word, load_segment, or learn_symbol, otherwise there isn't much point in calling rv32_elf.load\", 2)\
   end\
   local header = elf:read(52)\
   if not header or #header ~= 52 then\
      return nil, \"could not read ELF header\"\
   end\
   if header:sub(1,4) ~= \"\\x7FELF\" then return nil, \"not an ELF\" end\
   if header:sub(19,20) ~= \"\\xF3\\x00\" then return nil, \"not a RISC-V ELF\" end\
   if header:sub(5,5) ~= \"\\x01\" then return nil, \"not a 32-bit ELF\" end\
   if header:sub(6,6) ~= \"\\x01\" then return nil, \"not a two's complement little-endian ELF\" end\
   if header:sub(7,7) ~= \"\\x01\" then return nil, \"not a version 1 ELF\" end\
   -- ignoring 8 and 9 because the ABI is often set wrong and is hard to set right\
   -- ignoring 10-16 because they're reserved and should be ignored\
   if header:sub(17,18) ~= \"\\x02\\x00\" then return nil, \"not an executable ELF\" end\
   local e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx = string_unpack(\"<I4I4I4I4I4I2I2I2I2I2I2\", header, 21)\
   if e_ehsize < 52 then return nil, \"main header in ELF too small\" end\
   if e_phentsize < 32 then return nil, \"program headers in ELF too small\" end\
   if e_shentsize < 40 then return nil, \"section headers in ELF too small\" end\
   if params.load_segment or params.write_word then\
      for n=1,e_phnum do\
         if not elf:seek(\"set\", e_phoff + e_phentsize * (n-1)) then\
            return nil, \"could not seek to program header within ELF\"\
         end\
         header = elf:read(32)\
         if not header or #header ~= 32 then\
            return nil, \"could not read program header within ELF\"\
         end\
         local p_type, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_flags, p_align = string_unpack(\"<I4I4I4I4I4I4I4I4\", header)\
         if p_type ~= 1 then\
            -- ignore segments other than PT_LOAD\
            goto continue\
         end\
         if p_filesz > p_memsz then return nil, \"ELF has a program section larger on disk than in memory?\" end\
         if p_vaddr % 4 ~= 0 then return nil, \"section not aligned to 4-byte boundary\" end\
         if not elf:seek(\"set\", p_offset) then\
            return nil, \"could not seek to program data within ELF\"\
         end\
         local disk_size = p_filesz\
         if disk_size % 4 ~= 0 then\
            -- HACK: read some extra bytes :/\
            disk_size = disk_size + 4 - (disk_size % 4)\
         end\
         local data = elf:read(disk_size)\
         if not data or #data ~= disk_size then\
            return nil, \"could not read program data within ELF\"\
         end\
         if params.load_segment then\
            params.load_segment(p_vaddr, p_paddr, p_memsz, data)\
         elseif params.write_word then\
            for n=1,#data,4 do\
               local offset = (n-1)\
               params.write_word(bit32.band(p_paddr+offset,0xFFFFFFFF), string_unpack(\"<I4\", data, n), 0xFFFFFFFF)\
            end\
            for offset=#data,p_memsz,4 do\
               params.write_word(bit32.band(p_paddr+offset,0xFFFFFFFF), 0, 0xFFFFFFFF)\
            end\
         else\
            error(\"UNREACHABLE\")\
         end\
         ::continue::\
      end\
   end\
   if params.learn_symbol then\
      local symtab_header, strtab_header\
      for n=1,e_shnum do\
         if not elf:seek(\"set\", e_shoff + e_shentsize * (n-1)) then\
            return nil, \"could not seek to section header within ELF\"\
         end\
         header = elf:read(40)\
         if not header or #header ~= 40 then\
            return nil, \"could not read section header within ELF\"\
         end\
         local sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize = string_unpack(\"<I4I4I4I4I4I4I4I4I4I4\", header)\
         if sh_type == 2 then\
            if symtab_header then\
               return nil, \"ELF contains multiple symbol tables!\"\
            end\
            symtab_header = {\
               sh_name=sh_name,\
               sh_type=sh_type,\
               sh_flags=sh_flags,\
               sh_addr=sh_addr,\
               sh_offset=sh_offset,\
               sh_size=sh_size,\
               sh_link=sh_link,\
               sh_info=sh_info,\
               sh_addralign=sh_addralign,\
               sh_entsize=sh_entsize,\
            }\
         elseif sh_type == 3 and strtab_header == nil then\
            -- if there is more than one string table, we only care about\
            -- the first one\
            strtab_header = {\
               sh_name=sh_name,\
               sh_type=sh_type,\
               sh_flags=sh_flags,\
               sh_addr=sh_addr,\
               sh_offset=sh_offset,\
               sh_size=sh_size,\
               sh_link=sh_link,\
               sh_info=sh_info,\
               sh_addralign=sh_addralign,\
               sh_entsize=sh_entsize,\
            }\
         end\
      end\
      if not symtab_header or not strtab_header then\
         goto no_syms\
      end\
      if not elf:seek(\"set\", strtab_header.sh_offset) then\
         return nil, \"unable to seek to the string table within the ELF\"\
      end\
      local raw_strtab = elf:read(strtab_header.sh_size)\
      if not raw_strtab then\
         return nil, \"unable to read the string table within the ELF\"\
      end\
      local symbols = {}\
      if not elf:seek(\"set\", symtab_header.sh_offset) then\
         return nil, \"unable to seek to the symbol table within the ELF\"\
      end\
      if symtab_header.sh_entsize ~= 16 then\
         return nil, \"symbols not 16 bytes? CONFUSING\"\
      end\
      for n = 0, symtab_header.sh_size-1, 16 do\
         local buf = elf:read(16)\
         if not buf or #buf ~= 16 then\
            return nil, \"unable to read a symbol from the ELF\"\
         end\
         local st_name, st_value, st_size, st_info, st_other, st_shndx = string_unpack(\"<I4I4I4I1I1I2\", buf)\
         if st_name ~= 0 then\
            local symbol_name = raw_strtab:match(\"[^\\0]+\", st_name+1)\
            if symbol_name and symbol_name ~= \"\" then\
               params.learn_symbol(symbol_name, st_value, st_size, st_info, st_other, st_shndx)\
            end\
         end\
      end\
   end\
   ::no_syms::\
   return e_entry\
end\
\
return rv32_elf\
","@rv32_elf.52.lua")()
elseif version == "53" then
    return load("local rv32_elf = {}\
\
local string_unpack = string.unpack\
if not string_unpack then\
   string_unpack = function(source, input, pos)\
      pos = pos or 1\
      assert(source:sub(1,1) == \"<\")\
      local t = {}\
      for n=2,#source,2 do\
         assert(source:sub(n,n) == \"I\")\
         local count = source:sub(n+1,n+1)\
         assert(count == \"1\" or count == \"2\" or count == \"4\")\
         count = tonumber(count)\
         local bits = 0\
         local mult = 1\
         for m=1,count do\
            bits = bits + input:byte(pos) * mult\
            mult = mult * 256\
            pos = pos + 1\
         end\
         t[#t+1] = bits\
      end\
      return table.unpack(t)\
   end\
end\
\
function rv32_elf.load(elf, params)\
   if not params or (not params.write_word and not params.load_segment and not params.learn_symbol) then\
      error(\"you must provide either write_word, load_segment, or learn_symbol, otherwise there isn't much point in calling rv32_elf.load\", 2)\
   end\
   local header = elf:read(52)\
   if not header or #header ~= 52 then\
      return nil, \"could not read ELF header\"\
   end\
   if header:sub(1,4) ~= \"\\x7FELF\" then return nil, \"not an ELF\" end\
   if header:sub(19,20) ~= \"\\xF3\\x00\" then return nil, \"not a RISC-V ELF\" end\
   if header:sub(5,5) ~= \"\\x01\" then return nil, \"not a 32-bit ELF\" end\
   if header:sub(6,6) ~= \"\\x01\" then return nil, \"not a two's complement little-endian ELF\" end\
   if header:sub(7,7) ~= \"\\x01\" then return nil, \"not a version 1 ELF\" end\
   -- ignoring 8 and 9 because the ABI is often set wrong and is hard to set right\
   -- ignoring 10-16 because they're reserved and should be ignored\
   if header:sub(17,18) ~= \"\\x02\\x00\" then return nil, \"not an executable ELF\" end\
   local e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx = string_unpack(\"<I4I4I4I4I4I2I2I2I2I2I2\", header, 21)\
   if e_ehsize < 52 then return nil, \"main header in ELF too small\" end\
   if e_phentsize < 32 then return nil, \"program headers in ELF too small\" end\
   if e_shentsize < 40 then return nil, \"section headers in ELF too small\" end\
   if params.load_segment or params.write_word then\
      for n=1,e_phnum do\
         if not elf:seek(\"set\", e_phoff + e_phentsize * (n-1)) then\
            return nil, \"could not seek to program header within ELF\"\
         end\
         header = elf:read(32)\
         if not header or #header ~= 32 then\
            return nil, \"could not read program header within ELF\"\
         end\
         local p_type, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_flags, p_align = string_unpack(\"<I4I4I4I4I4I4I4I4\", header)\
         if p_type ~= 1 then\
            -- ignore segments other than PT_LOAD\
            goto continue\
         end\
         if p_filesz > p_memsz then return nil, \"ELF has a program section larger on disk than in memory?\" end\
         if p_vaddr % 4 ~= 0 then return nil, \"section not aligned to 4-byte boundary\" end\
         if not elf:seek(\"set\", p_offset) then\
            return nil, \"could not seek to program data within ELF\"\
         end\
         local disk_size = p_filesz\
         if disk_size % 4 ~= 0 then\
            -- HACK: read some extra bytes :/\
            disk_size = disk_size + 4 - (disk_size % 4)\
         end\
         local data = elf:read(disk_size)\
         if not data or #data ~= disk_size then\
            return nil, \"could not read program data within ELF\"\
         end\
         if params.load_segment then\
            params.load_segment(p_vaddr, p_paddr, p_memsz, data)\
         elseif params.write_word then\
            for n=1,#data,4 do\
               local offset = (n-1)\
               params.write_word(((p_paddr+offset) & (0xFFFFFFFF)), string_unpack(\"<I4\", data, n), 0xFFFFFFFF)\
            end\
            for offset=#data,p_memsz,4 do\
               params.write_word(((p_paddr+offset) & (0xFFFFFFFF)), 0, 0xFFFFFFFF)\
            end\
         else\
            error(\"UNREACHABLE\")\
         end\
         ::continue::\
      end\
   end\
   if params.learn_symbol then\
      local symtab_header, strtab_header\
      for n=1,e_shnum do\
         if not elf:seek(\"set\", e_shoff + e_shentsize * (n-1)) then\
            return nil, \"could not seek to section header within ELF\"\
         end\
         header = elf:read(40)\
         if not header or #header ~= 40 then\
            return nil, \"could not read section header within ELF\"\
         end\
         local sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize = string_unpack(\"<I4I4I4I4I4I4I4I4I4I4\", header)\
         if sh_type == 2 then\
            if symtab_header then\
               return nil, \"ELF contains multiple symbol tables!\"\
            end\
            symtab_header = {\
               sh_name=sh_name,\
               sh_type=sh_type,\
               sh_flags=sh_flags,\
               sh_addr=sh_addr,\
               sh_offset=sh_offset,\
               sh_size=sh_size,\
               sh_link=sh_link,\
               sh_info=sh_info,\
               sh_addralign=sh_addralign,\
               sh_entsize=sh_entsize,\
            }\
         elseif sh_type == 3 and strtab_header == nil then\
            -- if there is more than one string table, we only care about\
            -- the first one\
            strtab_header = {\
               sh_name=sh_name,\
               sh_type=sh_type,\
               sh_flags=sh_flags,\
               sh_addr=sh_addr,\
               sh_offset=sh_offset,\
               sh_size=sh_size,\
               sh_link=sh_link,\
               sh_info=sh_info,\
               sh_addralign=sh_addralign,\
               sh_entsize=sh_entsize,\
            }\
         end\
      end\
      if not symtab_header or not strtab_header then\
         goto no_syms\
      end\
      if not elf:seek(\"set\", strtab_header.sh_offset) then\
         return nil, \"unable to seek to the string table within the ELF\"\
      end\
      local raw_strtab = elf:read(strtab_header.sh_size)\
      if not raw_strtab then\
         return nil, \"unable to read the string table within the ELF\"\
      end\
      local symbols = {}\
      if not elf:seek(\"set\", symtab_header.sh_offset) then\
         return nil, \"unable to seek to the symbol table within the ELF\"\
      end\
      if symtab_header.sh_entsize ~= 16 then\
         return nil, \"symbols not 16 bytes? CONFUSING\"\
      end\
      for n = 0, symtab_header.sh_size-1, 16 do\
         local buf = elf:read(16)\
         if not buf or #buf ~= 16 then\
            return nil, \"unable to read a symbol from the ELF\"\
         end\
         local st_name, st_value, st_size, st_info, st_other, st_shndx = string_unpack(\"<I4I4I4I1I1I2\", buf)\
         if st_name ~= 0 then\
            local symbol_name = raw_strtab:match(\"[^\\0]+\", st_name+1)\
            if symbol_name and symbol_name ~= \"\" then\
               params.learn_symbol(symbol_name, st_value, st_size, st_info, st_other, st_shndx)\
            end\
         end\
      end\
   end\
   ::no_syms::\
   return e_entry\
end\
\
return rv32_elf\
","@rv32_elf.53.lua")()
else
    error("Your version of Lua is too old to use `rv32_elf`. Lua 5.2 or later is required.")
end
