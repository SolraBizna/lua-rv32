#!/bin/sh

emacs=
for maybemacs in emacs-nox emacs; do
    if command -v $maybemacs >/dev/null; then
        emacs=$maybemacs
        break
    fi
done

if [ "x$emacs" = x ]; then
    echo "WARNING: emacs is not installed, we won't try to fix the files' indentation"
elif ! "$emacs" -batch -l src/format-file.el -f lua-mode >/dev/null 2>&1; then
    echo "WARNING: emacs is installed but lua-mode isn't, we won't try to fix the files' indentation"
    emacs=
fi

set -e

# "Just use ninja, Solra!" Yes, well...
function up_to_date {
    out=$1
    shift
    for dep in "$@"; do
        if [ "$dep" -nt "$out" ]; then
            return 1 # NOT up to date
        fi
    done
    return 0 # up to date
}

for v in 52 52.trace 53 53.trace; do
    if up_to_date lib/rv32.$v.lua src/rv32.src.lua src/macros.$v.lua src/macros.$(basename $v .trace).lua src/macros.common.lua src/demac.lua; then
        continue
    fi
    echo "Creating lib/rv32.$v.lua..."
    lua src/demac.lua src/rv32.src.lua src/macros.$v.lua lib/rv32.$v.lua
    if ! [ "x$emacs" = x ]; then
        echo "  Re-indenting lib/rv32.$v.lua..."
        "$emacs" -batch lib/rv32.$v.lua -l ../src/format-file.el -f format-file >/dev/null 2>&1 || true
        rm -f lib/rv32.$v.lua~
    fi
    echo "  Done!"
done
for v in 52 53; do
    if up_to_date lib/rv32_elf.$v.lua src/rv32_elf.src.lua src/macros.$v.lua src/macros.common.lua src/demac.lua; then
        continue
    fi
    echo "Creating lib/rv32_elf.$v.lua..."
    lua src/demac.lua src/rv32_elf.src.lua src/macros.$v.lua lib/rv32_elf.$v.lua
    if ! [ "x$emacs" = x ]; then
        echo "  Re-indenting lib/rv32_elf.$v.lua..."
        "$emacs" -batch lib/rv32_elf.$v.lua -l ../src/format-file.el -f format-file >/dev/null 2>&1 || true
        rm -f lib/rv32_elf.$v.lua~
    fi
    echo "  Done!"
done
if command -v luac5.2 >/dev/null; then
    echo "Checking syntax on Lua 5.2 version..."
    luac5.2 -p lib/rv32.52.lua
    luac5.2 -p lib/rv32.52.trace.lua
    luac5.2 -p lib/rv32_elf.52.lua
    echo "  OK!"
fi
for maybe53 in 5.3 53 5.4 54 ""; do
    if ! command -v lua$maybe53 >/dev/null; then
        continue
    fi
    if ! lua$maybe53 -e 'assert(_VERSION >= "Lua 5.3")' >/dev/null 2>&1; then
        continue
    fi
    echo "Checking syntax on Lua 5.3+ version..."
    luac$maybe53 -p lib/rv32.53.lua
    luac$maybe53 -p lib/rv32.53.trace.lua
    luac$maybe53 -p lib/rv32_elf.53.lua
    echo "  OK!"
    break
done

if ! up_to_date lib/rv32.lua src/rv32.mergetmpl.lua lib/rv32.52.lua lib/rv32.53.lua lib/rv32.52.trace.lua lib/rv32.53.trace.lua; then
    echo "Creating merged emulator core..."
    (cd lib; lua ../src/merge.lua ../src/rv32.mergetmpl.lua rv32.lua)
    echo "  OK!"
fi

if ! up_to_date lib/rv32_elf.lua src/rv32_elf.mergetmpl.lua lib/rv32_elf.52.lua lib/rv32_elf.53.lua; then
    echo "Creating merged ELF loader..."
    (cd lib; lua ../src/merge.lua ../src/rv32_elf.mergetmpl.lua rv32_elf.lua)
    echo "  OK!"
fi

echo ""
echo "All done!"
