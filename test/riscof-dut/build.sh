#!/bin/sh

set -e

cd ../..
lua src/demac.lua test/riscof-dut/riscof-dut.src.lua src/macros.52.lua test/riscof-dut/riscof-dut.52.lua
lua src/demac.lua test/riscof-dut/riscof-dut.src.lua src/macros.53.lua test/riscof-dut/riscof-dut.53.lua
chmod +x test/riscof-dut/riscof-dut.52.lua test/riscof-dut/riscof-dut.53.lua
