#!/bin/sh

set -e

cd ../..
lua src/demac.lua test/embench/embench.src.lua src/macros.52.lua test/embench/embench.52.lua
lua src/demac.lua test/embench/embench.src.lua src/macros.53.lua test/embench/embench.53.lua
chmod +x test/embench/embench.52.lua test/embench/embench.53.lua
