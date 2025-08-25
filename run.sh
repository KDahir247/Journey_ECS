#!/bin/bash
set -e  # stop if any command fails

odin run journey_ecs.odin -file -debug -out:main.bin
objdump -d ~/Documents/GitHub/Journey_ECS/main.bin > ~/Documents/GitHub/Journey_ECS/assembly_dump.asm

echo "✅ Build + disassembly complete. Output: assembly_dump.asm"
