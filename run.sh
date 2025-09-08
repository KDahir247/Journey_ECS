#!/bin/bash
set -e  # stop if any command fails

odin build journey_ecs.odin -file -debug -out:main-debug.bin
odin build journey_ecs.odin -file -out:main-release.bin
objdump -d ~/Documents/GitHub/Journey_ECS/main-debug.bin > ~/Documents/GitHub/Journey_ECS/assembly_dump-debug.asm
objdump -d ~/Documents/GitHub/Journey_ECS/main-release.bin > ~/Documents/GitHub/Journey_ECS/assembly_dump-release.asm
odin run journey_ecs.odin -file -debug  -define:CSV_DUMP=true



echo "✅ Build + disassembly complete. Output: assembly_dump.asm"
