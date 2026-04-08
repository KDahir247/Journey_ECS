#!/bin/bash
set -e  # stop if any command fails

export ODIN_ROOT=/usr/lib/odin

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"


cache_size_bytes() {
  local s
  read -r s < "$1"
  case "$s" in
    *K) echo $(( ${s%K} << 10 )) ;;
    *M) echo $(( ${s%M} << 20 )) ;;
    *G) echo $(( ${s%G} << 30 )) ;;
    *)  echo "$s" ;;
  esac
}

L1D_SIZE=$(cache_size_bytes /sys/devices/system/cpu/cpu0/cache/index0/size)
L1D_LINE_SIZE=$(cat /sys/devices/system/cpu/cpu0/cache/index0/coherency_line_size)
L1D_ASSOC=$(cat /sys/devices/system/cpu/cpu0/cache/index0/ways_of_associativity)

L1I_SIZE=$(cache_size_bytes /sys/devices/system/cpu/cpu0/cache/index1/size)
L1I_LINE_SIZE=$(cat /sys/devices/system/cpu/cpu0/cache/index1/coherency_line_size)
L1I_ASSOC=$(cat /sys/devices/system/cpu/cpu0/cache/index1/ways_of_associativity)

L3_SIZE=$(cache_size_bytes /sys/devices/system/cpu/cpu0/cache/index3/size)
L3_LINE_SIZE=$(cat /sys/devices/system/cpu/cpu0/cache/index3/coherency_line_size)
L3_ASSOC=$(cat /sys/devices/system/cpu/cpu0/cache/index3/ways_of_associativity)


odin build journey_ecs.odin -file -debug -out:main-debug.bin
odin build journey_ecs.odin -file -o:size -microarch:native -out:main-release.bin
objdump -d ~/Documents/GitHub/Journey_ECS/main-debug.bin > $SCRIPT_DIR/assembly_dump-debug.asm
objdump -d ~/Documents/GitHub/Journey_ECS/main-release.bin > $SCRIPT_DIR/assembly_dump-release.asm

odin build journey_ecs.odin -file -debug -build-mode:llvm-ir

odin run journey_ecs.odin -file -debug -define:CSV_DUMP=true -define:CORE=$(getconf _NPROCESSORS_ONLN)

echo "✅ Build + disassembly complete. Output: assembly_dump.asm + assembly_dump-release.asm"

valgrind --tool=cachegrind --D1=$L1D_SIZE,$L1D_ASSOC,$L1D_LINE_SIZE --I1=$L1I_SIZE,$L1I_ASSOC,$L1I_LINE_SIZE --LL=$L3_SIZE,$L3_ASSOC,$L3_LINE_SIZE  --cache-sim=yes --branch-sim=yes --cachegrind-out-file=debug-cachegrind $SCRIPT_DIR/main-debug.bin
cg_annotate --threshold=0 debug-cachegrind > cachegrind_dump.txt

echo "annotation complete. Output: cachegrind_dump.txt"
