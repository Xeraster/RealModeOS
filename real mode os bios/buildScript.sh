#!bin/bash
#use this one when compiling real mode os to use it as a bios rom instead of a bootloader-loaded system

nasm -i ../ bios.asm -o bios.bin
nasm padding.asm -o nasm_hack.bin

cat bios.bin >> nasm_hack.bin
