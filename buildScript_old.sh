#!bin/bash

nasm BIOS.ASM -o bios.bin
nasm bootloader.asm -o bootloader.bin

cat bios.bin >> bootloader.bin