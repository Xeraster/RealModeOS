#!bin/bash

nasm system.asm -o bios.bin
nasm bootloader.asm -o bootloader.bin

cat bios.bin >> bootloader.bin