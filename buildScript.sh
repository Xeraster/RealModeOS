#!bin/bash

#/usr/bin/watcom/binl/wpp helloworld.cpp
#nasm BIOS.ASM -o bios.o
#nasm bootloader.asm -o bootloader.o

#nasm -g -f elf -o bootloader.o bootloader.asm
#nasm -g -f elf -o bios.o BIOS.ASM
nasm -f elf32 -o bootloader.o bootloader.asm
#nasm bootloader.asm -o bootloader.bin
#nasm -f elf32 -o bios.o BIOS.ASM

#ld -T link.ld
ld -melf_i386 -T link.ld
rm bootloader.bin -f
mv a.out bootloader.bin

#cat bios.bin >> bootloader.bin