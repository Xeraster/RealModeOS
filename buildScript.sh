#!bin/bash

/usr/bin/watcom/binl/wpp helloworld.cpp
#nasm BIOS.ASM -o bios.o
#nasm bootloader.asm -o bootloader.o

#nasm -g -f elf -o bootloader.o bootloader.asm
#nasm -g -f elf -o bios.o BIOS.ASM
nasm -f elf -o bootloader.o bootloader.asm
nasm -f elf -o bios.o BIOS.ASM

#ld -T link.ld
ld -melf_i386 -T link.ld

#cat bios.bin >> bootloader.bin