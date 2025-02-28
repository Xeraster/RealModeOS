![Alt text](realmodeos_screenshot.png?raw=true "a screenshot of the system when it boots")
# How to build
You will need nasm and if you want to run it without booting it on hardware you need qemu (bochs also works).

Run "sh buildScript.sh".

This will create a self-bootable file called bootloader.bin. You can verify that it worked by getting qemu and running the newly generated binary with the command "qemu-system-i386 bootloader.bin".

# Installation
If trying to run this on a physical computer, you can copy this thing directly to the hard drive starting at sector 0. You won't get any filesystem this way but real mode os doesn't support any filesystems anyway. You could probably also set up a dual boot by manually copying this to the start location of the secondary partition and getting a basic bootloader selection screen.

I imagine it's more practical for most people to modify the bootloader.asm file to be a msdos program and make real mode os a ms-dos executable. In bootloader.asm, change the "org 7c00h" to "org 100h", throw out all the hard drive interrupt vector stuff and then change the jump location to whereever the rest of the program will be loaded. I've done this before, I just don't have example code right now.

# But why though?
The only reason I still use this is because I have a homebrew x86 computer and this is what runs on the bios rom chip. It could be used as a starting point for making more complex stuff if desired.

There's also a way to get basic ata stuff, but if I never write a fat32 file explorer in assembly again it'll be too soon. (see my z80 kernel)

# Features
- basic vga suppot (mode 3h only)
- v9958 support (only 1 x86 computer with a v9958 video card was ever built and its mine, so this isn't relevant to most people)
- the ability to use ps/2 keyboard controllers at i/o locations besides 0x60 and 0x64. Modify kbdDataPort and kbdHighPort before compiling if using this, but for 99.9999999% of users its best to leave that setting the way it is. I only made this because I got the bus wiring wrong on that homnebrew 486 system to where the ps/2 was at 0x60 and 0x61 instead of 0x60 and 0x64.
- The text cursor position changing with the arrow keys mostly works.

# Commands
There are other mostly useless testing commands but here are the useful ones:
- help. Prints a list of commands you can use.
- read. it needs real mode segment and address syntax. For example "read 0000:0000" reads the lowest location in memory. "read F000:FFFF" reads memory location 0xFFFFF. Note that you get a full-screen table of hex and ascii values when you do this.
- write. example "write 0000:0003 FF" writes the byte 0xFF to memory location 0x00003.
- clear. clears the screen.
- jump. jump to a location in memory and tell the cpu to start executing that as an instruction.  Reboot the system with "jump 0000:7c00".
- call. same as the jump command except it uses the call opcode.
- pciinfo. Gives you basic information of the host system's pci bus, if present.
