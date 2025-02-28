;part of the hack that is required to get this to compile due to a nasm bug
hlt
times 0x10000-($-$$) db 0x90