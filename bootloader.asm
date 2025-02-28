;bootloader for booting real mode os from "normal" computers
org 7c00h

;bios data area bytes D0-EF are free real estate. use it for stuff
;D0 bit 1-0 = installed video card. 0 = vga. 1 = v9958. 2 = ice 40, 3 = other
;D0 bits 2-6: reserved. Nothing right now, but expect for this to do something else in the future
;D0 bit 7. if 1, keyboard port is at 61. if 0, it's a "normal" ps2 keyboard setup
mov bx, 0
mov es, bx
mov bx, 0x5D0
;mov al,[es:bx]
;and al, 11111100b
mov al, 0x00         ;0x00 because lazy
mov [es:bx], al      ;if it is being loaded from a bootloader at 07c00, it's almost certainly a vga

mov bx, 0x2000
mov ss, bx;make the 'stack segment' be zero. Maybe 0=within the first 64k of memory. Maybe 0=stack index 0 of 65535. Either way, doing this part first should make it work
mov ebp, 07FFCh;seems like a good spot
mov esp, ebp;arbitrary whatever location

xor ax, ax    ; make sure ds is set to 0
mov ds, ax
cld
; start putting in values:
mov ah, 2h    ; int13h function 2
mov al, 30    ; 30 x 512 bytes = 15360 bytes. Try to keep it under 30kb
mov ch, 0     ; from cylinder number 0
mov cl, 2     ; the sector number 2 - second sector (starts from 1, not 0)
mov dh, 0     ; head number 0
;xor bx, bx     ;don't remember what this does, it probably resets bx to zero in only 1 opcode
mov bx, 0x3000
mov es, bx    ; es should be 3000. 3000:0000 is where the system expects to be loaded on both normal x86 and homebrew x86 systems now
mov bx, 0     ;copy it to 0x3000:0000
int 13h
mov bx, 0x3000
mov ds, bx
jmp 0x3000:0x0000     ; jump to the next sector
   
   ; to fill this sector and make it bootable:
   times 510-($-$$) db 0 
   dw 0AA55h
