[BITS 16]
[CPU 586]
;org 7c00h
;can't use org when using nasm and elfs

SECTION .text

_start:
xor ax, ax    ; make sure ds is set to 0
mov ds, ax
cld
; start putting in values:
mov ah, 2h    ; int13h function 2
mov al, 20    ; 20 x 512 bytes = 10240 bytes
mov ch, 0     ; from cylinder number 0
mov cl, 2     ; the sector number 2 - second sector (starts from 1, not 0)
mov dh, 0     ; head number 0
xor bx, bx    
mov es, bx    ; es should be 0
mov bx, 7e00h ; 512bytes from origin address 7c00h
int 13h
jmp _begin     ; jump to the next sector
   
   ; to fill this sector and make it bootable:
   times 510-($-$$) db 0 
   dw 0AA55h
   times 1024-($-$$) db 0
%include "BIOS.ASM"