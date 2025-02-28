;use this command to program to the rom
;minipro -p AM29F010@DIP32 -w bios.bin
CPU 486
;BITS 32
;start of rom

;due to nasm bugs, hacks are required to get this to work
org 0xFFFF0000
;hlt         ;halt if it gets here for some reason
    
;times 0x10000-($-$$) db 0x90
;times 0x10000-($-$$) db 0x90
;org 0x0000
;=====================
;code here
;https://www.pjrc.com/tech/8051/board5/lcd_example.html
start:
    cli ;fuck interrupts
    ;we need to establish a stack so call and ret instructions can work
    mov bx, 0x2000
    mov ss, bx;make the 'stack segment' be zero. Maybe 0=within the first 64k of memory. Maybe 0=stack index 0 of 65535. Either way, doing this part first should make it work
    mov ebp, 07FFCh;top of the 32kb sram chip
    mov esp, ebp;arbitrary whatever location

    mov ax, 0x3000
    mov ds, ax

    ;bios data area bytes D0-EF are free real estate. use it for stuff
    ;D0 bit 1-0 = installed video card. 0 = vga. 1 = v9958. 2 = ice 40, 3 = other
    mov bx, 0
    mov es, bx
    mov bx, 0x5D0
    ;mov al,[es:bx]
    ;and al, 11111100b
    mov al, 0x81         ;0x81. bit 7 when set = non-normal keyboard layout. bit 0-1 when set to 1 means v9958 video card 
    mov [es:bx], al   ;there, now real mode os should have all the information it needs to use v9958 instead

    
    printTest:
    ;mov cx, 01000h           ;roughly a half second
    mov cx, 0xFF            ;if my calculations are correct, 0xFF should be plenty of time at 1MHz (shouldn't be required but, you know, *gestures broadly at everything*)
    call longDelayBios


    ;output to port E9 to indicate the end of this part on the debugger
    mov dx, 0xE9
    mov al, 69
    out dx, al
    
    call setupLcd

    mov bx, keyboardLabel
    call printStringLcd
    call setupPS2Controller
    
    mov bx, doneLabel
    call printStringLcd

    ;jump to real mode os HERE
    call realmodeosstart
    hlt

    call waitForLcdReady
    mov al, 00000010b           ;return home command
    mov dx, 0A000h
    out dx, al

    hlt

;remaking this (happy 4/20)
lcdBackspace:
    ;get address counter
    mov dx, 0xA001
    in al, dx
    and al, 01111111b       ;bit 7 is the busy flag, we actually don't need that here
    ;decrement address by 1
    dec al
    mov cl, al
    push cx
        push ax
            call waitForLcdReady
        pop ax
        or al, 10000000b
        mov dx, 0xA000
        out dx, al
        ;set it to space
        push ax
            call waitForLcdReady
        pop ax
        mov dx, 0xA002
        mov al, ' '             ;space ascii code
        out dx, al
    ;go back 1 addess again
    pop cx
    push ax
        call waitForLcdReady
    pop ax
    mov al, cl
    or al, 10000000b
    mov dx, 0xA000
    out dx, al
    ;done
ret

setupLcd:
    mov cx, 0500h              ;so long as cx never gets modified, you only have to set it once
    call longDelayBios

    ;set lcd to 8 bit 2 line mode
    ;call waitForLcdReady
    mov al, 00111000b
    mov dx, 0A000h
    out dx, al

    call longDelayBios

    ;turn display on
    ;call waitForLcdReady
    mov al, 00001110b
    mov dx, 0A000h
    out dx, al

    call longDelayBios

    ;set entry mode to the datasheet example default one
    ;call waitForLcdReady
    mov al, 00000110b
    mov dx, 0A000h
    out dx, al

    ;get the lcd ram counter to reset to zero
    call waitForLcdReady
    mov al, 10000000b          ;reset display data address in address coutner to 0
    mov al, 00000010b           ;return home command
    mov dx, 0A000h
    out dx, al
ret

;pre-conditions: none
;post-conditions: the lcd is 100% for sure ready for a command
;registers changed: al
waitForLcdReady:
    ;ld hl, $A001			;load the address of port 01b
    ;ld a, (hl)				;save the result to the a register
    ;and a, %10000000		;the busy flag at byte 7 is the only thing I care about right now
    ;cp %10000000			;if z = 0, the lcd is busy. is z != 1, the lcd is not busy
    ;jp z, waitForLcdReady	;if lcd is busy, do this again until it isn't busy anymore
    mov dx, 0A001h
    in al, dx               ;load the address of whichever port carries the ready signal and save to register al
    and al, 10000000b       ;the busy flag at byte 7 is the only thing I care about right now
    cmp al, 10000000b       
    je waitForLcdReady      ;if bit 7 is 1, the device is not ready. Loop until bit 7 is not 1.
ret

;cx needs to contain the number of wait loops
longDelayBios:
    mov bx, 0
    longDelayBiosContinue:
    inc bx
    cmp bx, cx
    jne longDelayBiosContinue
ret

;modified print string subroutine from real mode os
;bx = location in *ds* segment of string (wow so much for the possibility of that actually working)
;print location is in cursor values cursorX and cursorY in ram
printStringLcd:
        ;use printChar to reduce repeated code
        ;push bx
        ;push cx
        ;        call calculateCursorValues
        ;pop cx
        ;pop bx
        ;aint nobody got time for that

        mov al, [cs:bx]
        cmp al, 0           ;make everything null terminated character arrays because that's better
        je printStringLcdGTFO
        call charToLcd
        inc bx
        jmp printStringLcd

        printStringLcdGTFO:

ret

;put whatever's in al to the 20x2 lcd screen
charToLcd:
    push ax
        nop
        nop
        nop
        nop
        call waitForLcdReady
        nop
        nop
        nop
        nop
    pop ax
    nop
    nop
    nop
    nop
    mov dx, 0A002h
    out dx, al
ret

;converts whatever 4 bit value is in the al register to the
;valid ascii code for that respective number in hex
;"stolen" from real most os which was stolen from z80 bios
alToHexBios:
        add al, 48
        cmp al, 58
        jae Add7MoreBios
        jmp DontAdd7MoreBios

        Add7MoreBios:
                add al, 7
        DontAdd7MoreBios:

ret

;maybe try this one for printing 8 bit hex values to the lcd
alToLcdHex:
        push ax
        and al, 11110000b
        mov cl, 4
        ror al, cl
        call alToHexBios

        ;set color to white (00001111b) and then print al as char
        ;mov cl, 00001111b      ;20x2 lcd is monochrome
        call charToLcd
        pop ax
        
        ;set color to white (00001111b) and then print al as char
        and al, 00001111b
        call alToHexBios
        ;mov cl, 00001111b      ;20x2 lcd is monochrome
        call charToLcd

ret

;i'll inevitably fuck it up printing a 16 bit value manually. Better make a complete subroutine for that
axToLcdHex:
    push ax
        xchg al, ah ;high byte first for readability
        call alToLcdHex
    pop ax
    nop
    nop
    nop
    nop
    call alToLcdHex
ret

;also i fucked up the wiring so the ports are 60h-67h where even addresses = port 60h and odd addresses = port 64 (sorry)
;run this once while a keyboard is plugged in and then the keyboard should be ready to use keyboard functions directly copy-pasted from real mode os
setupPS2Controller:
    ;pretty much just hand-translated from z80 bios

    ;do whatever the fuck this does
    ;call kbd8042WaitWriteReady
    ;mov al, 0x20
    ;out 0x61, al

    ;delay for a while? nah, my chipset inserts a lot of wait states
    ;print whatever number the ps2 controller pooped out
    ;mov cx, 0xFF            ;do a little delay
    ;call longDelay

    ;in al, 0x60
    ;call alToLcdHex     ;get that shit on the screen
    ;set the controller configuration byte and be sure to enable port 1 clock
    ;call kbd8042WaitWriteReady
    ;mov al, 00100100b
    ;out 0x61, al

    ;call kbd8042WaitWriteReady
    ;mov al, 0x20
    ;out 0x61, al

    ;mov al, 0x61
    ;call alToLcdHex

    call kbd8042WaitWriteReadyBios
    mov al, 0xAD
    out 0x61, al

    ;even if everything is actually working correctly you still shouldn't wait ready here
    ;call kbd8042WaitReadReady
    in al, 0x60     ;flush output buffer by reading a byte

    mov cx, 0xFF
    call longDelayBios

    call kbd8042WaitWriteReadyBios
    mov al, 0x60    ;send the command to set the controller configuration byte
    out 0x61, al

    call kbd8042WaitWriteReadyBios
    mov al, 00100100b
    out 0x61, al       ;configuration byte

    ;perform a controller self-test
    call kbd8042WaitWriteReadyBios
    mov al, 0xAA
    out 0x61, al
    call kbd8042WaitReadReadyBios
    in al, 0x60

    ;if it prints "55" to the screen, then at least some of it is working correctly
    call alToLcdHex

    ;peform a ps2 device test
    call kbd8042WaitWriteReadyBios
    mov al, 0xAB
    out 0x61, al        ;the self-test command

    call kbd8042WaitReadReadyBios
    in al, 0x60         ;read results of ps2 device test
    call alToLcdHex     ;print test results to the screen

    ;enable devices
    call kbd8042WaitWriteReadyBios
    mov al, 0xAE
    out 0x61, al

    call kbd8042WaitWriteReadyBios
    in al, 0x60
    call alToLcdHex

ret

kbd8042WaitReadReadyBios:
;ld hl, $A005			;8042 status port
;ld a, (hl)
;and %00000001			;read the data register read ready bit
;cp 1
;jr nz, kbd8042WaitReadReady		;if it's not ready, keep waiting until it is

    mov dx, 0x61                    ;8042 status port   
    in al, dx
    and al, 00000001b              ;read the data register read ready bit
    cmp al, 1
    jne kbd8042WaitReadReadyBios

ret

kbd8042WaitWriteReadyBios:
    mov dx, 0x61
    in al, dx
    and al, 00000010b
    ;mov cl, 1
    ror al, 1              ;hopefully that's equivalent. can also use cl i think
    cmp al, 0
    jne kbd8042WaitWriteReadyBios

;ld hl, $A005
;ld a, (hl)
;and %00000010
;rrc a
;cp 0
;jr nz, kbd8042WaitWriteReady
ret


;copies the bios into ram and then jumps there
;(it actually works believe it or not)
biosToRam:
    ;we need to establish a stack so call and ret instructions can work
    mov bx, 0x2000
    mov ss, bx;make the 'stack segment' be zero. Maybe 0=within the first 64k of memory. Maybe 0=stack index 0 of 65535. Either way, doing this part first should make it work
    mov ebp, 07FFFh;32kb seems like a good spot
    mov esp, ebp;set stack pointer to top of stack
    ;finally. call and return can work

    mov bx, 0x3000
    mov es, bx      ;get 0x1000 into the es register. copy rom data to ram starting at address 0x10000
    mov bx, 0       ;set bx to zero I guess
    biosToRamContinue:
    ;nop
    ;nop
    ;nop
    ;nop
    mov al, [cs:bx]     ;copy the selected byte to al register (yes, I know this can be done in 16 and 32 bit transfers, the hardware sucks and I want this to be as easy on it as possible)
    ;nop
    ;nop
    ;nop
    ;nop
    mov [es:bx], al     ;copy the al register to the new place
    ;nop
    ;nop
    ;nop
    ;nop
    ;mov cl, [es:bx]	;read the same byte back into cl to make it easier to check if it worked with a signal analyzer
    ;nop
    ;nop
    ;nop
    ;nop
    inc bx
    cmp bx, 0xFFFF	        ;I need the whole thing
    jne biosToRamContinue

    ;output to port E9 to indicate the end of this part on the debugger
    mov dx, 0xE9
    mov al, 69
    out dx, al
    
    ;set cs change through the jmp instruction or else it will get all fucked up
    ;mov bx, 0x1000
    ;mov ds, bx
    ;mov cs, bx
    ;mov bx, 0
    
    jmp 0x3000:0000         ;jump to where the new code is hopefully stored


helloWorldString db 'hello world 486',0
theWordFuck db 'fuuuuuck',0
codeString db 'end of bios code',0
failString db 'mem test failed',0
sucessString db 'mem test pass',0
memTestingStarted db 'test:',0
testNumString db 'test = ',0
holyshit db 'HOLY SHIT IT WORKED!',0
error db 'generic error',0
keyboardLabel db 'ps2 ',0
doneLabel db 'done',0
videoDoneLabel db 'vd',0
biosWelcomeMessage db 'Scott',0x27,'s 486 bios',0


disableCache:
mov eax, cr0
or eax, 1<<30        ; Set bit CD
and eax, ~(1<<29)    ; Clear bit NW
mov cr0, eax
ret

%include "system.asm"

;======================
;pad the rest with nops
times 0x0FFF0-($-$$) db 0x90

;top of rom where the system boots from
;org 0xFFFFFFF0
nop
nop         ;I saw 2 nops before the short x86 jump in a book one time. No idea why or if that changes anything
;jmp start
jmp biosToRam               ;if this works I will throw a fucking party
;codeLastString db 'end'
;pad the rest with nops
times 0x0FFFF-($-$$)+1 db 0x90 
