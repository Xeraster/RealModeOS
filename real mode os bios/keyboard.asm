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

    call kbd8042WaitWriteReady
    mov al, 0xAD
    out 0x61, al

    ;even if everything is actually working correctly you still shouldn't wait ready here
    ;call kbd8042WaitReadReady
    in al, 0x60     ;flush output buffer by reading a byte

    mov cx, 0xFF
    call longDelay

    call kbd8042WaitWriteReady
    mov al, 0x60    ;send the command to set the controller configuration byte
    out 0x61, al

    call kbd8042WaitWriteReady
    mov al, 00100100b
    out 0x61, al       ;configuration byte

    ;perform a controller self-test
    call kbd8042WaitWriteReady
    mov al, 0xAA
    out 0x61, al
    call kbd8042WaitReadReady
    in al, 0x60

    ;if it prints "55" to the screen, then at least some of it is working correctly
    call alToLcdHex

    ;peform a ps2 device test
    call kbd8042WaitWriteReady
    mov al, 0xAB
    out 0x61, al        ;the self-test command

    call kbd8042WaitReadReady
    in al, 0x60         ;read results of ps2 device test
    call alToLcdHex     ;print test results to the screen

    ;enable devices
    call kbd8042WaitWriteReady
    mov al, 0xAE
    out 0x61, al

    call kbd8042WaitWriteReady
    in al, 0x60
    call alToLcdHex

ret

kbd8042WaitReadReady:
;ld hl, $A005			;8042 status port
;ld a, (hl)
;and %00000001			;read the data register read ready bit
;cp 1
;jr nz, kbd8042WaitReadReady		;if it's not ready, keep waiting until it is

    mov dx, 0x61                    ;8042 status port   
    in al, dx
    and al, 00000001b              ;read the data register read ready bit
    cmp al, 1
    jne kbd8042WaitReadReady

ret

kbd8042WaitWriteReady:
    mov dx, 0x61
    in al, dx
    and al, 00000010b
    ;mov cl, 1
    ror al, 1              ;hopefully that's equivalent. can also use cl i think
    cmp al, 0
    jne kbd8042WaitWriteReady

;ld hl, $A005
;ld a, (hl)
;and %00000010
;rrc a
;cp 0
;jr nz, kbd8042WaitWriteReady
ret

;wait for the user to mash a button and then display all the resulting scancodes to the screen
;return = al has the scancode in it
waitScancode:
    ; i mean, I guess
    call kbd8042WaitReadReady

    ;if this works I will shit the fuck out of my pants in excitement
    in al, 0x60
    ;call alToLcdHex
ret

;puts scancode in al. copied from real mode os and then I changed ds registgers to es
waitKey:
        ;modifying this to work on xt scancode set instead of modern scancode set
        
        ;get scancode from keyboard
        call kbd8042WaitReadReady
        in al, 060h

        ;check to see if it's any of the shift keys
        cmp al, 0x2A
        je waitKeyLeftShiftOn
        cmp al, 0x36
        je waitKeyRightShiftOn
        cmp al, 0xAA
        je waitKeyLeftShiftOff
        cmp al, 0xB6
        je waitKeyRightShiftOff
        jmp waitKeyContinue

        ;had to change all the instances of ah to al due to what i think is a nasm bug
        waitKeyLeftShiftOn:
        push ax
        mov al, [es:capsState]
        or al, 00010000b
        mov [es:capsState], al
        pop ax
        jmp waitKeyContinue

        waitKeyRightShiftOn:
        push ax
        mov al, [es:capsState]
        or al, 00001000b
        mov [es:capsState], al
        pop ax
        jmp waitKeyContinue

        waitKeyLeftShiftOff:
        push ax
        mov al, [es:capsState]
        and al, 11101111b
        mov [es:capsState], al
        pop ax
        jmp waitKeyContinue

        waitKeyRightShiftOff:
        push ax
        mov al, [es:capsState]
        and al, 11110111b
        mov [es:capsState], al
        pop ax
        ;jmp waitKeyContinue

        ;the end of the part where I changed all the ah to al

        waitKeyContinue:
        ;is the scancode E0? if so, that means it's a 16 bit scancode. Wait for next code and then process it
        cmp al, 0xE0
        je waitForOtherShit

        ;is bit 8 of al set? If so, disregard it. If not, continue to treat it like a normal keypress
        push ax
        and al, 10000000b
        cmp al, 10000000b
        pop ax
        jz waitKey
        ret
        ;at this point, al contains the scancode

        waitForOtherShit:
        ;get scancode from keyboard
        call kbd8042WaitReadReady
        in al, 060h
        add al, 0x40

        cmp al, 0x88
        je waitKeyFuckOff
        cmp al, 0x8D
        je waitKeyFuckOff
        cmp al, 0x8B
        je waitKeyFuckOff
        cmp al, 0x90
        je waitKeyFuckOff
        cmp al, 0x92            ;insert key
        je waitKeyFuckOff

        ;if not arrow, change scancode to some value that doesn't correspond to an ascii value
        mov al, 29
        ret

        waitKeyFuckOff:

ret

;changes no registers most of the time. if al = scancode for arrowkeys, it changes al to 0
;copied from real mode os
processSpecialKeystrokes:

        ;if scancode for caps lock, toggle caps
        cmp al, 0x3A
        je processSpecialKeystrokesCaps
        cmp al, 0x45
        je processSpecialKeystrokesNum

        ret             ;don't do this for now, it contains vga hardware dependent code

        processSpecialKeystrokesArrows:
        cmp al, 0x88    ;up scancode 0x48
        ;je upArrow
        cmp al, 0x8B    ;left scancode 0x4B
        ;je leftArrow
        cmp al, 0x90    ;down scancode 0x50
        ;je downArrow
        cmp al, 0x8D    ;right scancode 0x4D
        ;je rightArrow
        cmp al, 0x92
        ;je insertKey
        ;uncomment the je jumps when ready to reactivate this feature

        ret

        processSpecialKeystrokesCaps:
        push ax
                call toggleCaps
        pop ax
        ret

        processSpecialKeystrokesNum:
        push ax
                call toggleNum
        pop ax

ret

;modifies al register
;toggles caps led on an 8042 compatible ps2 keyboard
toggleCaps:

        ;load caps state byte
        mov al, [es:capsState]
        ;if caps lock bit = 1, caps is set.
        and al, 00000001b
        cmp al, 0
        ;toggle caps lock state
        je toggleCapsOn
        jmp toggleCapsOff

        toggleCapsOn:
                ;store new caps lock state into ram
                mov al, [es:capsState]
                or al, 00000001b
                mov [es:capsState], al

                call updateKeyboardLedsBasedOnCapsState
                ;send the set lights command to keyboard
                ;call kbd8042WaitWriteReady
                ;mov al, 0xED
                ;out 0x60, al

                ;turn on caps led
                ;call kbd8042WaitWriteReady
                ;mov al, 00000100b
                ;out 0x60, al

        ret

        toggleCapsOff:
                ;store new caps lock state into ram
                mov al, [es:capsState]
                and al, 11111110b
                mov [es:capsState], al

                call updateKeyboardLedsBasedOnCapsState
                ;send the set lights command to keyboard
                ;call kbd8042WaitWriteReady
                ;mov al, 0xED
                ;out 0x60, al

                ;turn off caps led
                ;call kbd8042WaitWriteReady
                ;mov al, 00000100b
                ;out 0x60, al
ret

toggleNum:
        ;load caps state byte
        mov al, [es:capsState]
        ;if caps lock bit = 1, caps is set.
        and al, 00000010b
        cmp al, 0
        ;toggle caps lock state
        je toggleNumOn
        jmp toggleNumOff

        toggleNumOn:
                ;store new caps lock state into ram
                mov al, [es:capsState]
                or al, 00000010b
                mov [es:capsState], al

                call updateKeyboardLedsBasedOnCapsState

        ret

        toggleNumOff:
                ;store new caps lock state into ram
                mov al, [es:capsState]
                and al, 11111101b
                mov [es:capsState], al

                call updateKeyboardLedsBasedOnCapsState

ret

updateKeyboardLedsBasedOnCapsState:
        
        ;move caps bit to 3rd position
        mov al, [es:capsState]
        and al, 00000001b
        shl al, 2

        mov ah, al  ;also had to employ the ah workaround hack here to circumvent nasm bug. shit, at this point I should just be fucking using gcc 
        ;(im trying to hold out on that until I have a video card for better troubleshooting capability when it inevitably ends up being one of those things I have to go through hell and back to get to work)

        ;aquire information about the num lock
        mov al, [es:capsState]
        and al, 00000010b

        ;cool, now merge the num lock and caps lock byte into 1 byte
        or al, ah
        ;al now contains the correct byte for the keyboard controller's led byte
        ;it is ready to be sent to the keyboard with the ED command

        push ax
        ;send the set lights command to keyboard
        call kbd8042WaitWriteReady
        mov al, 0xED
        out 0x60, al

        ;send the byte we just calculated
        call kbd8042WaitWriteReady
        pop ax
        out 0x60, al

        ;done


ret

;silent but deadly. wait, that doesnt make any sense.
waitAck:
        call kbd8042WaitReadReady
        in al, 060h
        cmp al, 0FAh
        jne waitAck

ret

;inputs: al contains an xt scancode set make code. ah = 0 for lowercase. ah > 0 for uppercase/shift characters
;post condition: al contains the ascii code of whatever the scancode was
;modifies ah, al and bx
scancodeToAscii:
        ;if ah = 0, use lowercase. if ah > 0, use uppercase/shift
        cmp ah, 0
        jna scancodeToAsciiMakeLower

        scancodeToAsciiMakeUpper:
        mov bx, uppercase
        jmp scancodeToAsciiContinue

        scancodeToAsciiMakeLower:
        mov bx, lowercase

        scancodeToAsciiContinue:
        mov ah, 0
        add bx, ax
        dec bx                  ;position zero is scancode 0x01. There isn't a scancode 0
        mov al, [es:bx]
        ret

ret

;send a command to the keyboard to get the scan code set
getscancodeset:
    ;send the scancode command 0xF0
    call kbd8042WaitWriteReady
    mov al, 0xF0
    out 0x60, al

    ;wait for the acknoledge byte
    call waitAck

    ;send value 0 for the "get scancode set" sub command
    call kbd8042WaitWriteReady
    mov al, 0
    out 0x60, al

    ;wait for ack
    call waitAck

    ;hopefully this will get me a value or something
    call kbd8042WaitReadReady
    in al, 0x60

    call alToLcdHex
ret

;set it to a different scancode, 1 2 or 3. use al
setscancodeset:
    push ax
        ;send the scancode command 0xF0
        call kbd8042WaitWriteReady
        mov al, 0xF0
        out 0x60, al

        ;wait for the acknoledge byte
        call waitAck

        ;send value 0 for the "get scancode set" sub command
        call kbd8042WaitWriteReady
    pop ax
    out 0x60, al
ret

;get command byte and put it in al
get8042CommandByte:

    ;send the configuration byte command
    call kbd8042WaitWriteReady
    mov al, 0x20                ;read configuration byte
    out 0x61, al                ;port 61h instead of 64h because i fucked up when making the pcb

    call kbd8042WaitReadReady
    in al, 0x61

ret

;bit 0 = caps. bit 1 = num lock. bit 2 = scroll lock. bit 3 = right shift. bit 4 = left shift. bit 5 = insert
capsState db 0

;scancode tables
lowercase db 0x1B,"1234567890-=",0x08,0x09,"qwertyuiop[]",0x10,0,"asdfghjkl;'`",0,"\zxcvbnm,./",0,0,0," ",0,0,0,0,0,0,0,0,0,0,0,0,0,"789-","456","+1230",0,0,0,0,0,0,0,0,0,0,0,0,"/",0x08,0
compatzeros1 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compatzeros2 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
uppercase db 0x1B,"!@#$%^&*()_+",0x08,0x09,"QWERTYUIOP{}",0x10,0,"ASDFGHJKL:",0x22,"~",0,"|ZXCVBNM<>?",0,0,0," ",0,0,0,0,0,0,0,0,0,0,0,0,0,"789-","456","+1230",0,0,0,0,0,0,0,0,0,0,0,0,"/",0x08,0
compatzeros3 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compatzeros4 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;still using the "Compatibility zeros" method of achieving simpler keyboard driver code used in my z80 bios. I have 128kb of rom to work with here, I can afford this lazy fix