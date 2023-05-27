SECTION .text
[CPU 586];I need this to be a 486 with the cpuid instruction, so setting it to 586 is close enough

;note: CGA AND EGA graphics cards are not supported
;if anyone is seriously bummed out about that, too bad. ega and cga sucks bad. the vga standard is just a glorified and overly complex framebuffering scheme anyway. 
;Just connect some discrete logic or a cheap cpld to 2 bank switched ram chips (flip flop each bank between host system and vga monitor control) and put that on an isa or slpc card, you'll get better performance that way anyway

;ORG 7e00h
;can't use org when using nasm and elfs

start:

;call kbd8042WaitReadReady
;in al, 060h

;write zeros to the entire screen
;call clearScreen
;clear interupt flag (to prevent dos from fucking this up)
cli

call setupBlankScreen

mov ax, 0B800h
mov es, ax

;apparently B8F00h is the location of the bottom line where dos program output should print to
;lines are 140d or A0h apart
mov cl, 0
call setCurX
mov cl, 0
call setCurY
mov bx, initString
mov cl, 00000110b
call printString


mov cl, 0
call setCurX
mov cl, 1
call setCurY
;mov di, 0140h
mov bx, cpuidk
;mov al, [ds:bx]
mov cl, 00001111b
call printString
call printCPUID

;set graphics mode to mode 10h
;remember 10h and 12h use bit planes
;mode 13h doesn't use that bit plane crap but its only 320x200
;mov ah, 00h
;mov al, 0Dh
;int 10h

;whatever, as retarded as it is, its best to just use the bit mask business instead of bit plane switching
;mov cl, 00001111b
;call vga_maskMap

;mov cl, 0
;call vga_ChangeWriteMode

;mov ah, 00h
;mov al, 03h
;int 10h
;call setupBlankScreen
;mov ax, 0B800h
;mov es, ax

;mov al, cl
;push cx
;call alToScreenHex
;pop cx
;mov al, ch
;call alToScreenHex

;set page to page 0 because why not
;mov al, 0h
;mov ah, 05h
;int 10h

;;palette register stuff, works for graphics and text modes, presumably for anything that runs in a 16 color vga mode
;mov ah, 0
;mov al, 00100100b
;call vga_editPaletteReg

;mov ah, 1
;mov al, 00010010b
;call vga_editPaletteReg

;mov ah, 2
;mov al, 00010010b
;call vga_editPaletteReg

;mov ah, 3
;mov al, 00001001b
;call vga_editPaletteReg

;mov ah, 4
;mov al, 00000010b
;call vga_editPaletteReg

;mov ah, 5
;mov al, 00000011b
;call vga_editPaletteReg

;mov ah, 6
;mov al, 00001000b
;call vga_editPaletteReg

;call textModeNewLine
;call vga_getModeControl


;send configuration byte that can enable the mouse
;call kbd8042WaitWriteReady
;mov al, 060h    ;configuration byte set command
;out 064h, al
;call kbd8042WaitWriteReady
;mov al, 01000111b       ;set the configuration byte to a thing that should enable the mouse
;out 064h, al

;call kbd8042WaitWriteReady
;mov al, 0A9h
;out 064h, al
;call kbd8042WaitReadReady
;in al, 060h
;call alToScreenHex

;the test results of the 2nd ps2 port should now be on the screen
;00 means test passed sucessfully

;now, let's enable the 2nd ps2 port and see what happens
;call kbd8042WaitWriteReady
;mov al, 0A8h
;out 064h, al
;should now be enabled

;print the letter G to let me know where its getting stuck
;mov al, "G"
;mov cl, 00000010b
;call printChar

;send identify command to mouse
;call kbd8042WaitWriteReady
;mov al, 0D4h
;out 064h, al

;print the letter G to let me know where its getting stuck
;mov al, "G"
;mov cl, 00000010b
;call printChar

;call kbd8042WaitWriteReady
;mov al, 0F4h
;out 064h, al

;print the letter G to let me know where its getting stuck
;mov al, "G"
;mov cl, 00000010b
;call printChar

;call kbd8042WaitReadReady

;call waitAck
;in al, 060h
;call alToScreenHex

;print the letter G to let me know where its getting stuck
;mov al, "G"
;mov cl, 00000010b
;call printChar

;call kbd8042WaitReadReady
;in al, 060h
;call alToScreenHex
call textModeNewLine

;wait for a keypress before returning to parent subroutine
keyLoop:
call waitKey
call processSpecialKeystrokes
push ax
;load caps lock value into ah before converting to ascii
mov ah, [ds:capsState]
and ah, 00011001b

;actually do the conversion of the keyboard scancode to an ascii character
call scancodeToAscii

push ax
        ;don't shift characters right if it's a "garbage character"
        cmp al, 32
        jb dontShiftCharsRight 
        ;if inset key mode disengaged, shift text right 1 space

        ;check insert bit
        mov ax, [ds:capsState]
        and al, 00100000b
        cmp al, 0

        ;if insert bit = 1, it's in insert mode. Don't shift characters right
        jne dontShiftCharsRight
        ;if insert bit = 0, shift characters right 1 position
        call insertionScreenScroll
        dontShiftCharsRight:
pop ax

;call alToScreenHex
mov cl, 00001111b
;call alToScreenHex
call printChar
pop ax
cmp al, 0x01
jne keyLoop

;restore interrupt flag
sti

mov ah, 4ch
int 21h

;parameters, cl = color. bx = location in ds segment of string
;print location is in cursor values cursorX and cursorY in ram
printString:
        ;use printChar to reduce repeated code
        push bx
        push cx
                call calculateCursorValues
        pop cx
        pop bx
        mov al, [ds:bx]
        cmp al, '$'
        je printStringGTFO
        push cx
        call printChar
        pop cx
        inc bx
        jmp printString

        printStringGTFO:

ret

;parameters: cl = color. al = char.
printChar:

        ;if video mode (as measured by location 0000:0449) != 3, do the graphics mode shit
        push bx
                mov bx, 0
                mov es, bx
                mov bx, 0449h
                mov ah, [es:bx]
        pop bx
        cmp ah, 03h
        jne printChar10

        printChar03:
        push ax
                mov ax, 0B800h
                mov es, ax
        pop ax
        ;don't try to print garbage. GTFO if ascii code is less than 32 (lower than spacebar). All the stuff under ascii code 32 is non-character stuff
        cmp al, 32
        jb printCharDontPrintGarbage

        printCharNoShift:
        ;print the character byte
        mov [es:di], al
        inc di

        ;print the color byte
        mov al, cl
        mov [es:di], al
        inc di

        call incrementCursor
        push ax
        push bx
        push dx
        call calculateCursorValues
        pop dx
        pop bx
        pop ax
        ;;
        ;;
        ;;
        ret

        printChar10:
                call vga_printChar

ret

        printCharDontPrintGarbage:
        
        ;check if backspace
        cmp al, 0x08
        je textModeBackspace
        
        ;check if tab
        cmp al, 0x09
        je textModeTab
        
        ;check if enter
        cmp al, 0x10
        je textModeEnter


ret

calculateCursorValues:
        ;calculate the correct line number from cursorY
        mov al, [cursorY]
        mov ah, 0
        mov bx, 00A0h
        mul bx
        mov di, ax

        ;add the column number in cursorX to di so that the character prints to the correct column
        mov al, [cursorX]
        mov ah, 0
        mov bx, 2
        mul bx
        add di, ax

        ;make it so that the vga cursor reflects these values
        call setCursorPosBasedOnData

ret


setCursorPosBasedOnData:
        
;;================================================================================================
;      ~Welcome to the cursor positioning code.~ 
;      The first big fuck of this entire program (and of many to come, I'm sure)
;
;        ;use 3D4-3D5 to access CRTC registers if in a non-monochrome
;        ;3D4 = address
;        ;3D5 = data
;
;===========================================================================================
        ;set CRTC address register to cursor location high register 0x0E
        mov al, 0x0E
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;calculate cursor values
        mov bl, [ds:cursorY]
        mov bh, 0
        mov al, 80
        mov ah, 0

        ;cursor Y pos * characters per row
        mul bx
        
        ;now, add the number of spaces in the current row to what we've calculated so far
        mov bl, [ds:cursorX]
        mov bh, 0
        add ax, bx

        ;increment that number by 1 so the cursor will always be right in front of the most recently typed character
        ;inc ax
        ;ax should now contain the correct cursor offset

        ;time to set the crtc cursor start address high register
        mov dx, [ds:CRTCData]
        push ax
                ;write the high byte first, exchange al and ah since you can only use al in the 8 bit out instruction
                xchg al, ah
                out dx, al

                ;increment crtc to set the start address low register
                mov al, 0x0F
                mov dx, [ds:CRTCAdr]
                ;call exchangeAlAh
                out dx, al

                ;ah has been unchanged and it contains the low byte. do the exchange instruction again to exchange al and ah
                xchg ah, al

                ;obtain the correct io port address and write the low byte to the cursor low pos register
                mov dx, [ds:CRTCData]
                out dx, al
        pop ax

        ;=====================================================================
        ;there. That should hopefully be it. Damn that sucks to write in assembly
        
        

ret

;A good way to blank a mode 3 text screen and set up attributes to be good and stuff
setupBlankScreen:
        
        mov ax, 0B800h
        mov es, ax
        mov di, 0

        mov cx, 2048
        setupBlankScreenLoop:

        ;write a null character to the character byte
        mov al, 0
        mov [es:di], al
        
        ;increment address counter
        inc di
        
        ;write to attribute byte. black background. white foreground
        mov al, 00001111b
        mov [es:di], al
        
        ;increment address counter, decrement loop counter
        inc di
        dec cx

        cmp cx, 0
        ja setupBlankScreenLoop

ret

;increments cursor in 80 column text 3 mode
;only changes cx register to useless bullcrap. preserves others
incrementCursor:

        ;check if the character line count this has exceeded 80
        mov cl, [ds:cursorX]
        cmp cl, 79
        jae makeNewLine

        ;if count not exceeded, increment count by 1
        inc cl
        mov [ds:cursorX], cl

        ret

        ;if count exceeded, set cursorX to zero then increment cursorY
        makeNewLine:
        push bx
                call textModeNewLine
        pop bx

ret

decrementCursor:
        mov cl, [ds:cursorX]
        cmp cl, 0
        je fuck
        dec cl
        mov [ds:cursorX], cl
        ret

        ;set cursorx to 1 less than its max and decrement cursor y by 1
        fuck:
        mov cl, 79
        mov [ds:cursorX], cl
        mov cl, [ds:cursorY]
        dec cl
        mov [ds:cursorY], cl


ret

;set whatever is in cl to cursorX
;modifies bx
setCurX:
        mov bx, cursorX
        mov [ds:bx], cl
ret

;set whatever is in cl to cursorY
;modifies bx
setCurY:
        mov bx, cursorY
        mov [ds:bx], cl
ret

;nukes bx, and cl register
textModeNewLine:
        ;set x position to zero
        mov bx, cursorX
        mov cl, 0
        mov [ds:bx], cl

        ;increment y position by 1
        mov cl, [ds:cursorY]
        inc cl
        mov bx, cursorY
        mov [ds:bx], cl

        ;call setCursorPosBasedOnData
        ;call calculateCursorValues

        ;check if cursor y position is equal to or greater than 23 and if so, scroll the screen by 2 lines
        mov al, [ds:cursorY]
        cmp al, 23
        ;if there are less than 23 rows of used text, there is no need to do any scrolling
        jl textModeNewLineExit

        ;set cl for 2 rows of scrolling
        mov cl, 2
        call scrollScreen

        ;be sure to update the blinking cursor values and stuff
        mov al, [ds:cursorY]
        sub al, 2
        mov [ds:cursorY], al

        textModeNewLineExit:
        ;for things to show up correctly, recalculate cursor values whether or not the screen got moveda
        call calculateCursorValues
ret

textModeBackspace:
        
        push ax
                mov al, [ds:capsState]
                and al, 00100000b
                cmp al, 0
        pop ax
        je textModeBackspaceNoInsert
        jmp textModeBackspaceInsert

        textModeBackspaceNoInsert:
                call deleteAndScrollLeft

        ret

        textModeBackspaceInsert:
        ;first decrement cursorX
        call decrementCursor
        call calculateCursorValues
        mov al, ' '
        mov cl, 00001111b
        call printChar
        call decrementCursor
        ;call setCursorPosBasedOnData
        call calculateCursorValues
ret

textModeTab:
        ;send the set lights command to keyboard
        ;call nearestBlankLine
        ;mov al, cl
        ;call alToScreenHex

        ;call insertionScreenScroll

        ;mov bx, commandHistory
        ;mov cl, 00001111b
        ;call printString
        mov bx, commandHistory
        mov ch, 9

        fuckpooploop:
        mov al, [ds:bx]
        mov cl, 00001111b
        push bx
        push cx
        call alToScreenHex
        pop cx
        pop bx
        inc bx
        dec ch
        cmp ch, 0 
        jne fuckpooploop

ret

;changes no registers most of the time. if al = scancode for arrowkeys, it changes al to 0
processSpecialKeystrokes:

        ;if scancode for caps lock, toggle caps
        cmp al, 0x3A
        je processSpecialKeystrokesCaps
        cmp al, 0x45
        je processSpecialKeystrokesNum

        processSpecialKeystrokesArrows:
        cmp al, 0x88    ;up scancode 0x48
        je upArrow
        cmp al, 0x8B    ;left scancode 0x4B
        je leftArrow
        cmp al, 0x90    ;down scancode 0x50
        je downArrow
        cmp al, 0x8D    ;right scancode 0x4D
        je rightArrow
        cmp al, 0x92
        je insertKey

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

upArrow:
ret

downArrow:
ret

leftArrow:
        call decrementCursor

        ;cursor decrementing currently needs calculate cursor values to be run for things to update
        push ax
        push bx
        push dx
        call calculateCursorValues
        pop dx
        pop bx
        pop ax

        ;set al to null character do that printChar won't print it
        mov al, 29
ret

rightArrow:
        call incrementCursor
        ;set al to null character do that printChar won't print it
        push ax
        push bx
        push dx
        call calculateCursorValues
        pop dx
        pop bx
        pop ax
        mov al, 29
ret

insertKey:

push ax
        mov al, [ds:capsState]
        and al, 00100000b
        cmp al, 0
pop ax
je insertKeyOn
;jmp insertKeyOff

insertKeyOff:

push ax
        ;reset the bit in position 5
        mov al, [ds:capsState]
        and al, 11011111b
        mov [ds:capsState], al

        ;change vga cursor to small line
        mov al, 0x0A
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;start at scanline
        mov dx, [ds:CRTCData]
        mov al, 0x0E
        out dx, al

        ;select vga cursor end register
        mov al, 0x0B
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;end at scanline F
        mov dx, [ds:CRTCData]
        mov al, 0x0F
        out dx, al
pop ax

ret

insertKeyOn:

push ax
        ;set the bit in position 5
        mov al, [ds:capsState]
        or al, 00100000b
        mov [ds:capsState], al

        ;change vga cursor to bigass square
        ;cursor start register
        mov al, 0x0A
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;start at scanline
        mov dx, [ds:CRTCData]
        mov al, 0x04
        out dx, al

        ;select vga cursor end register
        mov al, 0x0B
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;end at scanline F
        mov dx, [ds:CRTCData]
        mov al, 0x0F
        out dx, al
pop ax

ret

;modifies al register
;toggles caps led on an 8042 compatible ps2 keyboard
toggleCaps:

        ;load caps state byte
        mov al, [ds:capsState]
        ;if caps lock bit = 1, caps is set.
        and al, 00000001b
        cmp al, 0
        ;toggle caps lock state
        je toggleCapsOn
        jmp toggleCapsOff

        toggleCapsOn:
                ;store new caps lock state into ram
                mov al, [ds:capsState]
                or al, 00000001b
                mov [ds:capsState], al

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
                mov al, [ds:capsState]
                and al, 11111110b
                mov [ds:capsState], al

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
        mov al, [ds:capsState]
        ;if caps lock bit = 1, caps is set.
        and al, 00000010b
        cmp al, 0
        ;toggle caps lock state
        je toggleNumOn
        jmp toggleNumOff

        toggleNumOn:
                ;store new caps lock state into ram
                mov al, [ds:capsState]
                or al, 00000010b
                mov [ds:capsState], al

                call updateKeyboardLedsBasedOnCapsState

        ret

        toggleNumOff:
                ;store new caps lock state into ram
                mov al, [ds:capsState]
                and al, 11111101b
                mov [ds:capsState], al

                call updateKeyboardLedsBasedOnCapsState

ret

updateKeyboardLedsBasedOnCapsState:
        
        ;move caps bit to 3rd position
        mov ah, [ds:capsState]
        and ah, 00000001b
        shl ah, 2

        ;aquire information about the num lock
        mov al, [ds:capsState]
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

textModeEnter:
        call lineToCommandBuffer
        call textModeNewLine
        call processCommands
ret

;dos is stupid. Don't use this subroutine
dosNewLine:

        mov dl, 10
        mov ah, 02h
        int 21h
        mov dl, 13
        mov ah, 02h
        int 21h

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
        mov al, [ds:bx]
        ret

ret

clearScreen:

;set up the address of the top of the screen
mov ax, 0B800h
mov es, ax
mov di, 0

;set counter for FFFh bytes
mov cx, 0FFFh

screenClearLoop:
mov al, 0
mov [es:di], al
inc di
loop screenClearLoop

ret

;puts scancode in al
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

        waitKeyLeftShiftOn:
        push ax
        mov ah, [ds:capsState]
        or ah, 00010000b
        mov [ds:capsState], ah
        pop ax
        jmp waitKeyContinue

        waitKeyRightShiftOn:
        push ax
        mov ah, [ds:capsState]
        or ah, 00001000b
        mov [ds:capsState], ah
        pop ax
        jmp waitKeyContinue

        waitKeyLeftShiftOff:
        push ax
        mov ah, [ds:capsState]
        and ah, 11101111b
        mov [ds:capsState], ah
        pop ax
        jmp waitKeyContinue

        waitKeyRightShiftOff:
        push ax
        mov ah, [ds:capsState]
        and ah, 11110111b
        mov [ds:capsState], ah
        pop ax
        ;jmp waitKeyContinue

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

waitAck:
        call kbd8042WaitReadReady
        in al, 060h
        cmp al, 0FAh
        jne waitAck

ret

kbd8042WaitReadReady:
in al, 064h                    ;8042 status port
and al, 00000001b                   ;read the data register read ready bit
cmp al, 1
jne kbd8042WaitReadReady             ;if it's not ready, keep waiting until it is
ret

kbd8042WaitWriteReady:
in al, 064h
and al, 00000010b
ror al, 1
cmp al, 0
jne kbd8042WaitWriteReady
ret

;inputs = al = value of low nibble. ah = value of 2nd nibble
;cl = value of 3rd nibble. ch = value of high nibble
;outputs: ax = value in hex
asciiToHex16:

        call asciiToHex
        xchg cx, ax
        call asciiToHex
        xchg cx, ax
        mov ah, cl

ret

;converts 2 8 bit ascii characters to a hex value
;al = value of the low nibble
;ah = value of the high nibble
;any invalid characters will just get concatenated to a 4 bit value
;output: al = the 8 bit value of whatever was in ah and al converted from ascii to hex
;only modifies al and ah
asciiToHex:

        call alAciiToHex
        xchg al, ah
        call alAciiToHex
        xchg al, ah
        shl ah, 4
        add al, ah
        mov ah, 0

ret

;converts whatever is in the al register from an ascii value to hex
;invalid ascii characters are just going to get concatenated to a 4 bit value
;only modifies al register
alAciiToHex:
        

        sub al, 48
        cmp al, 9
        ja alAciiToHexIsLetter
        ret

        alAciiToHexIsLetter:
                add al, 48
                ;bit 5 is only for capitalization
                and al, 11011111b
                sub al, 55
                and al, 00001111b

ret

;converts whatever 4 bit value is in the al register to the
;valid ascii code for that respective number in hex
alToHex:
        add al, 48
        cmp al, 58
        jae Add7More
        jmp DontAdd7More

        Add7More:
                add al, 7
        DontAdd7More:

ret

alToScreenHex:
        push ax
        and al, 11110000b
        mov cl, 4
        ror al, cl
        call alToHex

        ;set color to white (00001111b) and then print al as char
        mov cl, 00001111b
        call printChar
        pop ax
        
        ;set color to white (00001111b) and then print al as char
        and al, 00001111b
        call alToHex
        mov cl, 00001111b
        call printChar
        ;call print al to screen (not yet implemented)

ret

;moves the screen up by how ever many rows is in the cl register
scrollScreen:

        ;first, convert cl from rows to number of characters
        mov al, cl
        mov ah, 0
        mov bx, 160
        mul bx
        ;ax now contains the number of characters to move. Save it into ram for later
        mov [ds:charsToShift], ax

        ;now, cx contains the number of characters to move instead of ax
        xchg ax, cx

        ;increment ax and dx. when dx = cx, operation completed
        mov ax, cx
        mov dx, 0x0000


        ;use bx to move this stuff 16 bits at a time
        scrollScreenLoop:
        mov di, ax
        mov bx, [es:di]
        mov di, dx
        mov [es:di], bx
        add ax, 2
        add dx, 2
        cmp dx, 4096
        jnae scrollScreenLoop

        ;now, erase the bottom-most rows so they don't have garbage in them
        mov di, 4096
        sub di, cx

        ;set bx to contain a null character and an asstribute of black background, white foreground
        ;remember it's in little endian
        mov bx, 0x0F00

        ;bottom cl row erasure loop
        scrollScrenCleanupLoop:
        mov [es:di], bx
        add di, 2
        cmp di, 4096
        jnae scrollScrenCleanupLoop


ret

nearestBlankVar dw 0x0000        ;address of nearest blank line that isn't above current line
posVar dw 0x0000        ;
shiftNum dw 0x0000
;a specialized function that scrolls the screen 1 position forward
insertionScreenScroll:

        call nearestBlankLine
        ;cl now has the ypos of the nearest blank line
        
        ;convert cl from rows number to absolute memory address
        mov al, cl
        mov ah, 0
        mov bx, 160
        mul bx
        mov [ds:nearestBlankVar], ax

        mov al, [ds:cursorY]    ;line number into al
        mov ah, 0
        mov bx, 160
        mul bx
        mov bl, [ds:cursorX]
        mov bh, 0
        push ax
                mov ax, 0x0002
                mul bx
                mov bx, ax
        pop ax
        add ax, bx
        sub ax, 2          ;ax needs to be 2 less in order to produce the correct behavior
        mov [ds:posVar], ax


        ;nearestBlankVar-posVar = num of bytes to shift forward
        ;nearestBlankVar = starting position of byte shifting
        mov bx, [ds:nearestBlankVar]
        ;sub bx, 2
        ;shift bytes down by 2 until working address = current address

        insertionScreenScrollLoop:
        mov ax, [es:bx]
        add bx, 2
        mov [es:bx], ax
        sub bx, 4
        mov ax, [ds:posVar]
        cmp bx, ax
        jne insertionScreenScrollLoop



ret

deleteAndScrollLeft:

        ;figure out distance between end of line and current x cursor position
        mov al, [ds:cursorX]
        mov ah, 80
        sub ah, al
        mov cl, ah

        ;calculate absolute address in vram of whereever the cursor is
        mov al, [ds:cursorY]    ;line number into al
        mov ah, 0
        mov bx, 160
        mul bx
        mov bl, [ds:cursorX]
        mov bh, 0
        push ax
                mov ax, 0x0002
                mul bx
                mov bx, ax
        pop ax
        add ax, bx

        mov bx, ax

        ;shift everything left. only do it for the current line because I'm being lazy right now
        deleteAndScrollLeftLoop:
        mov ax, [es:bx]
        sub bx, 2
        mov [es:bx], ax
        add bx, 4
        dec cl
        cmp cl, 0
        jne deleteAndScrollLeftLoop

        call decrementCursor
        call calculateCursorValues


ret

;returns cl = 0 if none found. otherwise, cl = line number
nearestBlankLine:

        mov ch, [ds:cursorY]
        inc ch
        push cx
                call isLineBlank
                cmp cl, 0
        pop cx
        jne lineFindLoop
        mov cl, ch
        ret

        lineFindLoop:
                inc ch
                cmp ch, 26
                je lineFindLoopError
                push cx
                call isLineBlank
                cmp cl, 0
                pop cx
                jne lineFindLoop
                mov cl, ch
                ret

        lineFindLoopError:
                mov cl, 0


ret

;figures out if the line number in ch is blank or not
;sets cl = 0 if true, cl = 1 if false
;modifies bx, ax, cx
isLineBlank:
        
        ;save current cursor y position into bx for later
        mov bh, [ds:cursorX]
        mov bl, [ds:cursorY]

        ;set cursor y position to value in ch
        xchg cl, ch

        push bx
        push cx
                call setCurY
                mov cl, 0
                call setCurX
        pop cx
        pop bx

        xchg cl, ch

        mov cl, 80
        isLineBlankLoop:
                push bx
                call getByteAtCurPos
                pop bx
                or al, 00100000b ;make it so that space and nullchar are both valid
                cmp al, 00100000b
                jne isLineBlankNo
                push ax
                        ;be sure to actually increase the cursor counter
                        mov al, [ds:cursorX]
                        inc al
                        mov [ds:cursorX], al
                pop ax
                dec cl
                cmp cl, 0
                jne isLineBlankLoop
                jmp isLineBlankYes


isLineBlankNo:
        mov cl, 1

        ;put cursorY and X back to where is was before running this function
        push cx
                mov cl, bl
                push bx
                call setCurY
                pop bx
                mov cl, bh
                push bx
                call setCurX
                pop bx
        pop cx
ret

isLineBlankYes:
        mov cl, 0

        ;put cursorY back to where it was before running this function
        push cx
                mov cl, bl
                push bx
                call setCurY
                pop bx
                mov cl, bh
                push bx
                call setCurX
                pop bx
        pop cx
ret

;returns byte of whatever char is at that position in video ram
;uses cursorX and cursorY ram values as parameters
getByteAtCurPos:
push di
        ;calculate the correct line number from cursorY
        mov al, [cursorY]
        mov ah, 0
        mov bx, 00A0h
        mul bx
        mov di, ax

        ;add the column number in cursorX to di so that the character prints to the correct column
        mov al, [cursorX]
        mov ah, 0
        mov bx, 2
        mul bx
        add di, ax

        mov ax, [es:di]
pop di
;ax now contains the mode 3 character value

ret

;write ah of whatever character is in al using cl as attributes
writeChars:
        
        ;a function used for writing a lot of the same thing ah times
        push ax
        push bx
        push cx
                call calculateCursorValues
        pop cx
        pop bx
        pop ax

        cmp ah, 0
        je writeCharsGTFO
        push cx
        push ax
        call printChar
        pop ax
        pop cx
        dec ah
        jmp writeChars

        writeCharsGTFO:
ret

printCPUID:

mov eax, 00h
cpuid
cmp al, 01
jl oldcpu
je newer486cpu
jmp newothercpu

oldcpu:
mov bx, cpu486typeold
mov al, [ds:bx]
mov cl, 00000010b
call printString
jmp cputypecontinue

newer486cpu:
;mov bx, cpu486typenew
;mov al, ds:[bx]
;mov cl, 00000011b
mov eax, 00h
cpuid
call enhanced486cpubrandchecks
mov eax, 01h
cpuid
shr al, 4
and ah, 00001111b
shl ah, 4
or al, ah
call enhanced486cpuidchecks
jmp cputypecontinue

newothercpu:
mov bx, cpuothertype
mov al, [ds:bx]
mov cl, 00001011b
call printString
jmp cputypecontinue

cputypecontinue:
;call printString

ret

enhanced486cpubrandchecks:

cmp ebx, 'Genu'
je printIntelVendorInfo
cmp ebx, 'Auth'
je printAMDVendorInfo
cmp ebx, 'Cyri'
jmp printCytrixVendorInfo

printIntelVendorInfo:
mov bx, cpubrandIntel
mov al, [ds:bx]
mov cl, 00000011b
call printString
ret

printAMDVendorInfo:
mov bx, cpubrandAMD
mov al, [ds:bx]
mov cl, 00000100b
call printString
ret

printCytrixVendorInfo:
mov bx, cpubrandCytrix
mov al, [ds:bx]
mov cl, 00001111b
call printString
ret

enhanced486cpuidchecks:

;is cpu a 5x86? cpuid for 5x86 dx5 is 4eh or 4fh
push ax
or al, 00000001b
cmp al, 4FH
pop ax
jne notADX5

mov bx, cputype486DX5
mov al, [ds:bx]
mov cl, 00001111b
call printString
ret

notADX5:
push ax
and al, 01010000b
cmp al, 01010000b
pop ax
jne notAPentium

mov bx, cputypeoverdrive
mov al, [ds:bx]
mov cl, 00001111b
call printString
ret

notAPentium:
and al, 00001111b
cmp al, 00000101b
jz cpuissx2
cmp al, 00000111b
jz cpuisdx2
cmp al, 00001000b
jz cpuisdx4
cmp al, 00000111b
jz cpuisdx2
jmp cpuunknown

cpuissx2:
mov bx, cputype486SX2
jmp cpuTypeFinish

cpuisdx2:
mov bx, cputype486DX2
jmp cpuTypeFinish

cpuisdx4:
mov bx, cputype486DX4
jmp cpuTypeFinish

cpuunknown:
mov bx, cputype486idfk
jmp cpuTypeFinish

cpuTypeFinish:
mov al, [ds:bx]
mov cl, 00001111b
call printString

ret

testString db 'test string$'
initString db '486 bios$'

andString db 'and ','$'
cpuidk db 'cpu type: ','$'
cpu486typeold db 'older 486','$'
cpu486typenew db 'later 486','$'
cpuothertype db 'something other than a 486','$'

cpubrandIntel db 'Intel $'
cpubrandAMD db 'AMD $'
cpubrandCytrix db 'Cytrix $'

;the cpus that don't have the cpuid instruction
cputype486DX1 db '486 DX$'
cputype486SX1 db '486 SX$'
cputype487SXDX db '487 SX/DX$'
cputype486SL db '486 SL$'

;the cpus that do have the cpuid instruction
cputype486SX2 db '486 SX2$'
cputype486DX2 db '486 DX2$'
cputype486DX4 db '486 DX4$'
cputype486DX5 db '5x86 DX5$'
cputype486idfk db '486 (unknown model)$'
cputypeoverdrive db 'Pentium Overdrive$'
cputypek5 db 'K5$'
cputypek6 db 'K6-II$'
cputypek6later db 'K6-III$'

cpuwtfcomment db '..wait, what? How the hell did you do that?','$'
helpmsg db "God DAMN you are stupid.. Here's some commands:$"
invalidcommandmsg db "Invalid command. What, are you stupid?$"
invalidparametersmsg db "Invalid parameters. Type command the right way next time, dumbass.$"
genericGoFuckYourself db "go fuck yourself because i'm not implementing that idiotic bullshit$"

; Family:                 0000 = 8086/8088
;                         0001 = 80186/80188
;                         0010 = 80286
;                         0011 = Intel386
;                         0100 = Intel486
;                         0101 = Pentium
;                         0110 = P6
;
; Model (family = 0101):  0001 = Pentium (510\66, 567\66)
;                         0010 = Pentium P54C (735\90, 815\100)
;                         0011 = Pentium overdrive processors
;                         0101 = Pentium overdrive for IntelDX4
;                         1111 = Unknown
;
; Model (family = 0100):  0000 = Intel486 DX
;                         0001 = Intel486 DX
;                         0010 = Intel486 SX
;                         0011 = Intel487 (SX), or Intel486 DX
;                         0100 = Intel486 SL
;                         0101 = IntelSX2
;                         0111 = IntelDX2 write-back enhanced
;                         1000 = IntelDX4 and IntelDX4 overdrive
;                         1111 = Unknown
;
; Model (family = 0011):  0000 = Intel386 DX
;                         0010 = Intel386 SX, Intel386 CX, Intel386 EX
;                         0100 = Intel386 SL
;                         1111 = Unknown
;
CRTCAdr dw 0x03D4
CRTCData dw 0x03D5
AttibCtrl dw 0x03C0

graphCtrlAddress dw 0x03CE ;vga graphics address register for the graphics controller registers
graphCtrlData dw 0x03CF ; vga graphics data register for the graphics controller registers

cursorVGAStart dw 0x0000
cursorVGAEnd dw 0x0000
charsToShift dw 0x0000
;screen vars
cursorX db 0
cursorY db 0

;bit 0 = caps. bit 1 = num lock. bit 2 = scroll lock. bit 3 = right shift. bit 4 = left shift. bit 5 = insert
capsState db 0

;scancode tables. dont fuck, with this, you're gonna have a bad time.
lowercase db 0x1B,"1234567890-=",0x08,0x09,"qwertyuiop[]",0x10,0,"asdfghjkl;'`",0,"\zxcvbnm,./",0,0,0," ",0,0,0,0,0,0,0,0,0,0,0,0,0,"789-","456","+1230",0,0,0,0,0,0,0,0,0,0,0,0,"/",0x08,0
compatzeros1 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compatzeros2 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
uppercase db 0x1B,"!@#$%^&*()_+",0x08,0x09,"QWERTYUIOP{}",0x10,0,"ASDFGHJKL:",0x22,"~",0,"|ZXCVBNM<>?",0,0,0," ",0,0,0,0,0,0,0,0,0,0,0,0,0,"789-","456","+1230",0,0,0,0,0,0,0,0,0,0,0,0,"/",0x08,0
compatzeros3 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compatzeros4 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;still using the "Compatibility zeros" method of achieving simpler keyboard driver code used in my z80 bios. I have 128kb of rom to work with here, I can afford this lazy fix

%include "COMMANDS.ASM"
%include "vgadriver.asm"
