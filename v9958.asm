;the v9958 driver code needs to go here
setup9958Text2Mode:
    mov bx, 0x3000     ;i'll fix the impossibly shitty video registor convention later. I have to roll with it for a little bt first then I can optimize once everythings working
    mov es, bx

	call init9958Video

	;set cursor and stuff
	call clearText			;this one is more likely to work than the clearVram subroutine
	mov ax, 0
	mov [es:row], al
	mov [es:col], al

	call RowsColumnsToCursorPos
ret

;brought over from the test bios
;cx needs to contain the number of wait loops
longDelay:
    mov bx, 0
    longDelayContinue:
    inc bx
    cmp bx, cx
    jne longDelayContinue
ret

;x86 v9958 driver

;port #0 VRAM Data (R/W)		$A020
;port #1 Status register (R) / VRAM Address (W) / Register set-up (W)	$A021
;port #2 Palette registers (W)	$A022
;port #3 Register indirect addressing (W)
;$2FFE - used for draw rectangle filled function
col: dw 0x0000
row: dw 0x0000

init9958Video:

    call clearVram             ;doesnt work for some reason
    call VdpCharsIntoRam
    call setupDefaultColors

	mov cx, 0FFFh
	call longDelay

    ;activate TEXT 2 mode
    mov al, 0
    mov bl, 0b00000100 		;change to %00000100 for text2. %00000000
    call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

    ;set up register 1
    mov al, 1
	mov bl, 0b01010000
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

    mov bl, 0b11110000 	;register 7. bits 7-4 = text color in text modes. bits 3-0 = screen backdrop color. uses 16 color values
	mov al, 7
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

	;set up register 12 and configure cursor color in text 2 mode
	mov bl, 0b00001010
	mov al, 12
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

	;configure register 13, the cursor blink time register
	mov bl, 0b01000100
	mov al, 13
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

    ;set values of pattern generator, pattern layout and pattern color table
	;I am using "MSX system default" values
	;text 2 pattern generator: 01000h-017FFh. Pattern layout: 00000h-0077Fh (00000h-0086Fh in 26.5 line mode). Pattern color table 800h-8EFh (800h-90Dh in 26.5 mode)
	mov bl, 0b00000011
	mov al, 2
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

	mov bl, 0b00000010
	mov al, 4
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

	mov bl, 0b00100111
	mov al, 3
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay

	mov bl, 0b00000000
	mov al, 10
	call VdpWriteToStandardRegister

ret

;sets the color palette to the default
;I'm making it the same as the Microsoft Windows default 16-color palette
;https://en.wikipedia.org/wiki/List_of_software_palettes#Microsoft_Windows_default_16-color_palette
setupDefaultColors:
    ;to use the palette registers on an x86 system:
	;bl register needs to contain palette register number you want to write to (0-15)
    ;al register needs to contain first palette byte
    ;ah register needs to contain second palette byte
    ;note that the pointer value in register 16 auto increments each time you do this

	;color 0 = black
	mov bl, 0
	mov al, 0
	mov ah, 0
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time


	;color 1 = maroon
	mov bl, 1
	mov al, 0b01000000
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 2 = dark green
	mov bl, 2
	mov al, 0b00000000
	mov ah, 0b00000100
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 3 = poop brown
	mov bl, 3
	mov al, 0b01000000
	mov ah, 0b00000100
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 4 = navy blue
	mov bl, 4
	mov al, 0b00000100
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 5 = purple
	mov bl, 5
	mov al, 0b01000100
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 6 = teal
	mov bl, 6
	mov al, 0b00000100
	mov ah, 0b00000100
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 7 = silver
	mov bl, 7
	mov al, 0b01000100
	mov ah, 0b00000100
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 8 = gray
	mov bl, 8
	mov al, 0b00100010
	mov ah, 0b00000010
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 9 = red
	mov bl, 9
	mov al, 0b01110000
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 10 = bright green
	mov bl, 10
	mov al, 0b00000000
	mov ah, 0b00000111
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 11 = yellow
	mov bl, 11
	mov al, 0b01110000
	mov ah, 0b00000111
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 12 = blue
	mov bl, 12
	mov al, 0b00000111
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time

	;color 13 = fuchsia
	mov bl, 13
	mov al, 0b01110111
	mov ah, 0b00000000
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay

	;color 14 = aqua
	mov bl, 14
	mov al, 0b00000111
	mov ah, 0b00000111
	call VdpWriteToPaletteRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time


	;color 15 = white
	mov bl, 15
	mov al, 0b01110111
	mov ah, 0b00000111
	call VdpWriteToPaletteRegister

ret

;sets up and configures graphics 4 mode
;	pattern layout (bitmap): 00000h-069ffh
;	sprite patterns 07800h-07fffh
;	sprite attributes 07600h-0767Ffh
;	sprite colors 07400h-075ffh
setupG4Mode:

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;put the vdp into graphics mode 4. m5 = 0. m4 = 1. m3 = 1. m2 = 0. m1 = 0
	;register 0
	mov bl, 00000110b 		;change to %00000100 for text2. %00000000
	mov al, 0
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;set up register 1
	mov bl, 01000000b
	mov al, 1
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;set up register 8
	mov bl, 00001000b
	mov al, 8
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;set register 23 to zero
	mov bl, 0
	mov al, 23
	call VdpWriteToStandardRegister

	call clearMostVram

	;here's what I need to set:
	;	pattern layout (bitmap): 00000h-069ffh
	;	sprite patterns 07800h-07fffh
	;	sprite attributes 07600h-0767Ffh
	;	sprite colors 07400h-075ffh

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;pattern layout table
	mov bl, 00011111b
	mov al, 2
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;sprite patterns
	mov bl, 00001111b
	mov al, 6
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;sprite attributes high
	mov bl, 00000000b
	mov al, 11
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;sprite attributes low ($7600)
	mov bl, 11101111b
	mov al, 5
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;sprite color table high
	mov bl, 00000001b
	mov al, 10
	call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay		;palette register stuff seems to need extra time
	;sprite color table low
	mov bl, 11010000b
	mov al, 3
	call VdpWriteToStandardRegister

ret

;bl register needs to contain palette register number you want to write to (0-15)
;al register needs to contain first palette byte
;ah register needs to contain second palette byte
;note that the pointer value in register 16 auto increments each time you do this
VdpWriteToPaletteRegister:
	push ax
	push bx
        ;bl alredy contains register number
        mov al, 16
		call VdpWriteToStandardRegister
	pop bx
	pop ax  ;BUG! was "pop bx"

	mov cx, 0FFFh
    call longDelay		;palette register stuff seems to need extra time

    ;somehow this is even more annoying than the z80 version
    mov dx, 0xA022
    out dx, al
    mov al, ah
    out dx, al

ret

;bl should contain register data
;al should contain register number
VdpWriteToStandardRegister:
    mov dx, 0xA021

	;write the data byte first because that's just what you do
    push ax
        mov al, bl
        out dx, al

		mov cx, 0FFh
		call longDelay
    pop ax

	;write the register number next
	;add a, 128
	or al, 10000000b		;different way of adding 128 to al
	out dx, al              ;dx should still be set to 0xA021
ret

;text 2 pattern generator: 01000h-017FFh. Pattern layout: 00000h-0077Fh (00000h-0086Fh in 26.5 line mode)
VdpCharsIntoRam:
    mov al, 14
    mov bl, 0                           ;bits a16, a15 and a14
    call VdpWriteToStandardRegister

	mov cx, 0FFFh
	call longDelay
	mov al, 00000000b					;vram addres bits a0-a7
	out dx, al
	mov cx, 0FFFh
	call longDelay
	mov al, 01010001b					;bits a8-a13. bit 6 is r/w. bit 7 should stay zero
	out dx, al
	mov cx, 0FFFh
	call longDelay
    ;mov dx, 0xA022
    ;mov al, 14
    ;mov bl, 0                           ;bits a0-a7
    ;call VdpWriteToStandardRegister

    ;mov al, 14
    ;mov bl, 00010001b                   ;bits a8-a13. bit 6 is r/w. bit 7 should stay zero
    ;call VdpWriteToStandardRegister

    ;now, time to make a bigass loop
	;get ready to start writing to port 0 - the vram access port
    mov dx, 0xA020
    mov bx, letters_space
    mov al, [es:bx]             ;load first byte of font data into al
    
    VdpCharsIntoRamLoopStart:
        ;use the same amount of nops even
		push bx
			mov cx, 1Fh
			call longDelay
		pop bx

        out dx, al
        inc bx
        mov al, [es:bx]
        cmp al, 0b11111111
        jne VdpCharsIntoRamLoopStart

ret

;clear first 8kb of vram
clearVram:
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov dx, 0xA021		;it has to be port 1 not port 0
	mov cx, 02FFh
	call longDelay
	mov al, 0
	out dx, al
	mov cx, 02FFh
	call longDelay
	add al, 64
	out dx, al
	mov cx, 02FFh
	call longDelay

	mov dx, 0xA020	;port 0 is data port, riiight..??
	mov bx, 0x2000 	;clear the first 8kb of vram
	mov al, 0

	clearContinue:
		out dx, al
        dec bx
        cmp bx, 0
        jne clearContinue

        ;i dont miss this crap
		;dec hl
		;ld a, h
		;or l
		;nop
		;nop
		;jr nz, clearContinue

ret

;clear the forst 2048 bytes of ram
clearText:
	mov cx, 0x3FF
	call longDelay
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov cx, 0x3FF
	call longDelay
	mov dx, 0xA021		;it has to be port 1 not port 0
	mov al, 0
	out dx, al
	mov cx, 0x3FF
	call longDelay
	add al, 64
	out dx, al

	mov dx, 0xA020	;port 0 is data port, riiight..??
	mov bx, 0x7FF 	;clear the first 2048 bytes of ram
	mov al, 0

	clearTextContinue:
		out dx, al
        dec bx
        cmp bx, 0
        jne clearTextContinue
		
ret

;clear first 8kb of vram
clearMostVram:
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov dx, 0xA021		;it has to be port 1 not port 0
	mov cx, 02FFh
	call longDelay
	mov al, 0
	out dx, al
	mov cx, 02FFh
	call longDelay
	add al, 64
	out dx, al
	mov cx, 02FFh
	call longDelay

	mov dx, 0xA020	;port 0 is data port, riiight..??
	mov bx, 0x8000 	;clear the first 32kb of vram
	mov al, 0

	clearMostVramContinue:
		out dx, al
        dec bx
        cmp bx, 0
        jne clearMostVramContinue

        ;i dont miss this crap
		;dec hl
		;ld a, h
		;or l
		;nop
		;nop
		;jr nz, clearContinue

ret

;copy whatever's in register al to next address in vram
;changed registers: idk
VdpPrintChar:

	push ax
		call RowsColumnsToVram
	pop ax

    mov dx, 0xA020
	out dx, al
	
	;between the col/row increment code and the backspace code, I think things aren't 100% correct and bug-free but whatever (watch what happens when you use the backspace near the end of the line, the 78-80 char column positions)
	;i'm going to wait until I get the blinking cursor implemented to fix that since then it will be easier

	;everytime a char is printed, update the counter
	mov bx, col
	mov ax, [es:col]
	cmp ax, 79
	je VdpPrintCharContinueNewline

	inc ax
	mov [es:col], ax	;lol, is that allowed?
	
	ret					;if it got here, done

	;if col is greater than 80, reset to 0 and increment row by 1
	VdpPrintCharContinueNewline:
		mov ax, 0		;reset column to zero
		mov [es:col], ax
		mov bx, row
		mov ax, [es:bx]	;load column address
		inc ax
		cmp ax, 24
		je VdpPrintCharContinueEndOfScreen

		mov [es:row], ax		;simply store the newly incremented value into row

		ret						;the end

	
	VdpPrintCharContinueEndOfScreen:
		;move cursor up a few lines
		;call the move screen up subrountine
		;that's it
		ret
    ;lets not do cursor stuff for this simple test
	;update the column counter
	;ld hl, $9EFA;fuck
	;ld a, (hl)
	;inc a
	;ld (hl), a
	;cp 80
	;jr nc, incRow
	;jr VdpPrintCharExit

	;if more than 80 columns, set column numbers to 0 and increment row number
	;incRow:
	;	ld a, 0
	;	ld (hl), a
	;	ld hl, $9EFB
	;	ld a, (hl)
	;	inc a
	;	ld (hl), a
	;VdpPrintCharExit:
ret

;don't update vram every time. That's too fucking slow
VdpPrintChar_NoAddressUpdate:
	mov dx, 0xA020
	out dx, al
	
	;between the col/row increment code and the backspace code, I think things aren't 100% correct and bug-free but whatever (watch what happens when you use the backspace near the end of the line, the 78-80 char column positions)
	;i'm going to wait until I get the blinking cursor implemented to fix that since then it will be easier

	;everytime a char is printed, update the counter
	mov bx, col
	mov ax, [es:col]
	cmp ax, 79
	je VdpPrintCharContinueNewline_NoAddressUpdate

	inc ax
	mov [es:col], ax	;lol, is that allowed?
	
	ret					;if it got here, done

	;if col is greater than 80, reset to 0 and increment row by 1
	VdpPrintCharContinueNewline_NoAddressUpdate:
		mov ax, 0		;reset column to zero
		mov [es:col], ax
		mov bx, row
		mov ax, [es:bx]	;load column address
		inc ax
		cmp ax, 24
		je VdpPrintCharContinueEndOfScreen_NoAddressUpdate

		mov [es:row], ax		;simply store the newly incremented value into row

		ret

	VdpPrintCharContinueEndOfScreen_NoAddressUpdate:
		;move cursor up a few lines
		;call the move screen up subrountine
		;that's it
		ret
ret

;ds:bx should contain memory address of the string to print
;print a string using the VdpPrintChar subroutine
VPrintString:
	;nah
    ;push bx
	;call RowsColumnsToVram
	;pop bx

	mov al, [es:bx]
	;==========================
	;push hl
	;call VdpPrintChar 		;load the first character of every string twice so I can see wtf it's really doing when it copies over that other pointless garbage
	;;pop hl
	;==========================
	VPrintStringLoop:
		push bx
		call VdpPrintChar
		pop bx
		inc bx
		mov al, [es:bx]
		cmp al, 0
		jnz VPrintStringLoop

ret

;ds:bx should contain memory address of the string to print
;print a string using the VdpPrintChar subroutine
VPrintString_RealModeOs:		;the one modified for optimal use in real mode os, but not elsewhere
	;nah
    ;push bx
	;call RowsColumnsToVram
	;pop bx

	;update the address pointer before and after the strign print but not during. use autoincrement function
	push ax
		call RowsColumnsToVram
	pop ax

	mov bx, [ds:printStringVars0]
    mov cl, [ds:printStringVars1]

	mov al, [es:bx]
	;==========================
	;push hl
	;call VdpPrintChar 		;load the first character of every string twice so I can see wtf it's really doing when it copies over that other pointless garbage
	;;pop hl
	;==========================
	VPrintStringLoop_RealModeOs:
		push bx
		call VdpPrintChar_NoAddressUpdate
		pop bx
		inc bx
		mov al, [es:bx]
		cmp al, 0
		jnz VPrintStringLoop_RealModeOs

	;i guess nothing bad happens if we dont save the value of ax, bx amd cx.. right?
	call RowsColumnsToVram
	call RowsColumnsToCursorPos

ret

;not tested but should work
alTo9958Hex:
        push ax
        and al, 11110000b
        mov cl, 4
        ror al, cl
        call alToHex

        ;set color to white (00001111b) and then print al as char
        ;mov cl, 00001111b      ;20x2 lcd is monochrome
        ;call charToLcd
		call VdpPrintChar
        pop ax
        
        ;set color to white (00001111b) and then print al as char
        and al, 00001111b
        call alToHex
        ;mov cl, 00001111b      ;20x2 lcd is monochrome
        ;call charToLcd
		call VdpPrintChar

ret

;put backspace on the screen
VdpBackspace:

	;gotta update the vram address pointer first
	push ax
		call RowsColumnsToVram
	pop ax

	;decrement the cursor position counter
	mov al, [es:col]
	cmp al, 0
	je VdpBackspaceGTFO			;don't ever backspace if at the beginning of a line
	dec al
	mov [es:col], al

	call RowsColumnsToVram			;update v9958 pointer based on that

	mov al, 32						;load the space ascii character into al
	call VdpPrintChar				;print the space to overwrite whatever was in that position

	;decrement the cursor position counter
	mov al, [es:col]
	cmp al, 0
	je VdpBackspaceGTFO			;don't ever backspace if at the beginning of a line
	dec al
	mov [es:col], al

	call RowsColumnsToVram
	ret								;done, gtfo

	VdpBackspaceGTFO:
ret

RowsColumnsToVramTempVar: dw 0x0000		;use this for temporary variable storage within RowsColumnsToVram
RowsColumnsToVram:
	mov al, 14
    mov bl, 0                           ;i dont remember what this does
    call VdpWriteToStandardRegister

	mov al, [es:row]		;get the current row amount
	mov cl, al
	mov al, 80
	mul cl				;rows * 80 = number of character spaces from
	mov cx, ax			;move the result of this multiplication into cx for safekeeping

	mov ah, 0
	mov al, [es:col]		;get the current column number
	xchg ax, cx
	add ax, cx
	
	mov [es:RowsColumnsToVramTempVar], ax		;save that number for later
	
	;after this, ax contains cursor position in linear vram. converting this to vram address is going to be hard tho
    mov al, 14
	mov bl, 0		;text2 mode won't ever have a high enough number for this to be set to anything besides 0
	call VdpWriteToStandardRegister
    mov cx, 0FFh
    call longDelay
	mov dx, 0xA021
	mov ax, [es:RowsColumnsToVramTempVar]
	out dx, al								;lower 8 bits to address pointer register
    mov cx, 0FFh
    call longDelay
	mov ax, [es:RowsColumnsToVramTempVar]
	xchg ah, al
	and al, 00111111b						;the correct upper bits that are available
	add al, 64
	out dx, al

	;there, hopefully that will do it

ret

RowsColumnsToCursorPosVar0: dw 0x0000		;temporary variable storage for stuff involving this one function
RowsColumnsToCursorPosVar1: dw 0x0000		;temporary variable storage for stuff involving this one function
RowsColumnsToCursorPosDisable db 0x00				;trying to speed up a slow subroutine that's slow due to the slow ass v9958 and its retarded cursor shit? Setting this byte to anything except zero disables this subroutine
RowsColumnsToCursorPos:
	;if the disable byte is not set to zero, exit
	mov al, [es:RowsColumnsToCursorPosDisable]
	cmp al, 0
	jne RowsColumnsToCursorPosGTFO

	call eraseCursorTable		;there should only ever be one cursor space

	mov ax, [es:row]			;load the row number from ram
	mov cl, 80
	mul cl						;ax <- al * cl 
	mov cx, ax					;save calculation result in cx for later

	mov ax, [es:col]
	add ax, cx											;the linear vram address based on rows cols variables should now be in ax

	;divide this number by 8
	;there are 8 cursor positions in one byte of vram
	mov cl, 8
	div cl					;fuck yeah x86 instruction set. Divide ax by 8. returns al = quotient. ah = remainder

	mov [es:RowsColumnsToCursorPosVar0], ax			;save result to ram for later
	;mov [es:RowsColumnsToCursorPosVar1], ax			;save result to ram for debugging
	;xchg ax, dx
	;mov [es:RowsColumnsToCursorPosVar1], ax			;save remainder to ram for later

	;set up the vram address pointer
	mov al, 14
	mov bl, 0			;for text2 mode, this will never need to be anything besides zero
	call VdpWriteToStandardRegister
    mov cx, 0FFh
    call longDelay
	mov ax, [es:RowsColumnsToCursorPosVar0]
	mov dx, 0xA021
	out dx, al			;whatever's in ax right now should be the correct thing for bits 7-0
    mov cx, 0FFh
    call longDelay
	;xchg ah, al			;get bits 15-8 into al
	;and al, 00111111b	;chop off the bits that need to be set to zero
	mov al, 00001000b	;cursor data starts at 0x800
	add al, 64			;next commaznd is data write so change bit 6 to a 1
	out dx, al

	;the vram address pointer should be ready now, all that's left is to write the cursor position byte into vram
	
	;at this point, ax + dx = the bit location to write the cursor bit to
	;or, ax is the byte location to write to and dx is the bit number
	mov ax, [es:RowsColumnsToCursorPosVar0]			;get the remainder from earlier
	xchg al, ah
	mov cl, al										;put remainder in cl register
	mov al, 0x80
	shr al, cl		;do basically the same thing the z80 code does to shift the bits to the correct position. I love this instruction so much because it makes things easier. You need at least a 286 to use it though.
	mov [es:RowsColumnsToCursorPosVar1], al

	mov cx, 0FFh
    call longDelay		;putting this delay here is the difference between it working at 8mhz or not. It does work at 25mhz also with this though
	mov dx, 0xA020
	out dx, al

	RowsColumnsToCursorPosGTFO:

ret

;clear all the cursor stuff
eraseCursorTable:
	;set the vram address pointer
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov cx, 0FFh
    call longDelay
	mov dx, 0xA021
	mov al, 0
	out dx, al
	mov cx, 0FFh
    call longDelay
	add al, 01001000b			;copying from z80 bios, didn't stop to consider what this does
	out dx, al
	mov cx, 0FFh
    call longDelay

	mov bx, 0x00FE				;number of bytes containing cursor information
	mov dx, 0xA020
	mov al, 0					;completely clear every byte in question
	
	eraseCursorTableContinue:
		out dx, al
		dec bx
		cmp bx, 0
		jne eraseCursorTableContinue

	;ld a, 14
	;ld d, 0
	;call VdpWriteToStandardRegister
	;ld d, 0
	;out (c), d
	;ld d, %01001000
	;out (c), d
	;ld hl, $00FE
	;ld c, $20
	;ld e, 0
	;eraseCursorTableContinue:
	;	out (c), e
	;	dec hl
	;	ld a, h
	;	or l
	;	nop
	;	nop
	;	jr nz, eraseCursorTableContinue

ret

;advance the textmode 2 cursor to the next line. Inserts a new line, not the same as incrementing the row byte by 80
VdpNewline:
	mov ax, [es:row]
	inc ax
	mov [es:row], ax

	mov ax, 0
	mov [es:col], ax

	;check if rows are less than 23 and if not, scroll the entire screen up by a little bit
	mov ax, [es:row]
	cmp al, 23
	jl VdpNewlineDontScrollTheScreen	;if the row number is less than 23 then scroll the screen

	call v9958ShiftScreenUp
	mov ax, [es:row]
	sub ax, 2
	mov [es:row], ax

	VdpNewlineDontScrollTheScreen:
	call RowsColumnsToCursorPos
	call RowsColumnsToVram
ret

;reset column to 0
VdpCarriageReturn:
ret

;can't remember if real mode os needs the video driver to include this or not but I'll make it anyway
VdpInsertTab:
ret

;pressing the right arrow key basically
VdpIncrementCursor:
	;everytime a char is printed, update the counter
	mov bx, col
	mov ax, [es:col]
	cmp ax, 79
	je VdpIncrementCursorContinueNewline

	inc ax
	mov [es:col], ax	;change value of byte
	
	ret					;if it got here, done

	;if col is greater than 80, reset to 0 and increment row by 1
	VdpIncrementCursorContinueNewline:
		mov ax, 0		;reset column to zero
		mov [es:col], ax
		mov bx, row
		mov ax, [es:bx]	;load column address
		inc ax
		cmp ax, 24
		je VdpPrintCharContinueEndOfScreen

		mov [es:row], ax		;simply store the newly incremented value into row
ret

;pressing the left arrow key but not backspace
VdpDecrementCursor:
	;decrement the cursor position counter
	mov al, [es:col]
	cmp al, 0
	je VdpBackspaceGTFO			;don't ever backspace if at the beginning of a line
	dec al
	mov [es:col], al

	call RowsColumnsToVram			;update v9958 pointer based on that
ret

;write a bunch of crap to the screen and see if anything happens
;kind of like the clear command but different
textmodeTest:
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov dx, 0xA021
	mov al, 0
	out dx, al;
	add al, 64
	out dx, al

	mov bx, 0x2000 	;clear the first 8kb of vram
	mov al, 80

	textmodeTestContinue:
		out dx, al
        dec bx
        cmp bx, 0
        jne textmodeTestContinue
ret

;fetch the most recently typed-in thing from vram
VdpVramToCommandBufferVar0 dw 0x0000		;i don't care the stack is "better", doing this type of thing is easier
VdpVramToCommandBuffer:
	;calculate the correct line number from the row byte. Each character spot in TEXT2 mode takes up 1 byte
	mov al, [ds:row]
	mov ah, 0
	mov cx, 80
    mul cl		;make ax = cl * al
	mov [ds:VdpVramToCommandBufferVar0], ax		;save the result of this calculation for later

	;switch the vram pointer to that address in vram
    mov al, 14
	mov bl, 0		;text2 mode won't ever have a high enough number for this to be set to anything besides 0
	call VdpWriteToStandardRegister
    mov cx, 0FFh
    call longDelay
	mov dx, 0xA021
	mov ax, [ds:VdpVramToCommandBufferVar0]
	out dx, al								;lower 8 bits to address pointer register
    mov cx, 0FFh
    call longDelay
	mov ax, [ds:VdpVramToCommandBufferVar0]
	xchg al, ah
	and al, 00001111b	;up to bit 11 means up to 2048 chars - the max number of chars on the screen in TEXT2 mode
	out dx, al

	;there. The read address should now be in the v9958 address pointer.
	
	mov bx, commandHistory

	VdpVramToCommandBufferCopyLoop:
	push bx
		mov cx, 2FFh
    	call longDelay
	pop bx
	mov dx, 0xA020	;data port im pretty sure
	in al, dx		;read that fucking byte
	mov [ds:bx], al			;put whatever byte was found into commandHistory
	inc bx			;just add 1
	cmp al, 0
	jne VdpVramToCommandBufferCopyLoop

	;done. copy a null character just in case and then a $ for good measure
	mov al, 0
	mov [ds:bx], al
	inc bx
	mov al, '$'
	mov [ds:bx], al
	
ret

;move the screen up when you need to
v9958ShiftScreenUp:
	mov cx, 3FFh
    call longDelay

	;set the vram pointer
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov cx, 4FFh
    call longDelay
	mov dx, 0xA021
	mov al, 10100000b
	out dx, al
	mov cx, 4FFh
    call longDelay
	mov al, 0		;note how bit 6, the read bit, is also set to zero
	out dx, al

	;address pointer is set up. copy the next 2048-160=1888 or 0x760 bytes into some place in ram
	mov bx, 0xF700		;the top of the current segment sounds like a good place

	v9958ShiftScreenUpCopyLoop:
	push bx
		mov cx, 0x02	;really small wait
		call longDelay
	pop bx
	mov dx, 0xA020
	in al, dx
	mov [ds:bx], al
	inc bx
	cmp bx, 0xFE60
	jne v9958ShiftScreenUpCopyLoop

	;there, all the data currently on the screen except for the topmost 160 bytes is now in ram probably
	;reset vram pointer to the beginning of vram
	mov al, 14
	mov bl, 0
	call VdpWriteToStandardRegister
	mov cx, 4FFh
    call longDelay
	mov dx, 0xA021
	mov al, 0
	out dx, al
	mov cx, 4FFh
    call longDelay
	mov al, 01000000b	;set bit 7 to 1 to indicate a future write command
	out dx, al

	mov bx, 0xF700		;get that stuff that got copied to ram
	v9958ShiftScreenUpCopyFromRamLoop:
	mov dx, 0xA020
	mov al, [ds:bx]
	out dx, al
	inc bx
	cmp bx, 0xFE60
	jne v9958ShiftScreenUpCopyFromRamLoop

	;almost done, now the rest of the screen needs to be erased
	mov al, 0
	v9958ShiftScreenUpCopyFillBottomWithNullsLoop:
	out dx, al
	inc bx
	cmp bx, 0xFF00
	jne v9958ShiftScreenUpCopyFillBottomWithNullsLoop

	;there. hopefully that works.

ret

;I only have like 30 seconds worth of stuff to showcase, so implement some random G4 mode stuff
v9958BasicG4Test:
	mov cx, 01FFh
	call longDelay
    ;set x starting point of line
    mov bl, 01000000b
    mov al, 36
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    mov bl, 00000000b
    mov al, 37
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;set y starting point of line
    mov bl, 00000100b
    mov al, 38
    call VdpWriteToStandardRegister
        
    mov cx, 01FFh
	call longDelay
    mov bl, 00000000b
    mov al, 39
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;set long side dots num
    mov bl, 00100100b
    mov al, 40
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    mov bl, 00000000b
    mov al, 41
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;set short side dots num
    mov bl, 00001000b
    mov al, 42
    call VdpWriteToStandardRegister
    mov cx, 01FFh
	call longDelay
    mov bl, 00000000b
    mov al, 43
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;set line color
    mov bl, 00000011b
    mov al, 44
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;set register 45
    mov bl, 00000010b
    mov al, 45
    call VdpWriteToStandardRegister

    mov cx, 01FFh
	call longDelay
    ;define logical operation - %01110000 for line command
    mov bl, 01110000b
    mov al, 46
    call VdpWriteToStandardRegister

	;this *should* copy all the example sprites
	call copySpriteToVram
	mov cx, 01FFh
	call longDelay

	;set sprite color table for sprite 0
	mov al, 14
	mov bl, 00000001b 	;bits a16, a15 and a14
	call VdpWriteToStandardRegister
	mov cx, 01FFh
	call longDelay
	mov al, 00000000b 	;bits a0-a7
	out dx, al
	mov cx, 01FFh
	call longDelay
	mov al, 01110100b 	;bits a8-a13. bit 6 is r/w. bit 7 should stay zero
	out dx, al
	
	;get ready to start writing to port 0 - the vram access port
	mov dx, 0xA020
	mov al, 00000101b
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al


	mov al, 00000110b
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al
	mov cx, 0xFF
	call longDelay
	out dx, al

	;copy the sprite info to the attribute table at $7000
	mov al, 14
	mov bl, 00000001b 	;bits a16, a15 and a14
	call VdpWriteToStandardRegister
	mov cx, 0x1FF
	call longDelay
	mov al, 00000000b 	;bits a0-a7
	out dx, al
	mov cx, 0x1FF
	call longDelay
	mov al, 01110110b 	;bits a8-a13. bit 6 is r/w. bit 7 should stay zero
	out dx, al

	;get ready to start writing to port 0 - the vram access port
	mov dx, 0xA020

	mov al, 0x50
	out dx, al ;xpos = 50
	mov cx, 0xFF
	call longDelay
	mov al, 0x40
	out dx, al 	;ypos = 50
	mov cx, 0xFF
	call longDelay
	mov al, 0
	out dx, al 		;pattern number 0
	mov cx, 0xFF
	call longDelay
	mov al, 0
	out dx, al
	mov cx, 0xFF
	call longDelay

	;do the stuff for the second sprite
	mov al, 0x20
	out dx, al
	mov cx, 0xFF
	call longDelay
	mov al, 0x20
	out dx, al
	mov cx, 0xFF
	call longDelay
	mov al, 1
	out dx, al
	mov cx, 0xFF
	call longDelay
	mov al, 0
	out dx, al
	mov cx, 0xFF
	call longDelay

    ;the user can press a key and it will all go away
    call waitKey

    call setup9958Text2Mode
ret

;copy sprite into sprite pattern table position 1 fpr g4test
copySpriteToVram:
	mov al, 14
	mov bl, 00000001b 	;bits a16, a15 and a14
	call VdpWriteToStandardRegister
	
	mov cx, 0FFh
	call longDelay
	mov dx, 0xA021
	mov al, 0		 	;bits a0-a7
	out dx, al
	mov cx, 0FFh
	call longDelay
	mov dx, 0xA021
	mov al, 01111000b 	;bits a8-a13. bit 6 is r/w. bit 7 should stay zero
	out dx, al

	;get ready to start writing to port 0 - the vram access port
	mov dx, 0xA020

	;ld hl, spritel1
	mov bx, spritel1
	;ld a, (hl)
	mov cl, 16			;do the loop this many times I think

	copySpriteToVramLoopStart:
		;important so do a longer delay each time
		push cx
			;no excuses. this better fucking work
			mov cx, 0x2FF
			call longDelay
		pop cx
		mov al, [es:bx]
		out dx, al
		inc bx
		dec cl
		cmp cl, 0
		jne copySpriteToVramLoopStart
ret


;font data
letters_space: db 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_exclamation: db 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000, 0b00100000, 0b00000000
letters_dquotes: db 0b00000000, 0b01010000, 0b01010000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_pound: db 0b00000000, 0b01010000, 0b11111000, 0b01010000, 0b11111000, 0b01010000, 0b00000000, 0b00000000
letters_dollar: db 0b00100000, 0b01111000, 0b10100000, 0b01110000, 0b00101000, 0b11110000, 0b00100000, 0b00000000
letters_percent: db 0b11101000, 0b10101000, 0b11110000, 0b00100000, 0b01011000, 0b10101000, 0b10111000, 0b00000000
letters_ampersand: db 0b00000000, 0b01110000, 0b11010000, 0b11110000, 0b10010000, 0b01010000, 0b01111000, 0b00000000
letters_quote: db 0b00011000, 0b00110000, 0b00100000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_lparenth: db 0b00001000, 0b00010000, 0b00100000, 0b00100000, 0b00100000, 0b00010000, 0b00001000, 0b00000000
letters_rparenth: db 0b00100000, 0b00010000, 0b00001000, 0b00001000, 0b00001000, 0b00010000, 0b00100000, 0b00000000
letters_asteri: db 0b00000000, 0b00000000, 0b00100000, 0b01110000, 0b00100000, 0b01010000, 0b00000000, 0b00000000
letters_plus: db 0b00000000, 0b00100000, 0b00100000, 0b11111000, 0b00100000, 0b00100000, 0b00000000, 0b00000000
letters_comma: db 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00100000, 0b00100000, 0b00000000
letters_minus: db 0b00000000, 0b00000000, 0b00000000, 0b11111000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_period: db 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01000000, 0b00000000
letters_fslash: db 0b00000000, 0b00001000, 0b00010000, 0b00100000, 0b01000000, 0b10000000, 0b00000000, 0b00000000
letters_0: db 0b01110000, 0b10001000, 0b11001000, 0b10101000, 0b10011000, 0b10001000, 0b01110000, 0b00000000
letters_1: db 0b01100000, 0b11100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b11111000, 0b00000000
letters_2: db 0b11110000, 0b10001000, 0b00001000, 0b01111000, 0b10000000, 0b10000000, 0b11111000, 0b00000000
letters_3: db 0b11111000, 0b10001000, 0b00001000, 0b01111000, 0b00001000, 0b10001000, 0b11111000, 0b00000000
letters_4: db 0b00110000, 0b01010000, 0b10010000, 0b11111000, 0b00010000, 0b00010000, 0b00010000, 0b00000000
letters_6: db 0b11111000, 0b10000000, 0b10000000, 0b11110000, 0b00001000, 0b00001000, 0b11110000, 0b00000000
letters_7: db 0b01110000, 0b10001000, 0b10000000, 0b11110000, 0b10001000, 0b10001000, 0b01110000, 0b00000000
letters_8: db 0b11111000, 0b00001000, 0b00010000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000
letters_9: db 0b01110000, 0b10001000, 0b10001000, 0b01110000, 0b10001000, 0b10001000, 0b01110000, 0b00000000
letters_colon: db 0b01110000, 0b10001000, 0b10001000, 0b01111000, 0b00001000, 0b00001000, 0b01110000, 0b00000000
letters_semicolon: db 0b00000000, 0b00100000, 0b00100000, 0b00000000, 0b00100000, 0b00100000, 0b00000000, 0b00000000
letters_leftarrow: db 0b00000000, 0b00100000, 0b00100000, 0b00000000, 0b00100000, 0b01000000, 0b00000000, 0b00000000
letters_equal: db 0b00011000, 0b00100000, 0b01000000, 0b10000000, 0b01000000, 0b00100000, 0b00011000, 0b00000000
letters_rightarrow: db 0b00000000, 0b00000000, 0b01111000, 0b00000000, 0b01111000, 0b00000000, 0b00000000, 0b00000000
letters_question: db 0b11000000, 0b00100000, 0b00010000, 0b00001000, 0b00010000, 0b00100000, 0b11000000, 0b00000000
letters_email: db 0b00110000, 0b01001000, 0b00001000, 0b00010000, 0b00100000, 0b00000000, 0b00100000, 0b00000000
letters_email2: db 0b00110000, 0b01001000, 0b00001000, 0b00010000, 0b00100000, 0b00000000, 0b00100000, 0b00000000;skips it whenever I only put it in once
letters_A: db 0b00100000, 0b01010000, 0b10001000, 0b10001000, 0b11111000, 0b10001000, 0b10001000, 0b00000000
letters_B: db 0b11110000, 0b10001000, 0b10001000, 0b11110000, 0b10001000, 0b10001000, 0b11110000, 0b00000000
letters_C: db 0b00111000, 0b01000000, 0b10000000, 0b10000000, 0b10000000, 0b01000000, 0b00111000, 0b00000000
letters_D: db 0b11110000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b11110000, 0b00000000
letters_E: db 0b11111000, 0b10000000, 0b10000000, 0b11100000, 0b10000000, 0b10000000, 0b11111000, 0b00000000
letters_F: db 0b11111000, 0b10000000, 0b10000000, 0b11100000, 0b10000000, 0b10000000, 0b10000000, 0b00000000
letters_G: db 0b01110000, 0b10001000, 0b10000000, 0b10000000, 0b10011000, 0b10001000, 0b01110000, 0b00000000
letters_H: db 0b10001000, 0b10001000, 0b10001000, 0b11111000, 0b10001000, 0b10001000, 0b10001000, 0b00000000
letters_I: db 0b11111000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b11111000, 0b00000000
letters_J: db 0b01111000, 0b00010000, 0b00010000, 0b00010000, 0b10010000, 0b10010000, 0b01100000, 0b00000000
letters_K: db 0b10001000, 0b10010000, 0b10100000, 0b11000000, 0b10100000, 0b10010000, 0b10001000, 0b00000000
letters_L: db 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b11111000, 0b00000000
letters_M: db 0b10001000, 0b11011000, 0b10101000, 0b10101000, 0b10001000, 0b10001000, 0b10001000, 0b00000000
letters_N: db 0b10001000, 0b11001000, 0b10101000, 0b10101000, 0b10101000, 0b10011000, 0b10001000, 0b00000000
letters_O: db 0b01110000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b01110000, 0b00000000
letters_P: db 0b11110000, 0b10001000, 0b10001000, 0b11110000, 0b10000000, 0b10000000, 0b10000000, 0b00000000
letters_Q: db 0b01100000, 0b10010000, 0b10010000, 0b10010000, 0b10110000, 0b10110000, 0b01111000, 0b00000000
letters_R: db 0b11110000, 0b10001000, 0b10001000, 0b11110000, 0b10100000, 0b10010000, 0b10001000, 0b00000000
letters_S: db 0b01111000, 0b10000000, 0b10000000, 0b01110000, 0b00001000, 0b00001000, 0b11110000, 0b00000000
letters_T: db 0b11111000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000
letters_U: db 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b01110000, 0b00000000
letters_V: db 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b10001000, 0b01010000, 0b00100000, 0b00000000
letters_W: db 0b10001000, 0b10101000, 0b10101000, 0b10101000, 0b10101000, 0b10101000, 0b01010000, 0b00000000
letters_X: db 0b10001000, 0b01010000, 0b01010000, 0b00100000, 0b01010000, 0b01010000, 0b10001000, 0b00000000
letters_Y: db 0b10001000, 0b10001000, 0b01010000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000
letters_Z: db 0b11111000, 0b00001000, 0b00010000, 0b00100000, 0b01000000, 0b10000000, 0b11111000, 0b00000000
letters_halfsquare1: db 0b11100000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b11100000, 0b00000000
letters_backwardsslash: db 0b10000000, 0b01000000, 0b00100000, 0b00100000, 0b00010000, 0b00001000, 0b00001000, 0b00000000
letters_halfsquare2: db 0b11100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b11100000, 0b00000000
letters_idk2: db 0b00100000, 0b01010000, 0b10001000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_idk3: db 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b11111000, 0b00000000
letters_alttilde: db 0b01000000, 0b00100000, 0b00010000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_a: db 0b00000000, 0b00000000, 0b01100000, 0b00010000, 0b11110000, 0b10010000, 0b01111000, 0b00000000
letters_b: db 0b10000000, 0b10000000, 0b10000000, 0b11110000, 0b10001000, 0b10001000, 0b11110000, 0b00000000
letters_c: db 0b00000000, 0b00000000, 0b01110000, 0b10000000, 0b10000000, 0b10000000, 0b01110000, 0b00000000
letters_d: db 0b00001000, 0b00001000, 0b00001000, 0b01111000, 0b10001000, 0b10001000, 0b01111000, 0b00000000
letters_e: db 0b00000000, 0b00000000, 0b01110000, 0b10001000, 0b11111000, 0b10000000, 0b01111000, 0b00000000
letters_f: db 0b00111000, 0b01000000, 0b01000000, 0b11111000, 0b01000000, 0b01000000, 0b01000000, 0b00000000
letters_g: db 0b00000000, 0b01111000, 0b10001000, 0b10001000, 0b01111000, 0b00001000, 0b01111000, 0b00000000
letters_h: db 0b10000000, 0b10000000, 0b10000000, 0b11110000, 0b10001000, 0b10001000, 0b10001000, 0b00000000
letters_i: db 0b00000000, 0b00100000, 0b00000000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000
letters_j: db 0b00100000, 0b00000000, 0b00100000, 0b00100000, 0b00100000, 0b10100000, 0b01000000, 0b00000000
letters_k: db 0b00000000, 0b10000000, 0b10001000, 0b10010000, 0b11100000, 0b10010000, 0b10001000, 0b00000000
letters_l: db 0b00000000, 0b01100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00110000, 0b00000000
letters_m: db 0b00000000, 0b00000000, 0b10001000, 0b11011000, 0b10101000, 0b10101000, 0b10001000, 0b00000000
letters_n: db 0b00000000, 0b00000000, 0b10000000, 0b11110000, 0b10001000, 0b10001000, 0b10001000, 0b00000000
letters_o: db 0b00000000, 0b00000000, 0b01110000, 0b10001000, 0b10001000, 0b10001000, 0b01110000, 0b00000000
letters_p: db 0b00000000, 0b00000000, 0b11110000, 0b10001000, 0b11110000, 0b10000000, 0b10000000, 0b00000000
letters_q: db 0b00000000, 0b00000000, 0b01111000, 0b10001000, 0b01111000, 0b00001000, 0b00001000, 0b00000000
letters_r: db 0b00000000, 0b00000000, 0b10110000, 0b11001000, 0b10000000, 0b10000000, 0b10000000, 0b00000000
letters_s: db 0b00000000, 0b00000000, 0b01111000, 0b10000000, 0b01110000, 0b00001000, 0b11110000, 0b00000000
letters_t: db 0b00100000, 0b00100000, 0b11111000, 0b00100000, 0b00100000, 0b00101000, 0b00010000, 0b00000000
letters_u: db 0b00000000, 0b00000000, 0b10010000, 0b10010000, 0b10010000, 0b10010000, 0b01111000, 0b00000000
letters_v: db 0b00000000, 0b00000000, 0b10001000, 0b10001000, 0b10001000, 0b01010000, 0b00100000, 0b00000000
letters_w: db 0b00000000, 0b00000000, 0b10001000, 0b10001000, 0b10101000, 0b10101000, 0b01010000, 0b00000000
letters_x: db 0b00000000, 0b00000000, 0b10001000, 0b01010000, 0b00100000, 0b01010000, 0b10001000, 0b00000000
letters_y: db 0b00000000, 0b00000000, 0b10001000, 0b01010000, 0b00100000, 0b00100000, 0b11000000, 0b00000000
letters_z: db 0b00000000, 0b00000000, 0b11111000, 0b00001000, 0b01110000, 0b10000000, 0b11111000, 0b00000000
letters_idk4: db 0b00110000, 0b01000000, 0b01000000, 0b11000000, 0b01000000, 0b01000000, 0b00110000, 0b00000000
letters_pipeiguess: db 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00100000, 0b00000000
letters_idk5: db 0b11000000, 0b00100000, 0b00100000, 0b00110000, 0b00100000, 0b00100000, 0b11000000, 0b00000000
letters_tilde: db 0b00000000, 0b01001000, 0b10110000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000
letters_errorchar: db 0b10101000, 0b01010100, 0b10101000, 0b01010100, 0b10101000, 0b01010100, 0b10101000, 0b00000000
letters_end: db 0b11111111	;the termination char - makes ending the vram loading loop take much less code. Just keep in mind that no font sprite can have a straight line across or it will end the loop early resulting in not all the character fonts getting loaded

spritesLoadedOk: db "Sprites loaded",0

;graphic sprites for g4test command
spritel1: db 00011000b
spritel2: db 00111100b
spritel3: db 01011010b
spritel4: db 11111111b
spritel5: db 11011011b
spritel6: db 01100110b
spritel7: db 00111100b
spritel8: db 00011000b

spritel9: db 00001000b
spritel10: db 00011000b
spritel11: db 00011000b
spritel12: db 00111100b
spritel13: db 00111100b
spritel14: db 01111110b
spritel15: db 11111111b
spritel16: db 11111111b
spriteTerminate: db 00000000b