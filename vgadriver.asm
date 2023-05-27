;try to get all the vga parameters
;
;
;
;
;
;
;

;note: host system is supposed to store the following vga values at the following ram locations:
;   449 = current video mode
;   44A = number of columns on screen
;   44C = length of active screen buffer
;   44E = start of current page
;   450 = cursor positions
;   460 = cursor mode
;   462 = current display page
;   463 = base host address of port
;   465 = current mode
;   466 = current color
;   484 = number of character rows

;none of this stuff was stolen from here (jk it was): https://github.com/kaneton/appendix-bios/blob/master/src/video.asm

vga_getModeControl:

        ;set CRTC address register to mode control register 0x17
        mov al, 0x0E
        mov dx, [ds:CRTCAdr]
        out dx, al

        ;read the data value at address
        mov dx, [ds:CRTCData]
        push dx
            mov bx, crtcmodeString
            mov cl, 00001111b
            call printString
        pop dx

        ;move value of mode which is in dl and move it to al
        mov dl, al
        call alToScreenHex
ret

;changes palette register number in ah to whatever value is in al
vga_editPaletteReg:
 
    mov cl, al ;save al for later

    mov dx, 3DAh ;read the status of input status #1 register which will reset the attribute access register to index selection mode
    in al, dx

    and ah, 00011111b
    mov dx, 3C0h        ;set al to address of attribute controller register
    mov al, ah
    out dx, al          ;set ah and index number to write value to attribute controller address register
    ;now the flipflop should have changed value so next write to 03C0h will be the value of the color

    ;next write to register 3C0h will be flipflopped to the attribute controller data register
    mov al, cl
    out dx, al

    ;now change bit 5 of attribute address register back to 1
    mov dx, 3DAh
    in al, dx       ;flip ram port so that the video card can read the palette registers instead of the host system
    mov dx, 3C0h
    mov al, 00100000b
    out dx, al

ret

vga_printChar:
    push ax
        mov ax, 0A000h
        mov es, ax
    pop ax

    mov al, 00000001b
    mov [es:di], al
    inc di
    mov al, 00010001b
    mov [es:di], al
    inc di
    mov al, 00100010b
    mov [es:di], al
    inc di
    mov al, 00100010b
    mov [es:di], al
    inc di
    mov al, 00110011b
    mov [es:di], al
    inc di
    mov al, 01000100b
    mov [es:di], al
    inc di
    mov al, 01010101b
    mov [es:di], al
    inc di

    ;mov [es:di], al
    ;inc di
    ;mov al, cl
    ;mov [es:di], al
    ;inc di

ret

;changes bit plane
;inputs: al = number of bit plane to use. Valid values are 0, 1, 2 and 3
vga_readMapSelect:
    mov dx, graphCtrlAddress    ;put address of graphics controller address register into dx
    mov cl, al                  ;put al into cl for safe keeping since we have to use al for io operations
    mov al, 04h                 ;index 4 = read map select (aka bit plane select register)
    out dx, al                  ;write index of read map select to address 03CEh

    mov dx, graphCtrlData       ;put address of graphics controller data register into dx
    mov al, cl                  ;put bit plane number back into cl
    out dx, al                  ;write provided bit plane value into 03CFh
ret

;change mask map register to whatever is in cl
vga_maskMap:
    mov dx, 03C4h               ;put sequence index register address into dx
    mov al, 2                   ;index 2 = map mask register
    out dx, al                  ;write index to index register

    mov dx, 03C5h               ;sequence data register
    mov al, cl                  ;put cl into al
    out dx, al                  ;and uh, that should do it

ret

;set set reset register to cl
vga_setreset_reg:
    mov dx, graphCtrlAddress    ;put address of graphics controller address register into dx
    mov cl, al                  ;put al into cl for safe keeping since we have to use al for io operations
    mov al, 01h                 ;index 1 = set reset register
    out dx, al                  ;write index of read map select to address 03CEh

    mov dx, graphCtrlData       ;put address of graphics controller data register into dx
    mov al, cl                  ;put bit plane number back into cl
    out dx, al 
ret

;maybe cycling through all he write mode settings will result in getting colors other than black and white working in 16 color graphics modes
;cl = whatever you want to change the write mode to. only bits 0 and 1 are respected
vga_ChangeWriteMode:
    mov dx, graphCtrlAddress
    mov al, 05h                 ;5 = index of mode register
    out dx, al

    mov dx, graphCtrlData       ;switch to control register address but we need to read the values
    in al, dx

    and cl, 00000011h           ;reset everything except bit 0 and 1 in cl register
    or al, cl                   ;paste relevent bits of cl over al

    mov dx, graphCtrlData
    out dx, al                  ;put that back in register

ret

colsPerRow dw 0x0000
rowsPerCol dw 0x0000
;gets target resolution of given 16 color graphics or text mode and copies it to colsPerRow and rowsPerCol
vga_resOfVgaGraphMode:
    mov bx, 0449h                   ;mov address of current video mode into bx
    mov al, [ds:bx]                 ;put value of current video mode into al
    mov [ds:vga_drawPoint_debugByte1], al

    cmp al, 03h                     ;check for 80x25 text mode
    je vga_resOfVgaGraphMode_03h    ;if mode 03h, load registers with 03h mode values
    cmp al, 0Dh                     ;check for 320x200 16 color graphics mode
    je vga_resOfVgaGraphMode_0Dh    ;if mode 0Dh, load registers with 0Dh mode values
    cmp al, 10h                     ;check for 640x350 16 color graphics mode
    je vga_resOfVgaGraphMode_10h    ;if mode 10h, load registers with 10h mode values
    cmp al, 12h                     ;check for 640x480 16 color graphics mode
    je vga_resOfVgaGraphMode_12h    ;if mode 12h, load registers with 12h mode values
    ;ret                             ;return if none of the other conditions are met

    vga_resOfVgaGraphMode_03h:
        mov dx, 80                  ;horizontal character resolution of 03h text mode
        mov cx, 25                  ;vertical character resolution of 03h text mode
        jp vga_resOfVgaGraphMode_writeValues
    vga_resOfVgaGraphMode_0Dh:
        mov dx, 320                 ;x res of 0Dh
        mov cx, 200                 ;y res of 0Dh
        jp vga_resOfVgaGraphMode_writeValues
    vga_resOfVgaGraphMode_10h:
        mov dx, 640                 ;x res of 10h
        mov cx, 350                 ;y res of 10h
        jp vga_resOfVgaGraphMode_writeValues
    vga_resOfVgaGraphMode_12h:
        mov dx, 640                 ;x res of 12h
        mov cx, 480                 ;y res of 12h

    vga_resOfVgaGraphMode_writeValues:
    ;time to write values into ram location
    mov bx, rowsPerCol
    mov ax, cx
    push ax
    mov [ds:bx], ax
    pop ax
    mov [ds:vga_drawPoint_debugWord1], ax

    mov bx, colsPerRow
    mov ax, dx
    push ax
    mov [ds:bx], ax
    pop ax ;TIL that moving ax to memory changes value of ax. wtf bro.
    mov [ds:vga_drawPoint_debugWord2], ax

ret

xPosDrawRect dw 0x0000
yPosDrawRect dw 0x0000
xSizeDrawRect dw 0x0000
ySizeDrawRect dw 0x0000
colorIndexDrawRect db 0x00
;draws a rectangle while in mode 10h
;xPosDrawRect = screen position x
;yPosDrawRect = screen position y
;xSizeDrawRect = sizex
;ySizeDrawRect = sizey
;colorIndexDrawRect = color index. lower 4 bits are your value, upper 4 bits need to be zero
vga_drawRect10h:

ret

;draw a point on the screen. Currently works in 16 color graphics modes including 0Dh, 10h and 12h.
vga_drawPoint_clMask db 00000001b ;use this to rerun vga_drawPoint_plane0 with the values 00000001b, 00000010b, 00000100b and 00001000b
vga_drawPoint_nextBitPlane db 0; legal values are 0, 1, 2 or 3
vga_drawPoint:

    push ax
        mov ax, 0A000h
        mov es, ax
    pop ax

    push cx                                     ;we need to save cx since cl hold the color information
    call vga_screencoordsToNumber_G16           ;convert values in xPosDrawRect and yPosDrawRect into ecx as a byte offset

        mov edx, 0
        mov eax, ecx                                ;put ecx into eax
        mov ebx, 8                             
        div ebx             ;now eax should contain the byte location and edx should contain the bit offset
        
        push eax
        mov [ds:vga_drawPoint_debugWord4], eax  ;but ecx into debug word 3 (type "debug2" at any time to see contents of that debug byte)
        pop eax
    pop cx                                      ;pop cx, as its time to start doing stuff with cl register which contains the pixel color

    pushad
        push eax
            mov al, 00000001b
            mov [ds:vga_drawPoint_clMask], al   ;set clmask to correct value for first pass
            mov al, 0
            mov [ds:vga_drawPoint_nextBitPlane], al
        pop eax

        ;i put this big stupid annoying 16 bit color bit plane multiplexer fucktupler shitgorithm in its own subroutine so I don't have to paste all that fucking bullshit over and over 
        call vga_drawPoint_plane0
    popad

    ret

    pushad
        push eax
            mov al, 00000010b
            mov [ds:vga_drawPoint_clMask], al   ;set clmask to correct value for first pass
            mov al, 1
            mov [ds:vga_drawPoint_nextBitPlane], al
        pop eax

        ;i put this big stupid annoying 16 bit color bit plane multiplexer fucktupler shitgorithm in its own subroutine so I don't have to paste all that fucking bullshit over and over 
        ;call vga_drawPoint_plane0
        ;temporarily commenting this out. I may not actually need it
    popad

    pushad
        push eax
            mov al, 00000100b
            mov [ds:vga_drawPoint_clMask], al   ;set clmask to correct value for first pass
            mov al, 2
            mov [ds:vga_drawPoint_nextBitPlane], al
        pop eax

        ;i put this big stupid annoying 16 bit color bit plane multiplexer fucktupler shitgorithm in its own subroutine so I don't have to paste all that fucking bullshit over and over 
        ;call vga_drawPoint_plane0
        ;temporarily commenting this out. I may not actually need it
    popad

    pushad
        push eax
            mov al, 00001000b
            mov [ds:vga_drawPoint_clMask], al   ;set clmask to correct value for first pass
            mov al, 3
            mov [ds:vga_drawPoint_nextBitPlane], al
        pop eax

        ;i put this big stupid annoying 16 bit color bit plane multiplexer fucktupler shitgorithm in its own subroutine so I don't have to paste all that fucking bullshit over and over 
        ;call vga_drawPoint_plane0
        ;temporarily commenting this out. I may not actually need it
    popad
ret

;draw to a bit plane in the shitty bit plane based vga 16 color modes.
vga_drawPoint_plane0:
        push cx
        push dx
        push eax
            ;mov al, [ds:vga_drawPoint_nextBitPlane]     ;load the selected bit plane from memory
            ;call vga_readMapSelect              ;go to relevant bit plane
            call vga_maskMap
        pop eax
        pop dx
        pop cx
        ;mov cl, 0Fh     ;stop the crash that happens when cl is anything other than 0Fh. i'll actually fix it but not right now


        xchg bx, ax
        mov al, [es:bx]
        xchg bx, ax
        ;now the byte at location eax (the byte in video memory to modify) is in bl

        mov [ds:vga_drawPoint_saveaxForLater], ax   ;save the vram location for later as using the stack is unsuitable

        push cx
            mov al, 1               ;al = 1 for bit offset 0
            mov cl, dl              ;put dl in cl because it has to be used for sal opcode
            sal al, cl              ;load bit offset into al

            ;debug check value of ax
            push ax
            push bx
            mov [ds:vga_drawPoint_debugWord5], dx
            pop bx
            pop ax
            
            ;xchg ax, cx             ;swap color index and bit location ax and cx because I need to do an and instruction
                                    ;now cl contains byte offset value and al contains bit modify mask
            ;if al == 0, reset bit. if al == 1, set the bit
            ;or al, cl
        pop cx
        push cx     ;now we need cl to contain color again instead of containing the bit offset 
            ;and cl, 00000001b           ;for bit plane 1, and al and 1 to get bit to modify. 00000010b for plane 1, 00000100b for plane 2. 00001000b for plane 3
            ;and cl, [ds:vga_drawPoint_clMask]
            mov cl, 00001111b

            cmp cl, 0
            jne plane0_al1
            plane0_al0:
                ;code for condition 0
                mov bl, dl
                and bl, al
                mov [ds:vga_drawPoint_debugByte1], al
            jp plane0_alContinue
            plane0_al1:
                ;code for condition 1
                mov bl, dl
                xor al, 11111111b
                and bl, al
                mov [ds:vga_drawPoint_debugByte1], bl
            plane0_alContinue:
                ;continue code
                mov ax, [ds:vga_drawPoint_saveaxForLater]
                xchg ax, bx
                mov [es:bx], al
                ;mov al, 77h
                ;mov [ds:vga_drawPoint_debugByte1], al
        pop cx

    ;vga_drawPoint_plane1:
    ;vga_drawPoint_plane2:
    ;vga_drawPoint_plane3:

ret

vga_drawPoint_debugByte1 db 0x50
vga_drawPoint_debugWord1 dw 0x0000
vga_drawPoint_debugWord2 dw 0x0000
vga_drawPoint_debugWord3 dd 0x00000000 ;currently connected to "debug2" command
vga_drawPoint_debugWord4 dd 0x00000000 ;currently connected to "debug2" command
vga_drawPoint_debugWord5 dd 0x0000 ;currently connected to "debug2" command
vga_drawPoint_saveaxForLater dw 0x0000

;xPosDrawRect and yPosDrawRect need to be filled with relevant values
;ecx = output in byte offset
;converts provided values into suitable memory locations
screenCordsTemp1 dw 0x0000
screenCordsTemp2 dw 0x0000
vga_screencoordsToNumber_G16:
    mov ax, 0
    mov ds, ax
    ;1 bpp, each bit of color index is in a seperate bit plane. 100% will only work on vga modes 10h and 12h, don't do it in a vesa or v9958 graphics mode

    ;there now everything's in ram where it won't get lost
    ;next, convert size and position values to an address between A0000h and A8000h
    ;make xPosDrawRect the high word and yPosDrawRect the low word

    call vga_resOfVgaGraphMode            ;get resolution of current graphics mode

    mov ax, [ds:yPosDrawRect]             ;load value of yPosDrawRect into ax
    mov bx, [ds:colsPerRow]               ;load number of columns per row for current video mode into bx
    mov dx, 0
    mul bx

    push dx
    mov [ds:screenCordsTemp2], dx
    pop dx
    mov [ds:screenCordsTemp1], ax
    mov ecx, 0                            ;make sure upper word of ecx is zero
    mov ecx, [ds:screenCordsTemp1]        ;get that shit loaded into a 32 bit register since the number can go up to like 1.8 million at 640x480 resolution

    mov bx, xPosDrawRect        ;get address of xposition into bx
    mov eax, 0                  ;make sure upper word of eax is zero
    mov ax, [ds:bx]             ;put 16 bit value at xPosDrawRect into ax
    ;now, xPosDrawRect, yPosDrawRect should have the bit position of where the x,y coordinate should be

    add ecx, eax                ;add values. now ecx should contain the offset from A0000

    push ecx
        mov [ds:vga_drawPoint_debugWord3], ecx  ;but ecx into debug word 3 (type "debug2" at any time to see contents of that debug byte)
    pop ecx

ret

;ok, time for the 3rd or 4th (haven't been keeping track) big fuck of this whole program: Bresenham's line algorithm
;https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
;draws a line in 16 color vga graphics modes
vga_drawLine_16g:
    
ret

;converts provided values into suitable memory locations
;cl - color
;-top of stack-
;ysize (16 bit value)
;xsize (16 bit value)
;ypost (16 bit value)
;xpos (16 bit value)
;after running it, the converted values get saved in ram to xPosDrawRect, yPosDrawRect, xSizeDrawRect, ySizeDrawRect and colorIndexDrawRect
vga_screencoordsToNumber_G256:
    ;8 bpp
ret

crtcmodeString db "crtc mode: $"