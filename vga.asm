;the vga driver code needs to be segregated to this file

;it's actually not "setting" vga mode 3. It's just using the already set up mode 3 that every 286 and later bios uses
vgaSetupText3h:
    mov ax, 0B800h
    mov es, ax
    mov di, 0

    mov cx, 2048
    vgaSetupText3hLoop:

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
    ja vgaSetupText3hLoop

    ;don't forget to zero out the cursor and set it
    mov cl, 0
    call setCurX
    mov cl, 0
    call setCurY
ret

;updates the cursor on the screen to the correct spot
vgaCursorPosToVram:
        ;calculate the correct line number from cursorY
        mov al, [ds:cursorY]
        mov ah, 0
        mov bx, 00A0h
        mul bx
        mov di, ax

        ;add the column number in cursorX to di so that the character prints to the correct column
        mov al, [ds:cursorX]
        mov ah, 0
        mov bx, 2
        mul bx
        add di, ax

        ;make it so that the vga cursor reflects these values
        call setCursorPosBasedOnData
ret

;parameters: cl = color. ax = char
vgaPrintChar:
    ;print the character byte
    mov [es:di], al
    inc di

    ;print the color byte
    mov al, cl
    mov [es:di], al
    inc di
ret

vgaIncrementCursor:
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

vgaDecrementCursor:
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

vgaTextModeNewline:
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
    jl vgaTextModeNewlineExit

    ;set cl for 2 rows of scrolling
    mov cl, 2
    call scrollScreen

    ;be sure to update the blinking cursor values and stuff
    mov al, [ds:cursorY]
    sub al, 2
    mov [ds:cursorY], al

    vgaTextModeNewlineExit:
    ;for things to show up correctly, recalculate cursor values whether or not the screen got moveda
    call calculateCursorValues
ret

vgaTextModeBackspace:
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

;call this subroutine any time you want to enter a new line in text mode
;screen scrolling is a driver-specific process so each video driver scrolling subroutine will have to contain code for scrolling on the target device
vgaNewline:
ret

;fetch the most recently typed-in thing from vram
vgaVramToCommandBuffer:
    ;calculate the correct line number from cursorY
    mov al, [ds:cursorY]
    mov ah, 0
    mov bx, 00A0h
    mul bx
    mov cx, ax

    ;add the column number in cursorX to di so that the character prints to the correct column
    mov al, 0
    mov ah, 0
    mov bx, 2
    mul bx
    add cx, ax

    mov bx, cx
    mov cx, commandHistory

    vgaVramToCommandBufferCopyLoop:
    mov al, [es:bx]
    xchg bx, cx
    mov [ds:bx], al
    xchg bx, cx
    inc cx
    add bx, 2
    cmp al, 0
    jne vgaVramToCommandBufferCopyLoop

    ;done. now copy a null character just because
    xchg bx, cx
    ;inc bx
    mov al, 0
    mov [ds:bx], al

    ;and then copy a $ character just for good measure. (the string print function was updated to terminate on null characters instead of $ but i dont want to change something that currently works)
    inc bx
    mov al, "$"
    mov [ds:bx], al
ret