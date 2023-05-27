;modifies ax, bx and cx
;makes bx contain address of vram when a segment register is set to B800
cursorPosToBx:
		;calculate the correct line number from cursorY
        mov al, [cursorY]
        mov ah, 0
        mov bx, 00A0h
        mul bx
        mov cx, ax

        ;add the column number in cursorX to di so that the character prints to the correct column
        mov al, [cursorX]
        mov ah, 0
        mov bx, 2
        mul bx
        add cx, ax

        mov bx, cx
ret

lineToCommandBuffer:

    ;calculate the correct line number from cursorY
    mov al, [cursorY]
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

    lineToCommandBufferCopyLoop:
    mov al, [es:bx]
    xchg bx, cx
    mov [ds:bx], al
    xchg bx, cx
    inc cx
    add bx, 2
    cmp al, 0
    jne lineToCommandBufferCopyLoop

    ;done. now copy a null character just because
    xchg bx, cx
    ;inc bx
    mov al, 0
    mov [ds:bx], al

    ;and then copy a $ character do you can plug this into the print string function
    inc bx
    mov al, "$"
    mov [ds:bx], al


ret

;bx = start address of string 1 you want to compare. For example, put a command in this one
;ax = start address of string 2 you want to compare. For example, put command buffer start in this one
;returns cl = 1 if equal. cl = 0 if false
;modifies ax, bx, and cx
doesStringContain:

    mov cl, [ds:bx]
    xchg ax, bx
    mov ch, [ds:bx]
    xchg ax, bx
    inc ax
    inc bx

    cmp cl, 0
    je doesStringContainYes
    cmp cl, ch
    je doesStringContain
    jmp doesStringContainNo

    doesStringContainYes:
        mov cl, 1
    ret

    doesStringContainNo:
        mov cl, 0

ret

processCommands:
    
    ;the help command
	mov ax, commandHistory
    mov bx, helpcmd
    call doesStringContain
    cmp cl, 1
    je helpCommand

    ;the read command
    mov ax, commandHistory
    mov bx, readcmd
    call doesStringContain
    cmp cl, 1
    je readCommand

    ;the write command
    mov ax, commandHistory
    mov bx, writecmd
    call doesStringContain
    cmp cl, 1
    je writeCommand

    ;clear screen command
    mov ax, commandHistory
    mov bx, clearcmd
    call doesStringContain
    cmp cl, 1
    je clearScreenCommand

    ;jump command
    mov ax, commandHistory
    mov bx, jmpcmd
    call doesStringContain
    cmp cl, 1
    je jmpCommand

    ;call command
    mov ax, commandHistory
    mov bx, callcmd
    call doesStringContain
    cmp cl, 1
    je callCommand

    ;pciinfo command
    mov ax, commandHistory
    mov bx, pciinfocmd
    call doesStringContain
    cmp cl, 1
    je pciinfoCommand

    ;gtest command
    mov ax, commandHistory
    mov bx, graphicsTest
    call doesStringContain
    cmp cl, 1
    je gtestCommand

    ;colortest command
    mov ax, commandHistory
    mov bx, colorTestcmd
    call doesStringContain
    cmp cl, 1
    je colorTestCommand

    ;debug1 command
    mov ax, commandHistory
    mov bx, debug1cmd
    call doesStringContain
    cmp cl, 1
    je debug1Command

    ;debug2 command
    mov ax, commandHistory
    mov bx, debug2cmd
    call doesStringContain
    cmp cl, 1
    je debug2Command

    ;debug3 command
    mov ax, commandHistory
    mov bx, debug3cmd
    call doesStringContain
    cmp cl, 1
    je debug3Command

    ;if it go to this point, the user didn't type a real command. Print the error message
    mov bx, invalidcommandmsg
    mov cl, 00001111b
    call printString
    call textModeNewLine

ret

helpCommand:
    ;guess fucking what it's time to change the help command to something actually useful
    mov bx, helpmsg
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, helpcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, readcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, writecmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, clearcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, jmpcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, callcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, pciinfocmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, graphicsTest
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, colorTestcmd
    mov cl, 00001111b
    call printString
    call textModeNewLine

ret

readcmdinfo db "proper usage of this command is: 'read xxxx:xxxx' where x is a hexadecimal number between 0-F$"
readCommandSpacesPerRow equ 0x10        ;max number of spaces per row
remainingLinesToPrint db 0
readCommandLinesToPrint equ 16
;the all important hex reader command
readCommand:
    mov bx, remainingLinesToPrint
    mov al, readCommandLinesToPrint
    mov [ds:bx], al

    mov bx, commandHistory
    ;add bx, 5
    add bx, 9

    mov al, [ds:bx]; check to see if the user typed in something that looks like "read xxxx:xxxx"
    cmp al, ":"
    jne readCommandInvalidUsage
    add bx, 5
    mov al, [ds:bx]     ;check to see if there is a blank space at the end of the segment address
    cmp al, " "
    je readCommandContinue
    cmp al, 0
    je readCommandContinue
    jmp readCommandInvalidUsage

    readCommandContinue:

    ;convert the inputted address from ascii into workable form then save into ram
    
    ;set bx to point to the command buffer
    mov bx, commandHistory
    ;the address parameters start at array spot 5
    add bx, 5

    ;convert the 16 bit segment then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:hexReadValueHigh], ax

    ;advance 2 spaces to get into the offset parameter
    add bx, 2

    ;convert the 16 bit offset then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:hexReadValueLow], ax

    mov cl, 11
    call setCurX
    call calculateCursorValues
    ;draw columns at the top for hex spaces
    mov al, 0
    readCommandInitialColumnLoop:
    push ax
        call alToScreenHex
        mov al, " "
        mov cl, 00001111b
        call printChar
    pop ax
    inc al
    cmp al, readCommandSpacesPerRow
    jne readCommandInitialColumnLoop

    call textModeNewLine

    ;now, the correct segment and offset values are saved into ram
    ;use the fs register for segment since i'm not using it for anything else
    mov fs, [ds:hexReadValueHigh]
    ;use bx register for offset
    mov bx, [ds:hexReadValueLow]

    rowHexLoopWhatever:
    push bx
        mov ax, fs
        push ax
            mov al, ah
            call alToScreenHex
        pop ax
        call alToScreenHex
        mov al, ":"
        mov cl, 00001111b
        call printChar
    pop bx

    mov al, bh
    push bx
        call alToScreenHex
    pop bx
    mov al, bl
    push bx
        call alToScreenHex

        mov al, " "
        mov cl, 00001111b
        call printChar
        mov al, " "
        mov cl, 00001111b
        call printChar

    pop bx

    mov cl, readCommandSpacesPerRow

    charLoopHexRowFuckPoop:
    mov al, [fs:bx]
    push bx
    push cx
        call alToScreenHex
        mov al, " "
        mov cl, 00001111b
        call printChar
    pop cx
    pop bx
    inc bx
    dec cl
    cmp cl, 0
    jne charLoopHexRowFuckPoop

    ;make a bigger space between the hex data and the ascii data
    push bx
    push cx
        call alToScreenHex
        mov al, " "
        mov cl, 00001111b
        call printChar
    pop cx
    pop bx

    ;now, draw the shit in ascii form at the edge of the screen
    mov cl, readCommandSpacesPerRow
    sub bx, readCommandSpacesPerRow

    charLoopAsciiRowFuckPoop:
    mov al, [fs:bx]

    push cx
    push bx
        ;make invalid ascii characters show up as dots
        cmp al, 31
        ja dontModifyAl
        mov al, "."

        dontModifyAl:
        mov cl, 00001111b
        call printChar
    pop bx
    pop cx
    inc bx
    dec cl
    cmp cl, 0
    jne charLoopAsciiRowFuckPoop


    push bx
        mov bx, remainingLinesToPrint
        mov al, [ds:bx]
        dec al
        mov [ds:bx], al
        push ax
        call textModeNewLine
        pop ax
    pop bx
    cmp al, 0
    jne rowHexLoopWhatever

    ;mov bx, readcmdnotimplemented
    ;mov cl, 00001111b
    ;call printString
    ;call textModeNewLine

    ret

    ;error. remind user how to use this command
    readCommandInvalidUsage:
    mov bx, invalidparametersmsg
    mov cl, 00001111b
    call printString
    call textModeNewLine

    mov bx, readcmdinfo
    mov cl, 00001111b
    call printString
    call textModeNewLine


ret

writeCommandErrorString db "Hey asshole, that isn't how you use the write command. The syntax is 'write xxxx:xxxx xx'$"
writeCommandErrorStringAlmost db "Close, but you forgot to enter a byte to write. Try 'write 0000:8A00 AA'$"
;write a byte to manually inputted location in memory$"
writeCommand:
    ;mov bx, genericGoFuckYourself
    ;mov cl, 00001111b
    ;call printString
    ;call textModeNewLine

    ;command format is "write xxxx:xxxx xx" where x has to be 0-9, a-f or A-F
    mov bx, commandHistory
    add bx, 10

    mov al, [ds:bx]; check to see if the user typed in something that looks like "write xxxx:xxxx"
    cmp al, ":"
    jne writeCommandInvalidUsage
    add bx, 6
    mov al, [ds:bx]     ;check to see if there is a blank space at the end of the segment address
    cmp al, " "
    je writeCommandContinue
    cmp al, 0           ;if the command ends as 'write xxxx:xxxx', it doesn't have a data byte. Inform the user of their mistake. Otherwise, continue
    je writeCommandInvalidUsageForgotData
    jmp writeCommandContinue

    writeCommandContinue:

        ;set bx to point to the command buffer
        mov bx, commandHistory
        ;the address parameters start at array spot 5
        add bx, 6

        ;convert the 16 bit segment then save to ram
        mov ch, [ds:bx]
        inc bx
        mov cl, [ds:bx]
        inc bx
        mov ah, [ds:bx]
        inc bx
        mov al, [ds:bx]
        call asciiToHex16
        mov [ds:hexReadValueHigh], ax

        ;advance 2 spaces to get into the offset parameter
        add bx, 2

        ;convert the 16 bit offset then save to ram
        mov ch, [ds:bx]
        inc bx
        mov cl, [ds:bx]
        inc bx
        mov ah, [ds:bx]
        inc bx
        mov al, [ds:bx]
        call asciiToHex16
        mov [ds:hexReadValueLow], ax

        ;get the parameter byte
        add bx, 2
        mov ax, [ds:bx]
        ;convert 8 bit ascii value from ascii to binary
        xchg al, ah
        call asciiToHex
        mov [ds:hexWriteByte], ax

        mov fs, [ds:hexReadValueHigh]
        mov bx, [ds:hexReadValueLow]
        mov al, [ds:hexWriteByte]
        ;hexWriteByte
        mov [fs:bx], al
        ;call alToScreenHex


    ret

    ;user forgot to input byte to write to given address
    writeCommandInvalidUsageForgotData:
        ;print an error string before returning. Inform the user exactly what they did wrong
        mov bx, writeCommandErrorStringAlmost
        mov cl, 00001111b
        call printString
        call textModeNewLine
    ret

    writeCommandInvalidUsage:

        ;print an error string before returning
        mov bx, writeCommandErrorString
        mov cl, 00001111b
        call printString
        call textModeNewLine

ret

clearScreenCommand:

    call setupBlankScreen
    mov cl, 0
    call setCurX
    mov cl, 0
    call setCurY
    call calculateCursorValues

ret

jmpValueLow dw 0x0000       ;offset
jmpValueHigh dw 0x0000      ;segment
jmpCommandError db "Guess what dumbass? You typed it wrong. Im not gonna give you any useful help though so get REKT.$"
jmpCommand:

    ;first, check if user put in a valid segment
    mov bx, commandHistory
    ;add bx, 5
    add bx, 9

    mov al, [ds:bx]; check to see if the user typed in something that looks like "read xxxx:xxxx"
    cmp al, ":"
    jne jmpCommandInvalidUsage
    add bx, 5
    mov al, [ds:bx]     ;check to see if there is a blank space at the end of the segment address
    cmp al, " "
    je jmpCommandContinue
    cmp al, 0
    je jmpCommandContinue
    jmp jmpCommandInvalidUsage

    jmpCommandInvalidUsage:
        mov bx, jmpCommandError
        mov cl, 00001111b
        call printString
        call textModeNewLine
        ret

    jmpCommandContinue:

    ;convert the inputted address from ascii into workable form then save into ram
    
    ;set bx to point to the command buffer
    mov bx, commandHistory
    ;the address parameters start at array spot 5
    add bx, 5

    ;convert the 16 bit segment then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:jmpValueHigh], ax

    ;advance 2 spaces to get into the offset parameter
    add bx, 2

    ;convert the 16 bit offset then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:jmpValueLow], ax

    mov fs, [ds:jmpValueHigh]
    mov bx, [ds:jmpValueLow]

    ;most of the time this is probably the last time the computer will ever work properly again until a reset but I'll put a newline under it just because
    jmp [fs:bx]

    call textModeNewLine


ret

callCommand:
    
    ;first, check if user put in a valid segment
    mov bx, commandHistory
    ;add bx, 5
    add bx, 9

    mov al, [ds:bx]; check to see if the user typed in something that looks like "read xxxx:xxxx"
    cmp al, ":"
    jne callCommandInvalidUsage
    add bx, 5
    mov al, [ds:bx]     ;check to see if there is a blank space at the end of the segment address
    cmp al, " "
    je callCommandContinue
    cmp al, 0
    je callCommandContinue
    jmp callCommandInvalidUsage

    callCommandInvalidUsage:
        mov bx, jmpCommandError
        mov cl, 00001111b
        call printString
        call textModeNewLine
        ret

    callCommandContinue:

    ;convert the inputted address from ascii into workable form then save into ram
    
    ;set bx to point to the command buffer
    mov bx, commandHistory
    ;the address parameters start at array spot 5
    add bx, 5

    ;convert the 16 bit segment then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:jmpValueHigh], ax

    ;advance 2 spaces to get into the offset parameter
    add bx, 2

    ;convert the 16 bit offset then save to ram
    mov ch, [ds:bx]
    inc bx
    mov cl, [ds:bx]
    inc bx
    mov ah, [ds:bx]
    inc bx
    mov al, [ds:bx]
    call asciiToHex16
    mov [ds:jmpValueLow], ax

    mov fs, [ds:jmpValueHigh]
    mov bx, [ds:jmpValueLow]

    ;most of the time this is probably the last time the computer will ever work properly again until a reset but I'll put a newline under it just because
    call [fs:bx]

    ;mov ax, fs

    ;push bx
    ;    push ax
    ;        mov al, ah
    ;        call alToScreenHex
    ;    pop ax
    ;    call alToScreenHex
    ;pop bx
    ;mov ax, bx
    ;push ax
    ;    mov al, ah
    ;    call alToScreenHex
    ;pop ax
    ;call alToScreenHex
    
    call textModeNewLine

ret

pciinfoCommand:
    
    ;print the string in preparation to display the pci present byte
    mov bx, pcipresent
    mov cl, 0Fh
    call printString
    ;trigger bios interrupt 1A with ax set to B101
    mov ax, 0B101h
    int 1Ah

    ;save bx and cx registers into ram for later because I don't feel like using a gajillion different push and pop commands
    mov [ds:pcibxsave], bx
    mov [ds:pcicxsave], cx

    ;hopefully, ah = present status. al = hardware mechanism. bh = interface level major version. bl = interface level minor version. cl = number of last pci bus in system
    push ax
        ;print the "present status" byte to the screen
        call alToScreenHex
    pop ax
    
    ;print the neccesary strings for pci
    push ax
        call textModeNewLine
        mov bx, pcimech
        mov cl, 0Fh
        call printString
    pop ax
    ;;print the hardware mechanism byte to the screen
    xchg al, ah
    call alToScreenHex

    ;print the strings for pci major version line
    call textModeNewLine
    mov bx, mjver
    mov cl, 0Fh
    call printString

    ;;retrieve bx values from where they got saved in ram
    mov ax, [ds:pcibxsave]
    
    ;print major version to screen
    xchg al, ah    
    push ax
        call alToScreenHex

        ;print the strings for pci minor version line
        call textModeNewLine
        mov bx, mnver
        mov cl, 0Fh
        call printString

    pop ax

    ;print the settings for pci minor version line
    xchg al, ah
    call alToScreenHex

    ;prepare and print the string for last pci bus value
    call textModeNewLine
    mov bx, lastpcibus
    mov cl, 0Fh
    call printString

    ;;retriev values of cx register
    mov ax, [ds:pcicxsave]
    call alToScreenHex

    call textModeNewLine
    ;;done

ret

;in gtest, switch to a graphics mode and switch back to text mode when user presses esc
gtestCommand:

    ;set graphics mode to mode 10h
    ;remember 10h and 12h use bit planes
    ;mode 13h doesn't use that bit plane crap but its only 320x200
    mov ah, 00h
    mov al, 0Dh
    int 10h

    ;draw some test points
    mov ax, 76
    mov [ds:xPosDrawRect], ax
    mov ax, 5
    mov [ds:yPosDrawRect], ax
    mov cl, 00001111b
    call vga_drawPoint

    mov ax, 77
    mov [ds:xPosDrawRect], ax
    mov ax, 4
    mov [ds:yPosDrawRect], ax
    mov cl, 00001111b
    call vga_drawPoint

    mov ax, 81
    mov [ds:xPosDrawRect], ax
    mov ax, 4
    mov [ds:yPosDrawRect], ax
    mov cl, 00001111b
    call vga_drawPoint

    mov ax, 85
    mov [ds:xPosDrawRect], ax
    mov ax, 4
    mov [ds:yPosDrawRect], ax
    mov cl, 00001111b
    call vga_drawPoint

    call waitKey            ;wait for the user to press any key

    ;the following code generally switches back to 03h 16 color text mode pretty reliably
    mov ah, 00h
    mov al, 03h
    int 10h
    call setupBlankScreen
    mov ax, 0B800h
    mov es, ax

ret

colorTestCommand:
    ;loop through all 16 mode 03h colors
    mov cl, 0
    mov al, "0"
    mov bx, 0

    colorTestCommandLoop:
        push ax
        push cx
        push bx
            call printChar
            call textModeNewLine
        pop bx
        pop cx
        pop ax

        inc bx
        inc cl
        inc al

        cmp al, ":"
        jne colorTestCommandLoop_continue
        colorTestCommandLoop_switchToA:
        mov al, "A"
        colorTestCommandLoop_continue:
        cmp bx, 16
        jne colorTestCommandLoop

    colorTestCommandLoopEnd:
ret


debug1Command:
    mov al, [ds:vga_drawPoint_debugByte1]
    call alToScreenHex

    call textModeNewLine
ret

debug2Command:
    mov bx, vga_drawPoint_debugWord3
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx

    call textModeNewLine

    mov bx, vga_drawPoint_debugWord4
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx

    call textModeNewLine

    mov bx, vga_drawPoint_debugWord5
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx

    call textModeNewLine
ret

debug3Command:
    ;mov bx, 0449h
    ;mov al, [ds:bx]
    ;call alToScreenHex
    ;call textModeNewLine
    mov bx, vga_drawPoint_debugWord1
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    call alToScreenHex

    call textModeNewLine

    mov bx, vga_drawPoint_debugWord2
    mov al, [ds:bx]
    push bx
    call alToScreenHex
    pop bx
    inc bx
    mov al, [ds:bx]
    call alToScreenHex

    call textModeNewLine
ret
;commands
helpcmd db "help",0,"$"
readcmd db "read",0,"$"
writecmd db "write",0,"$"
clearcmd db "clear",0,"$"
jmpcmd db "jump",0,"$"
callcmd db "call",0,"$"
pciinfocmd db "pciinfo",0,"$"
graphicsTest db "gtest",0,"$"
colorTestcmd db "colortest",0,"$"
debug1cmd dw "debug1",0,$    ;debug commands for whatever you want them to do
debug2cmd dw "debug2",0,$
debug3cmd dw "debug3",0,$

;extra strings reeeeee
pcipresent db "pci present: $"
pcimech db "pci mechanism: $"
mjver db "major version: $"
mnver db "minor version: $"
lastpcibus db "num buses: $"

hexReadValueLow dw 0x0000       ;offset
hexReadValueHigh dw 0x0000      ;segment\

pcibxsave dw 0x0000             ;place in ram for saving the bx and cx registers after running interrupt 1Ah
pcicxsave dw 0x0000
;every time you type a character, it goes into a doubly linked list at the end of whatever segment the program is in. This is where it ends up
commandBufferStart dw 0x0000
commandHistory db 0
hexWriteByte db 0
;there can't be anything under this line or shit will fuck up