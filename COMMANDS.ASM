;modifies ax, bx and cx
;makes bx contain address of vram when a segment register is set to B800
cursorPosToBx:
		;calculate the correct line number from cursorY
        mov al, [ds:cursorY]            ;changed from [cursorY]
        mov ah, 0
        mov bx, 00A0h
        mul bx
        mov cx, ax

        ;add the column number in cursorX to di so that the character prints to the correct column
        mov al, [ds:cursorX]            ;changed from [cursorX]
        mov ah, 0
        mov bx, 2
        mul bx
        add cx, ax

        mov bx, cx
ret

;the commands are fetched from the textmode vram. This is fine on vga text modes, can be hacked to work on v9958 but it won't work on an fpga framebuffer card that only has graphics modes
lineToCommandBuffer:

    mov bx, videoCardType
    mov al, [ds:bx]
    cmp al, 0                       ;if card type is 00, its vga. use vga drivers
    je lineToCommandBufferContinue_Vga
    cmp al, 1                       ;if card type is 01, its a v9958. use v9958 drivers. Sucks to be you.
    je lineToCommandBufferContinue_V9958
    cmp al, 2                       ;if card type is 10, its a ice 40 fpga
    je lineToCommandBufferContinue_Ice40
    cmp al, 3                       ;if card type is 11, its vga. use vga drivers
    jmp lineToCommandBufferContinue_unknown

    lineToCommandBufferContinue_Vga:
            call vgaVramToCommandBuffer
            ret
    lineToCommandBufferContinue_V9958:
            call VdpVramToCommandBuffer
            ;call VdpBackspace
            ;call RowsColumnsToCursorPos     ;keep the cursor up to date. can't remember if that's required here or not
            ret
    lineToCommandBufferContinue_Ice40:
            hlt                             ;nope
    lineToCommandBufferContinue_unknown:
            call vgaVramToCommandBuffer             ;meh, it's probably a vga
            ret

    ;there. done
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

    ;ata test command
    mov ax, commandHistory
    mov bx, atatestcmd
    call doesStringContain
    cmp cl, 1
    je ataTestCommand

    ;"stupidtest" command. used for misc quick tests
    mov ax, commandHistory
    mov bx, stupidtestcmd
    call doesStringContain
    cmp cl, 1
    je stupidTestCommand

    ;getscancodeset command
    mov ax, commandHistory
    mov bx, getScancodeSetcmd
    call doesStringContain
    cmp cl, 1
    je getscancodeset

    ;g4test command
    mov ax, commandHistory
    mov bx, G4TestCommand
    call doesStringContain
    cmp cl, 1
    je g4TestCommand

    ;if it go to this point, the user didn't type a real command. Print the error message
    mov bx, invalidcommandmsg
    mov cl, 00001111b
    call printString
    call textModeNewLine

ret

helpCommand:
    ;guess what it's time to change the help command to something actually useful
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

ret

readcmdinfo db "proper usage of this command is: 'read xxxx:xxxx' where x is a hexadecimal number between 0-F",0
readCommandSpacesPerRow equ 0x10        ;max number of spaces per row
remainingLinesToPrint db 0
readCommandLinesToPrint equ 16
;the all important hex reader command
readCommand:
    ;disable the v9958 cursor because it's slower than slow
    mov al, 1
    mov [es:RowsColumnsToCursorPosDisable], al

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

    ;enable the v9958 cursor again
    mov al, 0
    mov [es:RowsColumnsToCursorPosDisable], al
    call calculateCursorValues                      ;recalculate cursor here in case it was running on v9958. on vga this will be so fast it doesn't matter and break nothing
    ret

    ;error. remind user how to use this command
    readCommandInvalidUsage:
    mov bx, invalidparametersmsg
    mov cl, 00001111b
    call printString
    call textModeNewLine

    ;enable the v9958 cursor again
    mov al, 0
    mov [es:RowsColumnsToCursorPosDisable], al

    mov bx, readcmdinfo
    mov cl, 00001111b
    call printString
    call textModeNewLine


ret

writeCommandErrorString db "Hey asshole, that isn't how you use the write command. The syntax is 'write xxxx:xxxx xx'",0
writeCommandErrorStringAlmost db "Close, but you forgot to enter a byte to write. Try 'write 0000:8A00 AA'",0
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
    call calculateCursorValues

ret

jmpValueLow dw 0x0000       ;offset
jmpValueHigh dw 0x0000      ;segment
jmpCommandError db "Guess what dumbass? You typed it wrong. Im not gonna give you any useful help though so get REKT.",0
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

ataTestCommand:
    mov cl, 0x0F
    mov bx, ataCommandInit
    call printString
    call textModeNewLine

    pushad
    push es
    push di
        mov ax, 0x1000
        mov es, ax
        mov eax, 1
        ;;mov cl, 3 ;reading more than 1 sector is broken and fixing it has proven to be too hard
        mov di, 0
        call ata_lba_read
    pop di
    pop es
    popad

    ;there. the first sector of the disk is in memory starting at location 0x10000 probably
    ;print all the partition entries
    
    ;push ds
    mov cl, 16
    mov ax, 0x1000
    mov gs, ax
    mov bx, 0x1CD
    ;mov bx, 0x0F
    ataTestCommand_loop1:
        ;read 16 bytes
        ;damn I remember why I hate x86 assembly now
        push ax
        push cx
            mov al, [gs:bx]
            call alToScreenHex
            ;mov al, ' '
            ;call printChar
        pop cx
        pop ax
        dec cl
        dec bx
        cmp cl, 0
        jne ataTestCommand_loop1
    ;pop ds

    ;mov al, 4
    ;mov cl, 0x0F
    ;call alToScreenHex

ret

stupidTestCommand:
    ;LOL THERE ISNT A WAY TO PRINT INTEGERS. FFFFFFFFFUUUUUUUUUUUUUU...
    ;there is at least a way to print values as hex. close enough
    mov bx, 0
    stupidContinue:
    inc bx
    cmp bx, 0xFFFF
    jne stupidContinue

    push bx
    call textModeNewLine
    pop bx

    ;print wtf is in bx
    push bx
        xchg bl, bh
        mov al, bl
        call alToScreenHex
    pop bx
    mov al, bl
    call alToScreenHex

    call textModeNewLine

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

    ;wow does this function work? Everything I ever make has an "alToScreenHex" subroutine so it's easy to lose track
    call alToScreenHex
ret

;set it to a different scancode, 1 2 or 3. use al
setscancodeset:
    mov al, 3
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

g4TestErrorString db "This command only works with v9958 graphics.",0
g4TestCommand:
    ;if this value is anything other than 1, there isn't a v9958 connected to the system
    mov bx, videoCardType
    mov al, [ds:bx]
    cmp al, 1
    jne g4TestCommandGTFO       ;only do this is v9958
    g4TestCommandContinue:
        ;do stuff
        call setupG4Mode
        call v9958BasicG4Test
    ret
    g4TestCommandGTFO:
        mov bx, g4TestErrorString
        mov cl, 00001111b
        call printString
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
atatestcmd db "atatest",0,"$"
stupidtestcmd db "stupidtest",0,"$"
getScancodeSetcmd db "getscancodeset",0,"$"                 ;whatever the default one qemu uses is what I want my homebrew 486 computer to use
G4TestCommand db "g4test",0                                 ;test graphics 4 mode when plugged in to a v9958

;extra strings reeeeee
pcipresent db "pci present: ",0
pcimech db "pci mechanism: ",0
mjver db "major version: ",0
mnver db "minor version: ",0
lastpcibus db "num buses: ",0
ataCommandInit db "Welcome to the ata test command",0

hexReadValueLow dw 0x0000       ;offset
hexReadValueHigh dw 0x0000      ;segment\

pcibxsave dw 0x0000             ;place in ram for saving the bx and cx registers after running interrupt 1Ah
pcicxsave dw 0x0000
;every time you type a character, it goes into a doubly linked list at the end of whatever segment the program is in. This is where it ends up
commandBufferStart dw 0x0000
commandHistory db 0
hexWriteByte db 0
;there can't be anything under this line or shit will fuck up