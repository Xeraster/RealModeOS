	Page	60,132
;-----------------------------------------------------------------------------
;
; CPUID.ASM
;
;       Copyright (c) 1995-Present  Robert Collins
;
;       You have my permission to copy and distribute this software for
;       non-commercial purposes.  Any commercial use of this software or
;       source code is allowed, so long as the appropriate copyright
;       attributions (to me) are intact, *AND* my email address is properly
;       displayed.
;
;       Basically, give me credit, where credit is due, and show my email
;       address.
;
;-----------------------------------------------------------------------------
;
;       Robert R. Collins               email:  rcollins@x86.org
;
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; CPUID determines the CPU type in the system.
;-----------------------------------------------------------------------------
; This program will return the CPUID of an x86 processor.  Virtually no
; attempt is made to distinguish between different processor vendors.  If the
; processor supports the CPUID instruction, then the vendor ID will be
; printed.  If the processor doesn't support CPUID, then you're out of luck.
; In this case, feel free to use this source code as your testbed to include
; such a test.
;-----------------------------------------------------------------------------
;
; *** NOTE ***
;
;	The Intel386 processors use a different format for the processor
;	signature than their Intel486 or Pentium contemporaries.  Neither does
;	the Intel386 processor support the CPUID instruction, hence the
;	processor signature is only available upon RESET.  Therefore, the
;	earlier processors must have their processor signature "massaged"
;	before being returned by this program.
;
;	Maybe someday, if I have some time, I'll include a test which tries
;	to grab the processor signature upon RESET from the Intel386 and
;	early Intel486 processors which didn't support the CPUID instruction.
;
;-----------------------------------------------------------------------------
; Input:   None
; Output:  EDX = CPUID (if you already rely on CPUID being in EDX)
;	   EAX = CPUID (same as DX, used by high-level language calls)
;	   ERRORLEVEL = CPUID family (i.e. 5=Pentium, 4=Intel486...)
;
;	   CPUID defined as follows (as per Intel specifications)
;
;		 3   ...  1 11 1100 0000 0000
;		 1   ...  4 32 1098 7654 3210  Bit position
;		+----------+--+----+----+----+
;		| Reserved |  |    |	|    |
;		+----------+--+----+----+----+
;			    ^^ ^^^^ ^^^^ ^^^^
;			    || |||| |||| ||||
; Processor Type -----------++ |||| |||| ||||
; Family ----------------------++++ |||| ||||
; Model ----------------------------++++ ||||
; Stepping ------------------------------++++
;
;
; Processor Type :	    00 = Original OEM processor
;			    01 = Overdrive processor
;			    10 = Dual processor
;			    11 = Not used (by Intel)
;
; Family:		  0000 = 8086/8088
;			  0001 = 80186/80188
;			  0010 = 80286
;			  0011 = Intel386
;			  0100 = Intel486
;			  0101 = Pentium
;			  0110 = P6
;
; Model (family = 0101):  0001 = Pentium (510\66, 567\66)
;			  0010 = Pentium P54C (735\90, 815\100)
;			  0011 = Pentium overdrive processors
;			  0101 = Pentium overdrive for IntelDX4
;			  1111 = Unknown
;
; Model (family = 0100):  0000 = Intel486 DX
;			  0001 = Intel486 DX
;			  0010 = Intel486 SX
;			  0011 = Intel487 (SX), or Intel486 DX
;			  0100 = Intel486 SL
;			  0101 = IntelSX2
;			  0111 = IntelDX2 write-back enhanced
;			  1000 = IntelDX4 and IntelDX4 overdrive
;			  1111 = Unknown
;
; Model (family = 0011):  0000 = Intel386 DX
;			  0010 = Intel386 SX, Intel386 CX, Intel386 EX
;			  0100 = Intel386 SL
;			  1111 = Unknown
;
;-----------------------------------------------------------------------------
;
; CPUID values for the following processors which don't support the CPUID
; instruction:
;
;	00FFh = 8086/8088 (model and stepping unknown)
;	10FFh = V20/V30 (model and stepping unknown)
;	01FFh = 80186/80188
;	020Fh = 80286
;	030Fh = 80386 DX
;	032Fh = 80386 SX
;	033Fh = 80376
;	034Fh = 80386 SL
;	040Fh = 80486 DX
;	042Fh = 80486 SX
;	043Fh = 80487 SX
;
;-----------------------------------------------------------------------------
; Register(s) modified:  AX, BX, CX, EDX
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Compiler directives
;-----------------------------------------------------------------------------
	.model	small
	.radix	16
	.586p


;-----------------------------------------------------------------------------
; PUBLIC statements here
;-----------------------------------------------------------------------------
	Public	_CPUID_,	Signature,	OBJ_CPUID


;-----------------------------------------------------------------------------
; External declarations
;-----------------------------------------------------------------------------
       Extern  _CheckShutdownCPUID_:   Near    ; Defined in SHUTDOWN.ASM
       Extern  _ShutdownCPUID_ :       Near    ; Defined in SHUTDOWN.ASM


.data
;-----------------------------------------------------------------------------
; Local variable definitions
;-----------------------------------------------------------------------------
	Signature	dw	?
	INT6	equ	[bp-4]
	FCW	equ	[bp-6]		; Floating point Control Word


;-----------------------------------------------------------------------------
; Interrupt vector segment
;-----------------------------------------------------------------------------
ABS0	segment at 0
	org 6*4
	Orig_INT6	dd	?
ABS0	ends


	Align
	.code
	ASSUME	CS:_TEXT, DS:_DATA, ES:_DATA, SS:NOTHING
	OBJ_CPUID	label	word
    ifdef	RRC
	org	000CAh
    endif

;-----------------------------------------------------------------------------
  _CPUID_	proc	near
;-----------------------------------------------------------------------------
; Determine the CPU type by testing for differences in the CPU in the system.
;-----------------------------------------------------------------------------
; To determine if we are a 8086/8088, or 80186/80188, test the value of SP
; after it is placed on the stack.  The algorithm for "PUSH SP" differs from
; 8086/80186 to 80286+.  The algorithm difference is as follows:
;
; 8086/80186		80286+
; {			{
;   SP	  = SP - 2	  TEMP	= SP
;   SS:SP = SP		  SP	= SP - 2
; }			  SS:SP = TEMP
;			}
;
; Thus for the 8086/80186, the value of SP that gets pushed on the stack is
; the value after SP is decremented.  Hence, the value on the stack does not
; reflect the value of SP before the "PUSH" instruction.  Therefore, all we
; have to do to categorize the CPU as 8086/8088 or 80186/80188 is to
; "PUSH SP" and compare the value on the stack image to the value in SP.
;-----------------------------------------------------------------------------
	mov	ax,00FFh		; clear CPU type return register
	push	sp			; save SP on stack to look at
	pop	bx			; get SP saved on stack
	cmp	bx,sp			; if 8086/8088, these values will differ
	jz	short @Not_8086 	; nope, must be other CPU type

;-----------------------------------------------------------------------------
; If this test passes, then we need some other means to differentiate between
; 8088/8088 and 80186/80188.  This method I will use comes from "80186/188,
; 80C186/C188 Hardware Reference Manual" from Intel, PN# 270788, page
; A-2:	"When a word write is performed at offset FFFFh in a segment, the
; 8086 will write one byte at offset FFFFh, and the other at offset 0, while
; an 80186 family processor will write one byte at offset FFFFh, and the
; other at offset 10000h (one byte beyond the end of the segment).
;-----------------------------------------------------------------------------
; Before we can blast a value out to FFFFh, we must save anything there, so
; we don't crash anybody else's data.
;-----------------------------------------------------------------------------
	mov	bx,ds:[0ffffh]		; get original data
	mov	word ptr ds:[0ffffh],0aaaah	; write signature at test location
	cmp	byte ptr ds:[0],0aah	; 8086?
	mov	ds:[0ffffh],bx
	je	short @Test_V20
	inc	ah
	jmp	short CPU_8086_Exit

;-----------------------------------------------------------------------------
; The V20 and 8086 differ with respect to the handling of FLAGS during a MUL
; instruction.	The 8086 always sets ZF=0 (NZ).  The V20 sets ZF according to
; the result.
;-----------------------------------------------------------------------------
@Test_V20:
	push	ax			; save results
	xor	al,al			; force ZF
	mov	al,40h			; multiplicand
	mul	al			; V20 doesn't affect ZF
	pop	ax			; restore results
	jnz	short CPU_8086_Exit	; Intel?
	or	ah,10h			; Set V20 flag
	jmp	short CPU_8086_Exit	; go split


;-----------------------------------------------------------------------------
; When we get here, we know that we aren't a 8086/80186.  And since all
; subsequent processors will trap invalid opcodes via INT6, we will determine
; which CPU we are by trapping an invalid opcode.
;   We are an 80486 if:  XADD	DL,DL	executes correctly
;	      80386 if:  MOV	EDX,EDX executes correctly
;	      80286 if:  SMSW	DX	executes correctly
;-----------------------------------------------------------------------------
; Setup INT6 handler
;-----------------------------------------------------------------------------
@Not_8086:
	enter	6,0			; create stack frame
	mov	word ptr INT6,offset INT6_handler
	mov	INT6[2],cs
	call	set_INT6_vector 	; set pointer to our INT6 handler
	mov	ax,40Fh 		; initialize CPU flag=4 (80486)
	xor	cx,cx			; initialize semaphore

;-----------------------------------------------------------------------------
; Now, try and determine which CPU we are by executing invalid opcodes.
; The instructions I chose to invoke invalid opcodes, are themselves rather
; benign.  In each case, the chosen instruction modifies the DX register,
; and nothing else.  No system parameters are changed, e.g. protected mode,
; or other CPU dependant features.
;-----------------------------------------------------------------------------
; The 80486 instruction 'XADD' xchanges the registers, then adds them.
;-----------------------------------------------------------------------------
	xadd	dl,dl			; 80486
	jcxz	CPU_486
	dec	ah			; set 80386 semaphore
	inc	cx			; CX=0

;-----------------------------------------------------------------------------
; For a description on the effects of the following instructions, look in
; the Intel Programmers Reference Manual's for the 80186, 80286, or 80386.
;-----------------------------------------------------------------------------
	mov	edx,edx 		; 80386
	jcxz	CPU_386
	dec	ah			; set 80286 semaphore
	inc	cx			; CX=0

	smsw	dx			; 80286
	jcxz	CPU_exit
	mov	ax,-1			; set UNKNOWN_CPU semaphore

CPU_exit:
	mov	dx,ax			; make a copy
	call	set_INT6_vector
	leave

CPU_8086_Exit:
	ret

@Found486:
	jmp	CPU_486

;-----------------------------------------------------------------------------
; Determine the difference between the 80386-DX and 80386-SX.
; The 80386-SX differs with the 386 in the CR0 register.  Bit4 of CR0 is
; pulled high in the SX, but is latchable in the DX.  This test attempts to
; clear bit4.  If bit4 is still set, then we are an 80386-SX.
;-----------------------------------------------------------------------------
CPU_386:
	mov	Signature,ax		; save CPUID signature
	call	_CheckShutdownCPUID_	; check to see if we'll be successful
					;  at getting the CPUID via shutdown
	jc	@F			; won't work, don't try
	call	_ShutdownCPUID_ 	; try and get CPUID signature
	jc	@F			; didn't work...keep trying.
	ror	ah,4			; swap nibble locations
	shr	ax,4			; convert to standard CPUID format
	jmp	CPU_exit		; successful, go split

;-----------------------------------------------------------------------------
; When the reset method fails, we'll try and figure it out by "hand."
;-----------------------------------------------------------------------------
; The Intel386 DX CR0.bit4 is reserved, and can be set or cleared by
; software.  All other Intel386 processors have this bit hard-wired to 1.
; If we can set this bit, then we're an Intel386 DX, otherwise, we have to
; keep testing to see whether or not we are an SX variant (80376, Intel386 SL).
;-----------------------------------------------------------------------------
@@:	mov	ax,Signature		; restore the CPUID signature
	mov	edx,cr0 		; Get CR0
	push	edx			; save CR0
	and	dl,not 10h		; clear bit4
	mov	cr0,edx 		; set CR0
	mov	edx,cr0 		; and read CR0
	and	dl,10h			; bit4 forced high?
	pop	edx			; restore reg w/ CR0
	mov	cr0,edx 		; restore CR0
	jz	short CPU_exit		; nope, must be 80386-DX.

;-----------------------------------------------------------------------------
; If we get here, we're at least an 80386 SX variant.
;-----------------------------------------------------------------------------
; Now we'll check for an 80376 processor.  The 80376 is an Intel386 SX that
; is always in protected mode.	Hence, CR0.PE always equals 1.
;-----------------------------------------------------------------------------
	add	al,20h			; set 80386-SX flag.

ifdef i376
	push	eax			; save CPUID
	push	edx			; save CR0 value
	and	dl,not 11h		; try and clear PE
	mov	cr0,edx 		; store in CR0
	mov	eax,cr0 		; get CR0
	xor	edx,eax 		; are we left w/ 11h?  (80376)
	cmp	dl,11h			; 11h will indicate 80376 CPU
	pop	edx			; restore CR0 value
	pop	eax			; restore CPUID
	mov	cr0,edx 		; now restore CR0 to original value
	jne	short @Try386SL 	; must have been 80386 SX
	add	al,10h			; indicate 80376
	jmp	short CPU_exit		; go split
endif

;-----------------------------------------------------------------------------
; There are two ways to check for the Intel386 SL processor.  The first
; method enters the processor into protected mode.  The second method attempts
; to read the stepping information directly from the chip itself.  The
; latter method sounds a whole lot easier, so that's what we'll try here.
; But, just in case it doesn't work, I had better explain the other method
; of detecting the Intel386 SL using protected mode.
;
; The Intel386 SL only has (logical) 25 address lines.	This is an odd-ball
; amount of address lines, but it makes detecting this part, a snap.  Go
; into protected mode, and set a 4G limit.  First and store the dword at
; physical address 0.  Write to address 1000000h a value of 55aa55aah.	Do
; a near jump (JMP $+2) to flush the prefetch queue to guarantee that any
; capacitance on the data bus won't be 55aa55aa from our last memory write.
; Read the value at physical address 0.  If the value you wrote to 1000000h
; was read back at 0, then chances are you're *NOT* an Intel386 SL, but an
; Intel386 SX.	Try this test again, by writing 0aa55aa55 to physical
; address 2000000h.  Read back at physical address 0.  If you read back
; 0aa55aa55 at physical address 0, there's a near 100% chance you're an
; Intel386 SL.
;-----------------------------------------------------------------------------
; The other (easier) method of determining the processor stepping, is to read
; it from the on-board Signature register in the Intel386 SL.
;
; The 80386 SL has a register which allows reading the CPUID.  This
; register is called the signature register, and lies in the On-board
; Memory Control Unit (OMCU) at register 0x30E.  To read the signature
; register, first we must unlock access to the OMCU, read the signature,
; and relock access.
;-----------------------------------------------------------------------------
; To unlock access to the CPUPWRMODE register, we need to execute the
; following code sequence:
;	write 00h to port(23h)
;	write 80h to port(22h)
;	write 0080h to port(22h)	; word write
;-----------------------------------------------------------------------------
@Try386SL:
	call	Check_x86SL		; check for SL processor
	jmp	CPU_exit		; go split

;-----------------------------------------------------------------------------
; Determine whether or not the processor supports the CPUID instruction.
; If it does, then we can use it, and split.
;-----------------------------------------------------------------------------
CPU_486:mov	Signature,ax		; save CPUID signature
	pushfd				; save eflags
	pop	eax			; save eflags
	mov	edx,eax 		; make a copy
	xor	eax,200000h		; toggle the bit
	push	eax			; save it
	popfd				; check if CPUID flag could toggle
	pushfd				; save it, so we can check it
	pop	eax			; get it
	xor	eax,edx 		; did bit toggle?
	jz	@F			; can't use CPUID instruction
	push	edx
	popfd				; restore original EFLAGS value
	mov	eax,1			; set CPUID operand
	cpuid
	jmp	CPU_exit		; go split

;-----------------------------------------------------------------------------
; If we get here, then this processor doesn't support the CPUID instruction.
; So we have to perform the brute-force approach to detecting the CPUID.
;-----------------------------------------------------------------------------
; First, let's try and get the CPUID by the shutdown method.  If that's
; unsuccessful, let's try and iterate through this until we figure out which
; processor we are.
;-----------------------------------------------------------------------------
@@:	call	_CheckShutdownCPUID_	; check to see if we'll be successful
					;  at getting the CPUID via shutdown
	jc	@F			; won't work, don't try
	call	_ShutdownCPUID_ 	; try and get CPUID signature
	jnc	CPU_exit		; successful, go split

;-----------------------------------------------------------------------------
; Determine the difference between the 80486DX, 80486SX, and 80487SX.
; The '486SX and '487SX differ with the DX in the CR0 register.  Bit4 of CR0
; is pulled high in the SX, but is latchable in the DX.  This test attempts to
; clear bit4.  If bit4 is still set, then we are a '486SX or '487SX.
;-----------------------------------------------------------------------------
@@:	mov	ax,Signature		; restore signature
	mov	edx,cr0 		; Get CR0
	and	dl,not 10h		; clear bit4
	mov	cr0,edx 		; set CR0
	mov	edx,cr0 		; and read CR0
	and	dl,10h			; bit4 forced high?
	jz	CPU_exit		; nope, must be 80486-DX.
	add	al,20h			; set 80486-SX flag.

;-----------------------------------------------------------------------------
; Now that we have determined to be a '48x SX, we need to detect the
; difference between a '486SX and '487SX.  This is done by sensing the
; presence of the Numeric Processor eXtension (NPX).  The algorithm comes
; from the Intel 80387 programmers reference manual section 6.2.3.
;-----------------------------------------------------------------------------
	mov	word ptr FCW,5a5ah	; initialize to non-zero value
	fninit				; must use non wait form
	fnstsw	FCW			; store status
	cmp	byte ptr FCW,0		; was the correct status w/ 0's read?
	jne	CPU_exit		; nope
	fnstcw	FCW			; save control word
	mov	bx,FCW			; get control word into AX
	and	bx,103fh		; mask proper status bits
	cmp	bx,3fh			; NPX installed?
	jne	CPU_exit		; nope
	add	al,10h			; set '487SX flag
	jmp	CPU_exit		; go split

;-----------------------------------------------------------------------------
; Set the INT6 vector by exchanging it with the one currently on the stack.
;-----------------------------------------------------------------------------
set_INT6_vector:
	push	dx
	push	ds
	push	ABS0			; save interrupt vector segment
	pop	ds			; make DS=INT vector segment

ASSUME	DS:ABS0
	mov	dx,word ptr ds:Orig_INT6;	; get offset if INT6 handler
	xchg	INT6,dx 		; set new INT6 offset
	mov	word ptr ds:Orig_INT6,dx
	mov	dx,word ptr ds:Orig_INT6[2]	; get segment of INT6 handler
	xchg	INT6[2],dx		; set new INT6 segment
	mov	word ptr ds:Orig_INT6[2],dx
	pop	ds			; restore segment register
	pop	dx
	ret				; split
ASSUME	DS:NOTHING


;-----------------------------------------------------------------------------
  Check_x86SL	proc	near		; Check for Intel386SL, or Intel486SL
;					; processors
;-----------------------------------------------------------------------------
; The Intel386 SL and Intel486 SL have a register which allows reading the
; CPUID.  This register is called the signature register, and lies in the On-
; board Memory Control Unit (OMCU) at register 0x30E.  To read the signature
; register, first we must unlock access to the OMCU, read the signature,
; and relock access.
;-----------------------------------------------------------------------------
; To unlock access to the CPUPWRMODE register, we need to execute the
; following code sequence:
;	write 00h to port(23h)
;	write 80h to port(22h)
;	write 0080h to port(22h)	; word write
;-----------------------------------------------------------------------------
	pushf
	push	ax			; save current CPUID
	cli
	in	ax,22h			; get CPUPWRMODE register
	xor	ax,0ffffh		; all bits set?
	jz	@Not386SL		; yes, go split
	in	ax,22h			; get CPUPWRMODE register
	test	al,1			; CPUPWRMODE unlocked?
	jz	@EnaPWRMODE		; nope, don't try and lock it.

;-----------------------------------------------------------------------------
; The safest way to determine whether or not this is a 386 SL is to attempt
; to lock and unlock the CPUPWRMODE register.  If the register can be locked
; and unlocked as per 386 SL specifications, then there's a good chance that
; this isn't some chipset that amazingly supports the same enable/disable
; protocol.  So if we can enable and disable the CPUPWRMODE register, then
; we'll proceed with reading the CPUID signature register.
;-----------------------------------------------------------------------------
; Lock the CPUPWRMODE register
;-----------------------------------------------------------------------------
	mov	al,00			;
	out	23h,al			;
	mov	ax,180h 		; will lock CPUPWRMODE register
	out	22h,al
	out	22h,ax			; now CPUPWRMODE register should be
					;  locked.
	in	ax,22h			; get CPUPWRMODE register
	test	al,1			; CPUPWRMODE unlocked?
	jnz	@Not386SL		; yes, go try and unlock it

;-----------------------------------------------------------------------------
; Unlock the CPUPWRMODE register
;-----------------------------------------------------------------------------
@EnaPWRMODE:
	mov	al,00			;
	out	23h,al			;
	mov	ax,80h			; will unlock CPUPWRMODE register
	out	22h,al
	out	22h,ax			; now CPUPWRMODE register should be
					;  unlocked.
	in	ax,22h			; get CPUPWRMODE register
	test	al,1			; CPUPWRMODE unlocked?
	jz	@Not386SL		; yes, go try and unlock it

;-----------------------------------------------------------------------------
; Enable the On-board Memory Configuration Unit (OMCU).  If this is an
; Intel486 SL, then bits [4-2] are the unit configuration select bits.	If
; this is an Intel386 SL, then bits [3-2] are the unit configuration select
; bits.
;-----------------------------------------------------------------------------
	and	al,not 1100y		; clear configuration unit bits
	mov	dx,word ptr ss:[esp]	; get current CPUID
	and	dh,0fh			; only keep processor family bits
	cmp	dh,4			; 486 SL?
	jne	@F			; nope
	and	al,not 11100y		; clear configuration unit bits
@@:	or	al,10y			; set unit enable bit
	out	22h,ax			; now unit should be enabled

;-----------------------------------------------------------------------------
; Now read the CPUID signature register
;-----------------------------------------------------------------------------
	mov	dx,030Eh		; signature register
	in	ax,dx			; get CPUID signature
	mov	dx,ax			; make a copy

;-----------------------------------------------------------------------------
; Now as one final test, let's relock the CPUPWRMODE register and make sure
; it really gets locked.  Otherwise, we'll ignore the value we just read.
;-----------------------------------------------------------------------------
	mov	al,00			;
	out	23h,al			;
	mov	ax,180h 		; will lock CPUPWRMODE register
	out	22h,al
	out	22h,ax			; now CPUPWRMODE register should be
					;  locked.
	in	ax,22h			; get CPUPWRMODE register
	test	al,1			; CPUPWRMODE unlocked?
	jnz	@Not386SL		; yes, go try and unlock it

;-----------------------------------------------------------------------------
; OK, must be 80386 SL, and we have CPUID in DX.
;-----------------------------------------------------------------------------
	mov	ax,dx			; restore copy of CPUID
	ror	ah,4			; swap nibble locations
	shr	ax,4			; convert to standard CPUID format
	add	sp,2			; ignore value on stack
	push	ax

@Not386SL:
	pop	ax			; restore current CPUID
	popf
	ret				; split
Check_x86SL	endp


;-----------------------------------------------------------------------------
; INT6 handler sets a semaphore (CX=FFFF) and adjusts the return address to
; point past the invalid opcode.
;-----------------------------------------------------------------------------
INT6_handler:
	enter	0,0			; create new stack frame
	dec	cx			; make CX=FFFF
	add	word ptr ss:[bp][2],3	; point past invalid opcode
	leave
	iret

_CPUID_ 	endp


	.stack	800h

	end

