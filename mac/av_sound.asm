; sound driver Russell lieblich  update jan 4 1988
; modified by Duncan Blanchard

 	include MacTraps.D
	include ToolEqu.D
	include SysEqu.D

	XREF	bsize_a1,buff_a1	; [rename ...]
	XDEF	makesound
	
; globals (pointer in A5 at runtime)
	
loopFlag	DS.W	0
parmBlock	DS.B	50

makesound
	MOVEM.L	D0-D2/A0-A6,-(SP)	; preserve registers
	
	CMP.W		#3,D0		; stop looping?
	BEQ.S		off_sound	; yes
	and.w		#1,d0		; 0=oneshot, 1=looping
	bne.s		looping		; start looping

one_shot
	CLR.W		loopFlag(A5)
	suba.l		a2,a2		; no completion routine
	BRA.S		start

looping
	MOVE.W		#1,loopFlag(A5)
	lea		complete3,a2

; A2 = completion routine, zero if none

start	lea		parmblock(a5),a0
	MOVE.L		a2,12(A0)	; store completion routine

	LEA	        buff_a1,A2
	MOVE.L		A2,32(A0)	; pointer to the buffer
	lea		bsize_a1,a2
	MOVE.L		(a2),36(A0)	; size of buffer

	MOVE.W		#-4,24(A0)	; sound driver reference #$fffc
	_Write	,ASYNC			; same as _StartSound

exitAll	
	MOVEM.L		(SP)+,D0-D2/A0-A6	; recover A5 etc
	RTS

; turn off looping sound (when the current cycle ends ...)

off_sound
	clr.w		loopFlag(a5)
	bra.s		exitAll

; completion routine for asynchronous sound event

complete3
	MOVEM.L		D0-D2/A0-A6,-(SP)	; save old A5 too
	MOVE.L		currentA5,A5
	TST.W		loopFlag(a5)	; keep looping?
	bne.s		looping		; yes

; doneForNow, stop looping
; can we skip the killIO, and just do a final exit?

	LEA		parmBlock(A5),A0
	move.l		#0,12(a0)
	move.w		#-4,24(a0)	; sound driver reference #$fffc
	_killIO
	BRA.S		exitAll

