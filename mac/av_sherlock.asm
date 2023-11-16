; sound driver Russell lieblich  udate jan 4 1988
 	include MacTraps.D
	include ToolEqu.D
	include Sysequ.d

	XREF	bsize_a1,buff_a1
	XDEF	makesound
	
makesound
	MOVEM.L	D0-D2/A0-A6,-(SP)	; preserve registers
	
	LEA		parmBlock(A5),A1 ; A5 has pointer to appl. globals
	MOVE.W		#-4,24(A1)	; sound driver reference #$fffc
	CMP.W		#3,D0		; third sound?
	BEQ.S		off_sound	; yes, go to third sound
	and		#1,d0		;0=oneshot;  1=looping
	Bne		looping	; yes, go to fifth sound

one_shot
	MOVE.L		#0,12(A1)	; completion: asynchronous
        LEA	        buff_a1,A0	; buffer address of second sound
	MOVE.L		A0,32(A1)	; pointer to the buffer
	lea		bsize_a1,a0
	MOVE.L		0(a0),36(A1) ; size of buffer, for second sound
	BRA		final_call	; make the sound
;loopin horse = odd numbered sound
looping
	MOVE.W		#0,soundFlag(A5) ; turn on multiple play
	lea		complete3,a0
	MOVE.L		a0,12(A1)	; first sound - one shot
	lea		buff_a1,a0
	MOVE.L		A0,32(A1)
	move.l	        bsize_a1,36(a1)
;	MOVE.L		A3,36(A1)
	BRA 		final_call
off_sound
	move.w		#1,soundFlag(a5)
	bra.s		exitAll

final_call
	MOVE.L		A1,A0		; parameter block address
	_Write	,ASYNC			; same as _StartSound
exitAll	
	MOVEM.L		(SP)+,D0-D2/A0-A6	; recover register A2
	RTS


complete3
	MOVEM.L		D0-D2/A0-A6,-(SP)
	MOVE.L		currentA5,A5
	lea		parmblock(a5),a1
	MOVE.W		#-4,24(A1)	; sound driver reference #$fffc
	move		soundFlag(a5),d0
	bne		donefor
	bra		looping


doneFor
	LEA		parmBlock(A5),A0 ; A5 has pointer to appl. globals
	move.l		#0,12(a0)
	move.w		#-4,24(a0)
	_killIO
	BRA.S		exitAll
	
	
soundFlag	DS.W	1	
parmBlock	DS.B	50
buffHold	DS.L	1
