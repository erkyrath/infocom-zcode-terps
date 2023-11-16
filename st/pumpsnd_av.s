;	This File:	PUMPSND.S
;	Author:		Glyn H. Anderson
;	Created:	10-Nov-87  12:53
;	Last Edit:	23-Nov-87  14:13

BIOS		= 1		;BIOS trap number
GISelect	= $FFFF8800	;GI sound chip register select
GIRead		= GISelect	;GI sound chip register read
GIWrite		= GISelect+2	;GI sound chip register write

 .text

sample	set 8			;address of sound samples
oldSSP	set -4			;supervisor stack pointer saver
oldSR	set -6			;status register saver
waste	set -8			;tens of cycles to waste
repeat	set -10
stk_frm	set -10

pump_sound_::
	link	a6,#stk_frm		;create stack frame
	movem.l	d2-d3,-(sp)		;save regs

	move.l	sample(a6),a0		;get pointer to data file...
	move.b	2(a0),repeat(a6)	;get the repeat count
	move.l	#8000000,d0			;clock freq (8 MHz)
	divu	4(a0),d0			;over sample freq
	sub.w	#238-5,d0		;take out loop's length and round
	bpl.s	.1				;branch if we've got time to waste...
	moveq	#0,d0			;else don't waste any
.1:	and.l	#$0000FFFF,d0	;clear upper bits
	divu	#10,d0			;divide by ten
	move.w	d0,waste(a6)	;save waste count

	clr.l	-(sp)			;arg is 0L
	move.w	#$20,-(sp)		;routine no. is $20
	trap	#BIOS			;call Super( 0L )
	addq.l	#6,sp			;fix stack
	move.l	d0,oldSSP(a6)	;save SSP

	move	sr,d0			;get SR
	move.w	d0,oldSR(a6)	;save SR
	or.w	#$0700,d0		;disable interrupts
	move	d0,sr			;	"

	move.b	#$07,GISelect	;disable all tone and noise
	move.b	GIRead,d0		;	"
	or.b	#$3F,d0			;	"
	move.b	d0,GIWrite		;	"

	move.l	sample(a6),a0		;get pointer to data file...
	lea		my_own, a1		; get address of conversion table...

	move.w  8(a0),d3		;setup...length of data...
	adda	#10,a0			;start of data...
	
	bra.s	.pump_next		;start loop...
.pump_loop:
	moveq	#0,d0			;clear high bits
	move.b	(a0)+,d0		;get sample
	eor.b	#$80,d0			;offset the sample
	lsl.w	#1,d0			;index into WORDs
	move.w	0(a1,d0.w),d0	;get 12-bit version
	move.w	d0,d1			;copy it
	move.w	d0,d2			;twice
	lsr.w	#8,d0			;get A
	lsr.b	#4,d1			;get B
	and.b	#$0F,d2			;get C

	move.b	#$08,GISelect
	move.b	d0,GIWrite		;write A
	move.b	#$09,GISelect
	move.b	d1,GIWrite		;write B
	move.b	#$0A,GISelect
	move.b	d2,GIWrite		;write C

	move.w	waste(a6),d0	;get waste count
.waste_lp:
	dbf	d0,.waste_lp		;and waste some time...
.pump_next:
	dbf	d3,.pump_loop		;loop until all samples are output...

	move	oldSR(a6),sr	;restore interrupt level

	move.l	oldSSP(a6),-(sp) ;push old SSP
	move.w	#$20,-(sp)		;function #$20
	trap	#BIOS			;call Super( SSP )
	addq.l	#6,sp			;fix stack

	movem.l	(sp)+,d2-d3		;restore registers
	unlk	a6				;restore stack frame

	rts		;from pump_sound_

; This table maps values from -128 to +127 (or 0 to 255, actually)
; into amplitude inputs for the sound chip.  Each digit is the amplitude
; for one channel; I assume the output is additive.  Each channel is
; `logarithmic':  I assume this means that value 1 is 1/2**14 volts,
; 2 is 1/2**13, and so on.  Thus the first line of the table goes
; 3/2^14, 4/2^14, 6/2^14, 8/2^14, 10/2^14, 12/2^14, 12/2^14 (??), 14/2^14

my_own::
.dc.w	$111, $112, $113, $033, $114, $124, $142, $143
.dc.w	$152, $334, $433, $532, $533, $542, $534, $362
.dc.w	$363, $346, $364, $356, $464, $563, $465, $473
.dc.w	$572, $573, $475, $574, $566, $575, $476, $377
.dc.w	$576, $583, $485, $386, $766, $775, $780, $590
.dc.w	$591, $592, $593, $495, $691, $692, $693, $790
.dc.w	$882, $883, $965, $908, $974, $966, $975, $980
.dc.w	$891, $892, $893, $887, $797, $895, $6A3, $968
.dc.w   $949, $879, $959, $993, $994, $A66, $A75, $A38
.dc.w   $8A1, $8A2, $4B2, $4B3, $3B5, $3A9, $2B6, $3B6
.dc.w   $6B2, $6B3, $6B4, $7B1, $7B2, $7B3, $7B4, $7B5
.dc.w   $7A9, $2AA, $3AA, $92B, $93B, $89A, $87B, $59B
.dc.w   $33C, $34C, $52C, $53C, $61C, $79B, $63C, $46C
.dc.w   $64C, $72C, $73C, $74C, $75C, $5BA, $8B9, $A6B
.dc.w   $9B8, $9AA, $99B, $85C, $6C5, $7C3, $5C7, $3C8
.dc.w   $3BB, $87C, $4BB, $5C8, $5BB, $69C, $6C8, $6BB
.dc.w   $A9B, $9BA, $B9A, $BB2, $BA9, $9C4, $9C5, $A4C
.dc.w   $A5C, $B8B, $C1A, $C2A, $C3A, $C4A, $C5A, $C89
.dc.w   $C98, $C6A, $CA1, $CA3, $CA5, $C99, $CA6, $C1B
.dc.w   $CA7, $D25, $D35, $D53, $D54, $D55, $D63, $D64
.dc.w	$D65, $D72, $D66, $D74, $D75, $D76, $D80, $D77
.dc.w	$D68, $D85, $D86, $D87, $D90, $D92, $D94, $D95
.dc.w	$D96, $D3A, $D4A, $D5A, $D6A, $B3D, $B4D, $B5D
.dc.w	$DA1, $DA3, $DA5, $DA6, $DA7, $D4B, $D5B, $DA8
.dc.w	$D6B, $D7B, $DA9, $D8B, $DB3, $DB5, $DB6, $DB7
.dc.w	$D9B, $DB8, $BAD, $C0D, $C2D, $C4D, $C5D, $D0C
.dc.w	$D2C, $D3C, $D4C, $D5C, $D6C, $DBA, $D7C, $D8C
.dc.w	$C9D, $D9C, $DBB, $CD3, $CD6, $CD7, $DC7, $CD8
.dc.w	$DC8, $ADC, $A1E, $A3E, $A5E, $A6E, $A7E, $CDA
.dc.w	$6DD, $7DD, $8E6, $A9E, $8DD, $CDB, $E78, $9DD
.dc.w	$A7E, $CDA, $DCA, $BCD, $CBD, $5DD, $7E8, $BDC
.dc.w	$D5D, $E48, $D6D, $E76, $D7D, $DCB, $D8D, $9DD

