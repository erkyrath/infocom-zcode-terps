
;	This File:	PUMPSND.S
;	Author:		Glyn H. Anderson;  modified by Duncan Blanchard
;	Created:	10-Nov-87  12:53
;	Last Edit:	23-Nov-87  14:13;  23-Feb-88

; register conventions: in interrupt code, must preserve everything, 
; elsewhere, preserve all except a0/d0

; is there any way to determine system clock speed at runtime?  The "system 
; timer calibration" global, _timr_ms, seems to be defined uselessly.

CLKFRQ	    equ 8000000		;current ST clock freq (8 MHz)
GEMDOS	    equ 1		;trap number
XBIOS	    equ 14
conterm	    equ	$484		;console attribute bits (lowmem global)

GISelect    equ $FFFF8800	;GI sound chip register select
GIRead	    equ GISelect	;GI sound chip register read
GIWrite	    equ GISelect+2	;GI sound chip register write

*-----------------------
* globals
*-----------------------

    bss

i_samp	ds.l	1	;address of sound samples
i_reps	ds.w	1	;repeat count
i_p	ds.l	1	;pointer to next sample
i_p2	ds.l	1	;pointer to last sample (+1)
i_done	ds.w	1	;end-of-sound flag

oldSSP	ds.l	1	;saved supervisor stack pointer
oldSR	ds.w	1	;saved status register
oldct	ds.b	1	;saved keyclick/bell settings

*-----------------------
* pump_sound
*-----------------------

    text
    xdef    pump_sound

pump_sound
	move.l	4(a7),a0		;get pointer to data buffer
	move.w	8+2(a7),d0		;get caller's repeat count
	bsr	ps_init

psndx1	move.w	#$FF,-(sp)
	move.w	#6,-(sp)		;periodically check for a key
	trap	#GEMDOS
	addq.l	#4,sp			;flush stack
	cmpi.b	#$51,d0			;'Q' to abort
	beq.s	psndx3

	move.w	#500,d0			;5000-tick loop
psndx2	dbf	d0,psndx2
	tst.w	i_done			;busy-wait for now ...
	beq	psndx1

psndx3	bsr	ps_cleanup
	rts				;from pump_sound

*-----------------------
* super
*-----------------------

; if d0 is nonzero enter super mode, else exit super mode

super	tst.w	d0
	beq.s	sprx1

	clr.l	-(sp)
	move.w	#$20,-(sp)		;Super is function $20
	trap	#GEMDOS
	addq.l	#6,sp			;flush (system?) stack
	move.l	d0,oldSSP		;save user SP
	bra.s	sprx2

sprx1	move.l	oldSSP,-(sp)		;user SP
	move.w	#$20,-(sp)
	trap	#GEMDOS
	addq.l	#6,sp			;flush (user) stack
sprx2	rts

*-----------------------
* ps_init
*-----------------------

; setup for sound; a0 -> buffer, d0.w = reps
ps_init
	move.l	a0,i_samp		;store
***	clr.w	d0
***	move.b	2(a0),d0		;get data's repeat count (byte)
	move.w	d0,i_reps		;store

; enter Super mode, disable keyclick & bell (anything else?)

	moveq	#1,d0
	bsr	super

	move.b	conterm,d0
	move.b	d0,oldct		;save current settings
	andi.b	#$FA,d0
	move.b	d0,conterm		;disable os sounds

; disable interrupts and atomically adjust Mixer control bits 
; (don't disturb I/O control bits)

	move	sr,d0			;get status reg
	move.w	d0,oldSR
	or.w	#$0700,d0		;disable interrupts
	move	d0,sr

	move.l	#GISelect,a0
	move.b	#$07,(a0)		;request mixer controls
	move.b	(a0),d0			;get controls
	ori.b	#$3F,d0			;disable all noise and tones
	move.b	d0,2(a0)		;set controls

; restore normal interrupts & user mode

	move	oldSR,sr
	clr.w	d0
	bsr	super

; install our sound interrupt vector
; We use 68901 timer A in divide-by-4 mode, which maximizes the playback
; accuracy at high sample rates (within 1 percent at 20 KHz).  It also limits 
; the lowest frequency we can handle to 7.6 KHz.  If lower is needed we
; could easily add a second timer mode.

	bsr	rep_init		;set up interrupt vars
	clr.w	i_done			;reset flag

	move.l	#CLKFRQ,d0		;timer clock freq (same as cpu?)
	divu	4(a0),d0		;over sample freq -> ticks per sample
	lsr.w	#2,d0			;countdown using timer /4 mode
	cmpi.w	#$FF,d0			;too slow?
	ble.s	psix1
	move.w	#$FF,d0			;yes, clip to slowest 

psix1	pea	ps_loop			;interrupt handler
	move.w	d0,-(sp)		;count
	move.w	#$01,-(sp)		;control mode
	clr.w	-(sp)			;register A
	move.w	#$1F,-(sp)		;xbtimer call
	trap	#XBIOS			;  <-- interrupts begin here
	adda.w	#12,sp			;flush stack

	rts

*-----------------------
* ps_cleanup
*-----------------------

ps_cleanup

; un-install our interrupt

	clr.l	-(sp)			;interrupt handler
	clr.w	-(sp)			;count
	move.w	#$00,-(sp)		;control mode
	clr.w	-(sp)			;register A
	move.w	#$1F,-(sp)		;xbtimer call
	trap	#XBIOS			;  <-- interrupts stop here
	adda.w	#12,sp			;flush stack

; restore keyclick and bell settings

	moveq	#1,d0			;must be in Super mode 
	bsr	super
	move.b	oldct,d0		;get saved settings
	move.b	d0,conterm
	clr.w	d0
	bsr	super
	rts

*-----------------------
* rep_init
*-----------------------

rep_init
	move.l	i_samp,a0		;point to header
	addq.l	#8,a0			;point to data length
	moveq	#0,d0
	move.w  (a0)+,d0		;get it
	move.l	a0,i_p			;store pointer to start of data
	add.l	a0,d0
	move.l	d0,i_p2			;store pointer to end of data (+1)
	rts

*-----------------------
* ps_loop
*-----------------------

; interrupt entry point -- output the next sample

; Each pass through the critical section uses 288 ticks, plus interrupt 
; overhead of 44 ticks, so an 18k sample rate (one every 434 ticks at 8 MHz)
; consumes 76 percent of total cpu time.

ps_loop
	movem.l	d0/a0-a1,-(sp)	;[38]		;registers used
	move.l	i_p,a0		;[16] [+4?]	;pointer to next sample
	cmpa.l	i_p2,a0		;[18] [+4?]	;end of cycle?
	beq.s	pslx3		;[ 8] [10 tkn]	;yes			;[[ 80]]

pslx1	move.b	(a0)+,d0	; [8]		;get it
	move.l	a0,i_p		;[16] [+4?]	;update pointer

	lea	volmap(pc),a1	; [8]	;address of conversion table
	ext.w	d0		; [4]	;signed byte
	adda.w	d0,a1		; [8]
	add.w	d0,d0		; [4]
	adda.w	d0,a1		; [8]	;offset x3			;[[ 56]]

; update volume on each channel (as simultaneously as possible)

	move.l	#GISelect,a0	;[12]
	move.b	#8,(a0)		;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write A vol
	move.b	#9,(a0)		;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write B vol
	move.b	#10,(a0)	;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write C vol			;[[ 96]]

pslx2	movem.l	(sp)+,d0/a0-a1	;[36]	;restore regs
	rte			;[20]	;return from exception		;[[ 56]]

; handle end of cycle.  The longest path through this section shouldn't be
; allowed to exceed the length of the critical path above by very much, since
; the timing may be tight.

pslx3	tst.w	i_done			;been here before?
	bne.s	pslx2			;just waiting for user to disable ints

	subq.w	#1,i_reps		;any more reps?
	beq.s	pslx4			;no
	bsr	rep_init		;yes, set up next cycle
	bra.s	pslx2			;but wait until next int to start it

pslx4	move.w	#1,i_done		;finished, raise flag
	bra.s	pslx2

*-----------------------
* volmap
*-----------------------

    text	; keep read-only data in code seg for quick access

; This table maps signed bytes (-128..127) into amplitude inputs for the
; sound chip.  Each digit is the 4-bit amplitude for one channel.
; In the Atari ST the three channels are not weighted equally; the values 
; in the table were determined empirically.

; the table's entry point is in the middle, corresponding to signed '0'

	dc.b	$01,$1,$1,$01,$1,$2,$01,$1,$3,$00,$3,$3   ;4 groups per line
	dc.b	$01,$1,$4,$01,$2,$4,$01,$4,$2,$01,$4,$3
 	dc.b	$01,$5,$2,$03,$3,$4,$04,$3,$3,$05,$3,$2
	dc.b	$05,$3,$3,$05,$4,$2,$05,$3,$4,$03,$6,$2
 	dc.b	$03,$6,$3,$03,$4,$6,$03,$6,$4,$03,$5,$6
	dc.b	$04,$6,$4,$05,$6,$3,$04,$6,$5,$04,$7,$3
 	dc.b	$05,$7,$2,$05,$7,$3,$04,$7,$5,$05,$7,$4
	dc.b	$05,$6,$6,$05,$7,$5,$04,$7,$6,$03,$7,$7
 	dc.b	$05,$7,$6,$05,$8,$3,$04,$8,$5,$03,$8,$6
	dc.b	$07,$6,$6,$07,$7,$5,$07,$8,$0,$05,$9,$0
 	dc.b	$05,$9,$1,$05,$9,$2,$05,$9,$3,$04,$9,$5
	dc.b	$06,$9,$1,$06,$9,$2,$06,$9,$3,$07,$9,$0
 	dc.b	$08,$8,$2,$08,$8,$3,$09,$6,$5,$09,$0,$8
	dc.b	$09,$7,$4,$09,$6,$6,$09,$7,$5,$09,$8,$0
 	dc.b	$08,$9,$1,$08,$9,$2,$08,$9,$3,$08,$8,$7
	dc.b	$07,$9,$7,$08,$9,$5,$06,$A,$3,$09,$6,$8
 	dc.b	$09,$4,$9,$08,$7,$9,$09,$5,$9,$09,$9,$3
	dc.b	$09,$9,$4,$0A,$6,$6,$0A,$7,$5,$0A,$3,$8
 	dc.b	$08,$A,$1,$08,$A,$2,$04,$B,$2,$04,$B,$3
	dc.b	$03,$B,$5,$03,$A,$9,$02,$B,$6,$03,$B,$6
 	dc.b	$06,$B,$2,$06,$B,$3,$06,$B,$4,$07,$B,$1
	dc.b	$07,$B,$2,$07,$B,$3,$07,$B,$4,$07,$B,$5
 	dc.b	$07,$A,$9,$02,$A,$A,$03,$A,$A,$09,$2,$B
	dc.b	$09,$3,$B,$08,$9,$A,$08,$7,$B,$05,$9,$B
 	dc.b	$03,$3,$C,$03,$4,$C,$05,$2,$C,$05,$3,$C
	dc.b	$06,$1,$C,$07,$9,$B,$06,$3,$C,$04,$6,$C
 	dc.b	$06,$4,$C,$07,$2,$C,$07,$3,$C,$07,$4,$C
	dc.b	$07,$5,$C,$05,$B,$A,$08,$B,$9,$0A,$6,$B
 	dc.b	$09,$B,$8,$09,$A,$A,$09,$9,$B,$08,$5,$C
	dc.b	$06,$C,$5,$07,$C,$3,$05,$C,$7,$03,$C,$8
 	dc.b	$03,$B,$B,$08,$7,$C,$04,$B,$B,$05,$C,$8
	dc.b	$05,$B,$B,$06,$9,$C,$06,$C,$8,$06,$B,$B
volmap
 	dc.b	$0A,$9,$B,$09,$B,$A,$0B,$9,$A,$0B,$B,$2
	dc.b	$0B,$A,$9,$09,$C,$4,$09,$C,$5,$0A,$4,$C
 	dc.b	$0A,$5,$C,$0B,$8,$B,$0C,$1,$A,$0C,$2,$A
	dc.b	$0C,$3,$A,$0C,$4,$A,$0C,$5,$A,$0C,$8,$9
 	dc.b	$0C,$9,$8,$0C,$6,$A,$0C,$A,$1,$0C,$A,$3
	dc.b	$0C,$A,$5,$0C,$9,$9,$0C,$A,$6,$0C,$1,$B
 	dc.b	$0C,$A,$7,$0D,$2,$5,$0D,$3,$5,$0D,$5,$3
	dc.b	$0D,$5,$4,$0D,$5,$5,$0D,$6,$3,$0D,$6,$4
 	dc.b	$0D,$6,$5,$0D,$7,$2,$0D,$6,$6,$0D,$7,$4
	dc.b	$0D,$7,$5,$0D,$7,$6,$0D,$8,$0,$0D,$7,$7
 	dc.b	$0D,$6,$8,$0D,$8,$5,$0D,$8,$6,$0D,$8,$7
	dc.b	$0D,$9,$0,$0D,$9,$2,$0D,$9,$4,$0D,$9,$5
 	dc.b	$0D,$9,$6,$0D,$3,$A,$0D,$4,$A,$0D,$5,$A
	dc.b	$0D,$6,$A,$0B,$3,$D,$0B,$4,$D,$0B,$5,$D
 	dc.b	$0D,$A,$1,$0D,$A,$3,$0D,$A,$5,$0D,$A,$6
	dc.b	$0D,$A,$7,$0D,$4,$B,$0D,$5,$B,$0D,$A,$8
 	dc.b	$0D,$6,$B,$0D,$7,$B,$0D,$A,$9,$0D,$8,$B
	dc.b	$0D,$B,$3,$0D,$B,$5,$0D,$B,$6,$0D,$B,$7
 	dc.b	$0D,$9,$B,$0D,$B,$8,$0B,$A,$D,$0C,$0,$D
	dc.b	$0C,$2,$D,$0C,$4,$D,$0C,$5,$D,$0D,$0,$C
 	dc.b	$0D,$2,$C,$0D,$3,$C,$0D,$4,$C,$0D,$5,$C
	dc.b	$0D,$6,$C,$0D,$B,$A,$0D,$7,$C,$0D,$8,$C
 	dc.b	$0C,$9,$D,$0D,$9,$C,$0D,$B,$B,$0C,$D,$3
	dc.b	$0C,$D,$6,$0C,$D,$7,$0D,$C,$7,$0C,$D,$8
 	dc.b	$0D,$C,$8,$0A,$D,$C,$0A,$1,$E,$0A,$3,$E
	dc.b	$0A,$5,$E,$0A,$6,$E,$0A,$7,$E,$0C,$D,$A
 	dc.b	$06,$D,$D,$07,$D,$D,$08,$E,$6,$0A,$9,$E
	dc.b	$08,$D,$D,$0C,$D,$B,$0E,$7,$8,$09,$D,$D
 	dc.b	$0A,$7,$E,$0C,$D,$A,$0D,$C,$A,$0B,$C,$D
	dc.b	$0C,$B,$D,$05,$D,$D,$07,$E,$8,$0B,$D,$C
 	dc.b	$0D,$5,$D,$0E,$4,$8,$0D,$6,$D,$0E,$7,$6
	dc.b	$0D,$7,$D,$0D,$C,$B,$0D,$8,$D,$09,$D,$D

    end
		;of assembly
    text

*-----------------------
* DEAD CODE
*-----------------------

LOOPLEN     equ 220		;was 286, 214, 238-5
WASTELEN    equ 10		;length of waste loop
LLEN2	    equ 106
SYNCCNT	    equ 100		;resync every n samples

; from setup ...
	move.l	#CLKFRQ,d0		;ST clock freq
	divu	4(a0),d0		;over sample freq -> ticks per sample
	sub.w	#LOOPLEN,d0		;take out loop's length
	bpl.s	psix1			;branch if we've got time to waste...
	moveq	#0,d0			;else don't waste any
psix1	and.l	#$0000FFFF,d0		;clear upper bits
	divu	#WASTELEN,d0		;length of waste loop
	move.w	d0,i_waste		;save waste count

	sub.w	#LLEN2/10,d0		;reduced waste after sync section
	bpl.s	psx2
	moveq	#0,d0
psx2	move.w	d0,w2
	clr.w	sync		;resync first time thru

; synchronize with beginning of a new square wave 
; For smoother sq wave (and reduced cpu load), do this only every nth pass

	subq.w	#1,sync		;[16]	;time to resync?
	bgt.s	plx2		;[10]	;no	[8 if not taken]	;[[ 26]]
	move.w	#SYNCCNT,sync	;[16]
	move.w	w2,w_use	;[20]	;adjust waste val

;	move.b	#0,(a2)			;chan A period = longest poss
;	move.b	#$FF,(a3)		;(ignore 8 ls period bits)
	move.b	#1,(a2)		;[12]
	move.b	#$0F,(a3)	;[12]
;	move.b	#2,(a2)			;chan B period = longest poss
;	move.b	#$FF,(a3)
	move.b	#3,(a2)		;[12]
	move.b	#$0F,(a3)	;[12]
;	move.b	#4,(a2)			;chan C period = longest poss
;	move.b	#$FF,(a3)
	move.b	#5,(a2)		;[12]
	move.b	#$0F,(a3)	;[12]			;72+36-2	;[[106]]

; fetch and unpack 3 amplitude values

	clr.w	d0
	move.b	(a0)+,d0	; [8]	;get sample
	eor.b	#$80,d0		; [8]	;offset the sample
	lsl.w	#1,d0		; [8]	;index into WORDs

	move.w	0(a1,d0.w),d0	;[14]	;get 12-bit version
	move.w	d0,d1		; [4]	;copy it
	move.w	d0,d2		; [4]	;twice
	lsr.w	#8,d0		;[22]	;get A
	lsr.b	#4,d1		;[14]	;get B
	and.b	#$0F,d2		; [8]	;get C [66]

;;	move.w	0(a1,d0.w),d2	;[14]	;get 12-bit version
;;	move.w	d2,d1		; [4]
;;	and.b	#$0F,d2		; [8]	;extract C
;;	lsr.b	#4,d1		;[14]	;shift B into position
;;	move.w	d1,d0		; [4]
;;	and.b	#$0F,d1		; [8]	;extract B
;;	lsr.b	#4,d0		;[14]	;shift A into position [66]

; (after pump_next)

	move.b	#$07,GISelect	;done -- disable all tone and noise  [dbb]
	move.b	GIRead,d0
	or.b	#$3F,d0
	move.b	d0,GIWrite

    end

;;; I.  alternate critical loop; assumes modified word-packing in table

ps_loop
	move.w	d0,-(sp)	;[ 8]
	move.l	a0,-(sp)	;[14]
	subq.w	#1,i_n		;[16] [20?]	;reached end of cycle?
	beq.s	pslx3		;[ 8] [10 tk]	;yes			;[[ 46]]

pslx1	move.l	i_p,a0		;[16] [?]	;pointer to next sample
	move.b	(a0)+,d0	; [8]		;get it
	move.l	a0,i_p		;[16] [?]	;update pointer

	lea	volmap(pc),a0	; [8]	;address of conversion table
	ext.w	d0		; [4]	;signed byte
	add.w	d0,d0		; [4]
	move.w	0(a0,d0.w),d0	;[14]					;[[ 70]]

; update volume on each channel (as simultaneously as possible)

	move.l	#GISelect,a0	;[12]
	move.b	#8,(a0)		;[12]
	move.b	d0,2(a0)	;[12] 	;write A vol
	lsr.w	#5,d0		;[16]	;position next nybble, fifth bit zeroed
	move.b	#9,(a0)		;[12]
	move.b	d0,2(a0)	;[12] 	;write B vol
	lsr.w	#5,d0		;[16]
	move.b	#10,(a0)	;[12]
	move.b	d0,2(a0)	;[12] 	;write C vol			;[[116]]

pslx2	move.l	(sp)+,a0	;[12]	;exit
	move.w	(sp)+,d0	;[ 8]
	rts			;[16]		;total 268 (was 282)	;[[ 36]]


;;; II.  modified crit, interrupt vars located directly under voltab

ps_loop
	movem.l	d0/a0-a1,-(sp)	;[38]
	lea	volmap,a1	;[12]	;conversion table & vars
	move.l	-4(a1),a0	;[16]	;pointer to next sample
	cmp.w	-6(a1),a0	;[10]	;reached end of cycle?
	beq.s	pslx3		;[ 8] [10 tk]	;yes			;[[ 84]]

pslx1	clr.w	d0		; [4]	;unsigned byte
	move.b	(a0)+,d0	; [8]	;get it
	move.l	a0,-4(a1)	;[16]	;update pointer

	adda.w	d0,a1		; [8]
	add.w	d0,d0		; [4]
	adda.w	d0,a1		; [8]	;offset x3			;[[ 44]]

; update volume on each channel (as simultaneously as possible)

	move.l	#GISelect,a0	;[12]
	move.b	#8,(a0)		;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write A vol
	move.b	#9,(a0)		;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write B vol
	move.b	#10,(a0)	;[12]
	move.b	(a1)+,2(a0)	;[16] 	;write C vol			;[[ 96]]

pslx2	movem.l	(sp)+,d0/a0-a1	;[36]	;exit
	rts			;[16]		;total 276 (282)	;[[ 52]]


;;; III.  alternate critical loop; assumes modified word-packing in table
;;;       AND interrupt vars located directly under voltab

ps_loop
	move.w	d0,-(sp)	;[ 8]
	move.l	a0,-(sp)	;[14]

	lea	volmap-4,a0	;[12]	;conversion table & vars
	move.l	(a0),d0		;[12]	;pointer to next sample
	cmp.w	-2(a0),d0	;[10]	;reached end of cycle?
	beq.s	pslx3		;[ 8] [10 tk]	;yes			;[[ 84]]

pslx1	move.l	i_p,a0		;[16] [?]	;pointer to next sample
	move.b	(a0)+,d0	; [8]		;get it
	move.l	a0,i_p		;[16] [?]	;update pointer

	lea	volmap(pc),a0	; [8]	;address of conversion table
	ext.w	d0		; [4]	;signed byte
	add.w	d0,d0		; [4]
	move.w	0(a0,d0.w),d0	;[14]					;[[ 70]]

; update volume on each channel (as simultaneously as possible)

	move.l	#GISelect,a0	;[12]
	move.b	#8,(a0)		;[12]
	move.b	d0,2(a0)	;[12] 	;write A vol
	lsr.w	#5,d0		;[16]	;position next nybble, fifth bit zeroed
	move.b	#9,(a0)		;[12]
	move.b	d0,2(a0)	;[12] 	;write B vol
	lsr.w	#5,d0		;[16]
	move.b	#10,(a0)	;[12]
	move.b	d0,2(a0)	;[12] 	;write C vol			;[[116]]

pslx2	move.l	(sp)+,a0	;[12]	;exit
	move.w	(sp)+,d0	;[ 8]
	rts			;[16]		;total 268 (was 282)	;[[ 36]]

