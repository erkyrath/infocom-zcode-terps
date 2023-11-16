 	include MacTraps.D
	include ToolEqu.D
	include Sysequ.d

	XREF	makesound
	
;*************************************************************************
;*
;*	soundFlag receives the value that tells how we want to play
;*	our two sounds.  Values to be passed are as follows:
;*
;*		1: Play the first sound ONE TIME, asynchronously
;*		2: Play the first sound MULTIPLE times, using the
;*			random scan-rate modifier algorithm
;*		3: Turn off the first sound and restore the original
;*			scan rate (should only be used after 2 has
;*			been passed)
;*		4: Play the second sound ONE TIME, asynchronously
;*
;*************************************************************************

Start
	bsr     initmanagers		; initialize the system
	
	
	move.w	#1,d0			; horse
	bsr	makesound		; 
	
	move.w	#$3f,d0			; a short delay
	bsr	delay
	
	move    #3,d0                   ;turn off loop
	bsr      makesound

	move.w	#$3f,d0			; a short delay
	bsr	delay
	


	_exittoshell			; go home neatly (???)

delay					; receives length in d0
	move.l	#$1ffff,d1
inner_loop
	dbra	d1,inner_loop		; loop inside
	dbra	d0,delay		; loop outside
	rts				; all go home now...
	
InitManagers

	_MoreMasters
	MOVE.L		#$8FFFFFFF,D0
	_NewHandle
	PEA		-4(A5)
	_InitGraf
	_InitFonts
	_InitWindows
	_InitMenus
	_TEInit
	CLR.L		-(SP)
	_InitDialogs
	MOVE.L		#$0000FFFF,D0
	_FlushEvents
	_InitCursor
	RTS
	
