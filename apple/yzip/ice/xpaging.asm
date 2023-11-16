	TITLE	"Apple ][ YZIP (c)Infocom","MEMORY PAGING ROUTINES"

; -------------------------
; POINT [MPC] TO V-ADDR [I]
; -------------------------

SETWRD:	LDA	I+LO	
	STA	MPCL	
	LDA	I+HI	
	STA	MPCM	
	LDA	#0	
	STA	MPCH	; ZERO TOP BIT
	JMP	VLDMPC	

;
WANTED:	DB	00,00	
;
NEXT:	DB	00	
NSUBA:	DB	00	
PSUBA:	DB	00	
;
YTEMP:	DB	00	
ATEMP:	DB	00	
NSUBY:	DB	00	
;
; set [A](page), [Y](bank) to point to memory page where page in [A] is
;
SETPC:			
	sta	MEMPAGE		; save it for later addition
	cmp	#P2PAGE 	; IS IT A PAGE IN MAIN
	bcs	VF2		; No, it might be in aux mem

	lda	# HIGH ZBEGIN 	; ADD OFFSET TO GET RAM PAGE
	ldy	#MAIN		; in the main bank
	beq	VFEXI		; BRA to fetch
VF2:
	cmp	#PGBEGIN	; is it paged?
	bcs	VFERR		; yes it be paged, so can't deal with it
	cmp	#P3PAGE		; is it in Aux Mem, Part 2?
	bcs	VF3		; yes, so subtract different amount
;
; this is in lower aux 
;				
	lda	#(Z2PAGE-Z1SIZE) ; subtract size from offset
	ldy	#AUX		; show aux mem
	bne	VFEXI		; jump to end
VF3:
	lda	#(Z3PAGE-(Z1SIZE+Z2SIZE)) ; subtract out first 2 sides
	ldy	#P3BANK		; show page 3 bank
VFEXI:
	clc			; get ready for addition
	adc	MEMPAGE		; now get actual offset
	rts	
VFERR:
;
; out of range
;
	lda	#18
	jmp	ZERROR
;
; NEXTSPC - inc SPCL and check for wrapping round to next bank
;
NEXTSPC:
	inc	SPCL		; next lo byte
	bne	NXSP_EXIT	; no change then
	inc	SPCH		; next page
	lda	SPCH		; so get page
	cmp	# HIGH PRGLBL	; have we reached end of line?
	bne	NXSP_EXIT	; we be okay
	lda	SPCBNK		; get bank
	bne	NXSP1		; must go to Part 3
	inc	SPCBNK		; so point to aux bank
	lda	#Z2PAGE		; first page in aux
	sta	SPCH		; and point to it
	rts			; and all done
NXSP1:
	lda	#Z3PAGE		; get start of page 3
	sta	SPCH		; and point there
	lda	#P3BANK		; and point to this bank
	sta	SPCBNK		; okey
NXSP_EXIT:
	rts
;
; NEXTFPC - inc DPCL and check for wrapping round to next bank
;
NEXTFPC:
	inc	FPCL		; next lo byte
	bne	NXFP_EXIT	; no change then
	inc	FPCH		; next page
	lda	FPCH		; and get it for checking
	cmp	# HIGH PRGLBL	; have we reached end of line?
	bne	NXFP_EXIT	; we be okay
	lda	FPCBNK		; get bank
	bne	NXFP1		; must skip over stuff in middle
	inc	FPCBNK		; so point to aux bank
	lda	#Z2PAGE		; first page in aux
	sta	FPCH		; and point to it
	rts			; toots finis
NXFP1:
	lda	#Z3PAGE		; start of part 3
	sta	FPCH		; so show me
	lda	#P3BANK		; and point to this bank
	sta	FPCBNK		; okey
NXFP_EXIT:
	rts
;
; ADDFPC - add amount in [A] to current FPC and check for bank wrap
;
ADDFPC:
	clc			; get ready for add
	adc	FPCL		; add lo part
	sta	FPCL		; and save it
	bcc	AFPX		; all done if no page wrap
	inc	FPCH		; point to next page
	lda	FPCH		; get it for compare
	cmp	# HIGH PRGLBL	; at end of line in main bank?
	bne	AFPX		; nope, all done then
	lda	FPCBNK		; get bank
	beq	AFP1		; it is main, so we be ok
	lda	#Z3PAGE		; must go to part 3 if in aux mem
	sta	FPCH		; thanx
	lda	#P3BANK		; and point to this bank
	sta	FPCBNK		; okey
	rts			; done
AFP1:
	inc	FPCBNK		; point to aux
	lda	#Z2PAGE		; get start in aux
	sta	FPCH		; and save it
AFPX:
	rts
;
; ADDSPC - add amount in [A] to current SPC and check for bank wrap
;
ADDSPC:
	clc			; get ready for add
	adc	SPCL		; add lo part
	sta	SPCL		; and save it
	bcc	ASPX		; all done if no page wrap
	inc	SPCH		; point to next page
	lda	SPCH		; get it for compare
	cmp	# HIGH PRGLBL	; at end of line in main bank?
	bne	ASPX		; nope, all done then
	lda	SPCBNK		; get bank
	beq	ASP1		; it is main, so we be ok
	lda	#Z3PAGE		; must go to part 3 if in aux mem
	sta	SPCH		; thanx
	lda	#P3BANK		; and point to this bank
	sta	SPCBNK		; okey
	rts			; done
ASP1:
	inc	SPCBNK		; point to aux
	lda	#Z2PAGE		; get start in aux
	sta	SPCH		; and save it
ASPX:
	rts
;
; PREVFPC - DEC FPCL and check for wrapping round to next bank
;
PREVFPC:
	lda	FPCL		; get lo part
	bne	PFPC2		; it's not zero, so no wrapping
	lda	FPCH		; get current page
	cmp	#Z2PAGE		; have we reached beginning of page 2?
	beq	PFPC1		; wrap to first bank
	cmp	#Z3PAGE		; beginning of part 3?
	beq	PFPC3		; ayyup
	dec	FPCH		; point to previous page
	bne	PFPC2		; okay
PFPC1:
	lda	FPCBNK		; get bank
	beq	VF1ERR		; oops, can't go backwards from main bank
	lda	#MAIN		; so point to main bank
	beq	PFPC4		; and store it away
PFPC3:
	lda	#AUX		; and point to this bank
PFPC4:
	sta	FPCBNK		; okey
	lda	#(PRGLBL>>8)-1	; get me last page in part 2
	sta	FPCH		; and show me
PFPC2:
	dec	FPCL		; and point to previous byte
	rts
VF1ERR:
;
; out of range
;
	lda	#19
	jmp	ZERROR
;
; PREVSPC - DEC SPCL and check for wrapping round to main bank
;
PREVSPC:
	lda	SPCL		; get lo part
	bne	PSPC2		; it's not zero, so no wrapping
	lda	SPCH		; get current page
	cmp	#Z2PAGE		; have we reached beginning of page 2?
	beq	PSPC1		; wrap to first bank
	cmp	#Z3PAGE		; down past page 3?
	beq	PSPC3		; sure is
	dec	SPCH		; point to previous page
	bne	PSPC2		; okay
PSPC1:
	lda	SPCBNK		; get bank
	beq	VF1ERR		; oops, can't go backwards from main bank
	lda	#MAIN		; so point to main bank
	beq	PSPC4		; so save it
PSPC3:
	lda	#AUX		; and point to this bank
PSPC4:
	sta	FPCBNK		; okey
	lda	# HIGH PRGLBL-1	; get me last page in low part
	sta	SPCH		; and show me
PSPC2:
	dec	SPCL		; and point to previous byte
	rts
;
; FP2SP - copy the 3 parts of FPC to SPC
;
FP2SP:
	lda	FPCBNK
	sta	SPCBNK
	lda	FPCH
	sta	SPCH
	lda	FPCL
	sta	SPCL
	rts
	
; MAKE [MPCPNT],[MPCBNK] POINT TO
; THE RAM PAGE AND BANK THAT HOLDS
; THE V-PAGE MPCH,M
;
VLDMPC:			
	lda	MPCH		; check hi part
	bne	VLD3		; NOT IN FIRST V-64K, so must be paged
	lda	MPCM		; check to see if it is paged
	jsr	CHECKPRE	; is it preloaded?
	bcs	VLD3		; no, so it be paged
	jsr	SETPC		; so put page/bank into A/Y
	sty	MPCBNK	
	sta	MPCPNT+HI	
NOMUCK:
	rts	
;
; must be paged, so check for it or read it in
;
VLD3:	
	lda	MPCH	
	ldy	MPCM	
	jsr	FIND_PAGE	;RETURN BUFFER IN A THAT HAS VPAGE A,Y
	clc		
	adc	# HIGH PBEGIN	
	sta	MPCPNT+HI	
	ldy	#AUX	; paging buffers are in aux mem
	sty	MPCBNK	
;
; TEST FOR MUCK
;
	lda	MUCKFLG	
	beq	NOMUCK	
	bne	VLDZPC	;MAY HAVE MUCKED ZPC SO GO FIX
;
; SAME IDEA AS VLDMPC
;
VLDZPC:
        lda     INFODOS         ; check first for InfoDOS page
        beq     VLDZ1           ; none
        jsr     INFO_PAGE       ; well, is it?
        bcc     VLDZ1           ; nope
        rts                     ; all set otherwise
VLDZ1:
	lda	ZPCH	
	bne	VLDZ3		;NOT IN FIRST V-64K, so must be paged
	lda	ZPCM		; check to see if it is paged
	jsr	CHECKPRE	; is it preloaded?
	bcs	VLDZ3		; no, so it must be paged
	jsr	SETPC		; point to correct bank and page
	sty	ZPCBNK		; set bank
	sta	ZPCPNT+HI	; and MSB of pointer
NOZMUCK:
	rts		
VLDZ3:				;MUST BE PAGED
	lda	ZPCH	
	ldy	ZPCM	
	jsr	FIND_PAGE		;RETURN BUFFER IN A THAT HAS VPAGE A,Y
	clc		
	adc	# HIGH PBEGIN	
	sta	ZPCPNT+HI	
	ldy	#AUX	
	sty	ZPCBNK	
;
; TEST MUCKING
;
	lda	MUCKFLG	
	beq	NOZMUCK	
	jmp	VLDMPC	;MAY HAVE MUCKED MPC SO GO FIX


; FIND V-PAGE A,Y IF IT IS IN MEM
; AND RETURN WITH LINKED LIST
; PROPERLY MAINTAINED
; IF V-PAGE A,Y NOT IN MEM
; GET FROM DISK AND PUT IN RIGHT
; PLACE

MUCKFLG:	DB	00	;00 IF PAGING BUFFERS NOT MUCKED

FIND_PAGE:
	sta	WANTED+HI	
	sty	WANTED+LO	
	ldx	#0	
	stx	MUCKFLG		; CLEAR MUCK FLAG
	jsr	WHERE	
	bcc	TOUGH		; PAGE IS RESIDENT IN PAGING SPACE
;
; PAGE MUST BE BROUGHT IN FROM DISK
;
	ldx	CURRENT		;GET BUFFER TO PUT PAGE INTO
	lda	NEXTPNT,X	;BY LOOKING AT NEXT POINTER
	sta	CURRENT		;MAKE IT THE CURRENT BUFFER
	tax		
	lda	WANTED+HI	;LET BUFFER MAP KNOW
	sta	VPAGEH,X	;WHICH PAGE
	lda	WANTED+LO	;IS GOING TO
	and	#$FE		; make address be even
	sta	VPAGEL,X	;BE THERE
;***
; point to the next page too
;
	ora	#1		; add one to point to next 256 byte page
	pha			; save it
	txa			; get pointer
	tay			; into y
	iny			; point to next buffer
	pla			; get second buffer back
	sta	VPAGEL,Y	; so point to it
	lda	VPAGEH,X	; get MSB
	sta	VPAGEH,Y	; and save it
;
; A = WANTED+HI
; Y = WANTED+LO
; X = BUFFER
;
	lda	WANTED+LO	
	and	#$FE		; clear low bit to make it even
	tay			; want it in y
;*** 
	lda	WANTED+HI	
	ldx	CURRENT
	jsr	GETVPAGE	; PUT V-PAGE A,Y INTO PAGING BUFFER X
;***
	dec	MUCKFLG		; INDICATE A MUCKING
	bne	PAGEXIT		; and return current buffer
TOUGH:
	and	#$FE		; make even page, please
	sta	NEXT	
	cmp	CURRENT		; GETS REALY SCREWED IF CURRENT==NEXT
	beq	PAGEXIT		; DO NOT CHANGE POINTERS IF IT DOES
;
; Y=NEXT(CURRENT)
; DO THE RIGHT THING TO THE POINTERS
;
;	ldy	CURRENT	
;	lda	NEXTPNT,Y	
;	sta	NSUBCUR	
	lda	NEXT	
	jsr	DETATCH	
	ldy	CURRENT	
	lda	NEXT	
	jsr	INSERT	
	lda	NEXT	
	sta	CURRENT	
PAGEXIT:
;*** perhaps add one to point to correct buffer
	lda	WANTED+LO	; get LSB
	and	#$01		; pick up even/odd bit
	clc			; doing add
	adc	CURRENT		; point to correct buffer
	rts

GETVPAGE:			
	sta	DBLOCK+HI	
	sty	DBLOCK+LO	
	txa			; get which paging buffer
	clc		
	adc	# HIGH PBEGIN	; and set up abs addr
	sta	DBUFF+HI	; thank you, that's much better
	ldx	#AUX	
	stx	DSKBNK	
	jmp	GETDSK	

; INSERT A AFTER Y
; A.next = Y.next
; Y.next = A
; A.previous = Y
; [Y.next].previous = A
INSERT:
	tax
	lda	NEXTPNT,Y	; Y.next
	sta	NEXTPNT,X	; A.next = Y.next
	pha			; save Y.next for later
	txa
	sta	NEXTPNT,Y	; Y.next = A
	tya
	sta	PREVPNT,X	; A.prev = Y
	pla			; get Y.next back
	tay			; [Y.next].previous
	txa
	sta	PREVPNT,Y	; [Y.next].previous = A
	rts

	IF	0	
;
; old one, which puts A AFTER! Y
;
; PREV(A)=Y
; PREV(NEXT(Y))=A
; NEXT(A)=NEXT(Y)
; NEXT(Y)=A

	sta	ATEMP	
	sty	YTEMP	
	tax		
	tya		
	sta	PREVPNT,X	

	lda	NEXTPNT,Y	
	sta	NSUBY	
	txa		
	ldx	NSUBY	
	sta	PREVPNT,X	

	txa		
	ldx	ATEMP	
	sta	NEXTPNT,X	

	lda	ATEMP	
	sta	NEXTPNT,Y	
	rts		
	ENDIF

; DETATCH BUFFER >A<
; NEXT(PREV(A))=NEXT(A)
; PREV(NEXT(A))=PREV(A)

DETATCH:
	tax
	lda	NEXTPNT,X
	tay			; Y == A.next
;	STA	NSUBA
	lda	PREVPNT,X	; get A.previous
;	STA	PSUBA	
	tax			; X == A.previous
	tya			; get A.next
;	LDA	NSUBA	
	STA	NEXTPNT,X	; [A.previous].next = A.next
;	LDA	PSUBA	
	txa			; get A.previous
;	LDX	NSUBA	
	STA	PREVPNT,Y	; [A.next].previous = A.previous
	RTS		


; RETURN BUFFER OF PAGE [WANTED]
; IN >A< ELSE SEC  (Y=WANTED+LO)

WHERE:	LDX	#NUMBUFS-1	
WHLOOP:			
	LDA	WANTED+HI	
	CMP	VPAGEH,X	;>SAME
	BEQ	WHGOT	
WHNOGOT:			
	DEX		
	BPL	WHLOOP	
	SEC		
	RTS		
WHGOT:			
	TYA		
	CMP	VPAGEL,X	
	BNE	WHNOGOT	
	TXA		
	CLC		
	RTS		
;
; CHECKPRE - check to see if page in [A] is in preloaded
;
CHECKPRE:
	cmp	TBLPUR		; check against PURE tables
	bcc	CHKPEXY		; must be preloaded then
CHKP1:
	cmp	FUNPRE		; is it in function preload?
	bcc	CHKPEXN		; preloaded function > desired, not preloaded
	cmp	FUNPUR		; how bout at end?
	bcs	CHKPEXN		; it is not preloaded
CHKP3:
	clc			; doing add
	adc	FUNPGE		; get me memory page for function
CHKPEXY:
	clc			; show it is preloaded
	rts			; then we got it
CHKPEXN:
	sec			; show it ain't here
	rts
;
; INFO_PAGE - is it one of the special preloaded pages for infoDOS?  If it
;       is, then set up ZPCPNTR to point to it, and set carry.  Otherwise,
;       clear carry to show it ain't.
INFO_PAGE:
        lda     ZPCH            ; get 2 parts
        sta     BIGPAGE+HI
        lda     ZPCM
        sta     BIGPAGE+LO
        lsr     BIGPAGE+HI      ; /2 to get 512 block        
        ror     BIGPAGE+LO
        ldy     #SGTSEG         ; point to first segment, MSB
        lda     (INFODOS),Y     ; howzit look?
        iny                     ; point to LSB
        cmp     BIGPAGE+HI
        bcc     INFP1           ; might be interesting
        bne     INFPNX          ; not here, < than minimum
        lda     (INFODOS),Y     ; how bout LSB
        cmp     BIGPAGE+LO
        beq     INFPYX          ; found it
        bcs     INFPNX          ; nope, < than minimum again
;
; here, it's at least > than minimum
;
INFP1:
        iny                     ; point at end block, MSB
        lda     (INFODOS),Y     ; howz end segment look
        cmp     BIGPAGE+HI
        bcc     INFPNX          ; nope, > than maximum of special
        bne     INFPYX          ; yup, < than maximum of special
        iny                     ; LSB of last one
        lda     (INFODOS),Y     ; is LSB < special?
        cmp     BIGPAGE+LO      ; MSB of current one == MSB of special        
        bcc     INFPNX          ; nope, not here
INFPYX:
        ldy     #SGTSEG+1       ; point back to start block, LSB
        lda     (INFODOS),Y     ; get start block
        asl     A               ; *2 to get start page
        sta     BIGPAGE+LO      ; save it
        lda     ZPCM
        sec                     ; doing sub
        sbc     BIGPAGE+LO      ; get offset into special block
        clc                     ; now add in offset
        adc     # HIGH SP_START      ; get the start of special area
        sta     ZPCPNT+HI       ; show ZPCPNTR
        lda     #MAIN           ; main bank
        sta     ZPCBNK          ; okey
        sec                     ; show it was here
        rts
INFPNX:
        clc                     ; show not here
        rts                     ; g'day
        
        
        
CHKPEXN0:

GETBYT:			
	ldy	MPCL	
	jsr	MFETCH		; go and get it
	inc	MPCL		;POINT TO NEXT BYTE
	bne	GETGOT		;IF NO CROSS WE ARE STILL VALID
	inc	MPCM	
	bne	GET1
	inc	MPCH
GET1:
	pha			; save byte	
	jsr	VLDMPC	
	pla			; and get it back
GETGOT:
	tay		;SET FLAGS
	rts		;RED SLIPPER TIME
;
; NEXTPC - Fetch the byte at the current ZPC, point to next byte and
; 		validate pointer
;
NEXTPC:
	ldy	ZPCL		; get low pointer
	jsr	ZFETCH		; fetch @ZPCPNT
	inc	ZPCL	
	bne	NXTGOT	
	inc	ZPCM	
	bne	CRSZ1	
	inc	ZPCH	
CRSZ1:
	pha			; save opcode
	jsr	VLDZPC	
	pla			; and get it back
NXTGOT:
	tay		
	rts		

;
; STASHB - use SPC to save a byte in either aux or main mem
;
STASHB:	
	ldy	SPCBNK		; get the bank
	bmi	SB1		; must be in upper RAM
	sta	WRTBNK,Y	; set bank
	ldy	#0		; can only do this with Y
	sta	(SPC),Y		; get the sucker
	beq	SBEXI		; jump to end it
;
; this is in aux mem, >$E000
;
SB1:
	ldy	SPCH		; get high part
	sty	SBMOD+2		; and self mod my code
	ldy	SPCL		; and get the low part
	sta	ALTZP+AUX	; talk about aux mem
SBMOD:
	sta	Z3BEGIN,Y	; store the little byte
	sta	ALTZP+MAIN	; go back to main mem
SBEXI:
	sta	WRTBNK+MAIN	; and write to main
	rts
;
; FETCHB - fetch a byte from either main memory, aux memory, or upper
;	Aux memory
;
FETCHB:
	ldy	FPCBNK		; get the bank
	bmi	FB1		; must be in upper RAM
	sta	STORE80+OFF	; turn off this switch
	jsr	ZERO_FB		; go to low end fetch
	sta	STORE80+ON	; turn it back on
	rts
;
; this is in aux mem, >$E000
;
FB1:
	lda	FPCH		; get which page
	sta	FBMOD+2		; and show in operand
	ldy	FPCL		; get which byte
	sta	ALTZP+AUX	; talk about aux mem
FBMOD:	lda	Z3BEGIN,Y	; get the sucker
	sta	ALTZP+MAIN	; go back to main mem
	tay			; set condition code
	rts
;
; ZFETCH - after checking for which bank (main, aux 1 or aux 2), go get
;  the byte @ ZPCPNT, with the offset being in [Y]
;
ZFETCH:
	ldx	ZPCBNK		; get the bank
	bmi	ZFB1		; must be in upper RAM
	sta	STORE80+OFF
	jsr	ZERO_ZF		; go to low end fetch
	sta	STORE80+ON
	rts
;
; this is in aux mem, >$E000
;
ZFB1:
	lda	ZPNTH		; which page are we talking about
	sta	ZBMOD+2		; show in the operand
	sta	ALTZP+AUX	; talk about aux mem
ZBMOD:	lda	Z3BEGIN,Y	; get the sucker
	sta	ALTZP+MAIN	; go back to main mem
	tax			; set condition code
	rts
;
; MFETCH - after checking for which bank (main, aux 1 or aux 2), go get
;  the byte @MPCPNT, with the offset being in [Y]
;
MFETCH:
	ldx	MPCBNK		; get the bank
	bmi	MB1		; must be in upper RAM
	sta	STORE80+OFF
	jsr	ZERO_MF		; go to low end fetch
	sta	STORE80+ON
	rts
;
; this is in aux mem, >$E000
;
MB1:
	lda	MPNTH		; which page are we talking about
	sta	MBMOD+2		; show in the operand
	sta	ALTZP+AUX	; talk about aux mem

MBMOD:	lda	Z3BEGIN,Y	; get the sucker

	sta	ALTZP+MAIN	; go back to main mem
	tax			; set condition code
	rts


	END
