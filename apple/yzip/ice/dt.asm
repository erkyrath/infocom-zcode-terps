DLINE:	MACRO	STRING,SLEN
	ldx	\#LOW #STRING#	; get part of STRING
	lda	\#HIGH #STRING#	; get other part of string
	IF      (#SLEN#+"L") == "L"      ; is there a length defined?
	ldy	\##STRING#L	; get length of string
        printx  "IF"
	ELSE
	ldy	#SLEN#		; then just fetch it
        printx  "ELSE"
	ENDIF
	jsr	DISPLAY_LINE		; print the string
	ENDM

        DLINE TEST,TEX
        rts
DISPLAY_LINE:
        rts
TEST:   db      "This is a test"
TEX   EQU     $-TEST


        END
