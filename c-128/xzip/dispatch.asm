	PAGE	
	STTL "--- OPCODE DISPATCH TABLES ---"


	; 0-OPS

OPT0H:	DB	>ZRTRUE	;B0
	DB	>ZRFALS	;B1
	DB	>ZPRI	;B2
	DB	>ZPRR	;B3
	DB	>ZNOOP	;B4
	DB	>OSAVE	;B5
	DB	>OREST	;B6
	DB	>ZSTART	;B7
	DB	>ZRSTAK	;B8
	DB	>ZCATCH	;B9
	DB	>ZQUIT	;BA
	DB	>ZZCRLF	;BB
	DB	>ZUSL	;BC
	DB	>ZVER	;BD
	DB	>ZEXTOP	;BE
	DB	>ZORIG	;BF

OPT0L:	DB	<ZRTRUE	;B0
	DB	<ZRFALS	;B1
	DB	<ZPRI	;B2
	DB	<ZPRR	;B3
	DB	<ZNOOP	;B4
	DB	<OSAVE	;B5
	DB	<OREST	;B6
	DB	<ZSTART	;B7
	DB	<ZRSTAK	;B8
	DB	<ZCATCH	;B9
	DB	<ZQUIT	;BA
	DB	<ZZCRLF	;BB
	DB	<ZUSL	;BC
	DB	<ZVER	;BD
	DB	<ZEXTOP	;BE
	DB	<ZORIG	;BF

	; 1-OPS

OPT1H:	DB	>ZZERO	;80,90,A0
	DB	>ZNEXT	;81
	DB	>ZFIRST	;82
	DB	>ZLOC	;83
	DB	>ZPTSIZ	;84
	DB	>ZINC	;85
	DB	>ZDEC	;86
	DB	>ZPRB	;87
	DB	>ZCALL1	;88 (EZIP)
	DB	>ZREMOV	;89
	DB	>ZPRD	;8A
	DB	>ZRET	;8B
	DB	>ZJUMP	;8C
	DB	>ZPRINT	;8D
	DB	>ZVALUE	;8E
	DB	>ZICLL1	;8F

OPT1L:	DB	<ZZERO	;80
	DB	<ZNEXT	;81
	DB	<ZFIRST	;82
	DB	<ZLOC	;83
	DB	<ZPTSIZ	;84
	DB	<ZINC	;85
	DB	<ZDEC	;86
	DB	<ZPRB	;87
	DB	<ZCALL1	;88 (EZIP)
	DB	<ZREMOV	;89
	DB	<ZPRD	;8A
	DB	<ZRET	;8B
	DB	<ZJUMP	;8C
	DB	<ZPRINT	;8D
	DB	<ZVALUE	;8E
	DB	<ZICLL1	;8F

	; 2-OPS

OPT2H:	DB	>BADOP2	;00 (UNDEFINED)
	DB	>ZEQUAL	;01
	DB	>ZLESS	;02
	DB	>ZGRTR	;03
	DB	>ZDLESS	;04
	DB	>ZIGRTR	;05
	DB	>ZIN	;06
	DB	>ZBTST	;07
	DB	>ZBOR	;08
	DB	>ZBAND	;09
	DB	>ZFSETP	;0A
	DB	>ZFSET	;0B
	DB	>ZFCLR	;0C
	DB	>ZSET	;0D
	DB	>ZMOVE	;0E
	DB	>ZGET	;0F
	DB	>ZGETB	;10
	DB	>ZGETP	;11
	DB	>ZGETPT	;12
	DB	>ZNEXTP	;13
	DB	>ZADD	;14
	DB	>ZSUB	;15
	DB	>ZMUL	;16
	DB	>ZDIV	;17
	DB	>ZMOD	;18
	DB	>ZCALL2	;19 (EZIP)
	DB	>ZICLL2	;1A
	DB	>ZCOLOR	;1B
	DB	>ZTHROW	;1C
	DB	>BADOP2	;1D
	DB	>BADOP2	;1E
	DB	>BADOP2	;1F

OPT2L:	DB	<BADOP2	;00 (UNDEFINED)
	DB	<ZEQUAL	;01
	DB	<ZLESS	;02
	DB	<ZGRTR	;03
	DB	<ZDLESS	;04
	DB	<ZIGRTR	;05
	DB	<ZIN		;06
	DB	<ZBTST	;07
	DB	<ZBOR	;08
	DB	<ZBAND	;09
	DB	<ZFSETP	;0A
	DB	<ZFSET	;0B
	DB	<ZFCLR	;0C
	DB	<ZSET	;0D
	DB	<ZMOVE	;0E
	DB	<ZGET	;0F
	DB	<ZGETB	;10
	DB	<ZGETP	;11
	DB	<ZGETPT	;12
	DB	<ZNEXTP	;13
	DB	<ZADD	;14
	DB	<ZSUB	;15
	DB	<ZMUL	;16
	DB	<ZDIV	;17
	DB	<ZMOD	;18
	DB	<ZCALL2	;19 (EZIP)
	DB	<ZICLL2	;1A
	DB	<ZCOLOR	;1B
	DB	<ZTHROW	;1C
	DB	<BADOP2	;1D
	DB	<BADOP2	;1E
	DB	<BADOP2	;1F

	; X-OPS

OPTXH:	DB	>ZCALL	;E0
	DB	>ZPUT	;E1
	DB	>ZPUTB	;E2
	DB	>ZPUTP	;E3
	DB	>ZREAD	;E4
	DB	>ZPRC	;E5
	DB	>ZPRN	;E6
	DB	>ZRAND	;E7
	DB	>ZPUSH	;E8
	DB	>ZPOP	;E9
	DB	>ZSPLIT	;EA
	DB	>ZSCRN	;EB

	; (EZIPS FROM HERE ON)
	DB	>ZXCALL	;EC
	DB	>ZCLR	;ED
	DB	>ZERASE	;EE
	DB	>ZCURST	;EF
	DB	>ZCURGT	;F0 (NOT IMPLEMENTED)
	DB	>ZLIGHT	;F1
	DB	>ZBUFOUT	;F2
	DB	>ZDIRT	;F3
	DB	>ZDIRIN	;F4 (NOT IMPLEMENTED)
	DB	>ZSOUND	;F5
	DB	>ZINPUT	;F6
	DB	>ZINTBL	;F7
	DB	>ZBCOM 	;F8
	DB	>ZICALL	;F9
	DB	>ZIXCLL	;FA
	DB	>ZLEX	;FB
	DB	>ZWSTR	;FC
	DB	>ZCOPYT	;FD
	DB	>ZPRNTT	;FE
	DB	>ZASSND	;FF

OPTXL:	DB	<ZCALL	;E0
	DB	<ZPUT	;E1
	DB	<ZPUTB	;E2
	DB	<ZPUTP	;E3
	DB	<ZREAD	;E4
	DB	<ZPRC	;E5
	DB	<ZPRN	;E6
	DB	<ZRAND	;E7
	DB	<ZPUSH	;E8
	DB	<ZPOP	;E9
	DB	<ZSPLIT	;EA
	DB	<ZSCRN	;EB

	; (EZIPS FROM HERE ON)
	DB	<ZXCALL	;EC
	DB	<ZCLR	;ED
	DB	<ZERASE	;EE
	DB	<ZCURST	;EF
	DB	<ZCURGT	;F0 (NOT IMPLEMENTED)
	DB	<ZLIGHT	;F1
	DB	<ZBUFOUT	;F2
	DB	<ZDIRT	;F3
	DB	<ZDIRIN	;F4 (NOT IMPLEMENTED)
	DB	<ZSOUND	;F5
	DB	<ZINPUT	;F6
	DB	<ZINTBL	;F7
	DB	<ZBCOM 	;F8
	DB	<ZICALL	;F9
	DB	<ZIXCLL	;FA
	DB	<ZLEX	;FB
	DB	<ZWSTR	;FC
	DB	<ZCOPYT	;FD
	DB	<ZPRNTT	;FE
	DB	<ZASSND	;FF


EXTOPH:	DB	>ZSAVE	;100
	DB	>ZREST	;101
	DB	>ZSHIFT	;102
	DB	>ZASHFT	;103
	DB	>ZFONT	;104
	DB	>ZDISPL	;105
	DB	>ZPICNF	;106
	DB	>ZDCLR	;107
	DB	>ZMARG	;108
	DB	>ZISAVE	;109
	DB	>ZIREST	;10A

EXTLEN	EQU	$-EXTOPH

EXTOPL:	DB	<ZSAVE	;100
	DB	<ZREST	;101
	DB	<ZSHIFT	;102
	DB	<ZASHFT	;103
	DB	<ZFONT	;104
	DB	<ZDISPL	;105
	DB	<ZPICNF	;106
	DB	<ZDCLR	;107
	DB	<ZMARG	;108
	DB	<ZISAVE	;109
	DB	<ZIREST	;10A

	END
