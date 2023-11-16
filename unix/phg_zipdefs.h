/* Header file for C ZIP */

typedef unsigned char ZOBJECT;	/* FOR USE WITH OBJECTS */
typedef unsigned short int ZIPINT;	/* 16 INTEGERS ALL THE WAY */
#define ZMVERS 3		/* Z-MACHINE VERSION NUMBER */
#define LSTACK 512		/* STACK SIZE IN BYTES */
#define STKLEN 64		/* SYSTEM STACK LENGTH */
#define BLKSIZ 512		/* SIZE OF VIRTUAL PAGES */
#define MAXBLKS 256		/* VIRTUAL MACHINE UPPER LIMIT */
#define DATASIZ 128		/* USE 64K OF MEMORY AS DEFAULT */
#define IBUFSIZ 100
#define OBUFSIZ 100

		/* OFFSETS FOR HEADER INFORMATION */

#define PVERS1 0		/* ZVERSION BYTE */
#define PVERS2 1		/* ZVERSION MODE BYTE */
#define PZRKID 2		/* ZORK ID */
#define PENDLD 4		/* ENDLOD POINTER */
#define PSTART 6		/* Z ENTRY POINT */
#define PVOCTB 8		/* POINTER TO VOCABULARY TABLE */
#define POBJTB 10		/* POINTER TO OBJECT TABLE */
#define PGLOTB 12		/* POINTER TO GLOBAL TABLE */
#define PPURBT 14		/* POINTER TO BEGINNING OF PURE CODE */
#define PFLAGS 16		/* POINTER TO FLAGS */
#define PSERNM 18		/* 6 BYTE SERIAL NUMBER (DATE) */
#define PWRDTB 24		/* POINTER TO FULL WORDS FOR PUTSTR */
#define PLENTH 26		/* LENGTH OF GAME IN WORDS */
#define PCHKSM 28		/* CHECKSUM OF BYTES FROM THE END OF THE
				++ HEADER TO LENGTH */
#define HDRSIZ 64		/* LENGTH OF HEADER IN BYTES */

#define ESC 27
#define Z_EOL 1
#define EOL 10			/* LINE FEED IS THE UNIX WAY */
#define FEEP "\007"		/* BELL CHAR FOR BEEPING */
#define BKSPC 8			/* BACKSPACE VALUE */
#define PADCHR 5		/* VALUE OF PAD CHAR FOR VOCAB */
#define SPACE '\040'		/* INITIAL READ BREAD CHARS */
#define TAB '\011'
#define CR '\015'
#define FF '\014'
#define PERIOD '.'
#define COMMA ','
#define QMARK '?'
#define NULL 0
#define HEX 'x'
#define DEC 'd'
#define RM 80
#define LM 1

/*	O P C O D E S 		*/

/* ZERO OPS */

#define OPRTRU 176		/* RETURN TRUE */
#define OPRFAL 177		/* RETURN FALSE */
#define OPPRNI 178		/* PRINTI */
#define OPPRNR 179
#define OPNOOP 180
#define OPSAVE 181
#define OPREST 182
#define OPRSTT 183
#define OPRSTA 184
#define OPFSTA 185
#define OPQUIT 186
#define OPCRLF 187
#define OPUSL 188
#define OPVERI 189

/* ONE OPS */

#define OPQZER 128
#define OPQNEX 129
#define OPQFIR 130
#define OPLOC 131
#define OPPTSI 132
#define OPINC 133
#define OPDEC 134
#define OPPRNB 135
#define OPREMO 137
#define OPPRND 138
#define OPRETU 139
#define OPJUMP 140
#define OPPRIN 141
#define OPVALU 142
#define OPBCOM 143

/* TWO OPS AND EXTENDED OPS */

#define OPUNDF 0
#define OPQEQU 1
#define OPQLES 2
#define OPQGRT 3
#define OPQDLE 4
#define OPQIGR 5
#define OPQIN 6
#define OPBTST 7
#define OPBOR 8
#define OPBAND 9
#define OPQFSE 10
#define OPFSET 11
#define OPFCLE 12
#define OPSET 13
#define OPMOVE 14
#define OPGET 15
#define OPGETB 16
#define OPGETP 17
#define OPGTPT 18
#define OPNEXT 19
#define OPADD 20
#define OPSUB 21
#define OPMUL 22
#define OPDIV 23
#define OPMOD 24
#define LAST_TWO_OP OPMOD

#define XQEQU 193
#define OPCALL 224
#define OPPUT 225
#define OPPUTB 226
#define OPPUTP 227
#define OPREAD 228
#define OPPRNC 229
#define OPPRNN 230
#define OPRAND 231
#define OPPUSH 232
#define OPPOP 233
#define OPSPLT 234
#define OPSCRN 235


/*  P R O P E R T Y   D E F I N I T I O N    O F F S E T S   */

#define OBJLEN 9		/* LENGTH OF AN OBJECT */
#define DEF_PROP_TBL_LEN 53	/* LENGTH OF DEFAULT PROPERTY TABLE */
#define FLAGS1 0		/* OFFSET TO FLAG WORDS */
#define FLAGS2 2
#define PARENT 4		/* PARENT OBJECT NUMBER */
#define SIBLING 5		/* SIBLING OJECT NUMBER */
#define CHILD1 6		/* CHILD OBJECT NUMBER */
#define PROPS 7			/* POINTER TO PROPERTY TABLE */

#define PNUMSK 31		/* PROPERTY ID NUMBER MASK (LOW 5 BITS) */
#define PSZMSK 224		/* PROPERTY SIZE MASK (HIGH 3 BITS) */
#define PROPSIZE 5		/* SHIFT COUNT TO ISOLATE SIZE BITS */
#define MAXARGS 5		/* ZIL MAXIMUM NUMBER OF ARGS */

/* G E N E R A L   E Q U A T E S  */

#define SCRIPTBIT 1
#define PBUFSIZ 85
#define CONSOLE 0
#define PIPE 1
#define PIPEWIDTH 79
#define EREOL (printf("\033[K")) 
#define STATLEN 1
#define BLOCK_CROSSED ((zpc2 < 0) || (zpc2 >= BLKSIZ))
#define G_HERE 16
#define G_SCORE 17
#define G_MOVES 18
#define G_HOURS 17
#define G_MINS 18
#define NORMAL 0
#define REVERSE 1
#define FMODE 0644		/* set most bits */
#define PATHSIZ 64
#define TO_K 10
#define NO_INPUT -1
#define THEN
#define NULL 0
#define RDONLY 0
#define NOT_IN_CORE 0
#define LOCAL 16
#define EMPTY 0
#define BYTEMSK 255
#define BIT16 32768
#define BACKWARD 128	/* bits to test */
#define JMPLNTH 64
#define PREDMSK 63
#define BIT14 0x2000	/* for testing 14 bit compness */
#define COMP16 0xC000	/* for converting above to 16 bit */
#define BLOCKBITS 0x7F
#define BYTEBITS 0x1FF
#define CVTBLK 9
#define BYTEMODE 0
#define ZCHRLEN 5
#define ZCHRMSK 31
#define CSETLEN 26
#define NO16TH 0xEFFF
#define ZTRUE 1
#define ZFALSE 0
#define ZQUIT -1
#define ZRESTART 1
#define ONE_OP 128
#define ZERO_OP 176
#define EXT_OP 192
#define ZEROMSK 15
#define ONEMSK 15
#define TWOMSK 31
#define EXTMSK 63
#define TWOMOD1 64
#define TWOMOD2 32
#define ONEMODE 48

#define STATBIT 16
#define SPLTBIT 32

#define OFF 0
#define ON 1
#define STEP 2
#define SKIPC 4
#define SKIPN 8
#define VERBOSE 16
#define BRKPT 32
#define SKIPS 0xF3

/*  M A C R O S  */

#define PRED(arg) ((arg) ? ppred(ZTRUE) : ppred(ZFALSE))
#define PUTV(arg) (putval(arg))

#define POPZ() (*zsp++)
#define PUSHZ(arg) (*--zsp = arg)

#define PUSH(arg) (*--ssp = arg)
#define POP() (*ssp++)

#define GTAWRD(offset) ((*(dataspace+offset) << 8) | (*(dataspace+offset+1) & 255)) 
#define GTABYT(offset) (*(dataspace + offset))
#define PTABYT(offset,value) (*(dataspace + offset) = value)
#define PTAWRD(offset,value) ((*(dataspace+offset) = (value >> 8)), (*(dataspace+offset+1) = (value & 255))) 
#define GETLOC(var) (*(zstack + (zlocs - var)))
#define SETLOC(var, val) (*(zstack + (zlocs - var)) = val) 
#define BYTARG(arg) ((ZOBJECT)(argblk[arg]))

#define RESET_CS tempcs = permcs
#define CHRS_PER_ZWORD 6
#define TOKEN_TBL_LEN 59
#define WORDS_LEFT (curoff <= numread)
