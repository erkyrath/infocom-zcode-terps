/************************************************************************
*									*
*	H E A D E R   F I L E   F O R   C   D I P			*
*									*
************************************************************************/

#define ETRAP 1		/* nonzero for "heavy duty" error trapping */

typedef unsigned short int ZIPINT;	/* FOR VIRTUAL ADDRESSES */
typedef unsigned char ZIPOBJ;		/* FOR USE WITH OBJECTS  (EZIP) */
typedef unsigned char ZIPBYT;		/* for general unsigned bytes */

/* We define a special type for virtual byte pointers which may exceed
   64K (16 bits).  These occur only in connection with icon definitions
   in the DIP image file. 
*/
typedef unsigned long int DIPADDR;	/* 32 bits preferably */

typedef struct {	/* [information supplied in each icon header: ] */
    ZIPBYT bset;	/* icon blockset */
    ZIPBYT iters;	/* number of iterations */
    ZIPBYT width;	/* icon size */
    ZIPBYT height;
    DIPADDR addr;	/* byte ptr to icon's data */
} iconinfo;

/*****  GRAPHICS DEFINITIONS  *****/

#define GBLEN 8		/* number of bytes per DIP graphics block */

#define SCRNX1 0
#define SCRNX2 40	/* screen width in blocks */
#define SCRNY1 0
#define SCRNY2 24	/* screen height in blocks */

#define DO_NEGATE 0xFF	/* inverts bits when XORed with target */
#define NO_NEGATE 0

#define NO_INPUT 0x8F	/* indicates that joystick is centered */

#define DELAYMAX 3000	/* for AT&T delay timing */
#define DELAYMIN 0
#define DELAYINIT 1000
#define DELAYDELTA 250

/* format of each entry in an Active Icon Table */

#define AI_ADDR 0	/* word ptr to icon */
#define AI_LOCX 2	/* horizontal position of icon (2 bytes) */
#define AI_LOCY 4	/* vertical position of icon (2 bytes) */
#define AI_NEGATE 6	/* negate flag */
#define AI_ICUR 7	/* current iteration */
#define AI_BSET 8	/* block set id */
#define AI_ITOT 9	/* total number of iterations */
#define AI_WIDTH 10	/* width of icon */
#define AI_HEIGHT 11	/* height of icon */
#define AI_ENTRY 12	/* total length of each entry */


/*****  FILE HEADER STRUCTURE (PROGRAM AND IMAGE) *****/

#define PVERS1 0	/* Z-MACHINE VERSION BYTE */
#define PVERS2 1	/* MODE BYTE */
#define PZRKID 2	/* GAME ID */
#define PENDLD 4	/* END OF GAME FILE PRELOAD (WORD PTR) */
#define PSTART 6	/* EXECUTION START ADDR	    (WORD PTR "ODD") */
#define PPURBT 8	/* BEGINNING OF PURE CODE   (WORD PTR) */
#define PGLOTB 10	/* GLOBAL TABLE 	    (WORD PTR) */
#define PSERNM 12	/* 6 BYTE SERIAL NUMBER (DATE) */
#define PLENTH 18	/* LENGTH OF GAME FILE, IN WORDS */
#define PCHKSM 20	/* CHECKSUM OF GAME FILE, EXCLUDING HEADER */
#define PINTID 22	/* INTERPRETER ID, LETTER & NUMBER */

#define HDRSIZ 64	/* LENGTH OF HEADER, PROGRAM FILE */

#define ILENTH	0	/* length of image file, in words */
#define IENDLD	2	/* end of preload, word ptr */
#define	ICHKSM	4	/* checksum of image file */
#define	IBSETS	6	/* total number of blocksets in the file */
#define	IICONS	7	/* total number of icons in the file */

#define IHEAD	8	/* LENGTH OF HEADER, IMAGE FILE */

#define IFUDGE	1	/* padding between code and image files, in words */

/*****  GENERAL DEFINITIONS  *****/

#define ZMVERS 1		/* DIP Z-MACHINE VERSION NUMBER */
#define LSTACK 512		/* VIRTUAL STACK SIZE  [in words (?)] */
#define STKLEN 64		/* SYSTEM STACK SIZE [in longs (?)] */

#define BLKSIZ 512		/* SIZE OF VIRTUAL PAGES */
#define MAXBLKS 256		/* MAXIMUM GAME SIZE */

#define ZTRUE 1
#define ZFALSE 0
#define MAXARGS 16+1		/* 4 normally, 16 for OPCALL, 1 for count */

#define SCRIPTBIT 1
#define STATBIT 16
#define SPLTBIT 32

#define THEN			/* if <cond> THEN <exp>; else <exp>; */
#define NULL 0

#define G_HERE 16		/* ZIL status-line globals */
#define G_SCORE 17
#define G_MOVES 18
#define G_HOURS 17
#define G_MINS 18

/*****  CHAR INPUT AND STRING OUTPUT  *****/

#define NULL_INPUT -1
#define Z_EOL 1
#define EOL 10			/* LINE FEED IS THE UNIX EOL */
#define ESC 27
#define FEEP "\007"		/* BELL */
#define BKSPC 8			/* BACKSPACE */
#define PADCHR 5		/* PAD CHAR FOR ZSTRINGS */
#define SPACE '\040'		/* INITIAL READ BREAD CHARS */
#define TAB '\011'
#define CR '\015'
#define FF '\014'
#define PERIOD '.'
#define COMMA ','
#define QMARK '?'

#define CHRS_PER_ZWORD 6
#define TOKEN_TBL_LEN 59
#define BYTEMODE 0
#define ZCHRLEN 5
#define ZCHRMSK 31
#define CSETLEN 26

#define NORMAL 0
#define REVERSE 1

/*****  BUFFERING EQUATES  *****/

#define IBUFSIZ 100
#define OBUFSIZ 100

#define CONSOLE 0
#define PIPE 1
#define PBUFSIZ 85
#define PIPEWIDTH 79

#define STATLEN 1		/* STATUS LINE ROWS */

#define RM 80			/* right margin */
#define LM 1			/* left margin */

/*****  OBJECT AND PROPERTY DEFINITIONS  *****/

#define FLAGS1 0		/* OFFSET TO FLAG WORDS */
#define FLAGS2 2
#define PARENT 4		/* PARENT OBJECT NUMBER */
#define SIBLING 5		/* SIBLING OJECT NUMBER */
#define CHILD 6			/* CHILD OBJECT NUMBER */
#define PROPS 7			/* POINTER TO PROPERTY TABLE */

#define OBJLEN 9		/* LENGTH OF AN OBJECT */
#define DPTLEN (31 * 2)		/* DEFAULT PROP TABLE LENGTH, 31 ENTRIES */

#define PNUMSK 0x1F		/* PROPERTY ID NUMBER MASK (LOW 5 BITS) */
#define PSZMSK 0xE0		/* PROPERTY SIZE MASK (HIGH 3 BITS) */
#define PROPSIZE 5		/* SHIFT COUNT TO ISOLATE SIZE BITS */

/*****  PREDICATE AND VARIABLE DEFINITIONS  *****/

#define LOCAL 16
#define EMPTY 0
#define BYTEMSK 255
#define BIT8 0x80
#define BIT16 0x8000
#define BACKWARD 128	/* bits to test */
#define JMPLNTH 64
#define PREDMSK 63
#define BIT14 0x2000	/* for testing 14 bit two's-complement */
#define COMP16 0xC000	/* for converting above to 16 bit */
#define BITS87 0xC0	/* for OPCALL decoding */

/*****  PAGING DEFINITIONS  *****/

#define NOT_IN_CORE 0		/* a NULL pointer */
#define NO_PAGE -1		/* avoid conflict with page 0 */
#define BLOCKBITS 0x7F
#define BYTEBITS 0x1FF
#define CVTBLK 9
#define TO_K 10

/*****  FILE I/O DEFINITIONS  *****/

#define PATHSIZ 64
#define RDONLY 0
#define FMODE 0644		/* NEW FILE, set most bits */

/******  DEBUGGING DEFINITIONS  ******/

#define OFF 0
#define ON 1
#define STEP 2
#define SKIPC 4
#define SKIPN 8
#define VERBOSE 16
#define BRKPT 32
#define SKIPS 0xF3

#define HEX 'x'
#define DEC 'd'

/*****  MACROS  *****/

#define PRED(arg) ((arg) ? ppred(ZTRUE) : ppred(ZFALSE))
#define PUTV(arg) (putval(arg))

#define PUSH(arg) (*--ssp = arg)
#define POP() (*ssp++)
#define PUSHZ(arg) (*--zsp = arg)
#define POPZ() (*zsp++)

#define BYTARG(arg) ((ZIPBYT)(argblk[arg]))
#define GETLOC(var) (*(zstack + (zlocs - var)))
#define SETLOC(var, val) (*(zstack + (zlocs - var)) = val) 

#define GTABYT(ptr) (*(ptr))	/* ptr may be a compound */
#define GTVBYT(off) (*(dataspace + off))

#define PTABYT(ptr,value) (*(ptr) = value)
#define PTVBYT(off,value) (*(dataspace+off) = value)

/*	MOVED INTO PROGRAM SUBROUTINES:

#define GTAWRD(ptr) ((((ZIPBYT) *ptr) << 8) | (ZIPBYT) *(ptr+1))
#define GTVWRD(off) ((((ZIPBYT) *(dataspace+off)) << 8) | (ZIPBYT) *(dataspace+off+1))

#define PTAWRD(ptr,value) ((*ptr = (value >> 8)), (*(ptr+1) = (value & 255))) 
#define PTVWRD(off,value) ((*(dataspace+off) = (value >> 8)), (*(dataspace+off+1) = (value & 255))) 
*/

/*****  OPCODES  *****/

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

/* ZERO OPS */

#define OPNOOP	176
#define OPRTRU	177
#define OPRFAL	178
#define OPRSTA	179
#define OPFSTA	180
#define OPQUIT	181
#define OPCOPY	182
#define OPVERI	183

/* ONE OPS */

#define OPPUSH	128
#define OPPOP	129
#define OPVALU	130
#define OPINC	131
#define OPDEC	132
#define OPQZER	133
#define OPBCOM	134	/* "BNOT" */
#define OPJUMP	135
#define OPRETU	136

/* TWO OPS AND EXTENDED OPS */

#define OPUNDF	0	/* UNUSED */
#define OPADD	1
#define OPSUB	2
#define OPMUL	3
#define OPDIV	4
#define OPMOD	5
#define OPBAND	6
#define OPBOR	7
#define OPBXOR	8
#define OPBTST	9	/* "BITS" */
#define OPQEQU	10
#define OPQLES	11
#define OPQDLE	12
#define OPQGRT	13
#define OPQIGR	14
#define OPSET	15
#define OPGET	16
#define OPGETB	17
#define LAST_TWO_OP OPGETB

#define XQEQU	EXT_OP + OPQEQU
#define OPCALL	224
#define OPPUT	225
#define OPPUTB	226
#define OPINPUT	227
#define OPSHOWI	228
#define OPSETI	229
#define OPSWAPI	230
#define OPSOUND	231
#define OPRAND	232
#define OPCLEAR	233
#define OPSHOWN	234
#define OPWIND	235
#define OPITER	236
#define OPLOAD	237
#define OPDUMP	238
#define OPREST	239
#define OPSAVE	240

