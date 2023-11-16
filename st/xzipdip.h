
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


