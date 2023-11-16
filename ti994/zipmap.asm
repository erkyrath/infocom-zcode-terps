
******************************************************
* Z-LANGUAGE INTERPRETER PROGRAM FOR THE TI-99       *
******************************************************

* REGISTER USAGE
* ----------------------------------------------------------
*   R0 - GENERALLY, SINGLE VALUES ARE PASSED OR RETURNED
*   HERE.  OTHERWISE, R0 IS SCRATCH.

*   R1 THROUGH R4 - USED FOR PASSING MULTIPLE VALUES. 
*   OTHERWISE, THESE REGISTERS MUST BE PRESERVED ACROSS EACH
*   SUBROUTINE CALL.  TOP-LEVEL ROUTINES, OPx ROUTINES, ARE
*   EXCLUDED FROM THIS RESTRICTION.

*   R5 AND R6 - VIRTUAL STACK AND ZIP STACK POINTERS.

*   R7 THROUGH R15 - SCRATCH.  REGISTERS FROM R11 UP ARE
*   USED TRANSIENTLY BY "BL" AND "BLWP" SUBROUTINE CALLS.


* MEMORY USAGE
* ----------------------------------------------------------
* PRELOADED GAME CODE: >A000 TO >F400
*   SINCE THERE IS ROOM FOR EXACTLY 44 BLOCKS HERE, "ENDLOD"
*   FOR ANY GIVEN GAME MUST NOT EXCEED 44 x 512 (OR THE ZIP
*   INITIALIZER WILL SIGNAL AN ERROR).

* STACK AND BUFFER SPACE: >2000 TO >2700
*   THIS SPACE IS INITIALLY OCCUPIED BY THE SYSTEM LOADER 
*   BUT BECOMES AVAILABLE ONCE ZIP IS RUNNING.

* ZIP: >2700 T0 >3F30, >F100 TO >FF80
*   ZIP WAS COMPILED INTO TWO SECTIONS TO ALLOW PRELOADED
*   GAME CODE TO OCCUPY CONTIGUOUS MEMORY.  THE UPPER LIMITS
*   ARE DUE TO THE SYSTEM LOADER'S SYMBOL TABLE, STARTING
*   AT ABOUT >3F30, AND THE X-BASIC PROGRAM WHICH CALLS THE
*   SYSTEM LOADER, STARTING AT ABOUT >FF80.
*
*   BECAUSE MEMORY SPACE IS VERY TIGHT IN THE TI, INITIAL-
*   IZATION CODE IS LOCATED AT THE FRONT OF ZIP AND COMPILED
*   SUCH THAT IT OVERLAPS THE AREA OF PRELOADED GAME CODE.
*   IT NEED EXECUTE ONLY ONCE, AND IS THEN OVERWRITTEN. 
*   THIS TECHNIQUE INCREASES PRELOAD SPACE BY TWO BLOCKS.

* PAGE SWAPPING SPACE IS LOCATED IN THE SEPARATE VDP (VIDEO
*   DISPLAY PROCESSOR) MEMORY.  ABOUT 4K (25 PERCENT) OF VDP 
*   IS OCCUPIED BY DISK UTILITIES AND VIDEO DISPLAY TABLES.
*   JUDICIOUS ARRANGEMENT OF THE TABLES LEAVES ROOM FOR
*   EXACTLY 24 PAGE FRAMES.

        PAGE -----------------------------------------------

*   VDP MEMORY IS THE BEST CHOICE FOR THE LOCATION OF PAGE
*   FRAMES BECAUSE OF ITS FOLLOWING PROPERTIES/LIMITATIONS:
*
*   []  DATA READ FROM THE DISK IS ALWAYS DEPOSITED DIRECTLY
*       INTO VDP MEMORY.  A SPECIAL SUBROUTINE CALL IS
*       NECESSARY TO GET THIS DATA, ONE BYTE AT A TIME, TO A
*       PROGRAM IN MAIN MEMORY.  NOTE THAT THIS CORRESPONDS
*       CLOSELY TO THE WAY IN WHICH ZIP IS DESIGNED TO READ
*       PAGED DATA.
*
*   []  VDP MEMORY IS NOT WITHIN THE ADDRESS SPACE OF THE 
*       CPU; ASSEMBLY LANGUAGE PROGRAMS (E.G. ZIP) CANNOT
*       THEMSELVES BE EXECUTED FROM VDP MEMORY.


* SUBROUTINE LINKAGE
* ----------------------------------------------------------
*   SINCE THE TI-99 DOES NOT SUPPORT STACK-BASED SUBROUTINE
*   LINKAGE, ZIP INCLUDES A SUBROUTINE (JSR) WHICH EMULATES
*   THE PDP-11 SUBROUTINE CALL OF THE SAME NAME.  THE TI'S
*   INTRINSIC REGISTER-BASED LINKAGE INSTRUCTIONS ARE USED 
*   OCCASIONALLY FOR CALLING SYSTEM UTILITIES.

    PAGE ---------------------------------------------------

*********************************
* VDP MEMORY MAP                *
*********************************

* >0000  +-----------------------------------------------+
*        |  Script buffer - 40 chars max                 |
* >0030  +-----------------------------------------------+
*        |  Peripheral Access Blocks, free space         |
*        |                                               |
* >0100  +-----------------------------------------------+
*        |  Pattern Descriptor Table - 96 chars x 8      |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
* >0400  +-----------------------------------------------+
*        |  Screen Image Table - 40 cols x 24 rows       |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
* >07C0  +-----------------------------------------------+
* >07D0  +-----------------------------------------------+
*        |  Page Swapping Space - 24 pages x 512         |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*          
*        ~                                               ~
* 
*        |                                               |
*        |                                               |
* >37D0  +-----------------------------------------------+
* >37D8  +-----------------------------------------------+
*        |  Reserved by System: Disk I/O                 |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
* >3FFF  +-----------------------------------------------+

    PAGE -------------------------------------------------

*********************************
* MAIN MEMORY MAP               *
*********************************

* >2000  +-----------------------------------------------+
*        |  ZIP stacks, buffers                          |
*        |                                               |
*        |                                               |
* >2700  +-----------------------------------------------+
*        |  ZIP                                          |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
* >3F30  +-----------------------------------------------+
*        |  System REF/DEF Table (program entry address) |
*        |                                               |
* >3FFF  +-----------------------------------------------+





* >A000  +-----------------------------------------------+
*        |  Preloaded game code                          |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*        |                                               |
*
*        ~                                               ~
*
*        |                                               |
*        |                                               |
*        |                                               |
* >xxxx  +-----------------------------------------------+
*        | more ZIP                                      |
*        |                                               |
*        |                                               |
*        |                                               |
* >FF80  +-----------------------------------------------+
*        | Extended Basic Line Table                     |
* >FFFF  +-----------------------------------------------+

