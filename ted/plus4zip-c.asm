

AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   1    


                
 0000                   ; --------------------------
 0000                   ; ZIP/6502 2.0 VERSION C
 0000                   ; Z-CODE INTERPRETER PROGRAM
 0000                   ; FOR CBM PLUS/4
 0000                   ; --------------------------
                
 0000                   ; INFOCOM, INC.
 0000                   ; 55 WHEELER STREET
 0000                   ; CAMBRIDGE, MA 02136
                
 0000                   ; COMPANY PRIVATE -- NOT FOR DISTRIBUTION
                
 1000           MSTART  EQU     $1000           ; START OF FREE PROGRAM RAM
 0003           ZEROPG  EQU     $03             ; START OF FREE Z-PAGE RAM
 007F           ZPGTOP  EQU     $7F             ; END OF FREE Z-PAGE RAM
                
 0000           DEBUG   EQU     0               ; ASSEMBLY FLAG FOR DEBUGGER
                
 0000                   ; -----------
 0000                   ; ERROR CODES
 0000                   ; -----------
                
 0000                   ; 00 -- INSUFFICIENT RAM
 0000                   ; 01 -- ILLEGAL X-OP
 0000                   ; 02 -- ILLEGAL 0-OP
 0000                   ; 03 -- ILLEGAL 1-OP
 0000                   ; 04 -- ILLEGAL 2-OP
 0000                   ; 05 -- Z-STACK UNDERFLOW
 0000                   ; 06 -- Z-STACK OVERFLOW
 0000                   ; 07 -- ILLEGAL PROPERTY LENGTH (GETP)
 0000                   ; 08 -- DIVISION BY ZERO
 0000                   ; 09 -- ILLEGAL ARGUMENT COUNT (EQUAL?)
 0000                   ; 10 -- ILLEGAL PROPERTY ID (PUTP)
 0000                   ; 11 -- ILLEGAL PROPERTY LENGTH (PUTP)
 0000                   ; 12 -- DISK ADDRESS OUT OF RANGE
 0000                   ; 13 -- PARSER OVERFLOW
 0000                   ; 14 -- DRIVE ACCESS
                
                        INCLUD EQ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   2    
--- MEMORY ORGANIZATION ---

                
 00FF           TRUE    EQU     $FF
 0000           FALSE   EQU     0
 0000           LO      EQU     0
 0001           HI      EQU     1
                
 1000           IOBUFF  EQU     MSTART          ; 256-BYTE DISK BUFFER
 1100           ZSTAKL  EQU     MSTART+$100     ; Z-STACK LSBS
 1200           ZSTAKH  EQU     MSTART+$200     ; Z-STACK MSBS
 1300           PTABL   EQU     MSTART+$300     ; PAGING TABLE LSBS ($A0 BYTES)
 13A0           PTABH   EQU     MSTART+$3A0     ; PAGING TABLE MSBS ($A0 BYTES)
 1450           LRUMAP  EQU     MSTART+$450     ; TIMESTAMP MAP ($A0 BYTES) (BM 3/7/85)
 1500           LOCALS  EQU     MSTART+$500     ; LOCAL VARIABLE STORAGE (32 BYTES)
 1530           LBUFF   EQU     MSTART+$530     ; LINE INPUT BUFFER (80 BYTES)
 1590           BUFSAV  EQU     MSTART+$590     ; I/O AUX BUFFER (40 BYTES)
                
 1600           ZIP     EQU     MSTART+$600     ; START OF EXECUTABLE CODE
 3200           ZBEGIN  EQU     ZIP+$1C00       ; START OF Z-CODE (ASSUME 7K ZIP)
                
 0000                   ; ---------------------
 0000                   ; Z-CODE HEADER OFFSETS
 0000                   ; ---------------------
                
 0000           ZVERS   EQU     0               ; VERSION BYTE
 0001           ZMODE   EQU     1               ; MODE SELECT BYTE
 0002           ZID     EQU     2               ; GAME ID WORD
 0004           ZENDLD  EQU     4               ; START OF NON-PRELOADED Z-CODE
 0006           ZGO     EQU     6               ; EXECUTION ADDRESS
 0008           ZVOCAB  EQU     8               ; START OF VOCABULARY TABLE
 000A           ZOBJEC  EQU     10              ; START OF OBJECT TABLE
 000C           ZGLOBA  EQU     12              ; START OF GLOBAL VARIABLE TABLE
 000E           ZPURBT  EQU     14              ; START OF "PURE" Z-CODE
 0010           ZSCRIP  EQU     16              ; FLAG WORD
 0012           ZSERIA  EQU     18              ; 3-WORD ASCII SERIAL NUMBER
 0018           ZFWORD  EQU     24              ; START OF FWORDS TABLE
 001A           ZLENTH  EQU     26              ; LENGTH OF Z-PROGRAM IN WORDS
 001C           ZCHKSM  EQU     28              ; Z-CODE CHECKSUM WORD
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   3    
--- ZIP Z-PAGE VARIABLES ---

                
 0003           OPCODE  EQU     ZEROPG          ; (BYTE) CURRENT OPCODE
 0004           NARGS   EQU     OPCODE+1        ; (BYTE) # ARGUMENTS
 0005           ARG1    EQU     OPCODE+2        ; (WORD) ARGUMENT #1
 0007           ARG2    EQU     OPCODE+4        ; (WORD) ARGUMENT #2
 0009           ARG3    EQU     OPCODE+6        ; (WORD) ARGUMENT #3
 000B           ARG4    EQU     OPCODE+8        ; (WORD) ARGUMENT #4
 000D           ABYTE   EQU     OPCODE+10       ; (BYTE) X-OP ARGUMENT BYTE
 000E           ADEX    EQU     OPCODE+11       ; (BYTE) X-OP ARGUMENT INDEX
                
 000F           VALUE   EQU     OPCODE+12       ; (WORD) VALUE RETURN REGISTER
 0011           I       EQU     VALUE+2         ; (WORD) GEN-PURPOSE REGISTER #1
 0013           J       EQU     VALUE+4         ; (WORD) GEN-PURPOSE REGISTER #2
 0015           K       EQU     VALUE+6         ; (WORD) GEN-PURPOSE REGISTER #3
                
 0017           ZSP     EQU     VALUE+8         ; (BYTE) Z-STACK POINTER
 0018           OLDZSP  EQU     ZSP+1           ; (BYTE) OLD Z-STACK POINTER
                
 0019           ZPC     EQU     ZSP+2           ; (3 BYTES) ZIP PROGRAM COUNTER
 0019           ZPCL    EQU     ZPC             ; (BYTE) LOW 8 BITS OF [ZPC]
 001A           ZPCM    EQU     ZPC+1           ; (BYTE) MIDDLE 8 BITS OF [ZPC]
 001B           ZPCH    EQU     ZPC+2           ; (BYTE) HIGH BIT OF [ZPC]
 001C           ZPCFLG  EQU     ZPC+3           ; (BYTE) FLAG: "TRUE" IF [ZPCPNT] VALID
 001D           ZPCPNT  EQU     ZPC+4           ; (WORD) ABS POINTER TO CURRENT Z-PAGE
                
 001F           MPC     EQU     ZPC+6           ; (3 BYTES) MEMORY PROGRAM COUNTER
 001F           MPCL    EQU     MPC             ; (BYTE) LOW 8 BITS OF [MPC]
 0020           MPCM    EQU     MPC+1           ; (BYTE) MIDDLE 8 BITS OF [MPC]
 0021           MPCH    EQU     MPC+2           ; (BYTE) HIGH BIT OF [MPC]
 0022           MPCFLG  EQU     MPC+3           ; (BYTE) FLAG: "TRUE" IF [MPCPNT] VALID
 0023           MPCPNT  EQU     MPC+4           ; (WORD) ABS POINTER TO CURRENT M-PAGE
                
 0025           LRU     EQU     MPC+6           ; (BYTE) PAGING INDEX
 0026           ZCODE   EQU     LRU+1           ; (BYTE) 1ST ABSOLUTE PAGE OF PRELOAD
 0027           ZPURE   EQU     LRU+2           ; (BYTE) 1ST VIRTUAL PAGE OF "PURE" Z-CODE
 0028           PAGE0   EQU     LRU+3           ; (BYTE) 1ST PAGE OF ACTUAL SWAPPING SPACE
 0029           PMAX    EQU     LRU+4           ; (BYTE) MAXIMUM # OF SWAPPING PAGES
 002A           ZPAGE   EQU     LRU+5           ; (BYTE) CURRENT SWAPPING PAGE
 002B           TARGET  EQU     LRU+6           ; (WORD) TARGET PAGE FOR SWAPPING
 002D           STAMP   EQU     LRU+8           ; (BYTE) CURRENT TIMESTAMP (BM 3/8/85)
 002E           SWAP    EQU     LRU+9           ; (BYTE) EARLIEST PAGE (BM 3/8/85)
                
 002F           GLOBAL  EQU     LRU+10          ; (WORD) GLOBAL VARIABLE POINTER
 0031           VOCAB   EQU     GLOBAL+2        ; (WORD) VOCAB TABLE POINTER
 0033           FWORDS  EQU     GLOBAL+4        ; (WORD) F-WORDS TABLE POINTER
 0035           OBJTAB  EQU     GLOBAL+6        ; (WORD) OBJECT TABLE POINTER
                
 0000                   ; Z-STRING MANIPULATION VARIABLES
                
 0037           IN      EQU     GLOBAL+8        ; (6 BYTES) INPUT BUFFER
 003D           OUT     EQU     IN+6            ; (6 BYTES) OUTPUT BUFFER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   4    
--- ZIP Z-PAGE VARIABLES ---

                
 0043           SOURCE  EQU     OUT+6           ; (BYTE) SOURCE BUFFER POINTER
 0044           RESULT  EQU     SOURCE+1        ; (BYTE) RESULT TABLE POINTER
 0045           LINLEN  EQU     SOURCE+2        ; (BYTE) LENGTH OF CURRENT LINE
 0046           WRDLEN  EQU     SOURCE+3        ; (BYTE) LENGTH OF CURRENT WORD
 0047           ENTRY   EQU     SOURCE+4        ; (WORD) ADDR OF CURRENT RESULT ENTRY
 0049           NENTS   EQU     SOURCE+6        ; (WORD) # ENTRIES IN VOCAB TABLE
 004B           ESIZE   EQU     SOURCE+8        ; (BYTE) SIZE OF VOCAB TABLE ENTRIES
 004C           PSET    EQU     SOURCE+9        ; (BYTE) PERMANENT CHARSET
 004D           TSET    EQU     SOURCE+10       ; (BYTE) TEMPORARY CHARSET
 004E           ZCHAR   EQU     SOURCE+11       ; (BYTE) CURRENT Z-CHAR
 004F           OFFSET  EQU     SOURCE+12       ; (BYTE) F-WORD TABLE OFFSET
 0050           ZFLAG   EQU     SOURCE+13       ; (BYTE) Z-WORD ACCESS FLAG
 0051           ZWORD   EQU     SOURCE+14       ; (WORD) CURRENT Z-WORD
 0053           CONCNT  EQU     SOURCE+16       ; (BYTE) Z-STRING SOURCE COUNTER
 0054           CONIN   EQU     SOURCE+17       ; (BYTE) CONVERSION SOURCE INDEX
 0055           CONOUT  EQU     SOURCE+18       ; (BYTE) CONVERSION DEST INDEX
                
 0000                   ; MATH PACKAGE VARIABLES
                
 0056           QUOT    EQU     SOURCE+19       ; (WORD) QUOTIENT FOR DIVISION
 0058           REMAIN  EQU     QUOT+2          ; (WORD) REMAINDER FOR DIVISION
 005A           MTEMP   EQU     QUOT+4          ; (WORD) MATH TEMPORARY REGISTER
 005C           QSIGN   EQU     QUOT+6          ; (BYTE) SIGN OF QUOTIENT
 005D           RSIGN   EQU     QUOT+7          ; (BYTE) SIGN OF REMAINDER
 005E           DIGITS  EQU     QUOT+8          ; (BYTE) DIGIT COUNT FOR "PRINTN"
                
 005F           TIMEFL  EQU     QUOT+9          ; (BYTE) "TRUE" IF TIME MODE
 0060           LENGTH  EQU     TIMEFL+1        ; (BYTE) LENGTH OF LINE IN [LINBUF]
 0061           OLDLEN  EQU     TIMEFL+2        ; (BYTE) OLD LINE LENGTH
 0062           SCRIPT  EQU     TIMEFL+3        ; (BYTE) SCRIPT ENABLE FLAG
 0063           OLDX    EQU     TIMEFL+4        ; (BYTE) OLD CURSOR X
 0064           OLDY    EQU     TIMEFL+5        ; (BYTE) OLD CURSOR Y
 0065           LINCNT  EQU     TIMEFL+6        ; (BYTE) LINE COUNTER
 0066           LMAX    EQU     TIMEFL+7        ; (BYTE) MAX # LINES/SCREEN
                
 0067           IOCHAR  EQU     TIMEFL+8        ; (BYTE) CHARACTER BUFFER
 0068           SLINE   EQU     IOCHAR+1        ; (BYTE) BORDERLINE FOR SPLIT
 0069           SPSTAT  EQU     IOCHAR+2        ; (BYTE) SPLIT SCREEN STATUS FLAG
 006A           PSTAT   EQU     IOCHAR+3        ; (BYTE) PRINTER STATUS FLAG
 006B           PRLEN   EQU     IOCHAR+4        ; (BYTE) SCRIPT LINE LENGTH
 006C           LFROM   EQU     IOCHAR+5        ; (WORD) SCROLLING INDEX #1
 006E           LTO     EQU     IOCHAR+7        ; (WORD) SCROLLING INDEX #2
                
 0070           DBLOCK  EQU     IOCHAR+9        ; (WORD) Z-BLOCK TO READ
 0072           DBUFF   EQU     DBLOCK+2        ; (WORD) RAM PAGE TO ACCESS (LSB = 0)
 0074           TRACK   EQU     DBLOCK+4        ; (BYTE) TARGET TRACK
 0075           SECTOR  EQU     DBLOCK+5        ; (BYTE) TARGET SECTOR
 0076           GPOSIT  EQU     DBLOCK+6        ; (BYTE) DEFAULT SAVE POSITION
 0077           GDRIVE  EQU     DBLOCK+7        ; (BYTE) DEFAULT SAVE DRIVE
 0078           TPOSIT  EQU     DBLOCK+8        ; (BYTE) TEMP SAVE POSITION


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   5    
--- ZIP Z-PAGE VARIABLES ---

 0079           TDRIVE  EQU     DBLOCK+9        ; (BYTE) TEMP SAVE DRIVE
 007A           DRIVE   EQU     DBLOCK+10       ; (BYTE) CURRENT DRIVE
                
 007B           DVD     EQU     DBLOCK+11       ; (WORD) DISK DIVIDEND
 007D           DSOR    EQU     DVD+2           ; (WORD) DISK DIVISOR
 007F           DTEMP   EQU     DVD+4           ; (WORD) DISK TEMP VARIABLE
                
                        END
                
                        INCLUD HARDEQ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   6    
--- HARDWARE EQUATES: CBM PLUS/4 ---

                
 0000                   ; ----------------------------------
 0000                   ; MACHINE-DEPENDENT SOFTWARE EQUATES
 0000                   ; ----------------------------------
                
 0002           FAST    EQU     $02             ; (BYTE) FAST-READ AVAILABLE FLAG
                
 00D8           CSHAPE  EQU     $D8             ; (BYTE) CURRENT CURSOR SHAPE
 00D9           COLUMN  EQU     $D9             ; (BYTE) CURRENT CURSOR COLUMN
 00DA           SROW    EQU     $DA             ; (WORD) CURRENT SCREEN ROW ADDR
 00DC           CROW    EQU     $DC             ; (WORD) CURRENT COLOR ROW ADDR
 00DE           BLINK   EQU     $DE             ; (WORD) BLINK TIMER
                
 00E0           FINDEX  EQU     $E0             ; (BYTE) FAST-READ INDEX
 00E1           FDATA   EQU     $E1             ; (BYTE) FAST-READ DATA BUFFER
 00E2           FASTEN  EQU     $E2             ; (BYTE) FAST-READ ENABLE FLAG
                
 0000                   ; ---------------------------
 0000                   ; CBM PLUS/4 HARDWARE EQUATES
 0000                   ; ---------------------------
                
 0800           COLRAM  EQU     $0800           ; COLOR (ATTRIBUTE) RAM
 0C00           SCREEN  EQU     $0C00           ; SCREEN RAM
                
 0027           XSIZE   EQU     39              ; X-SIZE OF SCREEN
 0018           YSIZE   EQU     24              ; Y-SIZE OF SCREEN
                
 000D           EOL     EQU     $0D             ; EOL CHAR
 0020           SPACE   EQU     $20             ; SPACE CHAR
 0014           BACKSP  EQU     $14             ; BACKSPACE
 0051           WHITE   EQU     $51             ; WHITE COLOR FOR TEXT
                
 0022           FADDR   EQU     $22             ; FUNCTION KEY STRING ADDRESS
 0076           FKEY    EQU     $76             ; FUNCTION KEY ID
 00A5           TIME    EQU     $A5             ; SYSTEM JIFFY CLOCK
 00EF           NDX     EQU     $EF             ; # CHARS IN KEYBOARD QUEUE
 053B           COLOR   EQU     $053B           ; TEXT COLOR
 053C           BLINKA  EQU     $053C           ; BLINK ATTRIBUTE ($80 = BLINK)
 0543           SHFLAG  EQU     $0543           ; SHIFT KEY FLAG
 07E9           LINKEN  EQU     $07E9           ; LINE LINK ENABLE (BIT 6)
                
 0000                   ; ---
 0000                   ; TED
 0000                   ; ---
                
 FF00           TED     EQU     $FF00           ; START OF TED CHIP
 FF00           T1LSB   EQU     TED             ; TIMER #1 LSB
 FF01           T1MSB   EQU     TED+1           ; TIMER #1 MSB
 FF02           T2LSB   EQU     TED+2           ; TIMER #2 LSB
 FF03           T2MSB   EQU     TED+3           ; TIMER #2 MSB
 FF04           T3LSB   EQU     TED+4           ; TIMER #3 LSB


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   7    
--- HARDWARE EQUATES: CBM PLUS/4 ---

 FF05           T3MSB   EQU     TED+5           ; TIMER #3 MSB
 FF06           VSCROL  EQU     TED+6           ; V-SCROLL, DISPLAY CTRL
 FF07           HSCROL  EQU     TED+7           ; H-SCROLL, DISPLAY CTRL
 FF08           KEYLAT  EQU     TED+8           ; KEYBOARD LATCH
 FF09           INTREG  EQU     TED+9           ; INTERRUPT FLAGS
 FF0A           IMASK   EQU     TED+10          ; INTERRUPT MASK
 FF0B           RASTER  EQU     TED+11          ; RASTER LSB
 FF0C           CURSH   EQU     TED+12          ; CURSOR POSITION MSB
 FF0D           CURSL   EQU     TED+13          ; CURSOR POSITION LSB
 FF0E           V1FLSB  EQU     TED+14          ; VOICE #1 FREQ LSB
 FF0F           V2FLSB  EQU     TED+15          ; VOICE #2 FREQ LSB
 FF10           V2FMSB  EQU     TED+16          ; VOICE #2 FREQ MSB
 FF11           VOLUME  EQU     TED+17          ; VOLUME CTRL/VOICE SELECT
 FF12           BITMAP  EQU     TED+18          ; BITMAP BASE ADDR/VOICE #1 MSB
 FF13           CHBASE  EQU     TED+19          ; CHAR BASE ADDR
 FF14           SBASE   EQU     TED+20          ; SCREEN BASE ADDR
 FF15           BGCOL0  EQU     TED+21          ; BACKGROUND COLOR #0
 FF16           BGCOL1  EQU     TED+22          ; BACKGROUND COLOR #1
 FF17           BGCOL2  EQU     TED+23          ; BACKGROUND COLOR #2
 FF18           BGCOL3  EQU     TED+24          ; BACKGROUND COLOR #3
 FF19           BORDER  EQU     TED+25          ; BORDER COLOR
 FF1A           BMRMSB  EQU     TED+26          ; BIT MAP RELOAD MSB
 FF1B           BMRLSB  EQU     TED+27          ; BIT MAP RELOAD LSB
 FF1C           VLINEH  EQU     TED+28          ; VERTICAL LINE MSB
 FF1D           VLINEL  EQU     TED+29          ; VERTICAL LINE LSB
 FF1E           HSCAN   EQU     TED+30          ; HORIZONTAL LINE MSB
 FF1F           BLINRT  EQU     TED+31          ; BLINK RATE/VERTICAL SUBADDR
                
 FF3E           ROMIN   EQU     TED+62          ; ROM ENABLE
 FF3F           ROMOUT  EQU     TED+63          ; ROM DISABLE
                
 0000                   ; -------------------
 0000                   ; KERNAL JUMP VECTORS
 0000                   ; -------------------
                
 FFC6           CHKIN   EQU     $FFC6           ; OPEN CHANNEL FOR INPUT
 FFC9           CHKOUT  EQU     $FFC9           ; OPEN CHANNEL FOR OUTPUT
 FFCF           CHRIN   EQU     $FFCF           ; INPUT CHARACTER FROM CHANNEL
 FFD2           CHROUT  EQU     $FFD2           ; OUTPUT CHARACTER TO CHANNEL
 FFE7           CLALL   EQU     $FFE7           ; CLOSE ALL CHANNELS & FILES
 FFC3           CLOSE   EQU     $FFC3           ; CLOSE A FILE
 FFCC           CLRCHN  EQU     $FFCC           ; CLEAR CHANNEL
 FFE4           GETIN   EQU     $FFE4           ; GET CHAR FROM KEYBOARD QUEUE
 FFC0           OPEN    EQU     $FFC0           ; OPEN A FILE
 FFF0           PLOT    EQU     $FFF0           ; READ/SET CURSOR POSITION
 FFB7           READST  EQU     $FFB7           ; READ I/O STATUS
 FFBA           SETLFS  EQU     $FFBA           ; SET FILE ATTRIBUTES
 FF90           SETMSG  EQU     $FF90           ; SET KERNAL MESSAGES
 FFBD           SETNAM  EQU     $FFBD           ; SET FILENAME
 FF49           FUNCT   EQU     $FF49           ; DEFINE FUNCTION KEYS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   8    
--- HARDWARE EQUATES: CBM PLUS/4 ---

                        END
                        INCLUD COLD.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   9    
--- MACHINE COLDSTART: CBM PLUS/4 ---

                
 1600                   ORG     ZIP
                
 1600                   ; ---------
 1600                   ; COLDSTART
 1600                   ; ---------
                
 1600 78        COLD:   SEI                     ; DISABLE INTERRUPTS
                
 1601 A906              LDA     #6              ; DISABLE FUNCTION KEYS 7-1
 1603 8576              STA     FKEY            ; LEAVE "HELP" (F8) ALONE
 1605 A900      COLD0:  LDA     #0
 1607 2049FF            JSR     FUNCT
 160A C676              DEC     FKEY
 160C A576              LDA     FKEY
 160E 10F5              BPL     COLD0
                
 1610 A9C0              LDA     #$C0            ; COPY $C000-$FCFF TO RAM
 1612 8512              STA     I+HI            ; SET UP MSB
 1614 AA                TAX                     ; INIT PAGE COUNTER
                
 1615 A900              LDA     #0              ; SET UP LSB
 1617 8511              STA     I+LO
 1619 A8                TAY                     ; INIT PAGE INDEX
                
 161A B111      COLD1:  LDA     (I),Y           ; COPY ROM
 161C 9111              STA     (I),Y           ; INTO RAM
 161E C8                INY
 161F D0F9              BNE     COLD1
 1621 E612              INC     I+HI            ; NEXT PAGE
 1623 E8                INX
 1624 E0FD              CPX     #$FD
 1626 90F2              BCC     COLD1
                
 1628 A9FF              LDA     #$FF            ; COPY $FF40-$FFFF
 162A 8512              STA     I+HI
 162C A040              LDY     #$40
 162E B111      COLD2:  LDA     (I),Y
 1630 9111              STA     (I),Y
 1632 C8                INY
 1633 D0F9              BNE     COLD2
                
 1635 8D3FFF            STA     ROMOUT          ; DISABLE ROMS
 1638 A93F              LDA     #$3F            ; PATCH THIS KERNAL ROUTINE
 163A 8DE107            STA     $07E1           ; TO PREVENT ROM INTERFERENCE
                
                ;       LDA     #LOW WARM1      ; CHANGE THE HARDWARE RESET VECTOR
                ;       STA     $FFFC           ; TO POINT TO WARMSTART
                ;       LDA     #HIGH WARM1
                ;       STA     $FFFD
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE  10    
--- MACHINE COLDSTART: CBM PLUS/4 ---

 163D 58                CLI                     ; RE-ENABLE INTERRUPTS
                
 163E A931              LDA     #%00110001      ; GRAY
 1640 8D15FF            STA     BGCOL0          ; BACKGROUND
 1643 8D19FF            STA     BORDER          ; AND BORDER
                
 1646 A90E              LDA     #$0E
 1648 20D2FF            JSR     CHROUT          ; USE UPPER/LOWER CHARS
 164B A908              LDA     #$08
 164D 20D2FF            JSR     CHROUT          ; DISABLE CHARSET CHANGES
                
 1650 A900              LDA     #0
 1652 2090FF            JSR     SETMSG          ; DISABLE KERNAL MESSAGES
                
 1655 4C7116            JMP     WARM1           ; BRANCH ALWAYS
                
 1658                   ; ---------------
 1658                   ; WARMSTART ENTRY
 1658                   ; ---------------
                
 1658 54686520  SLOAD:  DB      "The story is loading ..."
 1670 0D                DB      EOL
 0019           SLOADL  EQU     $-SLOAD
                
 1671 D8        WARM1:  CLD
 1672 A2FF              LDX     #$FF
 1674 9A                TXS                     ; RESET MACHINE STACK
 1675 20E7FF            JSR     CLALL           ; CLOSE EVERYTHING
                
 1678 20922A            JSR     CLS             ; CLEAR SCREEN, ETC.
                
 167B A008              LDY     #8              ; POSITION "STORY LOADING" MESSAGE
 167D A20B              LDX     #11             ; AT (8,11)
 167F 18                CLC
 1680 20F0FF            JSR     PLOT
                
 1683 A258              LDX     #LOW SLOAD
 1685 A916              LDA     #HIGH SLOAD
 1687 A019              LDY     #SLOADL
 1689 20A329            JSR     DLINE           ; "THE STORY IS LOADING ..."
                
 168C A908              LDA     #8              ; MAKE BOOT DRIVE
 168E 20D72E            JSR     DOPEN           ; AND OPEN IT
                
                ;       LDA     FAST            ; FAST-READ AVAILABLE?
                ;       BEQ     WARM2           ; NO, SKIP AHEAD
                ;       JSR     FINIT           ; ELSE INIT FAST-READ
                
 1691                   ; FALL THROUGH TO ZIP WARMSTART AT "WARM2"
                
                        END


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE  11    
--- MACHINE COLDSTART: CBM PLUS/4 ---

                
                        INCLUD WARM.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  12    
--- WARMSTART ROUTINE ---

                
 1691                   ; -------------
 1691                   ; ZIP WARMSTART
 1691                   ; -------------
                
 1691 A900      WARM2:  LDA     #0              ; CLEAR ALL Z-PAGE VARIABLES
 1693 A203              LDX     #ZEROPG
 1695 9500      ST0:    STA     0,X
 1697 E8                INX
 1698 E07F              CPX     #ZPGTOP
 169A 90F9              BCC     ST0
                
 169C                   ; INIT THE PAGING TABLES & TIMESTAMP MAP (BM 3/8/85)
                
 169C AA                TAX                     ; = 0
 169D A8                TAY                     ; DITTO
 169E A9FF      ST1:    LDA     #$FF
 16A0 9D0013            STA     PTABL,X         ; PAGING TABLES ARE INITIALIZED
 16A3 9DA013            STA     PTABH,X         ; TO ALL $FF
 16A6 98                TYA                     ; TIMESTAMP MAP IS INITIALIZED
 16A7 9D5014            STA     LRUMAP,X        ; TO ZEROES
 16AA E8                INX
 16AB E0A0              CPX     #$A0
 16AD 90EF              BCC     ST1
                
 16AF E617              INC     ZSP             ; INIT Z-STACK POINTERS
 16B1 E618              INC     OLDZSP          ; TO "1"
 16B3 E662              INC     SCRIPT          ; ENABLE SCRIPTING
 16B5 E62D              INC     STAMP           ; INIT TIMESTAMP TO 1 (BM 3/8/85)
                
 16B7                   ; GRAB THE FIRST BLOCK OF PRELOAD
                
 16B7 A932              LDA     #HIGH ZBEGIN    ; MSB OF PRELOAD START ADDRESS
 16B9 8526              STA     ZCODE           ; FREEZE IT HERE
 16BB 8573              STA     DBUFF+HI        ; LSB IS ALWAYS ZERO
 16BD 20D92A            JSR     GETDSK          ; [DBLOCK] SET TO Z-BLOCK 0
                
 16C0                   ; EXTRACT GAME DATA FROM Z-CODE HEADER
                
 16C0 AE0432            LDX     ZBEGIN+ZENDLD   ; MSB OF ENDLOAD POINTER
 16C3 E8                INX                     ; ADD 1 TO GET
 16C4 8627              STX     ZPURE           ; 1ST "PURE" PAGE OF Z-CODE
                
 16C6 8A                TXA                     ; ADD START PAGE OF PRELOAD
 16C7 18                CLC                     ; TO CALC ABSOLUTE START ADDRESS
 16C8 6526              ADC     ZCODE           ; OF PAGING SPACE
 16CA 8528              STA     PAGE0
                
 16CC 20F525            JSR     MEMTOP          ; RETURNS TOP RAM PAGE IN [A]
 16CF 38                SEC
 16D0 E528              SBC     PAGE0           ; SUBTRACT ADDRESS OF PAGING SPACE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  13    
--- WARMSTART ROUTINE ---

 16D2 F002              BEQ     NORAM
 16D4 B005              BCS     SETNP           ; ERROR IF NOT ENOUGH RAM
                
 16D6                   ; *** ERROR #0 -- INSUFFICIENT RAM ***
                
 16D6 A900      NORAM:  LDA     #0
 16D8 4C9025            JMP     ZERROR
                
 16DB C9A0      SETNP:  CMP     #$A0            ; DON'T ALLOW MORE THAN
 16DD 9002              BCC     SETA0           ; $A0 PAGES
 16DF A9A0              LDA     #$A0            ; (BM 3/8/85)
 16E1 8529      SETA0:  STA     PMAX            ; SET # SWAPPING PAGES
                
 16E3 AD0132            LDA     ZBEGIN+ZMODE
 16E6 0920              ORA     #%00100000      ; ENABLE SPLIT-SCREEN
 16E8 8D0132            STA     ZBEGIN+ZMODE
                
 16EB 2902              AND     #%00000010      ; ISOLATE STATUS-FORMAT BIT
 16ED 855F              STA     TIMEFL          ; 0=SCORE, NZ=TIME
                
 16EF AD0C32            LDA     ZBEGIN+ZGLOBA   ; GET MSB OF GLOBAL TABLE ADDR
 16F2 18                CLC                     ; CONVERT TO
 16F3 6526              ADC     ZCODE           ; ABSOLUTE ADDRESS
 16F5 8530              STA     GLOBAL+HI
 16F7 AD0D32            LDA     ZBEGIN+ZGLOBA+1 ; LSB NEEDN'T CHANGE
 16FA 852F              STA     GLOBAL+LO
                
 16FC AD1832            LDA     ZBEGIN+ZFWORD   ; DO SAME FOR FWORDS TABLE
 16FF 18                CLC
 1700 6526              ADC     ZCODE
 1702 8534              STA     FWORDS+HI
 1704 AD1932            LDA     ZBEGIN+ZFWORD+1 ; NO CHANGE FOR LSB
 1707 8533              STA     FWORDS+LO
                
 1709 AD0832            LDA     ZBEGIN+ZVOCAB   ; NOW DO VOCABULARY TABLE
 170C 18                CLC
 170D 6526              ADC     ZCODE
 170F 8532              STA     VOCAB+HI
 1711 AD0932            LDA     ZBEGIN+ZVOCAB+1 ; LSB SAME
 1714 8531              STA     VOCAB+LO
                
 1716 AD0A32            LDA     ZBEGIN+ZOBJEC   ; NOT TO MENTION
 1719 18                CLC                     ; THE OBJECT TABLE
 171A 6526              ADC     ZCODE
 171C 8536              STA     OBJTAB+HI
 171E AD0B32            LDA     ZBEGIN+ZOBJEC+1 ; LSB SAME
 1721 8535              STA     OBJTAB+LO
                
 1723                   ; FETCH THE REST OF THE PRELOAD
                
 1723 A570      LDPRE:  LDA     DBLOCK+LO       ; CHECK CURRENT BLOCK #


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  14    
--- WARMSTART ROUTINE ---

 1725 C527              CMP     ZPURE           ; LOADED LAST PRELOAD PAGE YET?
 1727 B006              BCS     WARMEX          ; YES, TIME TO PLAY!
 1729 20D92A            JSR     GETDSK          ; ELSE GRAB NEXT Z-BLOCK
 172C 4C2317            JMP     LDPRE
                
 172F AD0632    WARMEX: LDA     ZBEGIN+ZGO      ; GET START ADDRESS OF Z-CODE
 1732 851A              STA     ZPCM            ; MSB
 1734 AD0732            LDA     ZBEGIN+ZGO+1    ; AND LSB
 1737 8519              STA     ZPCL            ; HIGH BIT ALREADY ZEROED
                
 1739 20922A            JSR     CLS             ; CLEAR SCREEN, DISABLE SPLIT
                
 173C                   ; ... AND FALL INTO MAIN LOOP
                
                        END
                        INCLUD MAIN.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  15    
--- MAIN LOOP ---

                
 173C A900      MLOOP:  LDA     #0
 173E 8504              STA     NARGS           ; RESET # ARGUMENTS
 1740 20B021            JSR     NEXTPC          ; GET NEXT INSTRUCTION INTO [A]
 1743 8503              STA     OPCODE          ; SAVE IT HERE
                
 1745                   IF      DEBUG
 1745                   STA     MBYTE
 1745                   LDA     #0              ; BREAKPOINT #0
 1745                   JSR     DOBUG
 1745                   LDA     MBYTE
 1745                   ENDIF
                
 1745                   ; DECODE AN OPCODE
                
 1745 AA                TAX                     ; SET FLAGS
 1746 3003              BMI     DC0             ; IF POSITIVE,
 1748 4C0618            JMP     OP2             ; IT'S A 2-OP
                
 174B C9B0      DC0:    CMP     #$B0
 174D B003              BCS     DC1
 174F 4CD717            JMP     OP1             ; OR MAYBE A 1-OP
                
 1752 C9C0      DC1:    CMP     #$C0
 1754 B003              BCS     OPEXT
 1756 4CC817            JMP     OP0             ; PERHAPS A 0-OP
                
 1759                   ; --------------
 1759                   ; HANDLE AN X-OP
 1759                   ; --------------
                
 1759 20B021    OPEXT:  JSR     NEXTPC          ; GRAB THE ARGUMENT ID BYTE
 175C 850D              STA     ABYTE           ; HOLD IT HERE
                
 175E A200              LDX     #0
 1760 860E              STX     ADEX            ; INIT ARGUMENT INDEX
 1762 F006              BEQ     OPX1            ; JUMP TO TOP OF LOOP
                
 1764 A50D      OPX0:   LDA     ABYTE           ; GET ARG BYTE
 1766 0A                ASL     A               ; SHIFT NEXT 2 ARG BITS
 1767 0A                ASL     A               ; INTO BITS 7 & 6
 1768 850D              STA     ABYTE           ; HOLD FOR LATER
                
 176A 29C0      OPX1:   AND     #%11000000      ; MASK OUT GARBAGE BITS
 176C D006              BNE     OPX2
 176E 205218            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 1771 4C8517            JMP     OPXNXT
                
 1774 C940      OPX2:   CMP     #%01000000      ; IS IT A SHORT IMMEDIATE?
 1776 D006              BNE     OPX3            ; NO, KEEP GUESSING
 1778 204E18            JSR     GETSHT          ; 01 = SHORT IMMEDIATE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  16    
--- MAIN LOOP ---

 177B 4C8517            JMP     OPXNXT
                
 177E C980      OPX3:   CMP     #%10000000      ; LAST TEST
 1780 D017              BNE     OPX4            ; 11 = NO MORE ARGUMENTS
 1782 206618            JSR     GETVAR          ; 10 = VARIABLE
                
 1785 A60E      OPXNXT: LDX     ADEX            ; RETRIEVE ARGUMENT INDEX
 1787 A50F              LDA     VALUE+LO        ; GRAB LSB OF VALUE
 1789 9505              STA     ARG1+LO,X       ; STORE IN ARGUMENT TABLE
 178B A510              LDA     VALUE+HI        ; GRAB MSB OF VALUE
 178D 9506              STA     ARG1+HI,X       ; STORE THAT, TOO
                
 178F E604              INC     NARGS           ; UPDATE ARGUMENT COUNTER
                
 1791 E8                INX
 1792 E8                INX
 1793 860E              STX     ADEX            ; UPDATE INDEX
 1795 E008              CPX     #8              ; DONE 4 ARGUMENTS YET?
 1797 90CB              BCC     OPX0            ; NO, GET SOME MORE
                
 1799                   ; ALL X-OP ARGUMENTS READY
                
 1799 A503      OPX4:   LDA     OPCODE          ; IS THIS
 179B C9E0              CMP     #$E0            ; AN EXTENDED 2-OP?
 179D B003              BCS     DOXOP           ; NO, IT'S A REAL X-OP
 179F 4C2F18            JMP     OP2EX           ; ELSE TREAT IT LIKE A 2-OP
                
 17A2 A210      DOXOP:  LDX     #LOW OPTX       ; GET ADDR OF X-OP TABLE
 17A4 A01A              LDY     #HIGH OPTX      ; INTO [X/Y]
 17A6 291F              AND     #%00011111      ; ISOLATE OP ID BITS
 17A8 C90C              CMP     #NOPSX          ; IS IT A LEGAL X-OP?
 17AA 9005              BCC     DODIS           ; YUP; TIME TO DISPATCH IT
                
 17AC                   ; *** ERROR #1 -- ILLEGAL X-OP ***
                
 17AC A901              LDA     #1
 17AE 4C9025            JMP     ZERROR
                
 17B1                   ; ---------------
 17B1                   ; OPCODE DISPATCH
 17B1                   ; ---------------
                
 17B1                   ; ENTRY: MASKED OPCODE INDEX IN [A]
 17B1                   ;        OP-TABLE ADDR IN X/Y (LSB/MSB)
                
 17B1 8611      DODIS:  STX     I+LO            ; SAVE TABLE ADDRESS
 17B3 8412              STY     I+HI            ; IN A POINTER
                
 17B5 0A                ASL     A               ; WORD-ALIGN THE OP INDEX
 17B6 A8                TAY
 17B7 B111              LDA     (I),Y           ; GET LSB OF DISPATCH ADDRESS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  17    
--- MAIN LOOP ---

 17B9 8DC317            STA     GO+LO           ; INSTALL AS JSR OPERAND
 17BC C8                INY
 17BD B111              LDA     (I),Y           ; SAME WITH MSB
 17BF 8DC417            STA     GO+HI
                
 17C2 20                DB      $20             ; 6502 "JSR" OPCODE
 17C3 0000      GO:     DW      $0000           ; DUMMY OPERAND BYTES
                
 17C5 4C3C17            JMP     MLOOP           ; GO BACK FOR ANOTHER OPCODE
                
 17C8                   ; -------------
 17C8                   ; HANDLE A 0-OP
 17C8                   ; -------------
                
 17C8 A2A2      OP0:    LDX     #LOW OPT0       ; GET 0-OP TABLE ADDR
 17CA A019              LDY     #HIGH OPT0      ; INTO [X/Y]
 17CC 290F              AND     #%00001111      ; ISOLATE 0-OP ID BITS
 17CE C90E              CMP     #NOPS0          ; OUT OF RANGE?
 17D0 90DF              BCC     DODIS           ; NO, DISPATCH IT
                
 17D2                   ; *** ERROR #2 -- ILLEGAL 0-OP ***
                
 17D2 A902              LDA     #2
 17D4 4C9025            JMP     ZERROR
                
 17D7                   ; -------------
 17D7                   ; HANDLE A 1-OP
 17D7                   ; -------------
                
 17D7 2930      OP1:    AND     #%00110000      ; ISOLATE ARGUMENT BITS
 17D9 D006              BNE     OP1A
 17DB 205218            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 17DE 4CF217            JMP     OP1EX
                
 17E1 C910      OP1A:   CMP     #%00010000      ; TEST AGAIN
 17E3 D006              BNE     OP1B
 17E5 204E18            JSR     GETSHT          ; 01 = SHORT IMMEDIATE
 17E8 4CF217            JMP     OP1EX
                
 17EB C920      OP1B:   CMP     #%00100000      ; ONE MORE TEST
 17ED D012              BNE     BADOP1          ; UNDEFINED STATE!
 17EF 206618            JSR     GETVAR          ; 10 = VARIABLE
                
 17F2 204318    OP1EX:  JSR     V2A1            ; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
 17F5 A2BE              LDX     #LOW OPT1       ; GET ADDR OF 1-OP TABLE
 17F7 A019              LDY     #HIGH OPT1      ; INTO [X/Y]
 17F9 A503              LDA     OPCODE          ; RESTORE OPCODE
 17FB 290F              AND     #%00001111      ; ISOLATE OP ID BITS
 17FD C910              CMP     #NOPS1          ; IF WITHIN RANGE,
 17FF 90B0              BCC     DODIS           ; EXECUTE THE 1-OP
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  18    
--- MAIN LOOP ---

 1801                   ; *** ERROR #3 -- ILLEGAL 1-OP ***
                
 1801 A903      BADOP1: LDA     #3
 1803 4C9025            JMP     ZERROR
                
 1806                   ; -------------
 1806                   ; HANDLE A 2-OP
 1806                   ; -------------
                
 1806 2940      OP2:    AND     #%01000000      ; ISOLATE 1ST ARG BIT
 1808 D006              BNE     OP2A
 180A 204E18            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 180D 4C1318            JMP     OP2B
 1810 206618    OP2A:   JSR     GETVAR          ; 1 = VARIABLE
 1813 204318    OP2B:   JSR     V2A1            ; [VALUE] TO [ARG1], UPDATE [NARGS]
                
 1816 A503              LDA     OPCODE          ; RESTORE OPCODE BYTE
 1818 2920              AND     #%00100000      ; ISOLATE 2ND ARG BIT
 181A D006              BNE     OP2C
 181C 204E18            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 181F 4C2518            JMP     OP2D
 1822 206618    OP2C:   JSR     GETVAR          ; 1 = VARIABLE
 1825 A50F      OP2D:   LDA     VALUE+LO        ; MOVE 2ND [VALUE]
 1827 8507              STA     ARG2+LO         ; INTO [ARG2]
 1829 A510              LDA     VALUE+HI
 182B 8508              STA     ARG2+HI
 182D E604              INC     NARGS           ; UPDATE ARGUMENT COUNT
                
 182F                   ; EXECUTE A 2-OP OR EXTENDED 2-OP
                
 182F A2DE      OP2EX:  LDX     #LOW OPT2       ; LSB OF DISPATCH TABLE
 1831 A019              LDY     #HIGH OPT2      ; MSB
 1833 A503              LDA     OPCODE          ; RESTORE OPCODE BYTE
 1835 291F              AND     #%00011111      ; ISOLATE OP ID BITS
 1837 C919              CMP     #NOPS2
 1839 B003              BCS     BADOP2          ; ERROR IF OUT OF RANGE
 183B 4CB117            JMP     DODIS           ; ELSE DISPATCH
                
 183E                   ; *** ERROR #4 -- ILLEGAL 2-OP ****
                
 183E A904      BADOP2: LDA     #4
 1840 4C9025            JMP     ZERROR
                
 1843                   ; --------------------------------------
 1843                   ; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
 1843                   ; --------------------------------------
                
 1843 A50F      V2A1:   LDA     VALUE+LO
 1845 8505              STA     ARG1+LO
 1847 A510              LDA     VALUE+HI
 1849 8506              STA     ARG1+HI


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  19    
--- MAIN LOOP ---

 184B E604              INC     NARGS
 184D 60                RTS
                
                        END
                        INCLUD SUBS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  20    
--- OPCODE SUPPORT SUBROUTINES ---

                
 184E                   ; -----------------------
 184E                   ; FETCH A SHORT IMMEDIATE
 184E                   ; -----------------------
                
 184E A900      GETSHT: LDA     #0              ; MSB IS ZERO
 1850 F003              BEQ     GETV            ; FETCH LSB FROM Z-CODE
                
 1852                   ; ----------------------
 1852                   ; FETCH A LONG IMMEDIATE
 1852                   ; ----------------------
                
 1852 20B021    GETLNG: JSR     NEXTPC          ; GRAB MSB
                
 1855 8510      GETV:   STA     VALUE+HI
 1857 20B021            JSR     NEXTPC          ; GRAB LSB
 185A 850F              STA     VALUE+LO
 185C 60                RTS
                
 185D                   ; ----------------
 185D                   ; FETCH A VARIABLE
 185D                   ; ----------------
                
 185D                   ; FROM INSIDE AN OPCODE (VARIABLE ID IN [A])
                
 185D AA        VARGET: TAX                     ; IF NON-ZERO,
 185E D00B              BNE     GETVR1          ; ACCESS A VARIABLE
                
 1860 208C18            JSR     POPVAL          ; ELSE PULL VAR OFF Z-STACK
 1863 4CA218            JMP     PSHVAL          ; WITHOUT ALTERING STACK
                
 1866                   ; FROM THE MAIN LOOP (VARIABLE ID IN Z-CODE)
                
 1866 20B021    GETVAR: JSR     NEXTPC          ; GRAB VAR-TYPE BYTE
 1869 F021              BEQ     POPVAL          ; VALUE IS ON Z-STACK
                
 186B                   ; IS VARIABLE LOCAL OR GLOBAL?
                
 186B C910      GETVR1: CMP     #$10            ; IF >= 16,
 186D B010              BCS     GETVRG          ; IT'S GLOBAL
                
 186F                   ; HANDLE A LOCAL VARIABLE
                
 186F 38        GETVRL: SEC
 1870 E901              SBC     #1              ; FORM A ZERO-ALIGNED
 1872 0A                ASL     A               ; WORD INDEX
 1873 AA                TAX                     ; INTO THE [LOCALS] TABLE
                
 1874 BD0015            LDA     LOCALS+LO,X     ; GRAB LSB
 1877 850F              STA     VALUE+LO
 1879 BD0115            LDA     LOCALS+HI,X     ; AND MSB


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  21    
--- OPCODE SUPPORT SUBROUTINES ---

 187C 8510              STA     VALUE+HI
 187E 60                RTS
                
 187F                   ; HANDLE A GLOBAL VARIABLE
                
 187F 20F018    GETVRG: JSR     GVCALC          ; GET ADDRESS OF GLOBAL INTO [I]
 1882 B111              LDA     (I),Y           ; MSB OF GLOBAL ([Y] = 0)
 1884 8510              STA     VALUE+HI
 1886 C8                INY                     ; = 1
 1887 B111              LDA     (I),Y           ; LSB OF GLOBAL
 1889 850F              STA     VALUE+LO        ; SAVE IT
 188B 60                RTS                     ; AND WE'RE DONE
                
 188C                   ; ----------------------------------
 188C                   ; POP Z-STACK INTO [VALUE] AND [X/A]
 188C                   ; ----------------------------------
                
 188C C617      POPVAL: DEC     ZSP
 188E F00D              BEQ     UNDER           ; UNDERFLOW IF ZERO!
                
 1890 A417              LDY     ZSP             ; READ STACK POINTER
 1892 BE0011            LDX     ZSTAKL,Y        ; GRAB LSB OF STACK VALUE
 1895 860F              STX     VALUE+LO        ; GIVE TO [VALUE]
 1897 B90012            LDA     ZSTAKH,Y        ; ALSO GRAB MSB
 189A 8510              STA     VALUE+HI        ; A SIMILAR FATE
 189C 60                RTS
                
 189D                   ; *** ERROR #5 -- Z-STACK UNDERFLOW ***
                
 189D A905      UNDER:  LDA     #5
 189F 4C9025            JMP     ZERROR
                
 18A2                   ; -----------------------
 18A2                   ; PUSH [VALUE] TO Z-STACK
 18A2                   ; -----------------------
                
 18A2 A60F      PSHVAL: LDX     VALUE+LO
 18A4 A510              LDA     VALUE+HI
                
 18A6                   ; ---------------------
 18A6                   ; PUSH [X/A] TO Z-STACK
 18A6                   ; ---------------------
                
 18A6 A417      PUSHXA: LDY     ZSP             ; READ STACK POINTER
 18A8 990012            STA     ZSTAKH,Y        ; PUSH MSB IN [A]
 18AB 8A                TXA
 18AC 990011            STA     ZSTAKL,Y        ; AND LSB IN [X]
                
 18AF E617              INC     ZSP             ; UPDATE Z-STACK POINTER
 18B1 F001              BEQ     OVER            ; OVERFLOW IF ZEROED!
 18B3 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  22    
--- OPCODE SUPPORT SUBROUTINES ---

                
 18B4                   ; *** ERROR #6 -- Z-STACK OVERFLOW ***
                
 18B4 A906      OVER:   LDA     #6
 18B6 4C9025            JMP     ZERROR
                
 18B9                   ; --------------
 18B9                   ; RETURN A VALUE
 18B9                   ; --------------
                
 18B9                   ; FROM WITHIN AN OPCODE (VARIABLE ID IN [A])
                
 18B9 AA        VARPUT: TAX                     ; IF ZERO,
 18BA D013              BNE     PUTVR1
                
 18BC C617              DEC     ZSP             ; FLUSH TOP WORD OFF STACK
 18BE D0E2              BNE     PSHVAL          ; AND REPLACE WITH [VALUE]
 18C0 F0DB              BEQ     UNDER           ; ERROR IF [ZSP] BECAME ZERO!
                
 18C2                   ; RETURN A ZERO
                
 18C2 A900      RET0:   LDA     #0
                
 18C4                   ; RETURN BYTE IN [A]
                
 18C4 850F      PUTBYT: STA     VALUE+LO
 18C6 A900              LDA     #0
 18C8 8510              STA     VALUE+HI                ; CLEAR MSB
                
 18CA                   ; RETURN [VALUE]
                
 18CA 20B021    PUTVAL: JSR     NEXTPC          ; GET VARIABLE ID BYTE
 18CD F0D3              BEQ     PSHVAL          ; [VALUE] GOES TO Z-STACK
                
 18CF                   ; LOCAL OR GLOBAL VARIABLE?
                
 18CF C910      PUTVR1: CMP     #$10            ; IF >= 16,
 18D1 B010              BCS     PUTVLG          ; IT'S GLOBAL
                
 18D3                   ; PUT A LOCAL VARIABLE
                
 18D3 38        PUTVLL: SEC
 18D4 E901              SBC     #1              ; FORM A ZERO-ALIGNED
 18D6 0A                ASL     A               ; WORD INDEX
 18D7 AA                TAX                     ; INTO THE [LOCALS] TABLE
                
 18D8 A50F              LDA     VALUE+LO        ; GRAB LSB
 18DA 9D0015            STA     LOCALS+LO,X     ; SAVE IN LOCAL TABLE
 18DD A510              LDA     VALUE+HI        ; DO SAME TO
 18DF 9D0115            STA     LOCALS+HI,X     ; MSB
 18E2 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  23    
--- OPCODE SUPPORT SUBROUTINES ---

                
 18E3                   ; RETURN A GLOBAL VARIABLE
                
 18E3 20F018    PUTVLG: JSR     GVCALC
 18E6 A510              LDA     VALUE+HI        ; GET MSB
 18E8 9111              STA     (I),Y           ; STORE AS 1ST BYTE ([Y] = 0)
 18EA C8                INY                     ; = 1
 18EB A50F              LDA     VALUE+LO        ; NOW GET LSB
 18ED 9111              STA     (I),Y           ; STORE AS 2ND BYTE
 18EF 60                RTS
                
 18F0                   ; -----------------------
 18F0                   ; CALC GLOBAL WORD OFFSET
 18F0                   ; -----------------------
                
 18F0                   ; ENTRY: VAR-ID BYTE (16-255) IN [A]
 18F0                   ; EXIT: ABSOLUTE ADDRESS OF GLOBAL VAR IN [I]
 18F0                   ;       [Y] = 0 FOR INDEXING
                
 18F0 38        GVCALC: SEC
 18F1 E910              SBC     #$10            ; FORM A ZERO-ALIGNED INDEX
 18F3 A000              LDY     #0              ; MAKE SURE MSB OF OFFSET AND [Y]
 18F5 8412              STY     I+HI            ; ARE CLEARED
                
 18F7 0A                ASL     A               ; MULTIPLY OFFSET BY 2
 18F8 2612              ROL     I+HI            ; TO WORD-ALIGN IT
                
 18FA 18                CLC                     ; ADD OFFSET TO ADDR OF GLOBAL TABLE
 18FB 652F              ADC     GLOBAL+LO       ; TO FORM THE ABSOLUTE
 18FD 8511              STA     I+LO            ; ADDRESS OF THE
 18FF A512              LDA     I+HI            ; DESIRED GLOBAL VARIABLE
 1901 6530              ADC     GLOBAL+HI       ; STORE ADDRESS BACK IN [VAL]
 1903 8512              STA     I+HI            ; AS A POINTER
                
 1905 60        WCEX:   RTS
                
 1906                   ; ---------------
 1906                   ; PREDICATE FAILS
 1906                   ; ---------------
                
 1906 20B021    PREDF:  JSR     NEXTPC          ; GET 1ST BRANCH BYTE
 1909 100C              BPL     PREDB           ; DO BRANCH IF BIT 7 OFF
                
 190B                   ; -----------------------
 190B                   ; IGNORE PREDICATE BRANCH
 190B                   ; -----------------------
                
 190B                   ; ENTRY: 1ST BRANCH BYTE IN [A]
                
 190B 2940      PREDNB: AND     #%01000000      ; TEST BIT 6
 190D D0F6              BNE     WCEX            ; SHORT BRANCH IF SET


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  24    
--- OPCODE SUPPORT SUBROUTINES ---

 190F 4CB021            JMP     NEXTPC          ; ELSE SKIP OVER 2ND BRANCH BYTE
                
 1912                   ; ------------------
 1912                   ; PREDICATE SUCCEEDS
 1912                   ; ------------------
                
 1912 20B021    PREDS:  JSR     NEXTPC          ; GET 1ST BRANCH BYTE
 1915 10F4              BPL     PREDNB          ; DON'T BRANCH IF BIT 7 CLEAR
                
 1917                   ; --------------------------
 1917                   ; PERFORM A PREDICATE BRANCH
 1917                   ; --------------------------
                
 1917                   ; ENTRY: 1ST PRED BYTE IN [A]
                
 1917 AA        PREDB:  TAX                     ; SAVE HERE
 1918 2940              AND     #%01000000      ; LONG OR SHORT BRANCH?
 191A F00B              BEQ     PREDLB          ; LONG IF BIT 6 IS CLEAR
                
 191C                   ; HANDLE A SHORT BRANCH
                
 191C 8A                TXA                     ; RESTORE PRED BYTE
 191D 293F              AND     #%00111111      ; FORM SHORT OFFSET
 191F 850F              STA     VALUE+LO        ; USE AS LSB OF BRANCH OFFSET
 1921 A900              LDA     #0
 1923 8510              STA     VALUE+HI        ; MSB OF OFFSET IS ZERO
 1925 F013              BEQ     PREDB1          ; DO THE BRANCH
                
 1927                   ; HANDLE A LONG BRANCH
                
 1927 8A        PREDLB: TXA                     ; RESTORE 1ST PRED BYTE
 1928 293F              AND     #%00111111      ; FORM MSB OF OFFSET
                
 192A AA                TAX                     ; SAVE HERE FOR REFERENCE
                
 192B 2920              AND     #%00100000      ; CHECK SIGN OF 14-BIT VALUE
 192D F004              BEQ     DOB2            ; POSITIVE IF ZERO, SO USE [X]
                
 192F 8A                TXA                     ; ELSE RESTORE BYTE
 1930 09E0              ORA     #%11100000      ; EXTEND THE SIGN BIT
 1932 AA                TAX                     ; BACK HERE FOR STORAGE
                
 1933 8610      DOB2:   STX     VALUE+HI
 1935 20B021            JSR     NEXTPC          ; FETCH LSB OF 14-BIT OFFSET
 1938 850F              STA     VALUE+LO
                
 193A                   ; BRANCH TO Z-ADDRESS IN [VALUE]
                
 193A A510      PREDB1: LDA     VALUE+HI        ; CHECK MSB OF OFFSET
 193C D00E              BNE     PREDB3          ; DO BRANCH IF NZ
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  25    
--- OPCODE SUPPORT SUBROUTINES ---

 193E A50F              LDA     VALUE+LO        ; IF LSB IS NON-ZERO,
 1940 D003              BNE     PREDB2          ; MAKE SURE IT ISN'T 1
 1942 4C331A            JMP     ZRFALS          ; ELSE DO AN "RFALSE"
                
 1945 C901      PREDB2: CMP     #1              ; IF OFFSET = 1
 1947 D003              BNE     PREDB3
 1949 4C281A            JMP     ZRTRUE          ; DO AN "RTRUE"
                
 194C                   ; ENTRY POINT FOR "JUMP"
                
 194C 208619    PREDB3: JSR     DECVAL          ; SUBTRACT 2 FROM THE OFFSET
 194F 208619            JSR     DECVAL          ; IN [VALUE]
                
 1952 A900              LDA     #0              ; CLEAR THE MSB
 1954 8512              STA     I+HI            ; OF [I]
                
 1956 A510              LDA     VALUE+HI        ; MAKE MSB OF OFFSET
 1958 8511              STA     I+LO            ; THE LSB OF [I]
 195A 0A                ASL     A               ; EXTEND THE SIGN OF OFFSET
 195B 2612              ROL     I+HI            ; INTO MSB OF [I]
                
 195D A50F              LDA     VALUE+LO        ; GET LSB OF OFFSET
 195F 18                CLC
 1960 6519              ADC     ZPCL            ; ADD LOW 8 BITS OF ZPC
 1962 9006              BCC     PREDB5          ; IF OVERFLOWED,
                
 1964 E611              INC     I+LO            ; UPDATE UPPER 9 BITS
 1966 D002              BNE     PREDB5
 1968 E612              INC     I+HI
                
 196A 8519      PREDB5: STA     ZPCL            ; UPDATE ZPC
                
 196C A511              LDA     I+LO            ; IF UPPER 9 BITS ARE ZERO,
 196E 0512              ORA     I+HI            ; NO NEED TO CHANGE PAGES
 1970 F013              BEQ     ZNOOP
                
 1972 A511              LDA     I+LO            ; ELSE CALC NEW UPPER BITS
 1974 18                CLC
 1975 651A              ADC     ZPCM
 1977 851A              STA     ZPCM
                
 1979 A512              LDA     I+HI
 197B 651B              ADC     ZPCH
 197D 2901              AND     #%00000001      ; USE ONLY BIT 0
 197F 851B              STA     ZPCH
                
 1981 A900              LDA     #0
 1983 851C              STA     ZPCFLG          ; [ZPC] NO LONGER VALID
                
 1985                   ; FALL THROUGH ...
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  26    
--- OPCODE SUPPORT SUBROUTINES ---

 1985                   ; ----
 1985                   ; NOOP
 1985                   ; ----
                
 1985 60        ZNOOP:  RTS
                
 1986                   ; -----------------
 1986                   ; DECREMENT [VALUE]
 1986                   ; -----------------
                
 1986 A50F      DECVAL: LDA     VALUE+LO
 1988 38                SEC
 1989 E901              SBC     #1
 198B 850F              STA     VALUE+LO
 198D B002              BCS     DVX
 198F C610              DEC     VALUE+HI
 1991 60        DVX:    RTS
                
 1992                   ; -----------------
 1992                   ; INCREMENT [VALUE]
 1992                   ; -----------------
                
 1992 E60F      INCVAL: INC     VALUE+LO
 1994 D002              BNE     IVX
 1996 E610              INC     VALUE+HI
 1998 60        IVX:    RTS
                
 1999                   ; ----------------------
 1999                   ; MOVE [ARG1] TO [VALUE]
 1999                   ; ----------------------
                
 1999 A505      A12VAL: LDA     ARG1+LO
 199B 850F              STA     VALUE+LO
 199D A506              LDA     ARG1+HI
 199F 8510              STA     VALUE+HI
 19A1 60                RTS
                
                        END
                        INCLUD DISPATCH.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  27    
--- OPCODE DISPATCH TABLES ---

                
 19A2                   ; 0-OPS
                
 19A2 281A      OPT0:   DW      ZRTRUE          ; 0
 19A4 331A              DW      ZRFALS          ; 1
 19A6 371A              DW      ZPRI            ; 2
 19A8 541A              DW      ZPRR            ; 3
 19AA 8519              DW      ZNOOP           ; 4
 19AC 9E2D              DW      ZSAVE           ; 5
 19AE 392E              DW      ZREST           ; 6
 19B0 CE25              DW      ZSTART          ; 7
 19B2 5D1A              DW      ZRSTAK          ; 8
 19B4 8C18              DW      POPVAL          ; 9
 19B6 B125              DW      ZQUIT           ; 10
 19B8 3D26              DW      ZCRLF           ; 11
 19BA C226              DW      ZUSL            ; 12
 19BC 631A              DW      ZVER            ; 13
                
 000E           NOPS0   EQU     14              ; NUMBER OF 0-OPS
                
 19BE                   ; 1-OPS
                
 19BE C11A      OPT1:   DW      ZZERO           ; 0
 19C0 CA1A              DW      ZNEXT           ; 1
 19C2 D31A              DW      ZFIRST          ; 2
 19C4 E61A              DW      ZLOC            ; 3
 19C6 F21A              DW      ZPTSIZ          ; 4
 19C8 0F1B              DW      ZINC            ; 5
 19CA 1A1B              DW      ZDEC            ; 6
 19CC 271B              DW      ZPRB            ; 7
 19CE 0118              DW      BADOP1          ; 8 (UNDEFINED)
 19D0 351B              DW      ZREMOV          ; 9
 19D2 771B              DW      ZPRD            ; 10
 19D4 941B              DW      ZRET            ; 11
 19D6 D11B              DW      ZJUMP           ; 12
 19D8 D71B              DW      ZPRINT          ; 13
 19DA E51B              DW      ZVALUE          ; 14
 19DC ED1B              DW      ZBCOM           ; 15
                
 0010           NOPS1   EQU     16              ; NUMBER OF 1-OPS
                
 19DE                   ; 2-OPS
                
 19DE 3E18      OPT2:   DW      BADOP2          ; 0 (UNDEFINED)
 19E0 9B1E              DW      ZEQUAL          ; 1
 19E2 FD1B              DW      ZLESS           ; 2
 19E4 111C              DW      ZGRTR           ; 3
 19E6 031C              DW      ZDLESS          ; 4
 19E8 1C1C              DW      ZIGRTR          ; 5
 19EA 4C1C              DW      ZIN             ; 6
 19EC 5C1C              DW      ZBTST           ; 7


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  28    
--- OPCODE DISPATCH TABLES ---

 19EE 6F1C              DW      ZBOR            ; 8
 19F0 7B1C              DW      ZBAND           ; 9
 19F2 871C              DW      ZFSETP          ; 10
 19F4 9B1C              DW      ZFSET           ; 11
 19F6 AE1C              DW      ZFCLR           ; 12
 19F8 C51C              DW      ZSET            ; 13
 19FA D21C              DW      ZMOVE           ; 14
 19FC FC1C              DW      ZGET            ; 15
 19FE 0C1D              DW      ZGETB           ; 16
 1A00 291D              DW      ZGETP           ; 17
 1A02 711D              DW      ZGETPT          ; 18
 1A04 9E1D              DW      ZNEXTP          ; 19
 1A06 BD1D              DW      ZADD            ; 20
 1A08 CA1D              DW      ZSUB            ; 21
 1A0A D71D              DW      ZMUL            ; 22
 1A0C FB1D              DW      ZDIV            ; 23
 1A0E 051E              DW      ZMOD            ; 24
                
 0019           NOPS2   EQU     25              ; NUMBER OF 2-OPS
                
 1A10                   ; X-OPS
                
 1A10 CE1E      OPTX:   DW      ZCALL           ; 0
 1A12 611F              DW      ZPUT            ; 1
 1A14 721F              DW      ZPUTB           ; 2
 1A16 8A1F              DW      ZPUTP           ; 3
 1A18 3620              DW      ZREAD           ; 4
 1A1A BB1F              DW      ZPRC            ; 5
 1A1C C01F              DW      ZPRN            ; 6
 1A1E 0520              DW      ZRAND           ; 7
 1A20 2720              DW      ZPUSH           ; 8
 1A22 2E20              DW      ZPOP            ; 9
 1A24 0B2A              DW      ZSPLIT          ; 10
 1A26 542A              DW      ZSCRN           ; 11
                
 000C           NOPSX   EQU     12              ; NUMBER OF X-OPS
                
                        END
                
                        INCLUD OPS0.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  29    
--- 0-OPS ---

                
 1A28                   ; -----
 1A28                   ; RTRUE
 1A28                   ; -----
                
 1A28                   ; SIMULATE A "RETURN 1"
                
 1A28 A201      ZRTRUE: LDX     #1
                
 1A2A A900      ZRT0:   LDA     #0
                
 1A2C 8605      ZRT1:   STX     ARG1+LO         ; GIVE TO
 1A2E 8506              STA     ARG1+HI         ; [ARG1]
 1A30 4C941B            JMP     ZRET            ; AND DO THE RETURN
                
 1A33                   ; ------
 1A33                   ; RFALSE
 1A33                   ; ------
                
 1A33                   ; SIMULATE A "RETURN 0"
                
 1A33 A200      ZRFALS: LDX     #0
 1A35 F0F3              BEQ     ZRT0
                
 1A37                   ; ------
 1A37                   ; PRINTI
 1A37                   ; ------
                
 1A37                   ; PRINT Z-STRING FOLLOWING THE OPCODE
                
 1A37 A51B      ZPRI:   LDA     ZPCH            ; MOVE [ZPC] INTO [MPC]
 1A39 8521              STA     MPCH
 1A3B A51A              LDA     ZPCM
 1A3D 8520              STA     MPCM
 1A3F A519              LDA     ZPCL
 1A41 851F              STA     MPCL
                
 1A43 A900              LDA     #0
 1A45 8522              STA     MPCFLG          ; [MPC] NO LONGER VALID
                
 1A47 20D922            JSR     PZSTR           ; PRINT THE Z-STRING AT [MPC]
                
 1A4A A205              LDX     #5              ; COPY STATE OF [MPC]
 1A4C B51F      PRIL:   LDA     MPC,X           ; INTO [ZPC]
 1A4E 9519              STA     ZPC,X
 1A50 CA                DEX
 1A51 10F9              BPL     PRIL
 1A53 60                RTS
                
 1A54                   ; ------
 1A54                   ; PRINTR


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  30    
--- 0-OPS ---

 1A54                   ; ------
                
 1A54                   ; DO A "PRINTI," FOLLOWED BY "CRLF" AND "RTRUE"
                
 1A54 20371A    ZPRR:   JSR     ZPRI
 1A57 203D26            JSR     ZCRLF
 1A5A 4C281A            JMP     ZRTRUE
                
 1A5D                   ; ------
 1A5D                   ; RSTACK
 1A5D                   ; ------
                
 1A5D                   ; "RETURN" WITH VALUE ON STACK
                
 1A5D 208C18    ZRSTAK: JSR     POPVAL          ; GET VALUE INTO [X/A]
 1A60 4C2C1A            JMP     ZRT1            ; AND GIVE IT TO "RETURN"
                
 1A63                   ; ------
 1A63                   ; VERIFY
 1A63                   ; ------
                
 1A63                   ; VERIFY GAME CODE ON DISK
                
 1A63 20E925    ZVER:   JSR     VERNUM          ; DISPLAY ZIP VERSION NUMBER
                
 1A66 A203              LDX     #3
 1A68 A900              LDA     #0
 1A6A 9513      ZVR:    STA     J+LO,X          ; CLEAR [J], [K]
 1A6C 951F              STA     MPC,X           ; [MPC] AND [MPCFLG]
 1A6E CA                DEX
 1A6F 10F9              BPL     ZVR
                
 1A71 A940              LDA     #64             ; POINT [MPC] TO Z-ADDRESS $00040
 1A73 851F              STA     MPCL            ; 1ST 64 BYTES AREN'T CHECKED
                
 1A75 AD1A32            LDA     ZBEGIN+ZLENTH   ; GET LENGTH OF Z-CODE
 1A78 8512              STA     I+HI            ; IN WORDS
 1A7A AD1B32            LDA     ZBEGIN+ZLENTH+1 ; FIRST MSB
 1A7D 8511              STA     I+LO            ; THEN LSB
                
 1A7F 0611              ASL     I+LO            ; CONVERT Z-CODE LENGTH
 1A81 2612              ROL     I+HI            ; TO # BYTES
 1A83 2615              ROL     K+LO            ; TOP BIT IN [K+LO]
                
 1A85 A916              LDA     #K+HI           ; PATCH THE "GETBYT" ROUTINE
 1A87 8DF121            STA     PATCH           ; TO USE [K+HI]=0 INSTEAD OF [ZPURE]
                
 1A8A 20E621    VSUM:   JSR     GETBYT          ; GET A Z-BYTE INTO [A]
 1A8D 18                CLC
 1A8E 6513              ADC     J+LO            ; ADD IT TO SUM
 1A90 8513              STA     J+LO            ; IN [J]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  31    
--- 0-OPS ---

 1A92 9002              BCC     VSUM0
 1A94 E614              INC     J+HI
                
 1A96 A51F      VSUM0:  LDA     MPCL            ; END OF Z-CODE YET?
 1A98 C511              CMP     I+LO            ; CHECK LSB
 1A9A D0EE              BNE     VSUM
                
 1A9C A520              LDA     MPCM            ; MIDDLE BYTE
 1A9E C512              CMP     I+HI
 1AA0 D0E8              BNE     VSUM
                
 1AA2 A521              LDA     MPCH            ; AND HIGH BIT
 1AA4 C515              CMP     K+LO
 1AA6 D0E2              BNE     VSUM
                
 1AA8 A927              LDA     #ZPURE          ; UNPATCH "GETBYT"
 1AAA 8DF121            STA     PATCH
                
 1AAD AD1D32            LDA     ZBEGIN+ZCHKSM+1 ; GET LSB OF CHECKSUM
 1AB0 C513              CMP     J+LO            ; DOES IT MATCH?
 1AB2 D00A              BNE     BADVER          ; NO, PREDICATE FAILS
                
 1AB4 AD1C32            LDA     ZBEGIN+ZCHKSM   ; ELSE CHECK MSB
 1AB7 C514              CMP     J+HI            ; LOOK GOOD?
 1AB9 D003              BNE     BADVER          ; IF MATCHED,
 1ABB 4C1219            JMP     PREDS           ; GAME IS OKAY
                
 1ABE 4C0619    BADVER: JMP     PREDF
                
                        END
                        INCLUD OPS1.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  32    
--- 1-OPS ---

                
 1AC1                   ; -----
 1AC1                   ; ZERO?
 1AC1                   ; -----
                
 1AC1                   ; [ARG1] = 0?
                
 1AC1 A505      ZZERO:  LDA     ARG1+LO
 1AC3 0506              ORA     ARG1+HI
 1AC5 F01C              BEQ     PFINE
                
 1AC7 4C0619    PYUCK:  JMP     PREDF
                
 1ACA                   ; -----
 1ACA                   ; NEXT?
 1ACA                   ; -----
                
 1ACA                   ; RETURN "NEXT" POINTER IN OBJECT [ARG1];
 1ACA                   ; FAIL IF LAST AND RETURN ZERO
                
 1ACA A505      ZNEXT:  LDA     ARG1+LO
 1ACC 20E424            JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
 1ACF A005              LDY     #5              ; POINT TO "NEXT" SLOT
 1AD1 D007              BNE     FIRST1
                
 1AD3                   ; ------
 1AD3                   ; FIRST?
 1AD3                   ; ------
                
 1AD3                   ; RETURN "FIRST" POINTER IN OBJECT [ARG1];
 1AD3                   ; FAIL IF LAST AND RETURN ZERO
                
 1AD3 A505      ZFIRST: LDA     ARG1+LO
 1AD5 20E424            JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
 1AD8 A006              LDY     #6              ; POINT TO "FIRST" SLOT
                
 1ADA B111      FIRST1: LDA     (I),Y           ; GET CONTENTS OF SLOT
 1ADC 20C418            JSR     PUTBYT          ; PASS IT TO VARIABLE
                
 1ADF A50F              LDA     VALUE+LO        ; EXAMINE THE VALUE JUST "PUT"
 1AE1 F0E4              BEQ     PYUCK           ; FAIL IF IT WAS ZERO
                
 1AE3 4C1219    PFINE:  JMP     PREDS           ; ELSE REJOICE
                
 1AE6                   ; ---
 1AE6                   ; LOC
 1AE6                   ; ---
                
 1AE6                   ; RETURN THE OBJECT CONTAINING OBJECT [ARG1];
 1AE6                   ; RETURN ZERO IF NONE
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  33    
--- 1-OPS ---

 1AE6 A505      ZLOC:   LDA     ARG1+LO
 1AE8 20E424            JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
 1AEB A004              LDY     #4              ; POINT TO "LOC" SLOT
 1AED B111              LDA     (I),Y           ; GET THE BYTE
 1AEF 4CC418            JMP     PUTBYT          ; AND SHIP IT OUT
                
 1AF2                   ; ------
 1AF2                   ; PTSIZE
 1AF2                   ; ------
                
 1AF2                   ; RETURN LENGTH OF PROP TABLE [ARG1] IN BYTES
                
 1AF2 A506      ZPTSIZ: LDA     ARG1+HI         ; MOVE ABS ADDR OF
 1AF4 18                CLC                     ; THE PROP TABLE
 1AF5 6526              ADC     ZCODE           ; INTO [I]
 1AF7 8512              STA     I+HI
                
 1AF9 A505              LDA     ARG1+LO         ; DECREMENT THE
 1AFB 38                SEC                     ; ADDRESS
 1AFC E901              SBC     #1              ; WHILE MOVING LSB
 1AFE 8511              STA     I+LO
 1B00 B002              BCS     PTZ0
 1B02 C612              DEC     I+HI
                
 1B04 A000      PTZ0:   LDY     #0              ; GET THE LENGTH
 1B06 202E25            JSR     PROPL           ; OF PROPERTY AT [I] INTO [A]
                
 1B09 18                CLC
 1B0A 6901              ADC     #1              ; INCREMENT RESULT
 1B0C 4CC418            JMP     PUTBYT          ; AND RETURN IT
                
 1B0F                   ; ---
 1B0F                   ; INC
 1B0F                   ; ---
                
 1B0F                   ; INCREMENT VARIABLE [ARG1]
                
 1B0F A505      ZINC:   LDA     ARG1+LO
 1B11 205D18            JSR     VARGET          ; FETCH VARIABLE INTO [VALUE]
 1B14 209219            JSR     INCVAL          ; INCREMENT IT
 1B17 4C221B            JMP     ZD0
                
 1B1A                   ; ---
 1B1A                   ; DEC
 1B1A                   ; ---
                
 1B1A                   ; DECREMENT VARIABLE [ARG1]
                
 1B1A A505      ZDEC:   LDA     ARG1+LO
 1B1C 205D18            JSR     VARGET          ; FETCH VAR INTO [VALUE]
 1B1F 208619            JSR     DECVAL          ; DECREMENT IT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  34    
--- 1-OPS ---

                
 1B22 A505      ZD0:    LDA     ARG1+LO         ; PUT RESULT BACK
 1B24 4CB918            JMP     VARPUT          ; INTO THE SAME VARIABLE
                
 1B27                   ; ------
 1B27                   ; PRINTB
 1B27                   ; ------
                
 1B27                   ; PRINT Z-STRING AT [ARG1]
                
 1B27 A505      ZPRB:   LDA     ARG1+LO
 1B29 8511              STA     I+LO
 1B2B A506              LDA     ARG1+HI
 1B2D 8512              STA     I+HI
                
 1B2F 20AD22            JSR     SETWRD          ; MOVE Z-ADDR TO [MPC]
 1B32 4CD922            JMP     PZSTR           ; AND PRINT
                
 1B35                   ; ------
 1B35                   ; REMOVE
 1B35                   ; ------
                
 1B35                   ; MOVE OBJECT [ARG1] INTO PSEUDO-OBJECT #0
                
 1B35 A505      ZREMOV: LDA     ARG1+LO         ; GET SOURCE OBJECT ADDR
 1B37 20E424            JSR     OBJLOC          ; INTO [I]
                
 1B3A A511              LDA     I+LO            ; COPY THE SOURCE ADDR
 1B3C 8513              STA     J+LO            ; INTO [J]
 1B3E A512              LDA     I+HI            ; FOR LATER REFERENCE
 1B40 8514              STA     J+HI
                
 1B42 A004              LDY     #4              ; POINT TO "LOC" SLOT
 1B44 B111              LDA     (I),Y           ; GET THE DATA
 1B46 F02E              BEQ     REMVEX          ; SCRAM IF NO OBJECT
                
 1B48 20E424            JSR     OBJLOC          ; ELSE GET ADDR OF OBJECT [A] INTO [I]
 1B4B A006              LDY     #6              ; POINT TO "FIRST" SLOT
 1B4D B111              LDA     (I),Y           ; GRAB DATA
 1B4F C505              CMP     ARG1+LO         ; IS THIS THE FIRST?
 1B51 D009              BNE     REMVC1          ; NO, KEEP SEARCHING
                
 1B53 A005              LDY     #5              ; ELSE COPY SOURCE'S "NEXT" SLOT
 1B55 B113              LDA     (J),Y
 1B57 C8                INY                     ; INTO DEST'S "FIRST" SLOT ([Y] = 6)
 1B58 9111              STA     (I),Y
 1B5A D011              BNE     REMVC2          ; BRANCH ALWAYS
                
 1B5C 20E424    REMVC1: JSR     OBJLOC
 1B5F A005              LDY     #5              ; GET "NEXT"
 1B61 B111              LDA     (I),Y


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  35    
--- 1-OPS ---

 1B63 C505              CMP     ARG1+LO         ; FOUND IT?
 1B65 D0F5              BNE     REMVC1          ; NO, KEEP TRYING
                
 1B67 A005              LDY     #5              ; WHEN FOUND
 1B69 B113              LDA     (J),Y           ; MOVE "NEXT" SLOT OF SOURCE
 1B6B 9111              STA     (I),Y           ; TO "NEXT" SLOT OF DEST
                
 1B6D A900      REMVC2: LDA     #0
 1B6F A004              LDY     #4              ; CLEAR "LOC"
 1B71 9113              STA     (J),Y
 1B73 C8                INY                     ; AND "NEXT" SLOTS ([Y] = 5)
 1B74 9113              STA     (J),Y           ; OF SOURCE OBJECT
                
 1B76 60        REMVEX: RTS
                
 1B77                   ; ------
 1B77                   ; PRINTD
 1B77                   ; ------
                
 1B77                   ; PRINT SHORT DESCRIPTION OF OBJECT [ARG1]
                
 1B77 A505      ZPRD:   LDA     ARG1+LO
                
 1B79                   ; ENTRY POINT FOR "USL"
                
 1B79 20E424    PRNTDC: JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
 1B7C A007              LDY     #7              ; GET PROP TABLE POINTER
 1B7E B111              LDA     (I),Y           ; FETCH MSB
 1B80 AA                TAX                     ; SAVE IT HERE
 1B81 C8                INY
 1B82 B111              LDA     (I),Y           ; FETCH LSB
 1B84 8511              STA     I+LO            ; STORE LSB
 1B86 8612              STX     I+HI            ; AND MSB
                
 1B88 E611              INC     I+LO            ; POINT PAST THE
 1B8A D002              BNE     PDC0            ; LENGTH BYTE
 1B8C E612              INC     I+HI
                
 1B8E 20AD22    PDC0:   JSR     SETWRD          ; CALC Z-STRING ADDR
 1B91 4CD922            JMP     PZSTR           ; AND PRINT IT
                
 1B94                   ; ------
 1B94                   ; RETURN
 1B94                   ; ------
                
 1B94                   ; RETURN FROM "CALL" WITH VALUE [ARG1]
                
 1B94 A518      ZRET:   LDA     OLDZSP          ; RE-SYNC THE
 1B96 8517              STA     ZSP             ; Z-STACK POINTER
                
 1B98 208C18            JSR     POPVAL          ; POP # LOCALS INTO [X/A]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  36    
--- 1-OPS ---

 1B9B 8612              STX     I+HI            ; SAVE HERE
 1B9D 8A                TXA                     ; SET FLAGS; ANY LOCALS?
 1B9E F019              BEQ     RET2            ; SKIP IF NOT
                
 1BA0                   ; RESTORE PUSHED LOCALS
                
 1BA0 CA                DEX                     ; ZERO-ALIGN
 1BA1 8A                TXA                     ; AND
 1BA2 0A                ASL     A               ; WORD-ALIGN # LOCALS
 1BA3 8511              STA     I+LO            ; FOR USE AS A STORAGE INDEX
                
 1BA5 208C18    RET1:   JSR     POPVAL          ; POP A LOCAL INTO [X/A]
                
 1BA8 A411              LDY     I+LO            ; RETRIEVE STORAGE INDEX
 1BAA 990115            STA     LOCALS+HI,Y     ; STORE MSB OF LOCAL
 1BAD 8A                TXA                     ; MOVE LSB
 1BAE 990015            STA     LOCALS+LO,Y     ; AND STORE THAT TOO
                
 1BB1 C611              DEC     I+LO
 1BB3 C611              DEC     I+LO            ; UPDATE STORAGE INDEX
                
 1BB5 C612              DEC     I+HI            ; AND LOCALS COUNT
 1BB7 D0EC              BNE     RET1            ; POP TILL NO MORE LOCALS
                
 1BB9                   ; RESTORE OTHER VARIABLES
                
 1BB9 208C18    RET2:   JSR     POPVAL          ; POP [ZPCH] AND [ZPCM]
 1BBC 861A              STX     ZPCM
 1BBE 851B              STA     ZPCH
                
 1BC0 208C18            JSR     POPVAL          ; POP AND RESTORE
 1BC3 8618              STX     OLDZSP
 1BC5 8519              STA     ZPCL
                
 1BC7 A900              LDA     #0
 1BC9 851C              STA     ZPCFLG          ; ZPC CHANGED!
                
 1BCB 209919            JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 1BCE 4CCA18            JMP     PUTVAL          ; AND RETURN IT
                
 1BD1                   ; ----
 1BD1                   ; JUMP
 1BD1                   ; ----
                
 1BD1                   ; JUMP TO Z-LOCATION IN [ARG1]
                
 1BD1 209919    ZJUMP:  JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 1BD4 4C4C19            JMP     PREDB3          ; A BRANCH THAT ALWAYS SUCCEEDS
                
 1BD7                   ; -----
 1BD7                   ; PRINT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  37    
--- 1-OPS ---

 1BD7                   ; -----
                
 1BD7                   ; PRINT Z-STRING AT WORD POINTER [ARG1]
                
 1BD7 A505      ZPRINT: LDA     ARG1+LO
 1BD9 8511              STA     I+LO
 1BDB A506              LDA     ARG1+HI
 1BDD 8512              STA     I+HI
                
 1BDF 20C722            JSR     SETSTR          ; CALC STRING ADDRESS
 1BE2 4CD922            JMP     PZSTR           ; AND PRINT IT
                
 1BE5                   ; -----
 1BE5                   ; VALUE
 1BE5                   ; -----
                
 1BE5                   ; RETURN VALUE OF VARIABLE [ARG1]
                
 1BE5 A505      ZVALUE: LDA     ARG1+LO
 1BE7 205D18            JSR     VARGET          ; GET THE VALUE
 1BEA 4CCA18            JMP     PUTVAL          ; EASY ENOUGH
                
 1BED                   ; ----
 1BED                   ; BCOM
 1BED                   ; ----
                
 1BED                   ; COMPLEMENT [ARG1]
                
 1BED A505      ZBCOM:  LDA     ARG1+LO
 1BEF 49FF              EOR     #$FF
 1BF1 AA                TAX
 1BF2 A506              LDA     ARG1+HI
 1BF4 49FF              EOR     #$FF
                
 1BF6                   ; FALL THROUGH ...
                
 1BF6                   ; ---------------------
 1BF6                   ; RETURN VALUE IN [X/A]
 1BF6                   ; ---------------------
                
 1BF6 860F      VEXIT:  STX     VALUE+LO
 1BF8 8510              STA     VALUE+HI
 1BFA 4CCA18            JMP     PUTVAL
                
                        END
                        INCLUD OPS2.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  38    
--- 2-OPS ---

                
 1BFD                   ; -----
 1BFD                   ; LESS?
 1BFD                   ; -----
                
 1BFD                   ; [ARG1] < [ARG2]?
                
 1BFD 209919    ZLESS:  JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 1C00 4C061C            JMP     DLS0            ; MOVE [ARG2] TO [I] & COMPARE
                
 1C03                   ; ------
 1C03                   ; DLESS?
 1C03                   ; ------
                
 1C03                   ; DECREMENT [ARG1]; SUCCEED IF < [ARG2]
                
 1C03 201A1B    ZDLESS: JSR     ZDEC            ; MOVES ([ARG1]-1) TO [VALUE]
                
 1C06 A507      DLS0:   LDA     ARG2+LO         ; MOVE [ARG2] TO [I]
 1C08 8511              STA     I+LO
 1C0A A508              LDA     ARG2+HI
 1C0C 8512              STA     I+HI
                
 1C0E 4C2F1C            JMP     COMPAR          ; COMPARE & RETURN
                
 1C11                   ; -----
 1C11                   ; GRTR?
 1C11                   ; -----
                
 1C11                   ; [ARG1] > [ARG2]?
                
 1C11 A505      ZGRTR:  LDA     ARG1+LO         ; MOVE [ARG1] TO [I]
 1C13 8511              STA     I+LO
 1C15 A506              LDA     ARG1+HI
 1C17 8512              STA     I+HI
                
 1C19 4C271C            JMP     A2VAL           ; MOVE [ARG2] TO [VALUE] & COMPARE
                
 1C1C                   ; ------
 1C1C                   ; IGRTR?
 1C1C                   ; ------
                
 1C1C                   ; INCREMENT [ARG1]; SUCCEED IF GREATER THAN [ARG2]
                
 1C1C 200F1B    ZIGRTR: JSR     ZINC            ; GET ([ARG1]+1) INTO [VALUE]
                
 1C1F A50F              LDA     VALUE+LO        ; MOVE [VALUE] TO [I]
 1C21 8511              STA     I+LO
 1C23 A510              LDA     VALUE+HI
 1C25 8512              STA     I+HI
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  39    
--- 2-OPS ---

 1C27 A507      A2VAL:  LDA     ARG2+LO         ; MOVE [ARG2] TO [VALUE]
 1C29 850F              STA     VALUE+LO
 1C2B A508              LDA     ARG2+HI
 1C2D 8510              STA     VALUE+HI
                
 1C2F 20361C    COMPAR: JSR     SCOMP           ; COMPARE [VALUE] AND [I]
 1C32 9038              BCC     PGOOD
 1C34 B023              BCS     PBAD
                
 1C36                   ; -----------------
 1C36                   ; SIGNED COMPARISON
 1C36                   ; -----------------
                
 1C36                   ; ENTRY: VALUES IN [VALUE] AND [I]
                
 1C36 A512      SCOMP:  LDA     I+HI
 1C38 4510              EOR     VALUE+HI
 1C3A 1005              BPL     SCMP
 1C3C A512              LDA     I+HI
 1C3E C510              CMP     VALUE+HI
 1C40 60                RTS
                
 1C41 A510      SCMP:   LDA     VALUE+HI
 1C43 C512              CMP     I+HI
 1C45 D004              BNE     SCEX
 1C47 A50F              LDA     VALUE+LO
 1C49 C511              CMP     I+LO
 1C4B 60        SCEX:   RTS
                
 1C4C                   ; ---
 1C4C                   ; IN?
 1C4C                   ; ---
                
 1C4C                   ; IS OBJECT [ARG1] CONTAINED IN OBJECT [ARG2]?
                
 1C4C A505      ZIN:    LDA     ARG1+LO
 1C4E 20E424            JSR     OBJLOC          ; GET ADDR OF TARGET OBJECT INTO [I]
                
 1C51 A004              LDY     #4              ; POINT TO "LOC" SLOT
 1C53 B111              LDA     (I),Y           ; GET DATA
 1C55 C507              CMP     ARG2+LO         ; IS IT THERE?
 1C57 F013              BEQ     PGOOD           ; YES, SUCCEED
                
 1C59 4C0619    PBAD:   JMP     PREDF           ; TOO BAD, CHUM ...
                
 1C5C                   ; ----
 1C5C                   ; BTST
 1C5C                   ; ----
                
 1C5C                   ; IS EVERY "ON" BIT IN [ARG1]
 1C5C                   ; ALSO "ON" IN [ARG2]?


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  40    
--- 2-OPS ---

                
 1C5C A507      ZBTST:  LDA     ARG2+LO         ; FIRST CHECK LSBS
 1C5E 2505              AND     ARG1+LO
 1C60 C507              CMP     ARG2+LO         ; LSBS MATCH?
 1C62 D0F5              BNE     PBAD            ; NO, EXIT NOW
                
 1C64 A508              LDA     ARG2+HI         ; ELSE CHECK MSBS
 1C66 2506              AND     ARG1+HI
 1C68 C508              CMP     ARG2+HI         ; MATCHED?
 1C6A D0ED              BNE     PBAD            ; SORRY ...
                
 1C6C 4C1219    PGOOD:  JMP     PREDS
                
 1C6F                   ; ---
 1C6F                   ; BOR
 1C6F                   ; ---
                
 1C6F                   ; RETURN [ARG1] "OR" [ARG2]
                
 1C6F A505      ZBOR:   LDA     ARG1+LO
 1C71 0507              ORA     ARG2+LO
 1C73 AA                TAX
 1C74 A506              LDA     ARG1+HI
 1C76 0508              ORA     ARG2+HI
 1C78 4CF61B            JMP     VEXIT
                
 1C7B                   ; ----
 1C7B                   ; BAND
 1C7B                   ; ----
                
 1C7B                   ; RETURN [ARG1] "AND" [ARG2]
                
 1C7B A505      ZBAND:  LDA     ARG1+LO
 1C7D 2507              AND     ARG2+LO
 1C7F AA                TAX
 1C80 A506              LDA     ARG1+HI
 1C82 2508              AND     ARG2+HI
 1C84 4CF61B            JMP     VEXIT
                
 1C87                   ; -----
 1C87                   ; FSET?
 1C87                   ; -----
                
 1C87                   ; IS FLAG [ARG1] SET IN OBJECT [ARG2]?
                
 1C87 204025    ZFSETP: JSR     FLAGSU          ; GET BITS INTO [K] AND [J]
 1C8A A516              LDA     K+HI            ; DO MSBS
 1C8C 2514              AND     J+HI
 1C8E 8516              STA     K+HI
                
 1C90 A515              LDA     K+LO            ; DO LSBS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  41    
--- 2-OPS ---

 1C92 2513              AND     J+LO
                
 1C94 0516              ORA     K+HI            ; ANY BITS ON?
 1C96 D0D4              BNE     PGOOD           ; TARGET BIT MUST BE ON
 1C98 4C0619            JMP     PREDF
                
 1C9B                   ; ----
 1C9B                   ; FSET
 1C9B                   ; ----
                
 1C9B                   ; SET FLAG [ARG2] IN OBJECT [ARG1]
                
 1C9B 204025    ZFSET:  JSR     FLAGSU          ; GET BITS INTO [K] & [J], ADDR IN [I]
                
 1C9E A000              LDY     #0
 1CA0 A516              LDA     K+HI            ; FIRST DO MSBS
 1CA2 0514              ORA     J+HI
 1CA4 9111              STA     (I),Y
                
 1CA6 C8                INY
 1CA7 A515              LDA     K+LO            ; THEN LSBS
 1CA9 0513              ORA     J+LO
 1CAB 9111              STA     (I),Y
 1CAD 60                RTS
                
 1CAE                   ; ------
 1CAE                   ; FCLEAR
 1CAE                   ; ------
                
 1CAE                   ; CLEAR FLAG [ARG2] IN OBJECT [ARG1]
                
 1CAE 204025    ZFCLR:  JSR     FLAGSU          ; GETS BITS INTO [J] & [K], ADDR IN [I]
                
 1CB1 A000              LDY     #0
 1CB3 A514              LDA     J+HI            ; FETCH MSB
 1CB5 49FF              EOR     #$FF            ; COMPLEMENT IT
 1CB7 2516              AND     K+HI            ; RUB OUT FLAG
 1CB9 9111              STA     (I),Y
                
 1CBB C8                INY
 1CBC A513              LDA     J+LO            ; SAME FOR LSB
 1CBE 49FF              EOR     #$FF
 1CC0 2515              AND     K+LO
 1CC2 9111              STA     (I),Y
 1CC4 60                RTS
                
 1CC5                   ; ---
 1CC5                   ; SET
 1CC5                   ; ---
                
 1CC5                   ; SET VARIABLE [ARG1] EQUAL TO [ARG2]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  42    
--- 2-OPS ---

                
 1CC5 A507      ZSET:   LDA     ARG2+LO         ; MOVE THE VALUE
 1CC7 850F              STA     VALUE+LO        ; INTO [VALUE]
 1CC9 A508              LDA     ARG2+HI
 1CCB 8510              STA     VALUE+HI
                
 1CCD A505              LDA     ARG1+LO         ; GET VARIABLE ID
 1CCF 4CB918            JMP     VARPUT          ; AND CHANGE THE VARIABLE
                
 1CD2                   ; ----
 1CD2                   ; MOVE
 1CD2                   ; ----
                
 1CD2                   ; MOVE OBJECT [ARG1] INTO OBJECT [ARG2]
                
 1CD2 20351B    ZMOVE:  JSR     ZREMOV          ; REMOVE FIRST
                
 1CD5 A505              LDA     ARG1+LO
 1CD7 20E424            JSR     OBJLOC          ; GET SOURCE OBJECT ADDR INTO [I]
                
 1CDA A511              LDA     I+LO            ; COPY SOURCE ADDRESS
 1CDC 8513              STA     J+LO            ; INTO [J]
 1CDE A512              LDA     I+HI
 1CE0 8514              STA     J+HI
                
 1CE2 A507              LDA     ARG2+LO         ; GET DEST OBJECT ID
 1CE4 A004              LDY     #4              ; POINT TO "LOC" SLOT OF SOURCE
 1CE6 9111              STA     (I),Y           ; AND MOVE IT IN
                
 1CE8 20E424            JSR     OBJLOC          ; GET ADDR OF DEST OBJECT INTO [I]
                
 1CEB A006              LDY     #6              ; POINT TO "FIRST" SLOT
 1CED B111              LDA     (I),Y           ; GET "FIRST" OF DEST
 1CEF AA                TAX                     ; SAVE HERE FOR A MOMENT
                
 1CF0 A505              LDA     ARG1+LO         ; GET SOURCE OBJECT ID
 1CF2 9111              STA     (I),Y           ; MAKE IT "FIRST" OF DEST
                
 1CF4 8A                TXA                     ; RESTORE "FIRST" OF DEST
 1CF5 F004              BEQ     ZMVEX           ; SCRAM IF ZERO
                
 1CF7 A005              LDY     #5              ; MAKE "FIRST" OF DEST
 1CF9 9113              STA     (J),Y           ; THE "NEXT" OF SOURCE
                
 1CFB 60        ZMVEX:  RTS
                
 1CFC                   ; ---
 1CFC                   ; GET
 1CFC                   ; ---
                
 1CFC                   ; RETURN ITEM [ARG2] IN WORD-TABLE [ARG1]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  43    
--- 2-OPS ---

                
 1CFC 20111D    ZGET:   JSR     WCALC           ; CALC ADDRESS
 1CFF 20E621            JSR     GETBYT          ; GET 1ST BYTE (MSB)
                
 1D02 8510      DOGET:  STA     VALUE+HI        ; SAVE MSB
 1D04 20E621            JSR     GETBYT          ; GET LSB
 1D07 850F              STA     VALUE+LO        ; SAVE AND
 1D09 4CCA18            JMP     PUTVAL          ; HAND IT OVER
                
 1D0C                   ; ----
 1D0C                   ; GETB
 1D0C                   ; ----
                
 1D0C                   ; RETURN ITEM [ARG2] IN BYTE-TABLE AT [ARG1]
                
 1D0C 20151D    ZGETB:  JSR     BCALC
 1D0F F0F1              BEQ     DOGET           ; [A] = 0, SO CLEAR MSB OF [VALUE]
                
 1D11                   ; --------------------
 1D11                   ; CALC TABLE ADDRESSES
 1D11                   ; --------------------
                
 1D11                   ; WORD-ALIGNED ENTRY
                
 1D11 0607      WCALC:  ASL     ARG2+LO         ; WORD-ALIGN FOR
 1D13 2608              ROL     ARG2+HI         ; WORD ACCESS
                
 1D15                   ; BYTE-ALIGNED ENTRY
                
 1D15 A507      BCALC:  LDA     ARG2+LO         ; ADD BASE ADDR OF TABLE
 1D17 18                CLC                     ; TO ITEM
 1D18 6505              ADC     ARG1+LO         ; INDEX
 1D1A 851F              STA     MPCL
                
 1D1C A508              LDA     ARG2+HI         ; SAME FOR MSBS
 1D1E 6506              ADC     ARG1+HI
 1D20 8520              STA     MPCM
                
 1D22 A900              LDA     #0
 1D24 8521              STA     MPCH            ; CLEAR TOP BIT
 1D26 8522              STA     MPCFLG          ; & INVALIDATE [MPC]
 1D28 60                RTS
                
 1D29                   ; ----
 1D29                   ; GETP
 1D29                   ; ----
                
 1D29                   ; RETURN PROPERTY [ARG2] OF OBJECT [ARG1];
 1D29                   ; IF NO PROP [ARG2], RETURN [ARG2]'TH ELEMENT OF OBJECT #0
                
 1D29 200D25    ZGETP:  JSR     PROPB


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  44    
--- 2-OPS ---

                
 1D2C 202925    GETP1:  JSR     PROPN
 1D2F C507              CMP     ARG2+LO
 1D31 F01B              BEQ     GETP3
 1D33 9006              BCC     GETP2
                
 1D35 203625            JSR     PROPNX
 1D38 4C2C1D            JMP     GETP1           ; TRY AGAIN WITH NEXT PROP
                
 1D3B A507      GETP2:  LDA     ARG2+LO         ; GET PROPERTY #
 1D3D 38                SEC                     ; ZERO-ALIGN IT
 1D3E E901              SBC     #1
 1D40 0A                ASL     A               ; WORD-ALIGN IT
 1D41 A8                TAY                     ; USE AS AN INDEX
 1D42 B135              LDA     (OBJTAB),Y      ; GET MSB OF PROPERTY
 1D44 8510              STA     VALUE+HI
 1D46 C8                INY
 1D47 B135              LDA     (OBJTAB),Y      ; DO SAME WITH LSB
 1D49 850F              STA     VALUE+LO
 1D4B 4CCA18            JMP     PUTVAL          ; RETURN DEFAULT IN [VALUE]
                
 1D4E 202E25    GETP3:  JSR     PROPL
 1D51 C8                INY                     ; MAKE [Y] POINT TO 1ST BYTE OF PROP
 1D52 AA                TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
 1D53 F009              BEQ     GETPB           ; GET A BYTE PROPERTY
 1D55 C901              CMP     #1              ; IF LENGTH = 1
 1D57 F00B              BEQ     GETPW           ; GET A WORD PROPERTY
                
 1D59                   ; *** ERROR #7: PROPERTY LENGTH ***
                
 1D59 A907              LDA     #7
 1D5B 4C9025            JMP     ZERROR
                
 1D5E                   ; GET A 1-BYTE PROPERTY
                
 1D5E B111      GETPB:  LDA     (I),Y           ; GET LSB INTO [A]
 1D60 A200              LDX     #0              ; CLEAR MSB IN [X]
 1D62 F006              BEQ     ETPEX
                
 1D64                   ; GET A 2-BYTE PROPERTY
                
 1D64 B111      GETPW:  LDA     (I),Y           ; GET MSB
 1D66 AA                TAX                     ; INTO [X]
 1D67 C8                INY                     ; POINT TO LSB
 1D68 B111              LDA     (I),Y           ; GET IT INTO [A]
                
 1D6A 850F      ETPEX:  STA     VALUE+LO        ; STORE LSB
 1D6C 8610              STX     VALUE+HI        ; AND MSB
 1D6E 4CCA18            JMP     PUTVAL
                
 1D71                   ; -----


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  45    
--- 2-OPS ---

 1D71                   ; GETPT
 1D71                   ; -----
                
 1D71                   ; RETURN POINTER TO PROP TABLE [ARG2]
 1D71                   ; IN OBJECT [ARG1]
                
 1D71 200D25    ZGETPT: JSR     PROPB
                
 1D74 202925    GETPT1: JSR     PROPN           ; RETURNS OFFSET IN [Y]
 1D77 C507              CMP     ARG2+LO
 1D79 F008              BEQ     GETPT2
 1D7B 901E              BCC     DORET
 1D7D 203625            JSR     PROPNX          ; TRY NEXT PROPERTY
 1D80 4C741D            JMP     GETPT1
                
 1D83 E611      GETPT2: INC     I+LO
 1D85 D002              BNE     GETPT3
 1D87 E612              INC     I+HI
                
 1D89 98        GETPT3: TYA                     ; FETCH OFFSET
 1D8A 18                CLC
 1D8B 6511              ADC     I+LO            ; ADD LSB OF TABLE ADDRESS
 1D8D 850F              STA     VALUE+LO
                
 1D8F A512              LDA     I+HI            ; AND MSB
 1D91 6900              ADC     #0
 1D93 38                SEC                     ; STRIP OFF
 1D94 E526              SBC     ZCODE           ; RELATIVE POINTER
 1D96 8510              STA     VALUE+HI
 1D98 4CCA18            JMP     PUTVAL          ; AND RETURN
                
 1D9B 4CC218    DORET:  JMP     RET0            ; ELSE RETURN A ZERO
                
 1D9E                   ; -----
 1D9E                   ; NEXTP
 1D9E                   ; -----
                
 1D9E                   ; RETURN INDEX # OF PROP FOLLOWING PROP [ARG2] IN OBJECT [ARG1];
 1D9E                   ; RETURN ZERO IF LAST; RETURN FIRST IF [ARG2]=0; ERROR IF NONE
                
 1D9E 200D25    ZNEXTP: JSR     PROPB
 1DA1 A507              LDA     ARG2+LO         ; IF [ARG2]=0
 1DA3 F012              BEQ     NXTP3           ; RETURN "FIRST" SLOT
                
 1DA5 202925    NXTP1:  JSR     PROPN           ; FETCH PROPERTY #
 1DA8 C507              CMP     ARG2+LO         ; COMPARE TO TARGET #
 1DAA F008              BEQ     NXTP2           ; FOUND IT!
 1DAC 90ED              BCC     DORET           ; LAST PROP, SO RETURN ZERO
 1DAE 203625            JSR     PROPNX          ; ELSE TRY NEXT PROPERTY
 1DB1 4CA51D            JMP     NXTP1
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  46    
--- 2-OPS ---

 1DB4 203625    NXTP2:  JSR     PROPNX          ; POINT TO FOLLOWING PROPERTY
                
 1DB7 202925    NXTP3:  JSR     PROPN           ; GET THE PROPERTY #
 1DBA 4CC418            JMP     PUTBYT          ; AND RETURN IT
                
 1DBD                   ; ---
 1DBD                   ; ADD
 1DBD                   ; ---
                
 1DBD                   ; RETURN [ARG1] + [ARG2]
                
 1DBD A505      ZADD:   LDA     ARG1+LO         ; ADD LSBS
 1DBF 18                CLC
 1DC0 6507              ADC     ARG2+LO
 1DC2 AA                TAX                     ; SAVE LSB HERE
 1DC3 A506              LDA     ARG1+HI         ; ADD MSBS
 1DC5 6508              ADC     ARG2+HI
 1DC7 4CF61B            JMP     VEXIT
                
 1DCA                   ; ---
 1DCA                   ; SUB
 1DCA                   ; ---
                
 1DCA                   ; RETURN [ARG1] - [ARG2]
                
 1DCA A505      ZSUB:   LDA     ARG1+LO         ; SUBTRACT LSBS
 1DCC 38                SEC
 1DCD E507              SBC     ARG2+LO
 1DCF AA                TAX                     ; SAVE LSB HERE
 1DD0 A506              LDA     ARG1+HI         ; SUBTRACT MSBS
 1DD2 E508              SBC     ARG2+HI
 1DD4 4CF61B            JMP     VEXIT           ; EXIT WITH [X]=LSB, [A]=MSB
                
 1DD7                   ; ---
 1DD7                   ; MUL
 1DD7                   ; ---
                
 1DD7                   ; RETURN [ARG1] * [ARG2]
                
 1DD7 20911E    ZMUL:   JSR     MINIT           ; INIT THINGS
                
 1DDA 665B      ZMLOOP: ROR     MTEMP+HI
 1DDC 665A              ROR     MTEMP+LO
 1DDE 6608              ROR     ARG2+HI
 1DE0 6607              ROR     ARG2+LO
 1DE2 900D              BCC     ZMNEXT
                
 1DE4 A505              LDA     ARG1+LO
 1DE6 18                CLC
 1DE7 655A              ADC     MTEMP+LO
 1DE9 855A              STA     MTEMP+LO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  47    
--- 2-OPS ---

 1DEB A506              LDA     ARG1+HI
 1DED 655B              ADC     MTEMP+HI
 1DEF 855B              STA     MTEMP+HI
                
 1DF1 CA        ZMNEXT: DEX
 1DF2 10E6              BPL     ZMLOOP
                
 1DF4 A607              LDX     ARG2+LO         ; PUT LSB OF PRODUCT
 1DF6 A508              LDA     ARG2+HI         ; AND MSB
 1DF8 4CF61B            JMP     VEXIT           ; WHERE "VEXIT" EXPECTS THEM
                
 1DFB                   ; ---
 1DFB                   ; DIV
 1DFB                   ; ---
                
 1DFB                   ; RETURN QUOTIENT OF [ARG1] / [ARG2]
                
 1DFB 200F1E    ZDIV:   JSR     DIVIDE
 1DFE A656              LDX     QUOT+LO
 1E00 A557              LDA     QUOT+HI
 1E02 4CF61B            JMP     VEXIT
                
 1E05                   ; ---
 1E05                   ; MOD
 1E05                   ; ---
                
 1E05                   ; RETURN REMAINDER OF [ARG1] / [ARG2]
                
 1E05 200F1E    ZMOD:   JSR     DIVIDE
 1E08 A658              LDX     REMAIN+LO       ; FETCH THE REMAINDER
 1E0A A559              LDA     REMAIN+HI       ; IN [REMAIN]
 1E0C 4CF61B            JMP     VEXIT           ; AND RETURN IT
                
 1E0F                   ; ---------------
 1E0F                   ; SIGNED DIVISION
 1E0F                   ; ---------------
                
 1E0F                   ; ENTRY: DIVIDEND IN [ARG1], DIVISOR IN [ARG2]
 1E0F                   ; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]
                
 1E0F A506      DIVIDE: LDA     ARG1+HI         ; SIGN OF REMAINDER
 1E11 855D              STA     RSIGN           ; IS THE SIGN OF THE DIVIDEND
 1E13 4508              EOR     ARG2+HI         ; SIGN OF QUOTIENT IS POSITIVE
 1E15 855C              STA     QSIGN           ; IF SIGNS OF TERMS ARE THE SAME
                
 1E17 A505              LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
 1E19 8556              STA     QUOT+LO
 1E1B A506              LDA     ARG1+HI
 1E1D 8557              STA     QUOT+HI         ; IF DIVIDEND IS POSITIVE
 1E1F 1003              BPL     ABSDIV          ; MOVE DIVISOR
 1E21 204D1E            JSR     ABQUOT          ; ELSE CALC ABS(DIVIDEND) FIRST


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  48    
--- 2-OPS ---

                
 1E24 A507      ABSDIV: LDA     ARG2+LO
 1E26 8558              STA     REMAIN+LO
 1E28 A508              LDA     ARG2+HI
 1E2A 8559              STA     REMAIN+HI       ; IF REMAINDER IS POSITIVE
 1E2C 1003              BPL     GODIV           ; WE'RE READY TO DIVIDE
 1E2E 203F1E            JSR     ABREM           ; ELSE CALC ABS(DIVISOR)
                
 1E31 205B1E    GODIV:  JSR     UDIV            ; DO UNSIGNED DIVIDE
                
 1E34 A55C              LDA     QSIGN           ; SHOULD QUOTIENT BE FLIPPED?
 1E36 1003              BPL     RFLIP           ; NO, TEST REMAINDER
 1E38 204D1E            JSR     ABQUOT          ; ELSE GET ABSOLUTE VALUE
                
 1E3B A55D      RFLIP:  LDA     RSIGN           ; SHOULD EMAINDER BE FLIPPED?
 1E3D 100D              BPL     DIVEX           ; NO, WE'RE DONE
                
 1E3F                   ; ELSE FALL THROUGH ...
                
 1E3F                   ; ----------------
 1E3F                   ; CALC ABS(REMAIN)
 1E3F                   ; ----------------
                
 1E3F A900      ABREM:  LDA     #0
 1E41 38                SEC
 1E42 E558              SBC     REMAIN+LO
 1E44 8558              STA     REMAIN+LO
 1E46 A900              LDA     #0
 1E48 E559              SBC     REMAIN+HI
 1E4A 8559              STA     REMAIN+HI
                
 1E4C 60        DIVEX:  RTS
                
 1E4D                   ; --------------
 1E4D                   ; CALC ABS(QUOT)
 1E4D                   ; --------------
                
 1E4D A900      ABQUOT: LDA     #0
 1E4F 38                SEC
 1E50 E556              SBC     QUOT+LO
 1E52 8556              STA     QUOT+LO
 1E54 A900              LDA     #0
 1E56 E557              SBC     QUOT+HI
 1E58 8557              STA     QUOT+HI
 1E5A 60                RTS
                
 1E5B                   ; -----------------
 1E5B                   ; UNSIGNED DIVISION
 1E5B                   ; -----------------
                
 1E5B                   ; ENTRY: DIVIDEND IN [QUOT], DIVISOR IN [REMAIN]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  49    
--- 2-OPS ---

 1E5B                   ; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]
                
 1E5B A558      UDIV:   LDA     REMAIN+LO       ; CHECK [REMAIN]
 1E5D 0559              ORA     REMAIN+HI       ; BEFORE PROCEEDING
 1E5F F02B              BEQ     DIVERR          ; CAN'T DIVIDE BY ZERO!
                
 1E61 20911E            JSR     MINIT           ; SET IT ALL UP
                
 1E64 2656      UDLOOP: ROL     QUOT+LO
 1E66 2657              ROL     QUOT+HI
 1E68 265A              ROL     MTEMP+LO
 1E6A 265B              ROL     MTEMP+HI
                
 1E6C A55A              LDA     MTEMP+LO
 1E6E 38                SEC
 1E6F E558              SBC     REMAIN+LO
 1E71 A8                TAY                     ; SAVE HERE
 1E72 A55B              LDA     MTEMP+HI
 1E74 E559              SBC     REMAIN+HI
 1E76 9004              BCC     UDNEXT
 1E78 845A              STY     MTEMP+LO
 1E7A 855B              STA     MTEMP+HI
                
 1E7C CA        UDNEXT: DEX
 1E7D D0E5              BNE     UDLOOP
                
 1E7F 2656              ROL     QUOT+LO         ; SHIFT LAST CARRY FOR QUOTIENT
 1E81 2657              ROL     QUOT+HI
                
 1E83 A55A              LDA     MTEMP+LO        ; MOVE REMAINDER
 1E85 8558              STA     REMAIN+LO       ; INTO [REMAIN]
 1E87 A55B              LDA     MTEMP+HI
 1E89 8559              STA     REMAIN+HI
 1E8B 60                RTS
                
 1E8C                   ; *** ERROR #8: DIVISION BY ZERO ***
                
 1E8C A908      DIVERR: LDA     #8
 1E8E 4C9025            JMP     ZERROR
                
 1E91                   ; ---------
 1E91                   ; MATH INIT
 1E91                   ; ---------
                
 1E91 A210      MINIT:  LDX     #16             ; INIT LOOPING INDEX
 1E93 A900              LDA     #0
 1E95 855A              STA     MTEMP+LO        ; CLEAR TEMP
 1E97 855B              STA     MTEMP+HI        ; REGISTER
 1E99 18                CLC                     ; AND CARRY
 1E9A 60                RTS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  50    
--- 2-OPS ---

                        END
                        INCLUD OPSX.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  51    
--- X-OPS ---

                
 1E9B                   ; ------
 1E9B                   ; EQUAL?
 1E9B                   ; ------
                
 1E9B                   ; IS [ARG1] = [ARG2] (OR [ARG3] OR [ARG4])?
                
 1E9B C604      ZEQUAL: DEC     NARGS           ; DOUBLE-CHECK # ARGS
 1E9D D005              BNE     DOEQ            ; MUST BE AT LEAST TWO, OR ...
                
 1E9F                   ; *** ERROR #9: NOT ENOUGH "EQUAL?" ARGS ***
                
 1E9F A909              LDA     #9
 1EA1 4C9025            JMP     ZERROR
                
 1EA4 A505      DOEQ:   LDA     ARG1+LO         ; FETCH LSB
 1EA6 A606              LDX     ARG1+HI         ; AND MSB OF [ARG1]
                
 1EA8 C507              CMP     ARG2+LO         ; TEST LSB OF [ARG2]
 1EAA D004              BNE     TRY2            ; NO GOOD, LOOK FOR ANOTHER ARG
 1EAC E408              CPX     ARG2+HI         ; ELSE TRY MSB OF [ARG2]
 1EAE F018              BEQ     EQOK            ; MATCHED!
                
 1EB0 C604      TRY2:   DEC     NARGS           ; OUT OF ARGS YET?
 1EB2 F017              BEQ     EQBAD           ; YES, WE FAILED
                
 1EB4 C509              CMP     ARG3+LO         ; TRY LSB OF [ARG3]
 1EB6 D004              BNE     TRY3            ; NO GOOD, LOOK FOR ANOTHER ARG
 1EB8 E40A              CPX     ARG3+HI         ; HOW ABOUT MSB OF [ARG3]?
 1EBA F00C              BEQ     EQOK            ; YAY!
                
 1EBC C604      TRY3:   DEC     NARGS           ; OUT OF ARGS YET?
 1EBE F00B              BEQ     EQBAD           ; IF NOT ...
                
 1EC0 C50B              CMP     ARG4+LO         ; TRY [ARG4]
 1EC2 D007              BNE     EQBAD           ; SORRY, CHUM
 1EC4 E40C              CPX     ARG4+HI         ; MSB MATCHED?
 1EC6 D003              BNE     EQBAD           ; TOO BAD
                
 1EC8 4C1219    EQOK:   JMP     PREDS           ; FINALLY MATCHED!
                
 1ECB 4C0619    EQBAD:  JMP     PREDF           ; FAILURE (SNIFF!)
                
 1ECE                   ; ----
 1ECE                   ; CALL
 1ECE                   ; ----
                
 1ECE                   ; BRANCH TO FUNCTION AT ([ARG1]*2), PASSING
 1ECE                   ; OPTIONAL PARAMETERS IN [ARG2]-[ARG4]
                
 1ECE A505      ZCALL:  LDA     ARG1+LO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  52    
--- X-OPS ---

 1ED0 0506              ORA     ARG1+HI         ; IS CALL ADDRESS ZERO?
 1ED2 D003              BNE     DOCALL          ; NO, CONTINUE
                
 1ED4 4CC418            JMP     PUTBYT          ; ELSE RETURN THE ZERO IN [A]
                
 1ED7 A618      DOCALL: LDX     OLDZSP          ; SAVE OLD STACK POINTER
 1ED9 A519              LDA     ZPCL            ; AND LSB OF [ZPC]
 1EDB 20A618            JSR     PUSHXA          ; ON THE Z-STACK
                
 1EDE A61A              LDX     ZPCM            ; SAVE MIDDLE 8 BITS
 1EE0 A51B              LDA     ZPCH            ; AND TOP BIT OF [ZPC]
 1EE2 20A618            JSR     PUSHXA          ; AS WELL
                
 1EE5                   ; FORM 16-BIT ADDRESS FROM [ARG1]
                
 1EE5 A900              LDA     #0              ; CLEAR HIGH BIT FOR ROTATE
 1EE7 851C              STA     ZPCFLG          ; AND INVALIDATE [ZPC]
                
 1EE9 0605              ASL     ARG1+LO         ; MULTIPLY [ARG1]
 1EEB 2606              ROL     ARG1+HI         ; BY TWO
 1EED 2A                ROL     A               ; HIGH BIT INTO [A]
 1EEE 851B              STA     ZPCH            ; NEW HIGH BIT OF [ZPC]
                
 1EF0 A506              LDA     ARG1+HI         ; GET NEW LOW BYTES
 1EF2 851A              STA     ZPCM
 1EF4 A505              LDA     ARG1+LO
 1EF6 8519              STA     ZPCL
                
 1EF8 20B021            JSR     NEXTPC          ; FETCH # LOCALS TO PASS
 1EFB 8513              STA     J+LO            ; SAVE HERE FOR COUNTING
 1EFD 8514              STA     J+HI            ; AND HERE FOR LATER REFERENCE
 1EFF F02B              BEQ     ZCALL2          ; SKIP IF NO LOCALS
                
 1F01 A900              LDA     #0
 1F03 8511              STA     I+LO            ; ELSE INIT STORAGE INDEX
                
 1F05 A411      ZCALL1: LDY     I+LO
 1F07 BE0015            LDX     LOCALS+LO,Y     ; GET LSB OF LOCAL INTO [X]
 1F0A B90115            LDA     LOCALS+HI,Y     ; AND MSB INTO [A]
 1F0D 8411              STY     I+LO            ; SAVE THE INDEX
 1F0F 20A618            JSR     PUSHXA          ; PUSH LOCAL IN [X/A] ONTO Z-STACK
                
 1F12 20B021            JSR     NEXTPC          ; GET MSB OF NEW LOCAL
 1F15 8512              STA     I+HI            ; SAVE IT HERE
 1F17 20B021            JSR     NEXTPC          ; NOW GET LSB
                
 1F1A A411              LDY     I+LO            ; RESTORE INDEX
 1F1C 990015            STA     LOCALS+LO,Y     ; STORE LSB INTO [LOCALS]
 1F1F A512              LDA     I+HI            ; RETRIEVE MSB
 1F21 990115            STA     LOCALS+HI,Y     ; STORE IT INTO [LOCALS]
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  53    
--- X-OPS ---

 1F24 C8                INY
 1F25 C8                INY                     ; UPDATE
 1F26 8411              STY     I+LO            ; THE STORAGE INDEX
                
 1F28 C613              DEC     J+LO            ; ANY MORE LOCALS?
 1F2A D0D9              BNE     ZCALL1          ; YES, KEEP LOOPING
                
 1F2C                   ; MOVE UP TO 3 ARGUMENTS TO [LOCALS]
                
 1F2C C604      ZCALL2: DEC     NARGS           ; EXTRA ARGS IN THIS CALL?
 1F2E F026              BEQ     ZCALL3          ; NO, CONTINUE
                
 1F30 A507              LDA     ARG2+LO         ; MOVE [ARG2] TO LOCAL #1
 1F32 8D0015            STA     LOCALS+LO
 1F35 A508              LDA     ARG2+HI
 1F37 8D0115            STA     LOCALS+HI
                
 1F3A C604              DEC     NARGS           ; ANY LEFT?
 1F3C F018              BEQ     ZCALL3          ; NO, SCRAM
                
 1F3E A509              LDA     ARG3+LO         ; MOVE [ARG3] TO LOCAL #2
 1F40 8D0215            STA     LOCALS+LO+2
 1F43 A50A              LDA     ARG3+HI
 1F45 8D0315            STA     LOCALS+HI+2
                
 1F48 C604              DEC     NARGS           ; ANY LEFT?
 1F4A F00A              BEQ     ZCALL3          ; NO, EXUENT
                
 1F4C A50B              LDA     ARG4+LO         ; MOVE [ARG4] TO LOCAL #3
 1F4E 8D0415            STA     LOCALS+LO+4
 1F51 A50C              LDA     ARG4+HI
 1F53 8D0515            STA     LOCALS+HI+4
                
 1F56 A614      ZCALL3: LDX     J+HI            ; RETRIEVE # LOCALS
 1F58 8A                TXA                     ; DUPE FOR NO GOOD REASON
 1F59 20A618            JSR     PUSHXA          ; PUSH # LOCALS ONTO Z-STACK
                
 1F5C A517              LDA     ZSP             ; REMEMBER WHERE
 1F5E 8518              STA     OLDZSP          ; WE CAME FROM
                
 1F60 60                RTS                     ; WHEW!
                
 1F61                   ; ---
 1F61                   ; PUT
 1F61                   ; ---
                
 1F61                   ; SET ITEM [ARG2] IN WORD-TABLE [ARG1] EQUAL TO [ARG3]
                
 1F61 0607      ZPUT:   ASL     ARG2+LO         ; WORD-ALIGN [ARG2]
 1F63 2608              ROL     ARG2+HI
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  54    
--- X-OPS ---

 1F65 20771F            JSR     PCALC           ; GET ITEM ADDR INTO [I]
                
 1F68 A50A              LDA     ARG3+HI         ; [ARG3]
 1F6A 9111              STA     (I),Y           ; INTO THE TABLE AT [I]
 1F6C C8                INY
                
 1F6D                   ; ENTRY FOR "PUTB"
                
 1F6D A509      PUTLSB: LDA     ARG3+LO
 1F6F 9111              STA     (I),Y
 1F71 60                RTS
                
 1F72                   ; ----
 1F72                   ; PUTB
 1F72                   ; ----
                
 1F72                   ; SET ITEM [ARG2] IN BYTE-TABLE [ARG1] EQUAL TO [ARG3]
                
 1F72 20771F    ZPUTB:  JSR     PCALC
 1F75 F0F6              BEQ     PUTLSB          ; BRANCH ALWAYS
                
 1F77                   ; ---------------------------
 1F77                   ; CALC ITEM ADDRESS FOR "PUT"
 1F77                   ; ---------------------------
                
 1F77 A507      PCALC:  LDA     ARG2+LO         ; ADD ITEM OFFSET IN [ARG2]
 1F79 18                CLC                     ; TO TABLE ADDR IN [ARG1]
 1F7A 6505              ADC     ARG1+LO         ; TO FORM A POINTER
 1F7C 8511              STA     I+LO            ; IN [I]
                
 1F7E A508              LDA     ARG2+HI         ; SAME FOR MSB
 1F80 6506              ADC     ARG1+HI
 1F82 18                CLC
 1F83 6526              ADC     ZCODE           ; MAKE IT ABSOLUTE
 1F85 8512              STA     I+HI
                
 1F87 A000              LDY     #0              ; ZERO FOR INDEXING
 1F89 60                RTS
                
 1F8A                   ; ----
 1F8A                   ; PUTP
 1F8A                   ; ----
                
 1F8A                   ; SET PROPERTY [ARG2] IN OBJECT [ARG1] EQUAL TO [ARG3]
                
 1F8A 200D25    ZPUTP:  JSR     PROPB
                
 1F8D 202925    PUTP1:  JSR     PROPN
 1F90 C507              CMP     ARG2+LO
 1F92 F008              BEQ     PUTP2
 1F94 901B              BCC     PNERR           ; ERROR IF LOWER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  55    
--- X-OPS ---

                
 1F96 203625            JSR     PROPNX          ; TRY NEXT PROPERTY
 1F99 4C8D1F            JMP     PUTP1
                
 1F9C 202E25    PUTP2:  JSR     PROPL
 1F9F C8                INY                     ; MAKE [Y] POINT TO 1ST PROPERTY BYTE
 1FA0 AA                TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
 1FA1 F009              BEQ     PUTP3           ; PUT A BYTE
 1FA3 C901              CMP     #1              ; PUT A WORD IF [A] = 1
 1FA5 D00F              BNE     PLERR           ; ELSE LENGTH IS BAD
                
 1FA7 A50A              LDA     ARG3+HI         ; GET MSB OF PROPERTY
 1FA9 9111              STA     (I),Y           ; AND STORE IN OBJECT
 1FAB C8                INY                     ; POINT TO LSB SLOT
                
 1FAC A509      PUTP3:  LDA     ARG3+LO         ; FETCH LSB
 1FAE 9111              STA     (I),Y           ; AND STORE IN OBJECT
 1FB0 60                RTS
                
 1FB1                   ; *** ERROR #10: BAD PROPERTY NUMBER ***
                
 1FB1 A90A      PNERR:  LDA     #10
 1FB3 4C9025            JMP     ZERROR
                
 1FB6                   ; *** ERROR #11: PUTP PROPERTY LENGTH ***
                
 1FB6 A90B      PLERR:  LDA     #11
 1FB8 4C9025            JMP     ZERROR
                
 1FBB                   ; ------
 1FBB                   ; PRINTC
 1FBB                   ; ------
                
 1FBB                   ; PRINT CHAR WITH ASCII VALUE IN [ARG1]
                
 1FBB A505      ZPRC:   LDA     ARG1+LO         ; GRAB THE CHAR
 1FBD 4CFF25            JMP     COUT            ; AND SHIP IT OUT
                
 1FC0                   ; ------
 1FC0                   ; PRINTN
 1FC0                   ; ------
                
 1FC0                   ; PRINT VALUE OF [ARG1] AS A SIGNED INTEGER
                
 1FC0 A505      ZPRN:   LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
 1FC2 8556              STA     QUOT+LO
 1FC4 A506              LDA     ARG1+HI
 1FC6 8557              STA     QUOT+HI
                
 1FC8                   ; PRINT [QUOT]
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  56    
--- X-OPS ---

 1FC8 A557      NUMBER: LDA     QUOT+HI         ; IF VALUE IS POSITIVE
 1FCA 1008              BPL     DIGCNT          ; CONTINUE
                
 1FCC A92D              LDA     #$2D            ; ELSE START WITH A MINUS SIGN
 1FCE 20FF25            JSR     COUT
                
 1FD1 204D1E            JSR     ABQUOT          ; AND CALC ABS([QUOT])
                
 1FD4                   ; COUNT # OF DECIMAL DIGITS
                
 1FD4 A900      DIGCNT: LDA     #0              ; RESET
 1FD6 855E              STA     DIGITS          ; DIGIT INDEX
                
 1FD8 A556      DGC:    LDA     QUOT+LO         ; IS QUOTIENT
 1FDA 0557              ORA     QUOT+HI         ; ZERO YET?
 1FDC F012              BEQ     PRNTN3          ; YES, READY TO PRINT
                
 1FDE A90A              LDA     #10             ; ELSE DIVIDE [QUOT]
 1FE0 8558              STA     REMAIN+LO       ; BY 10 (LSB)
 1FE2 A900              LDA     #0
 1FE4 8559              STA     REMAIN+HI       ; 10 (MSB)
                
 1FE6 205B1E            JSR     UDIV            ; UNSIGNED DIVIDE
                
 1FE9 A558              LDA     REMAIN+LO       ; FETCH LSB OF REMAINDER (THE DIGIT)
 1FEB 48                PHA                     ; SAVE IT ON STACK
 1FEC E65E              INC     DIGITS          ; UPDATE DIGIT COUNT
 1FEE D0E8              BNE     DGC             ; LOOP TILL QUOTIENT=0
                
 1FF0 A55E      PRNTN3: LDA     DIGITS          ; IF DIGIT COUNT IS NZ
 1FF2 D005              BNE     PRNTN4          ; CONTINUE
                
 1FF4 A930              LDA     #'0'            ; ELSE PRINT "0"
 1FF6 4CFF25            JMP     COUT            ; AND RETURN
                
 1FF9 68        PRNTN4: PLA                     ; PULL A DIGIT OFF THE STACK
 1FFA 18                CLC
 1FFB 6930              ADC     #'0'            ; CONVERT TO ASCII
 1FFD 20FF25            JSR     COUT            ; AND PRINT IT
 2000 C65E              DEC     DIGITS          ; OUT OF DIGITS YET?
 2002 D0F5              BNE     PRNTN4          ; NO, KEEP LOOPING
 2004 60                RTS
                
 2005                   ; ------
 2005                   ; RANDOM
 2005                   ; ------
                
 2005                   ; RETURN A RANDOM VALUE BETWEEN 0 AND [ARG1]
                
 2005 A505      ZRAND:  LDA     ARG1+LO         ; MAKE [ARG1] THE DIVISOR
 2007 8507              STA     ARG2+LO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  57    
--- X-OPS ---

 2009 A506              LDA     ARG1+HI
 200B 8508              STA     ARG2+HI
                
 200D 20F825            JSR     RANDOM          ; GET RANDOM BYTES INTO [A] AND [X]
 2010 8605              STX     ARG1+LO         ; MAKE THEM THE DIVIDEND
 2012 297F              AND     #$7F            ; MAKE SURE MSB IS POSITIVE
 2014 8506              STA     ARG1+HI
                
 2016 200F1E            JSR     DIVIDE          ; SIGNED DIVIDE, [ARG1] / [ARG2]
                
 2019 A558              LDA     REMAIN+LO       ; MOVE REMAINDER
 201B 850F              STA     VALUE+LO        ; INTO [VALUE]
 201D A559              LDA     REMAIN+HI
 201F 8510              STA     VALUE+HI
                
 2021 209219            JSR     INCVAL          ; INCREMENT [VALUE]
 2024 4CCA18            JMP     PUTVAL          ; AND RETURN RESULT
                
 2027                   ; ----
 2027                   ; PUSH
 2027                   ; ----
                
 2027                   ; PUSH [ARG1] ONTO THE Z-STACK
                
 2027 A605      ZPUSH:  LDX     ARG1+LO
 2029 A506              LDA     ARG1+HI
 202B 4CA618            JMP     PUSHXA
                
 202E                   ; ---
 202E                   ; POP
 202E                   ; ---
                
 202E                   ; POP WORD OFF Z-STACK, STORE IN VARIABLE [ARG1]
                
 202E 208C18    ZPOP:   JSR     POPVAL          ; VALUE INTO [VALUE]
 2031 A505              LDA     ARG1+LO         ; GET VARIABLE ID
 2033 4CB918            JMP     VARPUT          ; AND CHANGE THE VARIABLE
                
                        END
                        INCLUD READ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  58    
--- READ HANDLER ---

                
 2036                   ; ----
 2036                   ; READ
 2036                   ; ----
                
 2036                   ; READ LINE INTO TABLE [ARG1]; PARSE INTO TABLE [ARG2]
                
 2036 20C226    ZREAD:  JSR     ZUSL            ; UPDATE THE STATUS LINE
                
 2039 A506              LDA     ARG1+HI         ; MAKE THE TABLE ADDRESSES
 203B 18                CLC                     ; ABSOLUTE
 203C 6526              ADC     ZCODE           ; LSBS NEED NOT CHANGE
 203E 8506              STA     ARG1+HI
                
 2040 A508              LDA     ARG2+HI
 2042 18                CLC
 2043 6526              ADC     ZCODE
 2045 8508              STA     ARG2+HI
                
 2047 204629            JSR     INPUT           ; READ LINE; RETURN LENGTH IN [A]
 204A 8545              STA     LINLEN          ; SAVE # CHARS IN LINE
                
 204C A900              LDA     #0
 204E 8546              STA     WRDLEN          ; INIT # CHARS IN WORD COUNTER
                
 2050 A001              LDY     #1              ; POINT TO "# WORDS READ" SLOT
 2052 9107              STA     (ARG2),Y        ; AND CLEAR IT ([A] = 0)
                
 2054 8443              STY     SOURCE          ; INIT SOURCE TABLE PNTR ([Y] = 1)
 2056 C8                INY                     ; = 2
 2057 8444              STY     RESULT          ; AND RESULT TABLE POINTER
                
 2059                   ; MAIN LOOP STARTS HERE
                
 2059 A000      READL:  LDY     #0              ; POINT TO "MAX WORDS" SLOT
 205B B107              LDA     (ARG2),Y        ; AND READ IT
 205D F00A              BEQ     RL1             ; FOR THE "SAMPLER BUG"
 205F C8                INY                     ; POINT TO "# WORDS READ" SLOT
 2060 D107              CMP     (ARG2),Y        ; TOO MANY WORDS?
 2062 B005              BCS     RL1             ; NOT YET
                
 2064                   ; *** ERROR #13: PARSER OVERFLOW ***
                
 2064 A90D              LDA     #13
 2066 4C9025            JMP     ZERROR
                
 2069 A545      RL1:    LDA     LINLEN
 206B 0546              ORA     WRDLEN          ; OUT OF CHARS AND WORDS?
 206D D001              BNE     RL2             ; NOT YET
 206F 60        RLEX:   RTS                     ; ELSE EXIT
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  59    
--- READ HANDLER ---

 2070 A546      RL2:    LDA     WRDLEN          ; GET WORD LENGTH
 2072 C906              CMP     #6              ; 6 CHARS DONE?
 2074 9003              BCC     RL3             ; NO, KEEP GOING
 2076 200721            JSR     FLUSHW          ; ELSE FLUSH REMAINDER OF WORD
                
 2079 A546      RL3:    LDA     WRDLEN          ; GET WORD LENGTH AGAIN
 207B D024              BNE     READL2          ; CONTINUE IF NOT FIRST CHAR
                
 207D                   ; START A NEW WORD
                
 207D A205              LDX     #5              ; CLEAR Z-WORD INPUT BUFFER
 207F 9537      RLL:    STA     IN,X            ; [A] = 0
 2081 CA                DEX
 2082 10FB              BPL     RLL
                
 2084 20F920            JSR     EFIND           ; GET BASE ADDRESS INTO [ENTRY]
 2087 A543              LDA     SOURCE          ; STORE THE START POS OF THE WORD
 2089 A003              LDY     #3              ; INTO THE "WORD START" SLOT
 208B 9147              STA     (ENTRY),Y               ; OF THE RESULT TABLE
                
 208D A8                TAY
 208E B105              LDA     (ARG1),Y        ; GET A CHAR FROM SOURCE BUFFER
 2090 203421            JSR     SIB             ; IS IT A SELF-INSERTING BREAK?
 2093 B026              BCS     DOSIB           ; YES IF CARRY WAS SET
                
 2095 202221            JSR     NORM            ; IS IT A "NORMAL" BREAK?
 2098 9007              BCC     READL2          ; NO, CONTINUE
                
 209A E643              INC     SOURCE          ; ELSE FLUSH THE STRANDED BREAK
 209C C645              DEC     LINLEN          ; UPDATE # CHARS LEFT IN LINE
 209E 4C5920            JMP     READL           ; AND LOOP
                
 20A1 A545      READL2: LDA     LINLEN          ; OUT OF CHARS YET?
 20A3 F01E              BEQ     READL3          ; LOOKS THAT WAY
                
 20A5 A443              LDY     SOURCE
 20A7 B105              LDA     (ARG1),Y        ; ELSE GRAB NEXT CHAR
 20A9 201D21            JSR     BREAK           ; IS IT A BREAK?
 20AC B015              BCS     READL3          ; YES IF CARRY WAS SET
                
 20AE A646              LDX     WRDLEN          ; ELSE STORE THE CHAR
 20B0 9537              STA     IN,X            ; INTO THE INPUT BUFFER
                
 20B2 C645              DEC     LINLEN          ; ONE LESS CHAR IN LINE
 20B4 E646              INC     WRDLEN          ; ONE MORE IN WORD
 20B6 E643              INC     SOURCE          ; POINT TO NEXT CHAR IN SOURCE
 20B8 4C5920            JMP     READL           ; AND LOOP BACK
                
 20BB 8537      DOSIB:  STA     IN              ; PUT THE BREAK INTO 1ST WORD SLOT
 20BD C645              DEC     LINLEN          ; ONE LESS CHAR IN LINE
 20BF E646              INC     WRDLEN          ; ONE MORE IN WORD BUFFER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  60    
--- READ HANDLER ---

 20C1 E643              INC     SOURCE          ; POINT TO NEXT SOURCE CHAR
                
 20C3 A546      READL3: LDA     WRDLEN          ; ANY CHARS IN WORD YET?
 20C5 F092              BEQ     READL           ; APPARENTLY NOT, SO LOOP BACK
                
 20C7 20F920            JSR     EFIND           ; GET ENTRY ADDR INTO [ENTRY]
 20CA A546              LDA     WRDLEN          ; GET ACTUAL LNGTH OF WORD
 20CC A002              LDY     #2              ; STORE IT IN "WORD LENGTH" SLOT
 20CE 9147              STA     (ENTRY),Y       ; OF THE CURRENT ENTRY
                
 20D0 20F523            JSR     CONZST          ; CONVERT ASCII IN [IN] TO Z-STRING
 20D3 204621            JSR     FINDW           ; AND LOOK IT UP IN VOCABULARY
                
 20D6 A001              LDY     #1
 20D8 B107              LDA     (ARG2),Y        ; FETCH THE # WORDS READ
 20DA 18                CLC
 20DB 6901              ADC     #1              ; INCREMENT IT
 20DD 9107              STA     (ARG2),Y        ; AND UPDATE
                
 20DF 20F920            JSR     EFIND           ; MAKE [ENTRY] POINT TO ENTRY
                
 20E2 A000              LDY     #0
 20E4 8446              STY     WRDLEN          ; CLEAR # CHARS IN WORD
 20E6 A510              LDA     VALUE+HI        ; GET MSB OF VOCAB ENTRY ADDRESS
 20E8 9147              STA     (ENTRY),Y       ; AND STORE IN 1ST SLOT OF ENTRY
 20EA C8                INY
 20EB A50F              LDA     VALUE+LO        ; ALSO STORE LSB IN 2ND SLOT
 20ED 9147              STA     (ENTRY),Y
                
 20EF A544              LDA     RESULT          ; UPDATE THE
 20F1 18                CLC                     ; RESULT TABLE POINTER
 20F2 6904              ADC     #4              ; SO IT POINTS TO THE
 20F4 8544              STA     RESULT          ; NEXT ENTRY
                
 20F6 4C5920            JMP     READL           ; AND LOOP BACK
                
 20F9                   ; -----------------------------------
 20F9                   ; FIND BASE ADDR OF RESULT ENTRY SLOT
 20F9                   ; -----------------------------------
                
 20F9 A507      EFIND:  LDA     ARG2+LO         ; LSB OF RESULT TABLE BASE
 20FB 18                CLC
 20FC 6544              ADC     RESULT          ; AND CURRENT POINTER
 20FE 8547              STA     ENTRY+LO        ; SAVE IN [ENTRY]
 2100 A508              LDA     ARG2+HI         ; ALSO ADD MSB
 2102 6900              ADC     #0
 2104 8548              STA     ENTRY+HI
 2106 60                RTS
                
 2107                   ; ----------
 2107                   ; FLUSH WORD


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  61    
--- READ HANDLER ---

 2107                   ; ----------
                
 2107 A545      FLUSHW: LDA     LINLEN          ; ANY CHARS LEFT IN LINE?
 2109 F011              BEQ     FLEX            ; NO, SCRAM
                
 210B A443              LDY     SOURCE          ; GET CURRENT CHAR POINTER
 210D B105              LDA     (ARG1),Y        ; AND GRAB A CHAR
 210F 201D21            JSR     BREAK           ; IS IT A BREAK?
 2112 B008              BCS     FLEX            ; EXIT IF SO
 2114 C645              DEC     LINLEN          ; ELSE UPDATE CHAR COUNT
 2116 E646              INC     WRDLEN          ; AND WORD-CHAR COUNT
 2118 E643              INC     SOURCE          ; AND CHAR POINTER
 211A D0EB              BNE     FLUSHW          ; AND LOOP BACK (ALWAYS)
                
 211C 60        FLEX:   RTS
                
 211D                   ; ---------------------------------
 211D                   ; IS CHAR IN [A] ANY TYPE OF BREAK?
 211D                   ; ---------------------------------
                
 211D 203421    BREAK:  JSR     SIB             ; CHECK FOR A SIB FIRST
 2120 B022              BCS     FBRK            ; EXIT NOW IF MATCHED
                
 2122                   ; ELSE FALL THROUGH ...
                
 2122                   ; --------------------------------
 2122                   ; IS CHAR IN [A] A "NORMAL" BREAK?
 2122                   ; --------------------------------
                
 2122 A205      NORM:   LDX     #NBRKS-1        ; NUMBER OF "NORMAL" BREAKS
 2124 DD2E21    NBL:    CMP     BRKTBL,X        ; MATCHED?
 2127 F01B              BEQ     FBRK            ; YES, EXIT
 2129 CA                DEX
 212A 10F8              BPL     NBL             ; NO, KEEP LOOKING
 212C 18                CLC                     ; NO MATCH, CLEAR CARRY
 212D 60                RTS                     ; AND RETURN
                
 212E                   ; ------------------
 212E                   ; NORMAL BREAK CHARS
 212E                   ; ------------------
                
 212E 213F2C2E  BRKTBL: DB      "!?,."          ; IN ORDER OF
 2132 0D                DB      EOL             ; ASCENDING FREQUENCY
 2133 20                DB      SPACE           ; SPACE CHAR IS TESTED FIRST FOR SPEED
                
 0006           NBRKS   EQU     $-BRKTBL        ; # NORMAL BREAKS
                
 2134                   ; ---------------------
 2134                   ; IS CHAR IN [A] A SIB?
 2134                   ; ---------------------
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  62    
--- READ HANDLER ---

 2134 AA        SIB:    TAX                     ; SAVE TEST CHAR
 2135 A000              LDY     #0              ; 1ST BYTE IN VOCAB TABLE
 2137 B131              LDA     (VOCAB),Y       ; HAS # SIBS
 2139 A8                TAY                     ; USE AS AN INDEX
 213A 8A                TXA                     ; RESTORE TEST CHAR
 213B D131      SBL:    CMP     (VOCAB),Y       ; MATCHED?
 213D F005              BEQ     FBRK            ; YES, REPORT IT
 213F 88                DEY
 2140 D0F9              BNE     SBL             ; ELSE KEEP LOOPING
 2142 18                CLC                     ; NO MATCH, SO
 2143 60                RTS                     ; EXIT WITH CARRY CLEAR
                
 2144 38        FBRK:   SEC                     ; EXIT WITH CARRY SET
 2145 60                RTS                     ; IF MATCHED WITH A BREAK CHAR
                
 2146                   ; -----------------
 2146                   ; VOCABULARY SEARCH
 2146                   ; -----------------
                
 2146                   ; ENTRY: 4-BYTE TARGET Z-WORD IN [OUT]
 2146                   ; EXIT: ABS ENTRY ADDRESS IN [VALUE] IF FOUND;
 2146                   ;       OTHERWISE [VALUE] = 0
                
 2146 A000      FINDW:  LDY     #0              ; GET # SIBS
 2148 B131              LDA     (VOCAB),Y       ; IN VOCAB TABLE
 214A 18                CLC                     ; INCREMENT IT
 214B 6901              ADC     #1              ; FOR PROPER ALIGNMENT
 214D 6531              ADC     VOCAB+LO        ; NOW ADD THE BASE ADDR OF THE TABLE
 214F 850F              STA     VALUE+LO        ; TO GET THE ACTUAL BASE ADDR
 2151 A532              LDA     VOCAB+HI        ; OF THE VOCAB ENTRIES
 2153 6900              ADC     #0              ; WHICH IS SAVED
 2155 8510              STA     VALUE+HI        ; IN [VALUE]
                
 2157 B10F              LDA     (VALUE),Y       ; GET # BYTES PER ENTRY ([Y] = 0)
 2159 854B              STA     ESIZE           ; SAVE IT HERE
                
 215B 209219            JSR     INCVAL          ; POINT TO NEXT BYTE
 215E B10F              LDA     (VALUE),Y       ; GET # ENTRIES IN TABLE (MSB)
 2160 854A              STA     NENTS+HI        ; AND STUFF IT IN [NENTS]
                
 2162 209219            JSR     INCVAL          ; NEXT BYTE
 2165 B10F              LDA     (VALUE),Y       ; DON'T FORGET THE LSB!
 2167 8549              STA     NENTS+LO
                
 2169 209219            JSR     INCVAL          ; [VALUE] NOW POINTS TO 1ST ENTRY
                
 216C                   ; BEGIN THE SEARCH!
                
 216C A000      FWL1:   LDY     #0
 216E B10F              LDA     (VALUE),Y       ; GET 1ST BYTE OF ENTRY
 2170 C53D              CMP     OUT             ; MATCHED 1ST BYTE OF TARGET?


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  63    
--- READ HANDLER ---

 2172 D015              BNE     WNEXT           ; NO, SKIP TO NEXT WORD
                
 2174 C8                INY
 2175 B10F              LDA     (VALUE),Y
 2177 C53E              CMP     OUT+1           ; 2ND BYTE MATCHED?
 2179 D00E              BNE     WNEXT           ; NOPE
                
 217B C8                INY
 217C B10F              LDA     (VALUE),Y
 217E C53F              CMP     OUT+2           ; 3RD BYTE?
 2180 D007              BNE     WNEXT           ; SORRY ...
                
 2182 C8                INY
 2183 B10F              LDA     (VALUE),Y
 2185 C540              CMP     OUT+3           ; LAST BYTE
 2187 F01F              BEQ     FWSUCC          ; FOUND IT!
                
 2189 A54B      WNEXT:  LDA     ESIZE           ; GET ENTRY SIZE
 218B 18                CLC                     ; AND ADD IT TO ENTRY ADDRESS
 218C 650F              ADC     VALUE+LO        ; TO MAKE [VALUE]
 218E 850F              STA     VALUE+LO        ; POINT TO THE NEXT ENTRY
 2190 9002              BCC     WNX
 2192 E610              INC     VALUE+HI
                
 2194 A549      WNX:    LDA     NENTS+LO        ; DECREMENT THE
 2196 38                SEC                     ; ENTRY COUNTER
 2197 E901              SBC     #1
 2199 8549              STA     NENTS+LO
 219B B002              BCS     WNX1
 219D C64A              DEC     NENTS+HI
                
 219F 054A      WNX1:   ORA     NENTS+HI        ; KEEP SEARCHING
 21A1 D0C9              BNE     FWL1            ; UNTIL COUNT IS ZERO
                
 21A3 850F              STA     VALUE+LO
 21A5 8510              STA     VALUE+HI
 21A7 60                RTS                     ; THEN RETURN WITH [VALUE] = 0
                
 21A8                   ; ENTRY MATCHED!
                
 21A8 A510      FWSUCC: LDA     VALUE+HI        ; CONVERT ABSOLUTE ENTRY ADDRESS
 21AA 38                SEC                     ; IN [VALUE]
 21AB E526              SBC     ZCODE           ; TO RELATIVE Z-ADDRESS
 21AD 8510              STA     VALUE+HI        ; LSB NEEDN'T CHANGE
 21AF 60                RTS
                
                        END
                
                        INCLUD PAGING.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  64    
--- TIME-STAMP PAGING ROUTINE (BM 3/8/85) ---

                
 21B0                   ; -------------------------
 21B0                   ; FETCH NEXT BYTE OF Z-CODE
 21B0                   ; -------------------------
                
 21B0                   ; EXIT: BYTE AT [ZPC] IN [A] & [Y]; FLAGS SET
                
 21B0 A51C      NEXTPC: LDA     ZPCFLG          ; IS [ZPCPNT] VALID?
 21B2 D01E              BNE     NPC2            ; YES, GET THE BYTE
                
 21B4                   ; Z-PAGE HAS CHANGED!
                
 21B4 A51A              LDA     ZPCM            ; GET TOP
 21B6 A41B              LDY     ZPCH            ; 9 BITS OF [ZPC]
 21B8 D008              BNE     NPC0            ; SWAP PAGE IF TOP BIT IS SET
                
 21BA C527              CMP     ZPURE           ; IS THIS PAGE PRELOADED?
 21BC B004              BCS     NPC0            ; NO, SWAP IT IN
                
 21BE 6526              ADC     ZCODE           ; ELSE MAKE IT ABSOLUTE
 21C0 D007              BNE     NPC1            ; AND GIVE IT TO [ZPCPNT]
                
 21C2 A200      NPC0:   LDX     #0
 21C4 8622              STX     MPCFLG          ; INVALIDATE [MPC]
 21C6 201C22            JSR     PAGE            ; AND GET ABS PAGE ADDR INTO [A]
                
 21C9 851E      NPC1:   STA     ZPCPNT+HI       ; SET ABS PAGE ADDRESS
 21CB A2FF              LDX     #$FF
 21CD 861C              STX     ZPCFLG          ; VALIDATE [ZPCPNT]
 21CF E8                INX                     ; = 0
 21D0 861D              STX     ZPCPNT+LO       ; CLEAR LSB OF POINTER
                
 21D2 A419      NPC2:   LDY     ZPCL            ; FETCH PAGE INDEX
 21D4 B11D              LDA     (ZPCPNT),Y      ; GET Z-BYTE
                
 21D6 E619              INC     ZPCL            ; END OF PAGE YET?
 21D8 D00A              BNE     NPC3            ; NO, EXIT
                
 21DA A000              LDY     #0
 21DC 841C              STY     ZPCFLG          ; ELSE INVALIDATE [ZPCPNT]
                
 21DE E61A              INC     ZPCM            ; POINT [ZPC] TO
 21E0 D002              BNE     NPC3            ; THE NEXT
 21E2 E61B              INC     ZPCH            ; Z-PAGE
                
 21E4 A8        NPC3:   TAY                     ; SET FLAGS
 21E5 60                RTS                     ; AND RETURN
                
 21E6                   ; -------------------------------
 21E6                   ; GET NEXT BYTE OF VIRTUAL MEMORY
 21E6                   ; -------------------------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  65    
--- TIME-STAMP PAGING ROUTINE (BM 3/8/85) ---

                
 21E6                   ; EXIT: BYTE AT [MPC] IN [A] & [Y]; FLAGS SET
                
 21E6 A522      GETBYT: LDA     MPCFLG          ; IS [MPCPNT] VALID?
 21E8 D01E              BNE     GTBT2           ; YES, GET THE BYTE
                
 21EA                   ; Z-PAGE HAS CHANGED!
                
 21EA A520              LDA     MPCM            ; GET TOP
 21EC A421              LDY     MPCH            ; 9 BITS OF [MPC]
 21EE D008              BNE     GTBT0           ; SWAP PAGE IF TOP BIT IS SET
                
 21F1           PATCH   EQU     $+1             ; PATCH POINT FOR "VERIFY"
                
 21F0 C527              CMP     ZPURE           ; IS THIS PAGE PRELOADED?
 21F2 B004              BCS     GTBT0           ; NO, SWAP IT IN
                
 21F4 6526              ADC     ZCODE           ; ELSE MAKE IT ABSOLUTE
 21F6 D007              BNE     GTBT1           ; AND GIVE IT TO [MPCPNT]
                
 21F8 A200      GTBT0:  LDX     #0
 21FA 861C              STX     ZPCFLG          ; INVALIDATE [ZPC]
 21FC 201C22            JSR     PAGE            ; AND GET ABS PAGE ADDR INTO [A]
                
 21FF 8524      GTBT1:  STA     MPCPNT+HI       ; SET ABS PAGE ADDRESS
 2201 A2FF              LDX     #$FF
 2203 8622              STX     MPCFLG          ; VALIDATE [MPCPNT]
 2205 E8                INX                     ; = 0
 2206 8623              STX     MPCPNT+LO       ; CLEAR LSB OF POINTER
                
 2208 A41F      GTBT2:  LDY     MPCL            ; FETCH PAGE INDEX
 220A B123              LDA     (MPCPNT),Y      ; GET Z-BYTE
                
 220C E61F              INC     MPCL            ; END OF PAGE YET?
 220E D00A              BNE     GTBT3           ; NO, EXIT
                
 2210 A000              LDY     #0
 2212 8422              STY     MPCFLG          ; ELSE INVALIDATE [MPCPNT]
                
 2214 E620              INC     MPCM            ; POINT [MPC] TO
 2216 D002              BNE     GTBT3           ; THE NEXT
 2218 E621              INC     MPCH            ; Z-PAGE
                
 221A A8        GTBT3:  TAY                     ; SET FLAGS
 221B 60                RTS                     ; AND RETURN
                
 221C                   ; ------------------------
 221C                   ; LOCATE A SWAPABLE Z-PAGE
 221C                   ; ------------------------
                
 221C                   ; ENTRY: TARGET Z-PAGE IN [A/Y] (9 BITS)


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  66    
--- TIME-STAMP PAGING ROUTINE (BM 3/8/85) ---

 221C                   ; EXIT: ABSOLUTE PAGE IN [A]
                
 221C 852B      PAGE:   STA     TARGET+LO       ; SAVE THE
 221E 842C              STY     TARGET+HI       ; TARGET Z-PAGE HERE
                
 2220                   ; IS THIS Z-PAGE ALREADY PAGED IN?
                
 2220 A200              LDX     #0
 2222 862A              STX     ZPAGE           ; START AT BUFFER #0
                
 2224 DD0013    PG1:    CMP     PTABL,X         ; LSB MATCHED?
 2227 D008              BNE     PG2             ; NO, TRY NEXT BUFFER
 2229 98                TYA                     ; ELSE CHECK
 222A DDA013            CMP     PTABH,X         ; TOP BIT
 222D F02B              BEQ     PG4             ; MATCHED! BUFFER IN [ZPAGE]
 222F A52B              LDA     TARGET+LO       ; ELSE RESTORE LSB
 2231 E62A      PG2:    INC     ZPAGE           ; UPDATE TALLY
 2233 E8                INX
 2234 E429              CPX     PMAX            ; OUT OF BUFFERS YET?
 2236 90EC              BCC     PG1             ; NO, KEEP SEARCHING
                
 2238                   ; SWAP IN THE TARGET PAGE
                
 2238 209322    PG3:    JSR     EARLY           ; GET EARLIEST PAGE
 223B A62E              LDX     SWAP            ; INTO [SWAP] & [X]
 223D 862A              STX     ZPAGE           ; SAVE FOR LATER
                
 223F A52B              LDA     TARGET+LO       ; ASSIGN THE TARGET PAGE
 2241 9D0013            STA     PTABL,X         ; TO THE EARLIEST BUFFER
 2244 8570              STA     DBLOCK+LO       ; ALSO GIVE IT TO ZDOS
                
 2246 A52C              LDA     TARGET+HI       ; SAME FOR TOP BIT
 2248 2901              AND     #%00000001      ; USE ONLY BIT 0
 224A 9DA013            STA     PTABH,X
 224D 8571              STA     DBLOCK+HI
                
 224F 8A                TXA
 2250 18                CLC
 2251 6528              ADC     PAGE0           ; CALC ABS ADDR OF BUFFER
 2253 8573              STA     DBUFF+HI        ; GIVE IT TO ZDOS
                
 2255 20D92A            JSR     GETDSK          ; SWAP IN THE NEW PAGE
 2258 B034              BCS     DISKE           ; ERROR IF CARRY SET
                
 225A                   ; UPDATE THE TIMESTAMP
                
 225A A42A      PG4:    LDY     ZPAGE           ; GET THE BUFFER INDEX
 225C B95014            LDA     LRUMAP,Y        ; GET THIS BUFFER'S STAMP
 225F C52D              CMP     STAMP           ; SAME AS CURRENT STAMP?
 2261 F025              BEQ     PG8             ; YES, EXIT
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  67    
--- TIME-STAMP PAGING ROUTINE (BM 3/8/85) ---

 2263 E62D              INC     STAMP           ; UPDATE STAMP
 2265 D01C              BNE     PG7             ; CONTINUE IF NO OVERFLOW
                
 2267                   ; HANDLE STAMP OVERFLOW
                
 2267 209322            JSR     EARLY           ; GET EARLIEST STAMP INTO [LRU]
                
 226A A200              LDX     #0              ; INIT INDEX
 226C BD5014    PG5:    LDA     LRUMAP,X        ; GET A STAMP READING
 226F F006              BEQ     PG6             ; EXIT IF ALREADY ZERO
 2271 38                SEC                     ; ELSE SUBTRACT OFF
 2272 E525              SBC     LRU             ; THE EARLIEST TIMESTAMP
 2274 9D5014            STA     LRUMAP,X        ; AND REPLACE THE STAMP
 2277 E8        PG6:    INX
 2278 E429              CPX     PMAX            ; END OF SWAPPING SPACE?
 227A 90F0              BCC     PG5             ; LOOP TILL ALL STAMPS FIXED
                
 227C A900              LDA     #0              ; TURN BACK THE CLOCK
 227E 38                SEC                     ; TO REFLECT NEW
 227F E525              SBC     LRU             ; STAMP READING
 2281 852D              STA     STAMP
                
 2283 A52D      PG7:    LDA     STAMP           ; FETCH STAMP
 2285 995014            STA     LRUMAP,Y        ; STAMP TARGET PAGE WITH IT
                
 2288 A52A      PG8:    LDA     ZPAGE           ; GET BUFFER INDEX
 228A 18                CLC                     ; MAKE IT
 228B 6528              ADC     PAGE0           ; ABSOLUTE
 228D 60                RTS                     ; AND RETURN IT IN [A]
                
 228E                   ; *** ERROR #14: DRIVE ACCESS ***
                
 228E A90E      DISKE:  LDA     #14
 2290 4C9025            JMP     ZERROR
                
 2293                   ; -------------------------
 2293                   ; LOCATE EARLIEST TIMESTAMP
 2293                   ; -------------------------
                
 2293                   ; EXIT: [LRU] - EARLIEST TIMESTAMP
 2293                   ;       [SWAP] = INDEX TO EARLIEST BUFFER
                
 2293 A200      EARLY:  LDX     #0              ; INIT INDEX
 2295 862E              STX     SWAP            ; AND [SWAP]
 2297 AD5014            LDA     LRUMAP          ; GET STAMP OF BUFFER #0
 229A E8                INX                     ; START COMPARE WITH BUFFER #1
 229B DD5014    EAR0:   CMP     LRUMAP,X        ; IS THIS STAMP EARLIER THAN [A]?
 229E 9005              BCC     EAR1            ; NO, TRY NEXT STAMP
 22A0 BD5014            LDA     LRUMAP,X        ; ELSE FETCH EARLIER ENTRY
 22A3 862E              STX     SWAP            ; AND REMEMBER WHERE WE FOUND IT
 22A5 E8        EAR1:   INX                     ; POINT TO NEXT STAMP


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  68    
--- TIME-STAMP PAGING ROUTINE (BM 3/8/85) ---

 22A6 E429              CPX     PMAX            ; OUT OF STAMPS YET?
 22A8 90F1              BCC     EAR0            ; LOOP TILL EMPTY
                
 22AA 8525              STA     LRU             ; SAVE EARLIEST STAMP HERE
 22AC 60                RTS
                
 22AD                   ; -------------------------
 22AD                   ; POINT [MPC] TO V-ADDR [I]
 22AD                   ; -------------------------
                
 22AD A511      SETWRD: LDA     I+LO
 22AF 851F              STA     MPCL
 22B1 A512              LDA     I+HI
 22B3 8520              STA     MPCM
                
 22B5 A900              LDA     #0
 22B7 8521              STA     MPCH            ; ZERO TOP BIT
 22B9 8522              STA     MPCFLG          ; INVALIDATE [MPC]
 22BB 60                RTS
                
 22BC                   ; ----------------------------
 22BC                   ; GET Z-WORD AT [MPC] INTO [I]
 22BC                   ; ----------------------------
                
 22BC 20E621    GETWRD: JSR     GETBYT
 22BF 8512              STA     I+HI
 22C1 20E621            JSR     GETBYT
 22C4 8511              STA     I+LO
 22C6 60                RTS
                
                        END
                        INCLUD ZSTRING.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  69    
--- Z-STRING HANDLERS ---

                
 22C7                   ; -----------------------
 22C7                   ; POINT TO ZSTRING IN [I]
 22C7                   ; -----------------------
                
 22C7 A511      SETSTR: LDA     I+LO            ; WORD-ALIGN THE ADDRESS
 22C9 0A                ASL     A
 22CA 851F              STA     MPCL
 22CC A512              LDA     I+HI
 22CE 2A                ROL     A
 22CF 8520              STA     MPCM
 22D1 A900              LDA     #0
 22D3 8522              STA     MPCFLG          ; [MPC] IS CHANGING!
 22D5 2A                ROL     A
 22D6 8521              STA     MPCH
                
 22D8 60        ZSTEX:  RTS
                
 22D9                   ; -----------------------
 22D9                   ; PRINT Z-STRING AT [MPC]
 22D9                   ; -----------------------
                
 22D9 A200      PZSTR:  LDX     #0
 22DB 864C              STX     PSET            ; ASSUME PERMANENT CHARSET
 22DD 8650              STX     ZFLAG           ; CLEAR BYTE FLAG
 22DF CA                DEX                     ; = $FF
 22E0 864D              STX     TSET            ; NO TEMPSET ACTIVE
                
 22E2 20AD23    PZTOP:  JSR     GETZCH          ; GET A Z-CHAR
 22E5 B0F1              BCS     ZSTEX           ; END OF STRING IF CARRY IS SET
                
 22E7 854E              STA     ZCHAR           ; ELSE SAVE CHAR HERE
 22E9 AA                TAX                     ; SET FLAGS
 22EA F041              BEQ     BLANK           ; PRINT SPACE IF CHAR = 0
                
 22EC C904              CMP     #4              ; IS THIS AN F-WORD?
 22EE 905B              BCC     DOFREQ          ; APPARENTLY SO
                
 22F0 C906              CMP     #6              ; PERHAPS A SHIFT CODE?
 22F2 903D              BCC     NEWSET          ; YES, CHANGE CHARSETS
                
 22F4 20A123            JSR     GETSET          ; ELSE GET CHARSET
 22F7 AA                TAX                     ; SET FLAGS
 22F8 D00B              BNE     SET1            ; SKIP IF NOT CHARSET #0
                
 22FA                   ; PRINT A LOWER-CASE CHAR (CHARSET #0)
                
 22FA A95B              LDA     #$61-6          ; ASCII "a" MINUS Z-OFFSET
                
 22FC 18        TOASC:  CLC
 22FD 654E              ADC     ZCHAR           ; ADD Z-CHAR INDEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  70    
--- Z-STRING HANDLERS ---

                
 22FF 20FF25    SHOVE:  JSR     COUT            ; SHOW THE CHAR
 2302 4CE222            JMP     PZTOP           ; AND GRAB NEXT CHAR
                
 2305                   ; PRINT AN UPPER-CASE CHAR (CHARSET #1)
                
 2305 C901      SET1:   CMP     #1              ; MAKE SURE IT'S SET #1
 2307 D004              BNE     SET2            ; ELSE MUST BE SET #2
                
 2309 A93B              LDA     #$41-6          ; ASCII "A" MINUS Z-OFFSET
 230B D0EF              BNE     TOASC           ; SAME AS SET #0
                
 230D                   ; PRINT FROM CHARSET #2
                
 230D A54E      SET2:   LDA     ZCHAR           ; RETRIEVE THE Z-CHAR
 230F 38                SEC
 2310 E906              SBC     #6              ; ZERO-ALIGN IT
 2312 F007              BEQ     DIRECT          ; IF ZERO, IT'S A "DIRECT" ASCII
                
 2314 AA                TAX                     ; OTHERWISE USE CODE AS AN INDEX
 2315 BDCA24            LDA     CHRTBL,X        ; INTO THE CHARSET TABLE
 2318 4CFF22            JMP     SHOVE           ; AND PRINT THE CHAR
                
 231B                   ; DECODE A "DIRECT" ASCII CHAR
                
 231B 20AD23    DIRECT: JSR     GETZCH          ; FETCH NEXT Z-CHAR
 231E 0A                ASL     A
 231F 0A                ASL     A
 2320 0A                ASL     A
 2321 0A                ASL     A
 2322 0A                ASL     A               ; SHIFT INTO POSITION
 2323 854E              STA     ZCHAR           ; AND SAVE HERE
 2325 20AD23            JSR     GETZCH          ; GRAB YET ANOTHER Z-CHAR
 2328 054E              ORA     ZCHAR           ; SUPERIMPOSE THE 2ND BYTE
 232A 4CFF22            JMP     SHOVE           ; AND PRINT THE RESULT
                
 232D                   ; PRINT A SPACE
                
 232D A920      BLANK:  LDA     #SPACE          ; ASCII SPACE CHAR
 232F D0CE              BNE     SHOVE
                
 2331                   ; CHANGE CHARSET
                
 2331 38        NEWSET: SEC                     ; CONVERT THE SHIFT CODE
 2332 E903              SBC     #3              ; TO 1 OR 2
 2334 A8                TAY
 2335 20A123            JSR     GETSET          ; IS MODE TEMPORARY?
 2338 D005              BNE     TOPERM          ; YES, DO A PERMSHIFT
 233A 844D              STY     TSET            ; ELSE JUST A TEMPSHIFT
 233C 4CE222            JMP     PZTOP           ; AND CONTINUE
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  71    
--- Z-STRING HANDLERS ---

 233F 844C      TOPERM: STY     PSET            ; SET PERM CHARSET
 2341 C54C              CMP     PSET            ; SAME AS BEFORE?
 2343 F09D              BEQ     PZTOP           ; YES, CONTINUE
 2345 A900              LDA     #0
 2347 854C              STA     PSET            ; ELSE RESET CHARSET
 2349 F097              BEQ     PZTOP           ; BEFORE LOOPING BACK
                
 234B                   ; PRINT AN F-WORD
                
 234B 38        DOFREQ: SEC
 234C E901              SBC     #1              ; ZERO-ALIGN THE CODE
 234E 0A                ASL     A               ; AND MULTIPLY TIMES 64
 234F 0A                ASL     A               ; TO OBTAIN THE SEGMENT OFFSET
 2350 0A                ASL     A               ; INTO THE F-WORDS TABLE
 2351 0A                ASL     A
 2352 0A                ASL     A
 2353 0A                ASL     A
 2354 854F              STA     OFFSET          ; SAVE OFFSET FOR LATER
                
 2356 20AD23            JSR     GETZCH          ; NOW GET THE F-WORD POINTER
 2359 0A                ASL     A               ; WORD-ALIGN IT
 235A 18                CLC                     ; AND
 235B 654F              ADC     OFFSET          ; ADD THE SEGMENT OFFSET
 235D A8                TAY                     ; TO GET THE OFFSET OF THE F-WORD
 235E B133              LDA     (FWORDS),Y      ; FROM THE START OF THE F-WORDS TABLE
 2360 8512              STA     I+HI            ; SAVE MSB OF F-WORD ADDRESS
 2362 C8                INY
 2363 B133              LDA     (FWORDS),Y      ; ALSO SAVE LSB
 2365 8511              STA     I+LO            ; Z-ADDRESS OF F-WORD IS IN [I]
                
 2367                   ; SAVE THE STATE OF CURRENT Z-STRING
                
 2367 A521              LDA     MPCH
 2369 48                PHA
 236A A520              LDA     MPCM
 236C 48                PHA
 236D A51F              LDA     MPCL
 236F 48                PHA
 2370 A54C              LDA     PSET
 2372 48                PHA
 2373 A550              LDA     ZFLAG
 2375 48                PHA
 2376 A552              LDA     ZWORD+HI
 2378 48                PHA
 2379 A551              LDA     ZWORD+LO
 237B 48                PHA
                
 237C 20C722            JSR     SETSTR          ; PRINT THE Z-STRING
 237F 20D922            JSR     PZSTR           ; IN [I]
                
 2382                   ; RESTORE OLD Z-STRING


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  72    
--- Z-STRING HANDLERS ---

                
 2382 68                PLA
 2383 8551              STA     ZWORD+LO
 2385 68                PLA
 2386 8552              STA     ZWORD+HI
 2388 68                PLA
 2389 8550              STA     ZFLAG
 238B 68                PLA
 238C 854C              STA     PSET
 238E 68                PLA
 238F 851F              STA     MPCL
 2391 68                PLA
 2392 8520              STA     MPCM
 2394 68                PLA
 2395 8521              STA     MPCH
                
 2397 A2FF              LDX     #$FF
 2399 864D              STX     TSET            ; DISABLE TEMP CHARSET
 239B E8                INX                     ; = 0
 239C 8622              STX     MPCFLG          ; [MPC] HAS CHANGED
 239E 4CE222            JMP     PZTOP           ; CONTINUE INNOCENTLY
                
 23A1                   ; ----------------------
 23A1                   ; RETURN CURRENT CHARSET
 23A1                   ; ----------------------
                
 23A1 A54D      GETSET: LDA     TSET
 23A3 1003              BPL     GS
 23A5 A54C              LDA     PSET
 23A7 60                RTS
                
 23A8 A0FF      GS:     LDY     #$FF
 23AA 844D              STY     TSET
 23AC 60                RTS
                
 23AD                   ; -----------------
 23AD                   ; FETCH NEXT Z-CHAR
 23AD                   ; -----------------
                
 23AD A550      GETZCH: LDA     ZFLAG           ; WHICH BYTE IS THIS?
 23AF 1002              BPL     GTZ0            ; $FF = LAST
 23B1 38                SEC                     ; SET CARRY TO INDICATE
 23B2 60                RTS                     ; NO MORE CHARS
                
 23B3 D013      GTZ0:   BNE     GETZ1           ; NOT FIRST CHAR, EITHER
                
 23B5                   ; GET A Z-WORD INTO [ZWORD], RETURN 1ST CHAR IN TRIPLET
                
 23B5 E650              INC     ZFLAG           ; UPDATE CHAR COUNT
                
 23B7 20E621            JSR     GETBYT          ; GET TRIPLET AT [MPC]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  73    
--- Z-STRING HANDLERS ---

 23BA 8552              STA     ZWORD+HI        ; INTO [ZWORD]
 23BC 20E621            JSR     GETBYT
 23BF 8551              STA     ZWORD+LO
                
 23C1 A552              LDA     ZWORD+HI
 23C3 4A                LSR     A
 23C4 4A                LSR     A               ; SHIFT 1ST CHAR INTO PLACE
 23C5 4CF123            JMP     GTEXIT          ; AND RETURN IT
                
 23C8 38        GETZ1:  SEC
 23C9 E901              SBC     #1
 23CB D016              BNE     GETZ2           ; LAST CHAR IN TRIPLET IF ZERO
 23CD A902              LDA     #2              ; ELSE
 23CF 8550              STA     ZFLAG           ; RESET CHAR INDEX
                
 23D1 A551              LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET
 23D3 8511              STA     I+LO            ; MOVE HERE FOR SHIFTING
 23D5 A552              LDA     ZWORD+HI        ; GET TOP HALF
                
 23D7 0611              ASL     I+LO            ; SHIFT THE TOP 3 BITS OF LOWER HALF
 23D9 2A                ROL     A               ; INTO THE BOTTOM OF THE TOP HALF
 23DA 0611              ASL     I+LO
 23DC 2A                ROL     A
 23DD 0611              ASL     I+LO
 23DF 2A                ROL     A
 23E0 4CF123            JMP     GTEXIT
                
 23E3 A900      GETZ2:  LDA     #0              ; SET FLAG TO INDICATE
 23E5 8550              STA     ZFLAG           ; END OF TRIPLET
                
 23E7 A552              LDA     ZWORD+HI        ; TEST TOP HALF OF TRIPLET
 23E9 1004              BPL     GETZ3           ; CONTINUE IF NOT END OF STRING
 23EB A9FF              LDA     #$FF            ; ELSE
 23ED 8550              STA     ZFLAG           ; INDICATE LAST TRIPLET IN STRING
                
 23EF A551      GETZ3:  LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET
                
 23F1 291F      GTEXIT: AND     #%00011111      ; MASK OUT GARBAGE BITS
 23F3 18                CLC
 23F4 60                RTS
                
 23F5                   ; ---------------------------------
 23F5                   ; CONVERT [IN] TO Z-STRING IN [OUT]
 23F5                   ; ---------------------------------
                
 23F5 A905      CONZST: LDA     #$05            ; FILL OUTPUT BUFFER
 23F7 AA                TAX                     ; WITH PAD CHARS ($05)
 23F8 953D      CZSL:   STA     OUT,X
 23FA CA                DEX
 23FB 10FB              BPL     CZSL
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  74    
--- Z-STRING HANDLERS ---

 23FD A906              LDA     #6              ; INIT
 23FF 8553              STA     CONCNT          ; CHAR COUNT
                
 2401 A900              LDA     #0              ; CLEAR
 2403 8554              STA     CONIN           ; SOURCE AND
 2405 8555              STA     CONOUT          ; OUTPUT INDEXES
                
 2407 A654      CONTOP: LDX     CONIN           ; FETCH SOURCE INDEX
 2409 E654              INC     CONIN           ; AND UPDATE
 240B B537              LDA     IN,X            ; GRAB AN ASCII CHAR
 240D 854E              STA     ZCHAR           ; SAVE IT HERE
 240F D004              BNE     NEXTZ           ; CONTINUE IF CHAR WAS NZ
                
 2411 A905              LDA     #5              ; ELSE SHIP OUT
 2413 D02C              BNE     CSHIP           ; A PAD CHAR
                
 2415 A54E      NEXTZ:  LDA     ZCHAR
 2417 208B24            JSR     SAYSET          ; WHICH CHARSET TO USE?
 241A F020              BEQ     CSET0           ; LOWER-CASE IF ZERO
                
 241C 18                CLC                     ; ELSE DO A TEMP-SHIFT
 241D 6903              ADC     #3              ; 4 = CHARSET 1, 5 = CHARSET 2
 241F A655              LDX     CONOUT          ; FETCH OUTPUT INDEX
 2421 953D              STA     OUT,X           ; SEND THE SHIFT CHAR
                
 2423 E655              INC     CONOUT          ; UPDATE INDEX
 2425 C653              DEC     CONCNT          ; AND CHAR COUNT
 2427 D003              BNE     CTEST           ; IF OUT OF CHARS
 2429 4CA424            JMP     ZCRUSH          ; CRUSH 'EM!
                
 242C A54E      CTEST:  LDA     ZCHAR           ; TEST CHAR AGAIN
 242E 208B24            JSR     SAYSET
 2431 C902              CMP     #2
 2433 F019              BEQ     CSET2           ; CHARSET #2
                
 2435                   ; HANDLE CHARSET #1 (UPPER CASE ALPHA)
                
 2435 A54E              LDA     ZCHAR
 2437 38                SEC
 2438 E93B              SBC     #$41-6          ; CONVERT TO Z-CHAR
 243A 1005              BPL     CSHIP           ; AND SEND TO OUTPUT
                
 243C                   ; HANDLE CHARSET #0 (LOWER CASE ALPHA)
                
 243C A54E      CSET0:  LDA     ZCHAR
 243E 38                SEC
 243F E95B              SBC     #$61-6          ; CONVERT TO Z-CHAR
                
 2441                   ; SHIP Z-CHAR TO OUTPUT BUFFER
                
 2441 A655      CSHIP:  LDX     CONOUT          ; FETCH OUTPUT INDEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  75    
--- Z-STRING HANDLERS ---

 2443 953D              STA     OUT,X
                
 2445 E655              INC     CONOUT          ; UPDATE INDEX
 2447 C653              DEC     CONCNT          ; DONE 6 CHARS YET?
 2449 D0BC              BNE     CONTOP          ; NO, LOOP BACK
 244B 4CA424            JMP     ZCRUSH          ; ELSE CRUSH
                
 244E                   ; HANDLE CHARSET #2 (MISCELLANEOUS)
                
 244E A54E      CSET2:  LDA     ZCHAR           ; GRAB CHAR
 2450 207B24            JSR     CTABLE          ; IS IT IN CHARSET #3 TABLE?
 2453 D0EC              BNE     CSHIP           ; YES, SEND IT TO OUTPUT
                
 2455                   ; SEND A "DIRECT" ASCII CHAR
                
 2455 A906              LDA     #6              ; ASCII ALERT!
 2457 A655              LDX     CONOUT
 2459 953D              STA     OUT,X
                
 245B E655              INC     CONOUT          ; UPDATE INDEX
 245D C653              DEC     CONCNT          ; AND CHAR COUNT
 245F F043              BEQ     ZCRUSH          ; BUFFER FULL!
                
 2461                   ; SEND 1ST HALF OF "DIRECT"
                
 2461 A54E              LDA     ZCHAR
 2463 4A                LSR     A
 2464 4A                LSR     A
 2465 4A                LSR     A
 2466 4A                LSR     A
 2467 4A                LSR     A
 2468 2903              AND     #%00000011      ; MASK GARBAGE
 246A A655              LDX     CONOUT
 246C 953D              STA     OUT,X
                
 246E E655              INC     CONOUT
 2470 C653              DEC     CONCNT
 2472 F030              BEQ     ZCRUSH          ; BUFFER FULL!
                
 2474                   ; SEND 2ND HALF OF "DIRECT"
                
 2474 A54E              LDA     ZCHAR           ; GET CHAR YET AGAIN
 2476 291F              AND     #%00011111      ; MASK JUNK
 2478 4C4124            JMP     CSHIP           ; AND SHIP IT OUT
                
 247B                   ; ---------------------
 247B                   ; IS [A] IN CHARSET #3?
 247B                   ; ---------------------
                
 247B                   ; EXIT: [A] = CHAR CODE IF FOUND, Z-FLAG CLEARED
 247B                   ;       Z-FLAG SET IF NOT FOUND


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  76    
--- Z-STRING HANDLERS ---

                
 247B A219      CTABLE: LDX     #25
 247D DDCA24    CNL:    CMP     CHRTBL,X
 2480 F004              BEQ     CNOK
 2482 CA                DEX
 2483 D0F8              BNE     CNL
 2485 60                RTS                     ; Z-FLAG SET IF NO MATCH
                
 2486 8A        CNOK:   TXA                     ; CHAR CODE IS INDEX
 2487 18                CLC
 2488 6906              ADC     #6              ; PLUS 6
 248A 60                RTS
                
 248B                   ; -----------------------------
 248B                   ; RETURN CHARSET OF CHAR IN [A]
 248B                   ; -----------------------------
                
 248B C961      SAYSET: CMP     #'a'
 248D 9007              BCC     SAY1
 248F C97B              CMP     #'z'+1
 2491 B003              BCS     SAY1
 2493 A900              LDA     #0              ; IT'S CHARSET #0
 2495 60                RTS
                
 2496 C941      SAY1:   CMP     #'A'
 2498 9007              BCC     SAY2
 249A C95B              CMP     #'Z'+1
 249C B003              BCS     SAY2
 249E A901              LDA     #1              ; IT'S CHARSET #1
 24A0 60                RTS
                
 24A1 A902      SAY2:   LDA     #2              ; IT'S CHARSET #2
 24A3 60                RTS
                
 24A4                   ; ----------------------
 24A4                   ; CRUSH Z-CHARS IN [OUT]
 24A4                   ; ----------------------
                
 24A4 A53E      ZCRUSH: LDA     OUT+1           ; GET 2ND Z-CHAR
 24A6 0A                ASL     A               ; SHIFT BITS INTO POSITION
 24A7 0A                ASL     A
 24A8 0A                ASL     A
 24A9 0A                ASL     A
 24AA 263D              ROL     OUT             ; ALONG WITH 1ST Z-CHAR
 24AC 0A                ASL     A
 24AD 263D              ROL     OUT
 24AF 053F              ORA     OUT+2           ; SUPERIMPOSE 3RD Z-CHAR
 24B1 853E              STA     OUT+1
                
 24B3 A541              LDA     OUT+4           ; GET 5TH Z-CHAR
 24B5 0A                ASL     A               ; SHIFT BITS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  77    
--- Z-STRING HANDLERS ---

 24B6 0A                ASL     A
 24B7 0A                ASL     A
 24B8 0A                ASL     A
 24B9 2640              ROL     OUT+3           ; ALONG WITH 4TH Z-CHAR
 24BB 0A                ASL     A
 24BC 2640              ROL     OUT+3
 24BE 0542              ORA     OUT+5           ; SUPERIMPOSE 6TH Z-CHAR
 24C0 AA                TAX                     ; SAVE HERE
 24C1 A540              LDA     OUT+3           ; GRAB 4TH Z-CHAR
 24C3 0980              ORA     #%10000000      ; SET HIGH BIT
 24C5 853F              STA     OUT+2           ; MOVE CRUSHED Z-WORD
 24C7 8640              STX     OUT+3           ; INTO PLACE
 24C9 60                RTS
                
 24CA                   ; -----------------------
 24CA                   ; CHARSET #2 DECODE TABLE
 24CA                   ; -----------------------
                
 24CA 00        CHRTBL: DB      0               ; DUMMY BYTE FOR "DIRECT"
 24CB 0D                DB      $0D             ; EOL
 24CC 30313233          DB      "0123456789.,!?_#"
 24DC 27                DB      $27             ; SINGLE QUOTE
 24DD 22                DB      $22             ; DOUBLE QUOTE
 24DE 2F5C2D3A          DB      "/\-:()"
                
                        END
                        INCLUD OBJECTS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  78    
--- OBJECT & PROPERTY HANDLERS ---

                
 24E4                   ; ----------------------------------
 24E4                   ; GET ABSOLUTE ADDRESS OF OBJECT [A]
 24E4                   ; ----------------------------------
                
 24E4                   ; EXIT: ADDRESS IN [I]
                
 24E4 8511      OBJLOC: STA     I+LO            ; SAVE LSB FOR ADDING
                
 24E6 A200              LDX     #0              ; CLEAR MSB
 24E8 8612              STX     I+HI            ; FOR SHIFTING
                
 24EA 0A                ASL     A               ; MULTIPLY BY 8
 24EB 2612              ROL     I+HI
 24ED 0A                ASL     A
 24EE 2612              ROL     I+HI
 24F0 0A                ASL     A
 24F1 2612              ROL     I+HI
                
 24F3 18                CLC                     ; ADD TO ITSELF
 24F4 6511              ADC     I+LO            ; TO GET TIMES 9
 24F6 9002              BCC     OBJ1
 24F8 E612              INC     I+HI
                
 24FA 18        OBJ1:   CLC
 24FB 6935              ADC     #53             ; NOW ADD 53
 24FD 9002              BCC     OBJ2            ; (THE OBJECT TABLE OFFSET)
 24FF E612              INC     I+HI
                
 2501 18        OBJ2:   CLC                     ; NEXT ADD THE ABS ADDR
 2502 6535              ADC     OBJTAB+LO       ; OF THE OBJECT TABLE
 2504 8511              STA     I+LO
                
 2506 A512              LDA     I+HI
 2508 6536              ADC     OBJTAB+HI
 250A 8512              STA     I+HI
 250C 60                RTS
                
 250D                   ; -----------------------------
 250D                   ; GET ADDRESS OF PROPERTY TABLE
 250D                   ; -----------------------------
                
 250D                   ; EXIT: [I] HAS ABSOLUTE ADDR OF PROPERTY TABLE
 250D                   ;       [Y] HAS OFFSET TO START OF PROP IDS
                
 250D A505      PROPB:  LDA     ARG1+LO
 250F 20E424            JSR     OBJLOC
 2512 A007              LDY     #7
 2514 B111              LDA     (I),Y           ; GET MSB OF P-TABLE ADDRESS
 2516 18                CLC
 2517 6526              ADC     ZCODE           ; MAKE IT ABSOLUTE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  79    
--- OBJECT & PROPERTY HANDLERS ---

 2519 AA                TAX                     ; AND SAVE HERE
 251A C8                INY
 251B B111              LDA     (I),Y           ; NOW GET LSB
 251D 8511              STA     I+LO
 251F 8612              STX     I+HI            ; [I] NOW POINTS TO PROP TABLE
                
 2521 A000              LDY     #0
 2523 B111              LDA     (I),Y           ; GET LENGTH OF SHORT DESC
 2525 0A                ASL     A               ; WORD-ALIGN IT
 2526 A8                TAY                     ; EXPECTED HERE
 2527 C8                INY                     ; POINT JUST PAST THE DESCRIPTION
 2528 60                RTS
                
 2529                   ; -------------------
 2529                   ; FETCH A PROPERTY ID
 2529                   ; -------------------
                
 2529                   ; ENTRY: LIKE "PROPB" EXIT
                
 2529 B111      PROPN:  LDA     (I),Y
 252B 291F              AND     #%00011111      ; MASK OUT LENGTH BITS
 252D 60                RTS
                
 252E                   ; -------------------------------
 252E                   ; FETCH # BYTES IN PROPERTY VALUE
 252E                   ; -------------------------------
                
 252E                   ; ENTRY: LIKE "PROPB" EXIT
                
 252E B111      PROPL:  LDA     (I),Y
 2530 4A                LSR     A               ; LENGTH IS IN
 2531 4A                LSR     A               ; BITS 7-5
 2532 4A                LSR     A               ; SO SHIFT INTO PLACE
 2533 4A                LSR     A
 2534 4A                LSR     A
 2535 60                RTS
                
 2536                   ; ----------------------
 2536                   ; POINT TO NEXT PROPERTY
 2536                   ; ----------------------
                
 2536                   ; ENTRY: LIKE "PROPB" EXIT
                
 2536 202E25    PROPNX: JSR     PROPL           ; GET LENGTH OF CURRENT PROP
 2539 AA                TAX                     ; SAVE HERE
                
 253A C8        PPX:    INY                     ; LOOP UNTIL
 253B CA                DEX                     ; [Y] POINTS TO
 253C 10FC              BPL     PPX             ; START OF NEXT PROP
 253E C8                INY                     ; CORRECT ALIGNMENT
 253F 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  80    
--- OBJECT & PROPERTY HANDLERS ---

                
 2540                   ; ----------------
 2540                   ; GET OBJECT FLAGS
 2540                   ; ----------------
                
 2540                   ; ENTRY: OBJECT # IN [ARG1], FLAG # IN [ARG2]
 2540                   ; EXIT: FLAG WORD IN [K], BIT ID IN [J],
 2540                   ;       FLAG WORD ADDRESS IN [I]
                
 2540 A505      FLAGSU: LDA     ARG1+LO
 2542 20E424            JSR     OBJLOC          ; GET OBJECT ADDR IN [I]
                
 2545 A507              LDA     ARG2+LO         ; LOOK AT FLAG ID
 2547 C910              CMP     #$10            ; FIRST SET OF FLAGS?
 2549 900F              BCC     FLS1            ; YES, ADDR IN [I] IS CORRECT
                
 254B E910              SBC     #16             ; ELSE ZERO-ALIGN FLAG INDEX
 254D AA                TAX                     ; SAVE IT HERE
                
 254E A511              LDA     I+LO            ; ADD 2 TO ADDRESS IN [I]
 2550 18                CLC                     ; TO POINT TO ADDRESS OF
 2551 6902              ADC     #2              ; 2ND FLAG WORD
 2553 8511              STA     I+LO
 2555 9002              BCC     FLS0
 2557 E612              INC     I+HI
                
 2559 8A        FLS0:   TXA                     ; RESTORE INDEX
                
 255A 8515      FLS1:   STA     K+LO            ; SAVE FLAG ID HERE
                
 255C A201              LDX     #1              ; INIT THE
 255E 8613              STX     J+LO            ; FLAG WORD TO
 2560 CA                DEX                     ; $0001
 2561 8614              STX     J+HI
                
 2563 A90F              LDA     #15             ; SUBTRACT THE BIT POSITION
 2565 38                SEC                     ; FROM 15
 2566 E515              SBC     K+LO            ; TO GET THE SHIFT LOOP
 2568 AA                TAX                     ; INDEX
 2569 F007              BEQ     FLS2            ; EXIT NOW IF NO SHIFT NEEDED
                
 256B 0613      FLSL:   ASL     J+LO            ; SHIFT THE BIT
 256D 2614              ROL     J+HI            ; INTO POSITION
 256F CA                DEX
 2570 D0F9              BNE     FLSL
                
 2572 A000      FLS2:   LDY     #0              ; MOVE THE FLAG WORD
 2574 B111              LDA     (I),Y           ; INTO [J]
 2576 8516              STA     K+HI            ; FIRST THE MSB
 2578 C8                INY
 2579 B111              LDA     (I),Y


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  81    
--- OBJECT & PROPERTY HANDLERS ---

 257B 8515              STA     K+LO            ; THEN THE LSB
 257D 60                RTS
                
                        END
                
                        INCLUD IO.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  82    
--- GAME I/O: CBM PLUS/4 ---

                
 257E                   ; --------------
 257E                   ; INTERNAL ERROR
 257E                   ; --------------
                
 257E                   ; ENTRY: ERROR CODE IN [A]
                
 257E 496E7465  ERRM:   DB      "Internal error "
 258D 30302E    ENUMB:  DB      "00."
 0012           ERRML   EQU     $-ERRM
                
 2590 A001      ZERROR: LDY     #1              ; CONVERT ERROR CODE IN [A]
 2592 20122F    ECON:   JSR     DIV10           ; TO ASCII IN [ENUMB]
 2595 0930              ORA     #'0'
 2597 998D25            STA     ENUMB,Y
 259A 8A                TXA
 259B 88                DEY
 259C 10F4              BPL     ECON
                
 259E 20CCFF            JSR     CLRCHN          ; RESET I/O
 25A1 203D26            JSR     ZCRLF           ; FLUSH BUFFER
 25A4 A900              LDA     #0
 25A6 8562              STA     SCRIPT          ; DISABLE SCRIPTING
                
 25A8 A27E              LDX     #LOW ERRM
 25AA A925              LDA     #HIGH ERRM
 25AC A012              LDY     #ERRML
 25AE 20A329            JSR     DLINE
                
 25B1                   ; FALL THROUGH ...
                
 25B1                   ; ------------
 25B1                   ; QUIT/RESTART
 25B1                   ; ------------
                
 25B1 203D26    ZQUIT:  JSR     ZCRLF           ; FLUSH BUFFER
                
 25B4 A2C0              LDX     #LOW EOS
 25B6 A925              LDA     #HIGH EOS
 25B8 A00E              LDY     #EOSL
 25BA 20A329            JSR     DLINE           ; "END OF STORY"
                
                ;       LDA     FAST            ; FAST-READ ENGAGED?
                ;       BEQ     FREEZE
                ;       JSR     FOFF            ; DISENGAGE IF SO
                
 25BD 4CBD25    FREEZE: JMP     FREEZE
                
 25C0 456E6420  EOS:    DB      "End of story."
 25CD 0D                DB      EOL
 000E           EOSL    EQU     $-EOS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  83    
--- GAME I/O: CBM PLUS/4 ---

                
 25CE                   ; -------
 25CE                   ; RESTART
 25CE                   ; -------
                
 25CE 203D26    ZSTART: JSR     ZCRLF           ; CLEAR BUFFER
 25D1 4C7116            JMP     WARM1           ; AND DO A WARMSTART
                
 25D4                   ; --------------------
 25D4                   ; PRINT VERSION NUMBER
 25D4                   ; --------------------
                
 25D4 43424D20  VERS:   DB      "CBM Plus/4 Version C"
 25E8 0D                DB      EOL
 0015           VERSL   EQU     $-VERS
                
 25E9 203D26    VERNUM: JSR     ZCRLF           ; FLUSH BUFFER
                
 25EC A2D4              LDX     #LOW VERS
 25EE A925              LDA     #HIGH VERS
 25F0 A015              LDY     #VERSL
 25F2 4CA329            JMP     DLINE
                
 25F5                   ; --------------------------
 25F5                   ; RETURN TOP RAM PAGE IN [A]
 25F5                   ; --------------------------
                
 25F5 A9BE      MEMTOP: LDA     #$BE            ; IT'S A GIVEN
 25F7 60                RTS
                
 25F8                   ; --------------------------------
 25F8                   ; RETURN RANDOM BYTES IN [A] & [X]
 25F8                   ; --------------------------------
                
 25F8 AE1EFF    RANDOM: LDX     HSCAN           ; SOMETHING FROM HERE
 25FB AD1DFF            LDA     VLINEL          ; AND HERE
 25FE 60                RTS
                
 25FF                   ; -------------------
 25FF                   ; Z-PRINT A CHARACTER
 25FF                   ; -------------------
                
 25FF                   ; ENTRY: ASCII CHAR IN [A]
                
 25FF C90D      COUT:   CMP     #EOL            ; IF EOL,
 2601 F03A              BEQ     ZCRLF           ; DO IT!
 2603 C920              CMP     #SPACE          ; IGNORE ALL OTHER
 2605 900B              BCC     CEX             ; CONTROLS
                
 2607 A660              LDX     LENGTH          ; ELSE GET LINE POINTER
 2609 9D3015            STA     LBUFF,X         ; ADD CHAR TO BUFFER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  84    
--- GAME I/O: CBM PLUS/4 ---

 260C E027              CPX     #XSIZE          ; END OF LINE?
 260E B003              BCS     FLUSH           ; YES, FLUSH THE LINE
 2610 E660              INC     LENGTH          ; ELSE UPDATE POINTER
                
 2612 60        CEX:    RTS
                
 2613                   ; -------------------
 2613                   ; FLUSH OUTPUT BUFFER
 2613                   ; -------------------
                
 2613                   ; ENTRY: LENGTH OF BUFFER IN [X]
                
 2613 A920      FLUSH:  LDA     #SPACE
                
 2615 DD3015    FL0:    CMP     LBUFF,X         ; FIND LAST SPACE CHAR
 2618 F005              BEQ     FL1             ; IN THE LINE
 261A CA                DEX
 261B D0F8              BNE     FL0             ; IF NONE FOUND,
 261D A227              LDX     #XSIZE          ; FLUSH ENTIRE LINE
                
 261F 8661      FL1:    STX     OLDLEN          ; SAVE OLD LINE POS HERE
 2621 8660              STX     LENGTH          ; MAKE IT THE NEW LINE LENGTH
                
 2623 203D26            JSR     ZCRLF           ; PRINT LINE UP TO LAST SPACE
                
 2626                   ; START NEW LINE WITH REMAINDER OF OLD
                
 2626 A661              LDX     OLDLEN          ; GET OLD LINE POS
 2628 A000              LDY     #0              ; START NEW LINE AT BEGINNING
 262A E8        FL2:    INX
 262B E027              CPX     #XSIZE          ; CONTINUE IF
 262D 9005              BCC     FL3             ; INSIDE OR
 262F F003              BEQ     FL3             ; AT END OF LINE
 2631 8460              STY     LENGTH          ; ELSE SET NEW LINE LENGTH
 2633 60                RTS
                
 2634 BD3015    FL3:    LDA     LBUFF,X         ; GET CHAR FROM OLD LINE
 2637 993015            STA     LBUFF,Y         ; MOVE TO START OF NEW LINE
 263A C8                INY                     ; UPDATE LENGTH OF NEW LINE
 263B D0ED              BNE     FL2
                
 263D                   ; ---------------
 263D                   ; CARRIAGE RETURN
 263D                   ; ---------------
                
 263D E665      ZCRLF:  INC     LINCNT          ; NEW LINE GOING OUT
 263F A565              LDA     LINCNT          ; IS IT TIME TO
 2641 C566              CMP     LMAX            ; PRINT "MORE" YET?
 2643 9041              BCC     CR1             ; NO, CONTINUE
                
 2645                   ; SCREEN FULL; PRINT "MORE"


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  85    
--- GAME I/O: CBM PLUS/4 ---

                
 2645 20C226            JSR     ZUSL            ; UPDATE STATUS LINE
                
 2648 38                SEC
 2649 20F0FF            JSR     PLOT            ; GET CURSOR POSITION
 264C 8463              STY     OLDX
 264E 8664              STX     OLDY
                
 2650 A900              LDA     #0
 2652 8565              STA     LINCNT          ; RESET LINE COUNTER
 2654 8D3B05            STA     COLOR           ; PAINT IT, BLACK
 2657 85EF              STA     NDX             ; CLEAR QUEUE
                
 2659 A2A9              LDX     #LOW MORE
 265B A926              LDA     #HIGH MORE
 265D A006              LDY     #MOREL
 265F 20A329            JSR     DLINE           ; PRINT "MORE" DIRECTLY
                
 2662 A951              LDA     #WHITE          ; SWITCH BACK
 2664 8D3B05            STA     COLOR           ; TO WHITE TEXT
                
 2667 20E4FF    MWAIT:  JSR     GETIN           ; WAIT FOR ANY KEYPRESS
 266A AA                TAX
 266B F0FA              BEQ     MWAIT
                
 266D A463              LDY     OLDX
 266F A664              LDX     OLDY
 2671 18                CLC
 2672 20F0FF            JSR     PLOT            ; RESTORE CURSOR
                
 2675 A2AF              LDX     #LOW MCLR
 2677 A926              LDA     #HIGH MCLR
 2679 A006              LDY     #MOREL
 267B 20A329            JSR     DLINE           ; RUB OUT "MORE"
                
 267E A463              LDY     OLDX
 2680 A664              LDX     OLDY
 2682 18                CLC
 2683 20F0FF            JSR     PLOT            ; RESTORE CURSOR AGAIN
                
 2686 A660      CR1:    LDX     LENGTH
 2688 A90D              LDA     #EOL            ; INSTALL EOL AT
 268A 9D3015            STA     LBUFF,X         ; END OF CURRENT LINE
 268D E660              INC     LENGTH          ; UPDATE LINE LENGTH
                
 268F A460      LINOUT: LDY     LENGTH          ; IF BUFFER EMPTY,
 2691 F011              BEQ     LINEX           ; DON'T PRINT ANYTHING
                
 2693 846B              STY     PRLEN           ; SAVE LENGTH HERE FOR "PPRINT"
 2695 A200              LDX     #0              ; SEND CONTENTS OF [LBUFF]
 2697 BD3015    LOUT:   LDA     LBUFF,X         ; TO SCREEN


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  86    
--- GAME I/O: CBM PLUS/4 ---

 269A 20E128            JSR     CHAR
 269D E8                INX
 269E 88                DEY
 269F D0F6              BNE     LOUT
                
 26A1 20B629            JSR     PPRINT          ; PRINT [LBUFF] IF ENABLED
                
 26A4 A900      LINEX:  LDA     #0              ; RESET LINE LENGTH
 26A6 8560              STA     LENGTH          ; TO ZERO
 26A8 60                RTS                     ; AND RETURN
                
 26A9 5B4D4F52  MORE:   DB      "[MORE]"
 0006           MOREL   EQU     $-MORE
                
 26AF 20202020  MCLR:   DB      "      "
                
 26B5                   ; ----------------------
 26B5                   ; UPDATE THE STATUS LINE
 26B5                   ; ----------------------
                
 26B5 53636F72  SCORE:  DB      "Score: "
 0007           SCOREL  EQU     $-SCORE
                
 26BC 54696D65  CLOCK:  DB      "Time: "
 0006           CLOCKL  EQU     $-CLOCK
                
 26C2 38        ZUSL:   SEC                     ; SAVE THE CURRENT
 26C3 20F0FF            JSR     PLOT            ; CURSOR POSITION
 26C6 8463              STY     OLDX
 26C8 8664              STX     OLDY
                
 26CA A560              LDA     LENGTH          ; SAVE ALL
 26CC 48                PHA                     ; STRING-PRINTING
 26CD A521              LDA     MPCH            ; VARIABLES
 26CF 48                PHA
 26D0 A520              LDA     MPCM
 26D2 48                PHA
 26D3 A51F              LDA     MPCL
 26D5 48                PHA
 26D6 A54D              LDA     TSET
 26D8 48                PHA
 26D9 A54C              LDA     PSET
 26DB 48                PHA
 26DC A552              LDA     ZWORD+HI
 26DE 48                PHA
 26DF A551              LDA     ZWORD+LO
 26E1 48                PHA
 26E2 A550              LDA     ZFLAG
 26E4 48                PHA
 26E5 A55E              LDA     DIGITS
 26E7 48                PHA


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  87    
--- GAME I/O: CBM PLUS/4 ---

                
 26E8 A227              LDX     #XSIZE
 26EA BD3015    USL0:   LDA     LBUFF,X         ; MOVE CONTENTS OF [LBUFF]
 26ED 9D9015            STA     BUFSAV,X        ; TO [BUFSAV]
 26F0 A920              LDA     #SPACE          ; CLEAR
 26F2 9D3015            STA     LBUFF,X         ; [LBUFF] WITH SPACES
 26F5 CA                DEX
 26F6 10F2              BPL     USL0
                
 26F8 A900              LDA     #0
 26FA 8560              STA     LENGTH          ; RESET LINE LENGTH
 26FC 8562              STA     SCRIPT          ; DISABLE SCRIPTING
 26FE 8D3B05            STA     COLOR           ; PRINT STATUS LINE IN BLACK
                
 2701 A913              LDA     #$13            ; HOME THE
 2703 20D2FF            JSR     CHROUT          ; CURSOR
 2706 A912              LDA     #$12            ; PRINT IN
 2708 20D2FF            JSR     CHROUT          ; INVERSE VIDEO
                
 270B                   ; PRINT ROOM DESCRIPTION
                
 270B A910              LDA     #16             ; GLOBAL VAR #16 (ROOM ID)
 270D 207F18            JSR     GETVRG          ; GET IT INTO [VALUE]
 2710 A50F              LDA     VALUE+LO
 2712 20791B            JSR     PRNTDC          ; PRINT SHORT ROOM DESCRIPTION
                
 2715 A918              LDA     #24             ; MOVE LINE INDEX UP
 2717 8560              STA     LENGTH          ; TO TIME/SCORE POSITION
                
 2719 A911              LDA     #17             ; GLOBAL VAR #17 (SCORE/HOURS)
 271B 207F18            JSR     GETVRG          ; GET IT INTO [VALUE]
                
 271E A55F              LDA     TIMEFL          ; GET MODE FLAG
 2720 D032              BNE     DOTIME          ; USE TIME MODE IF NON-ZERO
                
 2722                   ; PRINT "SCORE"
                
 2722 A953              LDA     #'S'
 2724 20FF25            JSR     COUT
 2727 A963              LDA     #'c'
 2729 20FF25            JSR     COUT
 272C A96F              LDA     #'o'
 272E 20FF25            JSR     COUT
 2731 A972              LDA     #'r'
 2733 20FF25            JSR     COUT
 2736 A965              LDA     #'e'
 2738 20FF25            JSR     COUT
 273B A93A              LDA     #':'
 273D 20FF25            JSR     COUT
 2740 A920              LDA     #SPACE
 2742 20FF25            JSR     COUT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  88    
--- GAME I/O: CBM PLUS/4 ---

                
 2745 A50F              LDA     VALUE+LO        ; MOVE SCORE VALUE
 2747 8556              STA     QUOT+LO         ; INTO [QUOT]
 2749 A510              LDA     VALUE+HI        ; FOR PRINTING
 274B 8557              STA     QUOT+HI
 274D 20C81F            JSR     NUMBER          ; PRINT SCORE VALUE IN DECIMAL
                
 2750 A92F              LDA     #'/'            ; PRINT A SLASH
 2752 D035              BNE     MOVMIN          ; BRANCH ALWAYS
                
 2754                   ; PRINT "TIME"
                
 2754 A954      DOTIME: LDA     #'T'
 2756 20FF25            JSR     COUT
 2759 A969              LDA     #'i'
 275B 20FF25            JSR     COUT
 275E A96D              LDA     #'m'
 2760 20FF25            JSR     COUT
 2763 A965              LDA     #'e'
 2765 20FF25            JSR     COUT
 2768 A93A              LDA     #':'
 276A 20FF25            JSR     COUT
 276D A920              LDA     #SPACE
 276F 20FF25            JSR     COUT
                
 2772 A50F              LDA     VALUE+LO        ; 00 IS REALLY 24
 2774 D002              BNE     DT0
 2776 A918              LDA     #24
 2778 C90D      DT0:    CMP     #13             ; IS HOURS > 12,
 277A 9002              BCC     DT1
 277C E90C              SBC     #12             ; CONVERT TO 1-12
 277E 8556      DT1:    STA     QUOT+LO         ; MOVE FOR PRINTING
 2780 A900              LDA     #0
 2782 8557              STA     QUOT+HI         ; CLEAR MSB
 2784 20C81F            JSR     NUMBER
                
 2787 A93A              LDA     #':'            ; COLON
                
 2789 20FF25    MOVMIN: JSR     COUT            ; PRINT SLASH OR COLON
                
 278C A912              LDA     #18             ; GLOBAL VAR #18 (MOVES/MINUTES)
 278E 207F18            JSR     GETVRG          ; GET IT INTO [VALUE]
 2791 A50F              LDA     VALUE+LO        ; MOVE TO [QUOT]
 2793 8556              STA     QUOT+LO         ; FOR EVENTUAL PRINTING
 2795 A510              LDA     VALUE+HI
 2797 8557              STA     QUOT+HI
                
 2799 A55F              LDA     TIMEFL          ; WHICH MODE?
 279B D006              BNE     DOMINS          ; TIME IF NZ
                
 279D                   ; PRINT NUMBER OF MOVES


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  89    
--- GAME I/O: CBM PLUS/4 ---

                
 279D 20C81F            JSR     NUMBER          ; SHOW # MOVES
 27A0 4CCF27            JMP     STATEX          ; ALL DONE
                
 27A3                   ; PRINT MINUTES
                
 27A3 A50F      DOMINS: LDA     VALUE+LO        ; CHECK MINUTES
 27A5 C90A              CMP     #10             ; IF MORE THAN TEN
 27A7 B005              BCS     DOM0            ; CONTINUE
                
 27A9 A930              LDA     #'0'            ; ELSE PRINT A
 27AB 20FF25            JSR     COUT            ; PADDING "0" FIRST
                
 27AE 20C81F    DOM0:   JSR     NUMBER          ; SHOW MINUTES
                
 27B1 A920              LDA     #SPACE
 27B3 20FF25            JSR     COUT            ; SEPARATE THINGS
                
 27B6 A911              LDA     #17             ; CHECK "HOURS" AGAIN
 27B8 207F18            JSR     GETVRG
 27BB A50F              LDA     VALUE+LO
 27BD C90C              CMP     #12             ; PAST NOON?
 27BF B004              BCS     DOPM            ; YES, PRINT "PM"
                
 27C1 A941              LDA     #'A'            ; ELSE PRINT "AM"
 27C3 D002              BNE     DOXM            ; BRANCH ALWAYS
                
 27C5 A950      DOPM:   LDA     #'P'
                
 27C7 20FF25    DOXM:   JSR     COUT
 27CA A94D              LDA     #'M'
 27CC 20FF25            JSR     COUT
                
 27CF                   ; STATUS LINE READY
                
 27CF A928      STATEX: LDA     #40             ; PRINT THE ENTIRE
 27D1 8560              STA     LENGTH          ; STATUS LINE
 27D3 208626            JSR     CR1
                
 27D6 A951              LDA     #WHITE
 27D8 8D3B05            STA     COLOR           ; BACK TO WHITE TEXT
                
 27DB A227              LDX     #XSIZE          ; RESTORE OLD [LBUFF]
 27DD BD9015    USLX:   LDA     BUFSAV,X
 27E0 9D3015            STA     LBUFF,X
 27E3 CA                DEX
 27E4 10F7              BPL     USLX
                
 27E6 68                PLA                     ; RESTORE ALL
 27E7 855E              STA     DIGITS          ; SAVED VARIABLES
 27E9 68                PLA


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  90    
--- GAME I/O: CBM PLUS/4 ---

 27EA 8550              STA     ZFLAG
 27EC 68                PLA
 27ED 8551              STA     ZWORD+LO
 27EF 68                PLA
 27F0 8552              STA     ZWORD+HI
 27F2 68                PLA
 27F3 854C              STA     PSET
 27F5 68                PLA
 27F6 854D              STA     TSET
 27F8 68                PLA
 27F9 851F              STA     MPCL
 27FB 68                PLA
 27FC 8520              STA     MPCM
 27FE 68                PLA
 27FF 8521              STA     MPCH
 2801 68                PLA
 2802 8560              STA     LENGTH
                
 2804 A664              LDX     OLDY            ; RESTORE CURSOR
 2806 A463              LDY     OLDX
 2808 18                CLC
 2809 20F0FF            JSR     PLOT
                
 280C A2FF              LDX     #$FF
 280E 8662              STX     SCRIPT          ; RE-ENABLE SCRIPTING
 2810 E8                INX                     ; = 0
 2811 8622              STX     MPCFLG          ; INVALIDATE [MPC]
 2813 60                RTS
                
                        END
                        INCLUD MACHINE.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  91    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

                
 2814                   ; ----------------------------
 2814                   ; FETCH ASCII KEYCODE INTO [A]
 2814                   ; ----------------------------
                
 2814                   ; EXIT: ASCII IN [A] & [IOCHAR]
                
 00E0           CYCLE   EQU     $E0             ; SHORT BLINK CYCLE
 0064           CURSOR  EQU     $64             ; SCREEN CODE FOR UNDERLINE
                
 2814 8A        GETKEY: TXA                     ; SAVE [X] & [Y]
 2815 48                PHA
 2816 98                TYA
 2817 48                PHA
                
 2818 A900      GKEY0:  LDA     #0
 281A 85DE              STA     BLINK+LO        ; SET CURSOR BLINK
 281C 85DF              STA     BLINK+HI        ; FOR A LONG CYCLE
                
 281E 38                SEC                     ; GET CURSOR COORDINATES
 281F 20F0FF            JSR     PLOT            ; INTO [X/Y]
                
 2822 84D9              STY     COLUMN          ; SAVE X-POS HERE
                
 2824 BDA72A            LDA     LOLINE,X        ; GET LSB OF ROW ADDRESS
 2827 85DA              STA     SROW+LO         ; SAVE HERE
 2829 85DC              STA     CROW+LO         ; AND HERE
 282B BDC02A            LDA     HILINE,X        ; GET LSB OF ROW ADDRESS
 282E 85DB              STA     SROW+HI         ; USE AS-IS FOR SCREEN RAM
 2830 38                SEC                     ; SUBTRACT 1K
 2831 E904              SBC     #4              ; TO GET CORRESPONDING
 2833 85DD              STA     CROW+HI         ; COLOR RAM ADDRESS
                
 2835 A964              LDA     #CURSOR
 2837 85D8              STA     CSHAPE
 2839 91DA              STA     (SROW),Y        ; FORCE CURSOR ON
 283B A900              LDA     #0
 283D 91DC              STA     (CROW),Y        ; MAKE CURSOR BLACK
                
 283F 20E4FF    GKEY1:  JSR     GETIN           ; GET A KEYCODE
 2842 AA                TAX                     ; SAVE IT HERE
 2843 A4D9              LDY     COLUMN          ; NEED THIS FOR LATER
                
 2845 E6DE              INC     BLINK+LO        ; UPDATE THE
 2847 D018              BNE     NOBLIN          ; BLINK TIMER
 2849 E6DF              INC     BLINK+HI
 284B D014              BNE     NOBLIN
                
 284D A9E0              LDA     #CYCLE          ; RESET THE CURSOR
 284F 85DF              STA     BLINK+HI        ; FOR SHORT DUTY CYCLE
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  92    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2851 A5D8              LDA     CSHAPE          ; FLIP THE CURSOR SHAPE
 2853 C920              CMP     #SPACE
 2855 F004              BEQ     SHAP0           ; IF SPACE, USE CURSOR
 2857 A920              LDA     #SPACE          ; ELSE USE SPACE
 2859 D002              BNE     SHAP1
                
 285B A964      SHAP0:  LDA     #CURSOR
                
 285D 85D8      SHAP1:  STA     CSHAPE          ; UPDATE CURSOR SHAPE
 285F 91DA              STA     (SROW),Y        ; INTO SCREEN RAM
                
 2861 8A        NOBLIN: TXA                     ; ANY KEY PRESSED?
 2862 F0DB              BEQ     GKEY1           ; NOT IF CODE WAS ZERO
                
 2864                   ; CONVERT & MASK KEYCODE IN [A]
                
 2864 C941              CMP     #'A'            ; CONVERT UNSHIFTED ALPHA
 2866 9006              BCC     MASK            ; TO ASCII LOWER CASE
 2868 C95B              CMP     #'Z'+1
 286A B002              BCS     MASK
 286C 6920              ADC     #$20
                
 286E 297F      MASK:   AND     #%01111111      ; SCREEN OUT SHIFTS
                
 2870 C90D              CMP     #EOL            ; EOL?
 2872 F02A              BEQ     TICK
 2874 C914              CMP     #BACKSP         ; BACKSPACE?
 2876 F026              BEQ     TICK
 2878 C920              CMP     #SPACE          ; ANYTHING ELSE < "SPACE"
 287A 901C              BCC     BADKEY          ; IS BAD
                
 287C C93C              CMP     #'<'            ; CHANGE "<"
 287E D004              BNE     MASK0           ; TO ","
 2880 A92C              LDA     #','
 2882 D01A              BNE     TICK
                
 2884 C93E      MASK0:  CMP     #'>'            ; CHANGE ">"
 2886 D004              BNE     MASK1           ; TO "."
 2888 A92E              LDA     #'.'
 288A D012              BNE     TICK
                
 288C C97B      MASK1:  CMP     #'z'+1          ; PASS L-C ALPHA
 288E B008              BCS     BADKEY
 2890 C961              CMP     #'a'
 2892 B00A              BCS     TICK
                
 2894 C95B              CMP     #'Z'+1          ; PASS U-C ALPHA
 2896 9006              BCC     TICK            ; AND OTHER ASCII CHARS
                
 2898 20772A    BADKEY: JSR     BOOP            ; REJECT BAD KEYPRESS
 289B 4C1828            JMP     GKEY0           ; AND TRY AGAIN


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  93    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

                
 289E                   ; "CLICK" THE KEY
                
 289E 8567      TICK:   STA     IOCHAR          ; SAVE KEYCODE HERE
                
 28A0 A920              LDA     #SPACE
 28A2 91DA              STA     (SROW),Y        ; ERASE CURSOR
 28A4 A951              LDA     #WHITE
 28A6 91DC              STA     (CROW),Y        ; MAKE SURE COLOR RAM IS WHITE
                
 28A8 A900              LDA     #0              ; LSB CLICK FREQ
 28AA 8D0EFF            STA     V1FLSB
 28AD A907              LDA     #7              ; MSB CLICK FREQ W/BIT 2 SET
 28AF 8D12FF            STA     BITMAP
 28B2 A91F              LDA     #$1F            ; VOICE #1, FULL VOLUME
 28B4 8D11FF            STA     VOLUME
                
 28B7 A200              LDX     #0              ; A SHORT DELAY ...
 28B9 A005              LDY     #5
 28BB CA        TICK0:  DEX
 28BC D0FD              BNE     TICK0
 28BE 88                DEY
 28BF D0FA              BNE     TICK0
 28C1 8C11FF            STY     VOLUME          ; ... THEN SHUT OFF SOUND
                
 28C4 68                PLA                     ; RESTORE [X] & [Y]
 28C5 A8                TAY
 28C6 68                PLA
 28C7 AA                TAX
 28C8 A567              LDA     IOCHAR          ; RESTORE CODE INTO [A]
 28CA 60                RTS
                
 28CB                   ; -------------------------
 28CB                   ; OUTPUT AN ASCII CHARACTER
 28CB                   ; -------------------------
                
 28CB C961      LETTER: CMP     #'a'            ; LOWER-CASE?
 28CD 9005              BCC     LET0            ; NO, CONTINUE
 28CF 295F              AND     #%01011111      ; ELSE MASK FOR LOWER-CASE
 28D1 4CD2FF            JMP     CHROUT
                
 28D4 C941      LET0:   CMP     #'A'            ; UPPER-CASE?
 28D6 9006              BCC     LETEX
 28D8 C95B              CMP     #'Z'+1
 28DA B002              BCS     LETEX
 28DC 0920              ORA     #%00100000      ; MAKE UPPER
                
 28DE 4CD2FF    LETEX:  JMP     CHROUT
                
 28E1                   ; -----------------
 28E1                   ; PRINT CHAR IN [A]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  94    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 28E1                   ; -----------------
                
 28E1 8567      CHAR:   STA     IOCHAR          ; SAVE HERE
 28E3 8A                TXA                     ; SAVE [X] AND [Y]
 28E4 48                PHA
 28E5 98                TYA
 28E6 48                PHA
                
 28E7 38                SEC                     ; GET CURSOR X- AND Y-POS
 28E8 20F0FF            JSR     PLOT            ; INTO [Y] AND [X], RESPECTIVELY
 28EB 98                TYA
 28EC C928              CMP     #40             ; STRIP OFF THE
 28EE 9003              BCC     CHKEOL          ; LOGICAL LINE OFFSET
 28F0 E928              SBC     #40             ; UPDATE [Y] IF NECESSARY
 28F2 A8                TAY
                
 28F3 A567      CHKEOL: LDA     IOCHAR          ; RESTORE CHAR
 28F5 C90D              CMP     #EOL            ; IS IT EOL?
 28F7 F047              BEQ     OUTEOL          ; YES, SPECIAL HANDLING
                
 28F9                   ; HANDLE A NON-EOL CHAR
                
 28F9 E017              CPX     #YSIZE-1        ; ON LAST SCREEN LINE?
 28FB 9039              BCC     NOSCRL          ; NO, NO SCROLL NEEDED
 28FD C027              CPY     #XSIZE          ; LAST CHAR ON LINE?
 28FF 9035              BCC     NOSCRL          ; NO, DON'T SCROLL
                
 2901                   ; SCROLL THE SCREEN
                
 2901 CA        DOSCRL: DEX                     ; PUSH CURSOR UP ONE LINE
 2902 18                CLC
 2903 20F0FF            JSR     PLOT            ; RESET THE CURSOR
                
 2906 A668              LDX     SLINE           ; GET CURRENT SCROLL LINE
                
 2908 E018      SRL0:   CPX     #YSIZE
 290A F020              BEQ     SRL2            ; SCROLL DONE
                
 290C BDA72A            LDA     LOLINE,X        ; GET ADDR OF DEST LINE
 290F 856E              STA     LTO+LO          ; INTO [LTO]
 2911 BDC02A            LDA     HILINE,X
 2914 856F              STA     LTO+HI
                
 2916 E8                INX
 2917 BDA72A            LDA     LOLINE,X        ; GET ADDR OF SOURCE LINE
 291A 856C              STA     LFROM+LO        ; INTO [LFROM]
 291C BDC02A            LDA     HILINE,X
 291F 856D              STA     LFROM+HI
                
 2921 A027              LDY     #XSIZE
 2923 B16C      SRL1:   LDA     (LFROM),Y       ; MOVE SOURCE LINE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  95    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2925 916E              STA     (LTO),Y         ; TO DEST LINE
 2927 88                DEY
 2928 10F9              BPL     SRL1
                
 292A 30DC              BMI     SRL0            ; LOOP TILL [X] = YSIZE
                
 292C A227      SRL2:   LDX     #XSIZE
 292E A920              LDA     #SPACE
 2930 9DC00F    SRL3:   STA     SCREEN+960,X    ; CLEAR LAST LINE
 2933 CA                DEX                     ; OF SCREEN RAM
 2934 10FA              BPL     SRL3
                
 2936 A567      NOSCRL: LDA     IOCHAR          ; RESTORE CHAR
 2938 20CB28            JSR     LETTER          ; OFF TO THE SCREEN!
 293B 68                PLA                     ; RESTORE [X] AND [Y]
 293C A8                TAY
 293D 68                PLA
 293E AA                TAX
 293F 60                RTS
                
 2940                   ; HANDLE EOL
                
 2940 E017      OUTEOL: CPX     #YSIZE-1        ; LAST SCREEN LINE?
 2942 90F2              BCC     NOSCRL          ; NO, DON'T SCROLL
 2944 B0BB              BCS     DOSCRL          ; ELSE SCROLL
                
 2946                   ; ---------------------
 2946                   ; FETCH A LINE OF INPUT
 2946                   ; ---------------------
                
 2946                   ; ENTRY: ABS ADDR OF READ BUFFER IN [ARG1]
 2946                   ; EXIT: # CHARS READ IN [A]
                
 2946 208F26    INPUT:  JSR     LINOUT          ; FLUSH [LBUFF]
                
 2949 A000              LDY     #0
 294B 8465              STY     LINCNT          ; RESET LINE COUNT
 294D 84EF              STY     NDX             ; AND KEY QUEUE
                
 294F 201428    INLOOP: JSR     GETKEY          ; GET ASCII INTO [A] AND [IOCHAR]
                
 2952 C90D              CMP     #EOL            ; EOL?
 2954 F02A              BEQ     ENDLIN          ; LINE DONE IF SO
 2956 C914              CMP     #BACKSP         ; BACKSPACE?
 2958 F01C              BEQ     BACKUP          ; SPECIAL HANDLING
                
 295A 993015            STA     LBUFF,Y         ; ELSE ADD CHAR TO INPUT BUFFER
 295D C8                INY                     ; NEXT POSITION IN LINE
                
 295E 20E128    SHOWIT: JSR     CHAR            ; SEND TO SCREEN
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  96    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2961 C04D              CPY     #77             ; 2 SCREEN LINES FULL?
 2963 90EA              BCC     INLOOP          ; NO, GET ANOTHER CHAR
                
 2965                   ; HANDLE LINE OVERFLOW
                
 2965 201428    NOMORE: JSR     GETKEY
 2968 C90D              CMP     #EOL            ; IF EOL,
 296A F014              BEQ     ENDLIN          ; WRAP UP THE LINE
 296C C914              CMP     #BACKSP         ; BACKSPACE
 296E F006              BEQ     BACKUP          ; IS OKAY TOO
 2970 20772A            JSR     BOOP            ; ELSE COMPLAIN
 2973 4C6529            JMP     NOMORE          ; AND INSIST
                
 2976                   ; HANDLE BACKSPACE
                
 2976 88        BACKUP: DEY                     ; BACK UP THE POINTER
 2977 10E5              BPL     SHOWIT          ; SEND BS IF NOT START OF LINE
 2979 20772A            JSR     BOOP            ; ELSE SCREAM WITH PAIN
 297C A000              LDY     #0              ; RESET POINTER
 297E F0CF              BEQ     INLOOP          ; AND WAIT FOR SOMETHING BETTER
                
 2980                   ; HANDLE END OF LINE
                
 2980 993015    ENDLIN: STA     LBUFF,Y         ; SHIP EOL TO BUFFER
 2983 C8                INY                     ; UPDATE INDEX
 2984 8445              STY     LINLEN          ; SAVE HERE FOR "READ"
 2986 846B              STY     PRLEN           ; AND HERE FOR "PPRINT"
 2988 20E128            JSR     CHAR            ; AND SEND EOL TO SCREEN
                
 298B                   ; MOVE [LBUFF] TO [ARG1] W/LC CONVERSION
                
 298B B92F15    LEX1:   LDA     LBUFF-1,Y       ; GET A CHAR FROM [LBUFF]
 298E C941              CMP     #'A'            ; IF CHAR IS ALPHA,
 2990 9006              BCC     LEX2            ; CONVERT TO LOWER CASE
 2992 C95B              CMP     #'Z'+1
 2994 B002              BCS     LEX2
 2996 6920              ADC     #$20
 2998 9105      LEX2:   STA     (ARG1),Y        ; MOVE CHAR TO INPUT BUFFER AT [ARG1]
 299A 88                DEY                     ; LOOP TILL
 299B 10EE              BPL     LEX1            ; ALL CHARS MOVED
                
 299D 20B629            JSR     PPRINT          ; SCRIPT [LBUFF] IF ENABLED
                
 29A0 A545              LDA     LINLEN          ; RESTORE # CHARS
 29A2 60                RTS                     ; INTO [A]
                
 29A3                   ; -----------------------
 29A3                   ; DIRECT PRINT LINE [X/A]
 29A3                   ; -----------------------
                
 29A3                   ; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  97    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 29A3                   ;        STRING LENGTH IN [Y]
                
 29A3 8EAC29    DLINE:  STX     STRING+LO       ; DROP STRING ADDRESS
 29A6 8DAD29            STA     STRING+HI       ; INTO DUMMY OPERAND BYTES
 29A9 A200              LDX     #0              ; INIT BYTE-FETCHING INDEX
                
 29AB BD        DOUT:   DB      $BD             ; 6502 "LDA nnnn,X" OPCODE
 29AC 0000      STRING: DW      $0000           ; DUMMY OPERAND BYTES
 29AE 20E128            JSR     CHAR
 29B1 E8                INX
 29B2 88                DEY                     ; STRING DONE?
 29B3 D0F6              BNE     DOUT            ; NO, KEEP PRINTING
 29B5 60                RTS
                
 29B6                   ; -----------------------
 29B6                   ; SEND [LBUFF] TO PRINTER
 29B6                   ; -----------------------
                
 29B6                   ; ENTRY: LENTH OF LINE IN [PRLEN]
                
 29B6 A562      PPRINT: LDA     SCRIPT          ; SCRIPTING INTERNALLY ENABLED?
 29B8 F04E              BEQ     PEX             ; NO, SCRAM IMMEDIATELY
                
 29BA AD1132            LDA     ZBEGIN+ZSCRIP+1 ; CHECK SCRIPT FLAG
 29BD 2901              AND     #%00000001      ; SCRIPTING ON?
 29BF F038              BEQ     PP3             ; NO, CHECK FOR "UNSCRIPT"
                
 29C1 A56A              LDA     PSTAT           ; CHECK PRINTER STATUS
 29C3 3043              BMI     PEX             ; CAN'T OPEN IF NEGATIVE
 29C5 D01A              BNE     PP1             ; ALREADY OPEN, SCRIPT THE LINE
                
 29C7                   ; OPEN THE PRINTER FOR OUTPUT
                
                ;       LDA     FAST            ; FAST-READ ENGAGED?
                ;       BEQ     PP0             ; NO, IGNORE
                ;       JSR     FOFF            ; ELSE DISENGAGE
                ;       LDA     #8
                ;       JSR     DOPEN           ; AND RESET THE DRIVE
                
 29C7 E66A      PP0:    INC     PSTAT           ; SET STATUS TO "PRINTER OPENED" (1)
                
 29C9 A904              LDA     #4              ; LOGICAL FILE #4
 29CB AA                TAX                     ; DEVICE #4
 29CC A007              LDY     #7              ; ALLOW UPPER/LOWER CASE
 29CE 20BAFF            JSR     SETLFS          ; SET UP LOGICAL FILE
 29D1 A900              LDA     #0
 29D3 20BDFF            JSR     SETNAM          ; NO FILENAME REQUIRED
 29D6 20C0FF            JSR     OPEN            ; OPEN THE CHANNEL
 29D9 9006              BCC     PP1             ; OPEN OKAY IF CARRY CLEAR
                
 29DB A9FF      PPERR:  LDA     #$FF            ; ELSE SET PRINTER STATUS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  98    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 29DD 856A              STA     PSTAT           ; TO "CAN'T OPEN"
 29DF D022              BNE     PP5             ; AND SIMULATE AN "UNSCRIPT"
                
 29E1 A204      PP1:    LDX     #4              ; SET PRINTER CHANNEL
 29E3 20C9FF            JSR     CHKOUT          ; TO "OUTPUT"
 29E6 B0F3              BCS     PPERR           ; ERROR IF CARRY SET
                
 29E8 A000              LDY     #0              ; INIT INDEX
 29EA B93015    PP2:    LDA     LBUFF,Y
 29ED 20CB28            JSR     LETTER
 29F0 B0E9              BCS     PPERR           ; ERROR IF CARRY SET
 29F2 C8                INY
 29F3 C66B              DEC     PRLEN
 29F5 D0F3              BNE     PP2
                
 29F7 F00F              BEQ     PEX             ; RESET & RETURN
                
 29F9                   ; CHECK FOR "UNSCRIPT"
                
 29F9 A56A      PP3:    LDA     PSTAT           ; CHECK PRINTER STATUS
 29FB F00B              BEQ     PEX             ; EXIT IF PRINTER WAS OFF
 29FD 3009              BMI     PEX             ; OR UNOPENABLE
                
 29FF A900      PP4:    LDA     #0              ; RESET PRINTER STATUS FLAG
 2A01 856A              STA     PSTAT           ; TO "CLOSED"
                
 2A03                   ; ENTRY FOR PRINTER ERROR
                
 2A03 A904      PP5:    LDA     #4
 2A05 20C3FF            JSR     CLOSE           ; CLOSE THE PRINTER CHANNEL
                
                ;       LDA     FAST            ; FAST-READ AVAILABLE?
                ;       BEQ     PEX             ; NO, EXIT
                ;       LDA     #8              ; ELSE
                ;       JSR     DOPEN           ; RESET BOOT DRIVE
                ;       JMP     FINIT           ; RE-ENGAGE FAST-READ & RETURN
                
 2A08 4CCCFF    PEX:    JMP     CLRCHN
                
 2A0B                   ; ------------
 2A0B                   ; SPLIT SCREEN
 2A0B                   ; ------------
                
 2A0B                   ; SPLIT SCREEN AT LINE [ARG1]
 2A0B                   ; DISABLE SPLIT IF [ARG1] = 0
 2A0B                   ; IGNORE IF SPLIT ALREADY ENABLED OR [ARG1] >= 20
                
 2A0B A605      ZSPLIT: LDX     ARG1+LO         ; IF [ARG1] = 0,
 2A0D F02F              BEQ     OFFSPL          ; TURN OFF SPLIT SCREEN
                
 2A0F A569              LDA     SPSTAT          ; SPLIT ALREADY ENABLED?


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE  99    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2A11 D02A              BNE     SPLEX           ; IGNORE REQUEST IF SO
                
 2A13 E014              CPX     #20             ; IF [ARG1] >= 20,
 2A15 B026              BCS     SPLEX           ; IGNORE
                
 2A17 E8                INX
 2A18 8668              STX     SLINE           ; ELSE SET NEW SPLIT LINE
 2A1A 8669              STX     SPSTAT          ; SET "SPLIT ENABLED" FLAG
                
 2A1C BDA72A    SPL0:   LDA     LOLINE,X        ; MAKE [LFROM] POINT TO
 2A1F 856C              STA     LFROM+LO        ; LINE [X] IN WINDOW
 2A21 BDC02A            LDA     HILINE,X
 2A24 856D              STA     LFROM+HI
                
 2A26 A027              LDY     #XSIZE          ; CLEAR LINE [X]
 2A28 A920              LDA     #SPACE
 2A2A 916C      SPL1:   STA     (LFROM),Y
 2A2C 88                DEY
 2A2D 10FB              BPL     SPL1
                
 2A2F CA                DEX                     ; DONE ALL LINES?
 2A30 D0EA              BNE     SPL0            ; LOOP TILL WINDOW CLEARED
 2A32 8665              STX     LINCNT          ; RESET LINE COUNT TO ZERO
                
 2A34 A917      SPCALC: LDA     #YSIZE-1        ; CALCULATE # LINES TO SCROLL
 2A36 38                SEC                     ; BEFORE "MORE" APPEARS:
 2A37 E568              SBC     SLINE           ; LMAX = YSIZE-SLINE-1
 2A39 8566              STA     LMAX
 2A3B C666              DEC     LMAX
                
 2A3D 60        SPLEX:  RTS
                
 2A3E                   ; --------------------
 2A3E                   ; DISABLE SPLIT SCREEN
 2A3E                   ; --------------------
                
 2A3E 206A2A    OFFSPL: JSR     TOBOT
                
 2A41 A201      SPLOFF: LDX     #1
 2A43 8668              STX     SLINE           ; SPLIT AT LINE 1
 2A45 CA                DEX                     ; = 0
 2A46 8669              STX     SPSTAT          ; TURN OFF STATUS FLAG
 2A48 8665              STX     LINCNT          ; RESET LINE COUNT
 2A4A A915              LDA     #21
 2A4C 8566              STA     LMAX            ; SET MAXIMUM LINE SCROLL
 2A4E A940              LDA     #%01000000
 2A50 8DE907            STA     LINKEN          ; DISABLE LINE LINKING
 2A53 60                RTS
                
 2A54                   ; ------
 2A54                   ; SCREEN


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 100    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2A54                   ; ------
                
 2A54                   ; GO TO TOP WINDOW IF [A] = 0
 2A54                   ; GO TO BOTTOM IF [A] = 1
 2A54                   ; IGNORE IF SPLIT NOT ENABLED OR [A] <> 0 OR 1
                
 2A54 A569      ZSCRN:  LDA     SPSTAT          ; IF SPLIT NOT ENABLED,
 2A56 F0E5              BEQ     SPLEX           ; IGNORE REQUEST
                
 2A58 A505              LDA     ARG1+LO         ; IF [ARG1] = 0,
 2A5A 0506              ORA     ARG1+HI
 2A5C F00C              BEQ     TOBOT           ; GO TO BOTTOM WINDOW
 2A5E C901              CMP     #1              ; IF [ARG1] <> 1,
 2A60 D0DB              BNE     SPLEX           ; IGNORE THE REQUEST
                
 2A62                   ; SET TO TOP WINDOW
                
 2A62 A215      TOTOP:  LDX     #21             ; TEMPORARILY RESET
 2A64 8666              STX     LMAX            ; [LMAX] TO KILL "MORE"
 2A66 A201              LDX     #1              ; Y-POS = 1
 2A68 D005              BNE     DOSCRN
                
 2A6A                   ; SET TO BOTTOM WINDOW
                
 2A6A 20342A    TOBOT:  JSR     SPCALC          ; RE-CALC [LMAX]
 2A6D A217              LDX     #23             ; Y-POS = 23
                
 2A6F A000      DOSCRN: LDY     #0              ; X-POS = 0
 2A71 8465              STY     LINCNT          ; RESET LINE COUNT
 2A73 18                CLC
 2A74 4CF0FF            JMP     PLOT            ; SET CURSOR TO X=[Y], Y=[X]
                
 2A77                   ; ---------
 2A77                   ; RAZZ USER
 2A77                   ; ---------
                
 2A77 A900      BOOP:   LDA     #0              ; LSB BOOP FREQ
 2A79 8D0EFF            STA     V1FLSB
 2A7C A904              LDA     #4
 2A7E 8D12FF            STA     BITMAP          ; MSB FREQ W/BIT 2 SET
 2A81 A91F              LDA     #$1F
 2A83 8D11FF            STA     VOLUME          ; VOICE #1, FULL VOLUME
                
 2A86 A9FC              LDA     #252
 2A88 85A5              STA     TIME
 2A8A A5A5      BOOPL:  LDA     TIME            ; WAIT 6 JIFFIES
 2A8C D0FC              BNE     BOOPL
                
 2A8E 8D11FF            STA     VOLUME          ; SOUND OFF
 2A91 60                RTS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 101    
--- MACHINE-DEPENDENT I/O: CBM PLUS/4 ---

 2A92                   ; ------------------------
 2A92                   ; CLEAR SCREEN & COLOR RAM
 2A92                   ; ------------------------
                
 2A92 A951      CLS:    LDA     #WHITE          ; SET FOREGROUND COLOR
 2A94 8D3B05            STA     COLOR           ; TO WHITE
 2A97 A993              LDA     #147            ; "CLEAR SCREEN" CHAR
 2A99 20D2FF            JSR     CHROUT
                
 2A9C A000              LDY     #0              ; CURSOR TO (0,1)
 2A9E A201              LDX     #1
 2AA0 18                CLC
 2AA1 20F0FF            JSR     PLOT
                
 2AA4 4C412A            JMP     SPLOFF          ; DISABLE SPLIT SCREEN
                
 2AA7                   ; -------------------
 2AA7                   ; LINE ADDRESS TABLES
 2AA7                   ; -------------------
                
 2AA7 00285078  LOLINE: DB      $00,$28,$50,$78,$A0,$C8,$F0,$18
 2AAF 406890B8          DB      $40,$68,$90,$B8,$E0,$08,$30,$58
 2AB7 80A8D0F8          DB      $80,$A8,$D0,$F8,$20,$48,$70,$98
 2ABF C0                DB      $C0
                
 2AC0 0C0C0C0C  HILINE: DB      $0C,$0C,$0C,$0C,$0C,$0C,$0C
 2AC7 0D0D0D0D          DB      $0D,$0D,$0D,$0D,$0D,$0D
 2ACD 0E0E0E0E          DB      $0E,$0E,$0E,$0E,$0E,$0E,$0E
 2AD4 0F0F0F0F          DB      $0F,$0F,$0F,$0F,$0F
                
                        END
                        INCLUD ZDOS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 102    
--- Z-DOS: CBM PLUS/4 ---

                
 2AD9                   ; ---------------------
 2AD9                   ; GET Z-BLOCK FROM DISK
 2AD9                   ; ---------------------
                
 2AD9                   ; ENTRY: Z-BLOCK # IN [BLOCK]
 2AD9                   ;        TARGET PAGE IN [DBUFF+HI]
                
 2AD9 D8        GETDSK: CLD
 2ADA A908              LDA     #8
 2ADC 857A              STA     DRIVE           ; GAME ALWAYS PLAYS FROM DRIVE #8
                
 2ADE A570              LDA     DBLOCK+LO
 2AE0 857B              STA     DVD+LO
 2AE2 A571              LDA     DBLOCK+HI
 2AE4 2901              AND     #%00000001      ; FOR 128K VIRTUAL SYSTEM
 2AE6 857C              STA     DVD+HI
                
 2AE8 A900              LDA     #0
 2AEA 857E              STA     DSOR+HI         ; CLEAR MSB
 2AEC 857F              STA     DTEMP+LO        ; AND [DTEMP]
 2AEE 8580              STA     DTEMP+HI
                
 2AF0 A211              LDX     #17             ; 17 SECTORS/TRACK
 2AF2 867D              STX     DSOR+LO         ; LSB OF DIVISOR
 2AF4 CA                DEX                     ; (= 16) INIT DIVIDE LOOP INDEX
 2AF5 18                CLC
                
 2AF6 267B      DVLP:   ROL     DVD+LO
 2AF8 267C              ROL     DVD+HI
 2AFA 267F              ROL     DTEMP+LO
 2AFC 2680              ROL     DTEMP+HI
                
 2AFE A57F              LDA     DTEMP+LO
 2B00 38                SEC
 2B01 E57D              SBC     DSOR+LO
 2B03 A8                TAY
 2B04 A580              LDA     DTEMP+HI
 2B06 E57E              SBC     DSOR+HI
 2B08 9004              BCC     DVLP1
 2B0A 847F              STY     DTEMP+LO
 2B0C 8580              STA     DTEMP+HI
                
 2B0E CA        DVLP1:  DEX
 2B0F D0E5              BNE     DVLP
                
 2B11 267B              ROL     DVD+LO
 2B13 267C              ROL     DVD+HI          ; SHIFT LAST CARRY
                
 2B15 A57F              LDA     DTEMP+LO        ; REMAINDER IN [DTEMP]
 2B17 8575              STA     SECTOR          ; IS SECTOR ID (0-16)


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 103    
--- Z-DOS: CBM PLUS/4 ---

                
 2B19 A57B              LDA     DVD+LO          ; QUOTIENT IN [DVD] IS TRACK ID
 2B1B 18                CLC
 2B1C 6905              ADC     #5              ; Z-CODE STARTS ON TRACK 5
                
 2B1E C911              CMP     #17             ; BELOW TRACK 17?
 2B20 900F              BCC     DVLP2           ; YES, DONE
 2B22 18                CLC
 2B23 6901              ADC     #1              ; ELSE SKIP OVER 17
 2B25 C924              CMP     #36             ; OUT OF RANGE?
 2B27 B042              BCS     TRKERR          ; ERROR IF SO
 2B29 C912              CMP     #18             ; IS THIS TRACK 18?
 2B2B D004              BNE     DVLP2           ; NO, DONE
 2B2D E675              INC     SECTOR          ; ELSE SKIP OVER
 2B2F E675              INC     SECTOR          ; SECTORS 0 & 1
                
 2B31 8574      DVLP2:  STA     TRACK
                
                ;       LDA     DBLOCK+LO       ; GET LSB OF BLOCK ID
                ;       AND     #%00001111      ; MASK TO GET
                ;       STA     SECTOR          ; SECTOR # (0-15)
                
                ;       LDA     DBLOCK+HI       ; GET MSB OF BLOCK ID
                ;       AND     #%00001111      ; MASK OUT GARBAGE IN BITS 7-4
                ;       ASL     A               ; SHIFT THE LOW NIBBLE
                ;       ASL     A               ; INTO THE HIGH NIBBLE
                ;       ASL     A
                ;       ASL     A
                ;       STA     TRACK           ; AND SAVE IT HERE FOR A MOMENT
                
                ;       LDA     DBLOCK+LO       ; GET LSB OF BLOCK ID AGAIN
                ;       AND     #%11110000      ; MASK OUT SECTOR #
                ;       LSR     A               ; SHIFT THE HIGH NIBBLE
                ;       LSR     A               ; INTO THE LOW NIBBLE
                ;       LSR     A
                ;       LSR     A
                ;       ORA     TRACK           ; SUPERIMPOSE NEW HIGH NIBBLE
                
                ;       CLC
                ;       ADC     #5              ; Z-CODE STARTS ON TRACK 5
                
                ;       CMP     #17             ; BELOW TRACK 17?
                ;       BCC     TRAKOK          ; USE AS-IS IF SO
                ;       CLC                     ; ELSE SKIP OVER
                ;       ADC     #2              ; TRACKS 17 & 18
                ;       CMP     #36             ; ANYTHING HIGHER THAN TRACK 35
                ;       BCS     TRKERR          ; IS AN ERROR
                
                ;TRAKOK:STA     TRACK
                
 2B33                   ; ENTRY FOR "RESTORE" ([TRACK], [SECTOR] & [DRIVE] PRE-ASSIGNED)


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 104    
--- Z-DOS: CBM PLUS/4 ---

                
 2B33 18        GETRES: CLC                     ; CARRY CLEAR = "READ BLOCK"
 2B34 208E2F            JSR     DISK            ; GO DO IT!
 2B37 B037              BCS     DSKERR          ; ERROR IF CARRY SET
                
 2B39 A000              LDY     #0              ; MOVE CONTENTS OF [IOBUFF]
 2B3B B90010    GDKL:   LDA     IOBUFF,Y        ; TO THE
 2B3E 9172              STA     (DBUFF),Y       ; TARGET PAGE IN [DBUFF]
 2B40 C8                INY
 2B41 D0F8              BNE     GDKL
                
 2B43 E670              INC     DBLOCK+LO       ; POINT TO NEXT
 2B45 D002              BNE     GDEX            ; Z-BLOCK
 2B47 E671              INC     DBLOCK+HI
 2B49 4C5C2B    GDEX:   JMP     NXTSEC          ; POINT TO NEXT SECTOR & PAGE
                
 2B4C                   ; --------------------
 2B4C                   ; PUT [DBLOCK] TO DISK
 2B4C                   ; --------------------
                
 2B4C                   ; ENTRY: [TRACK], [SECTOR] & [DRIVE] ASSIGNED
 2B4C                   ;        PAGE TO WRITE IN [DBUFF]
                
 2B4C A000      PUTDSK: LDY     #0              ; MOVE PAGE AT [DBUFF]
 2B4E B172      PTKL:   LDA     (DBUFF),Y       ; INTO
 2B50 990010            STA     IOBUFF,Y        ; [IOBUFF] FOR I/O
 2B53 C8                INY
 2B54 D0F8              BNE     PTKL
                
 2B56 38                SEC                     ; CARRY SET = "WRITE BLOCK"
 2B57 208E2F            JSR     DISK
 2B5A B00E              BCS     WRTERR          ; CARRY SET IF ERROR
                
 2B5C E675      NXTSEC: INC     SECTOR          ; POINT TO NEXT SECTOR
 2B5E A575              LDA     SECTOR
 2B60 290F              AND     #%00001111      ; OVEFLOWED?
 2B62 D002              BNE     SECTOK          ; CONTINUE IF NOT
 2B64 E674              INC     TRACK           ; ELSE UPDATE TRACK #
 2B66 8575      SECTOK: STA     SECTOR          ; AND SECTOR #
                
 2B68 E673              INC     DBUFF+HI        ; POINT TO NEXT RAM PAGE
 2B6A 60        WRTERR: RTS
                
 2B6B                   ; *** ERROR #12: DISK ADDRESS OUT OF RANGE ***
                
 2B6B A90C      TRKERR: LDA     #12
 2B6D 4C9025            JMP     ZERROR
                
 2B70                   ; *** ERROR #14: DRIVE ACCESS ***
                
 2B70 A90E      DSKERR: LDA     #14


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 105    
--- Z-DOS: CBM PLUS/4 ---

 2B72 4C9025            JMP     ZERROR
                
 2B75                   ; -----------------------------
 2B75                   ; SET UP SAVE & RESTORE SCREENS
 2B75                   ; -----------------------------
                
 2B75 203D26    SAVRES: JSR     ZCRLF           ; CLEAR THE BUFFER
 2B78 20922A            JSR     CLS
 2B7B A200              LDX     #0
 2B7D 8662              STX     SCRIPT          ; DISABLE SCRIPTING
 2B7F A000              LDY     #0
 2B81 18                CLC
 2B82 4CF0FF            JMP     PLOT            ; HOME CURSOR & RETURN
                
 2B85                   ; -----------------
 2B85                   ; DISPLAY A DEFAULT
 2B85                   ; -----------------
                
 2B85                   ; ENTRY: DEFAULT (0-8) IN [A]
                
 2B85 20284465  DEFAL:  DB      " (Default is "
 2B92 2A293A    DEFNUM: DB      "*):"
 0010           DEFALL  EQU     $-DEFAL
                
 2B95 18        DODEF:  CLC
 2B96 6931              ADC     #'1'            ; CONVERT TO ASCII 1-9
 2B98 8D922B            STA     DEFNUM          ; INSERT IN STRING
                
 2B9B A285              LDX     #LOW DEFAL
 2B9D A92B              LDA     #HIGH DEFAL
 2B9F A010              LDY     #DEFALL
 2BA1 20A329            JSR     DLINE           ; PRINT THE STRING
                
 2BA4 A900              LDA     #0
 2BA6 85EF              STA     NDX             ; CLEAR KEY QUEUE
 2BA8 60                RTS
                
 2BA9                   ; -----------------------------
 2BA9                   ; GET SAVE & RESTORE PARAMETERS
 2BA9                   ; -----------------------------
                
 2BA9 0D        POSIT:  DB      EOL
 2BAA 506F7369          DB      "Position 1-5"
 000D           POSITL  EQU     $-POSIT
                
 2BB6 0D        WDRIV:  DB      EOL
 2BB7 44726976          DB      "Drive 8 or 9"
 000D           WDRIVL  EQU     $-WDRIV
                
 2BC3 0D        MIND:   DB      EOL
 2BC4 0D                DB      EOL


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 106    
--- Z-DOS: CBM PLUS/4 ---

 2BC5 506F7369          DB      "Position "
 2BCE 2A3B2044  MPOS:   DB      "*; Drive #"
 2BD8 2A2E      MDRI:   DB      "*."
 2BDA 0D                DB      EOL
 2BDB 41726520          DB      "Are you sure? (Y or N):"
 002F           MINDL   EQU     $-MIND
                
 2BF2 0D        INSM:   DB      EOL
 2BF3 496E7365          DB      "Insert SAVE disk into Drive #"
 2C10 2A2E      SAVDRI: DB      "*."
 0020           INSML   EQU     $-INSM
                
 2C12 594553    YES:    DB      'YES'
 2C15 0D                DB      EOL
 0004           YESL    EQU     $-YES
                
 2C16 4E4F      NO:     DB      'NO'
 2C18 0D                DB      EOL
 0003           NOL     EQU     $-NO
                
 2C19 A951      PARAMS: LDA     #WHITE
 2C1B 8D3B05            STA     COLOR           ; WHITE TEXT
                
 2C1E A2A9              LDX     #LOW POSIT
 2C20 A92B              LDA     #HIGH POSIT
 2C22 A00D              LDY     #POSITL
 2C24 20A329            JSR     DLINE           ; "POSITION (1-5)"
                
 2C27                   ; GET GAME POSITION
                
 2C27 A576      CHANGE: LDA     GPOSIT          ; SHOW THE CURRENT
 2C29 20952B            JSR     DODEF           ; DEFAULT POSITION
                
 2C2C 201428    GETPOS: JSR     GETKEY          ; WAIT FOR A KEY
 2C2F C90D              CMP     #EOL            ; IF [RETURN],
 2C31 F00D              BEQ     POSSET          ; USE DEFAULT
 2C33 38                SEC
 2C34 E931              SBC     #'1'            ; ELSE CONVERT ASCII TO BINARY
 2C36 C905              CMP     #5              ; IF BELOW "6"
 2C38 9008              BCC     SETPOS          ; MAKE IT THE NEW DEFAULT
 2C3A 20772A            JSR     BOOP            ; ELSE RAZZ
 2C3D 4C2C2C            JMP     GETPOS          ; AND TRY AGAIN
                
 2C40 A576      POSSET: LDA     GPOSIT          ; USE DEFAULT
                
 2C42 8578      SETPOS: STA     TPOSIT          ; USE KEYPRESS
 2C44 18                CLC
 2C45 6931              ADC     #'1'            ; CONVERT TO ASCII "1"-"5"
 2C47 8DCE2B            STA     MPOS            ; STORE IN TEMP STRING
 2C4A 8D982D            STA     SVPOS
 2C4D 8D332E            STA     RSPOS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 107    
--- Z-DOS: CBM PLUS/4 ---

 2C50 20CB28            JSR     LETTER          ; AND DISPLAY IT
                
 2C53                   ; GET DRIVE ID
                
 2C53 A2B6              LDX     #LOW WDRIV
 2C55 A92B              LDA     #HIGH WDRIV
 2C57 A00D              LDY     #WDRIVL
 2C59 20A329            JSR     DLINE           ; "DRIVE 8 OR 9"
                
 2C5C A577              LDA     GDRIVE          ; SHOW DEFAULT
 2C5E 18                CLC                     ; CONVERT 0 OR 1
 2C5F 6907              ADC     #7              ; TO 7 OR 8
 2C61 20952B            JSR     DODEF           ; SO DEFAULT WILL BE CORRECT
                
 2C64 201428    GETDRV: JSR     GETKEY          ; GET A KEYPRESS
 2C67 C90D              CMP     #EOL            ; IF [RETURN],
 2C69 F00D              BEQ     DRVSET          ; USE DEFAULT
 2C6B 38                SEC
 2C6C E938              SBC     #'8'            ; CONVERT TO BINARY 0 OR 1
 2C6E C902              CMP     #2              ; IF WITHIN RANGE,
 2C70 9008              BCC     SETDRV          ; SET NEW DEFAULT
 2C72 20772A            JSR     BOOP
 2C75 4C642C            JMP     GETDRV          ; ELSE TRY AGAIN
                
 2C78 A577      DRVSET: LDA     GDRIVE          ; USE DEFAULT
                
 2C7A 8579      SETDRV: STA     TDRIVE          ; USE [A]
 2C7C 18                CLC
 2C7D 6938              ADC     #'8'            ; CONVERT TO ASCII 8 OR 9
 2C7F 8D102C            STA     SAVDRI          ; STORE IN DRIVE STRING
 2C82 8DD82B            STA     MDRI            ; AND IN TEMP STRING
 2C85 20CB28            JSR     LETTER          ; AND SHOW NEW SETTING
                
 2C88 A2C3              LDX     #LOW MIND       ; SHOW TEMPORARY SETTINGS
 2C8A A92B              LDA     #HIGH MIND
 2C8C A02F              LDY     #MINDL
 2C8E 20A329            JSR     DLINE
                
 2C91 A900      GETYES: LDA     #0
 2C93 85EF              STA     NDX             ; CLEAR KEY QUEUE
 2C95 201428            JSR     GETKEY
 2C98 C959              CMP     #'Y'            ; IF REPLY IS "Y"
 2C9A F01E              BEQ     ALLSET          ; ACCEPT RESPONSES
 2C9C C979              CMP     #'y'
 2C9E F01A              BEQ     ALLSET
                
 2CA0 C94E              CMP     #'N'            ; IF REPLY IS N,
 2CA2 F00A              BEQ     RETRY           ; DO A RETRY
 2CA4 C96E              CMP     #'n'
 2CA6 F006              BEQ     RETRY
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 108    
--- Z-DOS: CBM PLUS/4 ---

 2CA8 20772A            JSR     BOOP            ; INSIST ON Y/RETURN
 2CAB 4C912C            JMP     GETYES          ; OR N
                
 2CAE A216      RETRY:  LDX     #LOW NO         ; ELSE PRINT "NO"
 2CB0 A92C              LDA     #HIGH NO
 2CB2 A003              LDY     #NOL
 2CB4 20A329            JSR     DLINE
 2CB7 4C192C            JMP     PARAMS          ; AND TRY AGAIN
                
 2CBA A212      ALLSET: LDX     #LOW YES        ; PRINT "YES"
 2CBC A92C              LDA     #HIGH YES
 2CBE A004              LDY     #YESL
 2CC0 20A329            JSR     DLINE
                
 2CC3 A579              LDA     TDRIVE          ; MAKE THE TEMPORARY DRIVE
 2CC5 8577              STA     GDRIVE          ; THE DEFAULT DRIVE
 2CC7 A578              LDA     TPOSIT          ; AND THE TEMP POSITION
 2CC9 8576              STA     GPOSIT          ; THE DEFAULT POSITION
                
 2CCB                   ; CALC TRACK & SECTOR OF GAME POSITION
                
 2CCB 0A                ASL     A               ; * 2
 2CCC 8574              STA     TRACK           ; SAVE HERE FOR A MOMENT
 2CCE 0A                ASL     A               ; * 4
 2CCF 18                CLC
 2CD0 6574              ADC     TRACK           ; * 6 (6 TRACKS PER POSITION)
 2CD2 8574              STA     TRACK
 2CD4 E674              INC     TRACK           ; 1ST TRACK IS 1!
 2CD6 A900              LDA     #0
 2CD8 8575              STA     SECTOR          ; ALWAYS START ON SECTOR #0
                
                ;       LDA     FAST            ; FAST-READ ENABLED?
                ;       BEQ     PRY             ; NO, CONTINUE
                ;       JSR     FOFF            ; ELSE DISENGAGE FAST-READ
                
 2CDA A577      PRY:    LDA     GDRIVE          ; TRY TO OPEN SPECIFIED DRIVE
 2CDC 18                CLC
 2CDD 6908              ADC     #8
 2CDF 20D72E            JSR     DOPEN           ; THE DEFAULT DRIVE
 2CE2 B00D              BCS     PARERR          ; CARRY SET IF OPEN FAILED
                
 2CE4 A2F2              LDX     #LOW INSM
 2CE6 A92B              LDA     #HIGH INSM
 2CE8 A020              LDY     #INSML
 2CEA 20A329            JSR     DLINE           ; "INSERT SAVE DISK IN DRIVE X."
 2CED 20F22C            JSR     RETURN          ; "PRESS [RETURN] TO CONTINUE."
 2CF0 18                CLC                     ; FOR SUCCESS
 2CF1 60        PARERR: RTS
                
 2CF2                   ; ---------------------
 2CF2                   ; "PRESS RETURN" PROMPT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 109    
--- Z-DOS: CBM PLUS/4 ---

 2CF2                   ; ---------------------
                
 2CF2 A20D      RETURN: LDX     #LOW RTN
 2CF4 A92D              LDA     #HIGH RTN
 2CF6 A01D              LDY     #RTNL
 2CF8 20A329            JSR     DLINE           ; SHOW PROMPT
                
 2CFB                   ; ENTRY FOR QUIT/RESTART
                
 2CFB A900      GETRET: LDA     #0
 2CFD 85EF              STA     NDX             ; CLEAR KEY QUEUE
 2CFF 201428            JSR     GETKEY          ; WAIT FOR [RETURN]
 2D02 C90D              CMP     #EOL
 2D04 F006              BEQ     RETEX
 2D06 20772A            JSR     BOOP            ; ACCEPT NO
 2D09 4CFB2C            JMP     GETRET          ; SUBSTITUTES!
                
 2D0C 60        RETEX:  RTS
                
 2D0D 0D        RTN:    DB      EOL
 2D0E 50726573          DB      "Press [RETURN] to continue."
 2D29 0D                DB      EOL
 001D           RTNL    EQU     $-RTN
                
 2D2A                   ; --------------------
 2D2A                   ; PROMPT FOR GAME DISK
 2D2A                   ; --------------------
                
 2D2A 0D        GAME:   DB      EOL
 2D2B 496E7365          DB      "Insert STORY disk into drive #8."
 0021           GAMEL   EQU     $-GAME
                
 2D4B A908      TOBOOT: LDA     #8
 2D4D 20D72E            JSR     DOPEN           ; CLOSE OLD, OPEN BOOT DRIVE
                
 2D50 A22A              LDX     #LOW GAME
 2D52 A92D              LDA     #HIGH GAME
 2D54 A021              LDY     #GAMEL
 2D56 20A329            JSR     DLINE           ; "INSERT STORY DISK IN DRIVE #8."
                
 2D59 20F22C            JSR     RETURN          ; "PRESS [RETURN] TO CONTINUE:"
                
                ;       LDA     FAST            ; FAST-READ ENABLED?
                ;       BEQ     TBT0            ; NO, SCRAM
                ;       JSR     FINIT           ; ELSE RE-INIT FAST CODE
                
 2D5C A9FF      TBT0:   LDA     #$FF            ; RE-ENABLE
 2D5E 8562              STA     SCRIPT          ; SCRIPTING
 2D60 4C922A            JMP     CLS             ; CLEAR SCREEN & RETURN
                
 2D63                   ; -------------------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 110    
--- Z-DOS: CBM PLUS/4 ---

 2D63                   ; SET UP PHONEY STATUS LINE
 2D63                   ; -------------------------
                
 2D63                   ; ENTRY: TEXT SET UP FOR "DLINE"
                
 2D63 20A329    SROOM:  JSR     DLINE
                
 2D66 A227              LDX     #39             ; INVERT & BLACKEN TOP LINE
 2D68 BD000C    SRLP:   LDA     SCREEN,X
 2D6B 0980              ORA     #%10000000
 2D6D 9D000C            STA     SCREEN,X
 2D70 A900              LDA     #0
 2D72 9D0008            STA     COLRAM,X
 2D75 CA                DEX
 2D76 10F0              BPL     SRLP
 2D78 60                RTS
                
 2D79                   ; ---------
 2D79                   ; SAVE GAME
 2D79                   ; ---------
                
 2D79 53617665  SAV:    DB      "Save Position"
 2D86 0D                DB      EOL
 000E           SAVL    EQU     $-SAV
                
 2D87 0D        SVING:  DB      EOL
 2D88 53617669          DB      "Saving position "
 2D98 2A202E2E  SVPOS:  DB      "* ..."
 2D9D 0D                DB      EOL
 0017           SVINGL  EQU     $-SVING
                
 2D9E 20752B    ZSAVE:  JSR     SAVRES          ; SET UP SCREEN
                
 2DA1 A279              LDX     #LOW SAV
 2DA3 A92D              LDA     #HIGH SAV
 2DA5 A00E              LDY     #SAVL
 2DA7 20632D            JSR     SROOM           ; "SAVE POSITION"
                
 2DAA 20192C            JSR     PARAMS          ; GET PARAMETERS
 2DAD 9006              BCC     DOSAVE          ; ERROR IF CARRY SET
                
 2DAF 204B2D    BADSAV: JSR     TOBOOT          ; GET BOOT DISK
 2DB2 4C0619            JMP     PREDF           ; PREDICATE FAILS
                
 2DB5 A287      DOSAVE: LDX     #LOW SVING
 2DB7 A92D              LDA     #HIGH SVING
 2DB9 A017              LDY     #SVINGL
 2DBB 20A329            JSR     DLINE           ; "SAVING POSITION X ..."
                
 2DBE                   ; SAVE GAME PARAMETERS IN [BUFSAV]
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 111    
--- Z-DOS: CBM PLUS/4 ---

 2DBE AD0232            LDA     ZBEGIN+ZID      ; MOVE GAME ID
 2DC1 8D9015            STA     BUFSAV+0        ; INTO 1ST 2 BYTES
 2DC4 AD0332            LDA     ZBEGIN+ZID+1    ; OF THE AUX LINE BUFFER
 2DC7 8D9115            STA     BUFSAV+1
                
 2DCA A517              LDA     ZSP             ; MOVE [ZSP]
 2DCC 8D9215            STA     BUFSAV+2        ; TO 3RD BYTE
 2DCF A518              LDA     OLDZSP          ; MOVE [OLDZSP]
 2DD1 8D9315            STA     BUFSAV+3        ; TO 4TH
                
 2DD4 A202              LDX     #2              ; MOVE CONTENTS OF [ZPC]
 2DD6 B519      ZPCSAV: LDA     ZPC,X           ; TO BYTES 5-7
 2DD8 9D9415            STA     BUFSAV+4,X      ; OF [BUFSAV]
 2DDB CA                DEX
 2DDC 10F8              BPL     ZPCSAV
                
 2DDE                   ; WRITE [LOCALS]/[BUFSAV] PAGE TO DISK
                
 2DDE A915              LDA     #HIGH LOCALS
 2DE0 8573              STA     DBUFF+HI        ; POINT TO THE PAGE
 2DE2 204C2B            JSR     PUTDSK          ; AND WRITE IT OUT
 2DE5 B0C8              BCS     BADSAV          ; CATCH WRITE ERROR HERE
                
 2DE7                   ; WRITE CONTENTS OF Z-STACK TO DISK
                
 2DE7 A911              LDA     #HIGH ZSTAKL    ; POINT TO 1ST PAGE
 2DE9 8573              STA     DBUFF+HI
 2DEB 204C2B            JSR     PUTDSK          ; WRITE 1ST AND
 2DEE B0BF              BCS     BADSAV
 2DF0 204C2B            JSR     PUTDSK          ; 2ND PAGE OF Z-STACK
 2DF3 B0BA              BCS     BADSAV
                
 2DF5                   ; WRITE ENTIRE GAME PRELOAD TO DISK
                
 2DF5 A526              LDA     ZCODE           ; POINT TO 1ST PAGE
 2DF7 8573              STA     DBUFF+HI        ; OF PRELOAD
                
 2DF9 AE0E32            LDX     ZBEGIN+ZPURBT   ; GET # IMPURE PAGES
 2DFC E8                INX                     ; USE FOR INDEXING
 2DFD 8611              STX     I+LO
                
 2DFF 204C2B    LSAVE:  JSR     PUTDSK
 2E02 B0AB              BCS     BADSAV
 2E04 C611              DEC     I+LO
 2E06 D0F7              BNE     LSAVE
                
 2E08 204B2D            JSR     TOBOOT          ; PROMPT FOR GAME DISK
 2E0B 4C1219            JMP     PREDS           ; ELSE PREDICATE SUCCEEDS
                
 2E0E                   ; ------------
 2E0E                   ; RESTORE GAME


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 112    
--- Z-DOS: CBM PLUS/4 ---

 2E0E                   ; ------------
                
 2E0E 52657374  RES:    DB      "Restore Position"
 2E1E 0D                DB      EOL
 0011           RESL    EQU     $-RES
                
 2E1F 0D        RSING:  DB      EOL
 2E20 52657374          DB      "Restoring position "
 2E33 2A202E2E  RSPOS:  DB      "* ..."
 2E38 0D                DB      EOL
 001A           RSINGL  EQU     $-RSING
                
 2E39 20752B    ZREST:  JSR     SAVRES
                
 2E3C A20E              LDX     #LOW RES
 2E3E A92E              LDA     #HIGH RES
 2E40 A011              LDY     #RESL
 2E42 20632D            JSR     SROOM           ; "RESTORE POSITION"
                
 2E45 20192C            JSR     PARAMS          ; GET PARAMETERS
 2E48 B036              BCS     BADRES          ; ERROR IF CARRY SET
                
 2E4A A21F              LDX     #LOW RSING
 2E4C A92E              LDA     #HIGH RSING
 2E4E A01A              LDY     #RSINGL
 2E50 20A329            JSR     DLINE           ; "RESTORING POSITION X ..."
                
 2E53                   ; SAVE LOCALS IN CASE OF ERROR
                
 2E53 A21F              LDX     #31
 2E55 BD0015    LOCSAV: LDA     LOCALS,X        ; COPY ALL LOCALS
 2E58 9D0001            STA     $0100,X         ; TO BOTTOM OF MACHINE STACK
 2E5B CA                DEX
 2E5C 10F7              BPL     LOCSAV
                
 2E5E A915              LDA     #HIGH LOCALS
 2E60 8573              STA     DBUFF+HI
 2E62 20332B            JSR     GETRES          ; RETRIEVE 1ST BLOCK OF PRELOAD
                
 2E65 AD9015            LDA     BUFSAV+0        ; DOES 1ST BYTE OF SAVED GAME ID
 2E68 CD0232            CMP     ZBEGIN+ZID      ; MATCH THE CURRENT ID?
 2E6B D008              BNE     WRONG           ; WRONG DISK IF NOT
                
 2E6D AD9115            LDA     BUFSAV+1        ; WHAT ABOUT THE 2ND BYTE?
 2E70 CD0332            CMP     ZBEGIN+ZID+1
 2E73 F011              BEQ     RIGHT           ; CONTINUE IF BOTH BYTES MATCH
                
 2E75                   ; HANDLE INCORRECT SAVE DISK
                
 2E75 A21F      WRONG:  LDX     #31             ; RESTORE ALL SAVED LOCALS
 2E77 BD0001    WR0:    LDA     $0100,X


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 113    
--- Z-DOS: CBM PLUS/4 ---

 2E7A 9D0015            STA     LOCALS,X
 2E7D CA                DEX
 2E7E 10F7              BPL     WR0
                
 2E80 204B2D    BADRES: JSR     TOBOOT          ; PROMPT FOR GAME DISK
 2E83 4C0619            JMP     PREDF           ; PREDICATE FAILS
                
 2E86                   ; CONTINUE RESTORE
                
 2E86 AD1032    RIGHT:  LDA     ZBEGIN+ZSCRIP   ; SAVE BOTH FLAG BYTES
 2E89 8511              STA     I+LO
 2E8B AD1132            LDA     ZBEGIN+ZSCRIP+1
 2E8E 8512              STA     I+HI
                
 2E90 A911              LDA     #HIGH ZSTAKL    ; RETRIEVE OLD CONTENTS OF
 2E92 8573              STA     DBUFF+HI        ; Z-STACK
 2E94 20332B            JSR     GETRES          ; GET 1ST BLOCK OF Z-STACK
 2E97 20332B            JSR     GETRES          ; AND 2ND BLOCK
                
 2E9A A526              LDA     ZCODE
 2E9C 8573              STA     DBUFF+HI
 2E9E 20332B            JSR     GETRES          ; GET 1ST BLOCK OF PRELOAD
                
 2EA1 A511              LDA     I+LO            ; RESTORE THE STATE
 2EA3 8D1032            STA     ZBEGIN+ZSCRIP   ; OF THE FLAG WORD
 2EA6 A512              LDA     I+HI
 2EA8 8D1132            STA     ZBEGIN+ZSCRIP+1
                
 2EAB AD0E32            LDA     ZBEGIN+ZPURBT   ; GET # PAGES TO LOAD
 2EAE 8511              STA     I+LO
                
 2EB0 20332B    LREST:  JSR     GETRES          ; FETCH THE REMAINDER
 2EB3 C611              DEC     I+LO            ; OF THE PRELOAD
 2EB5 D0F9              BNE     LREST
                
 2EB7                   ; RESTORE THE STATE OF THE SAVED GAME
                
 2EB7 AD9215            LDA     BUFSAV+2        ; RESTORE THE [ZSP]
 2EBA 8517              STA     ZSP
 2EBC AD9315            LDA     BUFSAV+3        ; AND THE [OLDZSP]
 2EBF 8518              STA     OLDZSP
                
 2EC1 A202              LDX     #2              ; RESTORE THE [ZPC]
 2EC3 BD9415    RESZPC: LDA     BUFSAV+4,X
 2EC6 9519              STA     ZPC,X
 2EC8 CA                DEX
 2EC9 10F8              BPL     RESZPC
                
 2ECB A900              LDA     #FALSE
 2ECD 851C              STA     ZPCFLG          ; INVALIDATE [ZPC]
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 114    
--- Z-DOS: CBM PLUS/4 ---

 2ECF 204B2D            JSR     TOBOOT          ; PROMPT FOR GAME DISK
 2ED2 4C1219            JMP     PREDS           ; PREDICATE SUCCEEDS
                
                        END
                        INCLUD DISK.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 115    
--- DISK ACCESS CODE: CBM PLUS/4 ---

                
 2ED5                   ; --------------
 2ED5                   ; OPEN DRIVE [A]
 2ED5                   ; --------------
                
 2ED5                   ; ENTRY: DISK ID (8 OR 9 BINARY) IN [A]
                
 2ED5 4930      I0:     DB      "I0"
 0002           I0L     EQU     $-I0
                
 2ED7 857A      DOPEN:  STA     DRIVE           ; SAVE DRIVE ID HERE
 2ED9 20082F            JSR     DCLOSE          ; CLOSE COMMAND & DATA CHANNELS
                
 2EDC A90F              LDA     #15             ; LOGICAL FILE #
 2EDE A8                TAY                     ; SECONDARY ADDRESS
 2EDF A67A              LDX     DRIVE           ; DEVICE # (8 OR 9)
 2EE1 20BAFF            JSR     SETLFS          ; SET UP LOGICAL FILE
                
 2EE4 A2D5              LDX     #LOW I0         ; POINT TO FILENAME
 2EE6 A02E              LDY     #HIGH I0        ; "I0:"
 2EE8 A902              LDA     #I0L            ; LENGTH OF FILENAME
 2EEA 20BDFF            JSR     SETNAM
                
 2EED 4CC0FF            JMP     OPEN            ; OPEN THE DISK (CARRY CLEAR IF OK)
                
 2EF0                   ; --------------------------
 2EF0                   ; OPEN DIRECT ACCESS CHANNEL
 2EF0                   ; --------------------------
                
 2EF0 23        POUND:  DB      "#"
 0001           POUNDL  EQU     $-POUND
                
 2EF1 200D2F    AOPEN:  JSR     ACLOSE
                
 2EF4 A902              LDA     #2              ; D/A CHANNEL ID
 2EF6 A8                TAY                     ; SECONDARY ID
 2EF7 A67A              LDX     DRIVE
 2EF9 20BAFF            JSR     SETLFS
                
 2EFC A2F0              LDX     #LOW POUND      ; POINT TO FILENAME
 2EFE A02E              LDY     #HIGH POUND     ; "#"
 2F00 A901              LDA     #POUNDL
 2F02 20BDFF            JSR     SETNAM
                
 2F05 4CC0FF            JMP     OPEN            ; OPEN CHANNEL (CARRY CLEAR IF OK)
                
 2F08                   ; -------------------
 2F08                   ; CLOSE CURRENT DRIVE
 2F08                   ; -------------------
                
 2F08 A90F      DCLOSE: LDA     #15             ; CLOSE COMMAND CHANNEL


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 116    
--- DISK ACCESS CODE: CBM PLUS/4 ---

 2F0A 20C3FF            JSR     CLOSE
                
 2F0D                   ; FALL THROUGH ...
                
 2F0D                   ; ---------------------
 2F0D                   ; CLOSE THE D/A CHANNEL
 2F0D                   ; ---------------------
                
 2F0D A902      ACLOSE: LDA     #2              ; AND THE
 2F0F 4CC3FF            JMP     CLOSE           ; DATA CHANNEL
                
 2F12                   ; ----------------
 2F12                   ; DIVIDE [A] BY 10
 2F12                   ; ----------------
                
 2F12                   ; EXIT: QUOTIENT IN [X], REMAINDER IN [A]
                
 2F12 A200      DIV10:  LDX     #0              ; START WITH ZERO QUOTIENT
                
 2F14 C90A      D10L:   CMP     #10             ; IF DIVISOR < 10,
 2F16 9005              BCC     D10EX           ; WE'RE DONE
 2F18 E90A              SBC     #10             ; ELSE SUBTRACT ANOTHER 10
 2F1A E8                INX                     ; UPDATE QUOTIENT
 2F1B D0F7              BNE     D10L            ; BRANCH ALWAYS
                
 2F1D 60        D10EX:  RTS
                
 2F1E                   ; ---------------
 2F1E                   ; SEND Ux COMMAND
 2F1E                   ; ---------------
                
 2F1E                   ; ENTRY: ASCII "1" OR "2" IN [A]
                
 2F1E 55        COMLIN: DB      "U"
 2F1F 2A        DCOMM:  DB      "*"
 2F20 3A322C30          DB      ":2,0,"
 2F25 2A2A2A2C  DTRAK:  DB      "***,"
 2F29 2A2A2A    DSECT:  DB      "***"
 2F2C 0D                DB      EOL
 000F           CMLL    EQU     $-COMLIN
                
 2F2D 8D1F2F    SENDU:  STA     DCOMM           ; INSERT COMMAND ("1" OR "2") IN STRING
                
 2F30                   ; CONVERT [TRACK] AND [SECTOR] TO ASCII IN [COMLIN]
                
 2F30 A574              LDA     TRACK
 2F32 A002              LDY     #2
 2F34 20122F    TCON:   JSR     DIV10           ; DIVIDE BY 10
 2F37 0930              ORA     #'0'            ; CONVERT TO ASCII
 2F39 99252F            STA     DTRAK,Y         ; STORE INTO STRING
 2F3C 8A                TXA                     ; GET QUOTIENT INTO [A]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 117    
--- DISK ACCESS CODE: CBM PLUS/4 ---

 2F3D 88                DEY                     ; ZERO-FILL USUSED BYTES
 2F3E 10F4              BPL     TCON
                
 2F40 A575              LDA     SECTOR          ; SAME FOR SECTOR ID
 2F42 A002              LDY     #2
 2F44 20122F    SCON:   JSR     DIV10
 2F47 0930              ORA     #'0'
 2F49 99292F            STA     DSECT,Y
 2F4C 8A                TXA
 2F4D 88                DEY
 2F4E 10F4              BPL     SCON
                
 2F50                   ; SEND COMMAND
                
 2F50 20CCFF            JSR     CLRCHN
 2F53 A20F              LDX     #15             ; OUTPUT TO THE
 2F55 20C9FF            JSR     CHKOUT          ; COMMAND CHANNEL
 2F58 B010              BCS     UEX             ; EXIT W/CARRY SET IF ERROR
                
 2F5A A000              LDY     #0
 2F5C B91E2F    SCM0:   LDA     COMLIN,Y        ; SEND THE COMMAND LINE
 2F5F 20D2FF            JSR     CHROUT          ; TO THE DRIVE CHANNEL
 2F62 B006              BCS     UEX
 2F64 C8                INY                     ; A BYTE AT A TIME
 2F65 C00F              CPY     #CMLL
 2F67 90F3              BCC     SCM0
 2F69 18                CLC                     ; NO ERRORS!
 2F6A 60        UEX:    RTS
                
 2F6B                   ; ----------------------
 2F6B                   ; SET THE BUFFER POINTER
 2F6B                   ; ----------------------
                
 2F6B 422D503A  BPLINE: DB      "B-P:2,0"
 2F72 0D                DB      EOL
 0008           BPLL    EQU     $-BPLINE
                
 2F73 20CCFF    SETBP:  JSR     CLRCHN
 2F76 A20F              LDX     #15             ; OUTPUT TO
 2F78 20C9FF            JSR     CHKOUT          ; COMMAND CHANNEL
 2F7B B010              BCS     BEX
                
 2F7D A000              LDY     #0
 2F7F B96B2F    SBPL:   LDA     BPLINE,Y
 2F82 20D2FF            JSR     CHROUT
 2F85 B006              BCS     BEX
 2F87 C8                INY
 2F88 C008              CPY     #BPLL
 2F8A 90F3              BCC     SBPL
 2F8C 18                CLC                     ; NO ERRORS!
 2F8D 60        BEX:    RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 118    
--- DISK ACCESS CODE: CBM PLUS/4 ---

                
 2F8E                   ; ------------------------------
 2F8E                   ; READ/WRITE A BLOCK TO [IOBUFF]
 2F8E                   ; ------------------------------
                
 2F8E                   ; ENTRY: [TRACK] = TRACK # (1-35)
 2F8E                   ;        [SECTOR] = SECTOR # (0-15)
 2F8E                   ;        [DRIVE] = DRIVE ID (8 OR 9)
 2F8E                   ;        CARRY CLEAR TO READ, CARRY SET TO WRITE
                
 2F8E B02A      DISK:   BCS     DWRITE          ; WRITE IF CARRY SET
                
 2F90                   ; READ A DISK BLOCK
                
                ;       LDA     FASTEN          ; FAST-READ AVAILABLE?
                ;       BEQ     SLOW            ; USE SLOW CODE IF NOT
                ;       LDA     FAST            ; FAST-READ ENGAGED?
                ;       BEQ     SLOW            ; NO, USE SLOW
                
                ;       JMP     DOFAST          ; ELSE USE FAST-READ ROUTINES
                
 2F90 20F12E    SLOW:   JSR     AOPEN           ; OPEN THE ACCESS CHANNEL
 2F93 B059              BCS     BADISK          ; CARRY SET IF ERROR
                
 2F95 A931              LDA     #'1'            ; SEND A "U1" COMMAND
 2F97 202D2F            JSR     SENDU
 2F9A B052              BCS     BADISK
                
 2F9C 20732F            JSR     SETBP           ; SET THE BUFFER POINTER
 2F9F B04D              BCS     BADISK
                
 2FA1 20CCFF            JSR     CLRCHN
 2FA4 A202              LDX     #2              ; INPUT FROM
 2FA6 20C6FF            JSR     CHKIN           ; DATA CHANNEL
 2FA9 B043              BCS     BADISK
                
 2FAB A000              LDY     #0
 2FAD 20CFFF    READ1:  JSR     CHRIN           ; GET A BYTE
 2FB0 B03C              BCS     BADISK
 2FB2 990010            STA     IOBUFF,Y        ; MOVE TO I/O BUFFER
 2FB5 C8                INY
 2FB6 D0F5              BNE     READ1           ; DO 256 BYTES
                
 2FB8 F028              BEQ     SHUTD           ; THEN EXIT
                
 2FBA                   ; WRITE A BLOCK
                
 2FBA 20F12E    DWRITE: JSR     AOPEN           ; OPEN THE ACCESS CHANNEL
 2FBD B02F              BCS     BADISK          ; CARRY SET IF ERROR
                
 2FBF 20732F            JSR     SETBP           ; SET THE BUFFER POINTER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT I/O                                                            PAGE 119    
--- DISK ACCESS CODE: CBM PLUS/4 ---

 2FC2 B02A              BCS     BADISK
                
 2FC4 20CCFF            JSR     CLRCHN
 2FC7 A202              LDX     #2              ; OUTPUT TO
 2FC9 20C9FF            JSR     CHKOUT          ; DATA CHANNEL
 2FCC B020              BCS     BADISK
                
 2FCE A000              LDY     #0
 2FD0 B90010    WRITE1: LDA     IOBUFF,Y        ; SEND CONTENTS OF [IOBUFF]
 2FD3 20D2FF            JSR     CHROUT          ; TO THE DRIVE
 2FD6 B016              BCS     BADISK
 2FD8 C8                INY
 2FD9 D0F5              BNE     WRITE1          ; WRITE 256 BYTES
                
 2FDB A932              LDA     #'2'            ; ISSUE A "U2" COMMAND
 2FDD 202D2F            JSR     SENDU
 2FE0 B00C              BCS     BADISK          ; EXIT W/CARRY SET IF ERROR
                
 2FE2 20B7FF    SHUTD:  JSR     READST          ; READ STATUS BYTE
 2FE5 2983              AND     #%10000011      ; MASK UNINTERESTING BITS
 2FE7 D005              BNE     BADISK          ; ERROR IF ANY BIT SET
                
 2FE9 20CCFF            JSR     CLRCHN          ; RESET CHANNELS
 2FEC 18                CLC                     ; CLEAR CARRY FOR SUCCESS
 2FED 60                RTS
                
 2FEE 20CCFF    BADISK: JSR     CLRCHN
 2FF1 38                SEC                     ; SET CARRY FOR FAILURE
 2FF2 60                RTS
                
                        END
                
 2FF3                   IF      DEBUG
 2FF3                   INCLUD BUGGER.ASM
 2FF3                   ENDIF
                
 0000                   END


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 120    
---- SYMBOL TABLE ----

A12VAL   1999          CHRIN    FFCF          DEFALL   0010          EARLY    2293          GETKEY   2814
A2VAL    1C27          CHROUT   FFD2          DEFNUM   2B92          ECON     2592          GETLNG   1852
ABQUOT   1E4D          CHRTBL   24CA          DGC      1FD8          EFIND    20F9          GETP1    1D2C
ABREM    1E3F          CLALL    FFE7          DIGCNT   1FD4          ENDLIN   2980          GETP2    1D3B
ABSDIV   1E24          CLOCK    26BC          DIGITS   005E          ENTRY    0047          GETP3    1D4E
ABYTE    000D          CLOCKL   0006          DIRECT   231B          ENUMB    258D          GETPB    1D5E
ACLOSE   2F0D          CLOSE    FFC3          DISK     2F8E          EOL      000D          GETPOS   2C2C
ADEX     000E          CLRCHN   FFCC          DISKE    228E          EOS      25C0          GETPT1   1D74
ALLSET   2CBA          CLS      2A92          DIV10    2F12          EOSL     000E          GETPT2   1D83
AOPEN    2EF1          CMLL     000F          DIVERR   1E8C          EQBAD    1ECB          GETPT3   1D89
ARG1     0005          CNL      247D          DIVEX    1E4C          EQOK     1EC8          GETPW    1D64
ARG2     0007          CNOK     2486          DIVIDE   1E0F          ERRM     257E          GETRES   2B33
ARG3     0009          COLD     1600          DLINE    29A3          ERRML    0012          GETRET   2CFB
ARG4     000B          COLD0    1605          DLS0     1C06          ESIZE    004B          GETSET   23A1
BACKSP   0014          COLD1    161A          DOB2     1933          ETPEX    1D6A          GETSHT   184E
BACKUP   2976          COLD2    162E          DOCALL   1ED7          FADDR    0022          GETV     1855
BADISK   2FEE          COLOR    053B          DODEF    2B95          FALSE    0000          GETVAR   1866
BADKEY   2898          COLRAM   0800          DODIS    17B1          FAST     0002          GETVR1   186B
BADOP1   1801          COLUMN   00D9          DOEQ     1EA4          FASTEN   00E2          GETVRG   187F
BADOP2   183E          COMLIN   2F1E          DOFREQ   234B          FBRK     2144          GETVRL   186F
BADRES   2E80          COMPAR   1C2F          DOGET    1D02          FDATA    00E1          GETWRD   22BC
BADSAV   2DAF          CONCNT   0053          DOM0     27AE          FINDEX   00E0          GETYES   2C91
BADVER   1ABE          CONIN    0054          DOMINS   27A3          FINDW    2146          GETZ1    23C8
BCALC    1D15          CONOUT   0055          DOPEN    2ED7          FIRST1   1ADA          GETZ2    23E3
BEX      2F8D          CONTOP   2407          DOPM     27C5          FKEY     0076          GETZ3    23EF
BGCOL0   FF15          CONZST   23F5          DORET    1D9B          FL0      2615          GETZCH   23AD
BGCOL1   FF16          COUT     25FF          DOSAVE   2DB5          FL1      261F          GKEY0    2818
BGCOL2   FF17          CR1      2686          DOSCRL   2901          FL2      262A          GKEY1    283F
BGCOL3   FF18          CROW     00DC          DOSCRN   2A6F          FL3      2634          GLOBAL   002F
BITMAP   FF12          CSET0    243C          DOSIB    20BB          FLAGSU   2540          GO       17C3
BLANK    232D          CSET2    244E          DOTIME   2754          FLEX     211C          GODIV    1E31
BLINK    00DE          CSHAPE   00D8          DOUT     29AB          FLS0     2559          GPOSIT   0076
BLINKA   053C          CSHIP    2441          DOXM     27C7          FLS1     255A          GS       23A8
BLINRT   FF1F          CTABLE   247B          DOXOP    17A2          FLS2     2572          GTBT0    21F8
BMRLSB   FF1B          CTEST    242C          DRIVE    007A          FLSL     256B          GTBT1    21FF
BMRMSB   FF1A          CURSH    FF0C          DRVSET   2C78          FLUSH    2613          GTBT2    2208
BOOP     2A77          CURSL    FF0D          DSECT    2F29          FLUSHW   2107          GTBT3    221A
BOOPL    2A8A          CURSOR   0064          DSKERR   2B70          FREEZE   25BD          GTEXIT   23F1
BORDER   FF19          CYCLE    00E0          DSOR     007D          FUNCT    FF49          GTZ0     23B3
BPLINE   2F6B          CZSL     23F8          DT0      2778          FWL1     216C          GVCALC   18F0
BPLL     0008          D10EX    2F1D          DT1      277E          FWORDS   0033          HI       0001
BREAK    211D          D10L     2F14          DTEMP    007F          FWSUCC   21A8          HILINE   2AC0
BRKTBL   212E          DBLOCK   0070          DTRAK    2F25          GAME     2D2A          HSCAN    FF1E
BUFSAV   1590          DBUFF    0072          DVD      007B          GAMEL    0021          HSCROL   FF07
CEX      2612          DC0      174B          DVLP     2AF6          GDEX     2B49          I        0011
CHANGE   2C27          DC1      1752          DVLP1    2B0E          GDKL     2B3B          I0       2ED5
CHAR     28E1          DCLOSE   2F08          DVLP2    2B31          GDRIVE   0077          I0L      0002
CHBASE   FF13          DCOMM    2F1F          DVX      1991          GETBYT   21E6          IMASK    FF0A
CHKEOL   28F3          DEBUG    0000          DWRITE   2FBA          GETDRV   2C64          IN       0037
CHKIN    FFC6          DECVAL   1986          EAR0     229B          GETDSK   2AD9          INCVAL   1992
CHKOUT   FFC9          DEFAL    2B85          EAR1     22A5          GETIN    FFE4          INLOOP   294F


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 121    
---- SYMBOL TABLE ----

INPUT    2946          MPCL     001F          OP2A     1810          PP1      29E1          RASTER   FF0B
INSM     2BF2          MPCM     0020          OP2B     1813          PP2      29EA          READ1    2FAD
INSML    0020          MPCPNT   0023          OP2C     1822          PP3      29F9          READL    2059
INTREG   FF09          MPOS     2BCE          OP2D     1825          PP4      29FF          READL2   20A1
IOBUFF   1000          MSTART   1000          OP2EX    182F          PP5      2A03          READL3   20C3
IOCHAR   0067          MTEMP    005A          OPCODE   0003          PPERR    29DB          READST   FFB7
IVX      1998          MWAIT    2667          OPEN     FFC0          PPRINT   29B6          REMAIN   0058
J        0013          NARGS    0004          OPEXT    1759          PPX      253A          REMVC1   1B5C
K        0015          NBL      2124          OPT0     19A2          PREDB    1917          REMVC2   1B6D
KEYLAT   FF08          NBRKS    0006          OPT1     19BE          PREDB1   193A          REMVEX   1B76
LBUFF    1530          NDX      00EF          OPT2     19DE          PREDB2   1945          RES      2E0E
LDPRE    1723          NENTS    0049          OPTX     1A10          PREDB3   194C          RESL     0011
LENGTH   0060          NEWSET   2331          OPX0     1764          PREDB5   196A          RESULT   0044
LET0     28D4          NEXTPC   21B0          OPX1     176A          PREDF    1906          RESZPC   2EC3
LETEX    28DE          NEXTZ    2415          OPX2     1774          PREDLB   1927          RET0     18C2
LETTER   28CB          NO       2C16          OPX3     177E          PREDNB   190B          RET1     1BA5
LEX1     298B          NOBLIN   2861          OPX4     1799          PREDS    1912          RET2     1BB9
LEX2     2998          NOL      0003          OPXNXT   1785          PRIL     1A4C          RETEX    2D0C
LFROM    006C          NOMORE   2965          OUT      003D          PRLEN    006B          RETRY    2CAE
LINCNT   0065          NOPS0    000E          OUTEOL   2940          PRNTDC   1B79          RETURN   2CF2
LINEX    26A4          NOPS1    0010          OVER     18B4          PRNTN3   1FF0          RFLIP    1E3B
LINKEN   07E9          NOPS2    0019          PAGE     221C          PRNTN4   1FF9          RIGHT    2E86
LINLEN   0045          NOPSX    000C          PAGE0    0028          PROPB    250D          RL1      2069
LINOUT   268F          NORAM    16D6          PARAMS   2C19          PROPL    252E          RL2      2070
LMAX     0066          NORM     2122          PARERR   2CF1          PROPN    2529          RL3      2079
LO       0000          NOSCRL   2936          PATCH    21F1          PROPNX   2536          RLEX     206F
LOCALS   1500          NPC0     21C2          PBAD     1C59          PRY      2CDA          RLL      207F
LOCSAV   2E55          NPC1     21C9          PCALC    1F77          PSET     004C          ROMIN    FF3E
LOLINE   2AA7          NPC2     21D2          PDC0     1B8E          PSHVAL   18A2          ROMOUT   FF3F
LOUT     2697          NPC3     21E4          PEX      2A08          PSTAT    006A          RSIGN    005D
LREST    2EB0          NUMBER   1FC8          PFINE    1AE3          PTABH    13A0          RSING    2E1F
LRU      0025          NXTP1    1DA5          PG1      2224          PTABL    1300          RSINGL   001A
LRUMAP   1450          NXTP2    1DB4          PG2      2231          PTKL     2B4E          RSPOS    2E33
LSAVE    2DFF          NXTP3    1DB7          PG3      2238          PTZ0     1B04          RTN      2D0D
LTO      006E          NXTSEC   2B5C          PG4      225A          PUSHXA   18A6          RTNL     001D
MASK     286E          OBJ1     24FA          PG5      226C          PUTBYT   18C4          SAV      2D79
MASK0    2884          OBJ2     2501          PG6      2277          PUTDSK   2B4C          SAVDRI   2C10
MASK1    288C          OBJLOC   24E4          PG7      2283          PUTLSB   1F6D          SAVL     000E
MCLR     26AF          OBJTAB   0035          PG8      2288          PUTP1    1F8D          SAVRES   2B75
MDRI     2BD8          OFFSET   004F          PGOOD    1C6C          PUTP2    1F9C          SAY1     2496
MEMTOP   25F5          OFFSPL   2A3E          PLERR    1FB6          PUTP3    1FAC          SAY2     24A1
MIND     2BC3          OLDLEN   0061          PLOT     FFF0          PUTVAL   18CA          SAYSET   248B
MINDL    002F          OLDX     0063          PMAX     0029          PUTVLG   18E3          SBASE    FF14
MINIT    1E91          OLDY     0064          PNERR    1FB1          PUTVLL   18D3          SBL      213B
MLOOP    173C          OLDZSP   0018          POPVAL   188C          PUTVR1   18CF          SBPL     2F7F
MORE     26A9          OP0      17C8          POSIT    2BA9          PYUCK    1AC7          SCEX     1C4B
MOREL    0006          OP1      17D7          POSITL   000D          PZSTR    22D9          SCM0     2F5C
MOVMIN   2789          OP1A     17E1          POSSET   2C40          PZTOP    22E2          SCMP     1C41
MPC      001F          OP1B     17EB          POUND    2EF0          QSIGN    005C          SCOMP    1C36
MPCFLG   0022          OP1EX    17F2          POUNDL   0001          QUOT     0056          SCON     2F44
MPCH     0021          OP2      1806          PP0      29C7          RANDOM   25F8          SCORE    26B5


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 122    
---- SYMBOL TABLE ----

SCOREL   0007          STRING   29AC          VERSL    0015          ZENDLD   0004          ZPRC     1FBB
SCREEN   0C00          SVING    2D87          VEXIT    1BF6          ZEQUAL   1E9B          ZPRD     1B77
SCRIPT   0062          SVINGL   0017          VLINEH   FF1C          ZEROPG   0003          ZPRI     1A37
SECTOK   2B66          SVPOS    2D98          VLINEL   FF1D          ZERROR   2590          ZPRINT   1BD7
SECTOR   0075          SWAP     002E          VOCAB    0031          ZFCLR    1CAE          ZPRN     1FC0
SENDU    2F2D          T1LSB    FF00          VOLUME   FF11          ZFIRST   1AD3          ZPRR     1A54
SET1     2305          T1MSB    FF01          VSCROL   FF06          ZFLAG    0050          ZPTSIZ   1AF2
SET2     230D          T2LSB    FF02          VSUM     1A8A          ZFSET    1C9B          ZPURBT   000E
SETA0    16E1          T2MSB    FF03          VSUM0    1A96          ZFSETP   1C87          ZPURE    0027
SETBP    2F73          T3LSB    FF04          WARM1    1671          ZFWORD   0018          ZPUSH    2027
SETDRV   2C7A          T3MSB    FF05          WARM2    1691          ZGET     1CFC          ZPUT     1F61
SETLFS   FFBA          TARGET   002B          WARMEX   172F          ZGETB    1D0C          ZPUTB    1F72
SETMSG   FF90          TBT0     2D5C          WCALC    1D11          ZGETP    1D29          ZPUTP    1F8A
SETNAM   FFBD          TCON     2F34          WCEX     1905          ZGETPT   1D71          ZQUIT    25B1
SETNP    16DB          TDRIVE   0079          WDRIV    2BB6          ZGLOBA   000C          ZRAND    2005
SETPOS   2C42          TED      FF00          WDRIVL   000D          ZGO      0006          ZREAD    2036
SETSTR   22C7          TICK     289E          WHITE    0051          ZGRTR    1C11          ZREMOV   1B35
SETWRD   22AD          TICK0    28BB          WNEXT    2189          ZID      0002          ZREST    2E39
SHAP0    285B          TIME     00A5          WNX      2194          ZIGRTR   1C1C          ZRET     1B94
SHAP1    285D          TIMEFL   005F          WNX1     219F          ZIN      1C4C          ZRFALS   1A33
SHFLAG   0543          TOASC    22FC          WR0      2E77          ZINC     1B0F          ZRSTAK   1A5D
SHOVE    22FF          TOBOOT   2D4B          WRDLEN   0046          ZIP      1600          ZRT0     1A2A
SHOWIT   295E          TOBOT    2A6A          WRITE1   2FD0          ZJUMP    1BD1          ZRT1     1A2C
SHUTD    2FE2          TOPERM   233F          WRONG    2E75          ZLENTH   001A          ZRTRUE   1A28
SIB      2134          TOTOP    2A62          WRTERR   2B6A          ZLESS    1BFD          ZSAVE    2D9E
SLINE    0068          TPOSIT   0078          XSIZE    0027          ZLOC     1AE6          ZSCRIP   0010
SLOAD    1658          TRACK    0074          YES      2C12          ZMLOOP   1DDA          ZSCRN    2A54
SLOADL   0019          TRKERR   2B6B          YESL     0004          ZMNEXT   1DF1          ZSERIA   0012
SLOW     2F90          TRUE     00FF          YSIZE    0018          ZMOD     1E05          ZSET     1CC5
SOURCE   0043          TRY2     1EB0          ZADD     1DBD          ZMODE    0001          ZSP      0017
SPACE    0020          TRY3     1EBC          ZBAND    1C7B          ZMOVE    1CD2          ZSPLIT   2A0B
SPCALC   2A34          TSET     004D          ZBCOM    1BED          ZMUL     1DD7          ZSTAKH   1200
SPL0     2A1C          UDIV     1E5B          ZBEGIN   3200          ZMVEX    1CFB          ZSTAKL   1100
SPL1     2A2A          UDLOOP   1E64          ZBOR     1C6F          ZNEXT    1ACA          ZSTART   25CE
SPLEX    2A3D          UDNEXT   1E7C          ZBTST    1C5C          ZNEXTP   1D9E          ZSTEX    22D8
SPLOFF   2A41          UEX      2F6A          ZCALL    1ECE          ZNOOP    1985          ZSUB     1DCA
SPSTAT   0069          UNDER    189D          ZCALL1   1F05          ZOBJEC   000A          ZUSL     26C2
SRL0     2908          USL0     26EA          ZCALL2   1F2C          ZPAGE    002A          ZVALUE   1BE5
SRL1     2923          USLX     27DD          ZCALL3   1F56          ZPC      0019          ZVER     1A63
SRL2     292C          V1FLSB   FF0E          ZCHAR    004E          ZPCFLG   001C          ZVERS    0000
SRL3     2930          V2A1     1843          ZCHKSM   001C          ZPCH     001B          ZVOCAB   0008
SRLP     2D68          V2FLSB   FF0F          ZCODE    0026          ZPCL     0019          ZVR      1A6A
SROOM    2D63          V2FMSB   FF10          ZCRLF    263D          ZPCM     001A          ZWORD    0051
SROW     00DA          VALUE    000F          ZCRUSH   24A4          ZPCPNT   001D          ZZERO    1AC1
ST0      1695          VARGET   185D          ZD0      1B22          ZPCSAV   2DD6          
ST1      169E          VARPUT   18B9          ZDEC     1B1A          ZPGTOP   007F          
STAMP    002D          VERNUM   25E9          ZDIV     1DFB          ZPOP     202E          
STATEX   27CF          VERS     25D4          ZDLESS   1C03          ZPRB     1B27          
                
***** NO ERRORS DETECTED *****
