

AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   1    


                
 0000                   ; --------------------------
 0000                   ; ZIP/6502 2.0
 0000                   ; Z-CODE INTERPRETER PROGRAM
 0000                   ; FOR ATARI 400/800/1200/XL
 0000                   ; --------------------------
                
 0000                   ; INFOCOM, INC.
 0000                   ; 55 WHEELER STREET
 0000                   ; CAMBRIDGE, MA 02136
                
 0000                   ; COMPANY PRIVATE -- NOT FOR DISTRIBUTION
                
 0080           ZEROPG  EQU     $80             ; 1ST FREE Z-PAGE LOCATION
                
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
                
                        INCLUD  EQ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   2    
--- MEMORY ORGANIZATION ---

                
 00FF           TRUE    EQU     $FF
 0000           FALSE   EQU     0
 0000           LO      EQU     0
 0001           HI      EQU     1
                
 0000                   ; SEE "HARDEQ.ASM" FOR ATARI MEMORY MAP
                
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

                
 0080           OPCODE  EQU     ZEROPG          ; (BYTE) CURRENT OPCODE
 0081           NARGS   EQU     OPCODE+1        ; (BYTE) # ARGUMENTS
 0082           ARG1    EQU     OPCODE+2        ; (WORD) ARGUMENT #1
 0084           ARG2    EQU     OPCODE+4        ; (WORD) ARGUMENT #2
 0086           ARG3    EQU     OPCODE+6        ; (WORD) ARGUMENT #3
 0088           ARG4    EQU     OPCODE+8        ; (WORD) ARGUMENT #4
 008A           ABYTE   EQU     OPCODE+10       ; (BYTE) X-OP ARGUMENT BYTE
 008B           ADEX    EQU     OPCODE+11       ; (BYTE) X-OP ARGUMENT INDEX
                
 008C           VALUE   EQU     OPCODE+12       ; (WORD) VALUE RETURN REGISTER
 008E           I       EQU     VALUE+2         ; (WORD) GEN-PURPOSE REGISTER #1
 0090           J       EQU     VALUE+4         ; (WORD) GEN-PURPOSE REGISTER #2
 0092           K       EQU     VALUE+6         ; (WORD) GEN-PURPOSE REGISTER #3
                
 0094           ZSP     EQU     VALUE+8         ; (BYTE) Z-STACK POINTER
 0095           OLDZSP  EQU     ZSP+1           ; (BYTE) OLD Z-STACK POINTER
                
 0096           ZPC     EQU     ZSP+2           ; (3 BYTES) ZIP PROGRAM COUNTER
 0096           ZPCL    EQU     ZPC             ; (BYTE) LOW 8 BITS OF [ZPC]
 0097           ZPCM    EQU     ZPC+1           ; (BYTE) MIDDLE 8 BITS OF [ZPC]
 0098           ZPCH    EQU     ZPC+2           ; (BYTE) HIGH BIT OF [ZPC]
 0099           ZPCFLG  EQU     ZPC+3           ; (BYTE) FLAG: "TRUE" IF [ZPCPNT] VALID
 009A           ZPCPNT  EQU     ZPC+4           ; (WORD) ABS POINTER TO CURRENT Z-PAGE
                
 009C           MPC     EQU     ZPC+6           ; (3 BYTES) MEMORY PROGRAM COUNTER
 009C           MPCL    EQU     MPC             ; (BYTE) LOW 8 BITS OF [MPC]
 009D           MPCM    EQU     MPC+1           ; (BYTE) MIDDLE 8 BITS OF [MPC]
 009E           MPCH    EQU     MPC+2           ; (BYTE) HIGH BIT OF [MPC]
 009F           MPCFLG  EQU     MPC+3           ; (BYTE) FLAG: "TRUE" IF [MPCPNT] VALID
 00A0           MPCPNT  EQU     MPC+4           ; (WORD) ABS POINTER TO CURRENT M-PAGE
                
 00A2           LRU     EQU     MPC+6           ; (BYTE) EARLIEST TIMESTAMP
 00A3           ZCODE   EQU     LRU+1           ; (BYTE) 1ST ABSOLUTE PAGE OF PRELOAD
 00A4           ZPURE   EQU     LRU+2           ; (BYTE) 1ST VIRTUAL PAGE OF "PURE" Z-CODE
 00A5           PAGE0   EQU     LRU+3           ; (BYTE) 1ST PAGE OF ACTUAL SWAPPING SPACE
 00A6           PMAX    EQU     LRU+4           ; (BYTE) MAXIMUM # OF SWAPPING PAGES
 00A7           ZPAGE   EQU     LRU+5           ; (BYTE) CURRENT SWAPPING PAGE
 00A8           TARGET  EQU     LRU+6           ; (WORD) TARGET PAGE FOR SWAPPING
 00AA           STAMP   EQU     LRU+8           ; (BYTE) CURRENT TIMESTAMP
 00AB           SWAP    EQU     LRU+9           ; (BYTE) EARLIEST BUFFER
                
 00AC           GLOBAL  EQU     LRU+10          ; (WORD) GLOBAL VARIABLE POINTER
 00AE           VOCAB   EQU     GLOBAL+2        ; (WORD) VOCAB TABLE POINTER
 00B0           FWORDS  EQU     GLOBAL+4        ; (WORD) F-WORDS TABLE POINTER
 00B2           OBJTAB  EQU     GLOBAL+6        ; (WORD) OBJECT TABLE POINTER
                
 0000                   ; Z-STRING MANIPULATION VARIABLES
                
 00B4           IN      EQU     GLOBAL+8        ; (6 BYTES) INPUT BUFFER
 00BA           OUT     EQU     IN+6            ; (6 BYTES) OUTPUT BUFFER


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   4    
--- ZIP Z-PAGE VARIABLES ---

                
 00C0           SOURCE  EQU     OUT+6           ; (BYTE) SOURCE BUFFER POINTER
 00C1           RESULT  EQU     SOURCE+1        ; (BYTE) RESULT TABLE POINTER
 00C2           LINLEN  EQU     SOURCE+2        ; (BYTE) LENGTH OF CURRENT LINE
 00C3           WRDLEN  EQU     SOURCE+3        ; (BYTE) LENGTH OF CURRENT WORD
 00C4           ENTRY   EQU     SOURCE+4        ; (WORD) ADDR OF CURRENT RESULT ENTRY
 00C6           NENTS   EQU     SOURCE+6        ; (WORD) # ENTRIES IN VOCAB TABLE
 00C8           ESIZE   EQU     SOURCE+8        ; (BYTE) SIZE OF VOCAB TABLE ENTRIES
 00C9           PSET    EQU     SOURCE+9        ; (BYTE) PERMANENT CHARSET
 00CA           TSET    EQU     SOURCE+10       ; (BYTE) TEMPORARY CHARSET
 00CB           ZCHAR   EQU     SOURCE+11       ; (BYTE) CURRENT Z-CHAR
 00CC           OFFSET  EQU     SOURCE+12       ; (BYTE) F-WORD TABLE OFFSET
 00CD           ZFLAG   EQU     SOURCE+13       ; (BYTE) Z-WORD ACCESS FLAG
 00CE           ZWORD   EQU     SOURCE+14       ; (WORD) CURRENT Z-WORD
 00D0           CONCNT  EQU     SOURCE+16       ; (BYTE) Z-STRING SOURCE COUNTER
 00D1           CONIN   EQU     SOURCE+17       ; (BYTE) CONVERSION SOURCE INDEX
 00D2           CONOUT  EQU     SOURCE+18       ; (BYTE) CONVERSION DEST INDEX
                
 00D3           QUOT    EQU     SOURCE+19       ; (WORD) QUOTIENT FOR DIVISION
 00D5           REMAIN  EQU     QUOT+2          ; (WORD) REMAINDER FOR DIVISION
 00D7           MTEMP   EQU     QUOT+4          ; (WORD) MATH TEMPORARY REGISTER
 00D9           QSIGN   EQU     QUOT+6          ; (BYTE) SIGN OF QUOTIENT
 00DA           RSIGN   EQU     QUOT+7          ; (BYTE) SIGN OF REMAINDER
 00DB           DIGITS  EQU     QUOT+8          ; (BYTE) DIGIT COUNT FOR "PRINTN"
                
 00DC           TIMEFL  EQU     QUOT+9          ; (BYTE) "TRUE" IF TIME MODE
 00DD           LENGTH  EQU     TIMEFL+1        ; (BYTE) LENGTH OF LINE IN [LINBUF]
 00DE           OLDLEN  EQU     TIMEFL+2        ; (BYTE) OLD LINE LENGTH
 00DF           SCRIPT  EQU     TIMEFL+3        ; (BYTE) SCRIPT ENABLE FLAG
 00E0           LINCNT  EQU     TIMEFL+4        ; (BYTE) LINE COUNTER
 00E1           LMAX    EQU     TIMEFL+5        ; (BYTE) MAX # LINES/SCREEN
                
 00E2           IOCHAR  EQU     TIMEFL+6        ; (BYTE) CHARACTER BUFFER
 00E3           SLINE   EQU     IOCHAR+1        ; (BYTE) BORDERLINE FOR SPLIT
 00E4           SPSTAT  EQU     IOCHAR+2        ; (BYTE) SPLIT SCREEN STATUS FLAG
 00E5           LFROM   EQU     IOCHAR+3        ; (WORD) "FROM" LINE ADDRESS
 00E7           LTO     EQU     IOCHAR+5        ; (WORD) "TO" LINE ADDRESS
 00E9           PSTAT   EQU     IOCHAR+7        ; (BYTE) PRINTER STATUS FLAG
 00EA           PRLEN   EQU     IOCHAR+8        ; (BYTE) SCRIPT LINE LENGTH
                
 00EB           DBLOCK  EQU     IOCHAR+9        ; (WORD) Z-BLOCK TO READ
 00ED           DBUFF   EQU     DBLOCK+2        ; (WORD) RAM PAGE TO ACCESS (LSB = 0)
 00EF           SECTOR  EQU     DBLOCK+4        ; (WORD) TARGET SECTOR
 00F1           GPOSIT  EQU     DBLOCK+6        ; (BYTE) DEFAULT SAVE POSITION
 00F2           GDRIVE  EQU     DBLOCK+7        ; (BYTE) DEFAULT SAVE DRIVE
 00F3           TPOSIT  EQU     DBLOCK+8        ; (BYTE) TEMP SAVE POSITION
 00F4           TDRIVE  EQU     DBLOCK+9        ; (BYTE) TEMP SAVE DRIVE
 00F5           DRIVE   EQU     DBLOCK+10       ; (BYTE) CURRENT DRIVE
                
 00F6           BLINK   EQU     DBLOCK+11       ; (WORD) CURSOR BLINK TIMER
 00F8           CSHAPE  EQU     BLINK+2         ; (BYTE) CURRENT CURSOR SHAPE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- EQUATES                                                                          PAGE   5    
--- ZIP Z-PAGE VARIABLES ---

                
                        END
                
                        INCLUD  HARDEQ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   6    
--- HARDWARE EQUATES: ATARI ---

                
 0000                   ; ----------------
 0000                   ; ATARI MEMORY MAP
 0000                   ; ----------------
                
 0400           IOBUFF  EQU     $0400           ; DISK I/O BUFFER
 0480           BUFTOP  EQU     $0480           ; TOP HALF OF I/O BUFFER
 0500           ZSTAKL  EQU     $0500           ; Z-STACK LSBS
 0600           ZSTAKH  EQU     $0600           ; Z-STACK MSBS
 0700           PTABL   EQU     $0700           ; PAGING TABLE LSBS
 0800           PTABH   EQU     $0800           ; PAGING TABLE MSBS
 0900           LRUMAP  EQU     $0900           ; TIMESTAMP MAP
 0A00           LOCALS  EQU     $0A00           ; LOCAL VARIABLE STORAGE (30 BYTES)
 0A20           BUFSAV  EQU     $0A20           ; AUXILIARY INPUT BUFFER (80 BYTES)
 0A80           LBUFF   EQU     $0A80           ; MAIN INPUT BUFFER (80 BYTES)
                
 0800           PLMRAM  EQU     $0800           ; START OF PLAYER/MISSILE RAM
 0B00           MISSL   EQU     $0B00           ; START OF MISSILE RAM (CURSOR)
                
 0C00           ZIP     EQU     $0C00           ; START OF ZIP CODE
 2600           ZBEGIN  EQU     $2600           ; START OF Z-CODE (ASSUME 6.5K ZIP)
                
 BC20           OLDLST  EQU     $BC20           ; CIO DEFAULT DL ADDR
 BC40           SCREEN  EQU     $BC40           ; START OF SCREEN RAM
                
 0000                   ; ---------
 0000                   ; CONSTANTS
 0000                   ; ---------
                
 0027           XSIZE   EQU     39              ; WIDTH OF SCREEN IN CHARACTERS (-1)
 0017           YSIZE   EQU     23              ; HEIGHT OF SCREEN IN LINES (-1)
                
 009B           EOL     EQU     $9B             ; EOL CHAR
 0020           SPACE   EQU     $20             ; SPACE CHAR
 007E           BACKSP  EQU     126             ; BACKSPACE
                
 0000                   ; ---------
 0000                   ; ZER0-PAGE
 0000                   ; ---------
                
 0009           BOOT    EQU     $09             ; BOOT FLAG
 000A           DOSVEC  EQU     $0A             ; DOS START VECTOR
 0010           POKMSK  EQU     $10             ; FOR BREAK KEY DISABLE
 0014           RTCLOK  EQU     $14             ; JIFFY CLOCK
 0052           LMARGN  EQU     $52             ; LEFT MARGIN
 0054           ROWCRS  EQU     $54             ; OS CURSOR ROW
 0055           COLCRS  EQU     $55             ; OS CURSOR COLUMN
                
 0000                   ; ---------
 0000                   ; PAGES 2-3
 0000                   ; ---------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   7    
--- HARDWARE EQUATES: ATARI ---

                
 022F           SDMCTL  EQU     $022F           ; DMA CONTROL
 0230           SDLSTL  EQU     $0230           ; DISPLAY LIST ADDRESS
 0244           COLDST  EQU     $0244           ; COLDSTART FLAG
 026F           GPRIOR  EQU     $026F           ; GRAPHICS PRIORITY
 02B2           LOGMAP  EQU     $02B2           ; LOGICAL LINE MAP (3 BYTES)
 02B6           INVFLG  EQU     $02B6           ; INVERSE TEXT FLAG
 02C5           COLOR1  EQU     $02C5           ; TEXT COLOR
 02C8           COLOR4  EQU     $02C8           ; BORDER COLOR
 02F0           CRSINH  EQU     $02F0           ; OS CURSOR INHIBIT
 02FC           CH      EQU     $02FC           ; KEYBOARD READ FLAG
 0301           DUNIT   EQU     $0301           ; DRIVE #
 0302           DCOMND  EQU     $0302           ; DISK COMMAND
 0303           DSTATS  EQU     $0303           ; DISK I/O STATUS
 0304           DBUFLO  EQU     $0304           ; DISK BUFFER ADDR (LSB)
 0305           DBUFHI  EQU     $0305           ; DISK BUFFER ADDR (MSB)
 030A           DAUX1   EQU     $030A           ; SECTOR ADDR (LSB)
 030B           DAUX2   EQU     $030B           ; SECTOR ADDR (MSB)
 0342           ICCOM   EQU     $0342           ; IOCB #0 COMMAND
 0344           ICBADR  EQU     $0344           ; IOCB #0 BUFFER ADDR
 0348           ICBLEN  EQU     $0348           ; IOCB #0 LENGTH
 034A           ICAUX1  EQU     $034A           ; IOCB #0 AUX BYTE #1
 034B           ICAUX2  EQU     $034B           ; IOCB #0 AUX BYTE #2
                
 0000                   ; ------------------
 0000                   ; GTIA, POKEY, ANTIC
 0000                   ; ------------------
                
 D000           HPOSP0  EQU     $D000           ; PLAYER #0 H-POS
 D004           HPOSM0  EQU     $D004           ; MISSILE #0 H-POS
 D00C           SIZEM   EQU     $D00C           ; MISSILE SIZES
 D01D           GRACTL  EQU     $D01D           ; P/M GRAPHICS CONTROL
 D01F           CONSOL  EQU     $D01F           ; CONSOLE KEY REGISTER
 D200           AUDF1   EQU     $D200           ; AUDIO CH1 FREQ
 D201           AUDC1   EQU     $D201           ; AUDIO CH1 CTRL
 D208           AUDCTL  EQU     $D208           ; AUDIO CONTROL
 D20A           MRAND   EQU     $D20A           ; RANDOM BYTE
 D20E           IRQEN   EQU     $D20E           ; IRQ ENABLE
 D20F           SKCTL   EQU     $D20F           ; SERIAL PORT CONTROL
 D301           PORTB   EQU     $D301           ; PORT B (XL ROM SWITCH)
 D400           DMACTL  EQU     $D400           ; DMA CONTROL
 D407           PMBASE  EQU     $D407           ; PMG RAM BASE ADDR
                
 0000                   ; ------
 0000                   ; OS ROM
 0000                   ; ------
                
 E453           DSKINV  EQU     $E453           ; SIO DISK ACCESS
 E456           CIOV    EQU     $E456           ; CIO VECTOR
                
                        END


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   8    
--- HARDWARE EQUATES: ATARI ---

                        INCLUD  COLD.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE   9    
--- MACHINE COLDSTART: ATARI ---

                
 0C00                   ORG     ZIP
                
 0C00                   ; -----------------
 0C00                   ; ATARI BOOT HEADER
 0C00                   ; -----------------
                
 0C00 00                DB      0               ; FLAG BYTE (IGNORED)
 0C01 38                DB      56              ; LOAD 7K OF CODE
 0C02 000C              DW      ZIP             ; WHERE TO LOAD THE SECTORS
 0C04 0E0C              DW      DUMMY           ; POINT TO INIT SUBROUTINE
                
 0C06                   ; --------------
 0C06                   ; BOOT COLDSTART
 0C06                   ; --------------
                
 0C06 A94F              LDA     #LOW COLD       ; POINT [DOSVEC] TO
 0C08 850A              STA     DOSVEC+LO       ; THE COLDSTART ROUTINE
 0C0A A90C              LDA     #HIGH COLD
 0C0C 850B              STA     DOSVEC+HI
                
 0C0E A9FF      DUMMY:  LDA     #$FF            ; DISABLE BASIC ROM
 0C10 8D01D3            STA     PORTB           ; IN XL-SERIES MACHINES
 0C13 18                CLC                     ; SUCCESS FLAG
 0C14 60                RTS
                
 0C15                   ; -------------------
 0C15                   ; CUSTOM DISPLAY LIST
 0C15                   ; -------------------
                
 0C15 707050    DLIST:  DB      $70,$70,$50                     ; 22 BLANK LINES
 0C18 42                DB      $42                             ; 1 TEXT LINE W/LMS
 0C19 40BC              DW      SCREEN                          ; ADDR OF SCREEN RAM
 0C1B 10                DB      $10                             ; 2 BLANK LINES
 0C1C 02020202          DB      $02,$02,$02,$02,$02,$02,$02     ; 7 TEXT LINES
 0C23 02020202          DB      $02,$02,$02,$02,$02,$02,$02,$02 ; 8 TEXT LINES
 0C2B 02020202          DB      $02,$02,$02,$02,$02,$02,$02,$02 ; 8 TEXT LINES
 0C33 41                DB      $41                             ; JVB
 0C34 150C              DW      DLIST                           ; ADDR OF D-LIST
                
 0C36 54686520  SLD:    DB      "The story is loading ..."
 0C4E 9B                DB      EOL
 0019           SLDL    EQU     $-SLD
                
 0C4F                   ; ---------
 0C4F                   ; COLDSTART
 0C4F                   ; ---------
                
 0C4F D8        COLD:   CLD
 0C50 A2FF              LDX     #$FF            ; RESET THE
 0C52 9A                TXS                     ; HARDWARE STACK


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE  10    
--- MACHINE COLDSTART: ATARI ---

 0C53 8EFC02            STX     CH              ; AND KEYBOARD REGISTER
 0C56 8E01D3            STX     PORTB           ; REMOVE BASIC ROM IN XL MACHINES
 0C59 E8                INX                     ; = 0
 0C5A 8E4402            STX     COLDST          ; COLDSTART OKAY
 0C5D 8E08D2            STX     AUDCTL          ; CLEAR SOUND
 0C60 8652              STX     LMARGN          ; NO LEFT MARGIN
 0C62 86DF              STX     SCRIPT          ; DISABLE SCRIPTING
 0C64 8ED81F            STX     SFLAG           ; DISABLE PREVIOS SCRIPT (BM 5/14/85)
 0C67 E8                INX                     ; = 1
 0C68 8609              STX     BOOT            ; BOOT SUCCESSFUL
 0C6A 8E6F02            STX     GPRIOR          ; MAXIMUM PMG PRIORITY
 0C6D 8EF002            STX     CRSINH          ; INHIBIT OS CURSOR
                
 0C70 A903              LDA     #3
 0C72 8D0FD2            STA     SKCTL           ; RESET SOUND
 0C75 8D1DD0            STA     GRACTL          ; ENABLE PLAYERS & MISSILES
                
 0C78 A970              LDA     #$70
 0C7A 8510              STA     POKMSK          ; DISABLE
 0C7C 8D0ED2            STA     IRQEN           ; THE BREAK KEY
                
 0C7F A90C              LDA     #12             ; WHITE
 0C81 8DC502            STA     COLOR1          ; TEXT
 0C84 A994              LDA     #148
 0C86 8DC802            STA     COLOR4          ; BLUE BORDER
                
 0C89 A211              LDX     #$11            ; CLEAR ALL PMG REGISTERS
 0C8B A900              LDA     #0
 0C8D 9D00D0    PMG0:   STA     HPOSP0,X
 0C90 CA                DEX
 0C91 10FA              BPL     PMG0
                
 0C93 AA                TAX                     ; [X] & [A] = 0
 0C94 9D000B    PMG1:   STA     MISSL,X         ; CLEAR CURSOR RAM
 0C97 E8                INX
 0C98 D0FA              BNE     PMG1
                
 0C9A A901              LDA     #1              ; DOUBLE-WIDTH FOR
 0C9C 8D0CD0            STA     SIZEM           ; MISSILE #0
 0C9F A908              LDA     #HIGH PLMRAM    ; POINT TO LOCATION
 0CA1 8D07D4            STA     PMBASE          ; OF PMG RAM
                
 0CA4 A915              LDA     #LOW DLIST      ; ENABLE
 0CA6 8D3002            STA     SDLSTL+LO       ; CUSTOM
 0CA9 A90C              LDA     #HIGH DLIST     ; DISPLAY
 0CAB 8D3102            STA     SDLSTL+HI       ; LIST
                
 0CAE                   ; FALL THROUGH ...
                
 0CAE                   ; ---------------
 0CAE                   ; WARMSTART ENTRY


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT INIT                                                           PAGE  11    
--- MACHINE COLDSTART: ATARI ---

 0CAE                   ; ---------------
                
 0CAE 20D320    WARM:   JSR     CLS             ; CLEAR SCREEN
                
 0CB1 A908              LDA     #8              ; POSITION CURSOR
 0CB3 8555              STA     COLCRS+LO       ; AT (8,0)
 0CB5 A900              LDA     #0
 0CB7 8554              STA     ROWCRS
                
 0CB9 A236              LDX     #LOW SLD
 0CBB A90C              LDA     #HIGH SLD
 0CBD A019              LDY     #SLDL
 0CBF 20A823            JSR     SROOM           ; "THE STORY IS LOADING ..."
                
 0CC2 A93E              LDA     #%00111110      ; ENABLE 1-LINE PMG, STANDARD FIELD
 0CC4 8D2F02            STA     SDMCTL          ; RESTORE ANTIC
                
 0CC7                   ; FALL THROUGH TO ZIP WARMSTART ...
                
                        END
                
                        INCLUD  WARM.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  12    
--- WARMSTART ROUTINE ---

                
 0CC7                   ; -------------
 0CC7                   ; ZIP WARMSTART
 0CC7                   ; -------------
                
 0CC7 A900      WARM1:  LDA     #0              ; CLEAR ALL Z-PAGE VARIABLES
 0CC9 A280              LDX     #ZEROPG
 0CCB 9500      ST0:    STA     0,X
 0CCD E8                INX
                ;       CPX     #ZPGTOP         ; NOT NEEDED FOR ATARI
 0CCE D0FB              BNE     ST0
                
 0CD0                   ; INIT THE PAGING TABLE
                
 0CD0 AA                TAX                     ; = 0
 0CD1 A9FF              LDA     #$FF
 0CD3 9D0007    ST1:    STA     PTABL,X
 0CD6 9D0008            STA     PTABH,X
 0CD9 E8                INX
 0CDA D0F7              BNE     ST1
                
 0CDC                   ; CLEAR THE TIMESTAMP MAP
                
 0CDC 8A                TXA                     ; = 0
 0CDD 9D0009    ST2:    STA     LRUMAP,X
 0CE0 E8                INX
 0CE1 D0FA              BNE     ST2
                
 0CE3 E694              INC     ZSP             ; INIT Z-STACK POINTERS
 0CE5 E695              INC     OLDZSP          ; TO "1"
 0CE7 E6AA              INC     STAMP           ; INIT TIMESTAMP
                
 0CE9                   ; GRAB THE FIRST BLOCK OF PRELOAD
                
 0CE9 A926              LDA     #HIGH ZBEGIN    ; MSB OF PRELOAD START ADDRESS
 0CEB 85A3              STA     ZCODE           ; FREEZE IT HERE
 0CED 85EE              STA     DBUFF+HI        ; LSB IS ALWAYS ZERO
 0CEF C6A4              DEC     ZPURE           ; (ATARI ONLY) FORCE FETCH FROM PRELOAD
 0CF1 203325            JSR     GETDSK          ; [DBLOCK] SET TO Z-BLOCK 0
                
 0CF4                   ; EXTRACT GAME DATA FROM Z-CODE HEADER
                
 0CF4 AE0426            LDX     ZBEGIN+ZENDLD   ; MSB OF ENDLOAD POINTER
 0CF7 E8                INX                     ; ADD 1 TO GET
 0CF8 86A4              STX     ZPURE           ; 1ST "PURE" PAGE OF Z-CODE
                
 0CFA 8A                TXA                     ; ADD START PAGE OF PRELOAD
 0CFB 18                CLC                     ; TO CALC ABSOLUTE START ADDRESS
 0CFC 65A3              ADC     ZCODE           ; OF PAGING SPACE
 0CFE 85A5              STA     PAGE0
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  13    
--- WARMSTART ROUTINE ---

 0D00 206A1C            JSR     MEMTOP          ; RETURNS TOP RAM PAGE IN [A]
 0D03 38                SEC
 0D04 E5A5              SBC     PAGE0           ; SUBTRACT ADDRESS OF PAGING SPACE
 0D06 F002              BEQ     NORAM
 0D08 B005              BCS     SETNP           ; ERROR IF NOT ENOUGH RAM
                
 0D0A                   ; *** ERROR #0 -- INSUFFICIENT RAM ***
                
 0D0A A900      NORAM:  LDA     #0
 0D0C 4CF91B            JMP     ZERROR
                
 0D0F 85A6      SETNP:  STA     PMAX            ; SET # SWAPPING PAGES
                
 0D11 AD0126            LDA     ZBEGIN+ZMODE
 0D14 0920              ORA     #%00100000      ; ENABLE SPLIT-SCREEN
 0D16 8D0126            STA     ZBEGIN+ZMODE
                
 0D19 2902              AND     #%00000010      ; ISOLATE STATUS-FORMAT BIT
 0D1B 85DC              STA     TIMEFL          ; 0=SCORE, NZ=TIME
                
 0D1D AD0C26            LDA     ZBEGIN+ZGLOBA   ; GET MSB OF GLOBAL TABLE ADDR
 0D20 18                CLC                     ; CONVERT TO
 0D21 65A3              ADC     ZCODE           ; ABSOLUTE ADDRESS
 0D23 85AD              STA     GLOBAL+HI
 0D25 AD0D26            LDA     ZBEGIN+ZGLOBA+1 ; LSB NEEDN'T CHANGE
 0D28 85AC              STA     GLOBAL+LO
                
 0D2A AD1826            LDA     ZBEGIN+ZFWORD   ; DO SAME FOR FWORDS TABLE
 0D2D 18                CLC
 0D2E 65A3              ADC     ZCODE
 0D30 85B1              STA     FWORDS+HI
 0D32 AD1926            LDA     ZBEGIN+ZFWORD+1 ; NO CHANGE FOR LSB
 0D35 85B0              STA     FWORDS+LO
                
 0D37 AD0826            LDA     ZBEGIN+ZVOCAB   ; NOW DO VOCABULARY TABLE
 0D3A 18                CLC
 0D3B 65A3              ADC     ZCODE
 0D3D 85AF              STA     VOCAB+HI
 0D3F AD0926            LDA     ZBEGIN+ZVOCAB+1 ; LSB SAME
 0D42 85AE              STA     VOCAB+LO
                
 0D44 AD0A26            LDA     ZBEGIN+ZOBJEC   ; NOT TO MENTION
 0D47 18                CLC                     ; THE OBJECT TABLE
 0D48 65A3              ADC     ZCODE
 0D4A 85B3              STA     OBJTAB+HI
 0D4C AD0B26            LDA     ZBEGIN+ZOBJEC+1 ; LSB SAME
 0D4F 85B2              STA     OBJTAB+LO
                
 0D51                   ; FETCH THE REST OF THE PRELOAD
                
 0D51 A5EB      LDPRE:  LDA     DBLOCK+LO       ; CHECK CURRENT BLOCK #


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  14    
--- WARMSTART ROUTINE ---

 0D53 C5A4              CMP     ZPURE           ; LOADED LAST PRELOAD PAGE YET?
 0D55 B006              BCS     WARMEX          ; YES, TIME TO PLAY!
 0D57 203325            JSR     GETDSK          ; ELSE GRAB NEXT Z-BLOCK
 0D5A 4C510D            JMP     LDPRE
                
 0D5D AD0626    WARMEX: LDA     ZBEGIN+ZGO      ; GET START ADDRESS OF Z-CODE
 0D60 8597              STA     ZPCM            ; MSB
 0D62 AD0726            LDA     ZBEGIN+ZGO+1    ; AND LSB
 0D65 8596              STA     ZPCL            ; HIGH BIT ALREADY ZEROED
                
 0D67 A915              LDA     #21
 0D69 85E1              STA     LMAX            ; PREVENT WEIRDNESS
                
 0D6B 207F23            JSR     SIDE2           ; REQUEST SIDE 2 OF DISK
 0D6E 20D320            JSR     CLS             ; CLEAR SCREEN
                
 0D71 A9FF              LDA     #$FF
 0D73 85DF              STA     SCRIPT          ; ENABLE SCRIPTING
                
 0D75 AD1126            LDA     ZBEGIN+ZSCRIP+1 ; SET SCRIPT FLAG
 0D78 0DD81F            ORA     SFLAG           ; TO PREVIOUS SETTING
 0D7B 8D1126            STA     ZBEGIN+ZSCRIP+1 ; (BM 5/14/85)
                
 0D7E                   ; ... AND FALL INTO MAIN LOOP
                
                        END
                        INCLUD  MAIN.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  15    
--- MAIN LOOP ---

                
 0D7E A900      MLOOP:  LDA     #0
 0D80 8581              STA     NARGS           ; RESET # ARGUMENTS
 0D82 201918            JSR     NEXTPC          ; GET NEXT INSTRUCTION INTO [A]
 0D85 8580              STA     OPCODE          ; SAVE IT HERE
                
 0D87                   IF      DEBUG
 0D87                   LDA     SECTOR+LO
 0D87                   STA     MBYTE
 0D87                   LDA     #0              ; BREAKPOINT #0
 0D87                   JSR     DOBUG
 0D87                   LDA     OPCODE
 0D87                   ENDIF
                
 0D87                   ; DECODE AN OPCODE
                
 0D87 AA                TAX                     ; SET FLAGS
 0D88 3003              BMI     DC0             ; IF POSITIVE,
 0D8A 4C480E            JMP     OP2             ; IT'S A 2-OP
                
 0D8D C9B0      DC0:    CMP     #$B0
 0D8F B003              BCS     DC1
 0D91 4C190E            JMP     OP1             ; OR MAYBE A 1-OP
                
 0D94 C9C0      DC1:    CMP     #$C0
 0D96 B003              BCS     OPEXT
 0D98 4C0A0E            JMP     OP0             ; PERHAPS A 0-OP
                
 0D9B                   ; --------------
 0D9B                   ; HANDLE AN X-OP
 0D9B                   ; --------------
                
 0D9B 201918    OPEXT:  JSR     NEXTPC          ; GRAB THE ARGUMENT ID BYTE
 0D9E 858A              STA     ABYTE           ; HOLD IT HERE
                
 0DA0 A200              LDX     #0
 0DA2 868B              STX     ADEX            ; INIT ARGUMENT INDEX
 0DA4 F006              BEQ     OPX1            ; JUMP TO TOP OF LOOP
                
 0DA6 A58A      OPX0:   LDA     ABYTE           ; GET ARG BYTE
 0DA8 0A                ASL     A               ; SHIFT NEXT 2 ARG BITS
 0DA9 0A                ASL     A               ; INTO BITS 7 & 6
 0DAA 858A              STA     ABYTE           ; HOLD FOR LATER
                
 0DAC 29C0      OPX1:   AND     #%11000000      ; MASK OUT GARBAGE BITS
 0DAE D006              BNE     OPX2
 0DB0 20940E            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 0DB3 4CC70D            JMP     OPXNXT
                
 0DB6 C940      OPX2:   CMP     #%01000000      ; IS IT A SHORT IMMEDIATE?
 0DB8 D006              BNE     OPX3            ; NO, KEEP GUESSING


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  16    
--- MAIN LOOP ---

 0DBA 20900E            JSR     GETSHT          ; 01 = SHORT IMMEDIATE
 0DBD 4CC70D            JMP     OPXNXT
                
 0DC0 C980      OPX3:   CMP     #%10000000      ; LAST TEST
 0DC2 D017              BNE     OPX4            ; 11 = NO MORE ARGUMENTS
 0DC4 20A80E            JSR     GETVAR          ; 10 = VARIABLE
                
 0DC7 A68B      OPXNXT: LDX     ADEX            ; RETRIEVE ARGUMENT INDEX
 0DC9 A58C              LDA     VALUE+LO        ; GRAB LSB OF VALUE
 0DCB 9582              STA     ARG1+LO,X       ; STORE IN ARGUMENT TABLE
 0DCD A58D              LDA     VALUE+HI        ; GRAB MSB OF VALUE
 0DCF 9583              STA     ARG1+HI,X       ; STORE THAT, TOO
                
 0DD1 E681              INC     NARGS           ; UPDATE ARGUMENT COUNTER
                
 0DD3 E8                INX
 0DD4 E8                INX
 0DD5 868B              STX     ADEX            ; UPDATE INDEX
 0DD7 E008              CPX     #8              ; DONE 4 ARGUMENTS YET?
 0DD9 90CB              BCC     OPX0            ; NO, GET SOME MORE
                
 0DDB                   ; ALL X-OP ARGUMENTS READY
                
 0DDB A580      OPX4:   LDA     OPCODE          ; IS THIS
 0DDD C9E0              CMP     #$E0            ; AN EXTENDED 2-OP?
 0DDF B003              BCS     DOXOP           ; NO, IT'S A REAL X-OP
 0DE1 4C710E            JMP     OP2EX           ; ELSE TREAT IT LIKE A 2-OP
                
 0DE4 A252      DOXOP:  LDX     #LOW OPTX       ; GET ADDR OF X-OP TABLE
 0DE6 A010              LDY     #HIGH OPTX      ; INTO [X/Y]
 0DE8 291F              AND     #%00011111      ; ISOLATE OP ID BITS
 0DEA C90C              CMP     #NOPSX          ; IS IT A LEGAL X-OP?
 0DEC 9005              BCC     DODIS           ; YUP; TIME TO DISPATCH IT
                
 0DEE                   ; *** ERROR #1 -- ILLEGAL X-OP ***
                
 0DEE A901              LDA     #1
 0DF0 4CF91B            JMP     ZERROR
                
 0DF3                   ; ---------------
 0DF3                   ; OPCODE DISPATCH
 0DF3                   ; ---------------
                
 0DF3                   ; ENTRY: MASKED OPCODE INDEX IN [A]
 0DF3                   ;        OP-TABLE ADDR IN X/Y (LSB/MSB)
                
 0DF3 868E      DODIS:  STX     I+LO            ; SAVE TABLE ADDRESS
 0DF5 848F              STY     I+HI            ; IN A POINTER
                
 0DF7 0A                ASL     A               ; WORD-ALIGN THE OP INDEX
 0DF8 A8                TAY


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  17    
--- MAIN LOOP ---

 0DF9 B18E              LDA     (I),Y           ; GET LSB OF DISPATCH ADDRESS
 0DFB 8D050E            STA     GO+LO           ; INSTALL AS JSR OPERAND
 0DFE C8                INY
 0DFF B18E              LDA     (I),Y           ; SAME WITH MSB
 0E01 8D060E            STA     GO+HI
                
 0E04 20                DB      $20             ; 6502 "JSR" OPCODE
 0E05 0000      GO:     DW      $0000           ; DUMMY OPERAND BYTES
                
 0E07 4C7E0D            JMP     MLOOP           ; GO BACK FOR ANOTHER OPCODE
                
 0E0A                   ; -------------
 0E0A                   ; HANDLE A 0-OP
 0E0A                   ; -------------
                
 0E0A A2E4      OP0:    LDX     #LOW OPT0       ; GET 0-OP TABLE ADDR
 0E0C A00F              LDY     #HIGH OPT0      ; INTO [X/Y]
 0E0E 290F              AND     #%00001111      ; ISOLATE 0-OP ID BITS
 0E10 C90E              CMP     #NOPS0          ; OUT OF RANGE?
 0E12 90DF              BCC     DODIS           ; NO, DISPATCH IT
                
 0E14                   ; *** ERROR #2 -- ILLEGAL 0-OP ***
                
 0E14 A902              LDA     #2
 0E16 4CF91B            JMP     ZERROR
                
 0E19                   ; -------------
 0E19                   ; HANDLE A 1-OP
 0E19                   ; -------------
                
 0E19 2930      OP1:    AND     #%00110000      ; ISOLATE ARGUMENT BITS
 0E1B D006              BNE     OP1A
 0E1D 20940E            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 0E20 4C340E            JMP     OP1EX
                
 0E23 C910      OP1A:   CMP     #%00010000      ; TEST AGAIN
 0E25 D006              BNE     OP1B
 0E27 20900E            JSR     GETSHT          ; 01 = SHORT IMMEDIATE
 0E2A 4C340E            JMP     OP1EX
                
 0E2D C920      OP1B:   CMP     #%00100000      ; ONE MORE TEST
 0E2F D012              BNE     BADOP1          ; UNDEFINED STATE!
 0E31 20A80E            JSR     GETVAR          ; 10 = VARIABLE
                
 0E34 20850E    OP1EX:  JSR     V2A1            ; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
 0E37 A200              LDX     #LOW OPT1       ; GET ADDR OF 1-OP TABLE
 0E39 A010              LDY     #HIGH OPT1      ; INTO [X/Y]
 0E3B A580              LDA     OPCODE          ; RESTORE OPCODE
 0E3D 290F              AND     #%00001111      ; ISOLATE OP ID BITS
 0E3F C910              CMP     #NOPS1          ; IF WITHIN RANGE,
 0E41 90B0              BCC     DODIS           ; EXECUTE THE 1-OP


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  18    
--- MAIN LOOP ---

                
 0E43                   ; *** ERROR #3 -- ILLEGAL 1-OP ***
                
 0E43 A903      BADOP1: LDA     #3
 0E45 4CF91B            JMP     ZERROR
                
 0E48                   ; -------------
 0E48                   ; HANDLE A 2-OP
 0E48                   ; -------------
                
 0E48 2940      OP2:    AND     #%01000000      ; ISOLATE 1ST ARG BIT
 0E4A D006              BNE     OP2A
 0E4C 20900E            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 0E4F 4C550E            JMP     OP2B
 0E52 20A80E    OP2A:   JSR     GETVAR          ; 1 = VARIABLE
 0E55 20850E    OP2B:   JSR     V2A1            ; [VALUE] TO [ARG1], UPDATE [NARGS]
                
 0E58 A580              LDA     OPCODE          ; RESTORE OPCODE BYTE
 0E5A 2920              AND     #%00100000      ; ISOLATE 2ND ARG BIT
 0E5C D006              BNE     OP2C
 0E5E 20900E            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 0E61 4C670E            JMP     OP2D
 0E64 20A80E    OP2C:   JSR     GETVAR          ; 1 = VARIABLE
 0E67 A58C      OP2D:   LDA     VALUE+LO        ; MOVE 2ND [VALUE]
 0E69 8584              STA     ARG2+LO         ; INTO [ARG2]
 0E6B A58D              LDA     VALUE+HI
 0E6D 8585              STA     ARG2+HI
 0E6F E681              INC     NARGS           ; UPDATE ARGUMENT COUNT
                
 0E71                   ; EXECUTE A 2-OP OR EXTENDED 2-OP
                
 0E71 A220      OP2EX:  LDX     #LOW OPT2       ; LSB OF DISPATCH TABLE
 0E73 A010              LDY     #HIGH OPT2      ; MSB
 0E75 A580              LDA     OPCODE          ; RESTORE OPCODE BYTE
 0E77 291F              AND     #%00011111      ; ISOLATE OP ID BITS
 0E79 C919              CMP     #NOPS2
 0E7B B003              BCS     BADOP2          ; ERROR IF OUT OF RANGE
 0E7D 4CF30D            JMP     DODIS           ; ELSE DISPATCH
                
 0E80                   ; *** ERROR #4 -- ILLEGAL 2-OP ****
                
 0E80 A904      BADOP2: LDA     #4
 0E82 4CF91B            JMP     ZERROR
                
 0E85                   ; --------------------------------------
 0E85                   ; MOVE [VALUE] TO [ARG1], UPDATE [NARGS]
 0E85                   ; --------------------------------------
                
 0E85 A58C      V2A1:   LDA     VALUE+LO
 0E87 8582              STA     ARG1+LO
 0E89 A58D              LDA     VALUE+HI


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  19    
--- MAIN LOOP ---

 0E8B 8583              STA     ARG1+HI
 0E8D E681              INC     NARGS
 0E8F 60                RTS
                
                        END
                        INCLUD  SUBS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  20    
--- OPCODE SUPPORT SUBROUTINES ---

                
 0E90                   ; -----------------------
 0E90                   ; FETCH A SHORT IMMEDIATE
 0E90                   ; -----------------------
                
 0E90 A900      GETSHT: LDA     #0              ; MSB IS ZERO
 0E92 F003              BEQ     GETV            ; FETCH LSB FROM Z-CODE
                
 0E94                   ; ----------------------
 0E94                   ; FETCH A LONG IMMEDIATE
 0E94                   ; ----------------------
                
 0E94 201918    GETLNG: JSR     NEXTPC          ; GRAB MSB
                
 0E97 858D      GETV:   STA     VALUE+HI
 0E99 201918            JSR     NEXTPC          ; GRAB LSB
 0E9C 858C              STA     VALUE+LO
 0E9E 60                RTS
                
 0E9F                   ; ----------------
 0E9F                   ; FETCH A VARIABLE
 0E9F                   ; ----------------
                
 0E9F                   ; FROM INSIDE AN OPCODE (VARIABLE ID IN [A])
                
 0E9F AA        VARGET: TAX                     ; IF NON-ZERO,
 0EA0 D00B              BNE     GETVR1          ; ACCESS A VARIABLE
                
 0EA2 20CE0E            JSR     POPVAL          ; ELSE PULL VAR OFF Z-STACK
 0EA5 4CE40E            JMP     PSHVAL          ; WITHOUT ALTERING STACK
                
 0EA8                   ; FROM THE MAIN LOOP (VARIABLE ID IN Z-CODE)
                
 0EA8 201918    GETVAR: JSR     NEXTPC          ; GRAB VAR-TYPE BYTE
 0EAB F021              BEQ     POPVAL          ; VALUE IS ON Z-STACK
                
 0EAD                   ; IS VARIABLE LOCAL OR GLOBAL?
                
 0EAD C910      GETVR1: CMP     #$10            ; IF >= 16,
 0EAF B010              BCS     GETVRG          ; IT'S GLOBAL
                
 0EB1                   ; HANDLE A LOCAL VARIABLE
                
 0EB1 38        GETVRL: SEC
 0EB2 E901              SBC     #1              ; FORM A ZERO-ALIGNED
 0EB4 0A                ASL     A               ; WORD INDEX
 0EB5 AA                TAX                     ; INTO THE [LOCALS] TABLE
                
 0EB6 BD000A            LDA     LOCALS+LO,X     ; GRAB LSB
 0EB9 858C              STA     VALUE+LO
 0EBB BD010A            LDA     LOCALS+HI,X     ; AND MSB


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  21    
--- OPCODE SUPPORT SUBROUTINES ---

 0EBE 858D              STA     VALUE+HI
 0EC0 60                RTS
                
 0EC1                   ; HANDLE A GLOBAL VARIABLE
                
 0EC1 20320F    GETVRG: JSR     GVCALC          ; GET ADDRESS OF GLOBAL INTO [I]
 0EC4 B18E              LDA     (I),Y           ; MSB OF GLOBAL ([Y] = 0)
 0EC6 858D              STA     VALUE+HI
 0EC8 C8                INY                     ; = 1
 0EC9 B18E              LDA     (I),Y           ; LSB OF GLOBAL
 0ECB 858C              STA     VALUE+LO        ; SAVE IT
 0ECD 60                RTS                     ; AND WE'RE DONE
                
 0ECE                   ; ----------------------------------
 0ECE                   ; POP Z-STACK INTO [VALUE] AND [X/A]
 0ECE                   ; ----------------------------------
                
 0ECE C694      POPVAL: DEC     ZSP
 0ED0 F00D              BEQ     UNDER           ; UNDERFLOW IF ZERO!
                
 0ED2 A494              LDY     ZSP             ; READ STACK POINTER
 0ED4 BE0005            LDX     ZSTAKL,Y        ; GRAB LSB OF STACK VALUE
 0ED7 868C              STX     VALUE+LO        ; GIVE TO [VALUE]
 0ED9 B90006            LDA     ZSTAKH,Y        ; ALSO GRAB MSB
 0EDC 858D              STA     VALUE+HI        ; A SIMILAR FATE
 0EDE 60                RTS
                
 0EDF                   ; *** ERROR #5 -- Z-STACK UNDERFLOW ***
                
 0EDF A905      UNDER:  LDA     #5
 0EE1 4CF91B            JMP     ZERROR
                
 0EE4                   ; -----------------------
 0EE4                   ; PUSH [VALUE] TO Z-STACK
 0EE4                   ; -----------------------
                
 0EE4 A68C      PSHVAL: LDX     VALUE+LO
 0EE6 A58D              LDA     VALUE+HI
                
 0EE8                   ; ---------------------
 0EE8                   ; PUSH [X/A] TO Z-STACK
 0EE8                   ; ---------------------
                
 0EE8 A494      PUSHXA: LDY     ZSP             ; READ STACK POINTER
 0EEA 990006            STA     ZSTAKH,Y        ; PUSH MSB IN [A]
 0EED 8A                TXA
 0EEE 990005            STA     ZSTAKL,Y        ; AND LSB IN [X]
                
 0EF1 E694              INC     ZSP             ; UPDATE Z-STACK POINTER
 0EF3 F001              BEQ     OVER            ; OVERFLOW IF ZEROED!
 0EF5 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  22    
--- OPCODE SUPPORT SUBROUTINES ---

                
 0EF6                   ; *** ERROR #6 -- Z-STACK OVERFLOW ***
                
 0EF6 A906      OVER:   LDA     #6
 0EF8 4CF91B            JMP     ZERROR
                
 0EFB                   ; --------------
 0EFB                   ; RETURN A VALUE
 0EFB                   ; --------------
                
 0EFB                   ; FROM WITHIN AN OPCODE (VARIABLE ID IN [A])
                
 0EFB AA        VARPUT: TAX                     ; IF ZERO,
 0EFC D013              BNE     PUTVR1
                
 0EFE C694              DEC     ZSP             ; FLUSH TOP WORD OFF STACK
 0F00 D0E2              BNE     PSHVAL          ; AND REPLACE WITH [VALUE]
 0F02 F0DB              BEQ     UNDER           ; ERROR IF [ZSP] BECAME ZERO!
                
 0F04                   ; RETURN A ZERO
                
 0F04 A900      RET0:   LDA     #0
                
 0F06                   ; RETURN BYTE IN [A]
                
 0F06 858C      PUTBYT: STA     VALUE+LO
 0F08 A900              LDA     #0
 0F0A 858D              STA     VALUE+HI                ; CLEAR MSB
                
 0F0C                   ; RETURN [VALUE]
                
 0F0C 201918    PUTVAL: JSR     NEXTPC          ; GET VARIABLE ID BYTE
 0F0F F0D3              BEQ     PSHVAL          ; [VALUE] GOES TO Z-STACK
                
 0F11                   ; LOCAL OR GLOBAL VARIABLE?
                
 0F11 C910      PUTVR1: CMP     #$10            ; IF >= 16,
 0F13 B010              BCS     PUTVLG          ; IT'S GLOBAL
                
 0F15                   ; PUT A LOCAL VARIABLE
                
 0F15 38        PUTVLL: SEC
 0F16 E901              SBC     #1              ; FORM A ZERO-ALIGNED
 0F18 0A                ASL     A               ; WORD INDEX
 0F19 AA                TAX                     ; INTO THE [LOCALS] TABLE
                
 0F1A A58C              LDA     VALUE+LO        ; GRAB LSB
 0F1C 9D000A            STA     LOCALS+LO,X     ; SAVE IN LOCAL TABLE
 0F1F A58D              LDA     VALUE+HI        ; DO SAME TO
 0F21 9D010A            STA     LOCALS+HI,X     ; MSB
 0F24 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  23    
--- OPCODE SUPPORT SUBROUTINES ---

                
 0F25                   ; RETURN A GLOBAL VARIABLE
                
 0F25 20320F    PUTVLG: JSR     GVCALC
 0F28 A58D              LDA     VALUE+HI        ; GET MSB
 0F2A 918E              STA     (I),Y           ; STORE AS 1ST BYTE ([Y] = 0)
 0F2C C8                INY                     ; = 1
 0F2D A58C              LDA     VALUE+LO        ; NOW GET LSB
 0F2F 918E              STA     (I),Y           ; STORE AS 2ND BYTE
 0F31 60                RTS
                
 0F32                   ; -----------------------
 0F32                   ; CALC GLOBAL WORD OFFSET
 0F32                   ; -----------------------
                
 0F32                   ; ENTRY: VAR-ID BYTE (16-255) IN [A]
 0F32                   ; EXIT: ABSOLUTE ADDRESS OF GLOBAL VAR IN [I]
 0F32                   ;       [Y] = 0 FOR INDEXING
                
 0F32 38        GVCALC: SEC
 0F33 E910              SBC     #$10            ; FORM A ZERO-ALIGNED INDEX
 0F35 A000              LDY     #0              ; MAKE SURE MSB OF OFFSET AND [Y]
 0F37 848F              STY     I+HI            ; ARE CLEARED
                
 0F39 0A                ASL     A               ; MULTIPLY OFFSET BY 2
 0F3A 268F              ROL     I+HI            ; TO WORD-ALIGN IT
                
 0F3C 18                CLC                     ; ADD OFFSET TO ADDR OF GLOBAL TABLE
 0F3D 65AC              ADC     GLOBAL+LO       ; TO FORM THE ABSOLUTE
 0F3F 858E              STA     I+LO            ; ADDRESS OF THE
 0F41 A58F              LDA     I+HI            ; DESIRED GLOBAL VARIABLE
 0F43 65AD              ADC     GLOBAL+HI       ; STORE ADDRESS BACK IN [VAL]
 0F45 858F              STA     I+HI            ; AS A POINTER
                
 0F47 60        WCEX:   RTS
                
 0F48                   ; ---------------
 0F48                   ; PREDICATE FAILS
 0F48                   ; ---------------
                
 0F48 201918    PREDF:  JSR     NEXTPC          ; GET 1ST BRANCH BYTE
 0F4B 100C              BPL     PREDB           ; DO BRANCH IF BIT 7 OFF
                
 0F4D                   ; -----------------------
 0F4D                   ; IGNORE PREDICATE BRANCH
 0F4D                   ; -----------------------
                
 0F4D                   ; ENTRY: 1ST BRANCH BYTE IN [A]
                
 0F4D 2940      PREDNB: AND     #%01000000      ; TEST BIT 6
 0F4F D0F6              BNE     WCEX            ; SHORT BRANCH IF SET


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  24    
--- OPCODE SUPPORT SUBROUTINES ---

 0F51 4C1918            JMP     NEXTPC          ; ELSE SKIP OVER 2ND BRANCH BYTE
                
 0F54                   ; ------------------
 0F54                   ; PREDICATE SUCCEEDS
 0F54                   ; ------------------
                
 0F54 201918    PREDS:  JSR     NEXTPC          ; GET 1ST BRANCH BYTE
 0F57 10F4              BPL     PREDNB          ; DON'T BRANCH IF BIT 7 CLEAR
                
 0F59                   ; --------------------------
 0F59                   ; PERFORM A PREDICATE BRANCH
 0F59                   ; --------------------------
                
 0F59                   ; ENTRY: 1ST PRED BYTE IN [A]
                
 0F59 AA        PREDB:  TAX                     ; SAVE HERE
 0F5A 2940              AND     #%01000000      ; LONG OR SHORT BRANCH?
 0F5C F00B              BEQ     PREDLB          ; LONG IF BIT 6 IS CLEAR
                
 0F5E                   ; HANDLE A SHORT BRANCH
                
 0F5E 8A                TXA                     ; RESTORE PRED BYTE
 0F5F 293F              AND     #%00111111      ; FORM SHORT OFFSET
 0F61 858C              STA     VALUE+LO        ; USE AS LSB OF BRANCH OFFSET
 0F63 A900              LDA     #0
 0F65 858D              STA     VALUE+HI        ; MSB OF OFFSET IS ZERO
 0F67 F013              BEQ     PREDB1          ; DO THE BRANCH
                
 0F69                   ; HANDLE A LONG BRANCH
                
 0F69 8A        PREDLB: TXA                     ; RESTORE 1ST PRED BYTE
 0F6A 293F              AND     #%00111111      ; FORM MSB OF OFFSET
                
 0F6C AA                TAX                     ; SAVE HERE FOR REFERENCE
                
 0F6D 2920              AND     #%00100000      ; CHECK SIGN OF 14-BIT VALUE
 0F6F F004              BEQ     DOB2            ; POSITIVE IF ZERO, SO USE [X]
                
 0F71 8A                TXA                     ; ELSE RESTORE BYTE
 0F72 09E0              ORA     #%11100000      ; EXTEND THE SIGN BIT
 0F74 AA                TAX                     ; BACK HERE FOR STORAGE
                
 0F75 868D      DOB2:   STX     VALUE+HI
 0F77 201918            JSR     NEXTPC          ; FETCH LSB OF 14-BIT OFFSET
 0F7A 858C              STA     VALUE+LO
                
 0F7C                   ; BRANCH TO Z-ADDRESS IN [VALUE]
                
 0F7C A58D      PREDB1: LDA     VALUE+HI        ; CHECK MSB OF OFFSET
 0F7E D00E              BNE     PREDB3          ; DO BRANCH IF NZ
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  25    
--- OPCODE SUPPORT SUBROUTINES ---

 0F80 A58C              LDA     VALUE+LO        ; IF LSB IS NON-ZERO,
 0F82 D003              BNE     PREDB2          ; MAKE SURE IT ISN'T 1
 0F84 4C7510            JMP     ZRFALS          ; ELSE DO AN "RFALSE"
                
 0F87 C901      PREDB2: CMP     #1              ; IF OFFSET = 1
 0F89 D003              BNE     PREDB3
 0F8B 4C6A10            JMP     ZRTRUE          ; DO AN "RTRUE"
                
 0F8E                   ; ENTRY POINT FOR "JUMP"
                
 0F8E 20C80F    PREDB3: JSR     DECVAL          ; SUBTRACT 2 FROM THE OFFSET
 0F91 20C80F            JSR     DECVAL          ; IN [VALUE]
                
 0F94 A900              LDA     #0              ; CLEAR THE MSB
 0F96 858F              STA     I+HI            ; OF [I]
                
 0F98 A58D              LDA     VALUE+HI        ; MAKE MSB OF OFFSET
 0F9A 858E              STA     I+LO            ; THE LSB OF [I]
 0F9C 0A                ASL     A               ; EXTEND THE SIGN OF OFFSET
 0F9D 268F              ROL     I+HI            ; INTO MSB OF [I]
                
 0F9F A58C              LDA     VALUE+LO        ; GET LSB OF OFFSET
 0FA1 18                CLC
 0FA2 6596              ADC     ZPCL            ; ADD LOW 8 BITS OF ZPC
 0FA4 9006              BCC     PREDB5          ; IF OVERFLOWED,
                
 0FA6 E68E              INC     I+LO            ; UPDATE UPPER 9 BITS
 0FA8 D002              BNE     PREDB5
 0FAA E68F              INC     I+HI
                
 0FAC 8596      PREDB5: STA     ZPCL            ; UPDATE ZPC
                
 0FAE A58E              LDA     I+LO            ; IF UPPER 9 BITS ARE ZERO,
 0FB0 058F              ORA     I+HI            ; NO NEED TO CHANGE PAGES
 0FB2 F013              BEQ     ZNOOP
                
 0FB4 A58E              LDA     I+LO            ; ELSE CALC NEW UPPER BITS
 0FB6 18                CLC
 0FB7 6597              ADC     ZPCM
 0FB9 8597              STA     ZPCM
                
 0FBB A58F              LDA     I+HI
 0FBD 6598              ADC     ZPCH
 0FBF 2901              AND     #%00000001      ; USE ONLY BIT 0
 0FC1 8598              STA     ZPCH
                
 0FC3 A900              LDA     #0
 0FC5 8599              STA     ZPCFLG          ; [ZPC] NO LONGER VALID
                
 0FC7                   ; FALL THROUGH ...
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  26    
--- OPCODE SUPPORT SUBROUTINES ---

 0FC7                   ; ----
 0FC7                   ; NOOP
 0FC7                   ; ----
                
 0FC7 60        ZNOOP:  RTS
                
 0FC8                   ; -----------------
 0FC8                   ; DECREMENT [VALUE]
 0FC8                   ; -----------------
                
 0FC8 A58C      DECVAL: LDA     VALUE+LO
 0FCA 38                SEC
 0FCB E901              SBC     #1
 0FCD 858C              STA     VALUE+LO
 0FCF B002              BCS     DVX
 0FD1 C68D              DEC     VALUE+HI
 0FD3 60        DVX:    RTS
                
 0FD4                   ; -----------------
 0FD4                   ; INCREMENT [VALUE]
 0FD4                   ; -----------------
                
 0FD4 E68C      INCVAL: INC     VALUE+LO
 0FD6 D002              BNE     IVX
 0FD8 E68D              INC     VALUE+HI
 0FDA 60        IVX:    RTS
                
 0FDB                   ; ----------------------
 0FDB                   ; MOVE [ARG1] TO [VALUE]
 0FDB                   ; ----------------------
                
 0FDB A582      A12VAL: LDA     ARG1+LO
 0FDD 858C              STA     VALUE+LO
 0FDF A583              LDA     ARG1+HI
 0FE1 858D              STA     VALUE+HI
 0FE3 60                RTS
                
                        END
                        INCLUD  DISPATCH.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  27    
--- OPCODE DISPATCH TABLES ---

                
 0FE4                   ; 0-OPS
                
 0FE4 6A10      OPT0:   DW      ZRTRUE          ; 0
 0FE6 7510              DW      ZRFALS          ; 1
 0FE8 7910              DW      ZPRI            ; 2
 0FEA 9610              DW      ZPRR            ; 3
 0FEC C70F              DW      ZNOOP           ; 4
 0FEE DF23              DW      ZSAVE           ; 5
 0FF0 7F24              DW      ZREST           ; 6
 0FF2 3E1C              DW      ZSTART          ; 7
 0FF4 9F10              DW      ZRSTAK          ; 8
 0FF6 CE0E              DW      POPVAL          ; 9
 0FF8 1F1C              DW      ZQUIT           ; 10
 0FFA C71C              DW      ZCRLF           ; 11
 0FFC 201D              DW      ZUSL            ; 12
 0FFE A510              DW      ZVER            ; 13
                
 000E           NOPS0   EQU     14              ; NUMBER OF 0-OPS
                
 1000                   ; 1-OPS
                
 1000 2711      OPT1:   DW      ZZERO           ; 0
 1002 3011              DW      ZNEXT           ; 1
 1004 3911              DW      ZFIRST          ; 2
 1006 4C11              DW      ZLOC            ; 3
 1008 5811              DW      ZPTSIZ          ; 4
 100A 7511              DW      ZINC            ; 5
 100C 8011              DW      ZDEC            ; 6
 100E 8D11              DW      ZPRB            ; 7
 1010 430E              DW      BADOP1          ; 8 (UNDEFINED)
 1012 9B11              DW      ZREMOV          ; 9
 1014 DD11              DW      ZPRD            ; 10
 1016 FA11              DW      ZRET            ; 11
 1018 3712              DW      ZJUMP           ; 12
 101A 3D12              DW      ZPRINT          ; 13
 101C 4B12              DW      ZVALUE          ; 14
 101E 5312              DW      ZBCOM           ; 15
                
 0010           NOPS1   EQU     16              ; NUMBER OF 1-OPS
                
 1020                   ; 2-OPS
                
 1020 800E      OPT2:   DW      BADOP2          ; 0 (UNDEFINED)
 1022 0115              DW      ZEQUAL          ; 1
 1024 6312              DW      ZLESS           ; 2
 1026 7712              DW      ZGRTR           ; 3
 1028 6912              DW      ZDLESS          ; 4
 102A 8212              DW      ZIGRTR          ; 5
 102C B212              DW      ZIN             ; 6
 102E C212              DW      ZBTST           ; 7


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- INIT & MAINLINE                                                                  PAGE  28    
--- OPCODE DISPATCH TABLES ---

 1030 D512              DW      ZBOR            ; 8
 1032 E112              DW      ZBAND           ; 9
 1034 ED12              DW      ZFSETP          ; 10
 1036 0113              DW      ZFSET           ; 11
 1038 1413              DW      ZFCLR           ; 12
 103A 2B13              DW      ZSET            ; 13
 103C 3813              DW      ZMOVE           ; 14
 103E 6213              DW      ZGET            ; 15
 1040 7213              DW      ZGETB           ; 16
 1042 8F13              DW      ZGETP           ; 17
 1044 D713              DW      ZGETPT          ; 18
 1046 0414              DW      ZNEXTP          ; 19
 1048 2314              DW      ZADD            ; 20
 104A 3014              DW      ZSUB            ; 21
 104C 3D14              DW      ZMUL            ; 22
 104E 6114              DW      ZDIV            ; 23
 1050 6B14              DW      ZMOD            ; 24
                
 0019           NOPS2   EQU     25              ; NUMBER OF 2-OPS
                
 1052                   ; X-OPS
                
 1052 3415      OPTX:   DW      ZCALL           ; 0
 1054 C715              DW      ZPUT            ; 1
 1056 D515              DW      ZPUTB           ; 2
 1058 F015              DW      ZPUTP           ; 3
 105A 9C16              DW      ZREAD           ; 4
 105C 2116              DW      ZPRC            ; 5
 105E 2616              DW      ZPRN            ; 6
 1060 6B16              DW      ZRAND           ; 7
 1062 8D16              DW      ZPUSH           ; 8
 1064 9416              DW      ZPOP            ; 9
 1066 4920              DW      ZSPLIT          ; 10
 1068 8E20              DW      ZSCRN           ; 11
                
 000C           NOPSX   EQU     12              ; NUMBER OF X-OPS
                
                        END
                
                        INCLUD  OPS0.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  29    
--- 0-OPS ---

                
 106A                   ; -----
 106A                   ; RTRUE
 106A                   ; -----
                
 106A                   ; SIMULATE A "RETURN 1"
                
 106A A201      ZRTRUE: LDX     #1
                
 106C A900      ZRT0:   LDA     #0
                
 106E 8682      ZRT1:   STX     ARG1+LO         ; GIVE TO
 1070 8583              STA     ARG1+HI         ; [ARG1]
 1072 4CFA11            JMP     ZRET            ; AND DO THE RETURN
                
 1075                   ; ------
 1075                   ; RFALSE
 1075                   ; ------
                
 1075                   ; SIMULATE A "RETURN 0"
                
 1075 A200      ZRFALS: LDX     #0
 1077 F0F3              BEQ     ZRT0
                
 1079                   ; ------
 1079                   ; PRINTI
 1079                   ; ------
                
 1079                   ; PRINT Z-STRING FOLLOWING THE OPCODE
                
 1079 A598      ZPRI:   LDA     ZPCH            ; MOVE [ZPC] INTO [MPC]
 107B 859E              STA     MPCH
 107D A597              LDA     ZPCM
 107F 859D              STA     MPCM
 1081 A596              LDA     ZPCL
 1083 859C              STA     MPCL
                
 1085 A900              LDA     #0
 1087 859F              STA     MPCFLG          ; [MPC] NO LONGER VALID
                
 1089 204219            JSR     PZSTR           ; PRINT THE Z-STRING AT [MPC]
                
 108C A205              LDX     #5              ; COPY STATE OF [MPC]
 108E B59C      PRIL:   LDA     MPC,X           ; INTO [ZPC]
 1090 9596              STA     ZPC,X
 1092 CA                DEX
 1093 10F9              BPL     PRIL
 1095 60                RTS
                
 1096                   ; ------
 1096                   ; PRINTR


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  30    
--- 0-OPS ---

 1096                   ; ------
                
 1096                   ; DO A "PRINTI," FOLLOWED BY "CRLF" AND "RTRUE"
                
 1096 207910    ZPRR:   JSR     ZPRI
 1099 20C71C            JSR     ZCRLF
 109C 4C6A10            JMP     ZRTRUE
                
 109F                   ; ------
 109F                   ; RSTACK
 109F                   ; ------
                
 109F                   ; "RETURN" WITH VALUE ON STACK
                
 109F 20CE0E    ZRSTAK: JSR     POPVAL          ; GET VALUE INTO [X/A]
 10A2 4C6E10            JMP     ZRT1            ; AND GIVE IT TO "RETURN"
                
 10A5                   ; ------
 10A5                   ; VERIFY
 10A5                   ; ------
                
 10A5                   ; VERIFY GAME CODE ON DISK
                
 10A5 205B1C    ZVER:   JSR     VERNUM          ; DISPLAY VERSION NUMBER, GET SIDE 1
                
 10A8 A203              LDX     #3
 10AA A900              LDA     #0
 10AC 9590      ZVR:    STA     J+LO,X          ; CLEAR [J], [K]
 10AE 959C              STA     MPC,X           ; [MPC] AND [MPCFLG]
 10B0 CA                DEX
 10B1 10F9              BPL     ZVR
                
 10B3 A940              LDA     #64             ; POINT [MPC] TO Z-ADDRESS $00040
 10B5 859C              STA     MPCL            ; 1ST 64 BYTES AREN'T CHECKED
                
 10B7 A993              LDA     #K+HI           ; PATCH THE "GETBYT" ROUTINE
 10B9 8D5A18            STA     PATCH           ; TO USE [K+HI]=0 INSTEAD OF [ZPURE]
                
 10BC AD0426            LDA     ZBEGIN+ZENDLD   ; GET LAST BYTE OF ENDLOAD
 10BF 858F              STA     I+HI            ; FIRST MSB
 10C1 AD0526            LDA     ZBEGIN+ZENDLD+1
 10C4 858E              STA     I+LO            ; THEN LSB
                
 10C6                   ; CHECKSUM THE PRELOAD (SIDE 1)
                
 10C6 204F18    VSUM:   JSR     GETBYT          ; GET A Z-BYTE INTO [A]
 10C9 18                CLC
 10CA 6590              ADC     J+LO            ; ADD IT TO SUM
 10CC 8590              STA     J+LO            ; IN [J]
 10CE 9002              BCC     VSUM0
 10D0 E691              INC     J+HI


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  31    
--- 0-OPS ---

                
 10D2 A59C      VSUM0:  LDA     MPCL            ; END OF Z-CODE YET?
 10D4 C58E              CMP     I+LO            ; CHECK LSB
 10D6 D0EE              BNE     VSUM
                
 10D8 A59D              LDA     MPCM            ; AND MIDDLE BYTE
 10DA C58F              CMP     I+HI            ; (HIGH BIT NEEDN'T BE CHECKED)
 10DC D0E8              BNE     VSUM
                
 10DE                   ; CHECKSUM "PURE" CODE (SIDE 2)
                
 10DE 207F23            JSR     SIDE2           ; PROMPT FOR SIDE 2
                
 10E1 AD1A26            LDA     ZBEGIN+ZLENTH   ; GET MSB
 10E4 858F              STA     I+HI            ; AND
 10E6 AD1B26            LDA     ZBEGIN+ZLENTH+1 ; LSB OF Z-CODE LENGTH IN BYTES
 10E9 0A                ASL     A               ; MULTIPLY BY
 10EA 858E              STA     I+LO            ; TWO
 10EC 268F              ROL     I+HI            ; TO GET # BYTES
 10EE 2692              ROL     K+LO            ; IN GAME
                
 10F0 204F18    VSUM2:  JSR     GETBYT          ; GET A Z-BYTE INTO [A]
 10F3 18                CLC
 10F4 6590              ADC     J+LO            ; ADD IT TO SUM
 10F6 8590              STA     J+LO            ; IN [J]
 10F8 9002              BCC     VSUM3
 10FA E691              INC     J+HI
                
 10FC A59C      VSUM3:  LDA     MPCL            ; END OF Z-CODE YET?
 10FE C58E              CMP     I+LO            ; CHECK LSB
 1100 D0EE              BNE     VSUM2
                
 1102 A59D              LDA     MPCM            ; MIDDLE BYTE
 1104 C58F              CMP     I+HI
 1106 D0E8              BNE     VSUM2
                
 1108 A59E              LDA     MPCH            ; AND HIGH BIT
 110A C592              CMP     K+LO
 110C D0E2              BNE     VSUM2
                
 110E A9A4              LDA     #ZPURE          ; UNPATCH "GETBYT"
 1110 8D5A18            STA     PATCH
                
 1113 AD1D26            LDA     ZBEGIN+ZCHKSM+1 ; GET LSB OF CHECKSUM
 1116 C590              CMP     J+LO            ; DOES IT MATCH?
 1118 D00A              BNE     BADVER          ; NO, PREDICATE FAILS
                
 111A AD1C26            LDA     ZBEGIN+ZCHKSM   ; ELSE CHECK MSB
 111D C591              CMP     J+HI            ; LOOK GOOD?
 111F D003              BNE     BADVER          ; IF MATCHED,
 1121 4C540F            JMP     PREDS           ; GAME IS OKAY


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  32    
--- 0-OPS ---

                
 1124 4C480F    BADVER: JMP     PREDF
                
                        END
                        INCLUD  OPS1.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  33    
--- 1-OPS ---

                
 1127                   ; -----
 1127                   ; ZERO?
 1127                   ; -----
                
 1127                   ; [ARG1] = 0?
                
 1127 A582      ZZERO:  LDA     ARG1+LO
 1129 0583              ORA     ARG1+HI
 112B F01C              BEQ     PFINE
                
 112D 4C480F    PYUCK:  JMP     PREDF
                
 1130                   ; -----
 1130                   ; NEXT?
 1130                   ; -----
                
 1130                   ; RETURN "NEXT" POINTER IN OBJECT [ARG1];
 1130                   ; FAIL IF LAST AND RETURN ZERO
                
 1130 A582      ZNEXT:  LDA     ARG1+LO
 1132 204D1B            JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
 1135 A005              LDY     #5              ; POINT TO "NEXT" SLOT
 1137 D007              BNE     FIRST1
                
 1139                   ; ------
 1139                   ; FIRST?
 1139                   ; ------
                
 1139                   ; RETURN "FIRST" POINTER IN OBJECT [ARG1];
 1139                   ; FAIL IF LAST AND RETURN ZERO
                
 1139 A582      ZFIRST: LDA     ARG1+LO
 113B 204D1B            JSR     OBJLOC          ; GET OBJECT ADDR INTO [I]
 113E A006              LDY     #6              ; POINT TO "FIRST" SLOT
                
 1140 B18E      FIRST1: LDA     (I),Y           ; GET CONTENTS OF SLOT
 1142 20060F            JSR     PUTBYT          ; PASS IT TO VARIABLE
                
 1145 A58C              LDA     VALUE+LO        ; EXAMINE THE VALUE JUST "PUT"
 1147 F0E4              BEQ     PYUCK           ; FAIL IF IT WAS ZERO
                
 1149 4C540F    PFINE:  JMP     PREDS           ; ELSE REJOICE
                
 114C                   ; ---
 114C                   ; LOC
 114C                   ; ---
                
 114C                   ; RETURN THE OBJECT CONTAINING OBJECT [ARG1];
 114C                   ; RETURN ZERO IF NONE
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  34    
--- 1-OPS ---

 114C A582      ZLOC:   LDA     ARG1+LO
 114E 204D1B            JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
 1151 A004              LDY     #4              ; POINT TO "LOC" SLOT
 1153 B18E              LDA     (I),Y           ; GET THE BYTE
 1155 4C060F            JMP     PUTBYT          ; AND SHIP IT OUT
                
 1158                   ; ------
 1158                   ; PTSIZE
 1158                   ; ------
                
 1158                   ; RETURN LENGTH OF PROP TABLE [ARG1] IN BYTES
                
 1158 A583      ZPTSIZ: LDA     ARG1+HI         ; MOVE ABS ADDR OF
 115A 18                CLC                     ; THE PROP TABLE
 115B 65A3              ADC     ZCODE           ; INTO [I]
 115D 858F              STA     I+HI
                
 115F A582              LDA     ARG1+LO         ; DECREMENT THE
 1161 38                SEC                     ; ADDRESS
 1162 E901              SBC     #1              ; WHILE MOVING LSB
 1164 858E              STA     I+LO
 1166 B002              BCS     PTZ0
 1168 C68F              DEC     I+HI
                
 116A A000      PTZ0:   LDY     #0              ; GET THE LENGTH
 116C 20971B            JSR     PROPL           ; OF PROPERTY AT [I] INTO [A]
                
 116F 18                CLC
 1170 6901              ADC     #1              ; INCREMENT RESULT
 1172 4C060F            JMP     PUTBYT          ; AND RETURN IT
                
 1175                   ; ---
 1175                   ; INC
 1175                   ; ---
                
 1175                   ; INCREMENT VARIABLE [ARG1]
                
 1175 A582      ZINC:   LDA     ARG1+LO
 1177 209F0E            JSR     VARGET          ; FETCH VARIABLE INTO [VALUE]
 117A 20D40F            JSR     INCVAL          ; INCREMENT IT
 117D 4C8811            JMP     ZD0
                
 1180                   ; ---
 1180                   ; DEC
 1180                   ; ---
                
 1180                   ; DECREMENT VARIABLE [ARG1]
                
 1180 A582      ZDEC:   LDA     ARG1+LO
 1182 209F0E            JSR     VARGET          ; FETCH VAR INTO [VALUE]
 1185 20C80F            JSR     DECVAL          ; DECREMENT IT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  35    
--- 1-OPS ---

                
 1188 A582      ZD0:    LDA     ARG1+LO         ; PUT RESULT BACK
 118A 4CFB0E            JMP     VARPUT          ; INTO THE SAME VARIABLE
                
 118D                   ; ------
 118D                   ; PRINTB
 118D                   ; ------
                
 118D                   ; PRINT Z-STRING AT [ARG1]
                
 118D A582      ZPRB:   LDA     ARG1+LO
 118F 858E              STA     I+LO
 1191 A583              LDA     ARG1+HI
 1193 858F              STA     I+HI
                
 1195 201619            JSR     SETWRD          ; MOVE Z-ADDR TO [MPC]
 1198 4C4219            JMP     PZSTR           ; AND PRINT
                
 119B                   ; ------
 119B                   ; REMOVE
 119B                   ; ------
                
 119B                   ; MOVE OBJECT [ARG1] INTO PSEUDO-OBJECT #0
                
 119B A582      ZREMOV: LDA     ARG1+LO         ; GET SOURCE OBJECT ADDR
 119D 204D1B            JSR     OBJLOC          ; INTO [I]
                
 11A0 A58E              LDA     I+LO            ; COPY THE SOURCE ADDR
 11A2 8590              STA     J+LO            ; INTO [J]
 11A4 A58F              LDA     I+HI            ; FOR LATER REFERENCE
 11A6 8591              STA     J+HI
                
 11A8 A004              LDY     #4              ; POINT TO "LOC" SLOT
 11AA B18E              LDA     (I),Y           ; GET THE DATA
 11AC F02E              BEQ     REMVEX          ; SCRAM IF NO OBJECT
                
 11AE 204D1B            JSR     OBJLOC          ; ELSE GET ADDR OF OBJECT [A] INTO [I]
 11B1 A006              LDY     #6              ; POINT TO "FIRST" SLOT
 11B3 B18E              LDA     (I),Y           ; GRAB DATA
 11B5 C582              CMP     ARG1+LO         ; IS THIS THE FIRST?
 11B7 D009              BNE     REMVC1          ; NO, KEEP SEARCHING
                
 11B9 A005              LDY     #5              ; ELSE COPY SOURCE'S "NEXT" SLOT
 11BB B190              LDA     (J),Y
 11BD C8                INY                     ; INTO DEST'S "FIRST" SLOT ([Y] = 6)
 11BE 918E              STA     (I),Y
 11C0 D011              BNE     REMVC2          ; BRANCH ALWAYS
                
 11C2 204D1B    REMVC1: JSR     OBJLOC
 11C5 A005              LDY     #5              ; GET "NEXT"
 11C7 B18E              LDA     (I),Y


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  36    
--- 1-OPS ---

 11C9 C582              CMP     ARG1+LO         ; FOUND IT?
 11CB D0F5              BNE     REMVC1          ; NO, KEEP TRYING
                
 11CD A005              LDY     #5              ; WHEN FOUND
 11CF B190              LDA     (J),Y           ; MOVE "NEXT" SLOT OF SOURCE
 11D1 918E              STA     (I),Y           ; TO "NEXT" SLOT OF DEST
                
 11D3 A900      REMVC2: LDA     #0
 11D5 A004              LDY     #4              ; CLEAR "LOC"
 11D7 9190              STA     (J),Y
 11D9 C8                INY                     ; AND "NEXT" SLOTS ([Y] = 5)
 11DA 9190              STA     (J),Y           ; OF SOURCE OBJECT
                
 11DC 60        REMVEX: RTS
                
 11DD                   ; ------
 11DD                   ; PRINTD
 11DD                   ; ------
                
 11DD                   ; PRINT SHORT DESCRIPTION OF OBJECT [ARG1]
                
 11DD A582      ZPRD:   LDA     ARG1+LO
                
 11DF                   ; ENTRY POINT FOR "USL"
                
 11DF 204D1B    PRNTDC: JSR     OBJLOC          ; GET ADDR OF OBJECT INTO [I]
 11E2 A007              LDY     #7              ; GET PROP TABLE POINTER
 11E4 B18E              LDA     (I),Y           ; FETCH MSB
 11E6 AA                TAX                     ; SAVE IT HERE
 11E7 C8                INY
 11E8 B18E              LDA     (I),Y           ; FETCH LSB
 11EA 858E              STA     I+LO            ; STORE LSB
 11EC 868F              STX     I+HI            ; AND MSB
                
 11EE E68E              INC     I+LO            ; POINT PAST THE
 11F0 D002              BNE     PDC0            ; LENGTH BYTE
 11F2 E68F              INC     I+HI
                
 11F4 201619    PDC0:   JSR     SETWRD          ; CALC Z-STRING ADDR
 11F7 4C4219            JMP     PZSTR           ; AND PRINT IT
                
 11FA                   ; ------
 11FA                   ; RETURN
 11FA                   ; ------
                
 11FA                   ; RETURN FROM "CALL" WITH VALUE [ARG1]
                
 11FA A595      ZRET:   LDA     OLDZSP          ; RE-SYNC THE
 11FC 8594              STA     ZSP             ; Z-STACK POINTER
                
 11FE 20CE0E            JSR     POPVAL          ; POP # LOCALS INTO [X/A]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  37    
--- 1-OPS ---

 1201 868F              STX     I+HI            ; SAVE HERE
 1203 8A                TXA                     ; SET FLAGS; ANY LOCALS?
 1204 F019              BEQ     RET2            ; SKIP IF NOT
                
 1206                   ; RESTORE PUSHED LOCALS
                
 1206 CA                DEX                     ; ZERO-ALIGN
 1207 8A                TXA                     ; AND
 1208 0A                ASL     A               ; WORD-ALIGN # LOCALS
 1209 858E              STA     I+LO            ; FOR USE AS A STORAGE INDEX
                
 120B 20CE0E    RET1:   JSR     POPVAL          ; POP A LOCAL INTO [X/A]
                
 120E A48E              LDY     I+LO            ; RETRIEVE STORAGE INDEX
 1210 99010A            STA     LOCALS+HI,Y     ; STORE MSB OF LOCAL
 1213 8A                TXA                     ; MOVE LSB
 1214 99000A            STA     LOCALS+LO,Y     ; AND STORE THAT TOO
                
 1217 C68E              DEC     I+LO
 1219 C68E              DEC     I+LO            ; UPDATE STORAGE INDEX
                
 121B C68F              DEC     I+HI            ; AND LOCALS COUNT
 121D D0EC              BNE     RET1            ; POP TILL NO MORE LOCALS
                
 121F                   ; RESTORE OTHER VARIABLES
                
 121F 20CE0E    RET2:   JSR     POPVAL          ; POP [ZPCH] AND [ZPCM]
 1222 8697              STX     ZPCM
 1224 8598              STA     ZPCH
                
 1226 20CE0E            JSR     POPVAL          ; POP AND RESTORE
 1229 8695              STX     OLDZSP
 122B 8596              STA     ZPCL
                
 122D A900              LDA     #0
 122F 8599              STA     ZPCFLG          ; ZPC CHANGED!
                
 1231 20DB0F            JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 1234 4C0C0F            JMP     PUTVAL          ; AND RETURN IT
                
 1237                   ; ----
 1237                   ; JUMP
 1237                   ; ----
                
 1237                   ; JUMP TO Z-LOCATION IN [ARG1]
                
 1237 20DB0F    ZJUMP:  JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 123A 4C8E0F            JMP     PREDB3          ; A BRANCH THAT ALWAYS SUCCEEDS
                
 123D                   ; -----
 123D                   ; PRINT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  38    
--- 1-OPS ---

 123D                   ; -----
                
 123D                   ; PRINT Z-STRING AT WORD POINTER [ARG1]
                
 123D A582      ZPRINT: LDA     ARG1+LO
 123F 858E              STA     I+LO
 1241 A583              LDA     ARG1+HI
 1243 858F              STA     I+HI
                
 1245 203019            JSR     SETSTR          ; CALC STRING ADDRESS
 1248 4C4219            JMP     PZSTR           ; AND PRINT IT
                
 124B                   ; -----
 124B                   ; VALUE
 124B                   ; -----
                
 124B                   ; RETURN VALUE OF VARIABLE [ARG1]
                
 124B A582      ZVALUE: LDA     ARG1+LO
 124D 209F0E            JSR     VARGET          ; GET THE VALUE
 1250 4C0C0F            JMP     PUTVAL          ; EASY ENOUGH
                
 1253                   ; ----
 1253                   ; BCOM
 1253                   ; ----
                
 1253                   ; COMPLEMENT [ARG1]
                
 1253 A582      ZBCOM:  LDA     ARG1+LO
 1255 49FF              EOR     #$FF
 1257 AA                TAX
 1258 A583              LDA     ARG1+HI
 125A 49FF              EOR     #$FF
                
 125C                   ; FALL THROUGH ...
                
 125C                   ; ---------------------
 125C                   ; RETURN VALUE IN [X/A]
 125C                   ; ---------------------
                
 125C 868C      VEXIT:  STX     VALUE+LO
 125E 858D              STA     VALUE+HI
 1260 4C0C0F            JMP     PUTVAL
                
                        END
                        INCLUD  OPS2.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  39    
--- 2-OPS ---

                
 1263                   ; -----
 1263                   ; LESS?
 1263                   ; -----
                
 1263                   ; [ARG1] < [ARG2]?
                
 1263 20DB0F    ZLESS:  JSR     A12VAL          ; MOVE [ARG1] TO [VALUE]
 1266 4C6C12            JMP     DLS0            ; MOVE [ARG2] TO [I] & COMPARE
                
 1269                   ; ------
 1269                   ; DLESS?
 1269                   ; ------
                
 1269                   ; DECREMENT [ARG1]; SUCCEED IF < [ARG2]
                
 1269 208011    ZDLESS: JSR     ZDEC            ; MOVES ([ARG1]-1) TO [VALUE]
                
 126C A584      DLS0:   LDA     ARG2+LO         ; MOVE [ARG2] TO [I]
 126E 858E              STA     I+LO
 1270 A585              LDA     ARG2+HI
 1272 858F              STA     I+HI
                
 1274 4C9512            JMP     COMPAR          ; COMPARE & RETURN
                
 1277                   ; -----
 1277                   ; GRTR?
 1277                   ; -----
                
 1277                   ; [ARG1] > [ARG2]?
                
 1277 A582      ZGRTR:  LDA     ARG1+LO         ; MOVE [ARG1] TO [I]
 1279 858E              STA     I+LO
 127B A583              LDA     ARG1+HI
 127D 858F              STA     I+HI
                
 127F 4C8D12            JMP     A2VAL           ; MOVE [ARG2] TO [VALUE] & COMPARE
                
 1282                   ; ------
 1282                   ; IGRTR?
 1282                   ; ------
                
 1282                   ; INCREMENT [ARG1]; SUCCEED IF GREATER THAN [ARG2]
                
 1282 207511    ZIGRTR: JSR     ZINC            ; GET ([ARG1]+1) INTO [VALUE]
                
 1285 A58C              LDA     VALUE+LO        ; MOVE [VALUE] TO [I]
 1287 858E              STA     I+LO
 1289 A58D              LDA     VALUE+HI
 128B 858F              STA     I+HI
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  40    
--- 2-OPS ---

 128D A584      A2VAL:  LDA     ARG2+LO         ; MOVE [ARG2] TO [VALUE]
 128F 858C              STA     VALUE+LO
 1291 A585              LDA     ARG2+HI
 1293 858D              STA     VALUE+HI
                
 1295 209C12    COMPAR: JSR     SCOMP           ; COMPARE [VALUE] AND [I]
 1298 9038              BCC     PGOOD
 129A B023              BCS     PBAD
                
 129C                   ; -----------------
 129C                   ; SIGNED COMPARISON
 129C                   ; -----------------
                
 129C                   ; ENTRY: VALUES IN [VALUE] AND [I]
                
 129C A58F      SCOMP:  LDA     I+HI
 129E 458D              EOR     VALUE+HI
 12A0 1005              BPL     SCMP
 12A2 A58F              LDA     I+HI
 12A4 C58D              CMP     VALUE+HI
 12A6 60                RTS
                
 12A7 A58D      SCMP:   LDA     VALUE+HI
 12A9 C58F              CMP     I+HI
 12AB D004              BNE     SCEX
 12AD A58C              LDA     VALUE+LO
 12AF C58E              CMP     I+LO
 12B1 60        SCEX:   RTS
                
 12B2                   ; ---
 12B2                   ; IN?
 12B2                   ; ---
                
 12B2                   ; IS OBJECT [ARG1] CONTAINED IN OBJECT [ARG2]?
                
 12B2 A582      ZIN:    LDA     ARG1+LO
 12B4 204D1B            JSR     OBJLOC          ; GET ADDR OF TARGET OBJECT INTO [I]
                
 12B7 A004              LDY     #4              ; POINT TO "LOC" SLOT
 12B9 B18E              LDA     (I),Y           ; GET DATA
 12BB C584              CMP     ARG2+LO         ; IS IT THERE?
 12BD F013              BEQ     PGOOD           ; YES, SUCCEED
                
 12BF 4C480F    PBAD:   JMP     PREDF           ; TOO BAD, CHUM ...
                
 12C2                   ; ----
 12C2                   ; BTST
 12C2                   ; ----
                
 12C2                   ; IS EVERY "ON" BIT IN [ARG1]
 12C2                   ; ALSO "ON" IN [ARG2]?


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  41    
--- 2-OPS ---

                
 12C2 A584      ZBTST:  LDA     ARG2+LO         ; FIRST CHECK LSBS
 12C4 2582              AND     ARG1+LO
 12C6 C584              CMP     ARG2+LO         ; LSBS MATCH?
 12C8 D0F5              BNE     PBAD            ; NO, EXIT NOW
                
 12CA A585              LDA     ARG2+HI         ; ELSE CHECK MSBS
 12CC 2583              AND     ARG1+HI
 12CE C585              CMP     ARG2+HI         ; MATCHED?
 12D0 D0ED              BNE     PBAD            ; SORRY ...
                
 12D2 4C540F    PGOOD:  JMP     PREDS
                
 12D5                   ; ---
 12D5                   ; BOR
 12D5                   ; ---
                
 12D5                   ; RETURN [ARG1] "OR" [ARG2]
                
 12D5 A582      ZBOR:   LDA     ARG1+LO
 12D7 0584              ORA     ARG2+LO
 12D9 AA                TAX
 12DA A583              LDA     ARG1+HI
 12DC 0585              ORA     ARG2+HI
 12DE 4C5C12            JMP     VEXIT
                
 12E1                   ; ----
 12E1                   ; BAND
 12E1                   ; ----
                
 12E1                   ; RETURN [ARG1] "AND" [ARG2]
                
 12E1 A582      ZBAND:  LDA     ARG1+LO
 12E3 2584              AND     ARG2+LO
 12E5 AA                TAX
 12E6 A583              LDA     ARG1+HI
 12E8 2585              AND     ARG2+HI
 12EA 4C5C12            JMP     VEXIT
                
 12ED                   ; -----
 12ED                   ; FSET?
 12ED                   ; -----
                
 12ED                   ; IS FLAG [ARG1] SET IN OBJECT [ARG2]?
                
 12ED 20A91B    ZFSETP: JSR     FLAGSU          ; GET BITS INTO [K] AND [J]
 12F0 A593              LDA     K+HI            ; DO MSBS
 12F2 2591              AND     J+HI
 12F4 8593              STA     K+HI
                
 12F6 A592              LDA     K+LO            ; DO LSBS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  42    
--- 2-OPS ---

 12F8 2590              AND     J+LO
                
 12FA 0593              ORA     K+HI            ; ANY BITS ON?
 12FC D0D4              BNE     PGOOD           ; TARGET BIT MUST BE ON
 12FE 4C480F            JMP     PREDF
                
 1301                   ; ----
 1301                   ; FSET
 1301                   ; ----
                
 1301                   ; SET FLAG [ARG2] IN OBJECT [ARG1]
                
 1301 20A91B    ZFSET:  JSR     FLAGSU          ; GET BITS INTO [K] & [J], ADDR IN [I]
                
 1304 A000              LDY     #0
 1306 A593              LDA     K+HI            ; FIRST DO MSBS
 1308 0591              ORA     J+HI
 130A 918E              STA     (I),Y
                
 130C C8                INY
 130D A592              LDA     K+LO            ; THEN LSBS
 130F 0590              ORA     J+LO
 1311 918E              STA     (I),Y
 1313 60                RTS
                
 1314                   ; ------
 1314                   ; FCLEAR
 1314                   ; ------
                
 1314                   ; CLEAR FLAG [ARG2] IN OBJECT [ARG1]
                
 1314 20A91B    ZFCLR:  JSR     FLAGSU          ; GETS BITS INTO [J] & [K], ADDR IN [I]
                
 1317 A000              LDY     #0
 1319 A591              LDA     J+HI            ; FETCH MSB
 131B 49FF              EOR     #$FF            ; COMPLEMENT IT
 131D 2593              AND     K+HI            ; RUB OUT FLAG
 131F 918E              STA     (I),Y
                
 1321 C8                INY
 1322 A590              LDA     J+LO            ; SAME FOR LSB
 1324 49FF              EOR     #$FF
 1326 2592              AND     K+LO
 1328 918E              STA     (I),Y
 132A 60                RTS
                
 132B                   ; ---
 132B                   ; SET
 132B                   ; ---
                
 132B                   ; SET VARIABLE [ARG1] EQUAL TO [ARG2]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  43    
--- 2-OPS ---

                
 132B A584      ZSET:   LDA     ARG2+LO         ; MOVE THE VALUE
 132D 858C              STA     VALUE+LO        ; INTO [VALUE]
 132F A585              LDA     ARG2+HI
 1331 858D              STA     VALUE+HI
                
 1333 A582              LDA     ARG1+LO         ; GET VARIABLE ID
 1335 4CFB0E            JMP     VARPUT          ; AND CHANGE THE VARIABLE
                
 1338                   ; ----
 1338                   ; MOVE
 1338                   ; ----
                
 1338                   ; MOVE OBJECT [ARG1] INTO OBJECT [ARG2]
                
 1338 209B11    ZMOVE:  JSR     ZREMOV          ; REMOVE FIRST
                
 133B A582              LDA     ARG1+LO
 133D 204D1B            JSR     OBJLOC          ; GET SOURCE OBJECT ADDR INTO [I]
                
 1340 A58E              LDA     I+LO            ; COPY SOURCE ADDRESS
 1342 8590              STA     J+LO            ; INTO [J]
 1344 A58F              LDA     I+HI
 1346 8591              STA     J+HI
                
 1348 A584              LDA     ARG2+LO         ; GET DEST OBJECT ID
 134A A004              LDY     #4              ; POINT TO "LOC" SLOT OF SOURCE
 134C 918E              STA     (I),Y           ; AND MOVE IT IN
                
 134E 204D1B            JSR     OBJLOC          ; GET ADDR OF DEST OBJECT INTO [I]
                
 1351 A006              LDY     #6              ; POINT TO "FIRST" SLOT
 1353 B18E              LDA     (I),Y           ; GET "FIRST" OF DEST
 1355 AA                TAX                     ; SAVE HERE FOR A MOMENT
                
 1356 A582              LDA     ARG1+LO         ; GET SOURCE OBJECT ID
 1358 918E              STA     (I),Y           ; MAKE IT "FIRST" OF DEST
                
 135A 8A                TXA                     ; RESTORE "FIRST" OF DEST
 135B F004              BEQ     ZMVEX           ; SCRAM IF ZERO
                
 135D A005              LDY     #5              ; MAKE "FIRST" OF DEST
 135F 9190              STA     (J),Y           ; THE "NEXT" OF SOURCE
                
 1361 60        ZMVEX:  RTS
                
 1362                   ; ---
 1362                   ; GET
 1362                   ; ---
                
 1362                   ; RETURN ITEM [ARG2] IN WORD-TABLE [ARG1]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  44    
--- 2-OPS ---

                
 1362 207713    ZGET:   JSR     WCALC           ; CALC ADDRESS
 1365 204F18            JSR     GETBYT          ; GET 1ST BYTE (MSB)
                
 1368 858D      DOGET:  STA     VALUE+HI        ; SAVE MSB
 136A 204F18            JSR     GETBYT          ; GET LSB
 136D 858C              STA     VALUE+LO        ; SAVE AND
 136F 4C0C0F            JMP     PUTVAL          ; HAND IT OVER
                
 1372                   ; ----
 1372                   ; GETB
 1372                   ; ----
                
 1372                   ; RETURN ITEM [ARG2] IN BYTE-TABLE AT [ARG1]
                
 1372 207B13    ZGETB:  JSR     BCALC
 1375 F0F1              BEQ     DOGET           ; [A] = 0, SO CLEAR MSB OF [VALUE]
                
 1377                   ; --------------------
 1377                   ; CALC TABLE ADDRESSES
 1377                   ; --------------------
                
 1377                   ; WORD-ALIGNED ENTRY
                
 1377 0684      WCALC:  ASL     ARG2+LO         ; WORD-ALIGN FOR
 1379 2685              ROL     ARG2+HI         ; WORD ACCESS
                
 137B                   ; BYTE-ALIGNED ENTRY
                
 137B A584      BCALC:  LDA     ARG2+LO         ; ADD BASE ADDR OF TABLE
 137D 18                CLC                     ; TO ITEM
 137E 6582              ADC     ARG1+LO         ; INDEX
 1380 859C              STA     MPCL
                
 1382 A585              LDA     ARG2+HI         ; SAME FOR MSBS
 1384 6583              ADC     ARG1+HI
 1386 859D              STA     MPCM
                
 1388 A900              LDA     #0
 138A 859E              STA     MPCH            ; CLEAR TOP BIT
 138C 859F              STA     MPCFLG          ; & INVALIDATE [MPC]
 138E 60                RTS
                
 138F                   ; ----
 138F                   ; GETP
 138F                   ; ----
                
 138F                   ; RETURN PROPERTY [ARG2] OF OBJECT [ARG1];
 138F                   ; IF NO PROP [ARG2], RETURN [ARG2]'TH ELEMENT OF OBJECT #0
                
 138F 20761B    ZGETP:  JSR     PROPB


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  45    
--- 2-OPS ---

                
 1392 20921B    GETP1:  JSR     PROPN
 1395 C584              CMP     ARG2+LO
 1397 F01B              BEQ     GETP3
 1399 9006              BCC     GETP2
                
 139B 209F1B            JSR     PROPNX
 139E 4C9213            JMP     GETP1           ; TRY AGAIN WITH NEXT PROP
                
 13A1 A584      GETP2:  LDA     ARG2+LO         ; GET PROPERTY #
 13A3 38                SEC                     ; ZERO-ALIGN IT
 13A4 E901              SBC     #1
 13A6 0A                ASL     A               ; WORD-ALIGN IT
 13A7 A8                TAY                     ; USE AS AN INDEX
 13A8 B1B2              LDA     (OBJTAB),Y      ; GET MSB OF PROPERTY
 13AA 858D              STA     VALUE+HI
 13AC C8                INY
 13AD B1B2              LDA     (OBJTAB),Y      ; DO SAME WITH LSB
 13AF 858C              STA     VALUE+LO
 13B1 4C0C0F            JMP     PUTVAL          ; RETURN DEFAULT IN [VALUE]
                
 13B4 20971B    GETP3:  JSR     PROPL
 13B7 C8                INY                     ; MAKE [Y] POINT TO 1ST BYTE OF PROP
 13B8 AA                TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
 13B9 F009              BEQ     GETPB           ; GET A BYTE PROPERTY
 13BB C901              CMP     #1              ; IF LENGTH = 1
 13BD F00B              BEQ     GETPW           ; GET A WORD PROPERTY
                
 13BF                   ; *** ERROR #7: PROPERTY LENGTH ***
                
 13BF A907              LDA     #7
 13C1 4CF91B            JMP     ZERROR
                
 13C4                   ; GET A 1-BYTE PROPERTY
                
 13C4 B18E      GETPB:  LDA     (I),Y           ; GET LSB INTO [A]
 13C6 A200              LDX     #0              ; CLEAR MSB IN [X]
 13C8 F006              BEQ     ETPEX
                
 13CA                   ; GET A 2-BYTE PROPERTY
                
 13CA B18E      GETPW:  LDA     (I),Y           ; GET MSB
 13CC AA                TAX                     ; INTO [X]
 13CD C8                INY                     ; POINT TO LSB
 13CE B18E              LDA     (I),Y           ; GET IT INTO [A]
                
 13D0 858C      ETPEX:  STA     VALUE+LO        ; STORE LSB
 13D2 868D              STX     VALUE+HI        ; AND MSB
 13D4 4C0C0F            JMP     PUTVAL
                
 13D7                   ; -----


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  46    
--- 2-OPS ---

 13D7                   ; GETPT
 13D7                   ; -----
                
 13D7                   ; RETURN POINTER TO PROP TABLE [ARG2]
 13D7                   ; IN OBJECT [ARG1]
                
 13D7 20761B    ZGETPT: JSR     PROPB
                
 13DA 20921B    GETPT1: JSR     PROPN           ; RETURNS OFFSET IN [Y]
 13DD C584              CMP     ARG2+LO
 13DF F008              BEQ     GETPT2
 13E1 901E              BCC     DORET
 13E3 209F1B            JSR     PROPNX          ; TRY NEXT PROPERTY
 13E6 4CDA13            JMP     GETPT1
                
 13E9 E68E      GETPT2: INC     I+LO
 13EB D002              BNE     GETPT3
 13ED E68F              INC     I+HI
                
 13EF 98        GETPT3: TYA                     ; FETCH OFFSET
 13F0 18                CLC
 13F1 658E              ADC     I+LO            ; ADD LSB OF TABLE ADDRESS
 13F3 858C              STA     VALUE+LO
                
 13F5 A58F              LDA     I+HI            ; AND MSB
 13F7 6900              ADC     #0
 13F9 38                SEC                     ; STRIP OFF
 13FA E5A3              SBC     ZCODE           ; RELATIVE POINTER
 13FC 858D              STA     VALUE+HI
 13FE 4C0C0F            JMP     PUTVAL          ; AND RETURN
                
 1401 4C040F    DORET:  JMP     RET0            ; ELSE RETURN A ZERO
                
 1404                   ; -----
 1404                   ; NEXTP
 1404                   ; -----
                
 1404                   ; RETURN INDEX # OF PROP FOLLOWING PROP [ARG2] IN OBJECT [ARG1];
 1404                   ; RETURN ZERO IF LAST; RETURN FIRST IF [ARG2]=0; ERROR IF NONE
                
 1404 20761B    ZNEXTP: JSR     PROPB
 1407 A584              LDA     ARG2+LO         ; IF [ARG2]=0
 1409 F012              BEQ     NXTP3           ; RETURN "FIRST" SLOT
                
 140B 20921B    NXTP1:  JSR     PROPN           ; FETCH PROPERTY #
 140E C584              CMP     ARG2+LO         ; COMPARE TO TARGET #
 1410 F008              BEQ     NXTP2           ; FOUND IT!
 1412 90ED              BCC     DORET           ; LAST PROP, SO RETURN ZERO
 1414 209F1B            JSR     PROPNX          ; ELSE TRY NEXT PROPERTY
 1417 4C0B14            JMP     NXTP1
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  47    
--- 2-OPS ---

 141A 209F1B    NXTP2:  JSR     PROPNX          ; POINT TO FOLLOWING PROPERTY
                
 141D 20921B    NXTP3:  JSR     PROPN           ; GET THE PROPERTY #
 1420 4C060F            JMP     PUTBYT          ; AND RETURN IT
                
 1423                   ; ---
 1423                   ; ADD
 1423                   ; ---
                
 1423                   ; RETURN [ARG1] + [ARG2]
                
 1423 A582      ZADD:   LDA     ARG1+LO         ; ADD LSBS
 1425 18                CLC
 1426 6584              ADC     ARG2+LO
 1428 AA                TAX                     ; SAVE LSB HERE
 1429 A583              LDA     ARG1+HI         ; ADD MSBS
 142B 6585              ADC     ARG2+HI
 142D 4C5C12            JMP     VEXIT
                
 1430                   ; ---
 1430                   ; SUB
 1430                   ; ---
                
 1430                   ; RETURN [ARG1] - [ARG2]
                
 1430 A582      ZSUB:   LDA     ARG1+LO         ; SUBTRACT LSBS
 1432 38                SEC
 1433 E584              SBC     ARG2+LO
 1435 AA                TAX                     ; SAVE LSB HERE
 1436 A583              LDA     ARG1+HI         ; SUBTRACT MSBS
 1438 E585              SBC     ARG2+HI
 143A 4C5C12            JMP     VEXIT           ; EXIT WITH [X]=LSB, [A]=MSB
                
 143D                   ; ---
 143D                   ; MUL
 143D                   ; ---
                
 143D                   ; RETURN [ARG1] * [ARG2]
                
 143D 20F714    ZMUL:   JSR     MINIT           ; INIT THINGS
                
 1440 66D8      ZMLOOP: ROR     MTEMP+HI
 1442 66D7              ROR     MTEMP+LO
 1444 6685              ROR     ARG2+HI
 1446 6684              ROR     ARG2+LO
 1448 900D              BCC     ZMNEXT
                
 144A A582              LDA     ARG1+LO
 144C 18                CLC
 144D 65D7              ADC     MTEMP+LO
 144F 85D7              STA     MTEMP+LO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  48    
--- 2-OPS ---

 1451 A583              LDA     ARG1+HI
 1453 65D8              ADC     MTEMP+HI
 1455 85D8              STA     MTEMP+HI
                
 1457 CA        ZMNEXT: DEX
 1458 10E6              BPL     ZMLOOP
                
 145A A684              LDX     ARG2+LO         ; PUT LSB OF PRODUCT
 145C A585              LDA     ARG2+HI         ; AND MSB
 145E 4C5C12            JMP     VEXIT           ; WHERE "VEXIT" EXPECTS THEM
                
 1461                   ; ---
 1461                   ; DIV
 1461                   ; ---
                
 1461                   ; RETURN QUOTIENT OF [ARG1] / [ARG2]
                
 1461 207514    ZDIV:   JSR     DIVIDE
 1464 A6D3              LDX     QUOT+LO
 1466 A5D4              LDA     QUOT+HI
 1468 4C5C12            JMP     VEXIT
                
 146B                   ; ---
 146B                   ; MOD
 146B                   ; ---
                
 146B                   ; RETURN REMAINDER OF [ARG1] / [ARG2]
                
 146B 207514    ZMOD:   JSR     DIVIDE
 146E A6D5              LDX     REMAIN+LO       ; FETCH THE REMAINDER
 1470 A5D6              LDA     REMAIN+HI       ; IN [REMAIN]
 1472 4C5C12            JMP     VEXIT           ; AND RETURN IT
                
 1475                   ; ---------------
 1475                   ; SIGNED DIVISION
 1475                   ; ---------------
                
 1475                   ; ENTRY: DIVIDEND IN [ARG1], DIVISOR IN [ARG2]
 1475                   ; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]
                
 1475 A583      DIVIDE: LDA     ARG1+HI         ; SIGN OF REMAINDER
 1477 85DA              STA     RSIGN           ; IS THE SIGN OF THE DIVIDEND
 1479 4585              EOR     ARG2+HI         ; SIGN OF QUOTIENT IS POSITIVE
 147B 85D9              STA     QSIGN           ; IF SIGNS OF TERMS ARE THE SAME
                
 147D A582              LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
 147F 85D3              STA     QUOT+LO
 1481 A583              LDA     ARG1+HI
 1483 85D4              STA     QUOT+HI         ; IF DIVIDEND IS POSITIVE
 1485 1003              BPL     ABSDIV          ; MOVE DIVISOR
 1487 20B314            JSR     ABQUOT          ; ELSE CALC ABS(DIVIDEND) FIRST


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  49    
--- 2-OPS ---

                
 148A A584      ABSDIV: LDA     ARG2+LO
 148C 85D5              STA     REMAIN+LO
 148E A585              LDA     ARG2+HI
 1490 85D6              STA     REMAIN+HI       ; IF REMAINDER IS POSITIVE
 1492 1003              BPL     GODIV           ; WE'RE READY TO DIVIDE
 1494 20A514            JSR     ABREM           ; ELSE CALC ABS(DIVISOR)
                
 1497 20C114    GODIV:  JSR     UDIV            ; DO UNSIGNED DIVIDE
                
 149A A5D9              LDA     QSIGN           ; SHOULD QUOTIENT BE FLIPPED?
 149C 1003              BPL     RFLIP           ; NO, TEST REMAINDER
 149E 20B314            JSR     ABQUOT          ; ELSE GET ABSOLUTE VALUE
                
 14A1 A5DA      RFLIP:  LDA     RSIGN           ; SHOULD EMAINDER BE FLIPPED?
 14A3 100D              BPL     DIVEX           ; NO, WE'RE DONE
                
 14A5                   ; ELSE FALL THROUGH ...
                
 14A5                   ; ----------------
 14A5                   ; CALC ABS(REMAIN)
 14A5                   ; ----------------
                
 14A5 A900      ABREM:  LDA     #0
 14A7 38                SEC
 14A8 E5D5              SBC     REMAIN+LO
 14AA 85D5              STA     REMAIN+LO
 14AC A900              LDA     #0
 14AE E5D6              SBC     REMAIN+HI
 14B0 85D6              STA     REMAIN+HI
                
 14B2 60        DIVEX:  RTS
                
 14B3                   ; --------------
 14B3                   ; CALC ABS(QUOT)
 14B3                   ; --------------
                
 14B3 A900      ABQUOT: LDA     #0
 14B5 38                SEC
 14B6 E5D3              SBC     QUOT+LO
 14B8 85D3              STA     QUOT+LO
 14BA A900              LDA     #0
 14BC E5D4              SBC     QUOT+HI
 14BE 85D4              STA     QUOT+HI
 14C0 60                RTS
                
 14C1                   ; -----------------
 14C1                   ; UNSIGNED DIVISION
 14C1                   ; -----------------
                
 14C1                   ; ENTRY: DIVIDEND IN [QUOT], DIVISOR IN [REMAIN]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  50    
--- 2-OPS ---

 14C1                   ; EXIT: QUOTIENT IN [QUOT], REMAINDER IN [REMAIN]
                
 14C1 A5D5      UDIV:   LDA     REMAIN+LO       ; CHECK [REMAIN]
 14C3 05D6              ORA     REMAIN+HI       ; BEFORE PROCEEDING
 14C5 F02B              BEQ     DIVERR          ; CAN'T DIVIDE BY ZERO!
                
 14C7 20F714            JSR     MINIT           ; SET IT ALL UP
                
 14CA 26D3      UDLOOP: ROL     QUOT+LO
 14CC 26D4              ROL     QUOT+HI
 14CE 26D7              ROL     MTEMP+LO
 14D0 26D8              ROL     MTEMP+HI
                
 14D2 A5D7              LDA     MTEMP+LO
 14D4 38                SEC
 14D5 E5D5              SBC     REMAIN+LO
 14D7 A8                TAY                     ; SAVE HERE
 14D8 A5D8              LDA     MTEMP+HI
 14DA E5D6              SBC     REMAIN+HI
 14DC 9004              BCC     UDNEXT
 14DE 84D7              STY     MTEMP+LO
 14E0 85D8              STA     MTEMP+HI
                
 14E2 CA        UDNEXT: DEX
 14E3 D0E5              BNE     UDLOOP
                
 14E5 26D3              ROL     QUOT+LO         ; SHIFT LAST CARRY FOR QUOTIENT
 14E7 26D4              ROL     QUOT+HI
                
 14E9 A5D7              LDA     MTEMP+LO        ; MOVE REMAINDER
 14EB 85D5              STA     REMAIN+LO       ; INTO [REMAIN]
 14ED A5D8              LDA     MTEMP+HI
 14EF 85D6              STA     REMAIN+HI
 14F1 60                RTS
                
 14F2                   ; *** ERROR #8: DIVISION BY ZERO ***
                
 14F2 A908      DIVERR: LDA     #8
 14F4 4CF91B            JMP     ZERROR
                
 14F7                   ; ---------
 14F7                   ; MATH INIT
 14F7                   ; ---------
                
 14F7 A210      MINIT:  LDX     #16             ; INIT LOOPING INDEX
 14F9 A900              LDA     #0
 14FB 85D7              STA     MTEMP+LO        ; CLEAR TEMP
 14FD 85D8              STA     MTEMP+HI        ; REGISTER
 14FF 18                CLC                     ; AND CARRY
 1500 60                RTS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  51    
--- 2-OPS ---

                        END
                        INCLUD  OPSX.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  52    
--- X-OPS ---

                
 1501                   ; ------
 1501                   ; EQUAL?
 1501                   ; ------
                
 1501                   ; IS [ARG1] = [ARG2] (OR [ARG3] OR [ARG4])?
                
 1501 C681      ZEQUAL: DEC     NARGS           ; DOUBLE-CHECK # ARGS
 1503 D005              BNE     DOEQ            ; MUST BE AT LEAST TWO, OR ...
                
 1505                   ; *** ERROR #9: NOT ENOUGH "EQUAL?" ARGS ***
                
 1505 A909              LDA     #9
 1507 4CF91B            JMP     ZERROR
                
 150A A582      DOEQ:   LDA     ARG1+LO         ; FETCH LSB
 150C A683              LDX     ARG1+HI         ; AND MSB OF [ARG1]
                
 150E C584              CMP     ARG2+LO         ; TEST LSB OF [ARG2]
 1510 D004              BNE     TRY2            ; NO GOOD, LOOK FOR ANOTHER ARG
 1512 E485              CPX     ARG2+HI         ; ELSE TRY MSB OF [ARG2]
 1514 F018              BEQ     EQOK            ; MATCHED!
                
 1516 C681      TRY2:   DEC     NARGS           ; OUT OF ARGS YET?
 1518 F017              BEQ     EQBAD           ; YES, WE FAILED
                
 151A C586              CMP     ARG3+LO         ; TRY LSB OF [ARG3]
 151C D004              BNE     TRY3            ; NO GOOD, LOOK FOR ANOTHER ARG
 151E E487              CPX     ARG3+HI         ; HOW ABOUT MSB OF [ARG3]?
 1520 F00C              BEQ     EQOK            ; YAY!
                
 1522 C681      TRY3:   DEC     NARGS           ; OUT OF ARGS YET?
 1524 F00B              BEQ     EQBAD           ; IF NOT ...
                
 1526 C588              CMP     ARG4+LO         ; TRY [ARG4]
 1528 D007              BNE     EQBAD           ; SORRY, CHUM
 152A E489              CPX     ARG4+HI         ; MSB MATCHED?
 152C D003              BNE     EQBAD           ; TOO BAD
                
 152E 4C540F    EQOK:   JMP     PREDS           ; FINALLY MATCHED!
                
 1531 4C480F    EQBAD:  JMP     PREDF           ; FAILURE (SNIFF!)
                
 1534                   ; ----
 1534                   ; CALL
 1534                   ; ----
                
 1534                   ; BRANCH TO FUNCTION AT ([ARG1]*2), PASSING
 1534                   ; OPTIONAL PARAMETERS IN [ARG2]-[ARG4]
                
 1534 A582      ZCALL:  LDA     ARG1+LO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  53    
--- X-OPS ---

 1536 0583              ORA     ARG1+HI         ; IS CALL ADDRESS ZERO?
 1538 D003              BNE     DOCALL          ; NO, CONTINUE
                
 153A 4C060F            JMP     PUTBYT          ; ELSE RETURN THE ZERO IN [A]
                
 153D A695      DOCALL: LDX     OLDZSP          ; SAVE OLD STACK POINTER
 153F A596              LDA     ZPCL            ; AND LSB OF [ZPC]
 1541 20E80E            JSR     PUSHXA          ; ON THE Z-STACK
                
 1544 A697              LDX     ZPCM            ; SAVE MIDDLE 8 BITS
 1546 A598              LDA     ZPCH            ; AND TOP BIT OF [ZPC]
 1548 20E80E            JSR     PUSHXA          ; AS WELL
                
 154B                   ; FORM 16-BIT ADDRESS FROM [ARG1]
                
 154B A900              LDA     #0              ; CLEAR HIGH BIT FOR ROTATE
 154D 8599              STA     ZPCFLG          ; AND INVALIDATE [ZPC]
                
 154F 0682              ASL     ARG1+LO         ; MULTIPLY [ARG1]
 1551 2683              ROL     ARG1+HI         ; BY TWO
 1553 2A                ROL     A               ; HIGH BIT INTO [A]
 1554 8598              STA     ZPCH            ; NEW HIGH BIT OF [ZPC]
                
 1556 A583              LDA     ARG1+HI         ; GET NEW LOW BYTES
 1558 8597              STA     ZPCM
 155A A582              LDA     ARG1+LO
 155C 8596              STA     ZPCL
                
 155E 201918            JSR     NEXTPC          ; FETCH # LOCALS TO PASS
 1561 8590              STA     J+LO            ; SAVE HERE FOR COUNTING
 1563 8591              STA     J+HI            ; AND HERE FOR LATER REFERENCE
 1565 F02B              BEQ     ZCALL2          ; SKIP IF NO LOCALS
                
 1567 A900              LDA     #0
 1569 858E              STA     I+LO            ; ELSE INIT STORAGE INDEX
                
 156B A48E      ZCALL1: LDY     I+LO
 156D BE000A            LDX     LOCALS+LO,Y     ; GET LSB OF LOCAL INTO [X]
 1570 B9010A            LDA     LOCALS+HI,Y     ; AND MSB INTO [A]
 1573 848E              STY     I+LO            ; SAVE THE INDEX
 1575 20E80E            JSR     PUSHXA          ; PUSH LOCAL IN [X/A] ONTO Z-STACK
                
 1578 201918            JSR     NEXTPC          ; GET MSB OF NEW LOCAL
 157B 858F              STA     I+HI            ; SAVE IT HERE
 157D 201918            JSR     NEXTPC          ; NOW GET LSB
                
 1580 A48E              LDY     I+LO            ; RESTORE INDEX
 1582 99000A            STA     LOCALS+LO,Y     ; STORE LSB INTO [LOCALS]
 1585 A58F              LDA     I+HI            ; RETRIEVE MSB
 1587 99010A            STA     LOCALS+HI,Y     ; STORE IT INTO [LOCALS]
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  54    
--- X-OPS ---

 158A C8                INY
 158B C8                INY                     ; UPDATE
 158C 848E              STY     I+LO            ; THE STORAGE INDEX
                
 158E C690              DEC     J+LO            ; ANY MORE LOCALS?
 1590 D0D9              BNE     ZCALL1          ; YES, KEEP LOOPING
                
 1592                   ; MOVE UP TO 3 ARGUMENTS TO [LOCALS]
                
 1592 C681      ZCALL2: DEC     NARGS           ; EXTRA ARGS IN THIS CALL?
 1594 F026              BEQ     ZCALL3          ; NO, CONTINUE
                
 1596 A584              LDA     ARG2+LO         ; MOVE [ARG2] TO LOCAL #1
 1598 8D000A            STA     LOCALS+LO
 159B A585              LDA     ARG2+HI
 159D 8D010A            STA     LOCALS+HI
                
 15A0 C681              DEC     NARGS           ; ANY LEFT?
 15A2 F018              BEQ     ZCALL3          ; NO, SCRAM
                
 15A4 A586              LDA     ARG3+LO         ; MOVE [ARG3] TO LOCAL #2
 15A6 8D020A            STA     LOCALS+LO+2
 15A9 A587              LDA     ARG3+HI
 15AB 8D030A            STA     LOCALS+HI+2
                
 15AE C681              DEC     NARGS           ; ANY LEFT?
 15B0 F00A              BEQ     ZCALL3          ; NO, EXUENT
                
 15B2 A588              LDA     ARG4+LO         ; MOVE [ARG4] TO LOCAL #3
 15B4 8D040A            STA     LOCALS+LO+4
 15B7 A589              LDA     ARG4+HI
 15B9 8D050A            STA     LOCALS+HI+4
                
 15BC A691      ZCALL3: LDX     J+HI            ; RETRIEVE # LOCALS
 15BE 8A                TXA                     ; DUPE FOR NO GOOD REASON
 15BF 20E80E            JSR     PUSHXA          ; PUSH # LOCALS ONTO Z-STACK
                
 15C2 A594              LDA     ZSP             ; REMEMBER WHERE
 15C4 8595              STA     OLDZSP          ; WE CAME FROM
                
 15C6 60                RTS                     ; WHEW!
                
 15C7                   ; ---
 15C7                   ; PUT
 15C7                   ; ---
                
 15C7                   ; SET ITEM [ARG2] IN WORD-TABLE [ARG1] EQUAL TO [ARG3]
                
 15C7 0684      ZPUT:   ASL     ARG2+LO         ; WORD-ALIGN [ARG2]
 15C9 2685              ROL     ARG2+HI
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  55    
--- X-OPS ---

 15CB 20DD15            JSR     PCALC           ; GET ITEM ADDR INTO [I]
 15CE A587              LDA     ARG3+HI         ; STORE MSB OF [ARG3]
 15D0 918E              STA     (I),Y           ; INTO MSB OF TABLE POSITION
 15D2 C8                INY                     ; POINT TO LSB
 15D3 D003              BNE     PUTLSB          ; BRANCH ALWAYS
                
 15D5                   ; ----
 15D5                   ; PUTB
 15D5                   ; ----
                
 15D5                   ; SET ITEM [ARG2] IN BYTE-TABLE [ARG1] EQUAL TO [ARG3]
                
 15D5 20DD15    ZPUTB:  JSR     PCALC
                
 15D8                   ; ENTRY FOR "PUT"
                
 15D8 A586      PUTLSB: LDA     ARG3+LO         ; GET LSB OF [ARG3]
 15DA 918E              STA     (I),Y           ; STORE IN TABLE AT [Y]
 15DC 60                RTS
                
 15DD                   ; ---------------------------
 15DD                   ; CALC ITEM ADDRESS FOR "PUT"
 15DD                   ; ---------------------------
                
 15DD A584      PCALC:  LDA     ARG2+LO         ; ADD ITEM OFFSET IN [ARG2]
 15DF 18                CLC                     ; TO TABLE ADDR IN [ARG1]
 15E0 6582              ADC     ARG1+LO         ; TO FORM A POINTER
 15E2 858E              STA     I+LO            ; IN [I]
                
 15E4 A585              LDA     ARG2+HI         ; SAME FOR MSB
 15E6 6583              ADC     ARG1+HI
 15E8 18                CLC
 15E9 65A3              ADC     ZCODE           ; MAKE IT ABSOLUTE
 15EB 858F              STA     I+HI
                
 15ED A000              LDY     #0              ; ZERO FOR INDEXING
 15EF 60                RTS
                
 15F0                   ; ----
 15F0                   ; PUTP
 15F0                   ; ----
                
 15F0                   ; SET PROPERTY [ARG2] IN OBJECT [ARG1] EQUAL TO [ARG3]
                
 15F0 20761B    ZPUTP:  JSR     PROPB
                
 15F3 20921B    PUTP1:  JSR     PROPN
 15F6 C584              CMP     ARG2+LO
 15F8 F008              BEQ     PUTP2
 15FA 901B              BCC     PNERR           ; ERROR IF LOWER
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  56    
--- X-OPS ---

 15FC 209F1B            JSR     PROPNX          ; TRY NEXT PROPERTY
 15FF 4CF315            JMP     PUTP1
                
 1602 20971B    PUTP2:  JSR     PROPL
 1605 C8                INY                     ; MAKE [Y] POINT TO 1ST PROPERTY BYTE
 1606 AA                TAX                     ; (SET FLAGS) IF LENGTH IN [A] = 0
 1607 F009              BEQ     PUTP3           ; PUT A BYTE
 1609 C901              CMP     #1              ; PUT A WORD IF [A] = 1
 160B D00F              BNE     PLERR           ; ELSE LENGTH IS BAD
                
 160D A587              LDA     ARG3+HI         ; GET MSB OF PROPERTY
 160F 918E              STA     (I),Y           ; AND STORE IN OBJECT
 1611 C8                INY                     ; POINT TO LSB SLOT
                
 1612 A586      PUTP3:  LDA     ARG3+LO         ; FETCH LSB
 1614 918E              STA     (I),Y           ; AND STORE IN OBJECT
 1616 60                RTS
                
 1617                   ; *** ERROR #10: BAD PROPERTY NUMBER ***
                
 1617 A90A      PNERR:  LDA     #10
 1619 4CF91B            JMP     ZERROR
                
 161C                   ; *** ERROR #11: PUTP PROPERTY LENGTH ***
                
 161C A90B      PLERR:  LDA     #11
 161E 4CF91B            JMP     ZERROR
                
 1621                   ; ------
 1621                   ; PRINTC
 1621                   ; ------
                
 1621                   ; PRINT CHAR WITH ASCII VALUE IN [ARG1]
                
 1621 A582      ZPRC:   LDA     ARG1+LO         ; GRAB THE CHAR
 1623 4C891C            JMP     COUT            ; AND SHIP IT OUT
                
 1626                   ; ------
 1626                   ; PRINTN
 1626                   ; ------
                
 1626                   ; PRINT VALUE OF [ARG1] AS A SIGNED INTEGER
                
 1626 A582      ZPRN:   LDA     ARG1+LO         ; MOVE [ARG1] TO [QUOT]
 1628 85D3              STA     QUOT+LO
 162A A583              LDA     ARG1+HI
 162C 85D4              STA     QUOT+HI
                
 162E                   ; PRINT [QUOT]
                
 162E A5D4      NUMBER: LDA     QUOT+HI         ; IF VALUE IS POSITIVE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  57    
--- X-OPS ---

 1630 1008              BPL     DIGCNT          ; CONTINUE
                
 1632 A92D              LDA     #$2D            ; ELSE START WITH A MINUS SIGN
 1634 20891C            JSR     COUT
                
 1637 20B314            JSR     ABQUOT          ; AND CALC ABS([QUOT])
                
 163A                   ; COUNT # OF DECIMAL DIGITS
                
 163A A900      DIGCNT: LDA     #0              ; RESET
 163C 85DB              STA     DIGITS          ; DIGIT INDEX
                
 163E A5D3      DGC:    LDA     QUOT+LO         ; IS QUOTIENT
 1640 05D4              ORA     QUOT+HI         ; ZERO YET?
 1642 F012              BEQ     PRNTN3          ; YES, READY TO PRINT
                
 1644 A90A              LDA     #10             ; ELSE DIVIDE [QUOT]
 1646 85D5              STA     REMAIN+LO       ; BY 10 (LSB)
 1648 A900              LDA     #0
 164A 85D6              STA     REMAIN+HI       ; 10 (MSB)
                
 164C 20C114            JSR     UDIV            ; UNSIGNED DIVIDE
                
 164F A5D5              LDA     REMAIN+LO       ; FETCH LSB OF REMAINDER (THE DIGIT)
 1651 48                PHA                     ; SAVE IT ON STACK
 1652 E6DB              INC     DIGITS          ; UPDATE DIGIT COUNT
 1654 D0E8              BNE     DGC             ; LOOP TILL QUOTIENT=0
                
 1656 A5DB      PRNTN3: LDA     DIGITS          ; IF DIGIT COUNT IS NZ
 1658 D005              BNE     PRNTN4          ; CONTINUE
                
 165A A930              LDA     #'0'            ; ELSE PRINT "0"
 165C 4C891C            JMP     COUT            ; AND RETURN
                
 165F 68        PRNTN4: PLA                     ; PULL A DIGIT OFF THE STACK
 1660 18                CLC
 1661 6930              ADC     #'0'            ; CONVERT TO ASCII
 1663 20891C            JSR     COUT            ; AND PRINT IT
 1666 C6DB              DEC     DIGITS          ; OUT OF DIGITS YET?
 1668 D0F5              BNE     PRNTN4          ; NO, KEEP LOOPING
 166A 60                RTS
                
 166B                   ; ------
 166B                   ; RANDOM
 166B                   ; ------
                
 166B                   ; RETURN A RANDOM VALUE BETWEEN 0 AND [ARG1]
                
 166B A582      ZRAND:  LDA     ARG1+LO         ; MAKE [ARG1] THE DIVISOR
 166D 8584              STA     ARG2+LO
 166F A583              LDA     ARG1+HI


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  58    
--- X-OPS ---

 1671 8585              STA     ARG2+HI
                
 1673 20811C            JSR     RANDOM          ; GET RANDOM BYTES INTO [A] AND [X]
 1676 8682              STX     ARG1+LO         ; MAKE THEM THE DIVIDEND
 1678 297F              AND     #$7F            ; MAKE SURE MSB IS POSITIVE
 167A 8583              STA     ARG1+HI
                
 167C 207514            JSR     DIVIDE          ; SIGNED DIVIDE, [ARG1] / [ARG2]
                
 167F A5D5              LDA     REMAIN+LO       ; MOVE REMAINDER
 1681 858C              STA     VALUE+LO        ; INTO [VALUE]
 1683 A5D6              LDA     REMAIN+HI
 1685 858D              STA     VALUE+HI
                
 1687 20D40F            JSR     INCVAL          ; INCREMENT [VALUE]
 168A 4C0C0F            JMP     PUTVAL          ; AND RETURN RESULT
                
 168D                   ; ----
 168D                   ; PUSH
 168D                   ; ----
                
 168D                   ; PUSH [ARG1] ONTO THE Z-STACK
                
 168D A682      ZPUSH:  LDX     ARG1+LO
 168F A583              LDA     ARG1+HI
 1691 4CE80E            JMP     PUSHXA
                
 1694                   ; ---
 1694                   ; POP
 1694                   ; ---
                
 1694                   ; POP WORD OFF Z-STACK, STORE IN VARIABLE [ARG1]
                
 1694 20CE0E    ZPOP:   JSR     POPVAL          ; VALUE INTO [VALUE]
 1697 A582              LDA     ARG1+LO         ; GET VARIABLE ID
 1699 4CFB0E            JMP     VARPUT          ; AND CHANGE THE VARIABLE
                
                        END
                        INCLUD  READ.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  59    
--- READ HANDLER ---

                
 169C                   ; ----
 169C                   ; READ
 169C                   ; ----
                
 169C                   ; READ LINE INTO TABLE [ARG1]; PARSE INTO TABLE [ARG2]
                
 169C 20201D    ZREAD:  JSR     ZUSL            ; UPDATE THE STATUS LINE
                
 169F A583              LDA     ARG1+HI         ; MAKE THE TABLE ADDRESSES
 16A1 18                CLC                     ; ABSOLUTE
 16A2 65A3              ADC     ZCODE           ; LSBS NEED NOT CHANGE
 16A4 8583              STA     ARG1+HI
                
 16A6 A585              LDA     ARG2+HI
 16A8 18                CLC
 16A9 65A3              ADC     ZCODE
 16AB 8585              STA     ARG2+HI
                
 16AD 205B1F            JSR     INPUT           ; READ LINE; RETURN LENGTH IN [A]
 16B0 85C2              STA     LINLEN          ; SAVE # CHARS IN LINE
                
 16B2 A900              LDA     #0
 16B4 85C3              STA     WRDLEN          ; INIT # CHARS IN WORD COUNTER
                
 16B6 A001              LDY     #1              ; POINT TO "# WORDS READ" SLOT
 16B8 9184              STA     (ARG2),Y        ; AND CLEAR IT ([A] = 0)
                
 16BA 84C0              STY     SOURCE          ; INIT SOURCE TABLE PNTR ([Y] = 1)
 16BC C8                INY                     ; = 2
 16BD 84C1              STY     RESULT          ; AND RESULT TABLE POINTER
                
 16BF                   ; MAIN LOOP STARTS HERE
                
 16BF A000      READL:  LDY     #0              ; POINT TO "MAX # WORDS" SLOT
 16C1 B184              LDA     (ARG2),Y        ; AND READ IT
                
 16C3 F004              BEQ     RLERR           ; IF ENTRY IS ZERO, PATCH IT (BM 5/14/85)
 16C5 C93C              CMP     #60             ; IF ENTRY <= 59,
 16C7 9004              BCC     RL0             ; CONTINUE
                
 16C9 A93B      RLERR:  LDA     #59             ; FORCE # TOKENS
 16CB 9184              STA     (ARG2),Y        ; TO BE 59
                
 16CD C8        RL0:    INY                     ; (Y = 1) POINT TO "# WORDS READ" SLOT
 16CE D184              CMP     (ARG2),Y        ; TOO MANY WORDS?
 16D0 9006              BCC     RLEX            ; EXIT IF SO (BM 5/1/85)
                
 16D2 A5C2      RL1:    LDA     LINLEN
 16D4 05C3              ORA     WRDLEN          ; OUT OF CHARS AND WORDS?
 16D6 D001              BNE     RL2             ; NOT YET


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  60    
--- READ HANDLER ---

                
 16D8 60        RLEX:   RTS                     ; ELSE EXIT
                
 16D9 A5C3      RL2:    LDA     WRDLEN          ; GET WORD LENGTH
 16DB C906              CMP     #6              ; 6 CHARS DONE?
 16DD 9003              BCC     RL3             ; NO, KEEP GOING
 16DF 207017            JSR     FLUSHW          ; ELSE FLUSH REMAINDER OF WORD
                
 16E2 A5C3      RL3:    LDA     WRDLEN          ; GET WORD LENGTH AGAIN
 16E4 D024              BNE     READL2          ; CONTINUE IF NOT FIRST CHAR
                
 16E6                   ; START A NEW WORD
                
 16E6 A205              LDX     #5              ; CLEAR Z-WORD INPUT BUFFER
 16E8 95B4      RLL:    STA     IN,X            ; [A] = 0
 16EA CA                DEX
 16EB 10FB              BPL     RLL
                
 16ED 206217            JSR     EFIND           ; GET BASE ADDRESS INTO [ENTRY]
 16F0 A5C0              LDA     SOURCE          ; STORE THE START POS OF THE WORD
 16F2 A003              LDY     #3              ; INTO THE "WORD START" SLOT
 16F4 91C4              STA     (ENTRY),Y               ; OF THE RESULT TABLE
                
 16F6 A8                TAY
 16F7 B182              LDA     (ARG1),Y        ; GET A CHAR FROM SOURCE BUFFER
 16F9 209D17            JSR     SIB             ; IS IT A SELF-INSERTING BREAK?
 16FC B026              BCS     DOSIB           ; YES IF CARRY WAS SET
                
 16FE 208B17            JSR     NORM            ; IS IT A "NORMAL" BREAK?
 1701 9007              BCC     READL2          ; NO, CONTINUE
                
 1703 E6C0              INC     SOURCE          ; ELSE FLUSH THE STRANDED BREAK
 1705 C6C2              DEC     LINLEN          ; UPDATE # CHARS LEFT IN LINE
 1707 4CBF16            JMP     READL           ; AND LOOP
                
 170A A5C2      READL2: LDA     LINLEN          ; OUT OF CHARS YET?
 170C F01E              BEQ     READL3          ; LOOKS THAT WAY
                
 170E A4C0              LDY     SOURCE
 1710 B182              LDA     (ARG1),Y        ; ELSE GRAB NEXT CHAR
 1712 208617            JSR     BREAK           ; IS IT A BREAK?
 1715 B015              BCS     READL3          ; YES IF CARRY WAS SET
                
 1717 A6C3              LDX     WRDLEN          ; ELSE STORE THE CHAR
 1719 95B4              STA     IN,X            ; INTO THE INPUT BUFFER
                
 171B C6C2              DEC     LINLEN          ; ONE LESS CHAR IN LINE
 171D E6C3              INC     WRDLEN          ; ONE MORE IN WORD
 171F E6C0              INC     SOURCE          ; POINT TO NEXT CHAR IN SOURCE
 1721 4CBF16            JMP     READL           ; AND LOOP BACK
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  61    
--- READ HANDLER ---

 1724 85B4      DOSIB:  STA     IN              ; PUT THE BREAK INTO 1ST WORD SLOT
 1726 C6C2              DEC     LINLEN          ; ONE LESS CHAR IN LINE
 1728 E6C3              INC     WRDLEN          ; ONE MORE IN WORD BUFFER
 172A E6C0              INC     SOURCE          ; POINT TO NEXT SOURCE CHAR
                
 172C A5C3      READL3: LDA     WRDLEN          ; ANY CHARS IN WORD YET?
 172E F08F              BEQ     READL           ; APPARENTLY NOT, SO LOOP BACK
                
 1730 206217            JSR     EFIND           ; GET ENTRY ADDR INTO [ENTRY]
 1733 A5C3              LDA     WRDLEN          ; GET ACTUAL LNGTH OF WORD
 1735 A002              LDY     #2              ; STORE IT IN "WORD LENGTH" SLOT
 1737 91C4              STA     (ENTRY),Y       ; OF THE CURRENT ENTRY
                
 1739 205E1A            JSR     CONZST          ; CONVERT ASCII IN [IN] TO Z-STRING
 173C 20AF17            JSR     FINDW           ; AND LOOK IT UP IN VOCABULARY
                
 173F A001              LDY     #1
 1741 B184              LDA     (ARG2),Y        ; FETCH THE # WORDS READ
 1743 18                CLC
 1744 6901              ADC     #1              ; INCREMENT IT
 1746 9184              STA     (ARG2),Y        ; AND UPDATE
                
 1748 206217            JSR     EFIND           ; MAKE [ENTRY] POINT TO ENTRY
                
 174B A000              LDY     #0
 174D 84C3              STY     WRDLEN          ; CLEAR # CHARS IN WORD
 174F A58D              LDA     VALUE+HI        ; GET MSB OF VOCAB ENTRY ADDRESS
 1751 91C4              STA     (ENTRY),Y       ; AND STORE IN 1ST SLOT OF ENTRY
 1753 C8                INY
 1754 A58C              LDA     VALUE+LO        ; ALSO STORE LSB IN 2ND SLOT
 1756 91C4              STA     (ENTRY),Y
                
 1758 A5C1              LDA     RESULT          ; UPDATE THE
 175A 18                CLC                     ; RESULT TABLE POINTER
 175B 6904              ADC     #4              ; SO IT POINTS TO THE
 175D 85C1              STA     RESULT          ; NEXT ENTRY
                
 175F 4CBF16            JMP     READL           ; AND LOOP BACK
                
 1762                   ; -----------------------------------
 1762                   ; FIND BASE ADDR OF RESULT ENTRY SLOT
 1762                   ; -----------------------------------
                
 1762 A584      EFIND:  LDA     ARG2+LO         ; LSB OF RESULT TABLE BASE
 1764 18                CLC
 1765 65C1              ADC     RESULT          ; AND CURRENT POINTER
 1767 85C4              STA     ENTRY+LO        ; SAVE IN [ENTRY]
 1769 A585              LDA     ARG2+HI         ; ALSO ADD MSB
 176B 6900              ADC     #0
 176D 85C5              STA     ENTRY+HI
 176F 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  62    
--- READ HANDLER ---

                
 1770                   ; ----------
 1770                   ; FLUSH WORD
 1770                   ; ----------
                
 1770 A5C2      FLUSHW: LDA     LINLEN          ; ANY CHARS LEFT IN LINE?
 1772 F011              BEQ     FLEX            ; NO, SCRAM
                
 1774 A4C0              LDY     SOURCE          ; GET CURRENT CHAR POINTER
 1776 B182              LDA     (ARG1),Y        ; AND GRAB A CHAR
 1778 208617            JSR     BREAK           ; IS IT A BREAK?
 177B B008              BCS     FLEX            ; EXIT IF SO
 177D C6C2              DEC     LINLEN          ; ELSE UPDATE CHAR COUNT
 177F E6C3              INC     WRDLEN          ; AND WORD-CHAR COUNT
 1781 E6C0              INC     SOURCE          ; AND CHAR POINTER
 1783 D0EB              BNE     FLUSHW          ; AND LOOP BACK (ALWAYS)
                
 1785 60        FLEX:   RTS
                
 1786                   ; ---------------------------------
 1786                   ; IS CHAR IN [A] ANY TYPE OF BREAK?
 1786                   ; ---------------------------------
                
 1786 209D17    BREAK:  JSR     SIB             ; CHECK FOR A SIB FIRST
 1789 B022              BCS     FBRK            ; EXIT NOW IF MATCHED
                
 178B                   ; ELSE FALL THROUGH ...
                
 178B                   ; --------------------------------
 178B                   ; IS CHAR IN [A] A "NORMAL" BREAK?
 178B                   ; --------------------------------
                
 178B A205      NORM:   LDX     #NBRKS-1        ; NUMBER OF "NORMAL" BREAKS
 178D DD9717    NBL:    CMP     BRKTBL,X        ; MATCHED?
 1790 F01B              BEQ     FBRK            ; YES, EXIT
 1792 CA                DEX
 1793 10F8              BPL     NBL             ; NO, KEEP LOOKING
 1795 18                CLC                     ; NO MATCH, CLEAR CARRY
 1796 60                RTS                     ; AND RETURN
                
 1797                   ; ------------------
 1797                   ; NORMAL BREAK CHARS
 1797                   ; ------------------
                
 1797 213F2C2E  BRKTBL: DB      "!?,."          ; IN ORDER OF
 179B 0D                DB      $0D             ; ASCENDING FREQUENCY
 179C 20                DB      SPACE           ; SPACE CHAR IS TESTED FIRST FOR SPEED
                
 0006           NBRKS   EQU     $-BRKTBL        ; # NORMAL BREAKS
                
 179D                   ; ---------------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  63    
--- READ HANDLER ---

 179D                   ; IS CHAR IN [A] A SIB?
 179D                   ; ---------------------
                
 179D AA        SIB:    TAX                     ; SAVE TEST CHAR
 179E A000              LDY     #0              ; 1ST BYTE IN VOCAB TABLE
 17A0 B1AE              LDA     (VOCAB),Y       ; HAS # SIBS
 17A2 A8                TAY                     ; USE AS AN INDEX
 17A3 8A                TXA                     ; RESTORE TEST CHAR
 17A4 D1AE      SBL:    CMP     (VOCAB),Y       ; MATCHED?
 17A6 F005              BEQ     FBRK            ; YES, REPORT IT
 17A8 88                DEY
 17A9 D0F9              BNE     SBL             ; ELSE KEEP LOOPING
 17AB 18                CLC                     ; NO MATCH, SO
 17AC 60                RTS                     ; EXIT WITH CARRY CLEAR
                
 17AD 38        FBRK:   SEC                     ; EXIT WITH CARRY SET
 17AE 60                RTS                     ; IF MATCHED WITH A BREAK CHAR
                
 17AF                   ; -----------------
 17AF                   ; VOCABULARY SEARCH
 17AF                   ; -----------------
                
 17AF                   ; ENTRY: 4-BYTE TARGET Z-WORD IN [OUT]
 17AF                   ; EXIT: ABS ENTRY ADDRESS IN [VALUE] IF FOUND;
 17AF                   ;       OTHERWISE [VALUE] = 0
                
 17AF A000      FINDW:  LDY     #0              ; GET # SIBS
 17B1 B1AE              LDA     (VOCAB),Y       ; IN VOCAB TABLE
 17B3 18                CLC                     ; INCREMENT IT
 17B4 6901              ADC     #1              ; FOR PROPER ALIGNMENT
 17B6 65AE              ADC     VOCAB+LO        ; NOW ADD THE BASE ADDR OF THE TABLE
 17B8 858C              STA     VALUE+LO        ; TO GET THE ACTUAL BASE ADDR
 17BA A5AF              LDA     VOCAB+HI        ; OF THE VOCAB ENTRIES
 17BC 6900              ADC     #0              ; WHICH IS SAVED
 17BE 858D              STA     VALUE+HI        ; IN [VALUE]
                
 17C0 B18C              LDA     (VALUE),Y       ; GET # BYTES PER ENTRY ([Y] = 0)
 17C2 85C8              STA     ESIZE           ; SAVE IT HERE
                
 17C4 20D40F            JSR     INCVAL          ; POINT TO NEXT BYTE
 17C7 B18C              LDA     (VALUE),Y       ; GET # ENTRIES IN TABLE (MSB)
 17C9 85C7              STA     NENTS+HI        ; AND STUFF IT IN [NENTS]
                
 17CB 20D40F            JSR     INCVAL          ; NEXT BYTE
 17CE B18C              LDA     (VALUE),Y       ; DON'T FORGET THE LSB!
 17D0 85C6              STA     NENTS+LO
                
 17D2 20D40F            JSR     INCVAL          ; [VALUE] NOW POINTS TO 1ST ENTRY
                
 17D5                   ; BEGIN THE SEARCH!
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE  64    
--- READ HANDLER ---

 17D5 A000      FWL1:   LDY     #0
 17D7 B18C              LDA     (VALUE),Y       ; GET 1ST BYTE OF ENTRY
 17D9 C5BA              CMP     OUT             ; MATCHED 1ST BYTE OF TARGET?
 17DB D015              BNE     WNEXT           ; NO, SKIP TO NEXT WORD
                
 17DD C8                INY
 17DE B18C              LDA     (VALUE),Y
 17E0 C5BB              CMP     OUT+1           ; 2ND BYTE MATCHED?
 17E2 D00E              BNE     WNEXT           ; NOPE
                
 17E4 C8                INY
 17E5 B18C              LDA     (VALUE),Y
 17E7 C5BC              CMP     OUT+2           ; 3RD BYTE?
 17E9 D007              BNE     WNEXT           ; SORRY ...
                
 17EB C8                INY
 17EC B18C              LDA     (VALUE),Y
 17EE C5BD              CMP     OUT+3           ; LAST BYTE
 17F0 F01F              BEQ     FWSUCC          ; FOUND IT!
                
 17F2 A5C8      WNEXT:  LDA     ESIZE           ; GET ENTRY SIZE
 17F4 18                CLC                     ; AND ADD IT TO ENTRY ADDRESS
 17F5 658C              ADC     VALUE+LO        ; TO MAKE [VALUE]
 17F7 858C              STA     VALUE+LO        ; POINT TO THE NEXT ENTRY
 17F9 9002              BCC     WNX
 17FB E68D              INC     VALUE+HI
                
 17FD A5C6      WNX:    LDA     NENTS+LO        ; DECREMENT THE
 17FF 38                SEC                     ; ENTRY COUNTER
 1800 E901              SBC     #1
 1802 85C6              STA     NENTS+LO
 1804 B002              BCS     WNX1
 1806 C6C7              DEC     NENTS+HI
                
 1808 05C7      WNX1:   ORA     NENTS+HI        ; KEEP SEARCHING
 180A D0C9              BNE     FWL1            ; UNTIL COUNT IS ZERO
                
 180C 858C              STA     VALUE+LO
 180E 858D              STA     VALUE+HI
 1810 60                RTS                     ; THEN RETURN WITH [VALUE] = 0
                
 1811                   ; ENTRY MATCHED!
                
 1811 A58D      FWSUCC: LDA     VALUE+HI        ; CONVERT ABSOLUTE ENTRY ADDRESS
 1813 38                SEC                     ; IN [VALUE]
 1814 E5A3              SBC     ZCODE           ; TO RELATIVE Z-ADDRESS
 1816 858D              STA     VALUE+HI        ; LSB NEEDN'T CHANGE
 1818 60                RTS
                
                        END
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  65    
--- READ HANDLER ---

                        INCLUD  PAGING.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  66    
--- TIME-STAMP PAGING ROUTINE ---

                
 1819                   ; -------------------------
 1819                   ; FETCH NEXT BYTE OF Z-CODE
 1819                   ; -------------------------
                
 1819                   ; EXIT: BYTE AT [ZPC] IN [A] & [Y]; FLAGS SET
                
 1819 A599      NEXTPC: LDA     ZPCFLG          ; IS [ZPCPNT] VALID?
 181B D01E              BNE     NPC2            ; YES, GET THE BYTE
                
 181D                   ; Z-PAGE HAS CHANGED!
                
 181D A597              LDA     ZPCM            ; GET TOP
 181F A498              LDY     ZPCH            ; 9 BITS OF [ZPC]
 1821 D008              BNE     NPC0            ; SWAP PAGE IF TOP BIT IS SET
                
 1823 C5A4              CMP     ZPURE           ; IS THIS PAGE PRELOADED?
 1825 B004              BCS     NPC0            ; NO, SWAP IT IN
                
 1827 65A3              ADC     ZCODE           ; ELSE MAKE IT ABSOLUTE
 1829 D007              BNE     NPC1            ; AND GIVE IT TO [ZPCPNT]
                
 182B A200      NPC0:   LDX     #0
 182D 869F              STX     MPCFLG          ; INVALIDATE [MPC]
 182F 208518            JSR     PAGE            ; AND GET ABS PAGE ADDR INTO [A]
                
 1832 859B      NPC1:   STA     ZPCPNT+HI       ; SET ABS PAGE ADDRESS
 1834 A2FF              LDX     #$FF
 1836 8699              STX     ZPCFLG          ; VALIDATE [ZPCPNT]
 1838 E8                INX                     ; = 0
 1839 869A              STX     ZPCPNT+LO       ; CLEAR LSB OF POINTER
                
 183B A496      NPC2:   LDY     ZPCL            ; FETCH PAGE INDEX
 183D B19A              LDA     (ZPCPNT),Y      ; GET Z-BYTE
                
 183F E696              INC     ZPCL            ; END OF PAGE YET?
 1841 D00A              BNE     NPC3            ; NO, EXIT
                
 1843 A000              LDY     #0
 1845 8499              STY     ZPCFLG          ; ELSE INVALIDATE [ZPCPNT]
                
 1847 E697              INC     ZPCM            ; POINT [ZPC] TO
 1849 D002              BNE     NPC3            ; THE NEXT
 184B E698              INC     ZPCH            ; Z-PAGE
                
 184D A8        NPC3:   TAY                     ; SET FLAGS
 184E 60                RTS                     ; AND RETURN
                
 184F                   ; -------------------------------
 184F                   ; GET NEXT BYTE OF VIRTUAL MEMORY
 184F                   ; -------------------------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  67    
--- TIME-STAMP PAGING ROUTINE ---

                
 184F                   ; EXIT: BYTE AT [MPC] IN [A] & [Y]; FLAGS SET
                
 184F A59F      GETBYT: LDA     MPCFLG          ; IS [MPCPNT] VALID?
 1851 D01E              BNE     GTBT2           ; YES, GET THE BYTE
                
 1853                   ; Z-PAGE HAS CHANGED!
                
 1853 A59D              LDA     MPCM            ; GET TOP
 1855 A49E              LDY     MPCH            ; 9 BITS OF [MPC]
 1857 D008              BNE     GTBT0           ; SWAP PAGE IF TOP BIT IS SET
                
 185A           PATCH   EQU     $+1             ; PATCH POINT FOR "VERIFY"
                
 1859 C5A4              CMP     ZPURE           ; IS THIS PAGE PRELOADED?
 185B B004              BCS     GTBT0           ; NO, SWAP IT IN
                
 185D 65A3              ADC     ZCODE           ; ELSE MAKE IT ABSOLUTE
 185F D007              BNE     GTBT1           ; AND GIVE IT TO [MPCPNT]
                
 1861 A200      GTBT0:  LDX     #0
 1863 8699              STX     ZPCFLG          ; INVALIDATE [ZPC]
 1865 208518            JSR     PAGE            ; AND GET ABS PAGE ADDR INTO [A]
                
 1868 85A1      GTBT1:  STA     MPCPNT+HI       ; SET ABS PAGE ADDRESS
 186A A2FF              LDX     #$FF
 186C 869F              STX     MPCFLG          ; VALIDATE [MPCPNT]
 186E E8                INX                     ; = 0
 186F 86A0              STX     MPCPNT+LO       ; CLEAR LSB OF POINTER
                
 1871 A49C      GTBT2:  LDY     MPCL            ; FETCH PAGE INDEX
 1873 B1A0              LDA     (MPCPNT),Y      ; GET Z-BYTE
                
 1875 E69C              INC     MPCL            ; END OF PAGE YET?
 1877 D00A              BNE     GTBT3           ; NO, EXIT
                
 1879 A000              LDY     #0
 187B 849F              STY     MPCFLG          ; ELSE INVALIDATE [MPCPNT]
                
 187D E69D              INC     MPCM            ; POINT [MPC] TO
 187F D002              BNE     GTBT3           ; THE NEXT
 1881 E69E              INC     MPCH            ; Z-PAGE
                
 1883 A8        GTBT3:  TAY                     ; SET FLAGS
 1884 60                RTS                     ; AND RETURN
                
 1885                   ; ------------------------
 1885                   ; LOCATE A SWAPABLE Z-PAGE
 1885                   ; ------------------------
                
 1885                   ; ENTRY: TARGET Z-PAGE IN [A/Y] (9 BITS)


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  68    
--- TIME-STAMP PAGING ROUTINE ---

 1885                   ; EXIT: ABSOLUTE PAGE IN [A]
                
 1885 85A8      PAGE:   STA     TARGET+LO       ; SAVE THE
 1887 84A9              STY     TARGET+HI       ; TARGET Z-PAGE HERE
                
 1889                   ; IS THIS Z-PAGE ALREADY PAGED IN?
                
 1889 A200              LDX     #0
 188B 86A7              STX     ZPAGE           ; START AT BUFFER #0
                
 188D DD0007    PG1:    CMP     PTABL,X         ; LSB MATCHED?
 1890 D008              BNE     PG2             ; NO, TRY NEXT BUFFER
 1892 98                TYA                     ; ELSE CHECK
 1893 DD0008            CMP     PTABH,X         ; TOP BIT
 1896 F02B              BEQ     PG4             ; MATCHED! BUFFER IN [ZPAGE]
 1898 A5A8              LDA     TARGET+LO       ; ELSE RESTORE LSB
 189A E6A7      PG2:    INC     ZPAGE           ; UPDATE TALLY
 189C E8                INX
 189D E4A6              CPX     PMAX            ; OUT OF BUFFERS YET?
 189F 90EC              BCC     PG1             ; NO, KEEP SEARCHING
                
 18A1                   ; SWAP IN THE TARGET PAGE
                
 18A1 20FC18    PG3:    JSR     EARLY           ; GET EARLIEST PAGE
 18A4 A6AB              LDX     SWAP            ; INTO [SWAP] & [X]
 18A6 86A7              STX     ZPAGE           ; SAVE FOR LATER
                
 18A8 A5A8              LDA     TARGET+LO       ; ASSIGN THE TARGET PAGE
 18AA 9D0007            STA     PTABL,X         ; TO THE EARLIEST BUFFER
 18AD 85EB              STA     DBLOCK+LO       ; ALSO GIVE IT TO ZDOS
                
 18AF A5A9              LDA     TARGET+HI       ; SAME FOR TOP BIT
 18B1 2901              AND     #%00000001      ; USE ONLY BIT 0
 18B3 9D0008            STA     PTABH,X
 18B6 85EC              STA     DBLOCK+HI
                
 18B8 8A                TXA
 18B9 18                CLC
 18BA 65A5              ADC     PAGE0           ; CALC ABS ADDR OF BUFFER
 18BC 85EE              STA     DBUFF+HI        ; GIVE IT TO ZDOS
                
 18BE 203325            JSR     GETDSK          ; SWAP IN THE NEW PAGE
 18C1 B034              BCS     DSKERR          ; ERROR IF CARRY SET
                
 18C3                   ; UPDATE THE TIMESTAMP
                
 18C3 A4A7      PG4:    LDY     ZPAGE           ; GET THE BUFFER INDEX
 18C5 B90009            LDA     LRUMAP,Y        ; GET THIS BUFFER'S STAMP
 18C8 C5AA              CMP     STAMP           ; SAME AS CURRENT STAMP?
 18CA F025              BEQ     PG8             ; YES, EXIT
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  69    
--- TIME-STAMP PAGING ROUTINE ---

 18CC E6AA              INC     STAMP           ; UPDATE STAMP
 18CE D01C              BNE     PG7             ; CONTINUE IF NO OVERFLOW
                
 18D0                   ; HANDLE STAMP OVERFLOW
                
 18D0 20FC18            JSR     EARLY           ; GET EARLIEST STAMP INTO [LRU]
                
 18D3 A200              LDX     #0              ; INIT INDEX
 18D5 BD0009    PG5:    LDA     LRUMAP,X        ; GET A STAMP READING
 18D8 F006              BEQ     PG6             ; EXIT IF ALREADY ZERO
 18DA 38                SEC                     ; ELSE SUBTRACT OFF
 18DB E5A2              SBC     LRU             ; THE EARLIEST TIMESTAMP
 18DD 9D0009            STA     LRUMAP,X        ; AND REPLACE THE STAMP
 18E0 E8        PG6:    INX
 18E1 E4A6              CPX     PMAX            ; END OF SWAPPING SPACE?
 18E3 90F0              BCC     PG5             ; LOOP TILL ALL STAMPS FIXED
                
 18E5 A900              LDA     #0              ; TURN BACK THE CLOCK
 18E7 38                SEC                     ; TO REFLECT NEW
 18E8 E5A2              SBC     LRU             ; STAMP READING
 18EA 85AA              STA     STAMP
                
 18EC A5AA      PG7:    LDA     STAMP           ; FETCH STAMP
 18EE 990009            STA     LRUMAP,Y        ; STAMP TARGET PAGE WITH IT
                
 18F1 A5A7      PG8:    LDA     ZPAGE           ; GET BUFFER INDEX
 18F3 18                CLC                     ; MAKE IT
 18F4 65A5              ADC     PAGE0           ; ABSOLUTE
 18F6 60                RTS                     ; AND RETURN IT IN [A]
                
 18F7                   ; *** ERROR #14: DRIVE ACCESS ***
                
 18F7 A90E      DSKERR: LDA     #14
 18F9 4CF91B            JMP     ZERROR
                
 18FC                   ; -------------------------
 18FC                   ; LOCATE EARLIEST TIMESTAMP
 18FC                   ; -------------------------
                
 18FC                   ; EXIT: [LRU] - EARLIEST TIMESTAMP
 18FC                   ;       [SWAP] = INDEX TO EARLIEST BUFFER
                
 18FC A200      EARLY:  LDX     #0              ; INIT INDEX
 18FE 86AB              STX     SWAP            ; AND [SWAP]
 1900 AD0009            LDA     LRUMAP          ; GET STAMP OF BUFFER #0
 1903 E8                INX                     ; START COMPARE WITH BUFFER #1
 1904 DD0009    EAR0:   CMP     LRUMAP,X        ; IS THIS STAMP EARLIER THAN [A]?
 1907 9005              BCC     EAR1            ; NO, TRY NEXT STAMP
 1909 BD0009            LDA     LRUMAP,X        ; ELSE FETCH EARLIER ENTRY
 190C 86AB              STX     SWAP            ; AND REMEMBER WHERE WE FOUND IT
 190E E8        EAR1:   INX                     ; POINT TO NEXT STAMP


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  70    
--- TIME-STAMP PAGING ROUTINE ---

 190F E4A6              CPX     PMAX            ; OUT OF STAMPS YET?
 1911 90F1              BCC     EAR0            ; LOOP TILL EMPTY
                
 1913 85A2              STA     LRU             ; SAVE EARLIEST STAMP HERE
 1915 60                RTS
                
 1916                   ; -------------------------
 1916                   ; POINT [MPC] TO V-ADDR [I]
 1916                   ; -------------------------
                
 1916 A58E      SETWRD: LDA     I+LO
 1918 859C              STA     MPCL
 191A A58F              LDA     I+HI
 191C 859D              STA     MPCM
                
 191E A900              LDA     #0
 1920 859E              STA     MPCH            ; ZERO TOP BIT
 1922 859F              STA     MPCFLG          ; INVALIDATE [MPC]
 1924 60                RTS
                
 1925                   ; ----------------------------
 1925                   ; GET Z-WORD AT [MPC] INTO [I]
 1925                   ; ----------------------------
                
 1925 204F18    GETWRD: JSR     GETBYT
 1928 858F              STA     I+HI
 192A 204F18            JSR     GETBYT
 192D 858E              STA     I+LO
 192F 60                RTS
                
                        END
                        INCLUD  ZSTRING.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  71    
--- Z-STRING HANDLERS ---

                
 1930                   ; -----------------------
 1930                   ; POINT TO ZSTRING IN [I]
 1930                   ; -----------------------
                
 1930 A58E      SETSTR: LDA     I+LO            ; WORD-ALIGN THE ADDRESS
 1932 0A                ASL     A
 1933 859C              STA     MPCL
 1935 A58F              LDA     I+HI
 1937 2A                ROL     A
 1938 859D              STA     MPCM
 193A A900              LDA     #0
 193C 859F              STA     MPCFLG          ; [MPC] IS CHANGING!
 193E 2A                ROL     A
 193F 859E              STA     MPCH
                
 1941 60        ZSTEX:  RTS
                
 1942                   ; -----------------------
 1942                   ; PRINT Z-STRING AT [MPC]
 1942                   ; -----------------------
                
 1942 A200      PZSTR:  LDX     #0
 1944 86C9              STX     PSET            ; ASSUME PERMANENT CHARSET
 1946 86CD              STX     ZFLAG           ; CLEAR BYTE FLAG
 1948 CA                DEX                     ; = $FF
 1949 86CA              STX     TSET            ; NO TEMPSET ACTIVE
                
 194B 20161A    PZTOP:  JSR     GETZCH          ; GET A Z-CHAR
 194E B0F1              BCS     ZSTEX           ; END OF STRING IF CARRY IS SET
                
 1950 85CB              STA     ZCHAR           ; ELSE SAVE CHAR HERE
 1952 AA                TAX                     ; SET FLAGS
 1953 F041              BEQ     BLANK           ; PRINT SPACE IF CHAR = 0
                
 1955 C904              CMP     #4              ; IS THIS AN F-WORD?
 1957 905B              BCC     DOFREQ          ; APPARENTLY SO
                
 1959 C906              CMP     #6              ; PERHAPS A SHIFT CODE?
 195B 903D              BCC     NEWSET          ; YES, CHANGE CHARSETS
                
 195D 200A1A            JSR     GETSET          ; ELSE GET CHARSET
 1960 AA                TAX                     ; SET FLAGS
 1961 D00B              BNE     SET1            ; SKIP IF NOT CHARSET #0
                
 1963                   ; PRINT A LOWER-CASE CHAR (CHARSET #0)
                
 1963 A95B              LDA     #$61-6          ; ASCII "a" MINUS Z-OFFSET
                
 1965 18        TOASC:  CLC
 1966 65CB              ADC     ZCHAR           ; ADD Z-CHAR INDEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  72    
--- Z-STRING HANDLERS ---

                
 1968 20891C    SHOVE:  JSR     COUT            ; SHOW THE CHAR
 196B 4C4B19            JMP     PZTOP           ; AND GRAB NEXT CHAR
                
 196E                   ; PRINT AN UPPER-CASE CHAR (CHARSET #1)
                
 196E C901      SET1:   CMP     #1              ; MAKE SURE IT'S SET #1
 1970 D004              BNE     SET2            ; ELSE MUST BE SET #2
                
 1972 A93B              LDA     #$41-6          ; ASCII "A" MINUS Z-OFFSET
 1974 D0EF              BNE     TOASC           ; SAME AS SET #0
                
 1976                   ; PRINT FROM CHARSET #2
                
 1976 A5CB      SET2:   LDA     ZCHAR           ; RETRIEVE THE Z-CHAR
 1978 38                SEC
 1979 E906              SBC     #6              ; ZERO-ALIGN IT
 197B F007              BEQ     DIRECT          ; IF ZERO, IT'S A "DIRECT" ASCII
                
 197D AA                TAX                     ; OTHERWISE USE CODE AS AN INDEX
 197E BD331B            LDA     CHRTBL,X        ; INTO THE CHARSET TABLE
 1981 4C6819            JMP     SHOVE           ; AND PRINT THE CHAR
                
 1984                   ; DECODE A "DIRECT" ASCII CHAR
                
 1984 20161A    DIRECT: JSR     GETZCH          ; FETCH NEXT Z-CHAR
 1987 0A                ASL     A
 1988 0A                ASL     A
 1989 0A                ASL     A
 198A 0A                ASL     A
 198B 0A                ASL     A               ; SHIFT INTO POSITION
 198C 85CB              STA     ZCHAR           ; AND SAVE HERE
 198E 20161A            JSR     GETZCH          ; GRAB YET ANOTHER Z-CHAR
 1991 05CB              ORA     ZCHAR           ; SUPERIMPOSE THE 2ND BYTE
 1993 4C6819            JMP     SHOVE           ; AND PRINT THE RESULT
                
 1996                   ; PRINT A SPACE
                
 1996 A920      BLANK:  LDA     #SPACE          ; ASCII SPACE CHAR
 1998 D0CE              BNE     SHOVE
                
 199A                   ; CHANGE CHARSET
                
 199A 38        NEWSET: SEC                     ; CONVERT THE SHIFT CODE
 199B E903              SBC     #3              ; TO 1 OR 2
 199D A8                TAY
 199E 200A1A            JSR     GETSET          ; IS MODE TEMPORARY?
 19A1 D005              BNE     TOPERM          ; YES, DO A PERMSHIFT
 19A3 84CA              STY     TSET            ; ELSE JUST A TEMPSHIFT
 19A5 4C4B19            JMP     PZTOP           ; AND CONTINUE
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  73    
--- Z-STRING HANDLERS ---

 19A8 84C9      TOPERM: STY     PSET            ; SET PERM CHARSET
 19AA C5C9              CMP     PSET            ; SAME AS BEFORE?
 19AC F09D              BEQ     PZTOP           ; YES, CONTINUE
 19AE A900              LDA     #0
 19B0 85C9              STA     PSET            ; ELSE RESET CHARSET
 19B2 F097              BEQ     PZTOP           ; BEFORE LOOPING BACK
                
 19B4                   ; PRINT AN F-WORD
                
 19B4 38        DOFREQ: SEC
 19B5 E901              SBC     #1              ; ZERO-ALIGN THE CODE
 19B7 0A                ASL     A               ; AND MULTIPLY TIMES 64
 19B8 0A                ASL     A               ; TO OBTAIN THE SEGMENT OFFSET
 19B9 0A                ASL     A               ; INTO THE F-WORDS TABLE
 19BA 0A                ASL     A
 19BB 0A                ASL     A
 19BC 0A                ASL     A
 19BD 85CC              STA     OFFSET          ; SAVE OFFSET FOR LATER
                
 19BF 20161A            JSR     GETZCH          ; NOW GET THE F-WORD POINTER
 19C2 0A                ASL     A               ; WORD-ALIGN IT
 19C3 18                CLC                     ; AND
 19C4 65CC              ADC     OFFSET          ; ADD THE SEGMENT OFFSET
 19C6 A8                TAY                     ; TO GET THE OFFSET OF THE F-WORD
 19C7 B1B0              LDA     (FWORDS),Y      ; FROM THE START OF THE F-WORDS TABLE
 19C9 858F              STA     I+HI            ; SAVE MSB OF F-WORD ADDRESS
 19CB C8                INY
 19CC B1B0              LDA     (FWORDS),Y      ; ALSO SAVE LSB
 19CE 858E              STA     I+LO            ; Z-ADDRESS OF F-WORD IS IN [I]
                
 19D0                   ; SAVE THE STATE OF CURRENT Z-STRING
                
 19D0 A59E              LDA     MPCH
 19D2 48                PHA
 19D3 A59D              LDA     MPCM
 19D5 48                PHA
 19D6 A59C              LDA     MPCL
 19D8 48                PHA
 19D9 A5C9              LDA     PSET
 19DB 48                PHA
 19DC A5CD              LDA     ZFLAG
 19DE 48                PHA
 19DF A5CF              LDA     ZWORD+HI
 19E1 48                PHA
 19E2 A5CE              LDA     ZWORD+LO
 19E4 48                PHA
                
 19E5 203019            JSR     SETSTR          ; PRINT THE Z-STRING
 19E8 204219            JSR     PZSTR           ; IN [I]
                
 19EB                   ; RESTORE OLD Z-STRING


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  74    
--- Z-STRING HANDLERS ---

                
 19EB 68                PLA
 19EC 85CE              STA     ZWORD+LO
 19EE 68                PLA
 19EF 85CF              STA     ZWORD+HI
 19F1 68                PLA
 19F2 85CD              STA     ZFLAG
 19F4 68                PLA
 19F5 85C9              STA     PSET
 19F7 68                PLA
 19F8 859C              STA     MPCL
 19FA 68                PLA
 19FB 859D              STA     MPCM
 19FD 68                PLA
 19FE 859E              STA     MPCH
                
 1A00 A2FF              LDX     #$FF
 1A02 86CA              STX     TSET            ; DISABLE TEMP CHARSET
 1A04 E8                INX                     ; = 0
 1A05 869F              STX     MPCFLG          ; [MPC] HAS CHANGED
 1A07 4C4B19            JMP     PZTOP           ; CONTINUE INNOCENTLY
                
 1A0A                   ; ----------------------
 1A0A                   ; RETURN CURRENT CHARSET
 1A0A                   ; ----------------------
                
 1A0A A5CA      GETSET: LDA     TSET
 1A0C 1003              BPL     GS
 1A0E A5C9              LDA     PSET
 1A10 60                RTS
                
 1A11 A0FF      GS:     LDY     #$FF
 1A13 84CA              STY     TSET
 1A15 60                RTS
                
 1A16                   ; -----------------
 1A16                   ; FETCH NEXT Z-CHAR
 1A16                   ; -----------------
                
 1A16 A5CD      GETZCH: LDA     ZFLAG           ; WHICH BYTE IS THIS?
 1A18 1002              BPL     GTZ0            ; $FF = LAST
 1A1A 38                SEC                     ; SET CARRY TO INDICATE
 1A1B 60                RTS                     ; NO MORE CHARS
                
 1A1C D013      GTZ0:   BNE     GETZ1           ; NOT FIRST CHAR, EITHER
                
 1A1E                   ; GET A Z-WORD INTO [ZWORD], RETURN 1ST CHAR IN TRIPLET
                
 1A1E E6CD              INC     ZFLAG           ; UPDATE CHAR COUNT
                
 1A20 204F18            JSR     GETBYT          ; GET TRIPLET AT [MPC]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  75    
--- Z-STRING HANDLERS ---

 1A23 85CF              STA     ZWORD+HI        ; INTO [ZWORD]
 1A25 204F18            JSR     GETBYT
 1A28 85CE              STA     ZWORD+LO
                
 1A2A A5CF              LDA     ZWORD+HI
 1A2C 4A                LSR     A
 1A2D 4A                LSR     A               ; SHIFT 1ST CHAR INTO PLACE
 1A2E 4C5A1A            JMP     GTEXIT          ; AND RETURN IT
                
 1A31 38        GETZ1:  SEC
 1A32 E901              SBC     #1
 1A34 D016              BNE     GETZ2           ; LAST CHAR IN TRIPLET IF ZERO
 1A36 A902              LDA     #2              ; ELSE
 1A38 85CD              STA     ZFLAG           ; RESET CHAR INDEX
                
 1A3A A5CE              LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET
 1A3C 858E              STA     I+LO            ; MOVE HERE FOR SHIFTING
 1A3E A5CF              LDA     ZWORD+HI        ; GET TOP HALF
                
 1A40 068E              ASL     I+LO            ; SHIFT THE TOP 3 BITS OF LOWER HALF
 1A42 2A                ROL     A               ; INTO THE BOTTOM OF THE TOP HALF
 1A43 068E              ASL     I+LO
 1A45 2A                ROL     A
 1A46 068E              ASL     I+LO
 1A48 2A                ROL     A
 1A49 4C5A1A            JMP     GTEXIT
                
 1A4C A900      GETZ2:  LDA     #0              ; SET FLAG TO INDICATE
 1A4E 85CD              STA     ZFLAG           ; END OF TRIPLET
                
 1A50 A5CF              LDA     ZWORD+HI        ; TEST TOP HALF OF TRIPLET
 1A52 1004              BPL     GETZ3           ; CONTINUE IF NOT END OF STRING
 1A54 A9FF              LDA     #$FF            ; ELSE
 1A56 85CD              STA     ZFLAG           ; INDICATE LAST TRIPLET IN STRING
                
 1A58 A5CE      GETZ3:  LDA     ZWORD+LO        ; GET BOTTOM HALF OF TRIPLET
                
 1A5A 291F      GTEXIT: AND     #%00011111      ; MASK OUT GARBAGE BITS
 1A5C 18                CLC
 1A5D 60                RTS
                
 1A5E                   ; ---------------------------------
 1A5E                   ; CONVERT [IN] TO Z-STRING IN [OUT]
 1A5E                   ; ---------------------------------
                
 1A5E A905      CONZST: LDA     #$05            ; FILL OUTPUT BUFFER
 1A60 AA                TAX                     ; WITH PAD CHARS ($05)
 1A61 95BA      CZSL:   STA     OUT,X
 1A63 CA                DEX
 1A64 10FB              BPL     CZSL
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  76    
--- Z-STRING HANDLERS ---

 1A66 A906              LDA     #6              ; INIT
 1A68 85D0              STA     CONCNT          ; CHAR COUNT
                
 1A6A A900              LDA     #0              ; CLEAR
 1A6C 85D1              STA     CONIN           ; SOURCE AND
 1A6E 85D2              STA     CONOUT          ; OUTPUT INDEXES
                
 1A70 A6D1      CONTOP: LDX     CONIN           ; FETCH SOURCE INDEX
 1A72 E6D1              INC     CONIN           ; AND UPDATE
 1A74 B5B4              LDA     IN,X            ; GRAB AN ASCII CHAR
 1A76 85CB              STA     ZCHAR           ; SAVE IT HERE
 1A78 D004              BNE     NEXTZ           ; CONTINUE IF CHAR WAS NZ
                
 1A7A A905              LDA     #5              ; ELSE SHIP OUT
 1A7C D02C              BNE     CSHIP           ; A PAD CHAR
                
 1A7E A5CB      NEXTZ:  LDA     ZCHAR
 1A80 20F41A            JSR     SAYSET          ; WHICH CHARSET TO USE?
 1A83 F020              BEQ     CSET0           ; LOWER-CASE IF ZERO
                
 1A85 18                CLC                     ; ELSE DO A TEMP-SHIFT
 1A86 6903              ADC     #3              ; 4 = CHARSET 1, 5 = CHARSET 2
 1A88 A6D2              LDX     CONOUT          ; FETCH OUTPUT INDEX
 1A8A 95BA              STA     OUT,X           ; SEND THE SHIFT CHAR
                
 1A8C E6D2              INC     CONOUT          ; UPDATE INDEX
 1A8E C6D0              DEC     CONCNT          ; AND CHAR COUNT
 1A90 D003              BNE     CTEST           ; IF OUT OF CHARS
 1A92 4C0D1B            JMP     ZCRUSH          ; CRUSH 'EM!
                
 1A95 A5CB      CTEST:  LDA     ZCHAR           ; TEST CHAR AGAIN
 1A97 20F41A            JSR     SAYSET
 1A9A C902              CMP     #2
 1A9C F019              BEQ     CSET2           ; CHARSET #2
                
 1A9E                   ; HANDLE CHARSET #1 (UPPER CASE ALPHA)
                
 1A9E A5CB              LDA     ZCHAR
 1AA0 38                SEC
 1AA1 E93B              SBC     #$41-6          ; CONVERT TO Z-CHAR
 1AA3 1005              BPL     CSHIP           ; AND SEND TO OUTPUT
                
 1AA5                   ; HANDLE CHARSET #0 (LOWER CASE ALPHA)
                
 1AA5 A5CB      CSET0:  LDA     ZCHAR
 1AA7 38                SEC
 1AA8 E95B              SBC     #$61-6          ; CONVERT TO Z-CHAR
                
 1AAA                   ; SHIP Z-CHAR TO OUTPUT BUFFER
                
 1AAA A6D2      CSHIP:  LDX     CONOUT          ; FETCH OUTPUT INDEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  77    
--- Z-STRING HANDLERS ---

 1AAC 95BA              STA     OUT,X
                
 1AAE E6D2              INC     CONOUT          ; UPDATE INDEX
 1AB0 C6D0              DEC     CONCNT          ; DONE 6 CHARS YET?
 1AB2 D0BC              BNE     CONTOP          ; NO, LOOP BACK
 1AB4 4C0D1B            JMP     ZCRUSH          ; ELSE CRUSH
                
 1AB7                   ; HANDLE CHARSET #2 (MISCELLANEOUS)
                
 1AB7 A5CB      CSET2:  LDA     ZCHAR           ; GRAB CHAR
 1AB9 20E41A            JSR     CTABLE          ; IS IT IN CHARSET #3 TABLE?
 1ABC D0EC              BNE     CSHIP           ; YES, SEND IT TO OUTPUT
                
 1ABE                   ; SEND A "DIRECT" ASCII CHAR
                
 1ABE A906              LDA     #6              ; ASCII ALERT!
 1AC0 A6D2              LDX     CONOUT
 1AC2 95BA              STA     OUT,X
                
 1AC4 E6D2              INC     CONOUT          ; UPDATE INDEX
 1AC6 C6D0              DEC     CONCNT          ; AND CHAR COUNT
 1AC8 F043              BEQ     ZCRUSH          ; BUFFER FULL!
                
 1ACA                   ; SEND 1ST HALF OF "DIRECT"
                
 1ACA A5CB              LDA     ZCHAR
 1ACC 4A                LSR     A
 1ACD 4A                LSR     A
 1ACE 4A                LSR     A
 1ACF 4A                LSR     A
 1AD0 4A                LSR     A
 1AD1 2903              AND     #%00000011      ; MASK GARBAGE
 1AD3 A6D2              LDX     CONOUT
 1AD5 95BA              STA     OUT,X
                
 1AD7 E6D2              INC     CONOUT
 1AD9 C6D0              DEC     CONCNT
 1ADB F030              BEQ     ZCRUSH          ; BUFFER FULL!
                
 1ADD                   ; SEND 2ND HALF OF "DIRECT"
                
 1ADD A5CB              LDA     ZCHAR           ; GET CHAR YET AGAIN
 1ADF 291F              AND     #%00011111      ; MASK JUNK
 1AE1 4CAA1A            JMP     CSHIP           ; AND SHIP IT OUT
                
 1AE4                   ; ---------------------
 1AE4                   ; IS [A] IN CHARSET #3?
 1AE4                   ; ---------------------
                
 1AE4                   ; EXIT: [A] = CHAR CODE IF FOUND, Z-FLAG CLEARED
 1AE4                   ;       Z-FLAG SET IF NOT FOUND


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  78    
--- Z-STRING HANDLERS ---

                
 1AE4 A219      CTABLE: LDX     #25
 1AE6 DD331B    CNL:    CMP     CHRTBL,X
 1AE9 F004              BEQ     CNOK
 1AEB CA                DEX
 1AEC D0F8              BNE     CNL
 1AEE 60                RTS                     ; Z-FLAG SET IF NO MATCH
                
 1AEF 8A        CNOK:   TXA                     ; CHAR CODE IS INDEX
 1AF0 18                CLC
 1AF1 6906              ADC     #6              ; PLUS 6
 1AF3 60                RTS
                
 1AF4                   ; -----------------------------
 1AF4                   ; RETURN CHARSET OF CHAR IN [A]
 1AF4                   ; -----------------------------
                
 1AF4 C961      SAYSET: CMP     #'a'
 1AF6 9007              BCC     SAY1
 1AF8 C97B              CMP     #'z'+1
 1AFA B003              BCS     SAY1
 1AFC A900              LDA     #0              ; IT'S CHARSET #0
 1AFE 60                RTS
                
 1AFF C941      SAY1:   CMP     #'A'
 1B01 9007              BCC     SAY2
 1B03 C95B              CMP     #'Z'+1
 1B05 B003              BCS     SAY2
 1B07 A901              LDA     #1              ; IT'S CHARSET #1
 1B09 60                RTS
                
 1B0A A902      SAY2:   LDA     #2              ; IT'S CHARSET #2
 1B0C 60                RTS
                
 1B0D                   ; ----------------------
 1B0D                   ; CRUSH Z-CHARS IN [OUT]
 1B0D                   ; ----------------------
                
 1B0D A5BB      ZCRUSH: LDA     OUT+1           ; GET 2ND Z-CHAR
 1B0F 0A                ASL     A               ; SHIFT BITS INTO POSITION
 1B10 0A                ASL     A
 1B11 0A                ASL     A
 1B12 0A                ASL     A
 1B13 26BA              ROL     OUT             ; ALONG WITH 1ST Z-CHAR
 1B15 0A                ASL     A
 1B16 26BA              ROL     OUT
 1B18 05BC              ORA     OUT+2           ; SUPERIMPOSE 3RD Z-CHAR
 1B1A 85BB              STA     OUT+1
                
 1B1C A5BE              LDA     OUT+4           ; GET 5TH Z-CHAR
 1B1E 0A                ASL     A               ; SHIFT BITS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  79    
--- Z-STRING HANDLERS ---

 1B1F 0A                ASL     A
 1B20 0A                ASL     A
 1B21 0A                ASL     A
 1B22 26BD              ROL     OUT+3           ; ALONG WITH 4TH Z-CHAR
 1B24 0A                ASL     A
 1B25 26BD              ROL     OUT+3
 1B27 05BF              ORA     OUT+5           ; SUPERIMPOSE 6TH Z-CHAR
 1B29 AA                TAX                     ; SAVE HERE
 1B2A A5BD              LDA     OUT+3           ; GRAB 4TH Z-CHAR
 1B2C 0980              ORA     #%10000000      ; SET HIGH BIT
 1B2E 85BC              STA     OUT+2           ; MOVE CRUSHED Z-WORD
 1B30 86BD              STX     OUT+3           ; INTO PLACE
 1B32 60                RTS
                
 1B33                   ; -----------------------
 1B33                   ; CHARSET #2 DECODE TABLE
 1B33                   ; -----------------------
                
 1B33 00        CHRTBL: DB      0               ; DUMMY BYTE FOR "DIRECT"
 1B34 0D                DB      $0D             ; EOL
 1B35 30313233          DB      "0123456789.,!?_#"
 1B45 27                DB      $27             ; SINGLE QUOTE
 1B46 22                DB      $22             ; DOUBLE QUOTE
 1B47 2F5C2D3A          DB      "/\-:()"
                
                        END
                        INCLUD  OBJECTS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  80    
--- OBJECT & PROPERTY HANDLERS ---

                
 1B4D                   ; ----------------------------------
 1B4D                   ; GET ABSOLUTE ADDRESS OF OBJECT [A]
 1B4D                   ; ----------------------------------
                
 1B4D                   ; EXIT: ADDRESS IN [I]
                
 1B4D 858E      OBJLOC: STA     I+LO            ; SAVE LSB FOR ADDING
                
 1B4F A200              LDX     #0              ; CLEAR MSB
 1B51 868F              STX     I+HI            ; FOR SHIFTING
                
 1B53 0A                ASL     A               ; MULTIPLY BY 8
 1B54 268F              ROL     I+HI
 1B56 0A                ASL     A
 1B57 268F              ROL     I+HI
 1B59 0A                ASL     A
 1B5A 268F              ROL     I+HI
                
 1B5C 18                CLC                     ; ADD TO ITSELF
 1B5D 658E              ADC     I+LO            ; TO GET TIMES 9
 1B5F 9002              BCC     OBJ1
 1B61 E68F              INC     I+HI
                
 1B63 18        OBJ1:   CLC
 1B64 6935              ADC     #53             ; NOW ADD 53
 1B66 9002              BCC     OBJ2            ; (THE OBJECT TABLE OFFSET)
 1B68 E68F              INC     I+HI
                
 1B6A 18        OBJ2:   CLC                     ; NEXT ADD THE ABS ADDR
 1B6B 65B2              ADC     OBJTAB+LO       ; OF THE OBJECT TABLE
 1B6D 858E              STA     I+LO
                
 1B6F A58F              LDA     I+HI
 1B71 65B3              ADC     OBJTAB+HI
 1B73 858F              STA     I+HI
 1B75 60                RTS
                
 1B76                   ; -----------------------------
 1B76                   ; GET ADDRESS OF PROPERTY TABLE
 1B76                   ; -----------------------------
                
 1B76                   ; EXIT: [I] HAS ABSOLUTE ADDR OF PROPERTY TABLE
 1B76                   ;       [Y] HAS OFFSET TO START OF PROP IDS
                
 1B76 A582      PROPB:  LDA     ARG1+LO
 1B78 204D1B            JSR     OBJLOC
 1B7B A007              LDY     #7
 1B7D B18E              LDA     (I),Y           ; GET MSB OF P-TABLE ADDRESS
 1B7F 18                CLC
 1B80 65A3              ADC     ZCODE           ; MAKE IT ABSOLUTE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  81    
--- OBJECT & PROPERTY HANDLERS ---

 1B82 AA                TAX                     ; AND SAVE HERE
 1B83 C8                INY
 1B84 B18E              LDA     (I),Y           ; NOW GET LSB
 1B86 858E              STA     I+LO
 1B88 868F              STX     I+HI            ; [I] NOW POINTS TO PROP TABLE
                
 1B8A A000              LDY     #0
 1B8C B18E              LDA     (I),Y           ; GET LENGTH OF SHORT DESC
 1B8E 0A                ASL     A               ; WORD-ALIGN IT
 1B8F A8                TAY                     ; EXPECTED HERE
 1B90 C8                INY                     ; POINT JUST PAST THE DESCRIPTION
 1B91 60                RTS
                
 1B92                   ; -------------------
 1B92                   ; FETCH A PROPERTY ID
 1B92                   ; -------------------
                
 1B92                   ; ENTRY: LIKE "PROPB" EXIT
                
 1B92 B18E      PROPN:  LDA     (I),Y
 1B94 291F              AND     #%00011111      ; MASK OUT LENGTH BITS
 1B96 60                RTS
                
 1B97                   ; -------------------------------
 1B97                   ; FETCH # BYTES IN PROPERTY VALUE
 1B97                   ; -------------------------------
                
 1B97                   ; ENTRY: LIKE "PROPB" EXIT
                
 1B97 B18E      PROPL:  LDA     (I),Y
 1B99 4A                LSR     A               ; LENGTH IS IN
 1B9A 4A                LSR     A               ; BITS 7-5
 1B9B 4A                LSR     A               ; SO SHIFT INTO PLACE
 1B9C 4A                LSR     A
 1B9D 4A                LSR     A
 1B9E 60                RTS
                
 1B9F                   ; ----------------------
 1B9F                   ; POINT TO NEXT PROPERTY
 1B9F                   ; ----------------------
                
 1B9F                   ; ENTRY: LIKE "PROPB" EXIT
                
 1B9F 20971B    PROPNX: JSR     PROPL           ; GET LENGTH OF CURRENT PROP
 1BA2 AA                TAX                     ; SAVE HERE
                
 1BA3 C8        PPX:    INY                     ; LOOP UNTIL
 1BA4 CA                DEX                     ; [Y] POINTS TO
 1BA5 10FC              BPL     PPX             ; START OF NEXT PROP
 1BA7 C8                INY                     ; CORRECT ALIGNMENT
 1BA8 60                RTS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  82    
--- OBJECT & PROPERTY HANDLERS ---

                
 1BA9                   ; ----------------
 1BA9                   ; GET OBJECT FLAGS
 1BA9                   ; ----------------
                
 1BA9                   ; ENTRY: OBJECT # IN [ARG1], FLAG # IN [ARG2]
 1BA9                   ; EXIT: FLAG WORD IN [K], BIT ID IN [J],
 1BA9                   ;       FLAG WORD ADDRESS IN [I]
                
 1BA9 A582      FLAGSU: LDA     ARG1+LO
 1BAB 204D1B            JSR     OBJLOC          ; GET OBJECT ADDR IN [I]
                
 1BAE A584              LDA     ARG2+LO         ; LOOK AT FLAG ID
 1BB0 C910              CMP     #$10            ; FIRST SET OF FLAGS?
 1BB2 900F              BCC     FLS1            ; YES, ADDR IN [I] IS CORRECT
                
 1BB4 E910              SBC     #16             ; ELSE ZERO-ALIGN FLAG INDEX
 1BB6 AA                TAX                     ; SAVE IT HERE
                
 1BB7 A58E              LDA     I+LO            ; ADD 2 TO ADDRESS IN [I]
 1BB9 18                CLC                     ; TO POINT TO ADDRESS OF
 1BBA 6902              ADC     #2              ; 2ND FLAG WORD
 1BBC 858E              STA     I+LO
 1BBE 9002              BCC     FLS0
 1BC0 E68F              INC     I+HI
                
 1BC2 8A        FLS0:   TXA                     ; RESTORE INDEX
                
 1BC3 8592      FLS1:   STA     K+LO            ; SAVE FLAG ID HERE
                
 1BC5 A201              LDX     #1              ; INIT THE
 1BC7 8690              STX     J+LO            ; FLAG WORD TO
 1BC9 CA                DEX                     ; $0001
 1BCA 8691              STX     J+HI
                
 1BCC A90F              LDA     #15             ; SUBTRACT THE BIT POSITION
 1BCE 38                SEC                     ; FROM 15
 1BCF E592              SBC     K+LO            ; TO GET THE SHIFT LOOP
 1BD1 AA                TAX                     ; INDEX
 1BD2 F007              BEQ     FLS2            ; EXIT NOW IF NO SHIFT NEEDED
                
 1BD4 0690      FLSL:   ASL     J+LO            ; SHIFT THE BIT
 1BD6 2691              ROL     J+HI            ; INTO POSITION
 1BD8 CA                DEX
 1BD9 D0F9              BNE     FLSL
                
 1BDB A000      FLS2:   LDY     #0              ; MOVE THE FLAG WORD
 1BDD B18E              LDA     (I),Y           ; INTO [J]
 1BDF 8593              STA     K+HI            ; FIRST THE MSB
 1BE1 C8                INY
 1BE2 B18E              LDA     (I),Y


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- OP SUPPORT & MEMORY MANAGEMENT                                                   PAGE  83    
--- OBJECT & PROPERTY HANDLERS ---

 1BE4 8592              STA     K+LO            ; THEN THE LSB
 1BE6 60                RTS
                
                        END
                
                        INCLUD  IO.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  84    
--- GAME I/O: ATARI ---

                
 1BE7                   ; --------------
 1BE7                   ; INTERNAL ERROR
 1BE7                   ; --------------
                
 1BE7                   ; ENTRY: ERROR CODE IN [A]
                
 1BE7 496E7465  ERRM:   DB      "Internal error "
 1BF6 30302E    ENUMB:  DB      "00."
 0012           ERRML   EQU     $-ERRM
                
 1BF9 A001      ZERROR: LDY     #1              ; CONVERT ERROR BYTE IN [A]
                
 1BFB A200      ZERR0:  LDX     #0              ; TO ASCII AT "ENUMB"
 1BFD C90A      ZERR1:  CMP     #10
 1BFF 9005              BCC     ZERR2
 1C01 E90A              SBC     #10
 1C03 E8                INX
 1C04 D0F7              BNE     ZERR1
                
 1C06 0930      ZERR2:  ORA     #'0'
 1C08 99F61B            STA     ENUMB,Y
 1C0B 8A                TXA
 1C0C 88                DEY
 1C0D 10EC              BPL     ZERR0
                
 1C0F 20C71C            JSR     ZCRLF           ; CLEAR LINE BUFFER
                
 1C12 A900              LDA     #0
 1C14 85DF              STA     SCRIPT          ; DISABLE SCRIPTING
                
 1C16 A2E7              LDX     #LOW ERRM
 1C18 A91B              LDA     #HIGH ERRM
 1C1A A012              LDY     #ERRML
 1C1C 20C21F            JSR     DLINE
                
 1C1F                   ; FALL THROUGH ...
                
 1C1F                   ; ----
 1C1F                   ; QUIT
 1C1F                   ; ----
                
 1C1F 20C71C    ZQUIT:  JSR     ZCRLF
                
 1C22 A231              LDX     #LOW TORES
 1C24 A91C              LDA     #HIGH TORES
 1C26 A00D              LDY     #TORESL
 1C28 20C21F            JSR     DLINE           ; "END OF STORY"
                
 1C2B 20C71C            JSR     ZCRLF
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  85    
--- GAME I/O: ATARI ---

 1C2E 4C2E1C    FREEZE: JMP     FREEZE
                
 1C31 456E6420  TORES:  DB      "End of story."
 000D           TORESL  EQU     $-TORES
                
 1C3E                   ; -------
 1C3E                   ; RESTART
 1C3E                   ; -------
                
 1C3E 207B23    ZSTART: JSR     SIDE1           ; PROMPT FOR SIDE 1
                
 1C41 AD1126            LDA     ZBEGIN+ZSCRIP+1 ; SAVE THE STATE OF
 1C44 2901              AND     #%00000001      ; THE SCRIPT FLAG
 1C46 8DD81F            STA     SFLAG           ; (BM 5/14/85)
                
 1C49 4CAE0C            JMP     WARM            ; AND DO A WARMSTART
                
 1C4C                   ; --------------------
 1C4C                   ; PRINT VERSION NUMBER
 1C4C                   ; --------------------
                
 1C4C 41746172  VERS:   DB      "Atari Version D"
 000F           VERSL   EQU     $-VERS
                
 1C5B 20C71C    VERNUM: JSR     ZCRLF           ; FLUSH BUFFER
                
 1C5E A24C              LDX     #LOW VERS
 1C60 A91C              LDA     #HIGH VERS
 1C62 A00F              LDY     #VERSL
 1C64 20C21F            JSR     DLINE           ; "VERSION X"
                
 1C67 4C7B23            JMP     SIDE1           ; GET SIDE 1 & RETURN
                
 1C6A                   ; --------------------------
 1C6A                   ; RETURN TOP RAM PAGE IN [A]
 1C6A                   ; --------------------------
                
 1C6A A200      MEMTOP: LDX     #0
 1C6C 8E00B0            STX     $B000           ; CLEAR THIS CELL
 1C6F AD00B0            LDA     $B000           ; TEST IT
 1C72 D00B              BNE     NOT48K          ; NOT 48K IF NZ
 1C74 CE00B0            DEC     $B000           ; SET TO $FF
 1C77 AD00B0            LDA     $B000           ; TEST AGAIN
 1C7A 1003              BPL     NOT48K          ; NOT 48K IF POSITIVE
 1C7C A9BB              LDA     #$BB            ; ELSE RETURN 48K
 1C7E 60                RTS
                
 1C7F 8A        NOT48K: TXA                     ; RETURN 0 IN [A]
 1C80 60                RTS
                
 1C81                   ; --------------------------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  86    
--- GAME I/O: ATARI ---

 1C81                   ; RETURN RANDOM BYTES IN [A] & [X]
 1C81                   ; --------------------------------
                
 1C81 AD0AD2    RANDOM: LDA     MRAND
 1C84 EA                NOP
 1C85 AE0AD2            LDX     MRAND
 1C88 60                RTS
                
 1C89                   ; -------------------
 1C89                   ; Z-PRINT A CHARACTER
 1C89                   ; -------------------
                
 1C89                   ; ENTRY: ASCII CHAR IN [A]
                
 1C89 C90D      COUT:   CMP     #$0D            ; IF ASCII EOL,
 1C8B F03A              BEQ     ZCRLF           ; DO IT!
 1C8D C920              CMP     #SPACE          ; IGNORE ALL OTHER
 1C8F 900B              BCC     CEX             ; CONTROLS
                
 1C91 A6DD              LDX     LENGTH          ; ELSE GET LINE POINTER
 1C93 9D800A            STA     LBUFF,X         ; ADD CHAR TO BUFFER
 1C96 E027              CPX     #XSIZE          ; END OF LINE?
 1C98 B003              BCS     FLUSH           ; YES, FLUSH THE LINE
 1C9A E6DD              INC     LENGTH          ; ELSE UPDATE POINTER
                
 1C9C 60        CEX:    RTS
                
 1C9D                   ; -------------------
 1C9D                   ; FLUSH OUTPUT BUFFER
 1C9D                   ; -------------------
                
 1C9D                   ; ENTRY: LENGTH OF BUFFER IN [X]
                
 1C9D A920      FLUSH:  LDA     #SPACE
                
 1C9F DD800A    FL0:    CMP     LBUFF,X         ; FIND LAST SPACE CHAR
 1CA2 F005              BEQ     FL1             ; IN THE LINE
 1CA4 CA                DEX
 1CA5 D0F8              BNE     FL0             ; IF NONE FOUND,
 1CA7 A227              LDX     #XSIZE          ; FLUSH ENTIRE LINE
                
 1CA9 86DE      FL1:    STX     OLDLEN          ; SAVE OLD LINE POS HERE
 1CAB 86DD              STX     LENGTH          ; MAKE IT THE NEW LINE LENGTH
                
 1CAD 20C71C            JSR     ZCRLF           ; PRINT LINE UP TO LAST SPACE
                
 1CB0                   ; START NEW LINE WITH REMAINDER OF OLD
                
 1CB0 A6DE              LDX     OLDLEN          ; GET OLD LINE POS
 1CB2 A000              LDY     #0              ; START NEW LINE AT BEGINNING
 1CB4 E8        FL2:    INX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  87    
--- GAME I/O: ATARI ---

 1CB5 E027              CPX     #XSIZE          ; CONTINUE IF
 1CB7 9005              BCC     FL3             ; INSIDE OR
 1CB9 F003              BEQ     FL3             ; AT END OF LINE
 1CBB 84DD              STY     LENGTH          ; ELSE SET NEW LINE LENGTH
 1CBD 60                RTS
                
 1CBE BD800A    FL3:    LDA     LBUFF,X         ; GET CHAR FROM OLD LINE
 1CC1 99800A            STA     LBUFF,Y         ; MOVE TO START OF NEW LINE
 1CC4 C8                INY                     ; UPDATE LENGTH OF NEW LINE
 1CC5 D0ED              BNE     FL2
                
 1CC7                   ; ---------------
 1CC7                   ; CARRIAGE RETURN
 1CC7                   ; ---------------
                
 1CC7 E6E0      ZCRLF:  INC     LINCNT          ; NEW LINE GOING OUT
 1CC9 A5E0              LDA     LINCNT          ; IS IT TIME TO
 1CCB C5E1              CMP     LMAX            ; PRINT "MORE" YET?
 1CCD 9028              BCC     CR1             ; NO, CONTINUE
                
 1CCF                   ; SCREEN FULL; PRINT "MORE"
                
 1CCF 20201D            JSR     ZUSL            ; UPDATE STATUS LINE
                
 1CD2 A900              LDA     #0
 1CD4 85E0              STA     LINCNT          ; RESET LINE COUNTER
                
 1CD6 A205              LDX     #5              ; POKE "[MORE]" INTO BOTTOM LEFT CORNER
 1CD8 BD1A1D    ZCRMP:  LDA     MORE,X
 1CDB 9DD8BF            STA     SCREEN+920,X
 1CDE CA                DEX
 1CDF 10F7              BPL     ZCRMP
                
 1CE1 A9FF              LDA     #$FF            ; WAIT FOR ANY KEY
 1CE3 8DFC02            STA     CH
 1CE6 ADFC02    ZCR0:   LDA     CH
 1CE9 C9FF              CMP     #$FF
 1CEB F0F9              BEQ     ZCR0
                
 1CED A205              LDX     #5              ; ERASE "[MORE]"
 1CEF A900              LDA     #0
 1CF1 9DD8BF    ZCRME:  STA     SCREEN+920,X
 1CF4 CA                DEX
 1CF5 10FA              BPL     ZCRME
                
 1CF7 A6DD      CR1:    LDX     LENGTH
 1CF9 A99B              LDA     #EOL            ; INSTALL EOL AT
 1CFB 9D800A            STA     LBUFF,X         ; END OF CURRENT LINE
 1CFE E6DD              INC     LENGTH          ; UPDATE LINE LENGTH
                
 1D00 A4DD      LINOUT: LDY     LENGTH          ; IF BUFFER EMPTY,


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  88    
--- GAME I/O: ATARI ---

 1D02 F015              BEQ     LINEX           ; DON'T PRINT ANYTHING
 1D04 84EA              STY     PRLEN           ; SAVE LENGTH HERE FOR "PPRINT"
                
 1D06 A200              LDX     #0              ; SEND CONTENTS OF [LBUFF]
 1D08 BD800A    LOUT:   LDA     LBUFF,X         ; TO SCREEN
 1D0B 20EB1E            JSR     CHAR
 1D0E E8                INX
 1D0F 88                DEY
 1D10 D0F6              BNE     LOUT
                
 1D12 20D91F            JSR     PPRINT          ; PRINT [LBUFF] IF ENABLED
 1D15 A900              LDA     #0              ; RESET LINE LENGTH
 1D17 85DD              STA     LENGTH          ; TO ZERO
                
 1D19 60        LINEX:  RTS                     ; AND RETURN
                
 1D1A BB        MORE:   DB      59+128          ; INVERSE "[MORE]"
 1D1B AD                DB      45+128          ; IN ATARI SCREEN CODE
 1D1C AF                DB      47+128
 1D1D B2                DB      50+128
 1D1E A5                DB      37+128
 1D1F BD                DB      61+128
                
 1D20                   ; ----------------------
 1D20                   ; UPDATE THE STATUS LINE
 1D20                   ; ----------------------
                
 1D20 A555      ZUSL:   LDA     COLCRS+LO       ; SAVE THE
 1D22 48                PHA                     ; CURRENT
 1D23 A554              LDA     ROWCRS          ; CURSOR
 1D25 48                PHA                     ; POSITION
                
 1D26 A5DD              LDA     LENGTH          ; SAVE ALL
 1D28 48                PHA                     ; STRING-PRINTING
 1D29 A59E              LDA     MPCH            ; VARIABLES
 1D2B 48                PHA
 1D2C A59D              LDA     MPCM
 1D2E 48                PHA
 1D2F A59C              LDA     MPCL
 1D31 48                PHA
 1D32 A5CA              LDA     TSET
 1D34 48                PHA
 1D35 A5C9              LDA     PSET
 1D37 48                PHA
 1D38 A5CF              LDA     ZWORD+HI
 1D3A 48                PHA
 1D3B A5CE              LDA     ZWORD+LO
 1D3D 48                PHA
 1D3E A5CD              LDA     ZFLAG
 1D40 48                PHA
 1D41 A5DB              LDA     DIGITS


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  89    
--- GAME I/O: ATARI ---

 1D43 48                PHA
                
 1D44 A227              LDX     #XSIZE
 1D46 BD800A    USL0:   LDA     LBUFF,X         ; MOVE CONTENTS OF [LBUFF]
 1D49 9D200A            STA     BUFSAV,X        ; TO [BUFSAV]
 1D4C A920              LDA     #SPACE          ; CLEAR
 1D4E 9D800A            STA     LBUFF,X         ; [LBUFF] WITH SPACES
 1D51 CA                DEX
 1D52 10F2              BPL     USL0
                
 1D54 A900              LDA     #0
 1D56 85DD              STA     LENGTH          ; RESET LINE LENGTH
 1D58 85DF              STA     SCRIPT          ; DISABLE SCRIPTING
 1D5A 8555              STA     COLCRS+LO       ; HOME THE
 1D5C 8554              STA     ROWCRS          ; CURSOR
                
 1D5E                   ; PRINT ROOM DESCRIPTION
                
 1D5E A910              LDA     #16             ; GLOBAL VAR #16 (ROOM ID)
 1D60 20C10E            JSR     GETVRG          ; GET IT INTO [VALUE]
 1D63 A58C              LDA     VALUE+LO
 1D65 20DF11            JSR     PRNTDC          ; PRINT SHORT ROOM DESCRIPTION
                
 1D68 A917              LDA     #23             ; MOVE LINE INDEX UP
 1D6A 85DD              STA     LENGTH          ; TO TIME/SCORE POSITION
                
 1D6C A920              LDA     #SPACE          ; TRUNCATE LONG DESCS
 1D6E 20891C            JSR     COUT            ; WITH A SPACE
                
 1D71 A911              LDA     #17             ; GLOBAL VAR #17 (SCORE/HOURS)
 1D73 20C10E            JSR     GETVRG          ; GET IT INTO [VALUE]
                
 1D76 A5DC              LDA     TIMEFL          ; GET MODE FLAG
 1D78 D032              BNE     DOTIME          ; USE TIME MODE IF NON-ZERO
                
 1D7A                   ; PRINT "SCORE"
                
 1D7A A953              LDA     #'S'
 1D7C 20891C            JSR     COUT
 1D7F A963              LDA     #'c'
 1D81 20891C            JSR     COUT
 1D84 A96F              LDA     #'o'
 1D86 20891C            JSR     COUT
 1D89 A972              LDA     #'r'
 1D8B 20891C            JSR     COUT
 1D8E A965              LDA     #'e'
 1D90 20891C            JSR     COUT
 1D93 A93A              LDA     #':'
 1D95 20891C            JSR     COUT
 1D98 A920              LDA     #SPACE
 1D9A 20891C            JSR     COUT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  90    
--- GAME I/O: ATARI ---

                
 1D9D A58C              LDA     VALUE+LO        ; MOVE SCORE VALUE
 1D9F 85D3              STA     QUOT+LO         ; INTO [QUOT]
 1DA1 A58D              LDA     VALUE+HI        ; FOR PRINTING
 1DA3 85D4              STA     QUOT+HI
 1DA5 202E16            JSR     NUMBER          ; PRINT SCORE VALUE IN DECIMAL
                
 1DA8 A92F              LDA     #'/'            ; PRINT A SLASH
 1DAA D035              BNE     MOVMIN          ; BRANCH ALWAYS
                
 1DAC                   ; PRINT "TIME"
                
 1DAC A954      DOTIME: LDA     #'T'
 1DAE 20891C            JSR     COUT
 1DB1 A969              LDA     #'i'
 1DB3 20891C            JSR     COUT
 1DB6 A96D              LDA     #'m'
 1DB8 20891C            JSR     COUT
 1DBB A965              LDA     #'e'
 1DBD 20891C            JSR     COUT
 1DC0 A93A              LDA     #':'
 1DC2 20891C            JSR     COUT
 1DC5 A920              LDA     #SPACE
 1DC7 20891C            JSR     COUT
                
 1DCA A58C              LDA     VALUE+LO        ; 00 IS REALLY 24
 1DCC D002              BNE     DT0
 1DCE A918              LDA     #24
 1DD0 C90D      DT0:    CMP     #13             ; IS HOURS > 12,
 1DD2 9002              BCC     DT1
 1DD4 E90C              SBC     #12             ; CONVERT TO 1-12
 1DD6 85D3      DT1:    STA     QUOT+LO         ; MOVE FOR PRINTING
 1DD8 A900              LDA     #0
 1DDA 85D4              STA     QUOT+HI         ; CLEAR MSB
 1DDC 202E16            JSR     NUMBER
                
 1DDF A93A              LDA     #':'            ; COLON
                
 1DE1 20891C    MOVMIN: JSR     COUT            ; PRINT SLASH OR COLON
                
 1DE4 A912              LDA     #18             ; GLOBAL VAR #18 (MOVES/MINUTES)
 1DE6 20C10E            JSR     GETVRG          ; GET IT INTO [VALUE]
 1DE9 A58C              LDA     VALUE+LO        ; MOVE TO [QUOT]
 1DEB 85D3              STA     QUOT+LO         ; FOR EVENTUAL PRINTING
 1DED A58D              LDA     VALUE+HI
 1DEF 85D4              STA     QUOT+HI
                
 1DF1 A5DC              LDA     TIMEFL          ; WHICH MODE?
 1DF3 D006              BNE     DOMINS          ; TIME IF NZ
                
 1DF5                   ; PRINT NUMBER OF MOVES


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  91    
--- GAME I/O: ATARI ---

                
 1DF5 202E16            JSR     NUMBER          ; SHOW # MOVES
 1DF8 4C271E            JMP     STATEX          ; ALL DONE
                
 1DFB                   ; PRINT MINUTES
                
 1DFB A58C      DOMINS: LDA     VALUE+LO        ; CHECK MINUTES
 1DFD C90A              CMP     #10             ; IF MORE THAN TEN
 1DFF B005              BCS     DOM0            ; CONTINUE
                
 1E01 A930              LDA     #'0'            ; ELSE PRINT A
 1E03 20891C            JSR     COUT            ; PADDING "0" FIRST
                
 1E06 202E16    DOM0:   JSR     NUMBER          ; SHOW MINUTES
                
 1E09 A920              LDA     #SPACE
 1E0B 20891C            JSR     COUT            ; SEPARATE THINGS
                
 1E0E A911              LDA     #17             ; CHECK "HOURS" AGAIN
 1E10 20C10E            JSR     GETVRG
 1E13 A58C              LDA     VALUE+LO
 1E15 C90C              CMP     #12             ; PAST NOON?
 1E17 B004              BCS     DOPM            ; YES, PRINT "PM"
                
 1E19 A961              LDA     #'a'            ; ELSE PRINT "AM"
 1E1B D002              BNE     DOXM            ; BRANCH ALWAYS
                
 1E1D A970      DOPM:   LDA     #'p'
                
 1E1F 20891C    DOXM:   JSR     COUT
 1E22 A96D              LDA     #'m'
 1E24 20891C            JSR     COUT
                
 1E27                   ; STATUS LINE READY
                
 1E27 A227      STATEX: LDX     #XSIZE
 1E29 A980              LDA     #$80            ; CLEAR THE
 1E2B 9D40BC    STX0:   STA     SCREEN,X        ; STATUS LINE!
 1E2E CA                DEX
 1E2F D0FA              BNE     STX0
 1E31 BD800A    STX1:   LDA     LBUFF,X         ; GET A CHAR FROM [LBUFF]
 1E34 0980              ORA     #%10000000      ; CONVERT TO INVERSE VIDEO
 1E36 20EB1E            JSR     CHAR            ; SEND TO SCREEN
 1E39 E8                INX                     ; LOOP TILL
 1E3A E4DD              CPX     LENGTH          ; ALL CHARS SENT
 1E3C 90F3              BCC     STX1
                
 1E3E A227              LDX     #XSIZE          ; RESTORE OLD [LBUFF]
 1E40 BD200A    STX2:   LDA     BUFSAV,X
 1E43 9D800A            STA     LBUFF,X
 1E46 CA                DEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  92    
--- GAME I/O: ATARI ---

 1E47 10F7              BPL     STX2
                
 1E49 68                PLA                     ; RESTORE ALL
 1E4A 85DB              STA     DIGITS          ; SAVED VARIABLES
 1E4C 68                PLA
 1E4D 85CD              STA     ZFLAG
 1E4F 68                PLA
 1E50 85CE              STA     ZWORD+LO
 1E52 68                PLA
 1E53 85CF              STA     ZWORD+HI
 1E55 68                PLA
 1E56 85C9              STA     PSET
 1E58 68                PLA
 1E59 85CA              STA     TSET
 1E5B 68                PLA
 1E5C 859C              STA     MPCL
 1E5E 68                PLA
 1E5F 859D              STA     MPCM
 1E61 68                PLA
 1E62 859E              STA     MPCH
 1E64 68                PLA
 1E65 85DD              STA     LENGTH
                
 1E67 68                PLA                     ; RESTORE THE
 1E68 8554              STA     ROWCRS          ; CURSOR POSITION
 1E6A 68                PLA
 1E6B 8555              STA     COLCRS+LO
                
 1E6D A2FF              LDX     #$FF
 1E6F 86DF              STX     SCRIPT          ; RE-ENABLE SCRIPTING
 1E71 E8                INX                     ; = 0
 1E72 869F              STX     MPCFLG          ; INVALIDATE [MPC]
 1E74 4CB120            JMP     NEWLOG          ; RESET THE LINE MAP & RETURN
                
                        END
                        INCLUD  MACHINE.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  93    
--- MACHINE-DEPENDENT I/O: ATARI ---

                
 1E77                   ; ----------------------------
 1E77                   ; FETCH ASCII KEYCODE INTO [A]
 1E77                   ; ----------------------------
                
 1E77                   ; EXIT: ASCII IN [A] & [IOCHAR]
                
 1E77 20BD20    BADKEY: JSR     BOOP
 1E7A 4C821E            JMP     GKEY0
                
 1E7D D8        GETKEY: CLD
 1E7E 8A                TXA                     ; SAVE [X] & [Y]
 1E7F 48                PHA
 1E80 98                TYA
 1E81 48                PHA
                
 1E82 A900      GKEY0:  LDA     #0
 1E84 85F6              STA     BLINK+LO        ; LENGTHEN BLINK DELAY
 1E86 85F7              STA     BLINK+HI        ; TO MAXIMUM
                
 1E88 A555              LDA     COLCRS+LO       ; CALC CURSOR X-POS
 1E8A 0A                ASL     A
 1E8B 0A                ASL     A
 1E8C 18                CLC
 1E8D 6930              ADC     #48
 1E8F 8D04D0            STA     HPOSM0
                
 1E92 A554              LDA     ROWCRS          ; CALC CURSOR Y-POS
 1E94 0A                ASL     A
 1E95 0A                ASL     A
 1E96 0A                ASL     A
 1E97 18                CLC
 1E98 6927              ADC     #39
 1E9A A8                TAY                     ; MOVE HERE FOR DRAWING
                
 1E9B A903              LDA     #%00000011      ; FORCE CURSOR "ON"
 1E9D 85F8              STA     CSHAPE
 1E9F 99000B            STA     MISSL,Y         ; AND DRAW IT
                
 1EA2 AEFC02    GKEY1:  LDX     CH              ; CHECK HARDWARE FOR A KEYPRESS
                
 1EA5 E6F6              INC     BLINK+LO
 1EA7 D011              BNE     NOBLIN
 1EA9 E6F7              INC     BLINK+HI
 1EAB D00D              BNE     NOBLIN
                
 1EAD A980              LDA     #$80
 1EAF 85F7              STA     BLINK+HI        ; RESET BLINK TIMER
                
 1EB1 A5F8              LDA     CSHAPE
 1EB3 4903              EOR     #%00000011


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  94    
--- MACHINE-DEPENDENT I/O: ATARI ---

 1EB5 85F8              STA     CSHAPE          ; FLIP CURSOR SHAPE
 1EB7 99000B            STA     MISSL,Y         ; DRAW CURSOR INTO MISSILE RAM
                
 1EBA E0FF      NOBLIN: CPX     #$FF            ; KEY PRESSED?
 1EBC F0E4              BEQ     GKEY1           ; NO, KEEP SCANNING
                
 1EBE A9FF              LDA     #$FF
 1EC0 8DFC02            STA     CH              ; RESET KEY HARDWARE
                
 1EC3 8A                TXA
 1EC4 30B1              BMI     BADKEY          ; REJECT CTRL KEYS
                
 1EC6 BD2621            LDA     ATASCI,X        ; GET CODE INTO [A]
 1EC9 C99B              CMP     #EOL            ; WAS IT EOL?
 1ECB F003              BEQ     CLICK           ; OKAY IF SO
 1ECD AA                TAX                     ; ANY OTHER NEGATIVE CODE
 1ECE 30A7              BMI     BADKEY          ; IS ILLEGAL
                
 1ED0                   ; ERASE CURSOR, "CLICK" THE SPEAKER
                
 1ED0 85E2      CLICK:  STA     IOCHAR          ; SAVE KEYCODE
 1ED2 A900              LDA     #0
 1ED4 99000B            STA     MISSL,Y         ; ERASE CURSOR
                
 1ED7 A080              LDY     #$80
 1ED9 8C1FD0    CLK0:   STY     CONSOL
 1EDC A208              LDX     #8
 1EDE CA        CLK1:   DEX
 1EDF D0FD              BNE     CLK1
 1EE1 88                DEY
 1EE2 D0F5              BNE     CLK0
                
 1EE4 68                PLA                     ; RESTORE
 1EE5 A8                TAY                     ; EVERYTHING
 1EE6 68                PLA
 1EE7 AA                TAX
 1EE8 A5E2              LDA     IOCHAR          ; GET CHAR INTO [A]
 1EEA 60                RTS                     ; AND RETURN IT
                
 1EEB                   ; -----------------
 1EEB                   ; PRINT CHAR IN [A]
 1EEB                   ; -----------------
                
 1EEB 85E2      CHAR:   STA     IOCHAR          ; SAVE HERE
 1EED 8A                TXA                     ; SAVE [X] AND [Y]
 1EEE 48                PHA
 1EEF 98                TYA
 1EF0 48                PHA
                
 1EF1 A454              LDY     ROWCRS          ; Y-POS INTO [Y]
 1EF3 A655              LDX     COLCRS+LO       ; X-POS INTO [X]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  95    
--- MACHINE-DEPENDENT I/O: ATARI ---

                
 1EF5 A5E2              LDA     IOCHAR          ; RESTORE CHAR
 1EF7 C99B              CMP     #EOL            ; IS IT EOL?
 1EF9 F056              BEQ     OUTEOL          ; YES, SPECIAL HANDLING
 1EFB C90D              CMP     #$0D            ; ALSO CHECK FOR
 1EFD F052              BEQ     OUTEOL          ; ASCII EOL
                
 1EFF                   ; HANDLE A NON-EOL CHAR
                
 1EFF C016              CPY     #YSIZE-1        ; ON LAST SCREEN LINE?
 1F01 9037              BCC     NOSCRL          ; NO, NO SCROLL NEEDED
 1F03 E027              CPX     #XSIZE          ; LAST CHAR ON LINE?
 1F05 9033              BCC     NOSCRL          ; NO, DON'T SCROLL
                
 1F07                   ; SCROLL THE SCREEN
                
 1F07 88        DOSCRL: DEY                     ; PUSH CURSOR UP ONE LINE
 1F08 8454              STY     ROWCRS
                
 1F0A A6E3              LDX     SLINE           ; GET CURRENT SCROLL LINE
                
 1F0C E017      SRL0:   CPX     #YSIZE
 1F0E F020              BEQ     SRL2            ; SCROLL DONE
                
 1F10 BDF620            LDA     LOLINE,X        ; GET ADDR OF DEST LINE
 1F13 85E7              STA     LTO+LO          ; INTO [LTO]
 1F15 BD0E21            LDA     HILINE,X
 1F18 85E8              STA     LTO+HI
                
 1F1A E8                INX
 1F1B BDF620            LDA     LOLINE,X        ; GET ADDR OF SOURCE LINE
 1F1E 85E5              STA     LFROM+LO        ; INTO [LFROM]
 1F20 BD0E21            LDA     HILINE,X
 1F23 85E6              STA     LFROM+HI
                
 1F25 A027              LDY     #XSIZE
 1F27 B1E5      SRL1:   LDA     (LFROM),Y       ; MOVE SOURCE LINE
 1F29 91E7              STA     (LTO),Y         ; TO DEST LINE
 1F2B 88                DEY
 1F2C 10F9              BPL     SRL1
                
 1F2E 30DC              BMI     SRL0            ; LOOP TILL [X] = YSIZE
                
 1F30 A227      SRL2:   LDX     #XSIZE
 1F32 A900              LDA     #0
 1F34 9DD8BF    SRL3:   STA     SCREEN+920,X    ; CLEAR LAST LINE
 1F37 CA                DEX                     ; OF SCREEN RAM
 1F38 10FA              BPL     SRL3
                
 1F3A A5E2      NOSCRL: LDA     IOCHAR          ; RESTORE CHAR
 1F3C A20B              LDX     #$0B            ; CIO "PUT CHAR"


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  96    
--- MACHINE-DEPENDENT I/O: ATARI ---

 1F3E 8E4203            STX     ICCOM           ; COMMAND
 1F41 A200              LDX     #0              ; IOCB #0 (E:)
 1F43 8E4803            STX     ICBLEN+LO       ; ZERO THE
 1F46 8E4903            STX     ICBLEN+HI       ; BUFFER LENGTH
 1F49 2056E4            JSR     CIOV            ; SEND IT OUT!
                
 1F4C 68                PLA                     ; RESTORE [X] AND [Y]
 1F4D A8                TAY
 1F4E 68                PLA
 1F4F AA                TAX
 1F50 60                RTS
                
 1F51                   ; HANDLE EOL
                
 1F51 A99B      OUTEOL: LDA     #$9B            ; MAKE SURE [IOCHAR]
 1F53 85E2              STA     IOCHAR          ; IS AN ATASCII EOL
 1F55 C016              CPY     #YSIZE-1        ; LAST SCREEN LINE?
 1F57 90E1              BCC     NOSCRL          ; NO, DON'T SCROLL
 1F59 B0AC              BCS     DOSCRL          ; ELSE SCROLL
                
 1F5B                   ; ---------------------
 1F5B                   ; FETCH A LINE OF INPUT
 1F5B                   ; ---------------------
                
 1F5B                   ; ENTRY: ABS ADDR OF READ BUFFER IN [ARG1]
 1F5B                   ; EXIT: # CHARS READ IN [A]
                
 1F5B 20001D    INPUT:  JSR     LINOUT          ; FLUSH [LBUFF]
                
 1F5E A0FF              LDY     #$FF
 1F60 8CFC02            STY     CH              ; CLEAR KEYBOARD
 1F63 C8                INY                     ; = 0
 1F64 84E0              STY     LINCNT          ; RESET LINE COUNT
                
 1F66 207D1E    INLOOP: JSR     GETKEY          ; GET ASCII INTO [A] AND [IOCHAR]
                
 1F69 C99B              CMP     #EOL            ; EOL?
 1F6B F02A              BEQ     ENDLIN          ; LINE DONE IF SO
 1F6D C97E              CMP     #BACKSP         ; BACKSPACE?
 1F6F F01C              BEQ     BACKUP          ; SPECIAL HANDLING
                
 1F71 99800A            STA     LBUFF,Y         ; ELSE ADD CHAR TO INPUT BUFFER
 1F74 C8                INY                     ; NEXT POSITION IN LINE
                
 1F75 20EB1E    SHOWIT: JSR     CHAR            ; SEND TO SCREEN
                
 1F78 C04D              CPY     #77             ; 2 SCREEN LINES FULL?
 1F7A 90EA              BCC     INLOOP          ; NO, GET ANOTHER CHAR
                
 1F7C                   ; HANDLE LINE OVERFLOW
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  97    
--- MACHINE-DEPENDENT I/O: ATARI ---

 1F7C 207D1E    NOMORE: JSR     GETKEY
 1F7F C99B              CMP     #EOL            ; IF EOL,
 1F81 F014              BEQ     ENDLIN          ; WRAP UP THE LINE
 1F83 C97E              CMP     #BACKSP         ; BACKSPACE
 1F85 F006              BEQ     BACKUP          ; IS OKAY TOO
 1F87 20BD20            JSR     BOOP            ; ELSE COMPLAIN
 1F8A 4C7C1F            JMP     NOMORE          ; AND INSIST
                
 1F8D                   ; HANDLE BACKSPACE
                
 1F8D 88        BACKUP: DEY                     ; BACK UP THE POINTER
 1F8E 10E5              BPL     SHOWIT          ; SEND BS IF NOT START OF LINE
 1F90 20BD20            JSR     BOOP            ; ELSE SCREAM WITH PAIN
 1F93 A000              LDY     #0              ; RESET POINTER
 1F95 F0CF              BEQ     INLOOP          ; AND WAIT FOR SOMETHING BETTER
                
 1F97                   ; HANDLE END OF LINE
                
 1F97 99800A    ENDLIN: STA     LBUFF,Y         ; SHIP EOL TO BUFFER
 1F9A C8                INY                     ; UPDATE INDEX
 1F9B 84C2              STY     LINLEN          ; SAVE HERE FOR "READ"
 1F9D 84EA              STY     PRLEN           ; AND HERE FOR "PPRINT"
                
 1F9F 20EB1E            JSR     CHAR            ; AND SEND EOL TO SCREEN
                
 1FA2                   ; MOVE [LBUFF] TO [ARG1] W/LC CONVERSION
                
 1FA2 B97F0A    LEX0:   LDA     LBUFF-1,Y       ; GET A CHAR FROM [LBUFF]
                
 1FA5 C99B              CMP     #EOL            ; ATASCII EOL?
 1FA7 D004              BNE     LEX1            ; IF SO,
 1FA9 A90D              LDA     #$0D            ; CONVERT TO ASCII
 1FAB D00A              BNE     LEX2
                
 1FAD C941      LEX1:   CMP     #'A'            ; IF CHAR IS ALPHA,
 1FAF 9006              BCC     LEX2            ; CONVERT TO LOWER CASE
 1FB1 C95B              CMP     #'Z'+1
 1FB3 B002              BCS     LEX2
 1FB5 6920              ADC     #$20
                
 1FB7 9182      LEX2:   STA     (ARG1),Y        ; MOVE CHAR TO INPUT BUFFER AT [ARG1]
 1FB9 88                DEY                     ; LOOP TILL
 1FBA 10E6              BPL     LEX0            ; ALL CHARS MOVED
                
 1FBC 20D91F            JSR     PPRINT          ; SCRIPT [LBUFF] IF ENABLED
                
 1FBF A5C2              LDA     LINLEN          ; RESTORE # CHARS
 1FC1 60                RTS                     ; INTO [A]
                
 1FC2                   ; -----------------------
 1FC2                   ; DIRECT PRINT LINE [X/A]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  98    
--- MACHINE-DEPENDENT I/O: ATARI ---

 1FC2                   ; -----------------------
                
 1FC2                   ; ENTRY: STRING ADDRESS IN [X/A] (LSB/MSB)
 1FC2                   ;        STRING LENGTH IN [Y]
                
 1FC2 8ECB1F    DLINE:  STX     STRING+LO       ; DROP STRING ADDRESS
 1FC5 8DCC1F            STA     STRING+HI       ; INTO DUMMY BYTES
                
 1FC8 A200              LDX     #0              ; INIT CHAR-FETCH INDEX
                
 1FCA BD        DOUT:   DB      $BD             ; 6502 "LDA nnnn,X" OPCODE
 1FCB 0000      STRING: DW      $0000           ; DUMMY OPERAND BYTES
 1FCD 20EB1E            JSR     CHAR
 1FD0 E8                INX
 1FD1 88                DEY                     ; LOOP TILL
 1FD2 D0F6              BNE     DOUT            ; OUT OF CHARS
                
 1FD4 60                RTS
                
 1FD5                   ; -----------------------
 1FD5                   ; SEND [LBUFF] TO PRINTER
 1FD5                   ; -----------------------
                
 1FD5                   ; ENTRY: LENTH OF LINE IN [PRLEN]
                
 1FD5 503A      PNAME:  DB      "P:"            ; FILENAME FOR PRINTER
 1FD7 9B                DB      EOL
                
 1FD8 00        SFLAG:  DB      0               ; PREVIOUS SCRIPTING STATE
                
 1FD9 A5DF      PPRINT: LDA     SCRIPT          ; SCRIPTING INTERNALLY ENABLED?
 1FDB F06B              BEQ     PEX             ; NO, SCRAM IMMEDIATELY
                
 1FDD AD1126            LDA     ZBEGIN+ZSCRIP+1 ; CHECK SCRIPT FLAG
 1FE0 2901              AND     #%00000001      ; SCRIPTING ON?
 1FE2 F064              BEQ     PEX             ; NO, EXIT
                
 1FE4 A5E9              LDA     PSTAT           ; CHECK PRINTER STATUS
 1FE6 3060              BMI     PEX             ; CAN'T OPEN IF NEGATIVE
 1FE8 D02F              BNE     PP1             ; ALREADY OPEN, SCRIPT THE LINE
                
 1FEA                   ; OPEN THE PRINTER FOR OUTPUT
                
 1FEA 203E20            JSR     CLOSEP          ; CLOSE IOCB #1 FIRST FOR SAFETY
                
 1FED A210              LDX     #$10            ; IOCB #1 (P:)
 1FEF A9D5              LDA     #LOW PNAME      ; POINT
 1FF1 9D4403            STA     ICBADR+LO,X     ; TO
 1FF4 A91F              LDA     #HIGH PNAME     ; P:
 1FF6 9D4503            STA     ICBADR+HI,X     ; FILENAME
 1FF9 A903              LDA     #$03            ; CIO "OPEN"


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE  99    
--- MACHINE-DEPENDENT I/O: ATARI ---

 1FFB 9D4203            STA     ICCOM,X         ; COMMAND
 1FFE A908              LDA     #$08            ; SET CHANNEL
 2000 9D4A03            STA     ICAUX1,X        ; FOR WRITE-ONLY
 2003 A900              LDA     #0
 2005 9D4B03            STA     ICAUX2,X        ; ZERO THIS BYTE
 2008 2056E4            JSR     CIOV            ; OPEN IT!
 200B 98                TYA                     ; STATUS CODE IN [Y]
 200C 302C              BMI     BADP            ; ERROR IF NEGATIVE
                
 200E A970              LDA     #$70
 2010 8510              STA     POKMSK
 2012 8D0ED2            STA     IRQEN           ; DISABLE BREAK KEY
                
 2015 A901              LDA     #1              ; SET [PSTAT]
 2017 85E9              STA     PSTAT           ; TO INDICATE "PRINTER READY"
                
 2019                   ; PRINT [LBUFF]
                
 2019 A210      PP1:    LDX     #$10            ; IOCB #1 (P:)
 201B A980              LDA     #LOW LBUFF      ; TELL CIO
 201D 9D4403            STA     ICBADR+LO,X     ; WHERE
 2020 A90A              LDA     #HIGH LBUFF     ; [LBUFF]
 2022 9D4503            STA     ICBADR+HI,X     ; IS HIDING
 2025 A5EA              LDA     PRLEN           ; # CHARS TO PRINT
 2027 9D4803            STA     ICBLEN+LO,X
 202A A900              LDA     #0              ; CLEAR THE
 202C 9D4903            STA     ICBLEN+HI,X     ; MSB OF LINE LENGTH
 202F A90B              LDA     #$0B            ; CIO "PUT BUFFER" COMMAND (BM 4/9/85)
 2031 9D4203            STA     ICCOM,X         ; COMMAND
 2034 2056E4            JSR     CIOV
 2037 98                TYA
 2038 100E              BPL     PEX             ; EXIT IF NO ERROR
                
 203A                   ; HANDLE PRINTER ERROR
                
 203A A9FF      BADP:   LDA     #$FF            ; SET PRINTER STATUS
 203C 85E9              STA     PSTAT           ; TO "CAN'T OPEN"
                
 203E                   ; CLOSE PRINTER CHANNEL (IOCB #1)
                
 203E A210      CLOSEP: LDX     #$10            ; IOCB #1 (P:)
 2040 A90C              LDA     #$0C            ; CIO "CLOSE"
 2042 9D4203            STA     ICCOM,X         ; COMMAND
 2045 2056E4            JSR     CIOV            ; CLOSE THE CHANNEL
                
 2048 60        PEX:    RTS
                
 2049                   ; ------------
 2049                   ; SPLIT SCREEN
 2049                   ; ------------
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 100    
--- MACHINE-DEPENDENT I/O: ATARI ---

 2049                   ; SPLIT SCREEN AT LINE [ARG1]
 2049                   ; DISABLE SPLIT IF [ARG1] = 0
 2049                   ; IGNORE IF SPLIT ALREADY ENABLED OR [ARG1] >= 20
                
 2049 A682      ZSPLIT: LDX     ARG1+LO         ; IF [ARG1] = 0,
 204B F02F              BEQ     OFFSPL          ; TURN OFF SPLIT SCREEN
                
 204D A5E4              LDA     SPSTAT          ; SPLIT ALREADY ENABLED?
 204F D02A              BNE     SPLEX           ; IGNORE REQUEST IF SO
                
 2051 E014              CPX     #20             ; IF [ARG1] >= 20,
 2053 B026              BCS     SPLEX           ; IGNORE
                
 2055 E8                INX
 2056 86E3              STX     SLINE           ; ELSE SET NEW SPLIT LINE
 2058 86E4              STX     SPSTAT          ; SET "SPLIT ENABLED" FLAG
                
 205A BDF620    SPL0:   LDA     LOLINE,X        ; MAKE [LFROM] POINT TO
 205D 85E5              STA     LFROM+LO        ; LINE [X] IN WINDOW
 205F BD0E21            LDA     HILINE,X
 2062 85E6              STA     LFROM+HI
                
 2064 A027              LDY     #XSIZE          ; CLEAR LINE [X]
 2066 A900              LDA     #0
 2068 91E5      SPL1:   STA     (LFROM),Y
 206A 88                DEY
 206B 10FB              BPL     SPL1
                
 206D CA                DEX                     ; DONE ALL LINES?
 206E D0EA              BNE     SPL0            ; LOOP TILL WINDOW CLEARED
 2070 86E0              STX     LINCNT          ; RESET LINE COUNT TO ZERO
                
 2072 A916      SPCALC: LDA     #YSIZE-1        ; CALCULATE # LINES TO SCROLL
 2074 38                SEC                     ; BEFORE "MORE" APPEARS:
 2075 E5E3              SBC     SLINE           ; LMAX = YSIZE-SLINE-1
 2077 85E1              STA     LMAX
 2079 C6E1              DEC     LMAX
                
 207B 60        SPLEX:  RTS
                
 207C                   ; --------------------
 207C                   ; DISABLE SPLIT SCREEN
 207C                   ; --------------------
                
 207C 20A720    OFFSPL: JSR     TOBOT1          ; SET CURSOR TO BOTTOM
                
 207F A201      SPLOFF: LDX     #1
 2081 86E3              STX     SLINE           ; SPLIT AT LINE 1
 2083 CA                DEX                     ; = 0
 2084 86E4              STX     SPSTAT          ; TURN OFF STATUS FLAG
 2086 86E0              STX     LINCNT          ; RESET LINE COUNT


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 101    
--- MACHINE-DEPENDENT I/O: ATARI ---

 2088 A915              LDA     #21
 208A 85E1              STA     LMAX            ; SET MAXIMUM LINE SCROLL
 208C D023              BNE     NEWLOG          ; RESET LINE MAP & RETURN
                
 208E                   ; ------
 208E                   ; SCREEN
 208E                   ; ------
                
 208E                   ; GO TO TOP WINDOW IF [A] = 0
 208E                   ; GO TO BOTTOM IF [A] = 1
 208E                   ; IGNORE IF SPLIT NOT ENABLED OR [A] <> 0 OR 1
                
 208E A5E4      ZSCRN:  LDA     SPSTAT          ; IF SPLIT NOT ENABLED,
 2090 F0E9              BEQ     SPLEX           ; IGNORE REQUEST
                
 2092 A582              LDA     ARG1+LO         ; IF [ARG1] = 0,
 2094 0583              ORA     ARG1+HI
 2096 F00C              BEQ     TOBOT0          ; GO TO BOTTOM WINDOW
 2098 C901              CMP     #1              ; IF [ARG1] <> 1,
 209A D0DF              BNE     SPLEX           ; IGNORE THE REQUEST
                
 209C                   ; SET TO TOP WINDOW
                
 209C A015      TOTOP:  LDY     #21             ; TEMPORARILY RESET
 209E 84E1              STY     LMAX            ; [LMAX] TO KILL "MORE"
 20A0 A001              LDY     #1              ; Y-POS = 1
 20A2 D005              BNE     DOSCRN
                
 20A4                   ; SET TO BOTTOM WINDOW
                
 20A4 207220    TOBOT0: JSR     SPCALC          ; RE-CALC [LMAX]
                
 20A7 A017      TOBOT1: LDY     #23             ; Y-POS = 23
                
 20A9 8454      DOSCRN: STY     ROWCRS          ; Y-POS = [Y]
 20AB A900              LDA     #0              ; X-POS = 0
 20AD 8555              STA     COLCRS+LO
 20AF 85E0              STA     LINCNT          ; RESET LINE COUNT
                
 20B1                   ; FALL THROUGH ...
                
 20B1                   ; ----------------------
 20B1                   ; RESET LOGICAL LINE MAP
 20B1                   ; ----------------------
                
 20B1 A9FF      NEWLOG: LDA     #$FF
 20B3 8DB202            STA     LOGMAP
 20B6 8DB302            STA     LOGMAP+1
 20B9 8DB402            STA     LOGMAP+2
 20BC 60                RTS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 102    
--- MACHINE-DEPENDENT I/O: ATARI ---

 20BD                   ; ---------
 20BD                   ; RAZZ USER
 20BD                   ; ---------
                
 20BD A9C8      BOOP:   LDA     #200            ; SET
 20BF 8D00D2            STA     AUDF1           ; FREQUENCY
 20C2 A9AA              LDA     #$AA            ; PURE TONE, VOLUME #10
 20C4 8D01D2            STA     AUDC1
 20C7 A9FC              LDA     #252            ; 4 JIFFY DELAY
 20C9 8514              STA     RTCLOK
 20CB A514      BOOP0:  LDA     RTCLOK
 20CD D0FC              BNE     BOOP0
 20CF 8D01D2            STA     AUDC1           ; SHUT OFF SOUND
 20D2 60                RTS
                
 20D3                   ; ------------
 20D3                   ; CLEAR SCREEN
 20D3                   ; ------------
                
 20D3 A940      CLS:    LDA     #LOW SCREEN
 20D5 858E              STA     I+LO
 20D7 A9BC              LDA     #HIGH SCREEN    ; POINT [I] TO
 20D9 858F              STA     I+HI            ; SCREEN RAM
                
 20DB A900              LDA     #0
 20DD 85DD              STA     LENGTH          ; RESET LINE LENGTH
 20DF A8                TAY
 20E0 A204              LDX     #4              ; CLEAR 4 PAGES
 20E2 918E      CLS0:   STA     (I),Y           ; FOR SCREEN
 20E4 C8                INY
 20E5 D0FB              BNE     CLS0
 20E7 E68F              INC     I+HI            ; POINT TO NEXT PAGE
 20E9 CA                DEX                     ; 4 PAGES DONE?
 20EA D0F6              BNE     CLS0            ; LOOP TILL EMPTY
                
 20EC A001              LDY     #1              ; SET Y-POS TO 1
 20EE 8454              STY     ROWCRS
 20F0 88                DEY                     ; X-POS TO 0
 20F1 8455              STY     COLCRS+LO
 20F3 4C7F20            JMP     SPLOFF          ; DISABLE SPLIT-SCREEN & RETURN
                
 20F6                   ; -------------------
 20F6                   ; LINE ADDRESS TABLES
 20F6                   ; -------------------
                
 20F6 406890B8  LOLINE: DB      $40,$68,$90,$B8,$E0,$08,$30,$58
 20FE 80A8D0F8          DB      $80,$A8,$D0,$F8,$20,$48,$70,$98
 2106 C0E81038          DB      $C0,$E8,$10,$38,$60,$88,$B0,$D8
                
 210E BCBCBCBC  HILINE: DB      $BC,$BC,$BC,$BC,$BC,$BD,$BD,$BD
 2116 BDBDBDBD          DB      $BD,$BD,$BD,$BD,$BE,$BE,$BE,$BE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 103    
--- MACHINE-DEPENDENT I/O: ATARI ---

 211E BEBEBFBF          DB      $BE,$BE,$BF,$BF,$BF,$BF,$BF,$BF
                
 2126                   ; ------------------------
 2126                   ; ATASCII CONVERSION TABLE
 2126                   ; ------------------------
                
 2126 6C6A3BFF  ATASCI: DB      $6C,$6A,$3B,$FF,$FF,$6B,$FF,$FF         ; UNSHIFTED
 212E 6FFF7075          DB      $6F,$FF,$70,$75,$9B,$69,$2D,$FF
 2136 76FF63FF          DB      $76,$FF,$63,$FF,$FF,$62,$78,$7A
 213E 34FF3336          DB      $34,$FF,$33,$36,$FF,$35,$32,$31
 2146 2C202E6E          DB      $2C,$20,$2E,$6E,$FF,$6D,$2F,$FF
 214E 72FF6579          DB      $72,$FF,$65,$79,$FF,$74,$77,$71
 2156 39FF3037          DB      $39,$FF,$30,$37,$7E,$38,$FF,$FF
 215E 666864FF          DB      $66,$68,$64,$FF,$FF,$67,$73,$61
                
 2166 4C4A3AFF          DB      $4C,$4A,$3A,$FF,$FF,$4B,$FF,$FF         ; SHIFTED
 216E 4FFF5055          DB      $4F,$FF,$50,$55,$9B,$49,$2D,$FF
 2176 56FF43FF          DB      $56,$FF,$43,$FF,$FF,$42,$58,$5A
 217E 24FF2336          DB      $24,$FF,$23,$36,$FF,$35,$22,$21
 2186 2C202E4E          DB      $2C,$20,$2E,$4E,$FF,$4D,$3F,$FF
 218E 52FF4559          DB      $52,$FF,$45,$59,$FF,$54,$57,$51
 2196 39FF3027          DB      $39,$FF,$30,$27,$7E,$38,$FF,$FF
 219E 464844FF          DB      $46,$48,$44,$FF,$FF,$47,$53,$41
                
                        END
                        INCLUD  ZDOS.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 104    
--- Z-DOS: ATARI ---

                
 21A6                   ; -----------------------------
 21A6                   ; SET UP SAVE & RESTORE SCREENS
 21A6                   ; -----------------------------
                
 21A6 20C71C    SAVRES: JSR     ZCRLF           ; CLEAR THE LINE BUFFER
 21A9 20D320            JSR     CLS             ; AND THE SCREEN
                
 21AC A900              LDA     #0
 21AE 85DF              STA     SCRIPT          ; DISABLE SCRIPTING
 21B0 8555              STA     COLCRS+LO       ; HOME CURSOR
 21B2 8554              STA     ROWCRS
 21B4 60                RTS
                
 21B5                   ; -----------------
 21B5                   ; DISPLAY A DEFAULT
 21B5                   ; -----------------
                
 21B5                   ; ENTRY: DEFAULT (1-8) IN [A]
                
 21B5 20284465  DEFAL:  DB      " (Default is "
 21C2 2A29203E  DEFNUM: DB      "*) >"
 0011           DEFALL  EQU     $-DEFAL
                
 21C6 18        DODEF:  CLC
 21C7 6931              ADC     #'1'            ; CONVERT TO ASCII 1-9
 21C9 8DC221            STA     DEFNUM          ; INSERT IN STRING
                
 21CC A2B5              LDX     #LOW DEFAL
 21CE A921              LDA     #HIGH DEFAL
 21D0 A011              LDY     #DEFALL
 21D2 4CC21F            JMP     DLINE           ; PRINT THE STRING
                
 21D5                   ; -----------------------------
 21D5                   ; GET SAVE & RESTORE PARAMETERS
 21D5                   ; -----------------------------
                
 21D5 9B        POSIT:  DB      EOL
 21D6 506F7369          DB      "Position 1-5"
 000D           POSITL  EQU     $-POSIT
                
 21E2 9B        WDRIV:  DB      EOL
 21E3 44726976          DB      "Drive 1 or 2"
 000D           WDRIVL  EQU     $-WDRIV
                
 21EF 9B        MIND:   DB      EOL
 21F0 9B                DB      EOL
 21F1 506F7369          DB      "Position "
 21FA 2A3B2044  MPOS:   DB      "*; Drive #"
 2204 2A2E      MDRI:   DB      "*."
 2206 9B                DB      EOL


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 105    
--- Z-DOS: ATARI ---

 2207 41726520          DB      "Are you sure? (Y or N) >"
 0030           MINDL   EQU     $-MIND
                
 221F 9B        INSM:   DB      EOL
 2220 496E7365          DB      "Insert SAVE disk into Drive #"
 223D 2A2E      SAVDRI: DB      "*."
 0020           INSML   EQU     $-INSM
                
 223F 594553    YES:    DB      "YES"
 2242 9B                DB      EOL
 0004           YESL    EQU     $-YES
                
 2243 4E4F      NO:     DB      "NO"
 2245 9B                DB      EOL
 0003           NOL     EQU     $-NO
                
 2246 A2D5      PARAMS: LDX     #LOW POSIT
 2248 A921              LDA     #HIGH POSIT
 224A A00D              LDY     #POSITL
 224C 20C21F            JSR     DLINE           ; "POSITION (1-5)"
                
 224F                   ; GET GAME SAVE POSITION
                
 224F A5F1              LDA     GPOSIT          ; SHOW THE CURRENT
 2251 20C621            JSR     DODEF           ; DEFAULT POSITION
                
 2254 207D1E    GETPOS: JSR     GETKEY          ; WAIT FOR A KEY
 2257 C99B              CMP     #EOL            ; IF [RETURN],
 2259 F00D              BEQ     POSSET          ; USE DEFAULT
 225B 38                SEC
 225C E931              SBC     #'1'            ; ELSE CONVERT ASCII TO BINARY
 225E C905              CMP     #5              ; IF BELOW "5"
 2260 9008              BCC     SETPOS          ; MAKE IT THE NEW DEFAULT
 2262 20BD20            JSR     BOOP            ; ELSE RAZZ
 2265 4C5422            JMP     GETPOS          ; AND TRY AGAIN
                
 2268 A5F1      POSSET: LDA     GPOSIT          ; USE DEFAULT
                
 226A 85F3      SETPOS: STA     TPOSIT          ; USE KEYPRESS
 226C 18                CLC
 226D 6931              ADC     #'1'            ; CONVERT TO ASCII "1"-"5"
 226F 8DFA21            STA     MPOS            ; STORE IN TEMP STRING
 2272 8DD923            STA     SVPOS
 2275 8D7924            STA     RSPOS
 2278 20EB1E            JSR     CHAR            ; AND DISPLAY IT
                
 227B                   ; GET DRIVE ID
                
 227B A2E2              LDX     #LOW WDRIV
 227D A921              LDA     #HIGH WDRIV
 227F A00D              LDY     #WDRIVL


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 106    
--- Z-DOS: ATARI ---

 2281 20C21F            JSR     DLINE           ; "DRIVE 1 OR 2"
                
 2284 A5F2              LDA     GDRIVE          ; SHOW DEFAULT
 2286 20C621            JSR     DODEF
                
 2289 207D1E    GETDRV: JSR     GETKEY          ; GET A KEYPRESS
 228C C99B              CMP     #EOL            ; IF [RETURN],
 228E F00D              BEQ     DRVSET          ; USE DEFAULT
 2290 38                SEC
 2291 E931              SBC     #'1'            ; CONVERT TO BINARY 0 OR 1
 2293 C902              CMP     #2              ; IF WITHIN RANGE,
 2295 9008              BCC     SETDRV          ; SET NEW DEFAULT
 2297 20BD20            JSR     BOOP
 229A 4C8922            JMP     GETDRV          ; ELSE TRY AGAIN
                
 229D A5F2      DRVSET: LDA     GDRIVE          ; USE DEFAULT
                
 229F 85F4      SETDRV: STA     TDRIVE          ; USE [A]
 22A1 18                CLC
 22A2 6931              ADC     #'1'            ; CONVERT TO ASCII 1 OR 2
 22A4 8D3D22            STA     SAVDRI          ; STORE IN DRIVE STRING
 22A7 8D0422            STA     MDRI            ; AND IN TEMP STRING
 22AA 20EB1E            JSR     CHAR            ; AND SHOW NEW SETTING
                
 22AD A2EF              LDX     #LOW MIND       ; SHOW TEMPORARY SETTINGS
 22AF A921              LDA     #HIGH MIND
 22B1 A030              LDY     #MINDL
 22B3 20C21F            JSR     DLINE
                
 22B6                   ; VALIDATE RESPONSES
                
 22B6 A9FF              LDA     #$FF
 22B8 8DFC02            STA     CH
 22BB 207D1E    GETYES: JSR     GETKEY
 22BE C979              CMP     #'y'            ; IF REPLY IS "Y"
 22C0 F022              BEQ     ALLSET          ; ACCEPT RESPONSES
 22C2 C959              CMP     #'Y'
 22C4 F01E              BEQ     ALLSET
 22C6 C99B              CMP     #EOL            ; EOL IS ALSO ACCEPTABLE
 22C8 F01A              BEQ     ALLSET
                
 22CA C96E              CMP     #'n'            ; IF REPLY IS "N"
 22CC F00A              BEQ     NOTSAT          ; RESTATE PARAMETERS
 22CE C94E              CMP     #'N'
 22D0 F006              BEQ     NOTSAT
                
 22D2 20BD20            JSR     BOOP            ; ELSE BOOP
 22D5 4CBB22            JMP     GETYES          ; INSIST ON Y OR N
                
 22D8 A243      NOTSAT: LDX     #LOW NO
 22DA A922              LDA     #HIGH NO


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 107    
--- Z-DOS: ATARI ---

 22DC A003              LDY     #NOL
 22DE 20C21F            JSR     DLINE           ; PRINT "NO"/EOL
 22E1 4C4622            JMP     PARAMS          ; AND TRY AGAIN
                
 22E4 A23F      ALLSET: LDX     #LOW YES
 22E6 A922              LDA     #HIGH YES
 22E8 A004              LDY     #YESL
 22EA 20C21F            JSR     DLINE           ; PRINT "YES"/EOL
                
 22ED A5F4              LDA     TDRIVE          ; MAKE THE TEMPORARY DRIVE
 22EF 85F2              STA     GDRIVE          ; THE DEFAULT DRIVE
 22F1 85F5              STA     DRIVE           ; AND SET [DRIVE] ACCORDINGLY
                
 22F3 E6F5              INC     DRIVE           ; 1-ALIGN THE DRIVE ID
                
 22F5 A6F3              LDX     TPOSIT          ; MAKE THE TEMP POSITION
 22F7 86F1              STX     GPOSIT          ; THE DEFAULT POSITION
                
 22F9                   ; CALC TRACK & SECTOR OF GAME POSITION
                
 22F9 BD2925            LDA     OFFLOS,X        ; INDEX INTO THE OFFSET TABLES
 22FC 85EF              STA     SECTOR+LO       ; SET [SECTOR] = 1ST SECTOR
 22FE BD2E25            LDA     OFFHIS,X        ; IN REQUESTED SAVE POSITION
 2301 85F0              STA     SECTOR+HI
                
 2303 A21F              LDX     #LOW INSM
 2305 A922              LDA     #HIGH INSM
 2307 A020              LDY     #INSML
 2309 20C21F            JSR     DLINE           ; "INSERT SAVE DISK IN DRIVE X."
                
 230C                   ; FALL THROUGH ...
                
 230C                   ; ---------------------
 230C                   ; "PRESS RETURN" PROMPT
 230C                   ; ---------------------
                
 230C 20C71C    RETURN: JSR     ZCRLF
 230F A233              LDX     #LOW RTN
 2311 A923              LDA     #HIGH RTN
 2313 A01B              LDY     #RTNL
 2315 20C21F            JSR     DLINE           ; SHOW PROMPT
                
 2318 20C71C            JSR     ZCRLF
 231B A93E              LDA     #'>'
 231D 20EB1E            JSR     CHAR
                
 2320                   ; ENTRY FOR QUIT/RESTART
                
 2320 A9FF      GETRET: LDA     #$FF
 2322 8DFC02            STA     CH
 2325 207D1E    GTRT0:  JSR     GETKEY          ; WAIT FOR [RETURN]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 108    
--- Z-DOS: ATARI ---

 2328 C99B              CMP     #EOL
 232A F006              BEQ     GTRT1
 232C 20BD20            JSR     BOOP            ; ACCEPT NO
 232F 4C2523            JMP     GTRT0           ; SUBSTITUTES!
                
 2332 60        GTRT1:  RTS
                
 2333 50726573  RTN:    DB      "Press [RETURN] to continue."
 001B           RTNL    EQU     $-RTN
                
 234E                   ; --------------------
 234E                   ; PROMPT FOR GAME DISK
 234E                   ; --------------------
                
 234E 496E7365  GAME:   DB      "Insert Side "
 235A 32206F66  DSIDE:  DB      "2 of the STORY disk into"
 0024           GAMEL   EQU     $-GAME
                
 2372 44726976  GAME2:  DB      "Drive #1."
 0009           GAME2L  EQU     $-GAME2
                
 237B A931      SIDE1:  LDA     #'1'            ; ASK FOR SIDE 1
 237D D002              BNE     DOSIDE
                
 237F A932      SIDE2:  LDA     #'2'            ; ASK FOR SIDE 2
                
 2381 8D5A23    DOSIDE: STA     DSIDE
                
 2384 A901              LDA     #1              ; MAKE SURE WE'RE ON
 2386 85F5              STA     DRIVE           ; THE BOOT DRIVE
                
 2388 20C71C            JSR     ZCRLF
 238B A24E              LDX     #LOW GAME
 238D A923              LDA     #HIGH GAME
 238F A024              LDY     #GAMEL
 2391 20C21F            JSR     DLINE           ; "INSERT STORY DISK"
                
 2394 20C71C            JSR     ZCRLF
 2397 A272              LDX     #LOW GAME2
 2399 A923              LDA     #HIGH GAME2
 239B A009              LDY     #GAME2L
 239D 20C21F            JSR     DLINE           ; "DRIVE #1"
                
 23A0 200C23            JSR     RETURN          ; "PRESS [RETURN] TO CONTINUE:"
                
 23A3 A9FF              LDA     #$FF            ; ENABLE
 23A5 85DF              STA     SCRIPT          ; SCRIPTING
 23A7 60                RTS
                
 23A8                   ; -------------------------
 23A8                   ; SET UP PHONEY STATUS LINE


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 109    
--- Z-DOS: ATARI ---

 23A8                   ; -------------------------
                
 23A8                   ; ENTRY: TEXT SET UP FOR "DLINE"
                
 23A8 20C21F    SROOM:  JSR     DLINE           ; PRINT LINE IN [X/Y/A]
                
 23AB A227              LDX     #39             ; INVERT & BLACKEN TOP LINE
 23AD BD40BC    SRLP:   LDA     SCREEN,X
 23B0 0980              ORA     #%10000000
 23B2 9D40BC            STA     SCREEN,X
 23B5 CA                DEX
 23B6 10F5              BPL     SRLP
 23B8 60                RTS
                
 23B9                   ; ---------
 23B9                   ; SAVE GAME
 23B9                   ; ---------
                
 23B9 53617665  SAV:    DB      "Save Position"
 23C6 9B                DB      EOL
 000E           SAVL    EQU     $-SAV
                
 23C7 9B        SVING:  DB      EOL
 23C8 9B                DB      EOL
 23C9 53617669          DB      "Saving position "
 23D9 2A202E2E  SVPOS:  DB      "* ..."
 23DE 9B                DB      EOL
 0018           SVINGL  EQU     $-SVING
                
 23DF 20A621    ZSAVE:  JSR     SAVRES          ; SET UP SCREEN
                
 23E2 A2B9              LDX     #LOW SAV
 23E4 A923              LDA     #HIGH SAV
 23E6 A00E              LDY     #SAVL
 23E8 20A823            JSR     SROOM           ; "SAVE POSITION"
                
 23EB 204622            JSR     PARAMS          ; GET PARAMETERS
                
 23EE A2C7              LDX     #LOW SVING
 23F0 A923              LDA     #HIGH SVING
 23F2 A018              LDY     #SVINGL
 23F4 20C21F            JSR     DLINE           ; "SAVING POSITION X ..."
                
 23F7                   ; SAVE GAME PARAMETERS IN [BUFSAV]
                
 23F7 AD0226            LDA     ZBEGIN+ZID      ; MOVE GAME ID
 23FA 8D200A            STA     BUFSAV+0        ; INTO 1ST 2 BYTES
 23FD AD0326            LDA     ZBEGIN+ZID+1    ; OF THE AUX LINE BUFFER
 2400 8D210A            STA     BUFSAV+1
                
 2403 A594              LDA     ZSP             ; MOVE [ZSP]


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 110    
--- Z-DOS: ATARI ---

 2405 8D220A            STA     BUFSAV+2        ; TO 3RD BYTE
 2408 A595              LDA     OLDZSP          ; MOVE [OLDZSP]
 240A 8D230A            STA     BUFSAV+3        ; TO 4TH
                
 240D A202              LDX     #2              ; MOVE CONTENTS OF [ZPC]
 240F B596      ZPCSAV: LDA     ZPC,X           ; TO BYTES 5-7
 2411 9D240A            STA     BUFSAV+4,X      ; OF [BUFSAV]
 2414 CA                DEX
 2415 10F8              BPL     ZPCSAV
                
 2417                   ; WRITE [LOCALS]/[BUFSAV] PAGE TO DISK
                
 2417 A90A              LDA     #HIGH LOCALS
 2419 85EE              STA     DBUFF+HI        ; POINT TO THE PAGE
 241B 208125            JSR     PUTDSK          ; AND WRITE IT OUT
 241E 9009              BCC     WSTACK          ; IF SUCCEEDED, WRITE STACK
                
 2420 207F23    BADSAV: JSR     SIDE2           ; ELSE REQUEST STORY DISK
 2423 20D320            JSR     CLS
 2426 4C480F            JMP     PREDF           ; AND FAIL
                
 2429                   ; WRITE CONTENTS OF Z-STACK TO DISK
                
 2429 A905      WSTACK: LDA     #HIGH ZSTAKL    ; POINT TO 1ST PAGE
 242B 85EE              STA     DBUFF+HI
 242D 208125            JSR     PUTDSK          ; WRITE 1ST AND
 2430 B0EE              BCS     BADSAV
 2432 208125            JSR     PUTDSK          ; 2ND PAGE OF Z-STACK
 2435 B0E9              BCS     BADSAV
                
 2437                   ; WRITE ENTIRE GAME PRELOAD TO DISK
                
 2437 A5A3              LDA     ZCODE           ; POINT TO 1ST PAGE
 2439 85EE              STA     DBUFF+HI        ; OF PRELOAD
                
 243B AE0E26            LDX     ZBEGIN+ZPURBT   ; GET # IMPURE PAGES
 243E E8                INX                     ; USE FOR INDEXING
 243F 868E              STX     I+LO
                
 2441 208125    LSAVE:  JSR     PUTDSK
 2444 B0DA              BCS     BADSAV
 2446 C68E              DEC     I+LO
 2448 D0F7              BNE     LSAVE
                
 244A 207F23            JSR     SIDE2           ; PROMPT FOR GAME DISK
 244D 20D320            JSR     CLS
 2450 4C540F            JMP     PREDS           ; ELSE PREDICATE SUCCEEDS
                
 2453                   ; ------------
 2453                   ; RESTORE GAME
 2453                   ; ------------


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 111    
--- Z-DOS: ATARI ---

                
 2453 52657374  RES:    DB      "Restore Position"
 2463 9B                DB      EOL
 0011           RESL    EQU     $-RES
                
 2464 9B        RSING:  DB      EOL
 2465 9B                DB      EOL
 2466 52657374          DB      "Restoring position "
 2479 2A202E2E  RSPOS:  DB      "* ..."
 247E 9B                DB      EOL
 001B           RSINGL  EQU     $-RSING
                
 247F 20A621    ZREST:  JSR     SAVRES
                
 2482 A253              LDX     #LOW RES
 2484 A924              LDA     #HIGH RES
 2486 A011              LDY     #RESL
 2488 20A823            JSR     SROOM           ; "RESTORE POSITION"
                
 248B 204622            JSR     PARAMS          ; GET PARAMETERS
                
 248E A264              LDX     #LOW RSING
 2490 A924              LDA     #HIGH RSING
 2492 A01B              LDY     #RSINGL
 2494 20C21F            JSR     DLINE           ; "RESTORING POSITION X ..."
                
 2497                   ; SAVE LOCALS IN CASE OF ERROR
                
 2497 A21F              LDX     #31
 2499 BD000A    LOCSAV: LDA     LOCALS,X        ; COPY ALL LOCALS
 249C 9D0001            STA     $0100,X         ; TO BOTTOM OF MACHINE STACK
 249F CA                DEX
 24A0 10F7              BPL     LOCSAV
                
 24A2 A90A              LDA     #HIGH LOCALS
 24A4 85EE              STA     DBUFF+HI
 24A6 206E25            JSR     RDISK           ; RETRIEVE 1ST BLOCK OF PRELOAD
 24A9 B010              BCS     WRONG           ; BAD DISK READ IF CARRY CLEAR
                
 24AB AD200A            LDA     BUFSAV+0        ; DOES 1ST BYTE OF SAVED GAME ID
 24AE CD0226            CMP     ZBEGIN+ZID      ; MATCH THE CURRENT ID?
 24B1 D008              BNE     WRONG           ; WRONG DISK IF NOT
                
 24B3 AD210A            LDA     BUFSAV+1        ; WHAT ABOUT THE 2ND BYTE?
 24B6 CD0326            CMP     ZBEGIN+ZID+1
 24B9 F014              BEQ     RIGHT           ; CONTINUE IF BOTH BYTES MATCH
                
 24BB                   ; HANDLE RESTORE ERROR
                
 24BB A21F      WRONG:  LDX     #31             ; RESTORE ALL SAVED LOCALS
 24BD BD0001    WR0:    LDA     $0100,X


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 112    
--- Z-DOS: ATARI ---

 24C0 9D000A            STA     LOCALS,X
 24C3 CA                DEX
 24C4 10F7              BPL     WR0
                
 24C6 207F23            JSR     SIDE2           ; PROMPT FOR GAME DISK
 24C9 20D320            JSR     CLS
 24CC 4C480F            JMP     PREDF           ; PREDICATE FAILS
                
 24CF                   ; CONTINUE RESTORE
                
 24CF AD1026    RIGHT:  LDA     ZBEGIN+ZSCRIP   ; SAVE BOTH FLAG BYTES
 24D2 858E              STA     I+LO
 24D4 AD1126            LDA     ZBEGIN+ZSCRIP+1
 24D7 858F              STA     I+HI
                
 24D9 A905              LDA     #HIGH ZSTAKL    ; RETRIEVE OLD CONTENTS OF
 24DB 85EE              STA     DBUFF+HI        ; Z-STACK
 24DD 206E25            JSR     RDISK           ; GET 1ST BLOCK OF Z-STACK
 24E0 B0D9              BCS     WRONG
 24E2 206E25            JSR     RDISK           ; AND 2ND BLOCK
 24E5 B0D4              BCS     WRONG
                
 24E7 A5A3              LDA     ZCODE
 24E9 85EE              STA     DBUFF+HI
 24EB 206E25            JSR     RDISK           ; GET 1ST BLOCK OF PRELOAD
 24EE B0CB              BCS     WRONG
                
 24F0 A58E              LDA     I+LO            ; RESTORE THE STATE
 24F2 8D1026            STA     ZBEGIN+ZSCRIP   ; OF THE FLAG WORD
 24F5 A58F              LDA     I+HI
 24F7 8D1126            STA     ZBEGIN+ZSCRIP+1
                
 24FA AD0E26            LDA     ZBEGIN+ZPURBT   ; GET # PAGES TO LOAD
 24FD 858E              STA     I+LO
                
 24FF 206E25    LREST:  JSR     RDISK           ; FETCH THE REMAINDER
 2502 B0B7              BCS     WRONG
 2504 C68E              DEC     I+LO            ; OF THE PRELOAD
 2506 D0F7              BNE     LREST
                
 2508                   ; RESTORE THE STATE OF THE SAVED GAME
                
 2508 AD220A            LDA     BUFSAV+2        ; RESTORE THE [ZSP]
 250B 8594              STA     ZSP
 250D AD230A            LDA     BUFSAV+3        ; AND THE [OLDZSP]
 2510 8595              STA     OLDZSP
                
 2512 A202              LDX     #2              ; RESTORE THE [ZPC]
 2514 BD240A    RESZPC: LDA     BUFSAV+4,X
 2517 9596              STA     ZPC,X
 2519 CA                DEX


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 113    
--- Z-DOS: ATARI ---

 251A 10F8              BPL     RESZPC
                
 251C A900              LDA     #0
 251E 8599              STA     ZPCFLG          ; INVALIDATE [ZPC]
                
 2520 207F23            JSR     SIDE2           ; PROMPT FOR GAME DISK
 2523 20D320            JSR     CLS
 2526 4C540F            JMP     PREDS           ; PREDICATE SUCCEEDS
                
 2529                   ; --------------------------
 2529                   ; SAVE/RESTORE OFFSET TABLES
 2529                   ; --------------------------
                
 2529                   ; 144 SECTORS (18K) PER SAVE POSITION
                
 2529 01        OFFLOS: DB      LOW 1
 252A 91                DB      LOW 145
 252B 21                DB      LOW 289
 252C B1                DB      LOW 433
 252D 41                DB      LOW 577
                
 252E 00        OFFHIS: DB      HIGH 1
 252F 00                DB      HIGH 145
 2530 01                DB      HIGH 289
 2531 01                DB      HIGH 433
 2532 02                DB      HIGH 577
                
                        END
                        INCLUD  DISK.ASM


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 114    
--- DISK ACCESS: ATARI ---

                
 2533                   ; --------------------
 2533                   ; READ A VIRTUAL BLOCK
 2533                   ; --------------------
                
 2533                   ; ENTRY: TARGET V-BLOCK IN [DBLOCK]
 2533                   ;        TARGET RAM PAGE IN [DBUFF]
 2533                   ; EXIT: CARRY CLEAR IF OKAY, SET IF NOT
                
 2533 D8        GETDSK: CLD
 2534 A901              LDA     #1              ; V-BLOCKS ALWAYS COME
 2536 85F5              STA     DRIVE           ; FROM DRIVE #1
                
 2538                   ; CALCULATE SECTOR OF [DBLOCK]
                
 2538 A6EB              LDX     DBLOCK+LO       ; COPY LSB OF [DBLOCK]
 253A 86EF              STX     SECTOR+LO       ; INTO [SECTOR]
                
 253C A5EC              LDA     DBLOCK+HI
 253E 2901              AND     #%00000001      ; MASK ALL BUT BIT 1
 2540 85F0              STA     SECTOR+HI
 2542 D015              BNE     INPURE          ; BLOCK IS PURE IF MSB <> 0
                
 2544 E4A4              CPX     ZPURE           ; ELSE CHECK LSB
 2546 B011              BCS     INPURE          ; PURE IF >= [ZPURE]
                
 2548                   ; HANDLE A PRELOAD BLOCK
                
 2548 06EF              ASL     SECTOR+LO       ; MULTIPLY BY 2
 254A 26F0              ROL     SECTOR+HI       ; FOR ATARI 128-BYTE SECTORS
                
 254C A5EF              LDA     SECTOR+LO
 254E 18                CLC
 254F 6949              ADC     #73             ; ADD DISK PRELOAD OFFSET
 2551 85EF              STA     SECTOR+LO
 2553 9019              BCC     RDISK
 2555 E6F0              INC     SECTOR+HI
 2557 D015              BNE     RDISK           ; AND READ THE SECTOR
                
 2559                   ; HANDLE A PURE BLOCK
                
 2559 A5EF      INPURE: LDA     SECTOR+LO
 255B 38                SEC                     ; STRIP OFF THE
 255C E5A4              SBC     ZPURE           ; VIRTUAL PRELOAD OFFSET
 255E 85EF              STA     SECTOR+LO
 2560 B002              BCS     INP0
 2562 C6F0              DEC     SECTOR+HI
                
 2564 06EF      INP0:   ASL     SECTOR+LO       ; MULTIPLY BY 2
 2566 26F0              ROL     SECTOR+HI       ; FOR ATARI 128-BYTE SECTORS
                


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 115    
--- DISK ACCESS: ATARI ---

 2568 E6EF              INC     SECTOR+LO       ; DISK "PURE" OFFSET IS 1
 256A D002              BNE     RDISK
 256C E6F0              INC     SECTOR+HI
                
 256E                   ; FALL THROUGH ...
                
 256E                   ; -----------------
 256E                   ; READ A DISK BLOCK
 256E                   ; -----------------
                
 256E                   ; ENTRY: TARGET SECTOR IN [SECTOR]
 256E                   ;        TARGET DRIVE IN [DRIVE]
 256E                   ;        PAGE TO READ IN [DBUFF]
 256E                   ; EXIT: CARRY CLEAR IF OKAY, SET IF NOT
                
 256E A952      RDISK:  LDA     #$52            ; "READ" COMMAND
 2570 20A225            JSR     DODISK          ; GET DATA INTO [IOBUFF]
 2573 B02C              BCS     IOERR           ; SOMETHING WRONG IF CARRY SET
                
 2575 A000              LDY     #0              ; MOVE DATA IN [IOBUFF]
 2577 B90004    RDSK0:  LDA     IOBUFF,Y        ; TO [DBUFF]
 257A 91ED              STA     (DBUFF),Y
 257C C8                INY
 257D D0F8              BNE     RDSK0
                
 257F F011              BEQ     SNEXT           ; UPDATE & RETURN
                
 2581                   ; ------------------
 2581                   ; WRITE A DISK BLOCK
 2581                   ; ------------------
                
 2581                   ; ENTRY: TARGET SECTOR IN [SECTOR]
 2581                   ;        TARGET DRIVE IN [DRIVE]
 2581                   ;        PAGE TO WRITE IN [DBUFF]
 2581                   ; EXIT: CARRY CLEAR IF OKAY, SET IF NOT
                
 2581 A000      PUTDSK: LDY     #0              ; MOVE THE PAGE
 2583 B1ED      PDSK0:  LDA     (DBUFF),Y       ; AT [DBUFF]
 2585 990004            STA     IOBUFF,Y        ; INTO [IOBUFF]
 2588 C8                INY
 2589 D0F8              BNE     PDSK0
                
 258B A957              LDA     #$57            ; "WRITE" COMMAND
 258D 20A225            JSR     DODISK
 2590 B00F              BCS     IOERR           ; SOMETHING WRONG IF CARRY SET
                
 2592 E6EE      SNEXT:  INC     DBUFF+HI        ; POINT TO NEXT RAM PAGE
                
 2594 E6EB              INC     DBLOCK+LO       ; NEXT V-PAGE
 2596 D002              BNE     SNX0
 2598 E6EC              INC     DBLOCK+HI


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 116    
--- DISK ACCESS: ATARI ---

                
 259A E6EF      SNX0:   INC     SECTOR+LO       ; AND NEXT SECTOR
 259C D002              BNE     SNX1
 259E E6F0              INC     SECTOR+HI
                
 25A0 18        SNX1:   CLC                     ; CLEAR CARRY FOR SUCCESS
                
 25A1 60        IOERR:  RTS                     ; ELSE RETURN WITH CARRY SET
                
 25A2                   ; ---------------
 25A2                   ; ACCESS THE DISK
 25A2                   ; ---------------
                
 25A2                   ; ENTRY: [A] = $52 TO READ, $57 TO WRITE
 25A2                   ;        [DRIVE] = TARGET DRIVE (1 OR 2)
 25A2                   ;        [SECTOR] = TARGET SECTOR
 25A2                   ; EXIT: CARRY CLEAR IF OKAY, SET IF NOT
                
 25A2 8D0203    DODISK: STA     DCOMND          ; SET READ/WRITE COMMAND
                
 25A5 A5F5              LDA     DRIVE           ; SPECIFY
 25A7 8D0103            STA     DUNIT           ; WHICH DRIVE TO USE
                
 25AA                   ; CHECK VALIDITY OF SECTOR RANGE
                
 25AA A6EF              LDX     SECTOR+LO       ; GET LSB AND
 25AC A5F0              LDA     SECTOR+HI       ; MSB OF TARGET SECTOR
 25AE C902              CMP     #$02            ; MSB WITHIN RANGE?
 25B0 9006              BCC     RANOK           ; OKAY IF < 2
 25B2 D042              BNE     RANERR          ; RANGE ERROR IF > 2
 25B4 E0D0              CPX     #$D0            ; IF MSB WAS $02, IS LSB < $CF?
 25B6 B03E              BCS     RANERR          ; ERROR IF NOT
                
 25B8 8E0A03    RANOK:  STX     DAUX1           ; TELL SIO
 25BB 8D0B03            STA     DAUX2           ; WHICH SECTOR TO USE
                
 25BE A900              LDA     #LOW IOBUFF     ; POINT TO
 25C0 8D0403            STA     DBUFLO          ; THE BOTTOM HALF
 25C3 A904              LDA     #HIGH IOBUFF    ; OF [IOBUFF]
 25C5 8D0503            STA     DBUFHI
                
 25C8 2053E4            JSR     DSKINV          ; ACCESS 1ST HALF OF [IOBUFF]
 25CB AD0303            LDA     DSTATS          ; CHECK STATUS
 25CE 3024              BMI     DERR            ; ERROR IF NEGATIVE
                
 25D0 E6EF              INC     SECTOR+LO       ; POINT TO NEXT SECTOR
 25D2 D002              BNE     DDSK0
 25D4 E6F0              INC     SECTOR+HI
                
 25D6 A5EF      DDSK0:  LDA     SECTOR+LO       ; UPDATE [DAUX1/2]
 25D8 8D0A03            STA     DAUX1


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC. --- MACHINE DEPENDENT                                                                PAGE 117    
--- DISK ACCESS: ATARI ---

 25DB A5F0              LDA     SECTOR+HI
 25DD 8D0B03            STA     DAUX2
                
 25E0 A980              LDA     #LOW BUFTOP     ; POINT TO TOP HALF
 25E2 8D0403            STA     DBUFLO          ; OF [IOBUFF]
 25E5 A904              LDA     #HIGH BUFTOP
 25E7 8D0503            STA     DBUFHI
                
 25EA 2053E4            JSR     DSKINV          ; ACCESS TOP HALF OF [IOBUFF]
 25ED AD0303            LDA     DSTATS          ; CHECK STATUS
 25F0 3002              BMI     DERR            ; ERROR IF NEGATIVE
 25F2 18                CLC                     ; CLEAR CARRY FOR NO ERRORS
 25F3 60                RTS
                
 25F4 38        DERR:   SEC                     ; OR SET IT IF ERROR
 25F5 60                RTS
                
 25F6                   ; *** ERROR #12: DISK ADDRESS OUT OF RANGE ***
                
 25F6 A90C      RANERR: LDA     #12
 25F8 4CF91B            JMP     ZERROR
                
                        END
                
 25FB                   IF      DEBUG
 25FB                   INCLUD  BUGGER.ASM
 25FB                   ENDIF
                
 0000                   END


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 118    
---- SYMBOL TABLE ----

A12VAL   0FDB          COLOR4   02C8          DOFREQ   19B4          FLS1     1BC3          GTBT0    1861
A2VAL    128D          COMPAR   1295          DOGET    1368          FLS2     1BDB          GTBT1    1868
ABQUOT   14B3          CONCNT   00D0          DOM0     1E06          FLSL     1BD4          GTBT2    1871
ABREM    14A5          CONIN    00D1          DOMINS   1DFB          FLUSH    1C9D          GTBT3    1883
ABSDIV   148A          CONOUT   00D2          DOPM     1E1D          FLUSHW   1770          GTEXIT   1A5A
ABYTE    008A          CONSOL   D01F          DORET    1401          FREEZE   1C2E          GTRT0    2325
ADEX     008B          CONTOP   1A70          DOSCRL   1F07          FWL1     17D5          GTRT1    2332
ALLSET   22E4          CONZST   1A5E          DOSCRN   20A9          FWORDS   00B0          GTZ0     1A1C
ARG1     0082          COUT     1C89          DOSIB    1724          FWSUCC   1811          GVCALC   0F32
ARG2     0084          CR1      1CF7          DOSIDE   2381          GAME     234E          HI       0001
ARG3     0086          CRSINH   02F0          DOSVEC   000A          GAME2    2372          HILINE   210E
ARG4     0088          CSET0    1AA5          DOTIME   1DAC          GAME2L   0009          HPOSM0   D004
ATASCI   2126          CSET2    1AB7          DOUT     1FCA          GAMEL    0024          HPOSP0   D000
AUDC1    D201          CSHAPE   00F8          DOXM     1E1F          GDRIVE   00F2          I        008E
AUDCTL   D208          CSHIP    1AAA          DOXOP    0DE4          GETBYT   184F          ICAUX1   034A
AUDF1    D200          CTABLE   1AE4          DRIVE    00F5          GETDRV   2289          ICAUX2   034B
BACKSP   007E          CTEST    1A95          DRVSET   229D          GETDSK   2533          ICBADR   0344
BACKUP   1F8D          CZSL     1A61          DSIDE    235A          GETKEY   1E7D          ICBLEN   0348
BADKEY   1E77          DAUX1    030A          DSKERR   18F7          GETLNG   0E94          ICCOM    0342
BADOP1   0E43          DAUX2    030B          DSKINV   E453          GETP1    1392          IN       00B4
BADOP2   0E80          DBLOCK   00EB          DSTATS   0303          GETP2    13A1          INCVAL   0FD4
BADP     203A          DBUFF    00ED          DT0      1DD0          GETP3    13B4          INLOOP   1F66
BADSAV   2420          DBUFHI   0305          DT1      1DD6          GETPB    13C4          INP0     2564
BADVER   1124          DBUFLO   0304          DUMMY    0C0E          GETPOS   2254          INPURE   2559
BCALC    137B          DC0      0D8D          DUNIT    0301          GETPT1   13DA          INPUT    1F5B
BLANK    1996          DC1      0D94          DVX      0FD3          GETPT2   13E9          INSM     221F
BLINK    00F6          DCOMND   0302          EAR0     1904          GETPT3   13EF          INSML    0020
BOOP     20BD          DDSK0    25D6          EAR1     190E          GETPW    13CA          INVFLG   02B6
BOOP0    20CB          DEBUG    0000          EARLY    18FC          GETRET   2320          IOBUFF   0400
BOOT     0009          DECVAL   0FC8          EFIND    1762          GETSET   1A0A          IOCHAR   00E2
BREAK    1786          DEFAL    21B5          ENDLIN   1F97          GETSHT   0E90          IOERR    25A1
BRKTBL   1797          DEFALL   0011          ENTRY    00C4          GETV     0E97          IRQEN    D20E
BUFSAV   0A20          DEFNUM   21C2          ENUMB    1BF6          GETVAR   0EA8          IVX      0FDA
BUFTOP   0480          DERR     25F4          EOL      009B          GETVR1   0EAD          J        0090
CEX      1C9C          DGC      163E          EQBAD    1531          GETVRG   0EC1          K        0092
CH       02FC          DIGCNT   163A          EQOK     152E          GETVRL   0EB1          LBUFF    0A80
CHAR     1EEB          DIGITS   00DB          ERRM     1BE7          GETWRD   1925          LDPRE    0D51
CHRTBL   1B33          DIRECT   1984          ERRML    0012          GETYES   22BB          LENGTH   00DD
CIOV     E456          DIVERR   14F2          ESIZE    00C8          GETZ1    1A31          LEX0     1FA2
CLICK    1ED0          DIVEX    14B2          ETPEX    13D0          GETZ2    1A4C          LEX1     1FAD
CLK0     1ED9          DIVIDE   1475          FALSE    0000          GETZ3    1A58          LEX2     1FB7
CLK1     1EDE          DLINE    1FC2          FBRK     17AD          GETZCH   1A16          LFROM    00E5
CLOSEP   203E          DLIST    0C15          FINDW    17AF          GKEY0    1E82          LINCNT   00E0
CLS      20D3          DLS0     126C          FIRST1   1140          GKEY1    1EA2          LINEX    1D19
CLS0     20E2          DMACTL   D400          FL0      1C9F          GLOBAL   00AC          LINLEN   00C2
CNL      1AE6          DOB2     0F75          FL1      1CA9          GO       0E05          LINOUT   1D00
CNOK     1AEF          DOCALL   153D          FL2      1CB4          GODIV    1497          LMARGN   0052
COLCRS   0055          DODEF    21C6          FL3      1CBE          GPOSIT   00F1          LMAX     00E1
COLD     0C4F          DODIS    0DF3          FLAGSU   1BA9          GPRIOR   026F          LO       0000
COLDST   0244          DODISK   25A2          FLEX     1785          GRACTL   D01D          LOCALS   0A00
COLOR1   02C5          DOEQ     150A          FLS0     1BC2          GS       1A11          LOCSAV   2499


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 119    
---- SYMBOL TABLE ----

LOGMAP   02B2          NUMBER   162E          PG1      188D          PUTBYT   0F06          RTNL     001B
LOLINE   20F6          NXTP1    140B          PG2      189A          PUTDSK   2581          SAV      23B9
LOUT     1D08          NXTP2    141A          PG3      18A1          PUTLSB   15D8          SAVDRI   223D
LREST    24FF          NXTP3    141D          PG4      18C3          PUTP1    15F3          SAVL     000E
LRU      00A2          OBJ1     1B63          PG5      18D5          PUTP2    1602          SAVRES   21A6
LRUMAP   0900          OBJ2     1B6A          PG6      18E0          PUTP3    1612          SAY1     1AFF
LSAVE    2441          OBJLOC   1B4D          PG7      18EC          PUTVAL   0F0C          SAY2     1B0A
LTO      00E7          OBJTAB   00B2          PG8      18F1          PUTVLG   0F25          SAYSET   1AF4
MDRI     2204          OFFHIS   252E          PGOOD    12D2          PUTVLL   0F15          SBL      17A4
MEMTOP   1C6A          OFFLOS   2529          PLERR    161C          PUTVR1   0F11          SCEX     12B1
MIND     21EF          OFFSET   00CC          PLMRAM   0800          PYUCK    112D          SCMP     12A7
MINDL    0030          OFFSPL   207C          PMAX     00A6          PZSTR    1942          SCOMP    129C
MINIT    14F7          OLDLEN   00DE          PMBASE   D407          PZTOP    194B          SCREEN   BC40
MISSL    0B00          OLDLST   BC20          PMG0     0C8D          QSIGN    00D9          SCRIPT   00DF
MLOOP    0D7E          OLDZSP   0095          PMG1     0C94          QUOT     00D3          SDLSTL   0230
MORE     1D1A          OP0      0E0A          PNAME    1FD5          RANDOM   1C81          SDMCTL   022F
MOVMIN   1DE1          OP1      0E19          PNERR    1617          RANERR   25F6          SECTOR   00EF
MPC      009C          OP1A     0E23          POKMSK   0010          RANOK    25B8          SET1     196E
MPCFLG   009F          OP1B     0E2D          POPVAL   0ECE          RDISK    256E          SET2     1976
MPCH     009E          OP1EX    0E34          PORTB    D301          RDSK0    2577          SETDRV   229F
MPCL     009C          OP2      0E48          POSIT    21D5          READL    16BF          SETNP    0D0F
MPCM     009D          OP2A     0E52          POSITL   000D          READL2   170A          SETPOS   226A
MPCPNT   00A0          OP2B     0E55          POSSET   2268          READL3   172C          SETSTR   1930
MPOS     21FA          OP2C     0E64          PP1      2019          REMAIN   00D5          SETWRD   1916
MRAND    D20A          OP2D     0E67          PPRINT   1FD9          REMVC1   11C2          SFLAG    1FD8
MTEMP    00D7          OP2EX    0E71          PPX      1BA3          REMVC2   11D3          SHOVE    1968
NARGS    0081          OPCODE   0080          PREDB    0F59          REMVEX   11DC          SHOWIT   1F75
NBL      178D          OPEXT    0D9B          PREDB1   0F7C          RES      2453          SIB      179D
NBRKS    0006          OPT0     0FE4          PREDB2   0F87          RESL     0011          SIDE1    237B
NENTS    00C6          OPT1     1000          PREDB3   0F8E          RESULT   00C1          SIDE2    237F
NEWLOG   20B1          OPT2     1020          PREDB5   0FAC          RESZPC   2514          SIZEM    D00C
NEWSET   199A          OPTX     1052          PREDF    0F48          RET0     0F04          SKCTL    D20F
NEXTPC   1819          OPX0     0DA6          PREDLB   0F69          RET1     120B          SLD      0C36
NEXTZ    1A7E          OPX1     0DAC          PREDNB   0F4D          RET2     121F          SLDL     0019
NO       2243          OPX2     0DB6          PREDS    0F54          RETURN   230C          SLINE    00E3
NOBLIN   1EBA          OPX3     0DC0          PRIL     108E          RFLIP    14A1          SNEXT    2592
NOL      0003          OPX4     0DDB          PRLEN    00EA          RIGHT    24CF          SNX0     259A
NOMORE   1F7C          OPXNXT   0DC7          PRNTDC   11DF          RL0      16CD          SNX1     25A0
NOPS0    000E          OUT      00BA          PRNTN3   1656          RL1      16D2          SOURCE   00C0
NOPS1    0010          OUTEOL   1F51          PRNTN4   165F          RL2      16D9          SPACE    0020
NOPS2    0019          OVER     0EF6          PROPB    1B76          RL3      16E2          SPCALC   2072
NOPSX    000C          PAGE     1885          PROPL    1B97          RLERR    16C9          SPL0     205A
NORAM    0D0A          PAGE0    00A5          PROPN    1B92          RLEX     16D8          SPL1     2068
NORM     178B          PARAMS   2246          PROPNX   1B9F          RLL      16E8          SPLEX    207B
NOSCRL   1F3A          PATCH    185A          PSET     00C9          ROWCRS   0054          SPLOFF   207F
NOT48K   1C7F          PBAD     12BF          PSHVAL   0EE4          RSIGN    00DA          SPSTAT   00E4
NOTSAT   22D8          PCALC    15DD          PSTAT    00E9          RSING    2464          SRL0     1F0C
NPC0     182B          PDC0     11F4          PTABH    0800          RSINGL   001B          SRL1     1F27
NPC1     1832          PDSK0    2583          PTABL    0700          RSPOS    2479          SRL2     1F30
NPC2     183B          PEX      2048          PTZ0     116A          RTCLOK   0014          SRL3     1F34
NPC3     184D          PFINE    1149          PUSHXA   0EE8          RTN      2333          SRLP     23AD


AVOCET SYSTEMS 6502 CROSS-ASSEMBLER -  VERSION 2.01C

ZIP/6502 INFOCOM, INC.                                                                                      PAGE 120    
---- SYMBOL TABLE ----

SROOM    23A8          VARPUT   0EFB          ZCALL3   15BC          ZIP      0C00          ZPUTB    15D5
ST0      0CCB          VERNUM   1C5B          ZCHAR    00CB          ZJUMP    1237          ZPUTP    15F0
ST1      0CD3          VERS     1C4C          ZCHKSM   001C          ZLENTH   001A          ZQUIT    1C1F
ST2      0CDD          VERSL    000F          ZCODE    00A3          ZLESS    1263          ZRAND    166B
STAMP    00AA          VEXIT    125C          ZCR0     1CE6          ZLOC     114C          ZREAD    169C
STATEX   1E27          VOCAB    00AE          ZCRLF    1CC7          ZMLOOP   1440          ZREMOV   119B
STRING   1FCB          VSUM     10C6          ZCRME    1CF1          ZMNEXT   1457          ZREST    247F
STX0     1E2B          VSUM0    10D2          ZCRMP    1CD8          ZMOD     146B          ZRET     11FA
STX1     1E31          VSUM2    10F0          ZCRUSH   1B0D          ZMODE    0001          ZRFALS   1075
STX2     1E40          VSUM3    10FC          ZD0      1188          ZMOVE    1338          ZRSTAK   109F
SVING    23C7          WARM     0CAE          ZDEC     1180          ZMUL     143D          ZRT0     106C
SVINGL   0018          WARM1    0CC7          ZDIV     1461          ZMVEX    1361          ZRT1     106E
SVPOS    23D9          WARMEX   0D5D          ZDLESS   1269          ZNEXT    1130          ZRTRUE   106A
SWAP     00AB          WCALC    1377          ZENDLD   0004          ZNEXTP   1404          ZSAVE    23DF
TARGET   00A8          WCEX     0F47          ZEQUAL   1501          ZNOOP    0FC7          ZSCRIP   0010
TDRIVE   00F4          WDRIV    21E2          ZEROPG   0080          ZOBJEC   000A          ZSCRN    208E
TIMEFL   00DC          WDRIVL   000D          ZERR0    1BFB          ZPAGE    00A7          ZSERIA   0012
TOASC    1965          WNEXT    17F2          ZERR1    1BFD          ZPC      0096          ZSET     132B
TOBOT0   20A4          WNX      17FD          ZERR2    1C06          ZPCFLG   0099          ZSP      0094
TOBOT1   20A7          WNX1     1808          ZERROR   1BF9          ZPCH     0098          ZSPLIT   2049
TOPERM   19A8          WR0      24BD          ZFCLR    1314          ZPCL     0096          ZSTAKH   0600
TORES    1C31          WRDLEN   00C3          ZFIRST   1139          ZPCM     0097          ZSTAKL   0500
TORESL   000D          WRONG    24BB          ZFLAG    00CD          ZPCPNT   009A          ZSTART   1C3E
TOTOP    209C          WSTACK   2429          ZFSET    1301          ZPCSAV   240F          ZSTEX    1941
TPOSIT   00F3          XSIZE    0027          ZFSETP   12ED          ZPOP     1694          ZSUB     1430
TRUE     00FF          YES      223F          ZFWORD   0018          ZPRB     118D          ZUSL     1D20
TRY2     1516          YESL     0004          ZGET     1362          ZPRC     1621          ZVALUE   124B
TRY3     1522          YSIZE    0017          ZGETB    1372          ZPRD     11DD          ZVER     10A5
TSET     00CA          ZADD     1423          ZGETP    138F          ZPRI     1079          ZVERS    0000
UDIV     14C1          ZBAND    12E1          ZGETPT   13D7          ZPRINT   123D          ZVOCAB   0008
UDLOOP   14CA          ZBCOM    1253          ZGLOBA   000C          ZPRN     1626          ZVR      10AC
UDNEXT   14E2          ZBEGIN   2600          ZGO      0006          ZPRR     1096          ZWORD    00CE
UNDER    0EDF          ZBOR     12D5          ZGRTR    1277          ZPTSIZ   1158          ZZERO    1127
USL0     1D46          ZBTST    12C2          ZID      0002          ZPURBT   000E          
V2A1     0E85          ZCALL    1534          ZIGRTR   1282          ZPURE    00A4          
VALUE    008C          ZCALL1   156B          ZIN      12B2          ZPUSH    168D          
VARGET   0E9F          ZCALL2   1592          ZINC     1175          ZPUT     15C7          
                
***** NO ERRORS DETECTED *****
