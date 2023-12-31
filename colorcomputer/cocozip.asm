

AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  1                 
                                                                                                                                    

                
 0000                   ; --------------------------------
 0000                   ; ZIP/6809
 0000                   ; Z-CODE INTERPRETER PROGRAM
 0000                   ; FOR MOTOROLA 6809 MICROPROCESSOR
 0000                   ; --------------------------------
                
 0000                   ; COMPANY PRIVATE -- NOT FOR DISTRIBUTION
                
 0A00           MSTART  EQU     $0A00                   ; START OF FREE PROGRAM RAM
                
 0000           DEBUG   EQU     0                       ; ASSEMBLY FLAG
                
 0000                   ; -----------
 0000                   ; ERROR CODES
 0000                   ; -----------
                
 0000                   ; 00 -- INSUFFICIENT RAM
 0000                   ; 01 -- ILLEGAL X-OP
 0000                   ; 02 -- ILLEGAL 0-OP
 0000                   ; 03 -- ILLEGAL 1-OP
 0000                   ; 04 -- ILLEGAL 2-OP
 0000                   ; 05 -- Z-STACK OVERFLOW
 0000                   ; 06 -- Z-STACK UNDERFLOW
 0000                   ; 07 -- ILLEGAL PROPERTY LENGTH (GETP)
 0000                   ; 08 -- DIVISION BY ZERO
 0000                   ; 09 -- ILLEGAL ARGUMENT COUNT (EQUAL?)
 0000                   ; 10 -- ILLEGAL PROPERTY ID (PUTP)
 0000                   ; 11 -- ILLEGAL PROPERTY LENGTH (PUTP)
 0000                   ; 12 -- DISK ADDRESS OUT OF RANGE
 0000                   ; 13 -- PARSER OVERFLOW
 0000                   ; 14 -- DRIVE ACCESS
                
                        INCLUD ZEQUATES.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  2                 
--- MEMORY ORGANIZATION ---                                                                                                         

                
 00FF           TRUE    EQU     $FF
 0000           FALSE   EQU     0
                
 0000           DSTART  EQU     0               ; START OF DIRECT-PAGE RAM
                
 0AFE           MSTACK  EQU     MSTART+$FE      ; TOP OF MACHINE STACK (254 BYTES)
 0B00           IOBUFF  EQU     MSTART+$100     ; 256-BYTE DISK I/O BUFFER
 0C00           ZSTACK  EQU     MSTART+$200     ; Z-STACK (255 WORDS)
 00FF           ZSTAKL  EQU     255             ; LENGTH OF Z-STACK IN WORDS
 0DFE           TOPSTA  EQU     (2*ZSTAKL)+ZSTACK       ; TOP OF Z-STACK
 0E00           PTABLE  EQU     MSTART+$400     ; START OF PAGING TABLES
 1000           LOCALS  EQU     MSTART+$600     ; LOCAL VARIABLE STORAGE (32 BYTES)
 1020           BUFFER  EQU     MSTART+$620     ; I/O LINE BUFFER (32 BYTES)
 1040           BUFSAV  EQU     MSTART+$640     ; I/O AUX BUFFER (32 BYTES)
 1100           ZIP     EQU     MSTART+$700     ; START OF EXECUTABLE CODE
 2500           ZCODE   EQU     ZIP+$1400       ; START OF Z-CODE (ASSUME 5K ZIP)
                
 0000                   ; Z-CODE HEADER OFFSETS
                
 0000           ZVERS   EQU     0               ; VERSION BYTE
 0001           ZMODE   EQU     1               ; MODE SELECT BYTE
 0002           ZID     EQU     2               ; GAME ID WORD
 0004           ZENDLD  EQU     4               ; START OF NON-PRELOADED Z-CODE
 0006           ZBEGIN  EQU     6               ; EXECUTION ADDRESS
 0008           ZVOCAB  EQU     8               ; START OF VOCABULARY TABLE
 000A           ZOBJEC  EQU     10              ; START OF OBJECT TABLE
 000C           ZGLOBA  EQU     12              ; START OF GLOBAL VARIABLE TABLE
 000E           ZPURBT  EQU     14              ; START OF "PURE" Z-CODE
 0010           ZSCRIP  EQU     16              ; FLAG WORD
 0012           ZSERIA  EQU     18              ; 3-WORD ASCII SERIAL NUMBER
 0018           ZFWORD  EQU     24              ; START OF FWORDS TABLE
 001A           ZLENTH  EQU     26              ; LENGTH OF Z-PROGRAM IN WORDS
 001C           ZCHKSM  EQU     28              ; Z-CODE CHECKSUM WORD
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  3                 
--- ZIP D-PAGE VARIABLES ---                                                                                                        

                
 0000           OPCODE  EQU     DSTART          ; CURRENT OPCODE
 0001           ARGCNT  EQU     OPCODE+1        ; # ARGUMENTS
 0002           ARG1    EQU     OPCODE+2        ; ARGUMENT #1 (WORD)
 0004           ARG2    EQU     OPCODE+4        ; ARGUMENT #2 (WORD)
 0006           ARG3    EQU     OPCODE+6        ; ARGUMENT #3 (WORD)
 0008           ARG4    EQU     OPCODE+8        ; ARGUMENT #4 (WORD)
                
 000A           LRU     EQU     OPCODE+10       ; (BYTE) LEAST RECENTLY USED PAGE INDEX
 000B           ZPURE   EQU     LRU+1           ; (BYTE) 1ST VIRTUAL PAGE OF PURE Z-CODE
 000C           PMAX    EQU     LRU+2           ; (BYTE) MAXIMUM # SWAPPING PAGES
 000D           ZPAGE   EQU     LRU+3           ; (BYTE) CURRENT SWAPPING PAGE
 000E           PAGE0   EQU     LRU+4           ; (BYTE) 1ST ABS PAGE OF SWAPPING SPACE
 000F           TABTOP  EQU     LRU+5           ; (WORD) ADDRESS OF LAST P-TABLE ENTRY
                
 0011           ZPCH    EQU     LRU+7           ; HIGHEST-ORDER BIT OF PC
 0012           ZPCM    EQU     ZPCH+1          ; MIDDLE 8 BITS OF PC
 0013           ZPCL    EQU     ZPCH+2          ; LOWER 8 BITS OF PC
 0014           ZPCPNT  EQU     ZPCH+3          ; POINTER TO ACTUAL PC PAGE (WORD)
 0016           ZPCFLG  EQU     ZPCH+5          ; FLAG: "TRUE" IF ZPCPNT VALID
                
 0018           MPCH    EQU     ZPCH+7          ; HIGHEST-ORDER BIT OF MEM POINTER
 0019           MPCM    EQU     MPCH+1          ; MIDDLE 8 BITS OF MEM POINTER
 001A           MPCL    EQU     MPCH+2          ; LOW-ORDER 8 BITS OF MEMORY POINTER
 001B           MPCPNT  EQU     MPCH+3          ; ACTUAL POINTER TO MEMORY (WORD)
 001D           MPCFLG  EQU     MPCH+5          ; FLAG: "TRUE" IF MPCPNT VALID
                
 001F           GLOBAL  EQU     MPCH+7          ; GLOBAL VARIABLE POINTER (WORD)
 0021           VOCAB   EQU     GLOBAL+2        ; VOCAB TABLE POINTER (WORD)
 0023           FWORDS  EQU     GLOBAL+4        ; FWORDS TABLE POINTER (WORD)
                
 0025           OZSTAK  EQU     GLOBAL+6        ; ZSP SAVE REGISTER (FOR ZCALL)
                
 0027           CSTEMP  EQU     OZSTAK+2        ; SET IF TEMP CHARSET IN EFFECT
 0028           CSPERM  EQU     CSTEMP+1        ; CURRENT PERM CHARSET
 0029           STBYTF  EQU     CSTEMP+2        ; 0=1ST, 1=2ND, 2=3RD, 0=LAST
                
 002A           ZSTWRD  EQU     CSTEMP+3        ; WORD STORAGE (WORD)
 002C           ZSTBUI  EQU     ZSTWRD+2        ; Z-STRING INPUT BUFFER (6 BYTES)
 0032           ZSTBUO  EQU     ZSTWRD+8        ; Z-STRING OUTPUT BUFFER (6 BYTES)
 0038           RTABP   EQU     ZSTWRD+14       ; RESULT TABLE POINTER
 0039           STABP   EQU     ZSTWRD+15       ; SOURCE TABLE POINTER
 003A           PZSTFO  EQU     ZSTWRD+16       ; FWORD TABLE BLOCK OFFSET
                
 003B           VAL     EQU     ZSTWRD+17       ; VALUE RETURN REGISTER (WORD)
 003D           TEMP    EQU     VAL+2           ; TEMPORARY REGISTER (WORD)
 003F           TEMP2   EQU     VAL+4           ; ANOTHER TEMPORARY REGISTER (WORD)
 0041           MASK    EQU     VAL+6           ; BIT-MASK REGISTER (WORD)
 0043           SQUOT   EQU     VAL+8           ; SIGN OF QUOTIENT
 0044           SREM    EQU     VAL+9           ; SIGN OF REMAINDER
 0045           MTEMP   EQU     VAL+10          ; MATH TEMP REGISTER (WORD)


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  4                 
--- ZIP D-PAGE VARIABLES ---                                                                                                        

                
 0047           DRIVE   EQU     VAL+12          ; DRIVE NUMBER
 0048           DBUFF   EQU     DRIVE+1         ; DISK I/O BUFFER POINTER (WORD)
 004A           DBLOCK  EQU     DRIVE+3         ; Z-BLOCK # (WORD)
 004C           TRACK   EQU     DRIVE+5         ; TRACK/SECTOR ADDRESS (WORD)
                
 004E           TIMEFL  EQU     DRIVE+7         ; "TRUE" IF TIME MODE
                
 004F           CHRPNT  EQU     TIMEFL+1        ; I/O BUFFER INDEX
 0050           CPSAV   EQU     CHRPNT+1        ; SAVE REGISTER FOR [CHRPNT]
 0051           BINDEX  EQU     CHRPNT+2        ; BUFFER DISPLAY INDEX
 0052           LINCNT  EQU     CHRPNT+3        ; # LINES DISPLAYED SINCE LAST USL
 0053           IOCHAR  EQU     CHRPNT+4        ; CURRENT I/O CHARACTER
 0054           GDRIVE  EQU     CHRPNT+5        ; GAME-SAVE DEFAULT DRIVE #
 0055           GPOSIT  EQU     CHRPNT+6        ; GAME-SAVE DEFAULT POSITION
 0056           RAND1   EQU     CHRPNT+7        ; RANDOM NUMBER REGISTER
 0057           RAND2   EQU     CHRPNT+8        ; DITTO
 0058           CYCLE   EQU     CHRPNT+9        ; TIMER FOR CURSOR BLINK (WORD)
 005A           BLINK   EQU     CHRPNT+11       ; MASK FOR CURSOR BLINK
 005B           CFLAG   EQU     CHRPNT+12       ; CURSOR ENABLE FLAG
 005C           SCRIPT  EQU     CHRPNT+13       ; SCRIPTING ENABLE FLAG
 005D           IHOLD   EQU     CHRPNT+14       ; INTERRUPT HOLD
                
 005E           ZPGTOP  EQU     CHRPNT+15       ; END OF DIRECT-PAGE VARIABLES
                
                        END
                        INCLUD WARM.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  5                 
--- WARMSTART ROUTINE ---                                                                                                           

                
 0000                   SETDP   0
 1100                   ORG     ZIP             ; START OF EXECUTABLE CODE
                
 1100 4F                CLRA                    ; USE PAGE ZERO
 1101 1F8B              TFR     A,DP            ; AS THE DIRECT PAGE
 1103 1A50              ORCC    #%01010000      ; DISABLE INTERRUPTS
 1105 B6FF03            LDA     INT60           ; DISABLE
 1108 84FE              ANDA    #%11111110      ; THE 60HZ
 110A B7FF03            STA     INT60           ; INTERRUPT
 110D B7FFDF            STA     ROMOFF          ; AND THE ROMS
 1110 10CE0AFE          LDS     #MSTACK         ; GIVE THE STACK A NEW HOME
 1114 7E1F73            JMP     COLD            ; PERFORM ONE-TIME INITIALIZATION
                
 1117                   ; WARMSTART ENTRY
                
 1117 10CE0AFE  START:  LDS     #MSTACK         ; RESET MACHINE STACK
                
 111B                   ; CLEAR ALL DIRECT-PAGE VARIABLES
                
 111B 8E0000            LDX     #DSTART
 111E 6F80      ST0:    CLR     ,X+
 1120 8C005E            CMPX    #ZPGTOP
 1123 25F9              BLO     ST0
                
 1125                   ; RESET THE PAGING TABLE
                
 1125 8E0E00            LDX     #PTABLE
 1128 86FF              LDA     #$FF
 112A A780      ST1:    STA     ,X+
 112C 8C1000            CMPX    #PTABLE+$200
 112F 25F9              BLO     ST1
                
 1131                   ; GET THE FIRST SECTOR OF Z-CODE
                
 1131 CC2500            LDD     #ZCODE          ; POINT TO 1ST
 1134 DD48              STD     DBUFF           ; Z-CODE LOCATION
 1136 BD2167            JSR     GETDSK          ; FETCH BLOCK #0 FROM DRIVE 0
                
 1139                   ; EXTRACT GAME DATA FROM Z-CODE HEADER
                
 1139 B62504            LDA     ZCODE+ZENDLD    ; GET MSB OF ENDLOAD POINTER
 113C 4C                INCA                    ; ADD ONE TO GET
 113D 970B              STA     ZPURE           ; 1ST PAGE IN "PURE" CODE
 113F 8B25              ADDA    #HIGH ZCODE     ; ADD BASE ADDRESS TO GET
 1141 970E              STA     PAGE0           ; 1ST PAGE OF SWAPPING SPACE
                
 1143 C6FE              LDB     #MEMTOP         ; TOP PAGE OF MEMORY
 1145 D00E              SUBB    PAGE0           ; SUBTRACT ADDRESS OF PAGING BUFFER
 1147 2304              BLS     NORAM           ; NOT ENOUGH SPACE!
 1149 C108              CMPB    #8


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  6                 
--- WARMSTART ROUTINE ---                                                                                                           

 114B 2404              BHS     SETNP           ; MUST HAVE AT LEAST 8 SWAPPING PAGES
                
 114D                   ; *** ERROR #0 -- INSUFFICIENT RAM ***
                
 114D 4F        NORAM:  CLRA
 114E BD1FA9            JSR     ZERROR
                
 1151                   ; [B] HAS # FREE SWAPPING PAGES
                
 1151 D70C      SETNP:  STB     PMAX            ; SET MAXIMUM # FREE PAGES
 1153 8E0E00            LDX     #PTABLE         ; ADD BASE ADDR OF P-TABLE
 1156 3A                ABX                     ; TO PAGING LIMIT
 1157 3A                ABX                     ; TWICE (FOR WORD-ALIGNMENT)
 1158 9F0F              STX     TABTOP          ; TO GET ADDR OF HIGHEST TABLE ENTRY
                
 115A B62501            LDA     ZCODE+ZMODE     ; GET MODE BYTE
 115D 8A08              ORA     #%00001000      ; SET THE "TANDY" ID BIT
 115F B72501            STA     ZCODE+ZMODE
 1162 8402              ANDA    #%00000010      ; ISOLATE STAT-LINE FORMAT BIT
 1164 974E              STA     TIMEFL          ; 0=SCORE/MOVES, NZ=HOURS/MINUTES
                
 1166 FC2506            LDD     ZCODE+ZBEGIN    ; GET START ADDRESS OF Z-CODE
 1169 DD12              STD     ZPCM            ; HIGH BITS AT ZPCH ALREADY CLEARED
                
 116B FC250C            LDD     ZCODE+ZGLOBA    ; GET RELATIVE ADDR OF GLOBAL TABLE
 116E C32500            ADDD    #ZCODE          ; CONVERT TO ABSOLUTE ADDRESS
 1171 DD1F              STD     GLOBAL
                
 1173 FC2518            LDD     ZCODE+ZFWORD    ; DO SAME FOR FWORDS TABLE
 1176 C32500            ADDD    #ZCODE
 1179 DD23              STD     FWORDS
                
 117B FC2508            LDD     ZCODE+ZVOCAB    ; AND VOCABULARY TABLE
 117E C32500            ADDD    #ZCODE
 1181 DD21              STD     VOCAB
                
 1183                   ; GRAB THE REST OF THE PRELOAD
                
 1183 960B              LDA     ZPURE           ; GET # PAGES IN PRELOAD + 1
 1185 973D              STA     TEMP            ; USE AS AN INDEX
 1187 BD2167    LDPRE:  JSR     GETDSK          ; GRAB THE BLOCK
 118A 0A3D              DEC     TEMP
 118C 26F9              BNE     LDPRE           ; KEEP READING TILL DONE
                
 118E CE0DFE            LDU     #TOPSTA         ; INIT THE ZSP
 1191 DF25              STU     OZSTAK          ; REMEMBER ITS POSITION
                
 1193 BD1F1A            JSR     CLS             ; CLEAR THE SCREEN
 1196 035C              COM     SCRIPT          ; ENABLE SCRIPTING
                
 1198                   ; FALL INTO MAIN LOOP


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- INITIALIZATION                                                                   PAGE  7                 
--- WARMSTART ROUTINE ---                                                                                                           

                
                        END
                
                        INCLUD MAIN.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE  8                 
--- MAIN LOOP ---                                                                                                                   

                
 1198 BD1D92    MLOOP:  JSR     ROMOUT          ; FOR SAFETY
 119B 0A57              DEC     RAND2           ; RANDOMNESS
 119D 0A57              DEC     RAND2
                
 119F 0F01              CLR     ARGCNT          ; RESET # ARGUMENTS
 11A1 BD1A55            JSR     NEXTPC          ; GET NEXT Z-BYTE
 11A4 9700              STA     OPCODE          ; SAVE OPCODE
                
 11A6                   IF      DEBUG
 11A6                   LDB     #'0'
 11A6                   JSR     DOBUG
 11A6                   LDA     OPCODE
 11A6                   ENDIF
                
 11A6 102A00A4          LBPL    OP2             ; 2-OP IF POSITIVE
 11AA 81B0              CMPA    #176
 11AC 256F              BLO     OP1             ; IT'S A 1-OP
 11AE 81C0              CMPA    #192
 11B0 255B              BLO     OP0             ; IF NOT A 0-OP ...
                
 11B2                   ; HANDLE AN X-OP
                
 11B2 BD1A55    OPEXT:  JSR     NEXTPC          ; GET ARGUMENT BYTE
 11B5 973F              STA     TEMP2           ; HOLD IT HERE
 11B7 0F40              CLR     TEMP2+1         ; INIT LOOP INDEX
 11B9 2006              BRA     OPX1
                
 11BB 963F      OPX0:   LDA     TEMP2           ; GRAB ARG BYTE
 11BD 48                ASLA                    ; SHIFT TO BITS 7 & 6
 11BE 48                ASLA
 11BF 973F              STA     TEMP2           ; SAVE RESULT
                
 11C1 84C0      OPX1:   ANDA    #%11000000      ; MASK OUT GARBAGE
 11C3 2605              BNE     OPX2
 11C5 BD128E            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 11C8 2010              BRA     OPXNXT
                
 11CA 8140      OPX2:   CMPA    #%01000000
 11CC 2605              BNE     OPX3
 11CE BD1286            JSR     GETSHT          ; 01 = SHORT IMMEDIATE
 11D1 2007              BRA     OPXNXT
                
 11D3 8180      OPX3:   CMPA    #%10000000
 11D5 2619              BNE     OPX4            ; 11 = NO MORE VARIABLES
 11D7 BD12A5            JSR     GETVAR          ; 10 = VARIABLE
                
 11DA D640      OPXNXT: LDB     TEMP2+1         ; GET INDEX
 11DC 8E0002            LDX     #ARG1           ; BASE ADDR OF ARGS
 11DF 3A                ABX                     ; ADD OFFSET IN B
 11E0 DC3D              LDD     TEMP            ; GRAB THE ARGUMENT'S VALUE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE  9                 
--- MAIN LOOP ---                                                                                                                   

 11E2 ED84              STD     ,X              ; AND SAVE IT
 11E4 0C01              INC     ARGCNT          ; KEEP TRACK
 11E6 0C40              INC     TEMP2+1         ; UPDATE
 11E8 0C40              INC     TEMP2+1         ; ARGUMENT INDEX
 11EA 9640              LDA     TEMP2+1         ; DONE 4 ARGS YET?
 11EC 8108              CMPA    #8
 11EE 25CB              BLO     OPX0            ; NO, KEEP GRABBING
                
 11F0                   ; DISPATCH THE X-OP
                
 11F0 D600      OPX4:   LDB     OPCODE          ; RETRIEVE THE OPCODE
 11F2 C1E0              CMPB    #224            ; IS IT AN EXTENDED 2-OP?
 11F4 1025007C          LBLO    OP2EX           ; YES, HANDLE LIKE A 2-OP
 11F8 C41F              ANDB    #%00011111      ; ELSE ISOLATE OP BITS
 11FA C10C              CMPB    #NOPSX          ; COMPARE TO LEGAL # OF X-OPS
 11FC 2505              BLO     DISPX           ; CONTINUE IF OKAY
                
 11FE                   ; *** ERROR #1 -- ILLEGAL X-OP ***
                
 11FE 8601              LDA     #1
 1200 BD1FA9            JSR     ZERROR
                
 1203 8E13FF    DISPX:  LDX     #OPTX           ; X-OP DISPATCH TABLE
 1206 58        DODIS:  ASLB                    ; FORM A WORD-OFFSET INTO IT
 1207 3A                ABX                     ; ADD THE OFFSET
                
 1208                   IF      DEBUG
 1208                   PSHS    X
 1208                   LDB     #'1'
 1208                   JSR     DOBUG
 1208                   PULS    X
 1208                   ENDIF
                
 1208 AD94              JSR     [,X]            ; HANDLE THE OPCODE
 120A 7E1198            JMP     MLOOP           ; AND GO BACK FOR ANOTHER
                
 120D                   ; HANDLE A 0-OP
                
 120D 8E1391    OP0:    LDX     #OPT0           ; 0-OP DISPATCH TABLE
 1210 D600              LDB     OPCODE          ; FETCH OPCODE
 1212 C40F              ANDB    #%00001111      ; ISOLATE OP BITS
 1214 C10E              CMPB    #NOPS0          ; OPCODE OUT OF RANGE?
 1216 25EE              BLO     DODIS           ; NO, GO DISPATCH IT
                
 1218                   ; *** ERROR #2 -- ILLEGAL 0-OP ***
                
 1218 8602              LDA     #2
 121A BD1FA9            JSR     ZERROR
                
 121D                   ; HANDLE A 1-OP
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 10                 
--- MAIN LOOP ---                                                                                                                   

 121D 8430      OP1:    ANDA    #%00110000      ; ISOLATE ARG BITS
 121F 2605              BNE     OP1A
 1221 BD128E            JSR     GETLNG          ; 00 = LONG IMMEDIATE
 1224 2015              BRA     OP1EX
                
 1226 8110      OP1A:   CMPA    #%00010000
 1228 2605              BNE     OP1B
 122A BD1286            JSR     GETSHT          ; 01 = SHORT IMMEDIATE
 122D 200C              BRA     OP1EX
                
 122F 8120      OP1B:   CMPA    #%00100000
 1231 2705              BEQ     OP1C
                
 1233                   ; *** ERROR #3 -- ILLEGAL 1-OP ***
                
 1233 8603      BADOP1: LDA     #3
 1235 BD1FA9            JSR     ZERROR
                
 1238 BD12A5    OP1C:   JSR     GETVAR          ; 10 = VARIABLE
                
 123B DC3D      OP1EX:  LDD     TEMP
 123D DD02              STD     ARG1            ; GRAB THE ARGUMENT
 123F 0C01              INC     ARGCNT          ; ONE ARGUMENT
 1241 8E13AD            LDX     #OPT1           ; ADDR OF 1-OP DISPATCH TABLE
 1244 D600              LDB     OPCODE          ; RESTORE OPCODE
 1246 C40F              ANDB    #%00001111      ; ISOLATE OP BITS
 1248 C110              CMPB    #NOPS1          ; IF OPCODE OUT OF RANGE,
 124A 24E7              BHS     BADOP1          ; REPORT IT
 124C 20B8              BRA     DODIS           ; ELSE DISPATCH THE 1-OP
                
 124E                   ; HANDLE A 2-OP
                
 124E 8440      OP2:    ANDA    #%01000000      ; ISOLATE 1ST ARG BIT
 1250 2605              BNE     OP2A
 1252 BD1286            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 1255 2003              BRA     OP2B
                
 1257 BD12A5    OP2A:   JSR     GETVAR          ; 1 = VARIABLE
                
 125A DC3D      OP2B:   LDD     TEMP            ; GRAB VALUE
 125C DD02              STD     ARG1            ; SAVE IN ARG1
 125E 0C01              INC     ARGCNT
                
 1260 9600              LDA     OPCODE          ; RESTORE OPCODE
 1262 8420              ANDA    #%00100000      ; ISOLATE 2ND ARG BIT
 1264 2605              BNE     OP2C
 1266 BD1286            JSR     GETSHT          ; 0 = SHORT IMMEDIATE
 1269 2003              BRA     OP2D
                
 126B BD12A5    OP2C:   JSR     GETVAR          ; 1 = VARIABLE
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 11                 
--- MAIN LOOP ---                                                                                                                   

 126E DC3D      OP2D:   LDD     TEMP            ; GRAB 2ND VALUE
 1270 DD04              STD     ARG2            ; STORE AS ARG2
 1272 0C01              INC     ARGCNT
                
 1274 8E13CD    OP2EX:  LDX     #OPT2           ; ADDR OF 2-OP DISPATCH TABLE
 1277 D600              LDB     OPCODE          ; RESTORE YET AGAIN
 1279 C41F              ANDB    #%00011111      ; ISOLATE OP BITS
 127B C119              CMPB    #NOPS2          ; OPCODE IN RANGE?
 127D 1025FF85          LBLO    DODIS           ; YES, GO DISPATCH IT
                
 1281                   ; *** ERROR #4 -- ILLEGAL 2-OP ***
                
 1281 8604      BADOP2: LDA     #4
 1283 BD1FA9            JSR     ZERROR
                
                        END
                        INCLUD MAINSUBS.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 12                 
--- MAIN LOOP SUPPORT ---                                                                                                           

                
 1286                   ; -----------------------
 1286                   ; FETCH A SHORT IMMEDIATE
 1286                   ; -----------------------
                
 1286 BD1A55    GETSHT: JSR     NEXTPC          ; NEXT Z-BYTE IS
 1289 973E              STA     TEMP+1          ; THE LSB OF ARGUMENT
 128B 0F3D              CLR     TEMP            ; MSB IS ZERO
 128D 39                RTS
                
 128E                   ; ----------------------
 128E                   ; FETCH A LONG IMMEDIATE
 128E                   ; ----------------------
                
 128E BD1A55    GETLNG: JSR     NEXTPC          ; NEXT Z-BYTE IS MSB
 1291 3402              PSHS    A               ; SAVE ON STACK
 1293 BD1A55            JSR     NEXTPC          ; NOW GRAB LSB
 1296 973E              STA     TEMP+1          ; STORE IT
 1298 3502              PULS    A               ; RETRIEVE MSB
 129A 973D              STA     TEMP            ; AND STORE IT
 129C 39                RTS
                
 129D                   ; ----------------
 129D                   ; FETCH A VARIABLE
 129D                   ; ----------------
                
 129D                   ; GET WITHIN AN OPCODE
                
 129D 4D        VARGET: TSTA                    ; IF NON-ZERO,
 129E 260B              BNE     GETVR1          ; ACCESS A VARIABLE
 12A0 BD130A            JSR     POPSTK          ; ELSE TAKE VAR OFF STACK
 12A3 205A              BRA     PSHSTK          ; WITHOUT ALTERING STACK
                
 12A5 BD1A55    GETVAR: JSR     NEXTPC          ; GRAB VAR-TYPE BYTE
 12A8 4D                TSTA                    ; IF ZERO,
 12A9 275F              BEQ     POPSTK          ; VALUE IS ON STACK
                
 12AB                   ; IS VARIABLE LOCAL OR GLOBAL?
                
 12AB 8110      GETVR1: CMPA    #16
 12AD 240D              BHS     GETVRG          ; IT'S GLOBAL
                
 12AF                   ; HANDLE A LOCAL VARIABLE
                
 12AF 4A        GETVRL: DECA                    ; FORM A ZERO-ALIGNED INDEX
 12B0 48                ASLA                    ; WORD INDEX
 12B1 8E1000            LDX     #LOCALS         ; INTO LOCAL VAR TABLE
 12B4 1F89              TFR     A,B             ; MOVE AND
 12B6 3A        GTVX:   ABX                     ; ADD INDEXING OFFSET
 12B7 EC84              LDD     ,X              ; FETCH VALUE
 12B9 DD3D              STD     TEMP            ; AND RETURN IT


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 13                 
--- MAIN LOOP SUPPORT ---                                                                                                           

 12BB 39                RTS
                
 12BC                   ; HANDLE A GLOBAL VARIABLE
                
 12BC 8010      GETVRG: SUBA    #16             ; ZERO-ALIGN
 12BE 9E1F              LDX     GLOBAL          ; BASE OF GLOBAL VAR TABLE
 12C0 1F89              TFR     A,B             ; CONVERT TO WORD-ALIGNED INDEX
 12C2 3A                ABX                     ; BY ADDING OFFSET TWICE (CLEVER, EH?)
 12C3 20F1              BRA     GTVX            ; 2ND ADD ABOVE
                
 12C5                   ; --------------
 12C5                   ; RETURN A VALUE
 12C5                   ; --------------
                
 12C5                   ; RETURN FROM WITHIN OPCODE
                
 12C5 4D        VARPUT: TSTA                    ; IF NON-ZERO
 12C6 261D              BNE     PUTVR1          ; ACCESS A VARIABLE
 12C8 3706              PULU    D               ; ELSE FLUSH TOP ITEM OFF STACK
 12CA 11830DFE          CMPU    #TOPSTA
 12CE 2249              BHI     UNDER           ; WATCH FOR UNDERFLOW!
 12D0 202D              BRA     PSHSTK          ; AND PUSH [TEMP] ONTO STACK
                
 12D2                   ; RETURN A ZERO
                
 12D2 4F        RET0:   CLRA                    ; CLEAR MSB
                
 12D3                   ; RETURN BYTE IN [A]
                
 12D3 973E      PUTBYT: STA     TEMP+1          ; USE [A] AS LSB
 12D5 0F3D              CLR     TEMP            ; ZERO MSB
                
 12D7                   ; RETURN VALUE IN [TEMP]
                
 12D7 9E3D      PUTVAL: LDX     TEMP            ; GET VALUE IN [TEMP]
 12D9 3410              PSHS    X               ; AND HOLD ON TO IT
 12DB BD1A55            JSR     NEXTPC          ; GET VAR-TYPE BYTE
 12DE 3510              PULS    X               ; RETRIEVE VALUE
 12E0 9F3D              STX     TEMP            ; PUT IT BACK IN [TEMP]
 12E2 4D                TSTA                    ; IF TYPE-BYTE IS ZERO,
 12E3 271A              BEQ     PSHSTK          ; VALUE GOES TO THE STACK
                
 12E5                   ; LOCAL OR GLOBAL?
                
 12E5 8110      PUTVR1: CMPA    #16
 12E7 240D              BHS     PUTVLG          ; IT'S GLOBAL
                
 12E9                   ; HANDLE A LOCAL VARIABLE
                
 12E9 4A        PUTVLL: DECA
 12EA 48                ASLA


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 14                 
--- MAIN LOOP SUPPORT ---                                                                                                           

 12EB 1F89              TFR     A,B
 12ED 8E1000            LDX     #LOCALS         ; INTO LOCAL VARIABLE TABLE
 12F0 3A        PTVX:   ABX
 12F1 DC3D              LDD     TEMP
 12F3 ED84              STD     ,X
 12F5 39                RTS
                
 12F6                   ; HANDLE A GLOBAL VARIABLE
                
 12F6 8010      PUTVLG: SUBA    #16             ; ZERO-ALIGN
 12F8 9E1F              LDX     GLOBAL          ; BASE OF GLOBAL VAR TABLE
 12FA 1F89              TFR     A,B             ; FORM WORD-ALIGNED INDEX
 12FC 3A                ABX                     ; BY ADDING OFFSET TO BASE
 12FD 20F1              BRA     PTVX            ; TWICE
                
 12FF                   ; --------------------
 12FF                   ; PUSH [TEMP] TO STACK
 12FF                   ; --------------------
                
 12FF DC3D      PSHSTK: LDD     TEMP
                
 1301                   ; PUSH [D] TO STACK
                
 1301 3606      PSHDZ:  PSHU    D
 1303 11830C00          CMPU    #ZSTACK
 1307 250C              BLO     OVER
 1309 39                RTS
                
 130A                   ; -------------------------
 130A                   ; POP STACK, SAVE IN [TEMP]
 130A                   ; -------------------------
                
 130A 3706      POPSTK: PULU    D               ; PULL A WORD
 130C DD3D              STD     TEMP            ; SAVE IT IN [TEMP]
 130E 11830DFE          CMPU    #TOPSTA
 1312 2205              BHI     UNDER
 1314 39                RTS
                
 1315                   ; *** ERROR #5 -- Z-STACK OVERFLOW ***
                
 1315 8605      OVER:   LDA     #5              ; ERROR #5 (Z-STACK OVERFLOW)
 1317 2002              BRA     STERR
                
 1319                   ; *** ERROR #6 -- Z-STACK UNDERFLOW ***
                
 1319 8606      UNDER:  LDA     #6              ; ERROR #6 (Z-STACK UNDERFLOW)
 131B BD1FA9    STERR:  JSR     ZERROR
                
 131E                   ; ---------------
 131E                   ; PREDICATE FAILS
 131E                   ; ---------------


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 15                 
--- MAIN LOOP SUPPORT ---                                                                                                           

                
 131E BD1A55    PREDF:  JSR     NEXTPC          ; GET 1ST BRANCH BYTE
 1321 4D                TSTA                    ; IF BIT 7 ISN'T SET,
 1322 2A0E              BPL     PREDB           ; DO THE BRANCH
                
 1324 8440      PREDNB: ANDA    #%01000000      ; ELSE TEST BIT 6
 1326 2603              BNE     PNBX            ; ALL DONE IF SET
 1328 BD1A55            JSR     NEXTPC          ; ELSE SKIP OVER 2ND BRANCH BYTE
 132B 39        PNBX:   RTS                     ; BEFORE LEAVING
                
 132C                   ; ------------------
 132C                   ; PREDICATE SUCCEEDS
 132C                   ; ------------------
                
 132C BD1A55    PREDS:  JSR     NEXTPC
 132F 4D                TSTA                    ; IF BIT 7 IS SET,
 1330 2AF2              BPL     PREDNB          ; BRANCH ON PREDICATE FAILURE
                
 1332                   ; ----------------
 1332                   ; PERFORM A BRANCH
 1332                   ; ----------------
                
 1332 8540      PREDB:  BITA    #%01000000      ; LONG OR SHORT BRANCH?
 1334 2708              BEQ     PREDLB          ; LONG IF BIT 6 IS OFF
 1336 843F              ANDA    #%00111111      ; ELSE FORM SHORT OFFSET
 1338 973E              STA     TEMP+1          ; USE AS LSB OF BRANCH OFFSET
 133A 0F3D              CLR     TEMP            ; ZERO MSB OF OFFSET
 133C 2013              BRA     PREDB1          ; AND DO THE BRANCH
                
 133E                   ; HANDLE A LONG BRANCH
                
 133E 843F      PREDLB: ANDA    #%00111111      ; FORM MSB OF OFFSET
 1340 8520              BITA    #%00100000      ; CHECK SIGN OF 14-BIT VALUE
 1342 2702              BEQ     DOB2            ; IT'S POSITIVE
 1344 8AE0              ORA     #%11100000      ; ELSE EXTEND SIGN BITS
 1346 3402      DOB2:   PSHS    A               ; SAVE MSB OF BRANCH
 1348 BD1A55            JSR     NEXTPC          ; GRAB NEXT Z-BYTE
 134B 973E              STA     TEMP+1          ; USE AS LSB OF BRANCH
 134D 3502              PULS    A
 134F 973D              STA     TEMP            ; RETRIEVE MSB
                
 1351                   ; BRANCH TO Z-ADDRESS IN [TEMP]
                
 1351 DC3D      PREDB1: LDD     TEMP            ; IF OFFSET IS ZERO,
 1353 102700C8          LBEQ    ZRFALS          ; DO AN "RFALSE"
 1357 830001            SUBD    #1              ; IF OFFSET IS ONE,
 135A 102700B9          LBEQ    ZRTRUE          ; DO AN "RTRUE"
                
 135E 830001    PREDB3: SUBD    #1              ; D = OFFSET-2
 1361 DD3D              STD     TEMP            ; SAVE NEW OFFSET
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 16                 
--- MAIN LOOP SUPPORT ---                                                                                                           

 1363                   ; USE [VAL] TO HOLD TOP 9 BITS OF OFFSET
                
 1363 973C              STA     VAL+1
 1365 5F                CLRB
 1366 48                ASLA                    ; EXTEND THE SIGN BIT
 1367 59                ROLB                    ; SHIFT CARRY TO BIT 0 OF [B]
 1368 D73B              STB     VAL             ; SAVE AS UPPER BYTE OF OFFSET
                
 136A 963E              LDA     TEMP+1          ; GET LOW BYTE OF OFFSET
 136C 1CFE              ANDCC   #%11111110      ; CLEAR CARRY
 136E 9913              ADCA    ZPCL            ; ADD LOW BYTE OF CURRENT ZPC
 1370 2406              BCC     PDB0            ; IF OVERFLOWED,
                
 1372 0C3C              INC     VAL+1           ; UPDATE
 1374 2602              BNE     PDB0            ; UPPER
 1376 0C3B              INC     VAL             ; 9 BITS
                
 1378 9713      PDB0:   STA     ZPCL            ; LOW-BYTES CALCED
                
 137A DC3B              LDD     VAL             ; IF 9 UPPER BITS ARE ZERO,
 137C 2712              BEQ     PDB1            ; NO NEED TO CHANGE PAGES
                
 137E 963C              LDA     VAL+1           ; ELSE ADD MIDDLE BYTES
 1380 1CFE              ANDCC   #%11111110      ; CLEAR CARRY
 1382 9912              ADCA    ZPCM
 1384 9712              STA     ZPCM
 1386 963B              LDA     VAL             ; NOW ADD THE TOP BITS
 1388 9911              ADCA    ZPCH            ; USING PREVIOUS CARRY
 138A 8401              ANDA    #%00000001      ; ISOLATE BIT 0
 138C 9711              STA     ZPCH
 138E 0F16              CLR     ZPCFLG          ; CHANGED PAGES
 1390 39        PDB1:   RTS
                
                        END
                        INCLUD DISPATCH.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 17                 
--- OPCODE DISPATCH TABLES ---                                                                                                      

                
 1391                   ; 0-OPS
                
 1391 1417      OPT0:   DW      ZRTRUE          ; 0
 1393 141F              DW      ZRFALS          ; 1
 1395 1422              DW      ZPRI            ; 2
 1397 1440              DW      ZPRR            ; 3
 1399 143F              DW      ZNOOP           ; 4
 139B 21AE              DW      ZSAVE           ; 5
 139D 2260              DW      ZREST           ; 6
 139F 1F6B              DW      ZSTART          ; 7
 13A1 1447              DW      ZRSTAK          ; 8
 13A3 130A              DW      POPSTK          ; 9
 13A5 1FC2              DW      ZQUIT           ; 10
 13A7 2037              DW      ZCRLF           ; 11
 13A9 207B              DW      ZUSL            ; 12
 13AB 144F              DW      ZVER            ; 13
                
 000E           NOPS0   EQU     14              ; NUMBER OF 0-OPS
                
 13AD                   ; 1-OPS
                
 13AD 149C      OPT1:   DW      ZZERO           ; 0
 13AF 14A5              DW      ZNEXT           ; 1
 13B1 14AE              DW      ZFIRST          ; 2
 13B3 14CC              DW      ZLOC            ; 3
 13B5 14DC              DW      ZPTSIZ          ; 4
 13B7 14EE              DW      ZINC            ; 5
 13B9 1506              DW      ZDEC            ; 6
 13BB 1512              DW      ZPRB            ; 7
 13BD 1233              DW      BADOP1          ; 8 (UNDEFINED)
 13BF 151C              DW      ZREMOV          ; 9
 13C1 155C              DW      ZPRD            ; 10
 13C3 1570              DW      ZRET            ; 11
 13C5 159F              DW      ZJUMP           ; 12
 13C7 15A9              DW      ZPRINT          ; 13
 13C9 15B3              DW      ZVALUE          ; 14
 13CB 15BB              DW      ZBCOM           ; 15
                
 0010           NOPS1   EQU     16              ; NUMBER OF 1-OPS
                
 13CD                   ; 2-OPS
                
 13CD 1281      OPT2:   DW      BADOP2          ; 0 (UNDEFINED)
 13CF 17E0              DW      ZEQUAL          ; 1
 13D1 15C4              DW      ZLESS           ; 2
 13D3 15CE              DW      ZGRTR           ; 3
 13D5 15D8              DW      ZDLESS          ; 4
 13D7 15E1              DW      ZIGRTR          ; 5
 13D9 1604              DW      ZIN             ; 6
 13DB 1614              DW      ZBTST           ; 7


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- MAINLINE                                                                         PAGE 18                 
--- OPCODE DISPATCH TABLES ---                                                                                                      

 13DD 1621              DW      ZBOR            ; 8
 13DF 162C              DW      ZBAND           ; 9
 13E1 1634              DW      ZFSETP          ; 10
 13E3 1645              DW      ZFSET           ; 11
 13E5 1653              DW      ZFCLR           ; 12
 13E7 1663              DW      ZSET            ; 13
 13E9 166C              DW      ZMOVE           ; 14
 13EB 1692              DW      ZGET            ; 15
 13ED 16A5              DW      ZGETB           ; 16
 13EF 16B8              DW      ZGETP           ; 17
 13F1 16FC              DW      ZGETPT          ; 18
 13F3 1720              DW      ZNEXTP          ; 19
 13F5 1740              DW      ZADD            ; 20
 13F7 1749              DW      ZSUB            ; 21
 13F9 174F              DW      ZMUL            ; 22
 13FB 176E              DW      ZDIV            ; 23
 13FD 1773              DW      ZMOD            ; 24
                
 0019           NOPS2   EQU     25              ; NUMBER OF 2-OPS
                
 13FF                   ; X-OPS
                
 13FF 1808      OPTX:   DW      ZCALL           ; 0
 1401 1876              DW      ZPUT            ; 1
 1403 1888              DW      ZPUTB           ; 2
 1405 1896              DW      ZPUTP           ; 3
 1407 1933              DW      ZREAD           ; 4
 1409 18CC              DW      ZPRC            ; 5
 140B 18D1              DW      ZPRN            ; 6
 140D 190C              DW      ZRAND           ; 7
 140F 1926              DW      ZPUSH           ; 8
 1411 192B              DW      ZPOP            ; 9
 1413 143F              DW      ZSPLIT          ; 10
 1415 143F              DW      ZSCRN           ; 11
                
 000C           NOPSX   EQU     12              ; NUMBER OF X-OPS
                
                        END
                
                        INCLUD OPS0.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 19                 
--- 0-OPS ---                                                                                                                       

                
 1417                   ; -----
 1417                   ; RTRUE
 1417                   ; -----
                
 1417                   ; Simulate a RETURN 1
                
 1417 C601      ZRTRUE: LDB     #1
                
 1419 4F        ZRT:    CLRA
 141A DD02              STD     ARG1            ; SAVE VALUE HERE
 141C 7E1570            JMP     ZRET
                
 141F                   ; ------
 141F                   ; RFALSE
 141F                   ; ------
                
 141F                   ; Simulate a RETURN 0
                
 141F 5F        ZRFALS: CLRB
 1420 20F7              BRA     ZRT
                
 1422                   ; ------
 1422                   ; PRINTI
 1422                   ; ------
                
 1422                   ; Print the Z-string immediately following the opcode
                
 1422 9611      ZPRI:   LDA     ZPCH            ; MOVE ZPC INTO MPC
 1424 9718              STA     MPCH
 1426 DC12              LDD     ZPCM
 1428 DD19              STD     MPCM
 142A 0F1D              CLR     MPCFLG          ; ZERO MPC FLAG
                
 142C BD1B1F            JSR     PZSTR           ; PRINT THE STRING AT [MPC]
                
 142F 9618              LDA     MPCH            ; UPDATE ZPC FROM MPC
 1431 9711              STA     ZPCH
 1433 DC19              LDD     MPCM
 1435 DD12              STD     ZPCM
 1437 961D              LDA     MPCFLG          ; ALSO UPDATE FLAG
 1439 9716              STA     ZPCFLG
 143B DC1B              LDD     MPCPNT          ; AND PAGE POINTER
 143D DD14              STD     ZPCPNT
                
 143F                   ; FALL THROUGH TO ...
                
 143F                   ; ----
 143F                   ; NOOP
 143F                   ; ----
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 20                 
--- 0-OPS ---                                                                                                                       

 143F 39        ZNOOP:  RTS
                
 1440                   ; ------
 1440                   ; PRINTR
 1440                   ; ------
                
 1440                   ; Execute a PRINTI, followed by CRLF and RTRUE
                
 1440 8DE0      ZPRR:   BSR     ZPRI
 1442 BD2037            JSR     ZCRLF
 1445 20D0              BRA     ZRTRUE
                
 1447                   ; ------
 1447                   ; RSTACK
 1447                   ; ------
                
 1447                   ; Execute a RETURN, with CALL value on top of the stack
                
 1447 BD130A    ZRSTAK: JSR     POPSTK
 144A DD02              STD     ARG1            ; TOS WAS LEFT IN [D]
 144C 7E1570            JMP     ZRET
                
 144F                   ; ------
 144F                   ; VERIFY
 144F                   ; ------
                
 144F                   ; Verify the game code
                
 144F BD1FEB    ZVER:   JSR     VERNUM          ; DISPLAY ZIP VERSION CODE
 1452 FC251A            LDD     ZCODE+ZLENTH    ; GET LENGTH OF Z-CODE
 1455 DD04              STD     ARG2            ; IN WORDS
                
 1457                   ; CLEAR VARIABLES
                
 1457 4F                CLRA
 1458 5F                CLRB
 1459 DD02              STD     ARG1
 145B DD06              STD     ARG3            ; BIT 17 OF Z-CODE LENGTH
 145D DD3D              STD     TEMP            ; BYTE COUNT
                
 145F                   ; CONVERT Z-CODE LENGTH TO BYTES
                
 145F 0805              ASL     ARG2+1          ; BOTTOM 8 BITS
 1461 0904              ROL     ARG2            ; MIDDLE 8 BITS
 1463 0907              ROL     ARG3+1          ; 17TH BIT OF LENGTH
                
 1465 8640              LDA     #$40            ; 1ST 64 BYTES
 1467 973E              STA     TEMP+1          ; ARE NOT CHECKED
 1469 BD1ACE            JSR     SETWRD          ; [TEMP] POINTS TO FIRST BYTE
                
 146C 8606              LDA     #ARG3           ; PATCH [GETBYT] ROUTINE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 21                 
--- 0-OPS ---                                                                                                                       

 146E B71AA9            STA     PATCH           ; SO PRELOAD WILL BE READ FROM DISK
                
 1471 BD1A8B    VSUM:   JSR     GETBYT          ; GET A BYTE
 1474 5F                CLRB                    ; CLEAR CARRY
 1475 9903              ADCA    ARG1+1          ; ADD TO SUM
 1477 9703              STA     ARG1+1
 1479 2402              BCC     VSUM0
 147B 0C02              INC     ARG1
                
 147D DC19      VSUM0:  LDD     MPCM            ; END OF GAME YET?
 147F 109304            CMPD    ARG2
 1482 26ED              BNE     VSUM
                
 1484 9618              LDA     MPCH            ; ALSO CHECK TOP BIT
 1486 9107              CMPA    ARG3+1
 1488 26E7              BNE     VSUM
                
 148A 860B              LDA     #ZPURE
 148C B71AA9            STA     PATCH           ; UNPATCH [GETBYT]
                
 148F FC251C            LDD     ZCODE+ZCHKSM    ; GET CHECKSUM
 1492 109302            CMPD    ARG1            ; SAME AS CALCULATED?
 1495 1027FE93          LBEQ    PREDS           ; YES, PREDICATE SUCCEEDS
 1499 7E131E            JMP     PREDF           ; ELSE FAILURE ...
                
                        END
                        INCLUD OPS1.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 22                 
--- 1-OPS ---                                                                                                                       

                
 149C                   ; -----
 149C                   ; ZERO?
 149C                   ; -----
                
 149C                   ; Is arg1 equal to zero? [PRED]
                
 149C DC02      ZZERO:  LDD     ARG1
 149E 1027FE8A          LBEQ    PREDS
 14A2 7E131E            JMP     PREDF
                
 14A5                   ; -----
 14A5                   ; NEXT?
 14A5                   ; -----
                
 14A5                   ; Return the NEXT pointer in object "arg1"; fail if
 14A5                   ; none left, and return zero [VALUE][PRED]
                
 14A5 9603      ZNEXT:  LDA     ARG1+1
 14A7 BD1D69            JSR     OBJLOC
 14AA C605              LDB     #5              ; SAME AS FIRST?
 14AC 2007              BRA     FIRST1
                
 14AE                   ; ------
 14AE                   ; FIRST?
 14AE                   ; ------
                
 14AE                   ; Return the FIRST pointer in object "arg1"; fail if
 14AE                   ; none, and return zero [VALUE][PRED]
                
 14AE 9603      ZFIRST: LDA     ARG1+1
 14B0 BD1D69            JSR     OBJLOC
 14B3 C606              LDB     #6
                
 14B5 9E3D      FIRST1: LDX     TEMP
 14B7 A685              LDA     B,X             ; FETCH SLOT
 14B9 973E              STA     TEMP+1          ; SAVE HERE
 14BB 3402              PSHS    A               ; AND ON STACK
 14BD 0F3D              CLR     TEMP            ; ZERO MSB
 14BF BD12D7            JSR     PUTVAL
 14C2 3502              PULS    A
 14C4 4D                TSTA
 14C5 1027FE55          LBEQ    PREDF           ; FAILURE
 14C9 7E132C            JMP     PREDS           ; OR SUCCESS
                
 14CC                   ; ---
 14CC                   ; LOC
 14CC                   ; ---
                
 14CC                   ; Return the object containing object "arg1"; zero if none [VALUE]
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 23                 
--- 1-OPS ---                                                                                                                       

 14CC 9603      ZLOC:   LDA     ARG1+1
 14CE BD1D69            JSR     OBJLOC
 14D1 9E3D              LDX     TEMP
 14D3 A604              LDA     4,X
 14D5 973E              STA     TEMP+1
 14D7 0F3D              CLR     TEMP
 14D9 7E12D7            JMP     PUTVAL
                
 14DC                   ; ------
 14DC                   ; PTSIZE
 14DC                   ; ------
                
 14DC                   ; Return length of prop table "arg1" in bytes [VALUE]
                
 14DC DC02      ZPTSIZ: LDD     ARG1
 14DE C32500            ADDD    #ZCODE
 14E1 830001            SUBD    #1
 14E4 DD3D              STD     TEMP
 14E6 5F                CLRB
 14E7 BD1D23            JSR     PROPL
 14EA 4C                INCA
 14EB 7E12D3            JMP     PUTBYT
                
 14EE                   ; ---
 14EE                   ; INC
 14EE                   ; ---
                
 14EE                   ; Increment arg1 [VALUE]
                
 14EE 9603      ZINC:   LDA     ARG1+1
 14F0 BD129D            JSR     VARGET
 14F3 DC3D              LDD     TEMP
 14F5 C30001            ADDD    #1
 14F8 DD3D      ZINC1:  STD     TEMP
 14FA 3406              PSHS    D
 14FC 9603              LDA     ARG1+1
 14FE BD12C5            JSR     VARPUT
 1501 3506              PULS    D
 1503 DD3D              STD     TEMP
 1505 39                RTS
                
 1506                   ; ---
 1506                   ; DEC
 1506                   ; ---
                
 1506                   ; Decrement arg1 [VALUE]
                
 1506 9603      ZDEC:   LDA     ARG1+1
 1508 BD129D            JSR     VARGET
 150B DC3D              LDD     TEMP
 150D 830001            SUBD    #1


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 24                 
--- 1-OPS ---                                                                                                                       

 1510 20E6              BRA     ZINC1
                
 1512                   ; ------
 1512                   ; PRINTB
 1512                   ; ------
                
 1512                   ; PRINT the string pointed to by BYTE-pointer "arg1"
                
 1512 DC02      ZPRB:   LDD     ARG1
 1514 DD3D              STD     TEMP
 1516 BD1ACE            JSR     SETWRD
 1519 7E1B1F            JMP     PZSTR
                
 151C                   ; ------
 151C                   ; REMOVE
 151C                   ; ------
                
 151C                   ; Move object "arg1" to pseudo-object #0
                
 151C 9603      ZREMOV: LDA     ARG1+1
 151E BD1D69            JSR     OBJLOC
 1521 9E3D              LDX     TEMP
 1523 A604              LDA     4,X
 1525 2734              BEQ     REMVEX          ; NO OBJECT
                
 1527 3410              PSHS    X               ; SAVE [TEMP]
                
 1529 BD1D69            JSR     OBJLOC
 152C 9E3D              LDX     TEMP
 152E A606              LDA     6,X
 1530 9103              CMPA    ARG1+1
 1532 260C              BNE     REMVC1
                
 1534 3510              PULS    X               ; RETRIEVE FORMER [TEMP]
 1536 3410              PSHS    X               ; SAVE COPY ON STACK
                
 1538 A605              LDA     5,X             ; OLD [TEMP] IS IN [X]
 153A 9E3D              LDX     TEMP
 153C A706              STA     6,X
                
 153E 2015              BRA     REMVC2
                
 1540 BD1D69    REMVC1: JSR     OBJLOC
 1543 9E3D              LDX     TEMP
 1545 A605              LDA     5,X
 1547 9103              CMPA    ARG1+1
 1549 26F5              BNE     REMVC1
                
 154B 3510              PULS    X
 154D 3410              PSHS    X
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 25                 
--- 1-OPS ---                                                                                                                       

 154F A605              LDA     5,X
 1551 9E3D              LDX     TEMP
 1553 A705              STA     5,X
                
 1555 3510      REMVC2: PULS    X
 1557 6F04              CLR     4,X
 1559 6F05              CLR     5,X
                
 155B 39        REMVEX: RTS
                
 155C                   ; ------
 155C                   ; PRINTD
 155C                   ; ------
                
 155C                   ; Print short description of object "arg1"
                
 155C 9603      ZPRD:   LDA     ARG1+1
                
 155E BD1D69    PRNTDC: JSR     OBJLOC
 1561 9E3D              LDX     TEMP
 1563 EC07              LDD     7,X
 1565 C30001            ADDD    #1              ; INCREMENT
 1568 DD3D              STD     TEMP            ; AND SAVE
 156A BD1ACE            JSR     SETWRD
 156D 7E1B1F            JMP     PZSTR
                
 1570                   ; ------
 1570                   ; RETURN
 1570                   ; ------
                
 1570                   ; Return from a CALL with value "arg1"
                
 1570 DE25      ZRET:   LDU     OZSTAK          ; STAY IN SYNC!
 1572 BD130A            JSR     POPSTK          ; POP # LOCALS
 1575 D73B              STB     VAL             ; SAVE COUNT HERE
 1577 270E              BEQ     RET2            ; SKIP IF NO LOCALS
                
 1579                   ; RESTORE LOCAL VARIABLES
                
 1579 8E1000            LDX     #LOCALS         ; SET UP A POINTER
 157C 58                ASLB                    ; WORD-ALIGN THE INDEX
 157D 3A                ABX                     ; [X] POINTS TO LAST LOCAL VAR
                
 157E BD130A    RET1:   JSR     POPSTK          ; POP A VALUE ([X] UNAFFECTED)
 1581 ED83              STD     ,--X            ; SAVE IN [LOCALS], UPDATE INDEX
 1583 0A3B              DEC     VAL
 1585 26F7              BNE     RET1            ; LOOP TILL ALL LOCALS POPPED
                
 1587                   ; RESTORE OTHER VARIABLES
                
 1587 BD130A    RET2:   JSR     POPSTK


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 26                 
--- 1-OPS ---                                                                                                                       

 158A DD11              STD     ZPCH            ; RESTORE TOP 9 BITS OF ZPC
 158C BD130A            JSR     POPSTK
 158F D713              STB     ZPCL            ; RESTORE LOWER 8 BITS OF ZPC
 1591 BD130A            JSR     POPSTK
 1594 DD25              STD     OZSTAK          ; AND OLD ZSP
 1596 0F16              CLR     ZPCFLG          ; PC NO LONGER VALID
                
 1598 DC02              LDD     ARG1
 159A DD3D              STD     TEMP            ; PASS THE RETURN VALUE
 159C 7E12D7            JMP     PUTVAL          ; TO PUTVAL
                
 159F                   ; ----
 159F                   ; JUMP
 159F                   ; ----
                
 159F                   ; Branch to location pointed to by 16-bit 2's-comp "arg1"
                
 159F DC02      ZJUMP:  LDD     ARG1            ; TREAT LIKE A BRANCH
 15A1 830001            SUBD    #1              ; THAT ALWAYS SUCCEEDS
 15A4 DD3D              STD     TEMP
 15A6 7E135E            JMP     PREDB3
                
 15A9                   ; -----
 15A9                   ; PRINT
 15A9                   ; -----
                
 15A9                   ; Print the z-string pointed to by WORD-pointer "arg1"
                
 15A9 DC02      ZPRINT: LDD     ARG1
 15AB DD3D              STD     TEMP            ; TELL SETSTR
 15AD BD1B10            JSR     SETSTR          ; WHERE THE STRING RESIDES
 15B0 7E1B1F            JMP     PZSTR           ; AND PRINT IT
                
 15B3                   ; -----
 15B3                   ; VALUE
 15B3                   ; -----
                
 15B3                   ; Return value of arg1 [VALUE]
                
 15B3 9603      ZVALUE: LDA     ARG1+1          ; GRAB VARIABLE ID
 15B5 BD129D            JSR     VARGET          ; FETCH ITS VALUE
 15B8 7E12D7            JMP     PUTVAL          ; AND RETURN IT
                
 15BB                   ; ----
 15BB                   ; BCOM
 15BB                   ; ----
                
 15BB                   ; Complement arg1 [VALUE]
                
 15BB DC02      ZBCOM:  LDD     ARG1            ; GRAB ARGUMENT
 15BD 43                COMA                    ; COMPLEMENT MSB


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 27                 
--- 1-OPS ---                                                                                                                       

 15BE 53                COMB                    ; AND LSB
 15BF DD3D              STD     TEMP            ; AND PASS TO PUTVAL
 15C1 7E12D7            JMP     PUTVAL
                
                        END
                        INCLUD OPS2.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 28                 
--- 2-OPS ---                                                                                                                       

                
 15C4                   ; -----
 15C4                   ; LESS?
 15C4                   ; -----
                
 15C4                   ; Is arg1 less than arg2? [PRED]
                
 15C4 DC02      ZLESS:  LDD     ARG1
 15C6 DD3D              STD     TEMP
 15C8 DC04              LDD     ARG2
 15CA DD3B              STD     VAL
 15CC 201E              BRA     CEXIT
                
 15CE                   ; -----
 15CE                   ; GRTR?
 15CE                   ; -----
                
 15CE                   ; Is arg1 greater than arg2? [PRED]
                
 15CE DC02      ZGRTR:  LDD     ARG1
 15D0 DD3B              STD     VAL
 15D2 DC04              LDD     ARG2
 15D4 DD3D              STD     TEMP
 15D6 2014              BRA     CEXIT
                
 15D8                   ; ------
 15D8                   ; DLESS?
 15D8                   ; ------
                
 15D8                   ; Decrement variable "arg1"; succeed if new value
 15D8                   ; is less than arg2 [PRED]
                
 15D8 BD1506    ZDLESS: JSR     ZDEC            ; DECREMENT THE VARIABLE
 15DB DC04              LDD     ARG2
 15DD DD3B              STD     VAL
 15DF 200B              BRA     CEXIT           ; AND COMPARE
                
 15E1                   ; ------
 15E1                   ; IGRTR?
 15E1                   ; ------
                
 15E1                   ; Increment variable "arg1"; succeed if new value is
 15E1                   ; greater than arg2 [PRED]
                
 15E1 BD14EE    ZIGRTR: JSR     ZINC            ; INCREMENT THE VARIABLE
 15E4 DC3D              LDD     TEMP
 15E6 DD3B              STD     VAL
 15E8 DC04              LDD     ARG2
 15EA DD3D              STD     TEMP
                
 15EC 8D05      CEXIT:  BSR     SCOMP


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 29                 
--- 2-OPS ---                                                                                                                       

 15EE 2521              BLO     POK
 15F0 7E131E    PBAD:   JMP     PREDF
                
 15F3                   ; -----------------
 15F3                   ; SIGNED COMPARISON
 15F3                   ; -----------------
                
 15F3 963B      SCOMP:  LDA     VAL             ; ARE ARGUMENTS
 15F5 983D              EORA    TEMP            ; SIGNED THE SAME?
 15F7 2A05              BPL     SCMP            ; YES, DO ORDINARY COMPARE
 15F9 963B              LDA     VAL             ; ELSE COMPARE
 15FB 913D              CMPA    TEMP            ; ONLY THE HIGH BYTES
 15FD 39                RTS
                
 15FE DC3D      SCMP:   LDD     TEMP
 1600 10933B            CMPD    VAL
 1603 39                RTS
                
 1604                   ; ---
 1604                   ; IN?
 1604                   ; ---
                
 1604                   ; Is object "arg1" contained in object "arg2?" [PRED]
                
 1604 9603      ZIN:    LDA     ARG1+1
 1606 BD1D69            JSR     OBJLOC
 1609 9E3D              LDX     TEMP
 160B 9605              LDA     ARG2+1
 160D A104              CMPA    4,X
 160F 26DF              BNE     PBAD
 1611 7E132C    POK:    JMP     PREDS
                
 1614                   ; ----
 1614                   ; BTST
 1614                   ; ----
                
 1614                   ; Is every "on" bit in arg1 also "on" in arg2? [PRED]
                
 1614 DC04      ZBTST:  LDD     ARG2
 1616 9402              ANDA    ARG1
 1618 D403              ANDB    ARG1+1
 161A 109304            CMPD    ARG2
 161D 27F2              BEQ     POK
 161F 20CF              BRA     PBAD
                
 1621                   ; ---
 1621                   ; BOR
 1621                   ; ---
                
 1621                   ; Return bitwise OR of arg1 and arg2 [VALUE]
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 30                 
--- 2-OPS ---                                                                                                                       

 1621 DC02      ZBOR:   LDD     ARG1
 1623 9A04              ORA     ARG2
 1625 DA05              ORB     ARG2+1
 1627 DD3D      ZB0:    STD     TEMP
 1629 7E12D7            JMP     PUTVAL
                
 162C                   ; ----
 162C                   ; BAND
 162C                   ; ----
                
 162C                   ; Return bitwise AND of arg1 and arg2 [VALUE]
                
 162C DC02      ZBAND:  LDD     ARG1
 162E 9404              ANDA    ARG2
 1630 D405              ANDB    ARG2+1
 1632 20F3              BRA     ZB0
                
 1634                   ; -----
 1634                   ; FSET?
 1634                   ; -----
                
 1634                   ; Is flag "arg2" set in object "arg1?" [PRED]
                
 1634 BD1D3B    ZFSETP: JSR     FLAGSU          ; GET BIT
 1637 DC3B              LDD     VAL
 1639 9441              ANDA    MASK
 163B 973B              STA     VAL
 163D D442              ANDB    MASK+1
 163F DA3B              ORB     VAL
 1641 26CE              BNE     POK             ; BIT IS ON
 1643 20AB              BRA     PBAD
                
 1645                   ; ----
 1645                   ; FSET
 1645                   ; ----
                
 1645                   ; Set flag "arg2" in object "arg1"
                
 1645 BD1D3B    ZFSET:  JSR     FLAGSU
 1648 9E3D              LDX     TEMP            ; ADDRESS OF FLAGS
 164A DC3B              LDD     VAL             ; GRAB FLAGS
 164C 9A41              ORA     MASK            ; SUPERIMPOSE THE
 164E DA42              ORB     MASK+1          ; MASKING PATTERN
 1650 ED84              STD     ,X              ; AND REPLACE FLAG
 1652 39                RTS
                
 1653                   ; ------
 1653                   ; FCLEAR
 1653                   ; ------
                
 1653                   ; Clear flag "arg2" in object "arg1"


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 31                 
--- 2-OPS ---                                                                                                                       

                
 1653 BD1D3B    ZFCLR:  JSR     FLAGSU
 1656 9E3D              LDX     TEMP            ; ADDRESS OF OBJECT
 1658 DC41              LDD     MASK            ; GRAB THE MASK
 165A 43                COMA                    ; COMPLEMENT IT
 165B 53                COMB
 165C 943B              ANDA    VAL             ; SUPERIMPOSE FLAGS
 165E D43C              ANDB    VAL+1           ; TO MASK OUT TARGET
 1660 ED84              STD     ,X              ; REPLACE THE FLAGS
 1662 39                RTS
                
 1663                   ; ---
 1663                   ; SET
 1663                   ; ---
                
 1663                   ; Set variable "arg1" equal to value "arg2"
                
 1663 DC04      ZSET:   LDD     ARG2
 1665 DD3D              STD     TEMP
 1667 9603              LDA     ARG1+1
 1669 7E12C5            JMP     VARPUT
                
 166C                   ; ----
 166C                   ; MOVE
 166C                   ; ----
                
 166C                   ; Put object "arg1" into object "arg2"
                
 166C BD151C    ZMOVE:  JSR     ZREMOV          ; REMOVE OBJECT FIRST
 166F 9603              LDA     ARG1+1
 1671 BD1D69            JSR     OBJLOC          ; GET ADDRESS OF OBJECT
 1674 9E3D              LDX     TEMP            ; PUT ADDRESS IN X
 1676 3410              PSHS    X               ; SAVE IT HERE TOO
 1678 9605              LDA     ARG2+1
 167A A704              STA     4,X
                
 167C BD1D69            JSR     OBJLOC
 167F 9E3D              LDX     TEMP
 1681 A606              LDA     6,X
 1683 973B              STA     VAL             ; HOLD HERE FOR A MOMENT
 1685 9603              LDA     ARG1+1
 1687 A706              STA     6,X
 1689 3510              PULS    X               ; RESTORE OLD [TEMP]
 168B 963B              LDA     VAL
 168D 2702              BEQ     ZMVEX
 168F A705              STA     5,X
 1691 39        ZMVEX:  RTS
                
 1692                   ; ---
 1692                   ; GET
 1692                   ; ---


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 32                 
--- 2-OPS ---                                                                                                                       

                
 1692                   ; Return value of item "arg2" in WORD-table at "arg1" [VALUE]
                
 1692 0805      ZGET:   ASL     ARG2+1
 1694 0904              ROL     ARG2            ; WORD-ALIGN ARG2
 1696 DC04              LDD     ARG2
 1698 D302              ADDD    ARG1            ; ADD OFFSET TO TABLE ADDRESS
 169A DD3D              STD     TEMP
 169C BD1ACE            JSR     SETWRD
 169F BD1AC1            JSR     GETWRD
 16A2 7E12D7            JMP     PUTVAL
                
 16A5                   ; ----
 16A5                   ; GETB
 16A5                   ; ----
                
 16A5                   ; Return value of item "arg2" in BYTE-table at "arg1" [VALUE]
                
 16A5 DC02      ZGETB:  LDD     ARG1
 16A7 D304              ADDD    ARG2
 16A9 DD3D              STD     TEMP
 16AB BD1ACE            JSR     SETWRD
 16AE BD1A8B            JSR     GETBYT
 16B1 973E              STA     TEMP+1
 16B3 0F3D              CLR     TEMP
 16B5 7E12D7            JMP     PUTVAL
                
 16B8                   ; ----
 16B8                   ; GETP
 16B8                   ; ----
                
 16B8                   ; Return prop "arg2" of object "arg1"; if specified prop
 16B8                   ; doesn't exist, return prop'th element of default object [VALUE]
                
 16B8 BD1D06    ZGETP:  JSR     PROPB           ; GET POINTER TO PROPS
 16BB BD1D1B    GETP1:  JSR     PROPN
 16BE 9105              CMPA    ARG2+1
 16C0 2718              BEQ     GETP2
 16C2 2505              BLO     GETP3
                
 16C4 BD1D30            JSR     PROPNX
 16C7 20F2              BRA     GETP1           ; TRY AGAIN WITH NEXT PROP
                
 16C9 FC250A    GETP3:  LDD     ZCODE+ZOBJEC    ; Z-ADDR OF OBJECT TABLE
 16CC C32500            ADDD    #ZCODE          ; FORM THE ABSOLUTE ADDRESS
 16CF 1F01              TFR     D,X             ; USE AS AN INDEX
 16D1 D605              LDB     ARG2+1          ; GET PROPERTY #
 16D3 5A                DECB
 16D4 58                ASLB
 16D5 3A                ABX                     ; ADD TO TABLE ADDRESS
 16D6 EC84              LDD     ,X              ; FETCH THE PROPERTY


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 33                 
--- 2-OPS ---                                                                                                                       

 16D8 201D              BRA     ETPEX           ; AND PASS IT ON
                
 16DA BD1D23    GETP2:  JSR     PROPL
 16DD 5C                INCB                    ; SOMETHING SHOULD BE IN B!
 16DE 4D                TSTA                    ; AND IN A!
 16DF 2710              BEQ     GETP2A
 16E1 8101              CMPA    #1
 16E3 2705              BEQ     GETP2B
                
 16E5                   ; *** ERROR #7: PROPERTY LENGTH ***
                
 16E5 8607              LDA     #7
 16E7 BD1FA9            JSR     ZERROR
                
 16EA 9E3D      GETP2B: LDX     TEMP
 16EC 3A                ABX
 16ED EC84              LDD     ,X
 16EF 2006              BRA     ETPEX
                
 16F1 9E3D      GETP2A: LDX     TEMP
 16F3 3A                ABX
 16F4 E684              LDB     ,X
 16F6 4F                CLRA
 16F7 DD3D      ETPEX:  STD     TEMP
 16F9 7E12D7            JMP     PUTVAL
                
 16FC                   ; -----
 16FC                   ; GETPT
 16FC                   ; -----
                
 16FC                   ; Return a POINTER to prop table "arg2" in object "arg1" [VALUE]
                
 16FC BD1D06    ZGETPT: JSR     PROPB
 16FF BD1D1B    GETPT1: JSR     PROPN
 1702 9105              CMPA    ARG2+1
 1704 2709              BEQ     GETPT2
 1706 1025FBC8          LBLO    RET0
 170A BD1D30            JSR     PROPNX          ; TRY NEXT ENTRY
 170D 20F0              BRA     GETPT1
                
 170F 0C3E      GETPT2: INC     TEMP+1
 1711 2602              BNE     GPT
 1713 0C3D              INC     TEMP
 1715 4F        GPT:    CLRA                    ; ADD OFFSET IN [B]
 1716 D33D              ADDD    TEMP
 1718 832500            SUBD    #ZCODE          ; CHANGE TO RELATIVE POINTER
 171B DD3D              STD     TEMP
 171D 7E12D7            JMP     PUTVAL
                
 1720                   ; -----
 1720                   ; NEXTP


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 34                 
--- 2-OPS ---                                                                                                                       

 1720                   ; -----
                
 1720                   ; Return prop index number of the prop following prop "arg2"
 1720                   ; in object "arg1"; return zero if last property; return
 1720                   ; 1st prop # if arg2=0; error if no prop "arg2" in "arg1" [VALUE]
                
 1720 BD1D06    ZNEXTP: JSR     PROPB
 1723 9605              LDA     ARG2+1
 1725 2713              BEQ     NXTP2
                
 1727 BD1D1B    NXTP1:  JSR     PROPN
 172A 9105              CMPA    ARG2+1
 172C 2709              BEQ     NXTP3
 172E 1025FBA0          LBCS    RET0
 1732 BD1D30            JSR     PROPNX          ; TRY NEXT ENTRY
 1735 20F0              BRA     NXTP1
                
 1737 BD1D30    NXTP3:  JSR     PROPNX
                
 173A BD1D1B    NXTP2:  JSR     PROPN
 173D 7E12D3            JMP     PUTBYT
                
 1740                   ; ---
 1740                   ; ADD
 1740                   ; ---
                
 1740                   ; Return (arg1+arg2) [VALUE]
                
 1740 DC02      ZADD:   LDD     ARG1
 1742 D304              ADDD    ARG2
 1744 DD3D      MATH:   STD     TEMP
 1746 7E12D7            JMP     PUTVAL
                
 1749                   ; ---
 1749                   ; SUB
 1749                   ; ---
                
 1749                   ; Return (arg1-arg2) [VALUE]
                
 1749 DC02      ZSUB:   LDD     ARG1
 174B 9304              SUBD    ARG2
 174D 20F5              BRA     MATH
                
 174F                   ; ---
 174F                   ; MUL
 174F                   ; ---
                
 174F                   ; Return (arg1*arg2) [VALUE]
                
 174F 8E0011    ZMUL:   LDX     #17             ; INIT LOOP INDEX
 1752 4F                CLRA                    ; CLEAR THE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 35                 
--- 2-OPS ---                                                                                                                       

 1753 5F                CLRB                    ; CARRY
 1754 DD45              STD     MTEMP           ; AND TEMP REGISTER
                
 1756 0645      ZMLOOP: ROR     MTEMP
 1758 0646              ROR     MTEMP+1
 175A 0604              ROR     ARG2            ; SHIFT A BIT
 175C 0605              ROR     ARG2+1          ; INTO POSITION
 175E 2406              BCC     ZMNEXT          ; NO ADDITION IF BIT CLEAR
                
 1760 DC02              LDD     ARG1
 1762 D345              ADDD    MTEMP
 1764 DD45              STD     MTEMP
                
 1766 301F      ZMNEXT: LEAX    -1,X            ; ALL BITS EXAMINED?
 1768 26EC              BNE     ZMLOOP          ; NO, KEEP SHIFTING
                
 176A DC04              LDD     ARG2            ; ELSE GRAB PRODUCT
 176C 20D6              BRA     MATH            ; AND RETURN
                
 176E                   ; ---------
 176E                   ; DIV & MOD
 176E                   ; ---------
                
 176E                   ; DIV: Return quotient of int(arg1/arg2) [VALUE]
 176E                   ; MOD: Return remainder of int(arg1/arg2) [VALUE]
                
 176E 8D09      ZDIV:   BSR     DVINIT
 1770 7E12D7            JMP     PUTVAL          ; AND SHIP OUT [TEMP]
                
 1773 8D04      ZMOD:   BSR     DVINIT
 1775 DC3B              LDD     VAL             ; RETURN THE
 1777 20CB              BRA     MATH            ; REMAINDER IN [VAL]
                
 1779                   ; -----------
 1779                   ; DIVIDE INIT
 1779                   ; -----------
                
 1779 DC02      DVINIT: LDD     ARG1
 177B DD3D              STD     TEMP
 177D DC04              LDD     ARG2
 177F DD3B              STD     VAL
                
 1781                   ; FALL THROUGH ...
                
 1781                   ; ---------------
 1781                   ; SIGNED DIVISION
 1781                   ; ---------------
                
 1781                   ; ENTRY: DIVIDEND IN [TEMP], DIVISOR IN [VAL]
 1781                   ; EXIT: QUOTIENT IN [TEMP], REMAINDER IN [VAL]
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 36                 
--- 2-OPS ---                                                                                                                       

 1781 963D      DIVIDE: LDA     TEMP            ; SIGN OF REMAINDER
 1783 9744              STA     SREM            ; IS ALWAYS SIGN OF DIVIDEND
 1785 983B              EORA    VAL             ; SIGN OF QUOTIENT IS POSITIVE
 1787 9743              STA     SQUOT           ; IF SIGNS OF TERMS ARE THE SAME
                
 1789 0D3D              TST     TEMP            ; IF DIVIDEND IS NEGATIVE,
 178B 2A02              BPL     TABS            ; CALC ABSOLUTE VALUE
 178D 8D19              BSR     ABTEMP
                
 178F 0D3B      TABS:   TST     VAL             ; IF DIVISOR IS NEGATIVE,
 1791 2A02              BPL     DOUDIV          ; DO THE SAME
 1793 8D0C              BSR     ABSVAL
                
 1795 8D18      DOUDIV: BSR     UDIV            ; UNSIGNED DIVIDE
                
 1797 0D43              TST     SQUOT
 1799 2A02              BPL     RFLIP
 179B 8D0B              BSR     ABTEMP
                
 179D 0D44      RFLIP:  TST     SREM
 179F 2A06              BPL     DIVEX
                
 17A1                   ; FALL THROUGH ...
                
 17A1                   ; -------------
 17A1                   ; CALC ABS(VAL)
 17A1                   ; -------------
                
 17A1 4F        ABSVAL: CLRA
 17A2 5F                CLRB
 17A3 933B              SUBD    VAL
 17A5 DD3B              STD     VAL
                
 17A7 39        DIVEX:  RTS
                
 17A8                   ; --------------
 17A8                   ; CALC ABS(TEMP)
 17A8                   ; --------------
                
 17A8 4F        ABTEMP: CLRA
 17A9 5F                CLRB
 17AA 933D              SUBD    TEMP
 17AC DD3D              STD     TEMP
 17AE 39                RTS
                
 17AF                   ; -----------------
 17AF                   ; UNSIGNED DIVISION
 17AF                   ; -----------------
                
 17AF                   ; ENTRY: DIVIDEND IN [TEMP], DIVISOR IN [VAL]
 17AF                   ; EXIT: QUOTIENT IN [TEMP], REMAINDER IN [VAL]


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 37                 
--- 2-OPS ---                                                                                                                       

                
 17AF DC3B      UDIV:   LDD     VAL
 17B1 2728              BEQ     DIVERR          ; CAN'T DIVIDE BY ZERO!
                
 17B3 8E0010            LDX     #16             ; INIT LOOP INDEX
 17B6 4F                CLRA                    ; CLEAR THE
 17B7 5F                CLRB                    ; CARRY
 17B8 DD45              STD     MTEMP           ; AND HI-DIVIDEND REGISTER
                
 17BA 093E      UDLOOP: ROL     TEMP+1
 17BC 093D              ROL     TEMP
 17BE 0946              ROL     MTEMP+1
 17C0 0945              ROL     MTEMP
                
 17C2 DC45              LDD     MTEMP           ; IS DIVIDEND < DIVISOR?
 17C4 933B              SUBD    VAL
 17C6 2505              BCS     UDNEXT          ; YES, CLEAR THE CARRY AND LOOP
 17C8 DD45              STD     MTEMP           ; ELSE UPDATE DIVIDEND
 17CA 43                COMA                    ; SET THE CARRY
 17CB 2001              BRA     DECX            ; AND LOOP
                
 17CD 4F        UDNEXT: CLRA                    ; CLEAR CARRY
                
 17CE 301F      DECX:   LEAX    -1,X
 17D0 26E8              BNE     UDLOOP
                
 17D2 093E              ROL     TEMP+1          ; SHIFT LAST CARRY INTO PLACE
 17D4 093D              ROL     TEMP
 17D6 DC45              LDD     MTEMP           ; MOVE REMAINDER INTO
 17D8 DD3B              STD     VAL             ; ITS RIGHTFUL PLACE
 17DA 39                RTS
                
 17DB                   ; *** ERROR #8: DIVISION ***
                
 17DB 8608      DIVERR: LDA     #8
 17DD BD1FA9            JSR     ZERROR
                
                        END
                        INCLUD OPSX.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 38                 
--- X-OPS ---                                                                                                                       

                
 17E0                   ; ------
 17E0                   ; EQUAL?
 17E0                   ; ------
                
 17E0 0A01      ZEQUAL: DEC     ARGCNT
 17E2 2605              BNE     DOEQ
                
 17E4                   ; *** ERROR #9: NOT ENOUGH "EQUAL?" ARGS ***
                
 17E4 8609              LDA     #9
 17E6 BD1FA9            JSR     ZERROR
                
 17E9 DC02      DOEQ:   LDD     ARG1
 17EB 109304            CMPD    ARG2
 17EE 2715              BEQ     EQOK
 17F0 0A01              DEC     ARGCNT
 17F2 270E              BEQ     EQBAD
                
 17F4 109306            CMPD    ARG3
 17F7 270C              BEQ     EQOK
 17F9 0A01              DEC     ARGCNT
 17FB 2705              BEQ     EQBAD
                
 17FD 109308            CMPD    ARG4
 1800 2703              BEQ     EQOK
 1802 7E131E    EQBAD:  JMP     PREDF
                
 1805 7E132C    EQOK:   JMP     PREDS
                
 1808                   ; ----
 1808                   ; CALL
 1808                   ; ----
                
 1808                   ; Branch to function pointed to by [arg1 * 2], passing
 1808                   ; the optional parameters "arg2" thru "arg4" [VALUE]
                
 1808 DC02      ZCALL:  LDD     ARG1            ; DID FUNCTION = 0?
 180A 2603              BNE     DOCALL          ; NO, CONTINUE
 180C 7E1744            JMP     MATH            ; ELSE RETURN A ZERO
                
 180F DC25      DOCALL: LDD     OZSTAK          ; ZSP FROM PREVIOUS ZCALL
 1811 BD1301            JSR     PSHDZ
 1814 D613              LDB     ZPCL            ; LOW 8 BITS OF ZPC
 1816 BD1301            JSR     PSHDZ           ; SAVE TO Z-STACK
 1819 DC11              LDD     ZPCH            ; PUSH H & M PC
 181B BD1301            JSR     PSHDZ
                
 181E                   ; MULTIPLY ARG1 BY 2; FORM 17-BIT ADDR
                
 181E 4F                CLRA


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 39                 
--- X-OPS ---                                                                                                                       

 181F 0803              ASL     ARG1+1          ; BOTTOM 8 BITS
 1821 0902              ROL     ARG1            ; MIDDLE 8
 1823 49                ROLA                    ; TOP BIT
 1824 9711              STA     ZPCH
 1826 DC02              LDD     ARG1
 1828 DD12              STD     ZPCM
 182A 0F16              CLR     ZPCFLG          ; [ZPC] HAS CHANGED ...
                
 182C BD1A55            JSR     NEXTPC          ; FETCH # NEW LOCALS
 182F 973F              STA     TEMP2           ; SAVE IT HERE FOR INDEXING
 1831 9740              STA     TEMP2+1         ; AND HERE FOR REFERENCE
 1833 271E              BEQ     ZCALL2          ; NO LOCALS IN THIS FUNCTION
                
 1835                   ; SAVE OLD LOCALS, REPLACE WITH NEW
                
 1835 8E1000            LDX     #LOCALS         ; INIT POINTER
 1838 EC84      ZCALL1: LDD     ,X              ; GRAB AN OLD LOCAL
 183A 3410              PSHS    X               ; SAVE THE POINTER
 183C BD1301            JSR     PSHDZ           ; PUSH OLD LOCAL TO Z-STACK
 183F BD1A55            JSR     NEXTPC          ; GET MSB OF NEW LOCAL
 1842 3402              PSHS    A               ; SAVE HERE
 1844 BD1A55            JSR     NEXTPC          ; NOW GET LSB
 1847 1F89              TFR     A,B             ; POSITION IT PROPERLY
 1849 3502              PULS    A               ; RETRIEVE MSB
 184B 3510              PULS    X               ; THIS IS WHERE IT GOES
 184D ED81              STD     ,X++            ; STORE NEW LOCAL, UPDATE POINTER
 184F 0A3F              DEC     TEMP2           ; ANY MORE OLD LOCALS?
 1851 26E5              BNE     ZCALL1          ; KEEP LOOPING TILL DONE
                
 1853 0A01      ZCALL2: DEC     ARGCNT          ; EXTRA ARGUMENTS IN THIS CALL?
 1855 2717              BEQ     ZCALL4          ; NO ARGS TO PASS
                
 1857                   ; MOVE UP TO 3 ARGS TO LOCAL STORAGE
                
 1857 DC04      ZCALL3: LDD     ARG2
 1859 FD1000            STD     LOCALS
 185C 0A01              DEC     ARGCNT
 185E 270E              BEQ     ZCALL4
 1860 DC06              LDD     ARG3
 1862 FD1002            STD     LOCALS+2
 1865 0A01              DEC     ARGCNT
 1867 2705              BEQ     ZCALL4
 1869 DC08              LDD     ARG4
 186B FD1004            STD     LOCALS+4
                
 186E D640      ZCALL4: LDB     TEMP2+1         ; REMEMBER # LOCALS SAVED
 1870 BD1301            JSR     PSHDZ           ; AND RETURN
 1873 DF25              STU     OZSTAK          ; "THE WAY WE WERE ..."
 1875 39                RTS
                
 1876                   ; ---


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 40                 
--- X-OPS ---                                                                                                                       

 1876                   ; PUT
 1876                   ; ---
                
 1876                   ; Set item "arg2" in WORD-table "arg1" equal to "arg3"
                
 1876 0805      ZPUT:   ASL     ARG2+1          ; WORD-ALIGN
 1878 0904              ROL     ARG2            ; ARG2
 187A DC04              LDD     ARG2
 187C D302              ADDD    ARG1            ; ADD Z-ADDR OF TABLE
 187E C32500            ADDD    #ZCODE          ; FORM ABSOLUTE ADDRESS
 1881 1F01              TFR     D,X             ; FOR USE AS AN INDEX
 1883 DC06              LDD     ARG3
 1885 ED84              STD     ,X
 1887 39                RTS
                
 1888                   ; ----
 1888                   ; PUTB
 1888                   ; ----
                
 1888                   ; Set item "arg2" in BYTE-table "arg1" equal to "arg3"
                
 1888 DC04      ZPUTB:  LDD     ARG2
 188A D302              ADDD    ARG1
 188C C32500            ADDD    #ZCODE
 188F 1F01              TFR     D,X
 1891 9607              LDA     ARG3+1
 1893 A784              STA     ,X
 1895 39                RTS
                
 1896                   ; ----
 1896                   ; PUTP
 1896                   ; ----
                
 1896                   ; Set property "arg2" in object "arg1" equal to "arg3"
                
 1896 BD1D06    ZPUTP:  JSR     PROPB
 1899 BD1D1B    PUTP1:  JSR     PROPN
 189C 9105              CMPA    ARG2+1
 189E 270C              BEQ     PUTP2
 18A0 2405              BHS     PTP
                
 18A2                   ; *** ERROR #10: BAD PROPERTY NUMBER ***
                
 18A2 860A              LDA     #10
 18A4 BD1FA9            JSR     ZERROR          ; ERROR #7 (BAD PROPERTY #)
                
 18A7 BD1D30    PTP:    JSR     PROPNX          ; NEXT ITEM
 18AA 20ED              BRA     PUTP1
                
 18AC BD1D23    PUTP2:  JSR     PROPL
 18AF 5C                INCB


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 41                 
--- X-OPS ---                                                                                                                       

 18B0 4D                TSTA
 18B1 2711              BEQ     PUTP2A
 18B3 8101              CMPA    #1
 18B5 2705              BEQ     PTP1
                
 18B7                   ; *** ERROR #11: PROPERTY LENGTH ***
                
 18B7 860B              LDA     #11
 18B9 BD1FA9            JSR     ZERROR          ; ERROR #8 (PROP TOO LONG)
                
 18BC 9E3D      PTP1:   LDX     TEMP
 18BE 3A                ABX
 18BF DC06              LDD     ARG3
 18C1 ED84              STD     ,X
 18C3 39                RTS
                
 18C4 9607      PUTP2A: LDA     ARG3+1
 18C6 9E3D              LDX     TEMP
 18C8 3A                ABX
 18C9 A784              STA     ,X
 18CB 39                RTS
                
 18CC                   ; ------
 18CC                   ; PRINTC
 18CC                   ; ------
                
 18CC                   ; Print the character with ASCII value "arg1"
                
 18CC 9603      ZPRC:   LDA     ARG1+1
 18CE 7E1FF3            JMP     COUT
                
 18D1                   ; ------
 18D1                   ; PRINTN
 18D1                   ; ------
                
 18D1                   ; Print "arg1" as a signed integer
                
 18D1 DC02      ZPRN:   LDD     ARG1
 18D3 DD3D              STD     TEMP
                
 18D5                   ; PRINT THE SIGNED VALUE IN [TEMP]
                
 18D5 DC3D      NUMBER: LDD     TEMP
 18D7 2A08              BPL     DIGCNT          ; IF NUMBER IS NEGATIVE,
 18D9 862D              LDA     #$2D            ; START WITH A MINUS SIGN
 18DB BD1FF3            JSR     COUT
 18DE BD17A8            JSR     ABTEMP          ; GET ABS(TEMP)
                
 18E1                   ; COUNT # OF DECIMAL DIGITS
                
 18E1 0F41      DIGCNT: CLR     MASK            ; RESET INDEX


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 42                 
--- X-OPS ---                                                                                                                       

 18E3 DC3D      DGC:    LDD     TEMP            ; CHECK QUOTIENT
 18E5 2710              BEQ     PRNTN3          ; SKIP IF ZERO
 18E7 CC000A            LDD     #10
 18EA DD3B              STD     VAL             ; ELSE DIVIDE BY 10
 18EC BD17AF            JSR     UDIV            ; UNSIGNED DIVIDE
 18EF 963C              LDA     VAL+1           ; GET LSB OF REMAINDER
 18F1 3402              PSHS    A               ; SAVE ON STACK
 18F3 0C41              INC     MASK            ; INCREMENT CHAR COUNT
 18F5 20EC              BRA     DGC             ; LOOP TILL ARG1=0
                
 18F7 9641      PRNTN3: LDA     MASK
 18F9 270C              BEQ     PZERO           ; PRINT AT LEAST A "0"
 18FB 3502      PRNTN4: PULS    A               ; GET A CHAR
 18FD 8B30              ADDA    #$30            ; CONVERT TO ASCII NUMBER
 18FF BD1FF3            JSR     COUT
 1902 0A41              DEC     MASK            ; OUT OF CHARS?
 1904 26F5              BNE     PRNTN4          ; KEEP PRINTING TILL
 1906 39                RTS                     ; DONE
                
 1907                   ; PRINT A ZERO
                
 1907 8630      PZERO:  LDA     #$30            ; ASCII "0"
 1909 7E1FF3            JMP     COUT
                
 190C                   ; ------
 190C                   ; RANDOM
 190C                   ; ------
                
 190C                   ; Return a random value between zero and "arg1" [VALUE]
                
 190C DC02      ZRAND:  LDD     ARG1            ; USE [ARG1]
 190E DD3B              STD     VAL             ; AS THE DIVISOR
                
 1910 DC56              LDD     RAND1           ; GET A RANDOM #
 1912 C3330B            ADDD    #$330B
 1915 DD56              STD     RAND1           ; A NEW SEED
 1917 847F              ANDA    #%01111111      ; MAKE POSITIVE
 1919 DD3D              STD     TEMP            ; MAKE IT THE DIVIDEND
                
 191B BD1781            JSR     DIVIDE          ; UNSIGNED DIVIDE!
 191E DC3B              LDD     VAL             ; GET REMAINDER
 1920 C30001            ADDD    #1              ; AT LEAST 1
 1923 7E1744            JMP     MATH
                
 1926                   ; ----
 1926                   ; PUSH
 1926                   ; ----
                
 1926                   ; Push "arg1" onto the Z-stack
                
 1926 DC02      ZPUSH:  LDD     ARG1


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 43                 
--- X-OPS ---                                                                                                                       

 1928 7E1301            JMP     PSHDZ
                
 192B                   ; ---
 192B                   ; POP
 192B                   ; ---
                
 192B                   ; Pop a word off Z-stack and store in variable "arg1"
                
 192B BD130A    ZPOP:   JSR     POPSTK
 192E 9603              LDA     ARG1+1          ; GET VARIABLE ID
 1930 7E12C5            JMP     VARPUT
                
 1933                   ; -----
 1933                   ; SPLIT
 1933                   ; -----
                
 143F           ZSPLIT  EQU     ZNOOP
                
 1933                   ; ------
 1933                   ; SCREEN
 1933                   ; ------
                
 143F           ZSCRN   EQU     ZNOOP
                
                        END
                        INCLUD READ.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 44                 
--- READ HANDLER ---                                                                                                                

                
 1933 BD207B    ZREAD:  JSR     ZUSL            ; UPDATE STATUS LINE FIRST
                
 1936 DC02              LDD     ARG1            ; CALC ABSOLUTE ADDRESS
 1938 C32500            ADDD    #ZCODE          ; OF READ BUFFERS
 193B DD02              STD     ARG1
 193D DC04              LDD     ARG2
 193F C32500            ADDD    #ZCODE
 1942 DD04              STD     ARG2
                
 1944 BD1E39            JSR     INPUT           ; READ LINE; RETURN LENGTH IN A
 1947 9741              STA     MASK            ; # CHARS IN LINE
 1949 0F42              CLR     MASK+1          ; # CHARS IN CURRENT WORD
                
 194B 9E04              LDX     ARG2            ; SET # OF WORDS READ
 194D 6F01              CLR     1,X             ; TO ZERO
                
 194F 8601              LDA     #1              ; = 1
 1951 9739              STA     STABP           ; INIT SOURCE TABLE POINTER
 1953 4C                INCA                    ; = 2
 1954 9738              STA     RTABP           ; AND RESULT TABLE POINTER
                
 1956 9E04      READL:  LDX     ARG2
 1958 A680              LDA     ,X+             ; FETCH MAXIMUM # OF WORDS
 195A A184              CMPA    ,X              ; COMPARE TO # WORDS READ
 195C 2405              BHS     RL1             ; STILL ROOM
                
 195E                   ; *** ERROR #13 -- PARSER OVERFLOW ***
                
 195E 860D              LDA     #13
 1960 BD1FA9            JSR     ZERROR
                
 1963 DC41      RL1:    LDD     MASK            ; OUT OF CHARS & WORDS?
 1965 2601              BNE     RL2             ; NOT YET
 1967 39        RDEX:   RTS                     ; ELSE SCRAM
                
 1968 9642      RL2:    LDA     MASK+1          ; GET CHAR COUNT
 196A 8106              CMPA    #6              ; 6 CHARS DONE?
 196C 2503              BLO     RL3             ; NOT YET
 196E BD19EE            JSR     FLUSHW          ; ELSE FLUSH WORD
                
 1971 9642      RL3:    LDA     MASK+1          ; FIRST CHAR IN WORD?
 1973 2624              BNE     READL2          ; NOPE
                
 1975                   ; CLEAR OUT WORD BUFFER [ZSTBUI]
                
 1975 5F                CLRB                    ; [A] IS ALREADY ZERO
 1976 DD2C              STD     ZSTBUI
 1978 DD2E              STD     ZSTBUI+2
 197A DD30              STD     ZSTBUI+4
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 45                 
--- READ HANDLER ---                                                                                                                

 197C D638              LDB     RTABP
 197E 9E04              LDX     ARG2
 1980 3A                ABX
 1981 D639              LDB     STABP
 1983 E703              STB     3,X             ; STORE POSITION
                
 1985 9E02              LDX     ARG1
 1987 A685              LDA     B,X             ; GRAB A CHAR FROM SOURCE BUFFER
 1989 BD1A10            JSR     SIBRKP          ; IS IT A SIB?
 198C 2529              BCS     RSIBRK          ; YES IF CARRY IS SET
 198E BD1A09            JSR     NBRKP           ; IS IT A "NORMAL" BREAK CHAR?
 1991 2406              BCC     READL2          ; NO, KEEP SCANNING
 1993 0C39              INC     STABP           ; ELSE FLUSH STRANDED BREAK
 1995 0A41              DEC     MASK            ; UPDATE # OF CHARS IN LINE
 1997 20BD              BRA     READL           ; AND LOOP BACK
                
 1999 9641      READL2: LDA     MASK            ; OUT OF CHARS?
 199B 2722              BEQ     READL3          ; SURE ENOUGH
 199D D639              LDB     STABP
 199F 9E02              LDX     ARG1
 19A1 A685              LDA     B,X             ; ELSE GRAB NEXT CHAR
 19A3 BD1A05            JSR     RBRKP           ; IS IT A BREAK?
 19A6 2517              BCS     READL3          ; YES IF CARRY SET
 19A8 D642              LDB     MASK+1          ; ELSE POINT TO
 19AA 8E002C            LDX     #ZSTBUI         ; WORD BUFFER
 19AD A785              STA     B,X             ; STORE CHAR IN BUFFER
 19AF 0A41              DEC     MASK            ; ONE LESS CHAR IN LINE
 19B1 0C42              INC     MASK+1          ; ONE MORE IN RESULT
 19B3 0C39              INC     STABP           ; POINT TO NEXT CHAR
 19B5 209F              BRA     READL           ; AND LOOP BACK
                
 19B7 972C      RSIBRK: STA     ZSTBUI          ; STORE THE SIB
 19B9 0A41              DEC     MASK            ; UPDATE LINE-CHAR COUNT
 19BB 0C42              INC     MASK+1          ; WORD-CHAR COUNT
 19BD 0C39              INC     STABP           ; AND # CHARS IN WORD
                
 19BF 9642      READL3: LDA     MASK+1          ; ANY CHARS IN WORD?
 19C1 2793              BEQ     READL           ; APPARENTLY NOT
                
 19C3 D638              LDB     RTABP           ; POINT TO
 19C5 9E04              LDX     ARG2            ; IN THIS ENTRY
 19C7 3A                ABX
 19C8 9642              LDA     MASK+1          ; FETCH ACTUAL WORD LENGTH
 19CA A702              STA     2,X             ; AND STORE IN 3RD BYTE
                
 19CC 9641              LDA     MASK
 19CE 3402              PSHS    A               ; SAVE THIS
 19D0 BD1C11            JSR     CONZST          ; CONVERT TO Z-STRING
 19D3 BD1A20            JSR     FINDW           ; LOOK UP IN VOCABULARY
 19D6 3502              PULS    A
 19D8 9741              STA     MASK            ; RESTORE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 46                 
--- READ HANDLER ---                                                                                                                

                
 19DA 9E04              LDX     ARG2
 19DC 6C01              INC     1,X             ; UPDATE # WORDS READ
 19DE D638              LDB     RTABP           ; POINT [X] TO 1ST BYTE
 19E0 3A                ABX                     ; IN CURRENT ENTRY
 19E1 CB04              ADDB    #4
 19E3 D738              STB     RTABP           ; POINT TO NEXT ENTRY
 19E5 DC3B              LDD     VAL             ; STORE [VAL] IN ENTRY
 19E7 ED84              STD     ,X
 19E9 0F42              CLR     MASK+1          ; RESET WORD-CHAR COUNT
 19EB 7E1956            JMP     READL           ; AND CONTINUE
                
 19EE                   ; ----------
 19EE                   ; FLUSH WORD
 19EE                   ; ----------
                
 19EE 9641      FLUSHW: LDA     MASK
 19F0 2712              BEQ     FLEX
 19F2 D639              LDB     STABP
 19F4 9E02              LDX     ARG1
 19F6 A685              LDA     B,X
 19F8 8D0B              BSR     RBRKP           ; WORD BREAK?
 19FA 2508              BCS     FLEX            ; EXIT IF SO
 19FC 0A41              DEC     MASK
 19FE 0C42              INC     MASK+1
 1A00 0C39              INC     STABP
 1A02 20EA              BRA     FLUSHW          ; KEEP LOOPING
 1A04 39        FLEX:   RTS
                
 1A05                   ; ---------------
 1A05                   ; BREAK CHAR SCAN
 1A05                   ; ---------------
                
 1A05 8D09      RBRKP:  BSR     SIBRKP          ; FIRST CHECK FOR SIBS
 1A07 2515              BCS     FBRK            ; EXIT IF MATCHED
                
 1A09                   ; FALL THROUGH TO ...
                
 1A09                   ; ----------------------
 1A09                   ; NORMAL BREAK CHAR SCAN
 1A09                   ; ----------------------
                
 1A09 8E1A4F    NBRKP:  LDX     #BRKTBL         ; BASE OF BREAK CHAR TABLE
 1A0C C605              LDB     #NBRKS-1        ; NUMBER OF NORMAL BREAK CHARS
 1A0E 2005              BRA     NBR1
                
 1A10                   ; ------------------------------
 1A10                   ; SELF-INSERTING BREAK CHAR SCAN
 1A10                   ; ------------------------------
                
 1A10 9E21      SIBRKP: LDX     VOCAB           ; BASE ADDRESS OF VOCAB TABLE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 47                 
--- READ HANDLER ---                                                                                                                

 1A12 E680              LDB     ,X+             ; GET # SIB CHARS
 1A14 5A                DECB                    ; ZERO-ALIGN COUNT
                
 1A15 A185      NBR1:   CMPA    B,X
 1A17 2705              BEQ     FBRK            ; MATCHED!
 1A19 5A                DECB
 1A1A 2AF9              BPL     NBR1            ; KEEP LOOPING
 1A1C 5F                CLRB                    ; NO MATCH, CLEAR CARRY
 1A1D 39                RTS
 1A1E 53        FBRK:   COMB                    ; SET CARRY TO FLAG MATCH
 1A1F 39                RTS
                
 1A20                   ; -----------------
 1A20                   ; VOCABULARY SEARCH
 1A20                   ; -----------------
                
 1A20 9E21      FINDW:  LDX     VOCAB           ; BASE ADDR OF VOCAB TABLE
 1A22 E680              LDB     ,X+             ; GET # SIB BYTES
 1A24 3A                ABX                     ; AND SKIP OVER THEM
                
 1A25 A680              LDA     ,X+             ; # BYTES PER TABLE ENTRY
 1A27 9742              STA     MASK+1          ; SAVE IT HERE
                
 1A29 EC81              LDD     ,X++            ; # OF ENTRIES IN TABLE
 1A2B DD3B              STD     VAL             ; SAVE THAT TOO
                
 1A2D EC84      FWL1:   LDD     ,X              ; CHECK FIRST Z-WORD
 1A2F 109332            CMPD    ZSTBUO
 1A32 2607              BNE     WNEXT           ; NO GOOD
 1A34 EC02              LDD     2,X             ; ELSE CHECK 2ND HALF
 1A36 109334            CMPD    ZSTBUO+2
 1A39 270D              BEQ     FWSUCC          ; MATCHED!
                
 1A3B D642      WNEXT:  LDB     MASK+1          ; MOVE [X] UP TO
 1A3D 3A                ABX                     ; NEXT TABLE ENTRY
 1A3E DC3B              LDD     VAL
 1A40 830001            SUBD    #1
 1A43 DD3B              STD     VAL             ; OUT OF ENTRIES YET?
 1A45 26E6              BNE     FWL1            ; NO, KEEP LOOKING
 1A47 39                RTS                     ; ELSE RETURN WITH [VAL]=0
                
 1A48 3089DB00  FWSUCC: LEAX    -ZCODE,X        ; CONVERT TO Z-ADDRESS
 1A4C 9F3B              STX     VAL             ; LEAVE RESULT IN [VAL]
 1A4E 39                RTS
                
 1A4F                   ; ------------------
 1A4F                   ; NORMAL BREAK CHARS
 1A4F                   ; ------------------
                
 1A4F 213F2C2E  BRKTBL: DB      "!?,."
 1A53 0D                DB      EOL


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE EXECUTORS                                                                 PAGE 48                 
--- READ HANDLER ---                                                                                                                

 1A54 20                DB      SPACE
                
 0006           NBRKS   EQU     6               ; # NORMAL BREAK CHARS
                
                        END
                
                        INCLUD PAGING.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 49                 
--- PAGING ROUTINES ---                                                                                                             

                
 1A55                   ; -------------------------
 1A55                   ; FETCH NEXT BYTE OF Z-CODE
 1A55                   ; -------------------------
                
 1A55 0D16      NEXTPC: TST     ZPCFLG          ; IF PAGE IS INVALID,
 1A57 2714              BEQ     NEWZ            ; SWITCH PAGES
                
 1A59 D613      NPC1:   LDB     ZPCL            ; GET BYTE INDEX
 1A5B 9E14              LDX     ZPCPNT          ; AND PAGE POINTER
 1A5D 3A                ABX                     ; ADD TO GET BYTE ADDRESS
 1A5E A684              LDA     ,X              ; AND FETCH THE BYTE INTO [A]
                
 1A60                   IF      DEBUG
 1A60                   PSHS    A
 1A60                   LDB     #'2'
 1A60                   JSR     DOBUG
 1A60                   PULS    A
 1A60                   ENDIF
                
 1A60 0C13              INC     ZPCL            ; NEXT BYTE IN PAGE
 1A62 2608              BNE     NXTX            ; IF END OF PAGE,
 1A64 0F16              CLR     ZPCFLG          ; INVALIDATE THE PAGE
                
 1A66 0C12              INC     ZPCM            ; POINT TO NEXT Z-PAGE
 1A68 2602              BNE     NXTX
 1A6A 0C11              INC     ZPCH
                
 1A6C 39        NXTX:   RTS
                
 1A6D                   ; ----------------------
 1A6D                   ; SWITCH TO A NEW Z-PAGE
 1A6D                   ; ----------------------
                
 1A6D DC11      NEWZ:   LDD     ZPCH            ; GET UPPER 9 BITS OF [ZPC]
 1A6F 4D                TSTA                    ; UPPER 64K?
 1A70 2604              BNE     PAGEZ           ; YES, MUST BE IN SWAPPING SPACE
                
 1A72 D10B              CMPB    ZPURE           ; IS THIS PAGE IN THE PRELOAD?
 1A74 2507              BLO     INZPRE          ; YES, LOOK FOR IT THERE
                
 1A76                   ; LOOK FOR PAGE [D] IN THE SWAPPING SPACE
                
 1A76 BD1AD7    PAGEZ:  JSR     FINDPG          ; GET ABS PAGE ADDR INTO [A]
 1A79 0F1D              CLR     MPCFLG          ; INVALIDATE [MPC] FOR SAFETY
 1A7B 2004              BRA     NEWZPG
                
 1A7D                   ; FETCH PAGE [D] FROM THE PRELOAD
                
 1A7D CB25      INZPRE: ADDB    #HIGH ZCODE     ; MAKE IT ABSOLUTE
 1A7F 1F98              TFR     B,A             ; EXPECTED HERE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 50                 
--- PAGING ROUTINES ---                                                                                                             

                
 1A81                   ; MAKE [ZPCPNT] POINT TO NEW Z-PAGE
                
 1A81 9714      NEWZPG: STA     ZPCPNT
 1A83 0F15              CLR     ZPCPNT+1        ; CLEAR LSB
 1A85 86FF              LDA     #TRUE           ; VALIDATE THE ZPC
 1A87 9716              STA     ZPCFLG
 1A89 20CE              BRA     NPC1            ; AND FETCH THE BYTE
                
 1A8B                   ; -------------------------
 1A8B                   ; GET A VIRTUAL MEMORY BYTE
 1A8B                   ; -------------------------
                
 1A8B 0D1D      GETBYT: TST     MPCFLG          ; IF PAGE IS INVALID,
 1A8D 2714              BEQ     NEWM            ; SWITCH PAGES
                
 1A8F D61A      GTBT:   LDB     MPCL            ; GET BYTE INDEX
 1A91 9E1B              LDX     MPCPNT          ; AND PAGE POINTER
 1A93 3A                ABX                     ; ADD TO GET BYTE ADDRESS
 1A94 A684              LDA     ,X              ; AND FETCH THE BYTE INTO [A]
                
 1A96                   IF      DEBUG
 1A96                   PSHS    A
 1A96                   LDB     #'3'
 1A96                   JSR     DOBUG
 1A96                   PULS    A
 1A96                   ENDIF
                
 1A96 0C1A              INC     MPCL            ; NEXT BYTE IN PAGE
 1A98 2608              BNE     NXTM            ; IF END OF PAGE,
 1A9A 0F1D              CLR     MPCFLG          ; INVALIDATE THE PAGE
                
 1A9C 0C19              INC     MPCM            ; POINT TO NEXT Z-PAGE
 1A9E 2602              BNE     NXTM
 1AA0 0C18              INC     MPCH
                
 1AA2 39        NXTM:   RTS
                
 1AA3                   ; ----------------------------
 1AA3                   ; SWITCH TO A NEW VIRTUAL PAGE
 1AA3                   ; ----------------------------
                
 1AA3 DC18      NEWM:   LDD     MPCH            ; GET UPPER 9 BITS OF [MPC]
 1AA5 4D                TSTA                    ; UPPER 64K?
 1AA6 2604              BNE     PAGEM           ; YES, LOOK IN SWAPPING SPACE
                
 1AA9           PATCH   EQU     $+1             ; PATCH POINT FOR "VERIFY"
                
 1AA8 D10B              CMPB    ZPURE           ; IS THIS PAGE IN THE PRELOAD?
 1AAA 2507              BLO     INMPRE          ; YES, LOOK FOR IT THERE
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 51                 
--- PAGING ROUTINES ---                                                                                                             

 1AAC                   ; LOOK FOR PAGE [D] IN THE SWAPPING SPACE
                
 1AAC BD1AD7    PAGEM:  JSR     FINDPG          ; GET ABS PAGE ADDR INTO [A]
 1AAF 0F16              CLR     ZPCFLG          ; INVALIDATE [ZPC] JUST IN CASE
 1AB1 2004              BRA     NEWMPG
                
 1AB3                   ; FETCH PAGE [D] FROM THE PRELOAD
                
 1AB3 CB25      INMPRE: ADDB    #HIGH ZCODE     ; MAKE IT ABSOLUTE
 1AB5 1F98              TFR     B,A             ; EXPECTED HERE
                
 1AB7                   ; MAKE [MPCPNT] POINT TO NEW Z-PAGE
                
 1AB7 971B      NEWMPG: STA     MPCPNT
 1AB9 0F1C              CLR     MPCPNT+1        ; CLEAR LSB
 1ABB 86FF              LDA     #TRUE           ; VALIDATE THE MPC
 1ABD 971D              STA     MPCFLG
 1ABF 20CE              BRA     GTBT            ; AND FETCH THE BYTE
                
 1AC1                   ; ---------------------------
 1AC1                   ; FETCH A VIRTUAL MEMORY WORD
 1AC1                   ; ---------------------------
                
 1AC1 8DC8      GETWRD: BSR     GETBYT          ; GET MSB
 1AC3 3402              PSHS    A               ; SAVE ON STACK
 1AC5 8DC4              BSR     GETBYT          ; NOW GET LSB
 1AC7 973E              STA     TEMP+1          ; GIVE IT TO [TEMP]
 1AC9 3502              PULS    A               ; RETRIEVE MSB
 1ACB 973D              STA     TEMP            ; GIVE THAT TO [TEMP] TOO
 1ACD 39                RTS
                
 1ACE                   ; ----------------------------------
 1ACE                   ; POINT [MPC] TO VIRTUAL BYTE [TEMP]
 1ACE                   ; ----------------------------------
                
 1ACE DC3D      SETWRD: LDD     TEMP
 1AD0 DD19              STD     MPCM
 1AD2 0F18              CLR     MPCH            ; CLEAR TOP BIT
 1AD4 0F1D              CLR     MPCFLG          ; INVALIDATE MPC
 1AD6 39                RTS
                
 1AD7                   ; -----------------------
 1AD7                   ; LOCATE VIRTUAL PAGE [D]
 1AD7                   ; -----------------------
                
 1AD7                   ; ENTRY: Z-PAGE # IN [D]
 1AD7                   ; EXIT: ABSOLUTE PAGE ADDRESS IN [A]
                
 1AD7 0F0D      FINDPG: CLR     ZPAGE           ; INIT PAGE INDEX
 1AD9 8E0E00            LDX     #PTABLE         ; START SEARCH AT BOTTOM OF TABLE
 1ADC 10A381    FP1:    CMPD    ,X++            ; FOUND IT?


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 52                 
--- PAGING ROUTINES ---                                                                                                             

 1ADF 272A              BEQ     PFOUND          ; YES!
 1AE1 0C0D              INC     ZPAGE           ; NEXT PAGE
 1AE3 9C0F              CMPX    TABTOP          ; TOP OF TABLE YET?
 1AE5 25F5              BLO     FP1             ; NO, KEEP SEARCHING
                
 1AE7                   ; GRAB MISSING PAGE FROM DISK
                
 1AE7 DD4A              STD     DBLOCK          ; TELL DISK WHICH PAGE TO GRAB
                
 1AE9 960A              LDA     LRU             ; GET LEAST RECENTLY USED PAGE #
 1AEB 970D              STA     ZPAGE           ; MAKE IT THE NEW PAGE
 1AED 9B0E              ADDA    PAGE0           ; CALC ABSOLUTE PAGE ADDRESS
 1AEF 9748              STA     DBUFF           ; TELL DISK WHERE TO PUT DATA
 1AF1 0F49              CLR     DBUFF+1         ; CLEAR MSB
                
 1AF3 8E0E00            LDX     #PTABLE         ; GET ADDRESS OF PAGING TABLE
 1AF6 D60A              LDB     LRU             ; AND TABLE INDEX
 1AF8 3A                ABX                     ; ADD LRU INDEX TWICE TO GET
 1AF9 3A                ABX                     ; WORD-ALIGNED TABLE POSITION
 1AFA DC4A              LDD     DBLOCK          ; SPLICE THE NEW Z-PAGE
 1AFC ED84              STD     ,X              ; INTO THE TABLE
                
 1AFE BD2167            JSR     GETDSK          ; GET THE NEW PAGE INTO RAM
                
 1B01 0C0A              INC     LRU             ; INCREMENT BUFFER POINTER
 1B03 960A              LDA     LRU             ; AND TEST IT
 1B05 910C              CMPA    PMAX            ; OUT OF PAGING SPACE?
 1B07 2502              BLO     PFOUND          ; NO, CONTINUE
 1B09 0F0A              CLR     LRU             ; ELSE RESET INDEX
                
 1B0B 960D      PFOUND: LDA     ZPAGE           ; GET THE PAGE INDEX
 1B0D 9B0E              ADDA    PAGE0           ; MAKE IT ABSOLUTE
 1B0F 39                RTS                     ; AND RETURN IT
                
                        END
                        INCLUD ZSTRING.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 53                 
--- Z-STRING HANDLERS ---                                                                                                           

                
 1B10                   ; -----------------
 1B10                   ; POINT TO Z-STRING
 1B10                   ; -----------------
                
 1B10 4F        SETSTR: CLRA
 1B11 083E              ASL     TEMP+1
 1B13 093D              ROL     TEMP
 1B15 49                ROLA
 1B16 9718              STA     MPCH
 1B18 DC3D              LDD     TEMP
 1B1A DD19              STD     MPCM
 1B1C 0F1D              CLR     MPCFLG
 1B1E 39        ZSTEX:  RTS
                
 1B1F                   ; --------------
 1B1F                   ; PRINT Z-STRING
 1B1F                   ; --------------
                
 1B1F 0F28      PZSTR:  CLR     CSPERM          ; PERMANENT CHARSET
 1B21 0F29              CLR     STBYTF          ; RESET STRING BYTE FLAG
 1B23 86FF              LDA     #$FF
 1B25 9727              STA     CSTEMP          ; NO TEMP CHARSET ACTIVE
                
 1B27 BD1BD9    PZSTRL: JSR     GETZCH          ; GET A Z-CHARACTER
 1B2A 25F2              BCS     ZSTEX           ; END OF STRING IF CARRY SET
 1B2C 9741              STA     MASK            ; SAVE CHAR HERE
 1B2E 2741              BEQ     PZSTRS          ; O = SPACE CHAR
 1B30 8104              CMPA    #4              ; IS THIS AN F-WORD?
 1B32 2557              BLO     PZSTRF          ; APPARENTLY SO
 1B34 8106              CMPA    #6              ; SHIFT CHAR?
 1B36 253D              BLO     PZSTRT          ; YES, CHANGE CHARSET
                
 1B38 BD1BCD            JSR     GETMOD
 1B3B 4D                TSTA                    ; IS THIS CHARSET 0?
 1B3C 2609              BNE     PZSTR1          ; NOPE
                
 1B3E                   ; PRINT LOWER-CASE CHAR (CHARSET 0)
                
 1B3E 865B              LDA     #$61-6          ; ASCII "a" MINUS Z-OFFSET
 1B40 9B41      PZSTP0: ADDA    MASK            ; ADD CHARACTER
 1B42 BD1FF3    PZSTP1: JSR     COUT            ; PRINT RESULT
 1B45 20E0              BRA     PZSTRL          ; AND FETCH ANOTHER Z-CHAR
                
 1B47                   ; CHARSET 1 OR 2?
                
 1B47 8101      PZSTR1: CMPA    #1              ; SET 1?
 1B49 2604              BNE     PZSTR2          ; NOPE, IT'S SET 2
                
 1B4B                   ; PRINT UPPER-CASE CHAR (CHARSET 1)
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 54                 
--- Z-STRING HANDLERS ---                                                                                                           

 1B4B 863B              LDA     #$41-6          ; ASCII "A" MINUS Z-OFFSET
 1B4D 20F1              BRA     PZSTP0          ; AND SO ON ...
                
 1B4F                   ; DECODE/PRINT CHARSET 2
                
 1B4F D641      PZSTR2: LDB     MASK            ; RETRIEVE Z-CHAR
 1B51 C006              SUBB    #6              ; CONVERT TO ZERO-ALIGNED INDEX
 1B53 2707              BEQ     PZSTRA          ; IF ZERO, IT'S "DIRECT" ASCII
 1B55 8E1CEC            LDX     #CHRTBL         ; ELSE GET BASE OF DECODE TABLE
 1B58 A685              LDA     B,X             ; GET CHAR FROM TABLE
 1B5A 20E6              BRA     PZSTP1          ; AND PRINT IT!
                
 1B5C                   ; DECODE/PRINT A "DIRECT" ASCII CHAR
                
 1B5C BD1BD9    PZSTRA: JSR     GETZCH          ; GET NEXT Z-BYTE
 1B5F 48                ASLA                    ; SHIFT INTO POSITION
 1B60 48                ASLA
 1B61 48                ASLA
 1B62 48                ASLA
 1B63 48                ASLA
 1B64 9741              STA     MASK            ; SAVE MSB
 1B66 BD1BD9            JSR     GETZCH          ; FETCH LSB
 1B69 9742              STA     MASK+1          ; SAVE THAT, TOO
 1B6B 9641              LDA     MASK            ; GET MSB
 1B6D 9A42              ORA     MASK+1          ; SUPERIMPOSE LSB
 1B6F 20D1              BRA     PZSTP1          ; AND PRINT RESULT
                
 1B71                   ; PRINT A SPACE
                
 1B71 8620      PZSTRS: LDA     #$20
 1B73 20CD              BRA     PZSTP1
                
 1B75                   ; CHANGE CHARACTER SETS
                
 1B75 8003      PZSTRT: SUBA    #3              ; CONVERT TO 1 OR 2
 1B77 1F89              TFR     A,B
 1B79 8D52              BSR     GETMOD
 1B7B 2604              BNE     PZSTRP          ; NO, DO PERMANENT SHIFT
 1B7D D727              STB     CSTEMP          ; JUST A TEMP-SHIFT
 1B7F 20A6              BRA     PZSTRL
                
 1B81 D728      PZSTRP: STB     CSPERM          ; PERMANENT SHIFT
 1B83 9128              CMPA    CSPERM          ; NEW SET SAME AS OLD?
 1B85 27A0              BEQ     PZSTRL          ; YES, EXIT
 1B87 0F28              CLR     CSPERM          ; ELSE BACK TO SET 0
 1B89 209C              BRA     PZSTRL          ; BEFORE FINISHING
                
 1B8B                   ; HANDLE AN F-WORD
                
 1B8B 4A        PZSTRF: DECA                    ; CONVERT TO 0-2
 1B8C C640              LDB     #64             ; TIMES 64


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 55                 
--- Z-STRING HANDLERS ---                                                                                                           

 1B8E 3D                MUL
 1B8F D73A              STB     PZSTFO          ; SAVE FOR LATER
 1B91 BD1BD9            JSR     GETZCH          ; GET F-WORD INDEX
 1B94 1F89              TFR     A,B             ; MOVE IT
 1B96 58                ASLB                    ; FORM WORD-ALIGNED INDEX
 1B97 DB3A              ADDB    PZSTFO          ; ADD OFFSET
 1B99 9E23              LDX     FWORDS          ; GET BASE ADDR OF FWORDS TABLE
 1B9B 3A                ABX                     ; ADD THE OFFSET
 1B9C EC84              LDD     ,X              ; GET THE FWORD POINTER
 1B9E DD3D              STD     TEMP            ; AND SAVE IT
                
 1BA0                   ; SAVE THE STATE OF CURRENT Z-PRINT
                
 1BA0 9618              LDA     MPCH
 1BA2 3402              PSHS    A
 1BA4 9628              LDA     CSPERM
 1BA6 D629              LDB     STBYTF
 1BA8 9E19              LDX     MPCM
 1BAA 109E2A            LDY     ZSTWRD
 1BAD 3436              PSHS    Y,X,B,A
                
 1BAF BD1B10            JSR     SETSTR          ; PRINT THE F-WORD
 1BB2 BD1B1F            JSR     PZSTR           ; POINTED TO BY [TEMP]
                
 1BB5                   ; RESTORE THE OLD Z-STRING
                
 1BB5 3536              PULS    Y,X,B,A
 1BB7 109F2A            STY     ZSTWRD
 1BBA 9F19              STX     MPCM
 1BBC D729              STB     STBYTF
 1BBE 9728              STA     CSPERM
 1BC0 3502              PULS    A
 1BC2 9718              STA     MPCH
                
 1BC4 86FF              LDA     #$FF
 1BC6 9727              STA     CSTEMP          ; DISABLE TEMP CHARSET
 1BC8 0F1D              CLR     MPCFLG          ; MPC HAS CHANGED!
 1BCA 7E1B27            JMP     PZSTRL          ; CONTINUE INNOCENTLY
                
 1BCD                   ; ----------------------
 1BCD                   ; RETURN CURRENT CHARSET
 1BCD                   ; ----------------------
                
 1BCD 9627      GETMOD: LDA     CSTEMP
 1BCF 2A03              BPL     GM
 1BD1 9628              LDA     CSPERM
 1BD3 39                RTS
                
 1BD4 C6FF      GM:     LDB     #$FF
 1BD6 D727              STB     CSTEMP
 1BD8 39                RTS


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 56                 
--- Z-STRING HANDLERS ---                                                                                                           

                
 1BD9                   ; ---------------
 1BD9                   ; GET NEXT Z-CHAR
 1BD9                   ; ---------------
                
 1BD9 9629      GETZCH: LDA     STBYTF          ; WHICH BYTE?
 1BDB 2A02              BPL     GTZ0
 1BDD 53                COMB                    ; SET CARRY
 1BDE 39                RTS                     ; TO INDICATE "NO MORE CHARS"
                
 1BDF 260F      GTZ0:   BNE     GETZH1          ; NOT FIRST CHAR
 1BE1 0C29              INC     STBYTF
 1BE3 BD1AC1            JSR     GETWRD
 1BE6 DC3D              LDD     TEMP
 1BE8 DD2A              STD     ZSTWRD
 1BEA 44                LSRA
 1BEB 44                LSRA
 1BEC 841F      GTEXIT: ANDA    #%00011111
 1BEE 5F                CLRB                    ; CLEAR CARRY
 1BEF 39                RTS
                
 1BF0 4A        GETZH1: DECA
 1BF1 2614              BNE     GETZH2          ; MUST BE LAST CHAR
 1BF3 8602              LDA     #2
 1BF5 9729              STA     STBYTF
 1BF7 DC2A              LDD     ZSTWRD
 1BF9 44                LSRA
 1BFA 56                RORB
 1BFB 962A              LDA     ZSTWRD
 1BFD 44                LSRA
 1BFE 44                LSRA
 1BFF 56                RORB
 1C00 54                LSRB
 1C01 54                LSRB
 1C02 54                LSRB
 1C03 1F98      GETZH3: TFR     B,A             ; EXPECTED HERE
 1C05 20E5              BRA     GTEXIT
                
 1C07 0F29      GETZH2: CLR     STBYTF
 1C09 DC2A              LDD     ZSTWRD
 1C0B 2AF6              BPL     GETZH3
 1C0D 0329              COM     STBYTF          ; INDICATE END OF STRING
 1C0F 20F2              BRA     GETZH3
                
 1C11                   ; -------------------
 1C11                   ; CONVERT TO Z-STRING
 1C11                   ; -------------------
                
 1C11 CC0505    CONZST: LDD     #$0505          ; FILL OUTPUT BUFFER
 1C14 DD32              STD     ZSTBUO          ; WITH PAD CHARS
 1C16 DD34              STD     ZSTBUO+2


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 57                 
--- Z-STRING HANDLERS ---                                                                                                           

 1C18 DD36              STD     ZSTBUO+4
                
 1C1A 4C                INCA                    ; = 6
 1C1B 9741              STA     MASK            ; INIT CHAR COUNT
                
 1C1D 0F3B              CLR     VAL             ; RESET OUTPUT AND
 1C1F 0F3D              CLR     TEMP            ; INPUT INDEXES
                
 1C21 D63D      CNZSL1: LDB     TEMP
 1C23 0C3D              INC     TEMP
 1C25 8E002C            LDX     #ZSTBUI         ; POINT TO INPUT BUFFER
 1C28 A685              LDA     B,X             ; GRAB NEXT CHAR
 1C2A 9742              STA     MASK+1          ; SAVE IT HERE
 1C2C 2604              BNE     CNZSL2          ; IF CHAR WAS ZERO,
 1C2E 8605              LDA     #5              ; USE A Z-PAD
 1C30 2025              BRA     CNZSLO
                
 1C32 9642      CNZSL2: LDA     MASK+1
 1C34 BD1CB2            JSR     ZCHRCS          ; WHICH CHARSET TO USE?
 1C37 4D                TSTA
 1C38 2711              BEQ     CNZSLC          ; IF CHARSET 0, USE LOWER CASE
 1C3A 8B03              ADDA    #3
 1C3C D63B              LDB     VAL             ; OUTPUT A TEMP SHIFT
 1C3E 8E0032            LDX     #ZSTBUO
 1C41 A785              STA     B,X
 1C43 0C3B              INC     VAL
 1C45 0A41              DEC     MASK
 1C47 10270084          LBEQ    CNZSLE
                
 1C4B 9642      CNZSLC: LDA     MASK+1
 1C4D BD1CB2            JSR     ZCHRCS
 1C50 4A                DECA
 1C51 2A13              BPL     CNZSC1          ; NOT CHARSET 0!
 1C53 9642              LDA     MASK+1
 1C55 805B              SUBA    #$61-6          ; ASCII "a" MINUS 6
                
 1C57 D63B      CNZSLO: LDB     VAL
 1C59 8E0032            LDX     #ZSTBUO
 1C5C A785              STA     B,X
 1C5E 0C3B              INC     VAL
 1C60 0A41              DEC     MASK
 1C62 276B              BEQ     CNZSLE          ; ALL FINISHED
 1C64 20BB              BRA     CNZSL1          ; ELSE LOOP BACK FOR MORE
                
 1C66 2606      CNZSC1: BNE     CNZSC3          ; MUST BE CHARSET 3
 1C68 9642              LDA     MASK+1
 1C6A 803B              SUBA    #$41-6          ; ASCII "A" MINUS 6
 1C6C 20E9              BRA     CNZSLO
                
 1C6E 9642      CNZSC3: LDA     MASK+1
 1C70 BD1CA0            JSR     CNZS2M          ; IS IT IN TABLE?


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 58                 
--- Z-STRING HANDLERS ---                                                                                                           

 1C73 26E2              BNE     CNZSLO          ; YES, OUTPUT THE CHAR
 1C75 8606              LDA     #6              ; ELSE IT'S A "DIRECT" ASCII CHAR
 1C77 D63B              LDB     VAL
 1C79 8E0032            LDX     #ZSTBUO
 1C7C A785              STA     B,X             ; SEND "DIRECT" TO OUTPUT
 1C7E 0C3B              INC     VAL
 1C80 0A41              DEC     MASK
 1C82 274B              BEQ     CNZSLE          ; NO MORE ROOM!
                
 1C84                   ; CONVERT CHAR TO 2-BYTE DIRECT ASCII
                
 1C84 9642              LDA     MASK+1
 1C86 44                LSRA
 1C87 44                LSRA
 1C88 44                LSRA
 1C89 44                LSRA
 1C8A 44                LSRA
 1C8B 8403              ANDA    #%00000011
 1C8D D63B              LDB     VAL
 1C8F 8E0032            LDX     #ZSTBUO
 1C92 A785              STA     B,X
 1C94 0C3B              INC     VAL
 1C96 0A41              DEC     MASK
 1C98 2735              BEQ     CNZSLE          ; NO MORE ROOM!
 1C9A 9642              LDA     MASK+1
 1C9C 841F              ANDA    #%00011111      ; FORM 2ND Z-BYTE
 1C9E 20B7              BRA     CNZSLO          ; AND OUTPUT IT
                
 1CA0                   ; ----------------------
 1CA0                   ; SEARCH CHARSET 3 TABLE
 1CA0                   ; ----------------------
                
 1CA0 8E1CEC    CNZS2M: LDX     #CHRTBL
 1CA3 C619              LDB     #25
 1CA5 A185      CNLOOP: CMPA    B,X
 1CA7 2704              BEQ     CNOK
 1CA9 5A                DECB
 1CAA 26F9              BNE     CNLOOP
 1CAC 39                RTS                     ; RETURN ZERO IN B IF NO MATCH
                
 1CAD 1F98      CNOK:   TFR     B,A             ; EXPECTED IN [A]
 1CAF 8B06              ADDA    #6              ; CONVERT TO Z-CHAR
 1CB1 39                RTS
                
 1CB2                   ; -------------------------
 1CB2                   ; DETERMINE CHARSET OF CHAR
 1CB2                   ; -------------------------
                
 1CB2 8161      ZCHRCS: CMPA    #$61            ; ASCII "a"
 1CB4 2506              BLO     ZCHR1
 1CB6 817B              CMPA    #$7B            ; ASCII "z"+1


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 59                 
--- Z-STRING HANDLERS ---                                                                                                           

 1CB8 2402              BHS     ZCHR1
 1CBA 4F                CLRA                    ; IT'S CHARSET 0
 1CBB 39                RTS
                
 1CBC 8141      ZCHR1:  CMPA    #$41            ; ASCII "A"
 1CBE 2507              BLO     ZCHR2
 1CC0 815B              CMPA    #$5B            ; ASCII "Z"+1
 1CC2 2403              BHS     ZCHR2
 1CC4 8601              LDA     #1              ; IT'S CHARSET 1
 1CC6 39                RTS
                
 1CC7 4D        ZCHR2:  TSTA
 1CC8 2704              BEQ     ZCHRX           ; EXIT IF ZERO
 1CCA 2B02              BMI     ZCHRX           ; OR NEGATIVE
 1CCC 8602              LDA     #2              ; ELSE IT'S CHARSET 2
 1CCE 39        ZCHRX:  RTS
                
 1CCF                   ; ---------------
 1CCF                   ; CRUSH 6 Z-CHARS
 1CCF                   ; ---------------
                
 1CCF DC32      CNZSLE: LDD     ZSTBUO          ; HANDLE 1ST TRIPLET
 1CD1 58                ASLB
 1CD2 58                ASLB
 1CD3 58                ASLB
 1CD4 58                ASLB
 1CD5 49                ROLA
 1CD6 58                ASLB
 1CD7 49                ROLA
 1CD8 DA34              ORB     ZSTBUO+2
 1CDA DD32              STD     ZSTBUO
                
 1CDC DC35              LDD     ZSTBUO+3        ; HANDLE 2ND TRIPLET
 1CDE 58                ASLB
 1CDF 58                ASLB
 1CE0 58                ASLB
 1CE1 58                ASLB
 1CE2 49                ROLA
 1CE3 58                ASLB
 1CE4 49                ROLA
 1CE5 DA37              ORB     ZSTBUO+5
 1CE7 8A80              ORA     #%10000000      ; SET SIGN BIT OF LAST Z-BYTE
 1CE9 DD34              STD     ZSTBUO+2
 1CEB 39                RTS
                
 1CEC                   ; ----------------------
 1CEC                   ; CHARSET 2 DECODE TABLE
 1CEC                   ; ----------------------
                
 1CEC 00        CHRTBL: DB      0               ; DUMMY BYTE
 1CED 0D                DB      $0D             ; CARRIAGE RETURN


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 60                 
--- Z-STRING HANDLERS ---                                                                                                           

 1CEE 3031323334        DB      "0123456789.,!?_#"
 1CFE 27                DB      $27             ; SINGLE QUOTE
 1CFF 22                DB      $22             ; DOUBLE QUOTE
 1D00 2F5C2D3A28        DB      "/\-:()"
                
                        END
                        INCLUD OBJECTS.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 61                 
--- OBJECT & PROPERTY HANDLERS ---                                                                                                  

                
 1D06 9603      PROPB:  LDA     ARG1+1
 1D08 BD1D69            JSR     OBJLOC
 1D0B 9E3D              LDX     TEMP
 1D0D EC07              LDD     7,X
 1D0F C32500            ADDD    #ZCODE
 1D12 DD3D              STD     TEMP            ; EXPECTED HERE
 1D14 1F01              TFR     D,X
 1D16 E684              LDB     ,X              ; GET FIRST BYTE (LENGTH OF DESC)
 1D18 58                ASLB                    ; WORD-ALIGN IT
 1D19 5C                INCB                    ; AND POINT JUST PAST IT
 1D1A 39                RTS
                
 1D1B 9E3D      PROPN:  LDX     TEMP
 1D1D 3A                ABX
 1D1E A684              LDA     ,X
 1D20 841F              ANDA    #%00011111
 1D22 39                RTS
                
 1D23 9E3D      PROPL:  LDX     TEMP
 1D25 3A                ABX
 1D26 A684              LDA     ,X
 1D28 46                RORA
 1D29 46                RORA
 1D2A 46                RORA
 1D2B 46                RORA
 1D2C 46                RORA
 1D2D 8407              ANDA    #%00000111
 1D2F 39                RTS
                
 1D30 8DF1      PROPNX: BSR     PROPL
 1D32 973B              STA     VAL
 1D34 5C        PPX:    INCB
 1D35 0A3B              DEC     VAL
 1D37 2AFB              BPL     PPX
 1D39 5C                INCB
 1D3A 39                RTS
                
 1D3B 9603      FLAGSU: LDA     ARG1+1
 1D3D BD1D69            JSR     OBJLOC
 1D40 9605              LDA     ARG2+1
 1D42 8110              CMPA    #16
 1D44 2508              BLO     FLGSU1
 1D46 8010              SUBA    #16
 1D48 9E3D              LDX     TEMP
 1D4A 3002              LEAX    2,X
 1D4C 9F3D              STX     TEMP
                
 1D4E 973C      FLGSU1: STA     VAL+1
 1D50 CC0001            LDD     #1
 1D53 DD41              STD     MASK


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- OPCODE SUPPORT                                                                   PAGE 62                 
--- OBJECT & PROPERTY HANDLERS ---                                                                                                  

 1D55 C60F              LDB     #15
 1D57 D03C              SUBB    VAL+1
                
 1D59 2707      FLGSU2: BEQ     FLGSU3
 1D5B 0842              ASL     MASK+1
 1D5D 0941              ROL     MASK
 1D5F 5A                DECB
 1D60 20F7              BRA     FLGSU2
                
 1D62 9E3D      FLGSU3: LDX     TEMP
 1D64 EC84              LDD     ,X
 1D66 DD3B              STD     VAL
 1D68 39                RTS
                
 1D69 C609      OBJLOC: LDB     #9              ; NUMBER IN [A] TIMES 9
 1D6B 3D                MUL
 1D6C C30035            ADDD    #53             ; PLUS 53
 1D6F F3250A            ADDD    ZCODE+ZOBJEC    ; Z-ADDRESS OF OBJECT TABLE
 1D72 C32500            ADDD    #ZCODE          ; FORM ABSOLUTE ADDRESS
 1D75 DD3D              STD     TEMP
 1D77 39                RTS
                
                        END
                
                        INCLUD IOPRIMS.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 63                 
--- OS EQUATES ---                                                                                                                  

                
 0400           SCREEN  EQU     $400            ; START OF SCREEN RAM
 0600           ENDSCR  EQU     $600            ; END OF SCREEN RAM
 A000           POLCAT  EQU     $A000           ; KEYCODE FETCH VECTOR
 A002           CHROUT  EQU     $A002           ; PRINT CHAR TO DEVICE
 006F           DEVNUM  EQU     $6F             ; $00 = SCREEN, $FE = PRINTER
 0088           CURSOR  EQU     $88             ; ABSOLUTE CURSOR ADDRESS
 FF03           INT60   EQU     $FF03           ; 60HZ INTERRUPT CONTROL
 FFDE           ROMON   EQU     $FFDE           ; ROM ENABLE
 FFDF           ROMOFF  EQU     $FFDF           ; ROM DISABLE
 00FE           MEMTOP  EQU     $FE             ; TOP PAGE OF AVAILABLE RAM
                
 000D           EOL     EQU     $0D             ; END-OF-LINE CHARACTER
 0008           BS      EQU     $08             ; BACKSPACE CHARACTER
 0020           SPACE   EQU     $20             ; SPACE CHARACTER
 0005           CDURAT  EQU     5               ; DURATION OF KEYCLICKS
 0019           CFREQ   EQU     25              ; FREQUENCY OF KEYCLICKS
 000A           BDURAT  EQU     10              ; DURATION OF RAZZ
 01F4           BFREQ   EQU     500             ; FREQUENCY OF RAZZ
 0180           FLASH   EQU     $180            ; CURSOR BLINK FREQUENCY
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 64                 
I/O PRIMITIVES                                                                                                                      

                
 1D78                   ; ------------------
 1D78                   ; ENABLE ROM FOR I/O
 1D78                   ; ------------------
                
 1D78 975D      ROMIN:  STA     IHOLD           ; SAVE [A]
 1D7A B6FF03            LDA     INT60           ; ENABLE 60HZ INTERRUPT
 1D7D 8A01              ORA     #%00000001
 1D7F B7FFDE            STA     ROMON
 1D82 B7FF03            STA     INT60
 1D85 1CAF              ANDCC   #%10101111      ; INTERRUPTS ON
 1D87 965D              LDA     IHOLD           ; RESTORE [A]
 1D89 39                RTS
                
 1D8A                   ; ------------------
 1D8A                   ; ACCESS OS KEY-READ
 1D8A                   ; ------------------
                
 1D8A 8DEC      CATPOL: BSR     ROMIN
 1D8C AD9FA000  CPL:    JSR     [POLCAT]
 1D90 27FA              BEQ     CPL
                
 1D92                   ; FALL THROUGH TO ...
                
 1D92                   ; ----------------------
 1D92                   ; DISABLE ROM FOR PAGING
 1D92                   ; ----------------------
                
 1D92 975D      ROMOUT: STA     IHOLD           ; SAVE [A]
 1D94 1A50              ORCC    #%01010000      ; DISABLE INTERUPTS
 1D96 B6FF03            LDA     INT60
 1D99 84FE              ANDA    #%11111110      ; DISABLE 60HZ INTERRUPT
 1D9B B7FF03            STA     INT60
 1D9E B7FFDF            STA     ROMOFF          ; AND ROM
 1DA1 965D              LDA     IHOLD           ; RESTORE [A]
 1DA3 39                RTS
                
 1DA4                   ; --------------------
 1DA4                   ; ACCESS OS CHAR-PRINT
 1DA4                   ; --------------------
                
 1DA4 8DD2      OUTCHR: BSR     ROMIN
 1DA6 AD9FA002          JSR     [CHROUT]
 1DAA 20E6              BRA     ROMOUT
                
 1DAC                   ; -----------------
 1DAC                   ; READ A SINGLE KEY
 1DAC                   ; -----------------
                
 1DAC                   ; EXIT: KEYCODE IN [A]
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 65                 
I/O PRIMITIVES                                                                                                                      

 1DAC BD1F49    BADKEY: JSR     BOOP            ; RAZZ
 1DAF 2004              BRA     WINK            ; AND TRY AGAIN
                
 1DB1 3474      GETKEY: PSHS    U,X,Y,B         ; SAVE THESE
 1DB3 8DC3              BSR     ROMIN
 1DB5 108E0600  WINK:   LDY     #FLASH*4
 1DB9 109F58            STY     CYCLE           ; LONG DELAY
 1DBC 8680              LDA     #%10000000
 1DBE 975A              STA     BLINK           ; INIT CURSOR MASK
 1DC0 AD9FA000  GK:     JSR     [POLCAT]        ; LET OS DO THE WORK
 1DC4 0C56              INC     RAND1           ; GENERATE RANDOMNESS
 1DC6 0D5B              TST     CFLAG           ; BLINK CURSOR?
 1DC8 2721              BEQ     NOBLIN          ; NOT IF [CFLAG]=0
 1DCA 109E58            LDY     CYCLE
 1DCD 313F              LEAY    -1,Y
 1DCF 109F58            STY     CYCLE           ; TIME TO BLINK?
 1DD2 260D              BNE     KTEST           ; NOT YET
 1DD4 108E0180          LDY     #FLASH          ; ELSE RESET BLINK TIMER
 1DD8 109F58            STY     CYCLE
 1DDB D65A              LDB     BLINK
 1DDD C880              EORB    #%10000000      ; FLIP CURSOR MASK
 1DDF D75A              STB     BLINK
 1DE1 9E88      KTEST:  LDX     CURSOR
 1DE3 E684              LDB     ,X
 1DE5 C47F              ANDB    #%01111111
 1DE7 DA5A              ORB     BLINK
 1DE9 E784              STB     ,X
 1DEB 4D        NOBLIN: TSTA
 1DEC 27D2              BEQ     GK              ; 0 = NO KEY YET
 1DEE 0D5B              TST     CFLAG
 1DF0 2704              BEQ     QKEY
 1DF2 C47F              ANDB    #%01111111      ; CLEAR CURSOR
 1DF4 E784              STB     ,X
                
 1DF6                   ; SCREEN KEYCODE IN [A]
                
 1DF6 810D      QKEY:   CMPA    #EOL            ; "ENTER" IS FINE
 1DF8 2718              BEQ     CLICK
 1DFA 8108              CMPA    #BS             ; SO IS "LEFT ARROW"
 1DFC 2714              BEQ     CLICK
                
 1DFE 8161              CMPA    #$61            ; LOWER-CASE ALPHA?
 1E00 2508              BLO     PKICK           ; NO, CHECK FOR OTHERS
 1E02 817B              CMPA    #$7B            ; NOTHING ABOVE "z" IS LEGAL
 1E04 24A6              BHS     BADKEY
 1E06 8020              SUBA    #$20            ; CONVERT TO UPPER-CASE ALPHA
 1E08 2008              BRA     CLICK
                
 1E0A 815B      PKICK:  CMPA    #$5B            ; NOTHING BETWEEN "Z" AND "a"
 1E0C 249E              BHS     BADKEY          ; IS LEGAL
 1E0E 8120              CMPA    #$20            ; NOTHING BELOW "SPACE"


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 66                 
I/O PRIMITIVES                                                                                                                      

 1E10 259A              BLO     BADKEY          ; IS LEGAL EITHER
                
 1E12                   ; "CLICK" SOUND FOR KEYS
                
 1E12 9753      CLICK:  STA     IOCHAR          ; SAVE KEY HERE
 1E14 BD1F30            JSR     AINIT           ; ENABLE SOUND
 1E17 C605              LDB     #CDURAT
 1E19 86FE      TICK:   LDA     #%11111110
 1E1B B7FF20            STA     $FF20
 1E1E 8D11              BSR     CDELAY
 1E20 8602              LDA     #%00000010
 1E22 B7FF20            STA     $FF20
 1E25 8D0A              BSR     CDELAY
 1E27 5A                DECB
 1E28 26EF              BNE     TICK
 1E2A 3574              PULS    U,X,Y,B         ; RESTORE THINGS
 1E2C 9653              LDA     IOCHAR          ; RETRIEVE THE KEYPRESS
 1E2E 7E1D92            JMP     ROMOUT          ; AND SWITCH OUT THE ROMS
                
 1E31                   ; DELAY FOR KEYCLICK
                
 1E31 8E0019    CDELAY: LDX     #CFREQ
 1E34 301F      CDEL:   LEAX    -1,X
 1E36 26FC              BNE     CDEL
 1E38 39                RTS
                
 1E39                   ; -------------------
 1E39                   ; READ A LINE OF TEXT
 1E39                   ; -------------------
                
 1E39                   ; ENTRY: [ARG1] HAS ADDRESS OF CHAR BUFFER
 1E39                   ;        LENGTH OF BUFFER IN 1ST BYTE
 1E39                   ; EXIT: # CHARS READ IN [A]
                
 1E39 BD2071    INPUT:  JSR     LINOUT          ; FLUSH OUTPUT BUFFER
 1E3C 0F52              CLR     LINCNT          ; RESET LINE COUNTER
 1E3E 0F6F              CLR     DEVNUM          ; POINT TO SCREEN
 1E40 9E02              LDX     ARG1            ; GET ADDRESS OF INPUT BUFFER
 1E42 E680              LDB     ,X+             ; GET MAX # CHARS
 1E44 C002              SUBB    #2              ; LEAVE A MARGIN FOR ERROR
 1E46 D751              STB     BINDEX          ; SAVE MAX # CHARS
 1E48 D75B              STB     CFLAG           ; ENABLE CURSOR
                
 1E4A 5F                CLRB                    ; RESET INDEX
 1E4B BD1DB1    INLOOP: JSR     GETKEY          ; KEY IN [A] AND [IOCHAR]
 1E4E 810D              CMPA    #EOL            ; IF EOL,
 1E50 2732              BEQ     ENDLIN          ; LINE IS DONE
 1E52 8108              CMPA    #BS             ; IF BACKSPACE,
 1E54 2725              BEQ     GOBACK          ; TAKE CARE OF IT
                
 1E56 8141              CMPA    #$41            ; IF LOWER THAN ASCII "A,"


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 67                 
I/O PRIMITIVES                                                                                                                      

 1E58 2502              BLO     SENDCH          ; SEND THE CHARACTER
 1E5A 8B20              ADDA    #$20            ; ELSE CONVERT TO LOWER-CASE
                
 1E5C A785      SENDCH: STA     B,X             ; SEND CHAR TO BUFFER
 1E5E 5C                INCB                    ; UPDATE INDEX
 1E5F 9653      TOSCR:  LDA     IOCHAR          ; RETRIEVE KEY CHAR
 1E61 8D4C              BSR     CHAR            ; ECHO CHAR TO SCREEN
 1E63 D151              CMPB    BINDEX          ; BUFFER FILLED?
 1E65 2404              BHS     NOMORE          ; YES -- INSIST ON BS OR EOL
 1E67 C13D              CMPB    #61             ; 2 SCREEN LINES FILLED?
 1E69 25E0              BLO     INLOOP          ; NO, KEEP GOING
                
 1E6B                   ; LINE FULL; INSIST ON EOL OR BACKSPACE
                
 1E6B BD1DB1    NOMORE: JSR     GETKEY          ; GET NEXT KEY
 1E6E 810D              CMPA    #EOL            ; IF EOL,
 1E70 2712              BEQ     ENDLIN          ; WE'RE FINE
 1E72 8108              CMPA    #BS             ; BACKSPACE
 1E74 2705              BEQ     GOBACK          ; IS OKAY TOO
 1E76 BD1F49            JSR     BOOP
 1E79 20F0              BRA     NOMORE          ; ELSE PERSIST
                
 1E7B                   ; HANDLE BACKSPACE
                
 1E7B 5A        GOBACK: DECB                    ; BACK UP CHAR COUNT
 1E7C 2AE1              BPL     TOSCR           ; SEND TO SCREEN IF NO UNDERFLOW
 1E7E 5F                CLRB                    ; ELSE RESET COUNT
 1E7F BD1F49            JSR     BOOP            ; RAZZ
 1E82 20C7              BRA     INLOOP          ; AND TRY AGAIN
                
 1E84                   ; HANDLE EOL
                
 1E84 A785      ENDLIN: STA     B,X             ; PUT EOL IN BUFFER
 1E86 8D27              BSR     CHAR            ; AND ON SCREEN
 1E88 5C                INCB                    ; UPDATE CHAR COUNT
 1E89 D751              STB     BINDEX          ; SAVE IT HERE
                
 1E8B                   ; FALL THROUGH TO ...
                
 1E8B                   ; ---------------------
 1E8B                   ; SCRIPT A LINE OF TEXT
 1E8B                   ; ---------------------
                
 1E8B                   ; ENTRY: ADDRESS OF TEXT IN [X]
 1E8B                   ;        LENGTH OF LINE IN [BINDEX]
                
 1E8B 0D5C      TOPRIN: TST     SCRIPT          ; SCRIPTING ENABLED?
 1E8D 2719              BEQ     INPEX           ; NO, EXIT IMMEDIATELY
 1E8F B62511            LDA     ZCODE+ZSCRIP+1  ; GET FLAGS BYTE
 1E92 8401              ANDA    #1              ; BIT 0 SET?
 1E94 2712              BEQ     INPEX           ; NO, IGNORE THE FOLLOWING


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 68                 
I/O PRIMITIVES                                                                                                                      

 1E96 86FE              LDA     #$FE            ; ELSE
 1E98 976F              STA     DEVNUM          ; POINT TO PRINTER
 1E9A D651              LDB     BINDEX          ; START AT 1ST BUFFER CHAR
 1E9C BD1D78            JSR     ROMIN
 1E9F A680      SCROUT: LDA     ,X+             ; GRAB A CHAR FROM BUFFER
 1EA1 AD9FA002          JSR     [CHROUT]
 1EA5 5A                DECB
 1EA6 26F7              BNE     SCROUT
 1EA8 0F6F      INPEX:  CLR     DEVNUM          ; POINT BACK TO SCREEN
 1EAA 9651              LDA     BINDEX          ; RETRIEVE # CHARS IN LINE
 1EAC 7E1D92            JMP     ROMOUT          ; MAKE SURE ROM IS GONE
                
 1EAF                   ; -------------------
 1EAF                   ; PRINT A SINGLE CHAR
 1EAF                   ; -------------------
                
 1EAF                   ; ENTRY: ASCII CODE IN [A]
                
 1EAF 9753      CHAR:   STA     IOCHAR          ; SAVE CHAR HERE
 1EB1 0F6F              CLR     DEVNUM          ; ALWAYS TO SCREEN
 1EB3 3434              PSHS    X,Y,B           ; SAVE THESE
 1EB5 0C56              INC     RAND1           ; RANDOMNESS
 1EB7 109E88            LDY     CURSOR          ; GET ADDRESS OF CURSOR
 1EBA 810D              CMPA    #EOL            ; EOL?
 1EBC 273F              BEQ     OUTEOL          ; HANDLE IT
 1EBE 108C05FF          CMPY    #ENDSCR-1       ; LAST CHAR ON SCREEN?
 1EC2 252B              BLO     NOSCRL          ; NO SCROLL NECESSARY
                
 1EC4                   ; SCROLL SCREEN, RETAINING STATUS LINE
                
 1EC4 108E05DF  DOSCRL: LDY     #ENDSCR-33      ; LAST POSITION IN LINE 14
 1EC8 109F88            STY     CURSOR          ; MOVE CURSOR
 1ECB 8E0440            LDX     #SCREEN+64      ; [X] POINTS TO "SOURCE" LINE
 1ECE 108E0420          LDY     #SCREEN+32      ; [Y] POINTS TO "DEST" LINE
                
 1ED2 C61F      SCRL1:  LDB     #31             ; INIT LINE INDEX
                
 1ED4 A685      SCRL2:  LDA     B,X             ; GET CHAR FROM SOURCE LINE
 1ED6 A7A5              STA     B,Y             ; MOVE TO DESTINATION LINE
 1ED8 5A                DECB                    ; CONTINUE TILL
 1ED9 2AF9              BPL     SCRL2           ; ENTIRE LINE IS MOVED
                
 1EDB 1F12              TFR     X,Y             ; SOURCE IS NOW DEST
 1EDD 308820            LEAX    32,X            ; ADD 32 TO SOURCE ADDRESS
 1EE0 8C0600            CMPX    #ENDSCR         ; END OF SCREEN YET?
 1EE3 25ED              BLO     SCRL1           ; NO, KEEP MOVING
                
 1EE5 8660              LDA     #$60            ; SPACE CHAR
 1EE7 A7A0      SCRL3:  STA     ,Y+
 1EE9 108C0600          CMPY    #ENDSCR
 1EED 25F8              BLO     SCRL3


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 69                 
I/O PRIMITIVES                                                                                                                      

                
 1EEF 9653      NOSCRL: LDA     IOCHAR          ; RESTORE CHAR
                
 1EF1                   ; LOWER-CASE OPTION
                
                ;       TST     LCASE           ; LOWER-CASE CAPABILITY?
                ;       BNE     GOCHR           ; YES, SEND CHAR AS-IS
                
 1EF1 8161              CMPA    #$61            ; LESS THAN "a"?
 1EF3 2502              BLO     GOCHR           ; YES, USE AS-IS
 1EF5 8020              SUBA    #$20            ; ELSE CONVERT TO UPPER CASE
 1EF7 BD1DA4    GOCHR:  JSR     OUTCHR          ; OFF TO THE SCREEN!
 1EFA 3534              PULS    X,Y,B           ; RESTORE
 1EFC 39                RTS
                
 1EFD                   ; EOL SCROLL?
                
 1EFD 108C05E0  OUTEOL: CMPY    #ENDSCR-32      ; ON LAST SCREEN LINE?
 1F01 25EC              BLO     NOSCRL          ; NO, HANDLE NORMALLY
 1F03 20BF              BRA     DOSCRL          ; ELSE MOVE CURSOR UP!
                
 1F05                   ; --------------------------
 1F05                   ; PRINT CONTENTS OF [BUFFER]
 1F05                   ; --------------------------
                
 1F05 D64F      BUFOUT: LDB     CHRPNT          ; # CHARS IN BUFFER
 1F07 8E1020            LDX     #BUFFER         ; BUFFER ADDRESS
                
 1F0A                   ; FALL THROUGH TO ...
                
 1F0A                   ; -------------
 1F0A                   ; PRINT MESSAGE
 1F0A                   ; -------------
                
 1F0A                   ; ENTRY: ADDRESS OF ASCII MESSAGE IN [X]
 1F0A                   ;        LENGTH OF MESSAGE IN [B]
                
 1F0A D751      LINE:   STB     BINDEX          ; SAVE LENGTH
 1F0C 5F                CLRB                    ; INIT INDEX
                
 1F0D A685      LN:     LDA     B,X             ; GET A CHAR
 1F0F BD1EAF            JSR     CHAR
 1F12 5C                INCB
 1F13 D151              CMPB    BINDEX
 1F15 25F6              BLO     LN
 1F17 7E1E8B            JMP     TOPRIN          ; HANDLE SCRIPTING
                
 1F1A                   ; ----------------
 1F1A                   ; CLEAR THE SCREEN
 1F1A                   ; ----------------
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 70                 
I/O PRIMITIVES                                                                                                                      

 1F1A 8660      CLS:    LDA     #$60            ; SPACE CHARS
 1F1C 8E0400            LDX     #SCREEN
 1F1F A780      CLS0:   STA     ,X+
 1F21 8C0600            CMPX    #ENDSCR
 1F24 25F9              BLO     CLS0
 1F26 CC0420            LDD     #SCREEN+32
 1F29 DD88              STD     CURSOR          ; CURSOR AT LINE 1
 1F2B 0F52              CLR     LINCNT          ; RESET LINE COUNTER
 1F2D 0F4F              CLR     CHRPNT          ; AND CHAR INDEX
 1F2F 39                RTS
                
 1F30                   ; --------------
 1F30                   ; SOUND HANDLERS
 1F30                   ; --------------
                
 1F30 B6FF01    AINIT:  LDA     $FF01           ; ENABLE SOUND
 1F33 84F7              ANDA    #%11110111
 1F35 B7FF01            STA     $FF01
 1F38 B6FF03            LDA     $FF03
 1F3B 84F7              ANDA    #%11110111
 1F3D B7FF03            STA     $FF03
 1F40 B6FF23            LDA     $FF23
 1F43 8A08              ORA     #%00001000
 1F45 B7FF23            STA     $FF23
 1F48 39                RTS
                
 1F49                   ; DO THE RAZZ
                
 1F49 3416      BOOP:   PSHS    X,D
 1F4B 8DE3              BSR     AINIT
 1F4D C60A              LDB     #BDURAT         ; GET DURATION
 1F4F 86FE      BOOP1:  LDA     #%11111110      ; CREST OF WAVE
 1F51 B7FF20            STA     $FF20           ; SEND TO DAC
 1F54 8D0D              BSR     DELAY           ; WAIT ...
 1F56 8602              LDA     #%00000010
 1F58 B7FF20            STA     $FF20           ; TROUGH
 1F5B 8D06              BSR     DELAY           ; WAIT AGAIN
 1F5D 5A                DECB                    ; ELSE NEXT DURATION
 1F5E 26EF              BNE     BOOP1
 1F60 3516              PULS    X,D
 1F62 39                RTS
                
 1F63                   ; TIME DELAY
                
 1F63 8E01F4    DELAY:  LDX     #BFREQ          ; INIT FREQUENCY
 1F66 301F      DELOOP: LEAX    -1,X
 1F68 26FC              BNE     DELOOP
 1F6A 39                RTS
                
                        END
                        INCLUD SCREEN.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 71                 
--- SCREEN & PRINTER I/O ---                                                                                                        

                
 1F6B                   ; ------------
 1F6B                   ; RESTART GAME
 1F6B                   ; ------------
                
 1F6B BD2037    ZSTART: JSR     ZCRLF           ; FLUSH OUTPUT BUFFER
 1F6E 0F5C              CLR     SCRIPT          ; DISABLE SCRIPTING
 1F70 BD22EF            JSR     ENTER           ; "PRESS ANY KEY TO CONTINUE"
                
 1F73                   ; FALL THROUGH TO ...
                
 1F73                   ; ---------
 1F73                   ; COLDSTART
 1F73                   ; ---------
                
 1F73 0F5C      COLD:   CLR     SCRIPT          ; DISABLE SCRIPTING
 1F75 BD1F1A            JSR     CLS             ; A CLEAN SLATE
 1F78 CC04C8            LDD     #SCREEN+200     ; POSITION
 1F7B DD88              STD     CURSOR          ; CURSOR
 1F7D 8E1F88            LDX     #LOADM
 1F80 C610              LDB     #LOADML
 1F82 BD1F0A            JSR     LINE            ; "LOADING GAME ..."
 1F85 7E1117            JMP     START           ; AND DO A WARMSTART
                
 1F88 4C4F414449LOADM:  DB      "LOADING GAME ..."
 0010           LOADML  EQU     $-LOADM
                
 1F98                   ; -----
 1F98                   ; ERROR
 1F98                   ; -----
                
 1F98                   ; ENTRY: ERROR CODE # IN [A]
                
 1F98 0D        INTERR: DB      EOL
 1F99 494E544552        DB      "INTERNAL ERROR #"
 0011           IERRL   EQU     $-INTERR
                
 1FA9 3402      ZERROR: PSHS    A               ; SAVE CODE #
 1FAB BD2037            JSR     ZCRLF           ; FLUSH BUFFER
 1FAE 8E1F98            LDX     #INTERR
 1FB1 C611              LDB     #IERRL
 1FB3 BD1F0A            JSR     LINE            ; "INTERNAL ERROR #"
 1FB6 3502              PULS    A               ; RETRIEVE CODE #
 1FB8 973E              STA     TEMP+1
 1FBA 0F3D              CLR     TEMP
 1FBC BD18D5            JSR     NUMBER          ; CONVERT ERROR CODE #
 1FBF BD2066            JSR     CR1             ; AND SHOW IT
                
 1FC2                   ; FALL THROUGH TO ...
                
 1FC2                   ; ----


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 72                 
--- SCREEN & PRINTER I/O ---                                                                                                        

 1FC2                   ; QUIT
 1FC2                   ; ----
                
 1FC2 8E1FD2    ZQUIT:  LDX     #ENDSES
 1FC5 C60F              LDB     #ENDSL
 1FC7 BD1F0A            JSR     LINE            ; "END OF SESSION"
 1FCA 7FFF40            CLR     MOTOR           ; SHUT DOWN DRIVE MOTOR
 1FCD BD1D78            JSR     ROMIN           ; MAKE SURE ROM'S ACTIVE
 1FD0 20FE      FREEZE: BRA     FREEZE          ; STOP DEAD
                
 1FD2 454E44204FENDSES: DB      "END OF SESSION"
 1FE0 0D        VCODE:  DB      EOL             ; SHARED EOL CHAR
 000F           ENDSL   EQU     $-ENDSES
                
 1FE1                   ; --------------------------
 1FE1                   ; DISPLAY ZIP VERSION NUMBER
 1FE1                   ; --------------------------
                
 1FE1 5645525349        DB      "VERSION A"
 1FEA 0D                DB      EOL
 000B           VCODEL  EQU     $-VCODE
                
 1FEB 8E1FE0    VERNUM: LDX     #VCODE
 1FEE C60B              LDB     #VCODEL
 1FF0 7E1F0A            JMP     LINE
                
 1FF3                   ; -----------------
 1FF3                   ; PRINT A CHARACTER
 1FF3                   ; -----------------
                
 1FF3 8E1020    COUT:   LDX     #BUFFER         ; POINT TO I/O BUFFER
 1FF6 D64F              LDB     CHRPNT          ; GET LINE INDEX
 1FF8 810D              CMPA    #EOL            ; IF THIS IS A CR,
 1FFA 273B              BEQ     ZCRLF           ; HANDLE AS SUCH
 1FFC 8120              CMPA    #SPACE          ; IGNORE OTHER CONTROLS
 1FFE 2508              BLO     COUT1
                
 2000 A785              STA     B,X             ; SEND CHAR TO BUFFER
 2002 C11F              CMPB    #31             ; END OF SCREEN LINE?
 2004 2403              BHS     FLUSH           ; YES, SO FLUSH CURRENT BUFFER
 2006 0C4F              INC     CHRPNT          ; ELSE UPDATE INDEX
 2008 39        COUT1:  RTS                     ; AND LEAVE
                
 2009                   ; FLUSH CONTENTS OF [BUFFER]
                
 2009 8620      FLUSH:  LDA     #SPACE
 200B A185      FLUSH1: CMPA    B,X             ; FIND LAST SPACE CHAR
 200D 2705              BEQ     FLUSH2          ; IN CURRENT LINE
 200F 5A                DECB
 2010 26F9              BNE     FLUSH1          ; KEEP SCANNING
 2012 C61F              LDB     #31             ; SEND ENTIRE LINE IF NONE FOUND


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 73                 
--- SCREEN & PRINTER I/O ---                                                                                                        

                
 2014 D750      FLUSH2: STB     CPSAV           ; SAVE
 2016 D74F              STB     CHRPNT          ; # CHARS IN LINE
 2018 BD2037            JSR     ZCRLF           ; OUTPUT 1ST PART OF LINE
                
 201B                   ; START NEW LINE WITH REMAINDER OF OLD
                
 201B 0C50      FLUSH3: INC     CPSAV           ; GET 1ST CHAR
 201D D650              LDB     CPSAV           ; OF REMAINDER
 201F C11F              CMPB    #31             ; END OF LINE YET?
 2021 2301              BLS     FLUSH4          ; NO, MOVE IT FORWARD
 2023 39                RTS                     ; ELSE WE'RE DONE HERE
                
 2024 8E1020    FLUSH4: LDX     #BUFFER         ; POINT TO BUFFER
 2027 A685              LDA     B,X             ; GET OLD CHAR
 2029 D64F              LDB     CHRPNT          ; THIS WAS RESET BY CRLF
 202B A785              STA     B,X             ; MOVE TO START OF BUFFER
 202D 0C4F              INC     CHRPNT          ; NEXT POSITION
 202F 20EA              BRA     FLUSH3          ; KEEP MOVING
                
 2031 5B6D6F7265MORES:  DB      "[more]"
 0006           MOREL   EQU     $-MORES
                
 2037                   ; ---------------
 2037                   ; CARRIAGE RETURN
 2037                   ; ---------------
                
 2037 0C52      ZCRLF:  INC     LINCNT          ; NEW LINE GOING OUT
 2039 9652              LDA     LINCNT
 203B 810E              CMPA    #14             ; 14 LINES SENT YET?
 203D 2527              BLO     CR1             ; NO, KEEP GOING
                
 203F 8D3A              BSR     ZUSL            ; UPDATE STATUS LINE
                
 2041 DC88              LDD     CURSOR          ; GET CURSOR POSITION
 2043 DD3F              STD     TEMP2           ; AND SAVE IT
                
 2045 8E2031            LDX     #MORES          ; "[MORE]"
 2048 C606              LDB     #MOREL
 204A BD2384            JSR     DLINE
                
 204D 0F5B              CLR     CFLAG           ; NO CURSOR!
 204F BD1DB1            JSR     GETKEY          ; GET A KEYPRESS
                
 2052 DC3F              LDD     TEMP2
 2054 DD88              STD     CURSOR          ; RESTORE CURSOR
                
 2056 8620              LDA     #SPACE          ; ERASE "MORE" MESSAGE
 2058 C606              LDB     #MOREL          ; WITH SPACES
 205A BD1DA4    SPCS:   JSR     OUTCHR
 205D 5A                DECB


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 74                 
--- SCREEN & PRINTER I/O ---                                                                                                        

 205E 26FA              BNE     SPCS
                
 2060 DC3F              LDD     TEMP2
 2062 DD88              STD     CURSOR          ; RESTORE CURSOR AGAIN
 2064 0F52              CLR     LINCNT          ; RESET LINE COUNTER
                
 2066 D64F      CR1:    LDB     CHRPNT
 2068 8E1020            LDX     #BUFFER
 206B 860D              LDA     #EOL            ; INSTALL AN EOL
 206D A785              STA     B,X             ; AT END OF CURRENT LINE
 206F 0C4F              INC     CHRPNT          ; ADD IT TO CHAR COUNT
                
 2071 0D4F      LINOUT: TST     CHRPNT          ; IF NO CHARS IN BUFFER
 2073 2705              BEQ     SCDONE          ; DON'T PRINT ANYTHING
 2075 BD1F05    OUTPUT: JSR     BUFOUT          ; ELSE DISPLAY BUFFER
 2078 0F4F              CLR     CHRPNT          ; RESET CHAR INDEX
 207A 39        SCDONE: RTS                     ; AND RETURN
                
 207B                   ; ------------------
 207B                   ; UPDATE STATUS LINE
 207B                   ; ------------------
                
 207B 964F      ZUSL:   LDA     CHRPNT          ; SAVE ALL Z-STRING VARS
 207D D629              LDB     STBYTF
 207F 9E88              LDX     CURSOR
 2081 109E2A            LDY     ZSTWRD
 2084 3436              PSHS    X,Y,D
 2086 9618              LDA     MPCH            ; HIGH BIT OF MPC
 2088 D651              LDB     BINDEX
 208A 9E19              LDX     MPCM            ; LOW BYTES OF MPC
 208C 109E27            LDY     CSTEMP          ; TEMP & PERM TOGETHER!
 208F 3436              PSHS    X,Y,D
                
 2091 108E1040          LDY     #BUFSAV         ; MOVE OUTPUT BUFFER
 2095 8E1020            LDX     #BUFFER         ; TO TEMPORARY STORAGE
 2098 C620              LDB     #SPACE          ; CLEAR [BUFFER] WITH SPACES
 209A A684      ZUSL1:  LDA     ,X
 209C E780              STB     ,X+
 209E A7A0              STA     ,Y+
 20A0 8C1040            CMPX    #BUFFER+32
 20A3 25F5              BLO     ZUSL1
                
 20A5 8E0400            LDX     #SCREEN
 20A8 8620              LDA     #$20            ; CLEAR OLD STATUS LINE
 20AA A780      ZSL:    STA     ,X+
 20AC 8C0420            CMPX    #SCREEN+32
 20AF 25F9              BLO     ZSL
                
 20B1                   ; DISPLAY ROOM NAME
                
 20B1 0F4F              CLR     CHRPNT          ; RESET CHAR INDEX


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 75                 
--- SCREEN & PRINTER I/O ---                                                                                                        

 20B3 0F5C              CLR     SCRIPT          ; DISABLE SCRIPTING
 20B5 CC0400            LDD     #SCREEN         ; HOME THE
 20B8 DD88              STD     CURSOR          ; CURSOR
                
 20BA 8610              LDA     #$10            ; GLOBAL VAR #0 (ROOM #)
 20BC BD129D            JSR     VARGET
 20BF 963E              LDA     TEMP+1
 20C1 BD155E            JSR     PRNTDC          ; GET SHORT DESC INTO [BUFFER]
                
 20C4 8617              LDA     #23             ; ADVANCE BUFFER INDEX
 20C6 974F              STA     CHRPNT          ; INTO SCORING POSITION
                
 20C8 8611              LDA     #$11            ; FETCH GLOBAL VARIABLE
 20CA BD129D            JSR     VARGET          ; #1 (SCORE/HOURS)
 20CD 0D4E              TST     TIMEFL          ; TIME MODE?
 20CF 2607              BNE     PTIME           ; YES IF NZ
                
 20D1                   ; PRINT SCORE
                
 20D1 BD18D5            JSR     NUMBER          ; PRINT THE VALUE
 20D4 862F              LDA     #$2F            ; ASCII SLASH
 20D6 2013              BRA     MOVEP
                
 20D8                   ; PRINT TIME (HOURS)
                
 20D8 963E      PTIME:  LDA     TEMP+1
 20DA 2602              BNE     PTIME1          ; 00 IS REALLY 24
 20DC 8618              LDA     #24
 20DE 810C      PTIME1: CMPA    #12
 20E0 2F04              BLE     PTIME2          ; IF HOURS IS GREATER THAN 12,
 20E2 800C              SUBA    #12             ; CONVERT TO 12-HOUR TIME
 20E4 973E              STA     TEMP+1
 20E6 BD18D5    PTIME2: JSR     NUMBER          ; SHOW HOURS VALUE
 20E9 863A              LDA     #$3A            ; ASCII COLON
                
 20EB BD1FF3    MOVEP:  JSR     COUT            ; SEND COLON (OR SLASH)
 20EE 8612              LDA     #$12            ; GLOBAL VAR #2 (MOVES/MINUTES)
 20F0 BD129D            JSR     VARGET
 20F3 0D4E              TST     TIMEFL          ; TIME MODE?
 20F5 272E              BEQ     PNUM            ; NO, DO MOVES
                
 20F7                   ; PRINT MINUTES
                
 20F7 963E              LDA     TEMP+1
 20F9 810A              CMPA    #10             ; IF LESS THAN 10 MINUTES,
 20FB 2405              BHS     MOVEP1
 20FD 8630              LDA     #$30            ; ADD ASCII ZERO FOR PADDING
 20FF BD1FF3            JSR     COUT
                
 2102 BD18D5    MOVEP1: JSR     NUMBER          ; SHOW MINUTES
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 76                 
--- SCREEN & PRINTER I/O ---                                                                                                        

 2105                   ; PRINT "AM/PM"
                
 2105 8620              LDA     #SPACE          ; SEPARATE TIMING
 2107 BD1FF3            JSR     COUT            ; FROM "AM/PM"
 210A 8611              LDA     #$11            ; GLOBAL #1 AGAIN
 210C BD129D            JSR     VARGET
 210F 963E              LDA     TEMP+1
 2111 810C              CMPA    #12             ; PAST NOON?
 2113 2404              BHS     USEPM           ; YES, IT'S PM
 2115 8641              LDA     #$41            ; "A"
 2117 2002              BRA     DOM
 2119 8650      USEPM:  LDA     #$50            ; "P"
 211B BD1FF3    DOM:    JSR     COUT
 211E 864D              LDA     #$4D            ; "M"
 2120 BD1FF3            JSR     COUT
 2123 2003              BRA     AHEAD           ; DONE!
                
 2125                   ; PRINT # MOVES
                
 2125 BD18D5    PNUM:   JSR     NUMBER          ; SIMPLE, EH?
                
 2128 BD2066    AHEAD:  JSR     CR1             ; DUMP BUFFER
 212B 8D2B              BSR     INVERT          ; INVERT STATUS LINE
                
 212D 108E1040          LDY     #BUFSAV         ; POINT TO "SAVE" BUFFER
 2131 8E1020            LDX     #BUFFER         ; POINT TO OUTPUT BUFFER
 2134 A6A0      USLEND: LDA     ,Y+
 2136 A780              STA     ,X+             ; RESTORE PREVIOUS CONTENTS
 2138 8C1040            CMPX    #BUFFER+32
 213B 25F7              BLO     USLEND
                
 213D 3536              PULS    X,Y,D           ; RESTORE EVERYTHING
 213F 109F27            STY     CSTEMP
 2142 9F19              STX     MPCM
 2144 D751              STB     BINDEX
 2146 9718              STA     MPCH
 2148 3536              PULS    X,Y,D
 214A 109F2A            STY     ZSTWRD
 214D 9F88              STX     CURSOR
 214F D729              STB     STBYTF
 2151 974F              STA     CHRPNT
 2153 035C              COM     SCRIPT          ; RE-ENABLE SCRIPTING
 2155 0F1D              CLR     MPCFLG          ; MPC NO LONGER VALID
 2157 39                RTS
                
 2158                   ; ------------------
 2158                   ; INVERT STATUS LINE
 2158                   ; ------------------
                
 2158 8E0400    INVERT: LDX     #SCREEN
 215B A684      REVERS: LDA     ,X


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 77                 
--- SCREEN & PRINTER I/O ---                                                                                                        

 215D 84BF              ANDA    #%10111111      ; CLEAR BIT 6 (REVERSE VIDEO)
 215F A780              STA     ,X+
 2161 8C0420            CMPX    #SCREEN+32
 2164 25F5              BLO     REVERS
 2166 39                RTS
                
                        END
                        INCLUD DISK.ASM


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 78                 
--- DISK I/O ---                                                                                                                    

                
 C004           DSKCON  EQU     $C004           ; CONTAINS DISK ACCESS VECTOR
 C006           DCB     EQU     $C006           ; CONTAINS ADDRESS OF DCB
 FF40           MOTOR   EQU     $FF40           ; DRIVE MOTOR ON/OFF SWITCH
 0025           BLOCK1  EQU     37              ; 1ST Z-CODE BLOCK (TRACK 2, SECTOR 1)
                
 2167                   ; ------------------------
 2167                   ; READ A Z-BLOCK FROM DISK
 2167                   ; ------------------------
                
 2167                   ; ENTRY: DRIVE # (0 OR 1) IN [DRIVE]
 2167                   ;        BLOCK # IN [DBLOCK]
 2167                   ;        BUFFER ADDRESS IN [DBUFF]
                
 2167 DC3D      GETDSK: LDD     TEMP            ; SAVE [TEMP] AND
 2169 9E3B              LDX     VAL             ; [VAL] FOR DIVISION
 216B 3416              PSHS    X,D
                
 216D                   ; CONVERT BLOCK # TO SECTOR/TRACK
                
 216D DC4A              LDD     DBLOCK          ; FETCH BLOCK #
 216F C30025            ADDD    #BLOCK1         ; ADD DISK OFFSET
 2172 10830121          CMPD    #16*18+1        ; IF BELOW TRACK 16
 2176 2503              BLO     TRAKZ           ; CONTINUE
 2178 C30024            ADDD    #36             ; ELSE SKIP OVER TRACKS 16 & 17
 217B DD3D      TRAKZ:  STD     TEMP            ; USE AS DIVIDEND
 217D CC0012            LDD     #18
 2180 DD3B              STD     VAL             ; DIVIDE BY 18 (# SECTORS PER TRACK)
 2182 BD17AF            JSR     UDIV            ; UNSIGNED DIVIDE
 2185 963E              LDA     TEMP+1          ; [TEMP] HAS QUOTIENT (TRACK #)
 2187 D63C              LDB     VAL+1           ; [VAL] HAS REMAINDER (SECTOR #)
 2189 2603              BNE     NXTSEC          ; IF REMAINDER WAS ZERO,
 218B C612              LDB     #18             ; CHANGE IT TO 18
 218D 4A                DECA                    ; AND PATCH TRACK #
 218E DD4C      NXTSEC: STD     TRACK
 2190 BD2211            JSR     DREAD           ; ACCESS THE DISK
 2193 0C4B              INC     DBLOCK+1        ; POINT TO NEXT Z-BLOCK
 2195 2602              BNE     REND
 2197 0C4A              INC     DBLOCK
 2199 3516      REND:   PULS    X,D             ; RESTORE VARIABLES
 219B 9F3B              STX     VAL
 219D DD3D              STD     TEMP
 219F 39                RTS
                
 21A0                   ; -----------------
 21A0                   ; SAVE/RESTORE INIT
 21A0                   ; -----------------
                
 21A0 BD2037    SAVRES: JSR     ZCRLF           ; FLUSH OUTPUT BUFFER
 21A3 BD1F1A            JSR     CLS
 21A6 CC0400            LDD     #SCREEN


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 79                 
--- DISK I/O ---                                                                                                                    

 21A9 DD88              STD     CURSOR          ; MOVE CURSOR TO STATUS LINE
 21AB 0F5C              CLR     SCRIPT          ; DISABLE SCRIPTING
 21AD 39                RTS
                
 21AE                   ; ---------
 21AE                   ; SAVE GAME
 21AE                   ; ---------
                
 21AE 8DF0      ZSAVE:  BSR     SAVRES          ; INIT THINGS
 21B0 8E2396            LDX     #SAV
 21B3 C606              LDB     #SAVL           ; "SAVING"
 21B5 BD2384            JSR     DLINE
 21B8 BD2301            JSR     PARAMS          ; GET POSITION AND DRIVE
                
 21BB 8E1040            LDX     #BUFSAV         ; POINT TO AUX BUFFER
 21BE FC2502            LDD     ZCODE+ZID       ; GET GAME ID CODE
 21C1 ED81              STD     ,X++            ; SAVE IN BUFFER
 21C3 DC25              LDD     OZSTAK          ; OLD STACK POINTER
 21C5 ED81              STD     ,X++
 21C7 EF81              STU     ,X++            ; AND CURRENT STACK POINTER
 21C9 9611              LDA     ZPCH            ; HI BYTE OF ZPC
 21CB A780              STA     ,X+
 21CD DC12              LDD     ZPCM            ; LOW ZPC BYTES
 21CF ED84              STD     ,X
                
 21D1 CC1000            LDD     #LOCALS
 21D4 DD48              STD     DBUFF
 21D6 8D26              BSR     DWRITE          ; WRITE LOCAL/BUFFER PAGE
                
 21D8 CC0C00            LDD     #ZSTACK         ; SAVE CONTENTS
 21DB DD48              STD     DBUFF           ; OF Z-STACK (2 PAGES)
 21DD 8D1F              BSR     DWRITE          ; FIRST HALF
 21DF 8D1D              BSR     DWRITE          ; 2ND HALF
                
 21E1                   ; SAVE GAME PRELOAD
                
 21E1 CC2500            LDD     #ZCODE          ; START OF PRELOAD
 21E4 DD48              STD     DBUFF
 21E6 B6250E            LDA     ZCODE+ZPURBT    ; SIZE OF PRELOAD (MSB, # PAGES)
 21E9 4C                INCA                    ; ROUND UP
 21EA 973D              STA     TEMP            ; USE [TEMP] AS INDEX
                
 21EC 8D10      LSAVE:  BSR     DWRITE          ; SAVE A PAGE
 21EE 0A3D              DEC     TEMP            ; SAVED ENTIRE PRELOAD YET?
 21F0 26FA              BNE     LSAVE           ; NO, KEEP SAVING
 21F2 7E22D8            JMP     RESUME
                
 21F5                   ; *** ERROR #12: DISK ADDRESS RANGE ***
                
 21F5 860C      DSKERR: LDA     #12
 21F7 2002              BRA     DSKEX


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 80                 
--- DISK I/O ---                                                                                                                    

                
 21F9                   ; *** ERROR #14: DISK ACCESS ***
                
 21F9 860E      DERR2:  LDA     #14
 21FB BD1FA9    DSKEX:  JSR     ZERROR
                
 21FE                   ; ------------
 21FE                   ; ACCESS DRIVE
 21FE                   ; ------------
                
 21FE                   ; ENTRY: [DBUFF] HOLDS BUFFER ADDRESS
 21FE                   ;        [TRACK] HOLDS TRACK, SECTOR ADDRESS
 21FE                   ;        [DRIVE] HOLDS DRIVE #
                
 21FE 8E0B00    DWRITE: LDX     #IOBUFF         ; POINT TO DISK I/O BUFFER
 2201 109E48            LDY     DBUFF           ; AND RAM PAGE TO BE WRITTEN
 2204 ECA1      DRLOOP: LDD     ,Y++            ; GRAB A WORD OUT OF RAM
 2206 ED81              STD     ,X++            ; MOVE IT INTO THE BUFFER
 2208 8C0C00            CMPX    #IOBUFF+$100    ; BUFFER FILLED YET?
 220B 25F7              BLO     DRLOOP          ; NO, KEEP MOVING
 220D 8603              LDA     #3              ; "WRITE SECTOR" COMMAND
 220F 2002              BRA     DIO
                
 2211 8602      DREAD:  LDA     #2              ; "READ SECTOR" COMMAND
                
 2213 BD1D78    DIO:    JSR     ROMIN           ; ENABLE ROMS
 2216 BEC006            LDX     DCB             ; GET ADDR OF DCB FROM ROM
 2219 D647              LDB     DRIVE           ; DRIVE # (0 OR 1)
 221B ED84              STD     ,X              ; PASS TO [DSKCON]
 221D DC4C              LDD     TRACK           ; TRACK & SECTOR ADDRESSES
 221F 8123              CMPA    #35
 2221 24D2              BHS     DSKERR          ; NO TRACKS HIGHER THAN 34
 2223 C113              CMPB    #19
 2225 24CE              BHS     DSKERR          ; OR SECTORS HIGHER THAN 18
 2227 ED02              STD     2,X             ; PASS IT
 2229 CC0B00            LDD     #IOBUFF         ; BUFFER ADDRESS
 222C ED04              STD     4,X
                
 222E                   ; ACCESS THE DRIVE
                
 222E AD9FC004          JSR     [DSKCON]        ; LET OS DO THE DIRTY WORK
 2232 BD1D92            JSR     ROMOUT          ; THEN TURN ROMS OFF AGAIN
                
 2235                   ; CHECK FOR ACCESS ERRORS
                
 2235 A606              LDA     6,X             ; GET STATUS BYTE
 2237 8540              BITA    #%01000000      ; WRITE-PROTECT ERROR?
 2239 2621              BNE     WPERR           ; YES, GO REPORT IT
 223B 4D                TSTA                    ; ANY OTHER ERRORS?
 223C 26BB              BNE     DERR2           ; ERROR IF ANY BIT SET
                


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 81                 
--- DISK I/O ---                                                                                                                    

 223E 8E0B00            LDX     #IOBUFF         ; MOVE CONTENTS OF I/O BUFFER
 2241 109E48            LDY     DBUFF           ; TO DESIRED RAM ADDRESS
 2244 EC81      QLOOP:  LDD     ,X++
 2246 EDA1              STD     ,Y++
 2248 8C0C00            CMPX    #IOBUFF+$100
 224B 25F7              BLO     QLOOP
                
 224D 0C48              INC     DBUFF           ; POINT TO NEXT PAGE OF RAM
 224F DC4C              LDD     TRACK           ; AND NEXT SECTOR
 2251 5C                INCB
 2252 C113              CMPB    #19
 2254 2503              BLO     DIOEX
 2256 C601              LDB     #1
 2258 4C                INCA
 2259 DD4C      DIOEX:  STD     TRACK
 225B 39                RTS
                
 225C                   ; -------------------
 225C                   ; WRITE-PROTECT ERROR
 225C                   ; -------------------
                
 225C 3506      WPERR:  PULS    D               ; PULL RETURN ADDRESS OFF STACK
 225E 2034              BRA     ERRWP           ; PROMPT FOR GAME DISK
                
 2260                   ; ------------
 2260                   ; RESTORE GAME
 2260                   ; ------------
                
 2260 BD21A0    ZREST:  JSR     SAVRES
 2263 8E238D            LDX     #RES
 2266 C609              LDB     #RESL
 2268 BD2384            JSR     DLINE           ; "RESTORING"
 226B BD2301            JSR     PARAMS
                
 226E                   ; SAVE LOCALS ON MACHINE STACK
 226E                   ; IN CASE OF ERROR
                
 226E 8E1000            LDX     #LOCALS         ; POINT TO LOCALS STORAGE
 2271 9F48              STX     DBUFF           ; POINT TO 1ST PAGE TO RESTORE
 2273 EC81      LOCLP:  LDD     ,X++            ; GRAB A LOCAL
 2275 3406              PSHS    D               ; AND PUSH IT
 2277 8C101E            CMPX    #LOCALS+30      ; SAVED 15 LOCALS YET?
 227A 25F7              BLO     LOCLP           ; NO, KEEP PUSHING
                
 227C BD2211            JSR     DREAD           ; RETRIEVE LOCALS/BUFFER PAGE
                
 227F FC1040            LDD     BUFSAV          ; READ SAVED GAME ID
 2282 10B32502          CMPD    ZCODE+ZID       ; IF IT MATCHES CURRENT GAME ID,
 2286 2711              BEQ     VERSOK          ; PROCEED WITH THE RESTORE
                
 2288                   ; WRONG SAVE DISK, ABORT RESTORE


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 82                 
--- DISK I/O ---                                                                                                                    

                
 2288 8E101E            LDX     #LOCALS+30      ; RESTORE PUSHED LOCALS
 228B 3506      RESLP:  PULS    D
 228D ED83              STD     ,--X
 228F 8C1000            CMPX    #LOCALS
 2292 22F7              BHI     RESLP
 2294 8D47      ERRWP:  BSR     TOBOOT          ; PROMPT FOR GAME DISK
 2296 7E131E            JMP     PREDF           ; PREDICATE FAILS
                
 2299 32E81E    VERSOK: LEAS    +30,S           ; POP OLD LOCALS OFF STACK
 229C FC2510            LDD     ZCODE+ZSCRIP
 229F DD3B              STD     VAL             ; SAVE FLAGS
                
 22A1 CC0C00            LDD     #ZSTACK         ; RETRIEVE
 22A4 DD48              STD     DBUFF           ; CONTENTS OF Z-STACK
 22A6 BD2211            JSR     DREAD
 22A9 BD2211            JSR     DREAD
                
 22AC CC2500    DOREST: LDD     #ZCODE          ; NOW RETRIEVE
 22AF DD48              STD     DBUFF           ; 1ST PAGE OF PRELOAD
 22B1 BD2211            JSR     DREAD
                
 22B4 B6250E            LDA     ZCODE+ZPURBT    ; DETERMINE # PAGES
 22B7 973D              STA     TEMP            ; TO RETRIEVE
                
 22B9 BD2211    LREST:  JSR     DREAD           ; FETCH REMAINDER OF PRELOAD
 22BC 0A3D              DEC     TEMP
 22BE 26F9              BNE     LREST
                
 22C0                   ; RESTORE STATE OF SAVED GAME
                
 22C0 8E1042            LDX     #BUFSAV+2       ; POINT TO SAVED VARIABLES
 22C3 EC81              LDD     ,X++
 22C5 DD25              STD     OZSTAK          ; RESTORE OLD STACK POINTERS
 22C7 EE81              LDU     ,X++
 22C9 A680              LDA     ,X+
 22CB 9711              STA     ZPCH            ; HIGH BYTE OF ZPC
 22CD EC84              LDD     ,X              ; LOW BYTES OF ZPC
 22CF DD12              STD     ZPCM
 22D1 0F16              CLR     ZPCFLG          ; PC HAS CHANGED!
                
 22D3 DC3B              LDD     VAL             ; RESTORE FLAGS
 22D5 FD2510            STD     ZCODE+ZSCRIP
                
 22D8                   ; RESUME GAME AFTER SAVE OR RESTORE
                
 22D8 8D03      RESUME: BSR     TOBOOT          ; PROMPT FOR GAME DISK
 22DA 7E132C            JMP     PREDS           ; PREDICATE SUCCEEDS
                
 22DD 0F47      TOBOOT: CLR     DRIVE           ; BACK TO BOOT DRIVE
 22DF 8E23BA            LDX     #GAME


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 83                 
--- DISK I/O ---                                                                                                                    

 22E2 C61E              LDB     #GAMEL
 22E4 BD2384            JSR     DLINE           ; "INSERT GAME DISK IN DRIVE 0,"
 22E7 BD22EF            JSR     ENTER           ; "PRESS ANY KEY TO CONTINUE"
 22EA 035C              COM     SCRIPT          ; RE-ENABLE SCRIPTING
 22EC 7E1F1A            JMP     CLS             ; CLEAR SCREEN AND RETURN
                
 22EF 8E23D8    ENTER:  LDX     #PRESS
 22F2 C61A              LDB     #PRESSL
 22F4 D75B              STB     CFLAG           ; ENABLE CURSOR
 22F6 BD1F0A            JSR     LINE            ; "PRESS ANY KEY TO CONTINUE"
 22F9 BD1DB1            JSR     GETKEY          ; GET A KEY
 22FC 860D              LDA     #EOL
 22FE 7E1FF3            JMP     COUT            ; DO EOL AND RETURN
                
 2301                   ; --------------------------------
 2301                   ; PROMPT SEQUENCE FOR SAVE/RESTORE
 2301                   ; --------------------------------
                
 2301 8E23F2    PARAMS: LDX     #POSIT
 2304 C614              LDB     #POSITL
 2306 BD2384            JSR     DLINE           ; "GAME ... POSITION 0-4 "
 2309 BD2158            JSR     INVERT          ; INVERT THE STATUS LINE
                
 230C                   ; GET POSITION
                
 230C 9655              LDA     GPOSIT
 230E BD2378            JSR     DODEF
 2311 BD1D8A    GETPOS: JSR     CATPOL
 2314 810D              CMPA    #EOL
 2316 270B              BEQ     SETPOS
 2318 8030              SUBA    #$30            ; CONVERT TO BINARY
 231A 8105              CMPA    #5              ; IF LOWER THAN "5"
 231C 2507              BLO     POSSET          ; SET NEW POSITION
 231E BD1F49            JSR     BOOP            ; ELSE RAZZ
 2321 20EE              BRA     GETPOS          ; AND TRY AGAIN
                
 2323 9655      SETPOS: LDA     GPOSIT          ; USE DEFAULT
 2325 9755      POSSET: STA     GPOSIT          ; NEW DEFAULT
 2327 8B30              ADDA    #$30            ; CONVERT TO ASCII
 2329 BD1DA4            JSR     OUTCHR          ; SHOW CHOICE
 232C 860D              LDA     #EOL
 232E BD1DA4            JSR     OUTCHR          ; SEND EOL
                
 2331 9655              LDA     GPOSIT          ; GET GAME POSITION
 2333 C606              LDB     #6              ; CALC BLOCK OFFSET (6 TRACKS/GAME)
 2335 3D                MUL
 2336 D74C              STB     TRACK           ; TRACK ADDRESS
 2338 C601              LDB     #1              ; START ON SECTOR 1
 233A D74D              STB     TRACK+1         ; SECTOR ADDRESS
                
 233C                   ; GET DRIVE #


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 84                 
--- DISK I/O ---                                                                                                                    

                
 233C 8E2406            LDX     #WDRIV
 233F C60D              LDB     #WDRIVL
 2341 BD2384            JSR     DLINE           ; "DRIVE 0 OR 1 "
 2344 9654              LDA     GDRIVE
 2346 8D30              BSR     DODEF           ; SHOW DEFAULT
                
 2348 BD1D8A    GETDRV: JSR     CATPOL
 234B 810D              CMPA    #EOL
 234D 270B              BEQ     DRVSET
 234F 8030              SUBA    #$30            ; CONVERT TO ASCII
 2351 8102              CMPA    #2
 2353 2507              BLO     SETDRV
 2355 BD1F49            JSR     BOOP
 2358 20EE              BRA     GETDRV          ; DRIVE # NO GOOD
                
 235A 9654      DRVSET: LDA     GDRIVE
 235C 9754      SETDRV: STA     GDRIVE          ; NEW DEFAULT
 235E 9747              STA     DRIVE
 2360 8B30              ADDA    #$30            ; CONVERT TO ASCII
 2362 B723B7            STA     GAMDRI          ; FOR PROMPT
 2365 BD1DA4            JSR     OUTCHR          ; SHOW CHOICE
 2368 860D              LDA     #EOL
 236A BD1DA4            JSR     OUTCHR
                
 236D 8E239C            LDX     #INSERM
 2370 C61E              LDB     #INSERML
 2372 BD2384            JSR     DLINE           ; "INSERT SAVE DISK IN DRIVE X,"
 2375 7E22EF            JMP     ENTER           ; ETC.
                
 2378                   ; ------------
 2378                   ; SHOW DEFAULT
 2378                   ; ------------
                
 2378 8B30      DODEF:  ADDA    #$30            ; CONVERT # TO ASCII
 237A B7241E            STA     DEFNUM          ; INSERT IN STRING
 237D 8E2413            LDX     #DEFALT
 2380 C60E              LDB     #DEFALL
 2382 D75B              STB     CFLAG           ; ENABLE CURSOR
                
 2384                   ; FALL THROUGH TO ...
                
 2384                   ; --------------------
 2384                   ; DIRECT SCREEN OUTPUT
 2384                   ; --------------------
                
 2384                   ; ENTRY: SAME AS "LINE" ROUTINE
                
 2384 A680      DLINE:  LDA     ,X+
 2386 BD1DA4            JSR     OUTCHR
 2389 5A                DECB


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC. --- COCO MACHINE DEPENDENT                                                           PAGE 85                 
--- DISK I/O ---                                                                                                                    

 238A 26F8              BNE     DLINE
 238C 39                RTS
                
 238D                   ; ---------------------
 238D                   ; TEXT FOR SAVE/RESTORE
 238D                   ; ---------------------
                
 238D 524553544FRES:    DB      "RESTORING"
 0009           RESL    EQU     $-RES
                
 2396 534156494ESAV:    DB      "SAVING"
 0006           SAVL    EQU     $-SAV
                
 239C 0D        INSERM: DB      EOL
 239D 494E534552        DB      "INSERT SAVE DISK IN DRIVE 0,"
 23B9 0D                DB      EOL
 23B7           GAMDRI  EQU     $-3
 001E           INSERML EQU     $-INSERM
                
 23BA 0D        GAME:   DB      EOL
 23BB 494E534552        DB      "INSERT GAME DISK IN DRIVE 0,"
 23D7 0D                DB      EOL
 001E           GAMEL   EQU     $-GAME
                
 23D8 5052455353PRESS:  DB      "PRESS ANY KEY TO CONTINUE"
 23F1 0D                DB      EOL
 001A           PRESSL  EQU     $-PRESS
                
 23F2 2047414D45POSIT:  DB      " GAME"                 ; TAIL END OF TITLE
 23F7 0D                DB      EOL
 23F8 0D                DB      EOL
 23F9 504F534954        DB      "POSITION 0-4 "
 0014           POSITL  EQU     $-POSIT
                
 2406 4452495645WDRIV:  DB      "DRIVE 0 OR 1 "
 000D           WDRIVL  EQU     $-WDRIV
                
 2413 2844454641DEFALT: DB      "(DEFAULT = 0):"
 241E           DEFNUM  EQU     $-3
 000E           DEFALL  EQU     $-DEFALT
                
 2421 454E44            DB      "END"
                
                        END
                
 2424                   IF DEBUG
 2424                   INCLUD BUGGER.ASM
 2424                   ENDIF
                
 0000                   END


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC.                                                                                      PAGE 86                 
---- SYMBOL TABLE ----                                                                                                              

ABSVAL   17A1          COUT1    2008          ENDSL    000F          GETVRL   12AF          MOVEP    20EB
ABTEMP   17A8          CPL      1D8C          ENTER    22EF          GETWRD   1AC1          MOVEP1   2102
AHEAD    2128          CPSAV    0050          EOL      000D          GETZCH   1BD9          MPCFLG   001D
AINIT    1F30          CR1      2066          EQBAD    1802          GETZH1   1BF0          MPCH     0018
ARG1     0002          CSPERM   0028          EQOK     1805          GETZH2   1C07          MPCL     001A
ARG2     0004          CSTEMP   0027          ERRWP    2294          GETZH3   1C03          MPCM     0019
ARG3     0006          CURSOR   0088          ETPEX    16F7          GK       1DC0          MPCPNT   001B
ARG4     0008          CYCLE    0058          FALSE    0000          GLOBAL   001F          MSTACK   0AFE
ARGCNT   0001          DBLOCK   004A          FBRK     1A1E          GM       1BD4          MSTART   0A00
BADKEY   1DAC          DBUFF    0048          FINDPG   1AD7          GOBACK   1E7B          MTEMP    0045
BADOP1   1233          DCB      C006          FINDW    1A20          GOCHR    1EF7          NBR1     1A15
BADOP2   1281          DEBUG    0000          FIRST1   14B5          GPOSIT   0055          NBRKP    1A09
BDURAT   000A          DECX     17CE          FLAGSU   1D3B          GPT      1715          NBRKS    0006
BFREQ    01F4          DEFALL   000E          FLASH    0180          GTBT     1A8F          NEWM     1AA3
BINDEX   0051          DEFALT   2413          FLEX     1A04          GTEXIT   1BEC          NEWMPG   1AB7
BLINK    005A          DEFNUM   241E          FLGSU1   1D4E          GTVX     12B6          NEWZ     1A6D
BLOCK1   0025          DELAY    1F63          FLGSU2   1D59          GTZ0     1BDF          NEWZPG   1A81
BOOP     1F49          DELOOP   1F66          FLGSU3   1D62          IERRL    0011          NEXTPC   1A55
BOOP1    1F4F          DERR2    21F9          FLUSH    2009          IHOLD    005D          NOBLIN   1DEB
BRKTBL   1A4F          DEVNUM   006F          FLUSH1   200B          INLOOP   1E4B          NOMORE   1E6B
BS       0008          DGC      18E3          FLUSH2   2014          INMPRE   1AB3          NOPS0    000E
BUFFER   1020          DIGCNT   18E1          FLUSH3   201B          INPEX    1EA8          NOPS1    0010
BUFOUT   1F05          DIO      2213          FLUSH4   2024          INPUT    1E39          NOPS2    0019
BUFSAV   1040          DIOEX    2259          FLUSHW   19EE          INSERM   239C          NOPSX    000C
CATPOL   1D8A          DISPX    1203          FP1      1ADC          INSERML  001E          NORAM    114D
CDEL     1E34          DIVERR   17DB          FREEZE   1FD0          INT60    FF03          NOSCRL   1EEF
CDELAY   1E31          DIVEX    17A7          FWL1     1A2D          INTERR   1F98          NPC1     1A59
CDURAT   0005          DIVIDE   1781          FWORDS   0023          INVERT   2158          NUMBER   18D5
CEXIT    15EC          DLINE    2384          FWSUCC   1A48          INZPRE   1A7D          NXTM     1AA2
CFLAG    005B          DOB2     1346          GAMDRI   23B7          IOBUFF   0B00          NXTP1    1727
CFREQ    0019          DOCALL   180F          GAME     23BA          IOCHAR   0053          NXTP2    173A
CHAR     1EAF          DODEF    2378          GAMEL    001E          KTEST    1DE1          NXTP3    1737
CHROUT   A002          DODIS    1206          GDRIVE   0054          LDPRE    1187          NXTSEC   218E
CHRPNT   004F          DOEQ     17E9          GETBYT   1A8B          LINCNT   0052          NXTX     1A6C
CHRTBL   1CEC          DOM      211B          GETDRV   2348          LINE     1F0A          OBJLOC   1D69
CLICK    1E12          DOREST   22AC          GETDSK   2167          LINOUT   2071          OP0      120D
CLS      1F1A          DOSCRL   1EC4          GETKEY   1DB1          LN       1F0D          OP1      121D
CLS0     1F1F          DOUDIV   1795          GETLNG   128E          LOADM    1F88          OP1A     1226
CNLOOP   1CA5          DREAD    2211          GETMOD   1BCD          LOADML   0010          OP1B     122F
CNOK     1CAD          DRIVE    0047          GETP1    16BB          LOCALS   1000          OP1C     1238
CNZS2M   1CA0          DRLOOP   2204          GETP2    16DA          LOCLP    2273          OP1EX    123B
CNZSC1   1C66          DRVSET   235A          GETP2A   16F1          LREST    22B9          OP2      124E
CNZSC3   1C6E          DSKCON   C004          GETP2B   16EA          LRU      000A          OP2A     1257
CNZSL1   1C21          DSKERR   21F5          GETP3    16C9          LSAVE    21EC          OP2B     125A
CNZSL2   1C32          DSKEX    21FB          GETPOS   2311          MASK     0041          OP2C     126B
CNZSLC   1C4B          DSTART   0000          GETPT1   16FF          MATH     1744          OP2D     126E
CNZSLE   1CCF          DVINIT   1779          GETPT2   170F          MEMTOP   00FE          OP2EX    1274
CNZSLO   1C57          DWRITE   21FE          GETSHT   1286          MLOOP    1198          OPCODE   0000
COLD     1F73          ENDLIN   1E84          GETVAR   12A5          MOREL    0006          OPEXT    11B2
CONZST   1C11          ENDSCR   0600          GETVR1   12AB          MORES    2031          OPT0     1391
COUT     1FF3          ENDSES   1FD2          GETVRG   12BC          MOTOR    FF40          OPT1     13AD


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC.                                                                                      PAGE 87                 
---- SYMBOL TABLE ----                                                                                                              

OPT2     13CD          PTABLE   0E00          RL2      1968          UDLOOP   17BA          ZGETB    16A5
OPTX     13FF          PTIME    20D8          RL3      1971          UDNEXT   17CD          ZGETP    16B8
OPX0     11BB          PTIME1   20DE          ROMIN    1D78          UNDER    1319          ZGETPT   16FC
OPX1     11C1          PTIME2   20E6          ROMOFF   FFDF          USEPM    2119          ZGLOBA   000C
OPX2     11CA          PTP      18A7          ROMON    FFDE          USLEND   2134          ZGRTR    15CE
OPX3     11D3          PTP1     18BC          ROMOUT   1D92          VAL      003B          ZID      0002
OPX4     11F0          PTVX     12F0          RSIBRK   19B7          VARGET   129D          ZIGRTR   15E1
OPXNXT   11DA          PUTBYT   12D3          RTABP    0038          VARPUT   12C5          ZIN      1604
OUTCHR   1DA4          PUTP1    1899          SAV      2396          VCODE    1FE0          ZINC     14EE
OUTEOL   1EFD          PUTP2    18AC          SAVL     0006          VCODEL   000B          ZINC1    14F8
OUTPUT   2075          PUTP2A   18C4          SAVRES   21A0          VERNUM   1FEB          ZIP      1100
OVER     1315          PUTVAL   12D7          SCDONE   207A          VERSOK   2299          ZJUMP    159F
OZSTAK   0025          PUTVLG   12F6          SCMP     15FE          VOCAB    0021          ZLENTH   001A
PAGE0    000E          PUTVLL   12E9          SCOMP    15F3          VSUM     1471          ZLESS    15C4
PAGEM    1AAC          PUTVR1   12E5          SCREEN   0400          VSUM0    147D          ZLOC     14CC
PAGEZ    1A76          PZERO    1907          SCRIPT   005C          WDRIV    2406          ZMLOOP   1756
PARAMS   2301          PZSTFO   003A          SCRL1    1ED2          WDRIVL   000D          ZMNEXT   1766
PATCH    1AA9          PZSTP0   1B40          SCRL2    1ED4          WINK     1DB5          ZMOD     1773
PBAD     15F0          PZSTP1   1B42          SCRL3    1EE7          WNEXT    1A3B          ZMODE    0001
PDB0     1378          PZSTR    1B1F          SCROUT   1E9F          WPERR    225C          ZMOVE    166C
PDB1     1390          PZSTR1   1B47          SENDCH   1E5C          ZADD     1740          ZMUL     174F
PFOUND   1B0B          PZSTR2   1B4F          SETDRV   235C          ZB0      1627          ZMVEX    1691
PKICK    1E0A          PZSTRA   1B5C          SETNP    1151          ZBAND    162C          ZNEXT    14A5
PMAX     000C          PZSTRF   1B8B          SETPOS   2323          ZBCOM    15BB          ZNEXTP   1720
PNBX     132B          PZSTRL   1B27          SETSTR   1B10          ZBEGIN   0006          ZNOOP    143F
PNUM     2125          PZSTRP   1B81          SETWRD   1ACE          ZBOR     1621          ZOBJEC   000A
POK      1611          PZSTRS   1B71          SIBRKP   1A10          ZBTST    1614          ZPAGE    000D
POLCAT   A000          PZSTRT   1B75          SPACE    0020          ZCALL    1808          ZPCFLG   0016
POPSTK   130A          QKEY     1DF6          SPCS     205A          ZCALL1   1838          ZPCH     0011
POSIT    23F2          QLOOP    2244          SQUOT    0043          ZCALL2   1853          ZPCL     0013
POSITL   0014          RAND1    0056          SREM     0044          ZCALL3   1857          ZPCM     0012
POSSET   2325          RAND2    0057          ST0      111E          ZCALL4   186E          ZPCPNT   0014
PPX      1D34          RBRKP    1A05          ST1      112A          ZCHKSM   001C          ZPGTOP   005E
PREDB    1332          RDEX     1967          STABP    0039          ZCHR1    1CBC          ZPOP     192B
PREDB1   1351          READL    1956          START    1117          ZCHR2    1CC7          ZPRB     1512
PREDB3   135E          READL2   1999          STBYTF   0029          ZCHRCS   1CB2          ZPRC     18CC
PREDF    131E          READL3   19BF          STERR    131B          ZCHRX    1CCE          ZPRD     155C
PREDLB   133E          REMVC1   1540          TABS     178F          ZCODE    2500          ZPRI     1422
PREDNB   1324          REMVC2   1555          TABTOP   000F          ZCRLF    2037          ZPRINT   15A9
PREDS    132C          REMVEX   155B          TEMP     003D          ZDEC     1506          ZPRN     18D1
PRESS    23D8          REND     2199          TEMP2    003F          ZDIV     176E          ZPRR     1440
PRESSL   001A          RES      238D          TICK     1E19          ZDLESS   15D8          ZPTSIZ   14DC
PRNTDC   155E          RESL     0009          TIMEFL   004E          ZENDLD   0004          ZPURBT   000E
PRNTN3   18F7          RESLP    228B          TOBOOT   22DD          ZEQUAL   17E0          ZPURE    000B
PRNTN4   18FB          RESUME   22D8          TOPRIN   1E8B          ZERROR   1FA9          ZPUSH    1926
PROPB    1D06          RET0     12D2          TOPSTA   0DFE          ZFCLR    1653          ZPUT     1876
PROPL    1D23          RET1     157E          TOSCR    1E5F          ZFIRST   14AE          ZPUTB    1888
PROPN    1D1B          RET2     1587          TRACK    004C          ZFSET    1645          ZPUTP    1896
PROPNX   1D30          REVERS   215B          TRAKZ    217B          ZFSETP   1634          ZQUIT    1FC2
PSHDZ    1301          RFLIP    179D          TRUE     00FF          ZFWORD   0018          ZRAND    190C
PSHSTK   12FF          RL1      1963          UDIV     17AF          ZGET     1692          ZREAD    1933


AVOCET SYSTEMS 6809 CROSS-ASSEMBLER -  VERSION 1.09C

ZIP/6809 INFOCOM, INC.                                                                                      PAGE 88                 
---- SYMBOL TABLE ----                                                                                                              

ZREMOV   151C          ZRTRUE   1417          ZSL      20AA          ZSTBUO   0032          ZVALUE   15B3
ZREST    2260          ZSAVE    21AE          ZSPLIT   143F          ZSTEX    1B1E          ZVER     144F
ZRET     1570          ZSCRIP   0010          ZSTACK   0C00          ZSTWRD   002A          ZVERS    0000
ZRFALS   141F          ZSCRN    143F          ZSTAKL   00FF          ZSUB     1749          ZVOCAB   0008
ZRSTAK   1447          ZSERIA   0012          ZSTART   1F6B          ZUSL     207B          ZZERO    149C
ZRT      1419          ZSET     1663          ZSTBUI   002C          ZUSL1    209A          
