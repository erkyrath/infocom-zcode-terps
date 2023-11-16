
PROGRAM STFTP;	     {FILE SENDING UTILITY FOR MACINTOSH}

(* This utility can transmit files in either of two formats, binary or picture.
The user is asked to choose the format before starting each transmission.

Binary format sends data straight from the file.  The length of the file should
be a multiple of 256, as with a file received by the Macintosh TFTP utility.

Picture format expects a MacPaint picture file in packed (compressed) format.
It unpacks the picture and sends a subsection which is the size of a standard
graphics screen (40 x 24 units, each 8 x 8 pixels).  The subsection send is
currently the upper left section of the default MacPaint window frame.
*)

{$DECL BUG}
{$SETC BUG = -1}      {-1 IS NO DEBUGGING, 1 WRITES TO EXTERNAL TERMINAL}

USES  {$U-}
      {$U Obj/Memtypes  } Memtypes,
      {$U OBJ/QUICKDRAW } QUICKDRAW,
      {$U OBJ/OSINTF    } OSINTF,
      {$U OBJ/TOOLINTF  } TOOLINTF,
      {$U OBJ/PACKINTF  } PACKINTF;

{$IFC BUG > -1}
  {$D+} {Put the procedures name just after it in the code, to help in debugging}
  {$R+} {Turn on range checking.  Violating the range at runtime will produce a
         check exception.}
{$ELSEC}
  {$D-} {Do not include the procedure name in the 'production' code}
{$ENDC}

CONST CHCMD=126;        {COMMAND PREFIX BYTE}
      CHDTA=0;
      CHEOF=4;
      CHACK=2;
      CHNAK=5;

{FOR PACKED PICTURE DATA,
 BIGGER MAKES IT FASTER BUT INCREASES RISK OF EOF READ ERROR}
      SRCBLOCKS = 2;
      SRCSIZE = 512 * SRCBLOCKS;

{THIS IS THE POSITION (IN BYTES) OF THE DEFAULT MACPAINT WINDOW}
      VOFFSET = 15;  {SCANLINE OFFSET = VOFFSET x 8}
      HOFFSET = 10;

TYPE TYPE264 = PACKED ARRAY[0..263] OF 0..255;	    {STFTP OUTPUT BUFFER TYPE}
TYPE TYPE512 = PACKED ARRAY[1..512] OF QDBYTE;

TYPE ProcOrFunc = (proc, func, neither);

VAR FILENAME,		       {TFTP globals}
    VOLNAME:    STR255;
    DUMMYBYTES: LONGINT;
    FILEREFNUM,
    VREFNUM,
    INREFNUM,
    OUTREFNUM,
    IO,
    APPLECOUNT: INTEGER;       {Output display counter}
    CH:	        CHAR;
    ZPARAMS:    OPPARAMTYPE;

    ZPICTURE,		       {picture file or binary file}
    STARTPIC:   BOOLEAN;

    SCANLINE:   INTEGER;
    SRCBUF: ARRAY [1..SRCBLOCKS] OF TYPE512;
    SRCPTR: PTR;

    debug: BOOLEAN;
    debugger: TEXT;
    lf: CHAR;


VAR screenRect,			  {Macintosh housekeeping globals}
    dragRect,
    pRect:	  Rect;
    myEvent:	  EventRecord;
    code,
    refNum:	  INTEGER;
    wRecord:	  WindowRecord;
    myWindow,
    whichWindow:  WindowPtr;
    theMenu,
    theItem:	  INTEGER;
    hTE:	  TEHandle;

PROCEDURE DebugInProc (prockind: ProcOrFunc; where: str255; location: ptr);
{This procedure writes the executing routine's name and location in memory on the
 external terminal.}
BEGIN
  Write (debugger, 'in ');
  IF prockind = proc THEN Write (debugger, 'Procedure ');
  IF prockind = func THEN Write (debugger, 'Function ');
  Writeln (debugger, where, ' @ ', ord4(location), lf);
END;

PROCEDURE ioInit;		  {Macintosh initializations}
BEGIN
  InitGraf(@thePort);
  InitFonts;
  FlushEvents(everyEvent,0);
  InitWindows;
  TEInit;
  InitDialogs(NIL);
  InitCursor;

  screenRect := screenBits.bounds;
  SetRect(dragRect,4,24,screenRect.right-4,screenRect.bottom-4);

  myWindow := GetNewWindow(256,@wRecord,POINTER(-1));
  SetPort(myWindow);

  pRect := thePort^.portRect;
  InsetRect(pRect,4,0);
  hTE := TENew(pRect,pRect);
END;

FUNCTION ttyIn: CHAR;   {Get a key, return it without echo}
VAR  doneFlag,
     temp:	    BOOLEAN;
     downKey:	    CHAR;

BEGIN
  doneFlag := FALSE;
  REPEAT
    SystemTask;
    TEIdle(hTE);

    temp := GetNextEvent(everyEvent, myEvent);
    CASE myEvent.what OF

      keyDown, autoKey:
        IF myWindow = FrontWindow THEN
           BEGIN
           downKey := CHR(myEvent.message MOD 256);
           ttyIn := downKey;
           doneFlag := TRUE;	  {exit the loop}
           END;

      diskEvt:		   {disk inserted and volume mounted}
        BEGIN
        ttyIn := CHR (0);
        doneFlag := TRUE;
        END;

      updateEvt:
        BEGIN
        SetPort (myWindow);
        BeginUpdate (myWindow);
        TEUpdate (thePort^.portRect,hTE);  {redraw the erased part of the window}
        EndUpdate (myWindow);
        END;

      END;    { of event case }

  UNTIL doneFlag;
END;

PROCEDURE ttyOut (Ch: CHAR; firstChar: INTEGER);
VAR okay: BOOLEAN;
BEGIN
  okay := TRUE;
  WITH hTE^^ DO
    BEGIN
    IF ORD (Ch) = 8 THEN	   {backspace?}
      BEGIN
      IF selStart <= firstChar
        THEN okay := FALSE	   {yes, back too far, don't allow it}
      END

    ELSE IF selStart < firstChar
        THEN okay := FALSE;	   {no, but don't allow this either}

    IF okay
      THEN TEKey (Ch, hTE)	   {okay, print the char}
      ELSE
        BEGIN
        sysBeep (3);			        {not okay, sound beeper}
        TESetSelect (TELength, TELength, hTE);  {and move cursor to end}
        END;
    END;
END;

(*  Dead, replaced by GETNAME ...

PROCEDURE ReadString (VAR mystring: STR255; default: STR255);
TYPE TXT = PACKED ARRAY [0..32000] of 0..255;
VAR txtptr:    ^TXT;
    firstChar,
    nextChar,
    lastChar:  INTEGER;
    dummy:     STR255;
    ch:	       CHAR;

BEGIN
  firstChar := hTE^^.TELength;
  REPEAT
    ch := ttyIn;	       {get a char}
    ttyOut (ch, firstChar);    {echo it, checking for legality}
  UNTIL ch = CHR (13);
  lastChar := hTE^^.TELength - 1;    {the last char is the CR}

  IF firstChar = lastChar
    THEN mystring := default   {if nothing was typed, use default string}
    ELSE
      BEGIN
      mystring := '';	       {initialize mystring = empty string}
      dummy := 'x';	       {dummy string has constant length = one char}

      txtptr := POINTER (hTE^^.hText^);
      nextChar := firstChar;

      REPEAT
        dummy[1] := CHR (txtptr^[nextChar]);
        mystring := CONCAT (mystring, dummy);  {this hack is blessed by Apple ...}
        nextChar := nextChar + 1;
      UNTIL nextChar = lastChar;	       {ignore the final CR}
      END;
END;
*)

PROCEDURE GetName (myPic: BOOLEAN; VAR myName: STR255; VAR myVolume: INTEGER);
  VAR where: POINT;
      numTypes: INTEGER;
      typeList: SFTypeList;
      reply: SFReply;
BEGIN
  where.h := 80; where.v := 100;
  IF myPic
    THEN			 {filter out all but picture files}
      BEGIN
      typeList [0] := 'PNTG';	 {creator was 'MPNT', by the way}
      numTypes := 1;
      END
    ELSE numTypes := -1;	 {perform no filtering}

  SFGetFile (where, '', NIL, numTypes, typeList, NIL, reply);

  IF reply.good = FALSE
    THEN myName := ''
    ELSE
      BEGIN
      myName := reply.fName;
      myVolume := reply.vRefNum;
      END;
END;


PROCEDURE CLEARSCREEN;
BEGIN
  TESETSELECT (0, HTE^^.TELENGTH, HTE);
  TEDELETE (HTE);
END;

PROCEDURE ZWRITE (TEXT: STR255);
BEGIN
  TEINSERT (POINTER(ORD4(@TEXT)+1), LENGTH(TEXT), HTE);
END;

PROCEDURE ZWRITELN (TEXT: STR255);
BEGIN
  ZWRITE (TEXT);
  TEKEY (CHAR(13), HTE);
END;

PROCEDURE ZCLOSE;
BEGIN
  IF FSCLOSE (FILEREFNUM) <> 0
     THEN ZWRITELN (' *** COULDN''T CLOSE RECEIVE FILE ***');

  IF FLUSHVOL (Pointer (NIL), VREFNUM) <> 0	  {MUST UPDATE DISK DIRECTORY!}
     THEN ZWRITELN (' *** DISK FLUSH ERROR *** ');
END;

PROCEDURE ZQUIT (MESSAGE: STR255);  {If error, exit program via ZQUIT}
BEGIN
  ZWRITELN (MESSAGE);
  ZCLOSE;

  ZWRITE ('REBOOT MACHINE TO EXIT PROGRAM ');
  WHILE 1 = 1 DO
    BEGIN
    END;    { *** HALT THE PROGRAM HERE *** }
END;


FUNCTION GETCH: CHAR;
  VAR CBUF: PACKED ARRAY [1..1] OF CHAR;
      COUNT1: LONGINT;
BEGIN
COUNT1 := 1;
IF FSREAD (INREFNUM, COUNT1, @CBUF) <> 0
  THEN ZQUIT ('***  EXTERNAL I/O READ ERROR  ***');
GETCH := CBUF [1];
END;


PROCEDURE SHOWACK (OKAY: BOOLEAN);
BEGIN
IF OKAY
  THEN ZWRITE ('A')	     {PRINT AN ACKNOWLEDGE}
  ELSE ZWRITE ('N');	     {PRINT A NO-ACKNOWLEDGE}
APPLECOUNT := APPLECOUNT + 1;
IF APPLECOUNT = 4 THEN
  BEGIN
  ZWRITE (' ');		     {SHOW A SPACE EVERY 1K}
  APPLECOUNT := 0;
  END;
END;


FUNCTION GETCMD: CHAR;
  VAR CH: CHAR;
BEGIN
  REPEAT
    CH := GETCH;	       {GET COMMAND HEADER}
  UNTIL ORD (CH) = CHCMD;
  GETCMD := GETCH;	       {GET COMMAND}
END;

PROCEDURE PUTCMD (CH: CHAR; VAR ZBUF: TYPE264);
BEGIN
  ZBUF [0] := ORD (CHR (CHCMD));    {COMMANDS ALWAYS PREFIXED WITH 126}
  ZBUF [1] := ORD (CH);
END;

PROCEDURE PUTHDR (VAR ZBUF: TYPE264);
BEGIN
  ZBUF [2] := ORD (CHR (1));
  ZBUF [3] := ORD (CHR (0));
  ZBUF [4] := ORD (CHR (254));
  ZBUF [5] := ORD (CHR (255));
END;


{ READ BYTECOUNT BYTES OF PACKED DATA FROM THE PICTURE FILE.
    *** SHOULD FIX THIS TO HANDLE POSSIBLE EOF ERRORS ***}

PROCEDURE READPIC (BUFPTR: PTR;	 BYTECOUNT: LONGINT);
BEGIN
  IF FSREAD (FILEREFNUM, BYTECOUNT, BUFPTR) <> 0
    THEN ZQUIT ('***  PICTURE FILE READ ERROR  ***');
END;


PROCEDURE MOREBITS;     {TIME TO READ NEXT CHUNK OF PACKED SOURCE??}
  VAR SENTBYTES: LONGINT;
BEGIN
SENTBYTES := SRCSIZE - 512;
IF ORD (SRCPTR) > ORD (@SRCBUF) + SENTBYTES THEN
  BEGIN
  SRCBUF [1] := SRCBUF [SRCBLOCKS];	       {MOVE UP LAST BLOCK}
  READPIC (@SRCBUF[2], SENTBYTES);	       {REFILL THE BUFFER}
  SRCPTR := POINTER (ORD(SRCPTR) - SENTBYTES);
  END;
END;


{IF SENDING A PICTURE FILE, READ (PACKED) DATA INTO SRCBUF, UNPACK INTO DSTBUF,
   THEN COPY 40 BYTES FROM EACH 72 BYTE SCANLINE TO ZBUF.
   IF SENDING ANY OTHER TYPE FILE, READ DATA DIRECTLY FROM DISK TO ZBUF}

FUNCTION READBLOCK (VAR ZBUF: TYPE264): BOOLEAN;
  TYPE TYPE72 = PACKED ARRAY [1..72] OF QDBYTE;	    {1 SCANLINE = 72 x 8 PIXELS}
  VAR COUNT256: LONGINT;
      INDEX: INTEGER;
      DSTBUF: ARRAY [1..6] OF TYPE72;	 {1 STFTP BLOCK = 6 (SHORT) SCANLINES}
      DSTPTR: PTR;

BEGIN
(* IF debug THEN DebugInProc (func, 'ReadBlock', @ReadBlock);
*)
IF NOT (ZPICTURE) THEN
  BEGIN
 {GIVEN A 264-BYTE STFTP BUFFER, FILL THE DATA AREA WITH THE NEXT CONSECUTIVE 256
    BYTES FROM OUR OPENED FILE.	 IF EOF, RETURN "FALSE", OTHERWISE "TRUE".}

  COUNT256 := 256;
  IO := FSREAD (FILEREFNUM, COUNT256, POINTER(ORD4(@ZBUF)+6) );
  IF IO = 0
    THEN READBLOCK := TRUE
    ELSE
      BEGIN
      IF IO = EOFERR		 {REACHED (OR PASSED) END OF FILE?}
        THEN READBLOCK := FALSE
        ELSE ZQUIT ('***  DISK READ ERROR  ***');
      END;
  END;

IF ZPICTURE THEN
  BEGIN
  IF STARTPIC THEN	  {IS THIS THE FIRST TIME THROUGH?}
    BEGIN
    FOR INDEX := 1 TO (VOFFSET * 8) DO  {YES, DISCARD THE TOPMOST n SCANLINES}
      BEGIN
      DSTPTR := @DSTBUF;
      UNPACKBITS (SRCPTR, DSTPTR, 72);  {BUMP UP SRCPTR, IGNORE DSTPTR}
      MOREBITS;			        {REFILL SOURCE BUFFER IF NEEDED}
      END;
    SCANLINE := 0;
    STARTPIC := FALSE;
    END;

  IF SCANLINE < 192	  {192 = 24 SCREEN LINES x 8 SCANLINES}
    THEN
      BEGIN
      DSTPTR := @DSTBUF;
      FOR INDEX := 1 TO 6 DO		  {UNPACK 6 SCANLINES FOR THIS BLOCK}
        BEGIN
        UNPACKBITS (SRCPTR, DSTPTR, 72);  {BUMP UP BOTH PTR VALUES}
        MOREBITS;			  {REFILL SOURCE BUFFER IF NEEDED}
        END;

      FOR INDEX := 0 TO 5 DO	      {SEND **ONLY** 40 BYTES FROM EACH SCANLINE}
        BEGIN
        BLOCKMOVE ( POINTER (ORD4(@DSTBUF) + (72*INDEX) + HOFFSET),
                    POINTER (ORD4(@ZBUF) + 6 + (40*INDEX)), 40);
        END;

      SCANLINE := SCANLINE + 6;
      READBLOCK := TRUE;
      END

    ELSE READBLOCK := FALSE;
  END;
END;


PROCEDURE PUTCKSUM (VAR ZBUF: TYPE264);
VAR SUM: 0..65280;	  {256 bytes x 255 max value}
    INDEX: INTEGER;
BEGIN
(*
  IF debug THEN DebugInProc (proc, 'PutCkSum', @PutCkSum);
*)
  SUM := 0;
  FOR INDEX := 6 TO 261 DO
     SUM := SUM + ZBUF [INDEX];

  ZBUF [262] := ORD (CHR (SUM DIV 256));   {MSByte OF CHECKSUM}
  ZBUF [263] := ORD (CHR (SUM MOD 256));   {LSByte OF CHECKSUM}
END;


{TRANSMIT A BLOCK REPEATEDLY UNTIL A VALID ACKNOWLEDGE IS RECEIVED}

PROCEDURE SENDIT (BYTECOUNT: LONGINT; VAR ZBUF: TYPE264);
  VAR OKAY: BOOLEAN;
BEGIN
(*  IF debug THEN DebugInProc (proc, 'SendIt', @SendIt);
*)
  REPEAT
  IF FSWRITE (OUTREFNUM, BYTECOUNT, @ZBUF) <> 0	  {TRANSMIT THE BLOCK}
    THEN ZQUIT ('***  I/O WRITE ERROR  ***');
  IF GETCMD = CHR (CHACK)			  {WAIT FOR AN ACK}
    THEN OKAY := TRUE
    ELSE OKAY := FALSE;
  SHOWACK (OKAY);
  UNTIL OKAY;
END;


PROCEDURE SENDFILE;
  VAR BUFFER: TYPE264;	 {264 BYTE STFTP BUFFER}
BEGIN
(*
  IF debug THEN DebugInProc (proc, 'SendFile', @SendFile);
*)
  APPLECOUNT := 0;
  IF ZPICTURE THEN	  {INITIALIZATION FOR PICTURE FILES ONLY}
    BEGIN
    READPIC (@SRCBUF, 512);     {DISCARD THE PICTURE HEADER BYTES}
    READPIC (@SRCBUF, SRCSIZE); {NOW "PRIME" THE BUFFER}
    SRCPTR := @SRCBUF;
    STARTPIC := TRUE;	        {WILL NEED TO DISCARD FIRST FEW SCANLINES TOO}
    END;

  PUTCMD (CHR(CHDTA), BUFFER);  {CREATE TFTP HEADER INFORMATION}
  PUTHDR (BUFFER);

  WHILE READBLOCK (BUFFER) DO   {REPEAT UNTIL NO MORE BLOCKS}
    BEGIN
    PUTCKSUM (BUFFER);
    SENDIT (264, BUFFER);
    END;

  PUTCMD (CHR(CHEOF), BUFFER);  {CREATE AN EOF MESSAGE}
  SENDIT (2, BUFFER);
END;   (* SENDFILE *)


PROCEDURE MAINLOOP;
BEGIN
(*
IF debug THEN DebugInProc (proc, 'MainLoop', @mainloop);
*)
  CLEARSCREEN;

  ZWRITE ('SEND A MACPAINT PICTURE FILE (Y OR N)?  ');
  REPEAT
     CH := TTYIN;
  UNTIL ((CH = 'Y') OR (CH = 'y')) OR ((CH = 'N') OR (CH = 'n'));
  TTYOUT (CH, 0);
  ZWRITELN (' ');
  IF (CH = 'Y') OR (CH = 'y')
     THEN ZPICTURE := TRUE
     ELSE ZPICTURE := FALSE;

  GetName (ZPICTURE, FILENAME, VREFNUM);

  IF FILENAME = ''
     THEN
        BEGIN
        ZWRITELN (' ');
        ZWRITE ('TRANSMISSION ABORTED.  ');
        EXIT (MAINLOOP);
        END
     ELSE
        BEGIN
        IF FSOPEN (FILENAME, VREFNUM, FILEREFNUM) <> 0
          THEN ZQUIT (' ***  COULDN''T OPEN THE SELECTED FILE  *** ');
        END;

  ZWRITELN ('LOGIN TO REMOTE SYSTEM, STRIKE ANY KEY TO BEGIN  ');
  CH := TTYIN; TTYOUT (CH, 0);

  SENDFILE;

  ZCLOSE;
  SYSBEEP (20);	       {SIGNAL FINISH}

  ZWRITELN (' ');
  ZWRITE ('TRANSMISSION COMPLETE.  ');
END;


FUNCTION GetBaud: INTEGER;  {prompt for a transmission speed}
BEGIN
  ZWRITE ('Line speed: 1200, 2400, 4800, or 9600 (Type 1,2,3, or 4)');
  REPEAT
     CH := TTYIN;
  UNTIL (CH > '0') AND (CH < '5');
  ZWRITELN ('');

  CASE ORD (Ch) OF
    49: GetBaud := $CC5E;
    50: GetBaud := $CC2E;
    51: GetBaud := $CC16;
    52: GetBaud := $CC0A;
    END;    { of event case }
END;

BEGIN  { MAIN PROGRAM }

{$IFC BUG = 1}
lf := CHR (10);
Rewrite (debugger, '.BOUT');   {the printer port}
{$ENDC}

debug := FALSE;	   {this variable activates external printing}

IF debug THEN
  BEGIN
  Writeln (debugger, lf,lf);
  DebugInProc (neither, 'STFTP top level', @STFTP);
  END;

IOINIT;

IF GETVOL (@VOLNAME, VREFNUM) <> 0	  {GET DEFAULT VOLUME INFORMATION}
   THEN ZQUIT (' ***  GETVOL ERROR *** ');


{ *** USE PORT B FOR NOW *** }

IF FSOPEN ('.BIN', VREFNUM, INREFNUM) <> 0
   THEN ZQUIT (' ***  EXTERNAL I/O OPEN ERROR  *** ');
IF FSOPEN ('.BOUT', VREFNUM, OUTREFNUM) <> 0
   THEN ZQUIT (' ***  EXTERNAL I/O OPEN ERROR  *** ');

ZPARAMS.ASNCCONFIG := GETBAUD;		  {PROMPT USER FOR LINE SPEED}
IF CONTROL (INREFNUM, 8, @ZPARAMS) <> 0
   THEN ZQUIT (' *** CONFIGURATION ERROR *** ');
IF CONTROL (OUTREFNUM, 8, @ZPARAMS) <> 0
   THEN ZQUIT (' *** CONFIGURATION ERROR *** ');

REPEAT
   MAINLOOP;
   ZWRITE ('PRESS ''Q'' TO QUIT, SPACE TO SEND ANOTHER');
   FLUSHEVENTS (EVERYEVENT,0);
   CH := TTYIN;
UNTIL (CH = 'Q') OR (CH = 'q');

END.

