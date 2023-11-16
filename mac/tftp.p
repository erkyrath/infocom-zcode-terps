
PROGRAM TFTP;		 {FILE TRANSFER PROGRAM FOR MACINTOSH}

USES  {$U-}
      {$U Obj/Memtypes  } Memtypes,
      {$U OBJ/QUICKDRAW } QUICKDRAW,
      {$U OBJ/OSINTF    } OSINTF,
      {$U OBJ/TOOLINTF  } TOOLINTF,
      {$U OBJ/PACKINTF  } PACKINTF;

CONST CHCMD=126;        {COMMAND PREFIX BYTE}
      CHDTA=0;
      CHEOF=4;
      CHACK=2;
      CHNAK=5;

TYPE BUFTYPE = PACKED ARRAY[0..255] OF 0..255;

VAR GAMENAME,			 {TFTP globals}
    VOLNAME:    STR255;
    IO,
    GAMEREFNUM,
    VREFNUM,
    INREFNUM,
    OUTREFNUM,
    APPLECOUNT: INTEGER;
    DUMMYBYTES,
    COUNT1,
    COUNT2,
    COUNT256:   LONGINT;
    CH:	        CHAR;
    ZPARAMS:    OPPARAMTYPE;

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


{ Open a dialog box and allow user to select name of the Interlogic game file
    (resource fork) whose data fork will receive the incoming data.

  This routine also returns the Volume RefNum, so TFTP may be run from a disk
    other than the game disk.
}

PROCEDURE GetName (VAR fileName: STR255; VAR volRefNum: INTEGER);
VAR
  where: POINT;
  numTypes: INTEGER;
  typeList: SFTypeList;		 {need to include OBJ/PACKINTF defs for these}
  reply: SFReply;

BEGIN
  where.h := 80; where.v := 100;

  numTypes := 1;		 {for no filtering, would use -1 here}
  typeList [0] := 'APPL';	 {filter out all but applications}

  SFGetFile (where, '', NIL, numTypes, typeList, NIL, reply);

  IF reply.good = FALSE
    THEN fileName := ''		 {return a null name if cancelled}
    ELSE
      BEGIN
      fileName := reply.fName;
      volRefNum := reply.vRefNum;
      END;
END;

(* DEAD -------------------------------

PROCEDURE GetName (VAR Name: STR255);   {read in a receive file name}
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

  lastChar := hTE^^.TELength - 1;    {this last char is a CR}

  IF firstChar = lastChar
    THEN Name := 'Game'	       {if no name, use a default name}
    ELSE
      BEGIN
      Name := '';	       {empty string for now}
      dummy := 'x';	       {length is constant -- one char}

      txtptr := POINTER (hTE^^.hText^);
      nextChar := firstChar;

      REPEAT
        dummy[1] := CHR (txtptr^[nextChar]);
        name := CONCAT (name, dummy);	      {this hack is blessed by Apple ...}
        nextChar := nextChar + 1;
      UNTIL nextChar = lastChar;       {ignore the final CR}
      END;
END;
*)

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
  IF FSCLOSE (GAMEREFNUM) <> 0
     THEN ZWRITELN (' *** COULDN''T CLOSE RECEIVE FILE ***');

  IF FLUSHVOL (NIL, VREFNUM) <> 0	     {MUST UPDATE DISK DIRECTORY!}
     THEN ZWRITELN (' *** VOLUME FLUSH ERROR *** ');
END;

PROCEDURE ZQUIT (MESSAGE: STR255);	 {If error, exit program via ZQUIT}
BEGIN
  ZWRITELN (MESSAGE);
  ZCLOSE;

  ZWRITE ('REBOOT MACHINE TO EXIT PROGRAM ');
  WHILE 1 = 1 DO
    BEGIN
    END;		  { *** HALT THE PROGRAM HERE *** }
END;

PROCEDURE SENDCMD (CH: CHAR);
   VAR S: PACKED ARRAY [1..2] OF CHAR;
   BEGIN
   S [1] := CHR(CHCMD);	        {ALWAYS 126}
   S [2] := CH;
   IF FSWRITE (OUTREFNUM, COUNT2, @S) <> 0
     THEN ZQUIT ('***  EXTERNAL I/O WRITE ERROR	 ***');
   END;

FUNCTION GETCH: CHAR;
   VAR S: PACKED ARRAY [1..1] OF CHAR;
   BEGIN
   IF FSREAD (INREFNUM, COUNT1, @S) <> 0
     THEN ZQUIT ('***  EXTERNAL I/O READ ERROR  ***');
   GETCH := S [1];
   END;

PROCEDURE SENDACK;
   BEGIN
   ZWRITE ('A');
   APPLECOUNT := APPLECOUNT + 1;
   IF APPLECOUNT = 4 THEN
      BEGIN
      ZWRITE (' ');	  {SPACE EVERY 1K}
      APPLECOUNT := 0;
      END;
   SENDCMD(CHR(CHACK));
   END;

PROCEDURE SENDNAK;
   BEGIN
   ZWRITE('N');
   SENDCMD(CHR(CHNAK));
   END;

FUNCTION GETCMD: CHAR;
   VAR CH: CHAR;
   BEGIN
   REPEAT
     CH := GETCH;	        {GET COMMAND HEADER}
     UNTIL ORD (CH) = CHCMD;
   GETCMD := GETCH;	        {GET COMMAND}
   END;

FUNCTION GETHDR: BOOLEAN;
   VAR CH: CHAR;
   BEGIN
   GETHDR := FALSE;
   IF ORD(GETCH) <> 1 THEN EXIT(GETHDR);
   IF ORD(GETCH) <> 0 THEN EXIT(GETHDR);
   IF ORD(GETCH) <> 254 THEN EXIT(GETHDR);
   IF ORD(GETCH) <> 255 THEN EXIT(GETHDR);
   GETHDR := TRUE;
   END;

FUNCTION RECV (VAR BUFFER: BUFTYPE): BOOLEAN;
   VAR INDEX:  0..255;
       SUM:    INTEGER;
       CKSUM:  ARRAY[1..2] OF INTEGER;

   BEGIN
   FOR INDEX := 0 TO 255 DO
      BUFFER[INDEX] := ORD(GETCH);
   FOR INDEX := 1 TO 2 DO
      CKSUM[INDEX] := ORD(GETCH);

   SUM := 0;
   FOR INDEX := 0 TO 255 DO
      SUM := SUM + BUFFER[INDEX];

   IF (CKSUM[1] * 256) + CKSUM[2] = SUM
      THEN RECV := TRUE
      ELSE RECV := FALSE;
   END;

PROCEDURE GETFILE;
VAR CMDCH:   CHAR;
    BUFFER:  BUFTYPE;

BEGIN
COUNT1 := 1; COUNT2 := 2; COUNT256 := 256;
APPLECOUNT := 0;
CMDCH := GETCMD;

WHILE ORD(CMDCH) <> CHEOF DO    {END OF GAME FILE?}
   BEGIN
   IF ORD(CMDCH) = CHDTA        {READ+CHECK TWO-BYTE COMMAND}
   THEN
      BEGIN
      IF GETHDR		        {READ+CHECK FOUR-BYTE HEADER}
      THEN
         BEGIN
         IF RECV (BUFFER)       {GET A BLOCK; CKECKSUM OK?}
            THEN
               BEGIN
               IF FSWRITE (GAMEREFNUM, COUNT256, @BUFFER) <> 0	 {ERROR?}
                  THEN ZQUIT ('***  DISK WRITE ERROR  ***');
               SENDACK;
               END
            ELSE SENDNAK;	   {BAD RECV CHECKSUM}
         END
      ELSE SENDNAK;	     {BAD HEADER}
      END
   ELSE SENDNAK;	 {BAD COMMAND BYTE}
   CMDCH := GETCMD;	 {GET NEXT COMMAND BYTE}
   END;
SENDACK;		 {ACKNOWLEDGE FINAL EOF COMMAND}
END;  (* GETFILE *)


BEGIN  (* MAIN PROGRAM *)

IOINIT;
CLEARSCREEN;

ZWRITELN ('');
ZWRITELN ('SELECT NAME OF THE FILE TO RECEIVE');

GetName (GameName, vRefNum);	 {get name of Interlogic game}
IF GameName <> '' THEN		 {ELSE exit}
   BEGIN
   IF FSOPEN (GAMENAME, VREFNUM, GAMEREFNUM) <> 0
      THEN ZQUIT ('Game file OPEN error');

   IF FSOPEN ('.AIN', VREFNUM, INREFNUM) <> 0
      THEN ZQUIT (' ***	 EXTERNAL I/O OPEN ERROR  *** ');
   IF FSOPEN ('.AOUT', VREFNUM, OUTREFNUM) <> 0
      THEN ZQUIT (' ***	 EXTERNAL I/O OPEN ERROR  *** ');

   ZPARAMS.ASNCCONFIG := $CC0A;				    {9600 BAUD}
   IF CONTROL (INREFNUM, 8, @ZPARAMS) <> 0
      THEN ZQUIT (' *** CONFIGURATION ERROR *** ');
   ZPARAMS.ASNCCONFIG := $CC0A;				    {9600 BAUD}
   IF CONTROL (OUTREFNUM, 8, @ZPARAMS) <> 0
      THEN ZQUIT (' *** CONFIGURATION ERROR *** ');

   ZWRITELN ('LOGIN TO REMOTE SYSTEM, PRESS ANY KEY TO BEGIN  ');
   CH := TTYIN; TTYOUT (CH, 0);

   GETFILE;

   ZCLOSE;
   SYSBEEP (20);        {SIGNAL FINISH}

   ZWRITELN (' ');
   ZWRITE ('TRANSFER COMPLETE.  PRESS ANY KEY TO EXIT PROGRAM.  ');

   FLUSHEVENTS (EVERYEVENT,0);
   CH := TTYIN;
   END;

END.

