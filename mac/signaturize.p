
{ Customize the Macintosh Finder information (signature of application, file type of document)
  for a given Infocom disk using a given code.  The default Infocom strings are INFO and ZSAV.  The
  customized strings are of the form INxx and ZSxx, where xx should be a two-character Production
  Department code, e.g., Z1, M3, or S4.

  Written by Duncan Blanchard  2-10-86
}

program Signaturize;

   {$U-} {Turn on/off the Lisa Libraries.}
   {$X-} {Turn on/off stack expansion.}
   {$R-} {Turn on/off range checking.}	 {off due to compiler bug.}
   {$D-} {Turn on/off procedure names in object code.}
   {$M+} {Turn on/off Macintosh code generation (preserves A2,D3)}
   {$ASM-} {no 68K assembly listing}
   {$L-} {don't list the following interfaces}

uses
   {$U Obj/MemTypes    } MemTypes,
   {$U Obj/QuickDraw   } QuickDraw,
   {$U Obj/OSIntf      } OSIntf,
   {$U Obj/ToolIntf    } ToolIntf,
   {$U Obj/PackIntf    } PackIntf;		      { for NumToString, etc. }

   {$L+} {enable listing}

CONST
    DATALEN = 512;      { size of a buffer }

TYPE
    DATABUF = PACKED ARRAY [0..DATALEN-1] OF 0..255;
    DATAPTR = ^DATABUF;

(* VAR -- none *)


{--------------------------------------------------------------------------------------}
{   Exit Routines								       }
{--------------------------------------------------------------------------------------}

PROCEDURE AllDone;
BEGIN
    writeln ('Click button to exit ...');
    while not button do;
    while button do;	    { await a click the cheap way (should look at events) }

    EXIT (Signaturize);
END;

PROCEDURE Baggit (message: STR255; error: INTEGER);
BEGIN
    writeln ('Error detected, code ', error);
    writeln ( message );
    writeln;
    writeln ('WARNING: This disk may now be internally inconsistent. To be safe, ');
    writeln ('RE-FORMAT it and try again, or consult a Mac hacker.');

    AllDone;
END;

{--------------------------------------------------------------------------------------}
{   Pattern Matching Routines							       }
{--------------------------------------------------------------------------------------}

{ Given two patterns of equal length, check for equality. }

FUNCTION SeqCompare (seq1, seq2: DATAPTR; seqlen: INTEGER): BOOLEAN;
VAR
    index: INTEGER;
    same:  BOOLEAN;
BEGIN
    same := TRUE;  {assume equal, drop out immediately upon finding otherwise.}
    index := 0;
    WHILE (same AND (index < seqlen)) DO
        BEGIN
        IF seq1^[index] <> seq2^[index] THEN same := FALSE;
        index := index + 1;
        END;
    SeqCompare := same;
END;

{ Given a short pattern and a long field of data to search through,
  Return offset of pattern into field if found, -1 if no match. }

FUNCTION MatchPattern (pattern, field: DATAPTR; plen, flen: INTEGER): INTEGER;
VAR
    index: INTEGER;
    searching:  BOOLEAN;
BEGIN
    searching := TRUE;
    index := 0;
    WHILE (searching AND (index < flen-plen)) DO    {stop BEFORE falling off end}
        BEGIN
{ We check for a match by repeatedly calling the compare routine.
  Not very efficient for large data fields.  First line speeds things up a bit. }

        IF pattern^[0] = field^[index]
            THEN IF SeqCompare (pattern, POINTER(ORD(field)+index), plen)
                THEN searching := FALSE;    {found it, stop searching}
        index := index + 1;
        END;
    IF {still} searching
        THEN MatchPattern := -1	      {pattern not found}
        ELSE MatchPattern := index-1;
END;


{--------------------------------------------------------------------------------------}
{  Low-Level Disk I/O								       }
{--------------------------------------------------------------------------------------}

{ We want direct access to raw disk sectors, as well as normal open files.  The major restriction
  imposed by the former is that, for Read and Write, both data offset and data length must be
  exact multiples of 512 bytes.

  A direct access is distinguished from a normal file access by a RefNum of $FFFB, which indicates
  the Disk Driver device.  In this case, the VRefNum (alias DrvNum) must specify the drive containing
  the disk -- always drive 1 by executive decree.  (How can I determine that a disk is in drive 2?)
}

FUNCTION ReadBlock (RefNum, BlockNum: INTEGER; Buffer: DATAPTR; VAR ActCount: LONGINT): OSErr;
CONST
    BLEN = 512;	  {all I/O is in multiples of 512}
VAR
    PB: ParamBlockRec;
BEGIN
    PB.ioCompletion    := NIL;
    PB.ioVRefNum       := 1;		    {if direct access, always drive 1, otherwise ignored}
    PB.ioRefNum	       := RefNum;

    PB.ioBuffer	       := POINTER (Buffer);
    PB.ioReqCount      := BLEN;		    {always one block}
    PB.ioPosMode       := fsFromStart;
    PB.ioPosOffset     := BlockNum * BLEN;

    ReadBlock := PBRead (@PB, FALSE);	    {not asynchronous}
    ActCount := PB.ioActCount;		    {return actual bytes read -- may be less than 512 if EOF}
END;

FUNCTION WriteBlock (RefNum, BlockNum: INTEGER; Buffer: DATAPTR): OSErr;
CONST
    BLEN = 512;	  {all I/O in multiples of 512}
VAR
    PB: ParamBlockRec;
BEGIN
    PB.ioCompletion    := NIL;
    PB.ioVRefNum       := 1;		    {if direct access, always drive 1, otherwise ignored}
    PB.ioRefNum	       := RefNum;

    PB.ioBuffer	       := POINTER (Buffer);
    PB.ioReqCount      := BLEN;		    {always one block}
    PB.ioPosMode       := fsFromStart;
    PB.ioPosOffset     := BlockNum * BLEN;

    WriteBlock := PBWrite (@PB, FALSE);	    {not asynchronous}
END;

{ Must use low-level call to access resource files }

FUNCTION OpenRFork (FName: STR255; VRefNum: INTEGER; VAR RefNum: INTEGER): OSErr;
VAR
    PB: ParamBlockRec;
BEGIN
    PB.ioCompletion    := NIL;
    PB.ioNamePtr       := StringPtr (@FName);
    PB.ioVRefNum       := VRefNum;	    {this time it's a real disk refnum}

    PB.ioVersNum       := 0;
    PB.ioPermssn       := fsRdWrPerm;
    PB.ioMisc	       := NIL;

    OpenRFork := PBOpenRF (@PB, FALSE);	    {not asynchronous}
    RefNum := PB.ioRefNum;		    {return the file refnum here}
END;

FUNCTION CloseRFork (VRefNum, RefNum: INTEGER): OSErr;
VAR
    PB: ParamBlockRec;
    err: OSErr;
BEGIN
    PB.ioCompletion    := NIL;
    PB.ioRefNum	       := RefNum;
    err := PBClose (@PB, FALSE);     {not asynchronous}

    IF err = noErr THEN
        BEGIN
        PB.ioCompletion	   := NIL;
        PB.ioNamePtr	   := NIL;
        PB.ioVRefNum	   := VRefNum;

        err := PBFlushVol (@PB, FALSE);	     {must flush volume buffer to update disk}
        END;
    CloseRFork := err;
END;

{--------------------------------------------------------------------------------------}
{  Search and Replace								       }
{--------------------------------------------------------------------------------------}

{ Read through a given file, searching for the first N occurrences of a given string.
  When found, patch it and write it out.  Return the number of occurrences found. }

FUNCTION PatchFile (refnum, N: INTEGER; oldstr, newstr: STR255): INTEGER;
VAR
    zbuffer: array[1..2] of DATABUF;    {two data buffers}

    oldseq, newseq:	    DATAPTR;    {same as given strings, just different format}
    seqlen:		    INTEGER;

    curBlock:		    INTEGER;    {block now in (first) buffer}
    hit:		    BOOLEAN;    {true if pattern found}
    hitOffset, hitCount:    INTEGER;    {offset of current hit; total hit count}

    actLen1, actLen2:	    LONGINT;    {actual number bytes read}
    err:		    OSErr;

BEGIN
    oldseq := DATAPTR (ORD4 (@oldstr)+1);
    newseq := DATAPTR (ORD4 (@newstr)+1);
    seqlen := LENGTH (oldstr);		    {same for old and new}

    curBlock := 0;
    hitCount := 0;

{ Since the string being sought might "crossover" between file blocks, the search uses a
  two-buffer strategy.  The buffers are always occupied by consecutive blocks.  }

    err := ReadBlock (refnum, curBlock, DATAPTR (@zbuffer[2]), actLen2);    {prime the 2nd buffer}
    if (err <> noErr) and (err <> eofErr)	    { accept only reasonable things }
        THEN Baggit ('File Read error', err);

{ UPDATE THE BUFFERS -- shift new curBlock into 1st buffer, read curBlock+1 into 2nd buffer}

    REPEAT
        BlockMove (PTR (@zbuffer[2]), PTR (@zbuffer[1]), 512);
        actLen1 := actLen2;

        err := ReadBlock (refnum, curBlock+1, DATAPTR (@zbuffer[2]), actlen2);
        if (err <> noErr) and (err <> eofErr)
            THEN Baggit ('File Read error', err);

{ SEARCH AND REPLACE -- find all occurances before writing back the blocks}

        hit := FALSE;
        REPEAT
            hitOffset := MatchPattern (oldseq, DATAPTR (@zbuffer[1]), seqlen, actLen1 + actLen2);

        (* IDEA: Reduce search length by looking only at last <seqlen> chars of 1st buffer.
                 Also, use <hitOffset+seqlen> to reduce search length after 1st hit *)

            IF hitOffset <> -1 THEN		    {found one, patch it}
                BEGIN
                BlockMove (PTR (newseq), PTR (ORD4 (@zbuffer[1]) + hitOffset), seqlen);

                hit := TRUE;    hitCount := hitCount + 1;
                END;
        UNTIL (hitOffset = -1) OR (hitCount = N);

        IF hit THEN  {write out buffers}	    (* IDEA: might need to write just one buffer *)
            BEGIN
            err := WriteBlock (refnum, curBlock, DATAPTR (@zbuffer[1]) );
            if (err <> noErr)
                THEN Baggit ('File Write error', err);

            err := WriteBlock (refnum, curBlock+1, DATAPTR (@zbuffer[2]) );
            if (err <> noErr)
                THEN Baggit ('File Write error', err);
            END;

        curBlock := curBlock + 1;		    {next block}

{ CHECK IF DONE (EOF reached, or hit limit reached) }

    UNTIL (actLen2 < 512) OR (hitCount = N);

    PatchFile := hitCount;      {return as result}
END;


{--------------------------------------------------------------------------------------}
{  Top Level									       }
{--------------------------------------------------------------------------------------}

PROCEDURE PatchDisk;    {a total of two files, plus the disk directory, require patching}
CONST
    DRIVE1  = 1;	        {always use drive 1 -- see note for ReadBlock}
    ALL	    = 999;	        {indicate maximum occurences of pattern to patch}
VAR
    newCode,		        {two chars, xx, supplied by user}
    newINFO,		        {INxx}
    newZSAV:    STRING[32];     {ZSxx}

    reply:      sfReply;        {standard file reply}
    boxloc:     point;	        {where dialog boxes go}
    typeList:   sfTypeList;

    refnum,		        {of current opened file}
    hits:       INTEGER;
    err:        OSErr;

BEGIN
while TRUE do				  { main loop repeats forever until user cancels }
    BEGIN
    eraseRect (thePort^.portRect);	  { clean the screen }
    moveTo (0, 30);

{prompt for name of game file}

    writeln ('Put the disk to be signaturized in the INTERNAL drive ');
    writeln ('(must first eject any other disk present),  ');
    writeln ('then select the game file from the directory list below: ');

    setPt (boxloc, 100, 100);
    sfGetFile (boxloc, '', NIL, -1, typeList, NIL, reply);    { no fancy filtering; any file type }

    eraseRect (thePort^.portRect);	  { clean the screen again after sfGetFile}
    IF NOT reply.good THEN AllDone;	  { if they cancelled, quit }

{prompt for new signature (two chars)}

    REPEAT
        writeln;
        writeln	 ('Enter a two-character Production code, plus a RETURN. ');
        write	 ('(To try again (if error), enter more than two chars, plus a RETURN):	 ');
        readln	 ( newCode );
    UNTIL length (newCode) = 2;

    newINFO := concat ('IN', newCode);
    newZSAV := concat ('ZS', newCode);

{patch the game file}

    err := OpenRFork (reply.fName, DRIVE1, refnum);	      {open the game file (resource fork only) }
    if err <> noErr
        THEN Baggit ('Could not open game file', err);

    hits := PatchFile (refnum, ALL, 'INFO', newINFO);	      {patch the 3 INFO strings}
    if hits <> 3
        THEN Baggit ('Bad INFO count in game file', hits);

    hits := PatchFile (refnum, ALL, 'ZSAV', newZSAV);	      {patch the 3 ZSAV strings}
    if hits <> 3
        THEN Baggit ('Bad ZSAV count in game file', hits);

    err := CloseRFork (DRIVE1, refnum);
    if err <> noErr
        THEN Baggit ('Game file close error', err);

{patch the Desktop file}

    err := OpenRFork ('DESKTOP', DRIVE1, refnum);	      {open Desktop (the resource fork only) }
    if err <> noErr
        THEN Baggit ('Could not open Desktop', err);

    hits := PatchFile (refnum, ALL, 'INFO', newINFO);	      {patch the 2 INFO strings}
    if hits <> 2
        THEN Baggit ('Bad INFO count in Desktop', hits);

    hits := PatchFile (refnum, ALL, 'ZSAV', newZSAV);	      {patch the 1 ZSAV string}
    if hits <> 1
        THEN Baggit ('Bad ZSAV count in Desktop', hits);

    err := CloseRFork (DRIVE1, refnum);
    if err <> noErr
        THEN Baggit ('Desktop close error', err);

{patch the disk directory.
   The magic number $FFFB indicates a direct call to the Disk Driver device.
   Must limit hit count to 1 to avoid a search through the entire volume.
}
    hits := PatchFile ($FFFB, 1, 'INFO', newINFO);	      {patch the 1 INFO string}
    if hits <> 1
        THEN Baggit ('Bad INFO count in volume directory', hits);

    err := FlushVol (NIL, DRIVE1);	   {make sure the directory is updated}
    IF err <> noErr
        THEN Baggit ('FlushVol error', err);

{all done with this one}

    writeln;
    write ('Signaturization successful!	 Do another? (Y/N):  ');
    readln   ( newCode );

    IF (newCode[1] <> 'Y') AND (newCode[1] <> 'y') THEN AllDone;

    END;  { of loop, repeat }
END;

BEGIN  {main program entry point}

    initGraf (@thePort);		  { we use quickdraw, and little else }
    initWindows;			  { doubt we need most of these... }
    initFonts;
    initMenus;
    initDialogs (NIL);

    PatchDisk;
END.					  { of program }



