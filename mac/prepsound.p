
{ sound file munger for the Macintosh
  WRITTEN BY: Duncan Blanchard  05-Feb-88

  this is a sound pre-processor for the Mac, which takes an Amiga-format
  sound file and does two things:
    - map each sample byte from signed -> unsigned
    - make waveform begin/end near zero (unsigned), to avoid a nasty click
  
  for now, we read file 'SD' and write to 'SD.out'
}

PROGRAM MungSound;

	{$SETC RAMP := TRUE}	{ false for some loud sounds }

{ The first two switches are specified in the command line that invokes
  the compiler.  Embedded names and range checking should be suppressed 
  in shipped code, but are useful for debugging. }

	{cmdline $R-} { Turn on/off range checking }
	{cmdline $D-} { Turn on/off procedure names in object code }

{ Exporting of names is off initially, then changed below. }

	{$Z-} { do/don't export all routine names }
	{$L-} { don't include lengthy interface files in listings }
USES
	MemTypes,
	QuickDraw,
	OSIntf,
	ToolIntf;
{	PackIntf }
{	MacPrint }

	{$L+} { reenable listing for our code}
	{$Z*} { Export following (all) routine names (but not vars)}

TYPE
{ the following record def combines two adjacent byte members, because
  of the vagaries of Pascal packing }

  SHeader = PACKED RECORD  	{ sound file header format: }
	flen:	INTEGER;	{ len (bytes) of file following }
	rp:	INTEGER;	{ repeat count / midi pitch }
	rate:	INTEGER;	{ original sampling rate }
	unused:	INTEGER;
	dlen:	INTEGER		{ len (bytes) of data following }
    END;
  SHeaderPtr = ^SHeader;
  
CONST
  ASHLEN = 10;			{ length, Activision sound header }
  MSHLEN = 6;			{ length, Mac sound header }
  MSLEN = (64 * 1024) + MSHLEN;	{ 64K max, plus space for header }

VAR
  sheadp: SHeaderPtr;
  soundp: FFSynthPtr;
  inChan, outChan: INTEGER;

{-------------------------------}
{ MungSnd			}
{-------------------------------}

FUNCTION MungSnd (id: INTEGER): INTEGER;
CONST
  RAMPTIME = 30;	{ minimum ramp is (1/100?) (1/150?) sec }
  
VAR
  p, begp, endp: Ptr;	{ ptr to unsigned byte (see mem mgr) }
  n: LONGINT;		{ len passed & actual len /returned/ here }
  dlen32: LONGINT;	{ sound data len (unsigned, 32 bits) }
  err: INTEGER;
  i, rampLen: INTEGER;
  x: LONGINT;		{ big enough for our ramp calc }

BEGIN
{ make sure buffers exist }

  IF (sheadp = NIL) OR (soundp = NIL) THEN
  BEGIN
    MungSnd := -1;
    EXIT (MungSnd);
  END;

{ open sound data file (ignore id arg for now ...) }

  err := FSOpen ('SD', 0, inChan);
  IF err <> 0 THEN
  BEGIN
    inChan := 0;		{ mark as unopened }
    MungSnd := err;
    EXIT (MungSnd);
  END;
  
{ get data header, and data body }

  n := ASHLEN;
  err := FSRead (inChan, n, Ptr(sheadp));
  IF err <> 0 THEN
  BEGIN
    MungSnd := err;
    EXIT (MungSnd);
  END;
  
  dlen32 := BAND (sheadp^.dlen, $0FFFF);	{ unsigned! }
  n := dlen32;
  begp := Ptr (ORD4(soundp) + MSHLEN);	{ leave space for Mac header }
  endp := Ptr (ORD4(begp) + dlen32);

  err := FSRead (inChan, n, begp);	{ get data }
  IF err <> 0 THEN
  BEGIN
    MungSnd := err;
    EXIT (MungSnd);
  END;
    
{ for Mac, map each sample byte from signed -> unsigned }
  
  p := begp;
  WHILE ORD4(p) < ORD4(endp) DO
  BEGIN
    p^ := BXOR (p^, $080);
    p := Ptr (ORD4(p) + 1);
  END;

{$IFC RAMP}

{ To avoid a loud "click," especially in quieter effects, seems that 
  we must "ramp up" from zero at the beginning of the waveform, and 
  "ramp down" at the end.  The ramp length should be as short as is
  needed to remove the click.

  In certain louder effects ramping may actually be undesirable, 
  since the "click" may be unnoticable but the silence may cause a
  slight but audible break between loops.  }
    
  rampLen := sheadp^.rate DIV RAMPTIME;
  
  p := begp;
  FOR i := 0 TO rampLen-1 DO
  BEGIN
    x := BAND (p^, $0FF);		{ unsigned fer shure }
    p^ := (x * i) DIV rampLen;
    p := Ptr (ORD4(p) + 1);
  END;

  p := endp;
  FOR i := 0 TO rampLen-1 DO
  BEGIN
    p := Ptr (ORD4(p) - 1);		{ work backwards from end }
    x := BAND (p^, $0FF);
    p^ := (x * i) DIV rampLen;
  END;

{$ENDC}

{ create/open an output file (ignore id arg for now ...)  Don't set 
  creator = INFO, to avoid click-launching.  No type yet, either. }
  
  err := Create ('SD.out', 0, '    ', '    ');
  IF err <> 0 THEN
  BEGIN
    outChan := 0;		{ mark as unopened }
    MungSnd := err;
    EXIT (MungSnd);
  END;
  
  err := FSOpen ('SD.out', 0, outChan);
  IF err <> 0 THEN
  BEGIN
    outChan := 0;		{ mark as unopened }
    MungSnd := err;
    EXIT (MungSnd);
  END;
  
  { write header, write data }

  n := ASHLEN;
  err := FSWrite (outChan, n, Ptr(sheadp));
  IF err <> 0 THEN
  BEGIN
    MungSnd := err;
    EXIT (MungSnd);
  END;
  
  n := dlen32;
  err := FSWrite (outChan, n, begp);
  IF err <> 0 THEN
  BEGIN
    MungSnd := err;
    EXIT (MungSnd);
  END;
    
  MungSnd := 0;		{ all okay }
END;

{-------------------------------}
{ Cleanup			}
{-------------------------------}

PROCEDURE Cleanup;
VAR
  err: INTEGER;
BEGIN
  IF inChan <> 0 THEN
    err := FSClose (inChan);

  IF outChan <> 0 THEN
    err := FSClose (outChan);
  
  err := FlushVol (NIL, 0);	{ make sure disk is updated }
END;
  
{-------------------------------}
{ ZSound			}
{-------------------------------}

PROCEDURE ZSound;
VAR
  n: INTEGER;
BEGIN  
{ one-time allocation of buffers for sound data }

  sheadp := SHeaderPtr (NewPtr(ASHLEN));
  soundp := FFSynthPtr (NewPtr(MSLEN));
  
  n := MungSnd (99);
  IF n <> 0 THEN
  BEGIN
    SysBeep (8);  SysBeep (8);  SysBeep (8);	{ cheap error signal }
  END;

  Cleanup;		{ close files }
END;

{-------------------------------}
{ Main				}
{-------------------------------}

BEGIN
  MoreMasters;			{ could probably skip most inits }
  InitGraf (@thePort);
  InitWindows;
  InitFonts;
  InitMenus;
  InitCursor;
  FlushEvents (everyEvent,0);
  
  ZSound;
END.

