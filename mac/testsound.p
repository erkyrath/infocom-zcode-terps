
{ sound driver for the Macintosh
  WRITTEN BY: Duncan Blanchard  2-Feb-87  }

PROGRAM TestSound;

{ The first two switches are now specified in the command line that 
  invokes the compiler.  Embedded names and range checking should be 
  suppressed in shipped code, but are useful for debugging. }

	{cmdline $R-} { Turn on/off range checking }
	{cmdline $D-} { Turn on/off procedure names in object code }

{ Exporting of names is off initially, then changed below. }

	{$Z-} { do/don't export all routine names }
	{$L-} { don't include lengthy interface files in listings }
USES
	MemTypes,
	QuickDraw,
	OSIntf,
	ToolIntf,
	PackIntf;
{	MacPrint }

	{$L+} { reenable listing for our code }
	{$Z*} { export following (all) routine names (but not vars) }

TYPE
  STR32 = STRING[32];		{ parameters must be a simple type }
{ this is useless in Pascal; still sign-extends into longints }
{ UInteger = 0..65535;  }	{ 16-bit unsigned }
  UInteger = INTEGER;		{ just mask where necessary, sigh }

{ the following record def combines two adjacent byte members, because
  of the vagaries of Pascal packing }

  SHeader = PACKED RECORD  	{ sound file header format: }
	flen:	UInteger;	{ len (bytes) of file following }
	rp:	UInteger;	{ repeat count / midi pitch }
	rate:	UInteger;	{ original sampling rate }
	unused:	UInteger;
	dlen:	UInteger	{ len (bytes) of data following }
    END;
  SHeaderPtr = ^SHeader;
  
CONST
  ASHLEN = 10;			{ length, Activision sound header }
  MSHLEN = 6;			{ length, Mac sound header }
  MSLEN = MSHLEN + (64 * 1024);	{ max length, plus header }

VAR
  sheadp:	SHeaderPtr;
  soundp:	FFSynthPtr;
  mfchan:	INTEGER;
  dfchan:	INTEGER;
  mpitch:	Byte;
  sreps:	INTEGER;
  sdone:	BOOLEAN;
  uservol:	INTEGER;	{ user's original volume setting, 0..7 }

{-------------------------------}
{ SndFSOpen			}
{-------------------------------}

{ This routine is a front end for FSOpen.  Search for file, return
  (1) chan and (2) status.

  We expect sound files to be in a separate folder called 'Sound', itself 
  in the same folder (disk) as the game file.  However, a Mac folder 
  may or may not be a true subdirectory.  It's not under the old MFS, 
  which we might still run into on 400K disks and ramdisks. }

FUNCTION SndFSOpen (VAR name: STR32; VAR chan: INTEGER): OSErr;
VAR
  name2: STR32;
  err: OSErr;
BEGIN
  name2 := concat (':Sound:', name);	{ use a partial pathname }
  err := FSOpen (name2, 0, chan);	{ first look in subdir if any }
  IF err <> 0 THEN
    err := FSOpen (name, 0, chan);	{ then look in game dir }

  IF err <> 0 THEN
    chan := 0;			{ make sure it's marked as unopened }
  SndFSOpen := err;		{ return status }
END;

{-------------------------------}
{ InitMidi			}
{-------------------------------}

{ open control file (combined midi & name files in Amiga format),  
  return (1) midi pitch and (2) data file name
    
  If no control file exists, return default values. This lets us
  purposely omit control files for many sounds.  }

FUNCTION InitMidi (id: INTEGER; VAR dfname: STR32): OSErr;
CONST
  MPADJUST = $18;	{ mystery pitch adjustment }
{ midi file offsets }
  MFLEN = 0;		{ total length of triplets (normally 9) }
  TRIPL1 = 2;		{ midi chan, pitch, vol }
  TRIPL2 = 5;
  TRIPL3 = 8;
  MFMAX = 64;		{ max length of the above (let's say) }
{ name file offsets }
  M1COUNT = 0;		{ number of data files (Mac: always 1) }
  M1CHAN = 1;
  M1NAME = 2;		{ name of first data file (asciz) }

VAR
  idstr: STR255;	{ Pascal typing forces 255 }
  mfname, temp: STR32;
  mfdata: Packed Array [1..MFMAX] of SignedByte;
  n: LONGINT;
  mfln, i, err: INTEGER;

BEGIN
  NumToString (id, idstr);
  dfname := concat ('S', idstr);	{ default is Sn }
  mpitch := $4A - MPADJUST;		{ default is $32 }

  mfname := concat ('M', idstr);
  err := SndFSOpen (mfname, mfchan);	{ search/open }
  IF err <> 0 THEN
  BEGIN
    InitMidi := err;
    EXIT (InitMidi);
  END;

{ read in length of midi file }
  n := 2;
  err := FSRead (mfchan, n, @mfln);
  IF err <> 0 THEN
  BEGIN
    InitMidi := err;
    EXIT (InitMidi);
  END;
  IF mfln > MFMAX - 2 THEN		{ be safe }
  BEGIN
    InitMidi := -1;
    EXIT (InitMidi);
  END;

{ read in rest of midi file, plus start of name file }
  n := mfln + 2;
  err := FSRead (mfchan, n, @mfdata[1]);
  IF err <> 0 THEN
  BEGIN
    InitMidi := err;
    EXIT (InitMidi);
  END;

{ read in name (of data file) }
  n := 1;
  i := 0;
  REPEAT
    i := i + 1;
    err := FSRead (mfchan, n, @temp[i]);
    IF err <> 0 THEN
    BEGIN
      InitMidi := err;
      EXIT (InitMidi);
    END;
  UNTIL temp[i] = Chr (0);
  temp[0] := Chr (i - 1);

  dfname := temp;			{ return val }
  mpitch := mfdata[2] - MPADJUST;	{ return val }

  InitMidi := 0;	{ return 0 if all okay }
END;

{-------------------------------}
{ CntCalc			}
{-------------------------------}

{ sr = original sampling rate;
  midv = desired midi pitch;  refv = reference midi pitch
 "count" = sr * (CHROMA ^ delta) * 44.93 / 1000000  }
  
FUNCTION CntCalc (sr: UInteger; midv, refv: SignedByte): Fixed;
VAR
  val, base, base12: Fixed;
  delta, i: SignedByte;
  
{ Note: fixed point constants are not possible since the format is 
  "known" only to the Toolbox, not the compiler.  The constants below
  could be computed once at startup for more speed.
CONST
  CHROMA = 1.05946309; } { 12th root of 2 (equal-tempered scale) }

BEGIN					{ sr in range 9K..19K approx }
  val := FixMul (FixRatio (sr,10000), FixRatio (4493,10000));

  delta := midv - refv;			{ if zero, we're done }
  IF delta > 0 THEN
    base := FixRatio (10595,10000)	{ base := CHROMA; }

  ELSE IF delta < 0 THEN
  BEGIN
    base := FixRatio (10000,10595);	{ base := 1 / CHROMA; }
    delta := -delta;
  END;

{ compute CHROMA ^ delta.  Could use a lookup table for more speed. }
  WHILE delta > 0 DO
  BEGIN
    val := FixMul (val, base);
    delta := delta - 1;
  END;

  CntCalc := val;
END;

{-------------------------------}
{ InitData			}
{-------------------------------}

{ open file, read sound data into global buffers }

FUNCTION InitData (VAR dfname: STR32): OSErr;
VAR
  n: LONGINT;		{ len passed, actual len /returned/ here }
  p: Ptr;		{ ptr to unsigned byte (see mem mgr) }
  sr: UInteger;
  dpitch: Byte;
  err: OSErr;

BEGIN
  err := SndFSOpen (dfname, dfchan);		{ search/open }
  IF err <> 0 THEN
  BEGIN
    InitData := err;
    EXIT (InitData);
  END;
  
  n := ASHLEN;
  err := FSRead (dfchan, n, Ptr(sheadp));	{ get data header }
  IF err <> 0 THEN
  BEGIN
    InitData := err;
    EXIT (InitData);
  END;
  
  n := BAND (sheadp^.dlen, $0FFFF);	{ 64K max, unsigned }
  p := Ptr (ORD4(soundp) + MSHLEN);	{ leave space for Mac header }
  err := FSRead (dfchan, n, p);		{ get data }
  IF err <> 0 THEN
  BEGIN
    InitData := err;
    EXIT (InitData);
  END;

  sr := sheadp^.rate;
  dpitch := BAND (sheadp^.rp, $0FF);	{ extract low byte }
  soundp^.count := CntCalc (sr, mpitch, dpitch);
  soundp^.mode := ffMode;		{ free-form synthesizer }

  InitData := 0;		{ all okay }
END;

{-------------------------------}
{ InitCleanUp			}
{-------------------------------}

PROCEDURE InitCleanUp;		{ just close any open files }
VAR
  err: OSErr;
BEGIN
  IF mfchan <> 0 THEN
    err := FSClose (mfchan);
  IF dfchan <> 0 THEN
    err := FSClose (dfchan);
END;

{-------------------------------}
{ StartSnd, StopSnd		}
{-------------------------------}

PROCEDURE StartActual; FORWARD;

PROCEDURE StartSnd (reps, zvol: INTEGER);
VAR
  macvol, v: INTEGER;		{ zvol in 0..8, macvol in 0..7 }
BEGIN
  IF zvol < 0 THEN		{ if -1, default to max }
    zvol := 8;
  v := uservol * zvol;
  macvol := v DIV 8;		{ make proportional to user's setting }
  IF (v MOD 8) > 0 THEN
    macvol := macvol + 1;	{ round up (only 0 -> silence) }

  SetSoundVol (macvol);

  IF reps < 0 THEN				{ if -1, use midi val }
    sreps := BAND (BSR (sheadp^.rp, 8), $0FF)	{ extract high byte }
  ELSE
    sreps := reps;
  IF sreps = 0 THEN sreps := 8;		{ for now, "inf" = 8 ... }

  sdone := FALSE;
  StartActual;
END;

PROCEDURE StopSnd;	{ immed abort (could wait until end of cycle?) }
BEGIN
  IF NOT sdone THEN
  BEGIN
    sdone := TRUE;	{ set flag }
    StopSound;		{ THEN call -- also executes completion }
  END;

  SetSoundVol (uservol);	{ restore original setting }
END;

{-------------------------------}
{ ComplSnd			}
{-------------------------------}

{ completion routine for asynchronous sound driver call.
  if reps-remaining count is positive, decrement it.  
  if it becomes zero, set "done" flag, otherwise start next loop. }

PROCEDURE ComplSnd;
BEGIN
  SetUpA5;	{ since we're an interrupt, and use globals }

  IF sreps > 0 THEN		{ if already zero, infinite loop }
  BEGIN
    sreps := sreps - 1;
    IF sreps = 0 THEN		{ finished, set flag }
      sdone := TRUE;
  END;

  IF NOT sdone THEN		{ not finished, begin next loop }
    StartActual;

  RestoreA5;			{ never call EXIT in this routine }
END;

{-------------------------------}
{ StartActual			}
{-------------------------------}

PROCEDURE StartActual;
VAR
  n: LONGINT;
BEGIN
{ the "record len" parameter for a free-form synthesizer should 
  cover /only/ the data area and not the 6-byte header, despite
  implications otherwise in IM.  Otherwise the first few random bytes 
  following the data produce a nasty noise spike.  }
  
{ n := BAND (sheadp^.dlen, $0FFFF) + MSHLEN; }
  n := BAND (sheadp^.dlen, $0FFFF);
  StartSound (Ptr(soundp), n, @ComplSnd);
END;




{-------------------------------}
{ GTAWord			}
{-------------------------------}

{ pick up the word at the specified loc }

FUNCTION GTAWord (loc: LONGINT): INTEGER;
VAR p: ^INTEGER;
BEGIN
  p := Pointer (loc);
  GTAWord := p^;

(*  This code, intended to pick up only a byte, still tries to read a word, 
    and crashes because of the odd address.  Why?
  VAR p: ^char;
  p := Pointer ($400009);
  IF char (p^) = char ($FF) THEN ...
*)
END;

{-------------------------------}
{ OnLisa			}
{-------------------------------}

FUNCTION OnLisa: BOOLEAN;	{ check if running on Lisa [was in 68K] }
CONST 
  MACID = $400008;		{ magic ROM addr }
VAR id: INTEGER;
BEGIN
  id := GTAWord (MACID);	{ machine id + ROM version }

  IF BAND (id, $FF) = $FF	{ check /odd/ byte for Lisa-ness }
    THEN OnLisa := TRUE		{ bingo }
  ELSE OnLisa := FALSE;
END;

{-------------------------------}
{ OnMac2			}
{-------------------------------}

FUNCTION OnMac2: BOOLEAN;
CONST 
  ROM85 = $028E;		{ a different magic addr (in RAM) }
VAR id: INTEGER;
BEGIN
  id := GTAWord (ROM85);

{ We want to write "IF id > $3FFF", but can't because the Pascal 
  comparison is signed ... }

  IF (id = $FFFF) OR (id = $7FFF)	
    THEN OnMac2 := FALSE
  ELSE OnMac2 := TRUE;		{ "Mac2 or later" }
END;

{-------------------------------}
{ BellSound			}
{-------------------------------}

PROCEDURE BellSound (id: INTEGER);	{ beep or boop }
CONST
  buffSize = 4;
VAR
  swBuff: ARRAY [1..buffSize] OF INTEGER;  { square wave rec, 1 element }
BEGIN
  IF id = 1 THEN 
  BEGIN
    SysBeep (8);	{ normal beep }
    EXIT (BellSound);
  END;

  IF id = 2 THEN 
  BEGIN
    IF OnLisa THEN 
      SysBeep (16)	{ Lisa's boop -- only certain way to make sound }
    ELSE
(***  SysBeep (16);	{ Mac: but avoid MPW 1.0 sound driver bug } ***)
    BEGIN		{ Mac: real boop }
      swBuff[1] := swMode;	{ mode is square wave }
      swBuff[2] := $B97;	{ middle C }
      swBuff[3] := 128;		{ amplitude 0-255 }
      swBuff[4] := 8;		{ duration in ticks }
      StartSound (@swBuff, 2*buffSize, Ptr(-1));
    END;
  END;
END;

{-------------------------------}
{ ZSound			}
{-------------------------------}

CONST
  S_INIT = 1;
  S_START = 2;
  S_STOP = 3;
  S_CLEANUP = 4;

VAR
  lastID: INTEGER;
  sInited, sStarted: BOOLEAN;

PROCEDURE ZSound (id, action, vol, reps: INTEGER);
VAR
  dfname: STR32;	{ sound data file name }
  err: INTEGER;

BEGIN  
  IF sheadp = NIL THEN		{ make sure buffers exist }
    EXIT (ZSound);
  IF soundp = NIL THEN
    EXIT (ZSound);

  IF (id = 1) OR (id = 2) THEN
  BEGIN
    BellSound (id);		{ beep/boop }
    EXIT (ZSound);
  END;

  IF id = 0 THEN		{ use MRU effect }
  BEGIN
    IF lastID = 0 THEN		{ none, exit }
      EXIT (ZSound)
    ELSE id := lastID;
  END
  ELSE IF id <> lastID THEN	{ first clean up old, if any }
  BEGIN
    ZSound (lastID, S_CLEANUP, vol, reps);	{ (recurse) }
    lastID := id;
  END;

  IF (action = S_INIT) OR (action = S_START) THEN
    IF NOT sInited THEN
    BEGIN
      err := InitMidi (id, dfname);	{ if error, just use defaults }
      err := InitData (dfname);
      IF err <> 0 THEN
      BEGIN
        InitCleanUp;			{ Mac: close files immed }
        EXIT (ZSound);
      END;
      sInited := TRUE;
    END;
  
  IF action = S_START THEN
  BEGIN
    StopSnd;				{ Mac: reset if in middle }
    StartSnd (vol, reps);
    sStarted := TRUE;
  END;

  IF (action = S_STOP) OR (action = S_CLEANUP) THEN
    IF sStarted THEN
    BEGIN
      StopSnd;
      sStarted := FALSE;
    END;
  
  IF action = S_CLEANUP THEN
  BEGIN
    IF sInited THEN
    BEGIN
      InitCleanUp;
      sInited := FALSE;
    END;
    lastID := 0;		{ back to original state }
  END;
END;

{-------------------------------}
{ ZEndSound			}
{-------------------------------}

{ called from 68K, returns TRUE to report end of last sound }

FUNCTION ZEndSound: BOOLEAN;
BEGIN
  ZEndSound := sdone;
END;

{-------------------------------}
{ Main				}
{-------------------------------}

BEGIN
  MoreMasters;
  InitGraf (@thePort);
  InitWindows;
  InitFonts;
  InitMenus;
  InitCursor;
  FlushEvents (everyEvent,0);

{ one-time allocation of buffers for sound data }
  sheadp := SHeaderPtr (NewPtr(ASHLEN));
  soundp := FFSynthPtr (NewPtr(MSLEN));
  
  mfchan := 0;		{ mark as unopened }
  dfchan := 0;

  lastID := 0;
  sInited := FALSE;
  sStarted := FALSE;

  GetSoundVol (uservol);	{ remember user's setting }

  ZSound (99, S_START, -1, -1);
  WHILE NOT ZEndSound DO	{ for now, just busy loop ... }
    BEGIN
    END;
  ZSound (99, S_CLEANUP, -1, -1);

END.

