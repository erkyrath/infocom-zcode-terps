
{=======================================}
{ GRAPHICS ROUTINES			}
{=======================================}

{ these routines work with compressed, multi-picture gfx files }

CONST
    MAXGFXCOLORS = 16;
    GHEADLEN = 16;	{ length of gfx megafile header }

{ header flag bits }

    GF_EHUFF = $02;	{ dir entries include Huff ptrs (may be zero) }
    GF_GHUFF = $04;	{ global Huff ptr is included in file header }

{ define some "custom" flag bits (high so won't clash) }

    GF_SPLASH = $80;	{ startup screen, displayed while game loads }

{ entry flag bits }

    GF_TRANS = $0001;	{ this pic uses Color 0 for "transparent" }
    GF_PHUFF = $0002;	{ this pic is huffed }
    GF_XOR = $0004;	{ this pic was XORed every second line }
    GF_MONO = $0008;	{ this pic is mono, and scaled for a 480x300 screen (std Mac) }

{ PICSET stuff }

{ Note: could instead get picset buflen from gfx_Header (adjusted for each game), 
  OR could set it arbitrarily large, based on available memory. }
    PSDEFLEN = 5*1024;	{ default buflen }
    MAXITEMLEN = 1024;	{ "prevent loading of inappropriate (too big) pics" }
    
    PSENTRYLEN = 6;	{ 3 words (for now), id/off/len }
    PSINITMARK = 987;	{ magic number }
	

TYPE
    WPtr = ^INTEGER;
    LPtr = ^LONGINT;

    gfx_Header =	{ file's master header }
    RECORD		
	gflags:	INTEGER;	{ file id (byte), global flags (byte) }
	ghuffOff:
		LONGINT;	{ global huff ptr [optional] }
	npics,			{ number of local directory entries }
	nxpics:	INTEGER;	{ [Mac: unused] }
	entryLen,		{ length of each entry (12-14-16) }
	cksum,
	version:		{ current version of gfx file format }
		INTEGER;	

    { additional fields }
	inited:	BOOLEAN;	{ true after successful init }
	dirLen:	LONGINT;	{ length of entire directory }
	LBYTES:	INTEGER;	{ #bytes to specify data len (starting at each dataOff) }
    END;
    
    gfx_Entry =		{ one directory entry (in gfx megafile) }
    RECORD
	id:	INTEGER;
	rx,
	ry:	INTEGER;	{ raw size (from disk) }
	eflags:	INTEGER;	{ flags for this entry  [vs. header flags] }
	dataOff,
	palOff,
	huffOff:
		LONGINT;
    { additional fields }
	trans,			{ [copy of GF_TRANS flag] }
	mono,			{ [copy of GF_MONO flag] }
      { fcolor, }
	fstip:	BOOLEAN;	{ set if stippling active }
	x,
	y:	INTEGER;	{ internal size (after shrinking, stips) }
	depth:	INTEGER; 	{ 4 (or 1 if stippled) }
	rowBytes:  INTEGER;
	dx,
	dy:	INTEGER;	{ display size; may be scaled (1.5, 2.0) }
    END;

    RGB_24 = PACKED ARRAY[1..3] of char;	{ 24 bits }
    gfx_Palette =	{ (potentially) shared color/stipple tables }
    RECORD
	pOff:	LONGINT;
	pSiz:	INTEGER;
	palette:
		PACKED ARRAY[1..3*MAXGFXCOLORS] of char;
	     {	PACKED ARRAY[1..MAXGFXCOLORS] of RGB_24;  }
	sSiz:	INTEGER;
	stips:	PACKED ARRAY[1..MAXGFXCOLORS] of char;	{ stipple id table }
    { extended stipple table (shifted & packed Values - 4 sets of Words) }
	exstips:
		PACKED ARRAY[1..MAXGFXCOLORS*2*4] of char;
    END;

    gfx_Hufftree =	{ (potentially) shared huffman data }
    RECORD
	hOff:	LONGINT;
	hufftree:
		PACKED ARRAY[1..256{max}] of char;
    END;

    gfx_Picset =	{ picture buffering vars }
    RECORD
    	psInited: INTEGER;	{ marked after successful init }
	psPtr: Ptr;		{ overall ptr; id stuff starts here }
	psLen: LONGINT;		{ overall (allocated) len }
	psData: Ptr;		{ data starts here [recalced for each picset] }
	
	p: Ptr;			{ search results returned here }
	len: LONGINT;
    END;

    gfx_Data = 
    RECORD		{ globals for data buffer }
	pMin:	Ptr;
	minSize,		{ fully compressed size }
	midSize,		{ size AFTER dehuff, but BEFORE derun }
	maxSize: LONGINT	{ fully decompressed size (expected) }
	END;

    uncompRec = 
    RECORD		{ this block passed to UncompH }
	inbuf,
	outbuf,
	huff_tree:	Ptr;
	pic_x,				{ pixel width }
	midlen,				{ after unhuff, before de-run }
	outbuflen:	LONGINT;	{ "interrupt count" }
	firstCall,			{ raised by caller, initially }
	lastCall:	Boolean;	{ raised by callee, when done }
	saveRegs:	ARRAY[1..16] OF LONGINT;	{ 16x4 bytes max }
	END;
    uncompRecPtr = ^uncompRec;

VAR
    gfxChan:	INTEGER;	{ gfx megafile }
    dirPtr:	Ptr;		{ gfx directory, in core }
    
    gh:		gfx_Header;
    ge:		gfx_Entry;
    gp:		gfx_Palette;
    gt:		gfx_Hufftree;
    ps:		gfx_Picset;
    gd:		gfx_Data;

{-------------------------------}
{ PutWord, PutLong		}
{-------------------------------}

{ Following are some useful, general-purpose routines }

PROCEDURE PutWord (VAR p: Wptr; word: INTEGER);		{ store word, advance ptr }
BEGIN
	p^ := word;
	p := WPtr (ORD4(p) + 2);
END;

(**
PROCEDURE PutLong (VAR p: Lptr; long: LONGINT);		{ store long, advance ptr }
BEGIN
	p^ := long;
	p := LPtr (ORD4(p) + 4);
END;
**)

{-------------------------------}
{ GetWord, GetWordB, GetLong	}	{ see also GTAWord }
{-------------------------------}

FUNCTION GetWord (VAR p: Wptr): INTEGER;		{ return word, advance ptr }
BEGIN
	GetWord := p^;
	p := WPtr (ORD4(p) + 2);
END;

{ use this call if the word might be odd-aligned }
(**
FUNCTION GetWordB (VAR p: Wptr): INTEGER;		{ return word, advance ptr }
BEGIN
	GetWordB := BOR (BSL (Ptr(p)^, 8), 
			 BAND (Ptr (ORD4(p)+1)^, 255));
	p := WPtr (ORD4(p) + 2);
END;

FUNCTION GetLong (VAR p: Lptr): LONGINT;		{ return long, advance ptr }
BEGIN
	GetLong := p^;
	p := LPtr (ORD4(p) + 4);
END;
**)

{-------------------------------}
{ RoundUp, RoundDown		}
{-------------------------------}

FUNCTION RoundUp (n: LONGINT): LONGINT;
BEGIN
	IF Odd (n) THEN RoundUp := n+1 ELSE RoundUp := n;
END;

FUNCTION RoundDown (n: LONGINT): LONGINT;
BEGIN
	IF Odd (n) THEN RoundDown := n-1 ELSE RoundDown := n;
END;

{-------------------------------}
{ LongMult			}	{ "LongMul" causes a conflict }
{-------------------------------}

{ use this call if a product of two integers might exceed 32K, 
  to prevent bad Pascal sign-extension }
  
FUNCTION LongMult (a, b: INTEGER): LONGINT;
BEGIN
	LongMult := LONGINT (a) * LONGINT (b);
END;

{-------------------------------}
{ FSReadGFX, FSReadClr		}
{-------------------------------}

{ read from gfx file, starting from given offset (-1 means current) }

FUNCTION FSReadGFX (off, len: LONGINT; p: Ptr): osErr;
BEGIN
	IF off <> -1 THEN
  		IF SetFPos (gfxChan, FSFROMSTART, off) <> noErr THEN
			BEGIN
			FSReadGFX := -1;	{ report an error }
			EXIT (FSReadGFX);
			END;

	FSReadGFX := FSRead (gfxChan, len, p);	
END;

{ a handy func to Read and right-justify a value, of len < n bytes,
  into a n byte field.  The unused bytes are zeroed. }

FUNCTION FSReadClr (off, len: LONGINT; p: Ptr; n: LONGINT): osErr;
BEGIN
	n := n - len;
	WHILE n > 0 DO				{ clear the high bytes }
		BEGIN
		p^ := 0;
		p := Ptr (ORD4(p) + 1);
		n := n - 1;
		END;

	FSReadClr := FSReadGFX (off, len, p);	{ then read as usual }
END;

{-------------------------------}
{ varCopy, varCopyClr		}
{-------------------------------}

PROCEDURE varCopy (p1, p2: Ptr; n: INTEGER);
BEGIN
	WHILE n > 0 DO
		BEGIN
		p2^ := p1^;
		p1 := Ptr (ORD4(p1) + 1);
		p2 := Ptr (ORD4(p2) + 1);
		n := n - 1;
		END;
END;

{ a handy func to Copy and right-justify a value, of len < ntot bytes,
  into a ntot-byte field.  The unused bytes are zeroed. }

PROCEDURE varCopyClr (p1, p2: Ptr; n, ntot: INTEGER);
BEGIN
	ntot := ntot - n;
	WHILE ntot > 0 DO			{ clear the high bytes in dest }
		BEGIN
		p2^ := 0;
		p2 := Ptr (ORD4(p2) + 1);
		ntot := ntot - 1;
		END;

	varCopy (p1, p2, n);	{ then copy as usual }
END;

{-------------------------------}
{ ReadGFXEntry			}
{-------------------------------}

FUNCTION binary_search (dir: Ptr; e_count, e_size, id: LONGINT): Ptr;  
	C; EXTERNAL;

{ read [into master record] a local directory entry }

FUNCTION ReadGFXEntry (id: INTEGER): OsErr;
VAR
	pEnt, pRec: Ptr;
	curDepth: INTEGER;

PROCEDURE doCopy (n: INTEGER);
BEGIN
	WHILE n > 0 DO
		BEGIN
		pRec^ := pEnt^;
		pEnt := Ptr (ORD4(pEnt) + 1);	
		pRec := Ptr (ORD4(pRec) + 1);
		n := n - 1;
		END;
END;
PROCEDURE doZero (n: INTEGER);
BEGIN
	WHILE n > 0 DO
		BEGIN
		pRec^ := 0;
		pRec := Ptr (ORD4(pRec) + 1);
		n := n - 1;
		END;
END;

BEGIN
	ReadGFXEntry := -1;			{ default }
	IF NOT gh.inited
		THEN EXIT (ReadGFXEntry);	{ make sure file opened okay }

	IF ge.id = id THEN			{ same id as last time? }
		BEGIN
		ReadGFXEntry := 0;		{ yes, all done }
		EXIT (ReadGFXEntry);
		END;
		
	pEnt := binary_search (dirPtr, gh.npics, gh.entryLen DIV 2, id);
	IF pEnt = NIL
		THEN EXIT (ReadGFXEntry);

	pRec := @ge;
	doCopy (8);				{ read first 4 fields }

	ge.trans := BAND (ge.eflags, GF_TRANS) <> 0;
	ge.mono := BAND (ge.eflags, GF_MONO) <> 0;

	doZero (1);
	doCopy (3);				{ read/align dataOff }

	{ is there a palette entry? }
	IF ge.mono THEN				{ NO [implied] }
		BEGIN
		doZero (4);
		pEnt := Ptr (ORD4(pEnt) + 1);	{ skip over /single/ pad byte }
		END
	ELSE					{ Yes, read/align palOff }
		BEGIN
		doZero (1);
		doCopy (3);
		END;

	IF BAND (gh.gflags, GF_EHUFF + GF_GHUFF) = GF_EHUFF {E on, G off?} THEN
		BEGIN
		doZero (2);
		doCopy (2);			{ read/align local huff ptr }
		ge.huffOff := 2 * ge.huffOff;	{ [make byte offset] }
		END
	ELSE
		ge.huffOff := gh.ghuffOff;	{ else default to global ptr }

	ge.y := ge.ry;		{ initially, set to AM values }
	ge.x := ge.rx;
	ge.depth := 4;  

	IF ge.mono THEN		{ special pic (mono, sized) for std Mac? }
		ge.fstip := FALSE
	ELSE
		BEGIN
		IF myCW  			{ get current depth }
			THEN curDepth := CGrafPtr(myWindow)^.portPixMap^^.pixelSize	
			ELSE curDepth := 1;
		{ detect if stippling needed }
		ge.fstip := (curDepth = 1);
		END;

{ must be a multiple of /4/ for in-place stippling (divides it by 2) }
	ge.rowbytes := CalcRow4Bytes (ge.rx, GFXAM_DEPTH);

{ set dy/dx now, so PICINF will report correctly }

	IF ge.mono OR myTiny THEN		{ scale 1x for display }
		BEGIN
		ge.dy := ge.ry;
		ge.dx := ge.rx;
		END
	ELSE IF myBig THEN			{ scale 2x for display }
		BEGIN
		ge.dy := ge.ry * 2;
		ge.dx := ge.rx * 2;
		END
	ELSE					{ scale 1.5x for display }
		BEGIN
		ge.dy := (ge.ry * 3) DIV 2;
		ge.dx := (ge.rx * 3) DIV 2;
		
		IF ge.fstip THEN		{ must be even, round down }
			BEGIN
			ge.dy := RoundDown (ge.dy);	
			ge.dx := RoundDown (ge.dx);
			END;
		END;

	ReadGFXEntry := 0;			{ all okay }
END;

{-------------------------------}
{ ReadGFXPalette		}
{-------------------------------}

PROCEDURE BuildStips (table1, table2: Ptr);
	EXTERNAL;  {68K}
PROCEDURE FillCSpec (CSpec: Ptr; ix: INTEGER; bytePal: Ptr);
	EXTERNAL;  {68K}

{ read and setup palette (color or stipple); also read huffman tree }

FUNCTION ReadGFXPalette: OSErr;
VAR
	n: INTEGER;
	p: Ptr;
	myPal: PaletteHandle;
	
{ a local procedure }
PROCEDURE doRead (off, n: LONGINT; p: Ptr);
BEGIN
	IF FSReadGFX (off, n, p) <> noErr THEN EXIT (ReadGFXPalette);
END;

BEGIN
    ReadGFXPalette := -1;			{ default val }

    IF (ge.palOff <> 0) AND (gp.pOff <> ge.palOff) THEN	  { get new palette }
        BEGIN
	gp.psiz := 0;				{ zero high byte }
	p := Ptr (ORD4(@gp.psiz) + 1);	
	doRead (ge.palOff, 1, p);		{ read low byte }
	IF gp.psiz > MAXGFXCOLORS
		THEN EXIT (ReadGFXPalette);
	doRead (-1, 3*gp.psiz, @gp.palette);

{ We ALWAYS read and process the stipple data.  It doesn't take much time, and 
  it means we are always ready to switch into stipple mode if the user changes
  the screen.  Note: when all games have their own b/w files, we should avoid 
  this in b/w mode. }

	gp.ssiz := 0;				{ zero high byte }
	p := Ptr (ORD4(@gp.ssiz) + 1);	
	doRead (-1, 1, p);			{ read low byte }
	IF gp.ssiz > MAXGFXCOLORS
		THEN EXIT (ReadGFXPalette);
	doRead (-1, gp.ssiz, @gp.stips);
	
	{ use stipple palette to build extended/value table } 
	BuildStips (@gp.stips, @gp.exstips);

{ As of now, we process the color data ONLY if we're currently in color mode, 
  not stipple mode.  Means that switching from stips to color in mid-game will
  temporarily (until the next palette is read) display strange colors. }

	IF NOT ge.fstip THEN		{ AND NOT ge.mono  [implicit from paloff check] }
	{ use zcolor palette to build custom color table }
		BEGIN
		WITH gfxPM.pmTable^^ DO  { dereference outside of loop }
			BEGIN
			p := @gp.palette;
		{ don't touch initial, final entries (set up during init)
		  Final color table layout is: 1, 2-15, 0 (see IM V-158) }
			FOR n := 1 TO MAXGFXCOLORS-2 DO	
				BEGIN
				{ first two args are 0-origin }
				FillCSpec (@ctTable[n], n+1, p);				
				p := Ptr (ORD4(p) + 3);	

(**)			{ install in altscreen too, for Blits }
			{ Otherwise, updates map all colors to Black ... }
				altPM^^.pmTable^^.ctTable[n] := ctTable[n];
(**)
				END;

		{ tell system that colors may have changed }
      			ctSeed := GetCTSeed;
(**)			altPM^^.pmTable^^.ctSeed := GetCTSeed;  (**)

		{ if gp.psiz < 16, should it be copied into palette? }
			END;
	
	{ go whap on palette -- install new colors with zero tolerance }
		myPal := GetPalette (myWindow);	{ get default [from GetNewCWindow] }
	(**)	SetPalette (myWindow, myPal, TRUE {FALSE}); (**)  { DO get color updates }

		CTab2Palette (gfxPM.pmTable, myPal, pmTolerant, $0000);
		ActivatePalette (myWindow);
		END;
	gp.pOff := ge.palOff;		{ remember which palette this is }
	END;

    IF BAND (ge.eflags, GF_PHUFF) <> 0 THEN 	{ is /this/ pic huffed? }
	IF gt.hOff <> ge.huffOff THEN		{ tree not already loaded? }
		BEGIN
		doRead (ge.huffOff, 256 {max}, @gt.hufftree);
		gt.hOff := ge.huffOff;		{ remember which tree this is }
		END;

    ReadGFXPalette := 0;			{ all okay }
END;

{-------------------------------}
{ ReadGFXData			}
{-------------------------------}

FUNCTION searchPicset (id: INTEGER): osErr;
	FORWARD;
FUNCTION UncompH (p: uncompRecPtr): LONGINT; EXTERNAL;		{ now in 68K, for speed }
(**
FUNCTION uncompress_huff (inbuf, outbuf, huff_tree: Ptr; 
	inlen, pic_x: LONGINT): LONGINT;  C; EXTERNAL;	{ dead }
FUNCTION uncompress_nohuff (inbuf, outbuf: Ptr; 
	inlen, pic_x: LONGINT): LONGINT;  C; EXTERNAL;  **)

{ Read in (or find in PICSET) the data for one picture, then decompress it.
  A single large buffer, allocated at startup and shared with BLITBITS, is used 
  for all operations.  By avoiding dynamic allocations, we guarantee that 
  PICDISP will never fail due to an out-of-memory error.  By sharing with BlitBits,
  we minimize gfx memory appetite.
  
  The size of the buffer is equal to the maximum size of a decompressed picture
  (plus one extra scanline).  (For a monochrome screen, the buffer also includes 
  room for a /simultaneous/ full screen blit, used for Transparent mode.) 
  The data (if not found in PICSET) is initially read into the high end 
  of the buffer, then decompressed into the low end. }
  
FUNCTION ReadGFXData: OSErr;
VAR
	p:	Ptr;
	dummy:	INTEGER;
	max2:	LONGINT;		{ fully decompressed size (actual) }
	ucr:	uncompRec;
BEGIN
	ReadGFXData := -1;			{ default val }

	IF searchPicset (ge.id) = 0 THEN	{ first, check cache }
		BEGIN
		gd.pMin := ps.p;
		gd.minSize := ps.len;
		END
	ELSE
		BEGIN
		IF FSReadClr (ge.dataOff, gh.LBYTES, @gd.minSize, 4) <> noErr	{ read size bytes }
			THEN EXIT (ReadGFXData);
		IF gd.minSize > altLen			{ fits in buffer? }
			THEN EXIT (ReadGFXData);

{ For YZIP, always call CheckMyUpdate before doing any output.  The case of graphics output
  is tricky:
    - must call /before/ gfx data is read into altbuf, since that action trashes the screen
  image saved in the shared buffer!
    - must call /after/ any call that could trigger a disk-swap request.  These include:
        * ReadGFXEntry - "never" (directory always preloaded)
	* ReadGFXPalette - "sometimes" (only if different than previous)
	* 2-byte size word, above - "usually" (unless cached by OS)
	
    - in the nasty worst case, none of the above triggers the request, but reading the data
  itself does.  Since caching is MRU (presumably?), this could happen only if the 'size word'
  existed withing the same block as a more recent picture with a slightly lower id.  COULD 
  test for this case by touching the next disk block with a special read:
		dummy := FSReadGFX (ge.dataOff+512 (1K? 2K?), 2, @dummy);
		
  but this would routinely thrash (hiccup) the disk for every picture (or only pics that 
  crossed tracks?  How about effect on load speed?)
  Bottom line - we don't check for this, and hope it happens rarely or never.  The ultimate 
  solution would be to have a completely independant buffer for gfx (or none at all, and output 
  directly to screen -- difficult on Mac).
}
		CheckMyUpdate;			

		{ make sure gd.pMin is word-aligned, for reading speed }
		gd.pMin := Ptr (ORD4(altPtr) + (altLen - RoundUp(gd.minSize)));
		{ read in the (compressed) data }
		IF FSReadGFX (-1, gd.minSize, gd.pMin) <> noErr 	
			THEN EXIT (ReadGFXData);
		END;	{ of IF searchPicset ... }

{ decompress (into bytes).  actual data starts at second row.
  For now, we've stopped supporting the not-huffed case.  If this becomes a problem,
  need only to create an UNCOMPNH based on UNCOMPH. }

	gd.maxSize := LONGINT (ge.rx) * LONGINT (ge.ry);	{ in bytes }

	IF BAND (ge.eflags, GF_PHUFF) = 0 THEN
		BEGIN
	(**	max2 := uncompress_nohuff (gd.pMin, altPtr, gd.minSize, ge.rx) **)
		EXIT (ReadGFXData);		{ "not handled" }
		END;

	varCopyClr (gd.pMin, @gd.midSize, gh.LBYTES, 4);
	gd.pMin := Ptr (ORD4(gd.pMin) + gh.LBYTES);

{ [for mono/stips, decompression and display processing are now combined, and handled elsewhere.] }
	IF ge.mono OR ge.fstip THEN
		BEGIN
		ReadGFXData := 0;		{ [not an error] }
		EXIT (ReadGFXData);
		END;

	IF gd.maxSize + ge.rx > altLen THEN	{ fits in buffer? }
		BEGIN
		IF gfxDebug			{ signal an error ... }
			THEN SysBeep (4);
		EXIT (ReadGFXData);		{ avoid a crash }
		END;

	WITH ucr DO
		BEGIN
		inbuf	:= gd.pMin;
		outbuf	:= altPtr;
		huff_tree  := @gt.hufftree;
		pic_x	:= ge.rx;
		midlen	:= gd.midSize {gd.minSize};
		outbuflen  := altlen;	{ no interrupt for color }
		firstCall  := TRUE;
		lastCall   := FALSE;
		END;

	max2 := UncompH (@ucr);

	IF max2 <> gd.maxSize THEN		{ /should/ be exact pic size }
		BEGIN
		IF gfxDebug
			THEN SysBeep (4);
		{ EXIT (ReadGFXData); }	{ [don't abort for this one] }
		END;

	ReadGFXData := 0;		{ return with no errors }
END;

{-------------------------------}
{ CopyPic			}
{-------------------------------}

{ copy to OR from the screen }
{ >> careful: pass Pixmaps by name not value, else chopped ! << }
{ (pass rects by name, only to avoid making copies) }

PROCEDURE CopyPic (srcBits, dstBits: BitMapPtr; VAR r1, r2: Rect);
VAR
	mode: INTEGER;
BEGIN
	mode := SRCCOPY;			{ default }
	IF NOT (ge.fstip OR ge.mono) 		{ i.e., if color }
	    THEN IF ge.trans	{ color 0 means "transparent" }
		THEN mode := 36 {TRANSPARENT};
		{ use transIndex (in ColorTable) instead ?? }
	
	{ draw (a piece of) the bitmap to the screen }
	ZCopyBits (srcBits, dstBits, r1, r2, mode, NIL);
END;

{-------------------------------}
{ ShowGFXM			}
{-------------------------------}

CONST
	DECOMP_LEN 	= 8*1024;	{ ~16 rows x 480 }
	DECOMP_MINLEN	= 4*1024;
TYPE
    MonoPicRec = 
    RECORD		{ this block passed to Monopic }
	srcX,			{ in pixels }
	srcY,
	dstRB:	INTEGER;	{ in bytes }
	trans:	Boolean;	{ transparency flag }
	psrc,			{ note: src rows are unpadded }
	pdst:	Ptr;		{ [return updated ptr] }
	END;
    MonoPicRecPtr = ^MonoPicRec;

PROCEDURE MonoPic (p: MonoPicRecPtr);  EXTERNAL;  {68K}
(** PROCEDURE MonoPic (srcX, srcY, dstRB: INTEGER; src, dst: Ptr); EXTERNAL;  {68K} **)

{ Display, in straight Mono, (a section of) a picture, data in altbuf;
  position given in pixel units.
  
  Because a mono pic decompresses into so many bytes (144000 prior to MonoPic), we
  now break up decompression and mono rendering into smaller chunks.  To minimize overhead
  lossage,  chunks shouldn't be too small.  Our buffer is partitioned as follows:
  
  ---------------------------------------
  |  Transbuf  (18000 bytes)		|
  |					|
  ---------------------------------------
  |  Outbuf  (DECOMP_LEN or more)	|
  ---------------------------------------
  |  Inbuf  (18000 bytes or less)	|
  |					|
  ---------------------------------------	}

PROCEDURE ShowGFXM (ypos, xpos: INTEGER);
VAR
	r1, r2:		rect;
	pDecomp,			{ was pMax }
	pTrans:		Ptr;
	max2,				{ fully decompressed size (actual) }
	decompLen,			{ space available for decompression }
	decompLen1,			{   ... rounded down to exact multiple of picx }
	decompRet,			{ #bytes returned in latest decomp buffer }
	transLen:	LONGINT;	{ length of transparency buffer }
	decompRows:	INTEGER;	{ #rows returned per UncompH call }
	unroll:		Boolean;
	ucr:	uncompRec;
	mpr:	MonoPicRec;
(**	debug_ticks:	LONGINT;   **)	{ FOR TIMING TESTS ONLY }
BEGIN
(**	debug_ticks := TickCount;  **)
	
	ge.depth := 1;  			{ adjust for mono }
	ge.rowbytes := CalcRowBytes (ge.x, 1);	{ recalc this one }

{ calc rects (for "straight mono", always scaled 1.00) }

	SetRect (r1, 0, 0, ge.x, ge.y);
	r2 := r1;
(**	SetRect (r2, 0, 0, ge.dx, ge.dy);  **)	{ same }
	OffsetRect (r2, wMarg + xpos, ypos);

(**	rawlen {gd.maxSize} := LongMult (ge.x, ge.y); **)  { DON'T CARE, use chunks now } 
	transLen := LongMult (ge.y, ge.rowbytes);
	decompLen := altLen - (transLen + gd.minSize);
	IF decompLen < DECOMP_MINLEN THEN  	{ minSize was too big! }
		BEGIN
		IF gfxDebug			{ signal an error ... }
			THEN SysBeep (4);
		EXIT (ShowGFXM);
		END;

{ allow one extra line for unEOR, plus 128 bytes max for unRun spillover }
	decompRows := (decompLen - (ge.x + 128)) DIV ge.x;
	IF decompRows > ge.y THEN		{ don't exceed actual pic height }
		decompRows := ge.y;
{ further strategy: if it's a "big" pic, then limit decompRows to a small number and 
  incrementally "unroll" the actual display, to avoid a long dead moment.  The smaller
  decompRows is, the smoother the unroll but the bigger the overhead.
  
  Experiments with "debug_ticks" (on a Mac 512K) show that dividing a 480x300 pic into
  n-row chunks increases the total display time as follows:
  	n=300	0
  	n=4	0.23 sec
	n=2	0.45 sec
	n=1	0.90 sec	(ouch)
}
  	IF translen < (18000 DIV 4) THEN	{ is pic smaller than 1/4 screen? }
		unroll := FALSE			{ don't bother to unroll }
	ELSE
		BEGIN
		IF decompRows > 2 {4} THEN	{ n=2 looks quite smooth }
			decompRows := 2 {4};
		unroll := TRUE;
		END;  
	decompLen1 := LongMult (decompRows, ge.x);
	
	pTrans := altPtr;
	pDecomp := Ptr (ORD4(altPtr) + transLen);

	WITH gfxPM DO
		BEGIN
		baseAddr := pTrans;	{ set gfxPM base to Trans base }
		rowBytes := ge.rowBytes;
		bounds := r1;
		END;

	{ if Transparent, blit in (entire) old pic first }
	IF ge.trans THEN
		CopyPic (@thePort^.portBits, BitMapPtr(@gfxPM), r2, r1);
	IF unroll THEN
		BEGIN
		r1.bottom := r1.top;	{ setup for unroll }
		r2.bottom := r2.top;
		END;

	WITH ucr DO
		BEGIN
		inbuf	:= gd.pMin;
		outbuf	:= pDecomp  {altPtr};
		huff_tree  := @gt.hufftree;
		pic_x	:= ge.rx;
		midlen	:= gd.midSize {gd.minSize};
		outbuflen  := decompLen1  {altlen};	{ interrupt point }
		firstCall  := TRUE;
		lastCall   := FALSE;
		END;
	WITH mpr DO
		BEGIN
		srcX := ge.x;
		srcY := decompRows  {ge.y};
		dstRB := ge.rowbytes;
		trans := ge.trans;
		psrc := Ptr (ORD4(pDecomp) + ge.rx);	{ 1st row is unEOR hack }
		pdst := pTrans;		{ [returns updated ptr] }
		END;

	max2 := 0;
	WHILE NOT ucr.lastCall DO
		BEGIN
		decompRet := UncompH (@ucr);
		max2 := max2 + decompRet;
		
		IF ucr.lastCall THEN	{ final chunk probably has fewer rows ... }
			mpr.srcY := decompRet DIV ge.x;
		MonoPic (@mpr);

		IF unroll THEN 		{ blit new pic to screen incrementally }
			BEGIN
			r1.top := r1.bottom;
			r1.bottom := r1.bottom + mpr.srcY {decompRows};
			r2.top := r2.bottom;
			r2.bottom := r2.bottom + mpr.srcY;

			CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
			END;
		END;
		
	IF max2 <> gd.maxSize THEN		{ /should/ be exact pic size }
		BEGIN
		IF gfxDebug
			THEN SysBeep (4);
		{ EXIT (ReadGFXData); }		{ [but don't abort for this one] }
		END;

	IF NOT unroll THEN	{ blit new pic to screen, all at once }
		CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
	
(**	debug_ticks := TickCount - debug_ticks;  **)	{ time to decomp/display pic }
END;

{-------------------------------}
{ ShowGFXS			}
{-------------------------------}

PROCEDURE Shrink75 (base: Ptr; cols100, cols75, rows75: INTEGER);
	EXTERNAL;  {68K}
PROCEDURE StipPic (srcX, srcY, 		{ color, 1 byte/pixel, [clipped if necessary] }
	dstRB: INTEGER;			{ mono rowBytes -- SRCX/4, ROUNDED UP & EVEN }
	src, dst, maps: Ptr;
	trans: BOOLEAN);
	EXTERNAL;  {68K}

{ display in Mono/Stipples (a section of) a picture, data in altbuf; position given in 
  pixel units.  MAY need to shrink (0.75 scaling), MUST stipple (auto 2.00 scaling).
  We now support incremental decompression/shrinkage/stippling/copybits, like the mono case.
  This routine is much too convoluted, should be broken up somehow (split off the 75 percent 
  case?). }

PROCEDURE ShowGFXS (ypos, xpos: INTEGER);
VAR
	r1, r2: rect;
	{ pMax, }
	pStips: Ptr;
	shrink: BOOLEAN;
	tx, ty, trb: INTEGER;	{ intermediate vals }
	{ rawlen, }
	stiplen: LONGINT;	{ length of stipple buffer }
	sx, sy, srb: INTEGER;	{ final stipple vals }

	pDecomp,			{ was pMax }
	pTrans:		Ptr;
	max2,				{ fully decompressed size (actual) }
	decompLen,			{ space available for decompression }
	decompLen1,			{   ... rounded down to exact multiple of picx }
	decompRet:	LONGINT;	{ #bytes returned in latest decomp buffer }
	decompRows,			{ #rows returned per UncompH call }
	decompR75:	INTEGER;	{ 75 percent of above }
	transLen:	LONGINT;	{ length of transparency buffer }
	unroll:		Boolean;
	ucr:	uncompRec;
	mpr:	MonoPicRec;
BEGIN
     (* pMax := Ptr (ORD4(altPtr) + ge.rx);  *)	{ start of byte data }

{ If stippling AND 1.50 scaling, reduce to 0.75 FIRST (so QD won't mung stips) 
  [if color, (would) never scale /down/, even for 1.50, just let QD expand]. }
	shrink := {ge.fstip AND} NOT myBig;
	
	ty := ge.y;
	tx := ge.x;
	trb := ge.rowBytes;

	IF shrink THEN
	{ recalc "internal" pic size, based on previously calc'ed display size }
		BEGIN
		ty := ge.dy DIV 2;
		tx := ge.dx DIV 2;
	{ new rowBytes must also be multiple of /4/, for in-place stippling }
		trb := CalcRow4Bytes (tx, GFXAM_DEPTH);
		END;

{ we now write into these 's' fields instead of the 'ge.' globals.  This is because
  if ShowGFXS is called twice in a row for the same picture, the globals got mashed. 
  [Removal of the (dead) 0.75 option would simplify things a lot.] }

	sy := ge.dy;			{ set final (post-stip) vals }
	sx := ge.dx;
	srb := trb DIV 2;
    (*  sdepth := 1; *)


	transLen := LongMult (sy, srb);
	decompLen := altLen - (transLen + gd.minSize);
	IF decompLen < DECOMP_MINLEN THEN  	{ minSize was too big! }
		BEGIN
		IF gfxDebug			{ signal an error ... }
			THEN SysBeep (4);
		EXIT (ShowGFXS);
		END;

{ display strategy: if it's a "big" pic, then limit decompRows to a small number and 
  incrementally "unroll" the actual display, to avoid a long dead moment.  The smaller
  decompRows is, the smoother the unroll but the bigger the overhead.
}
	unroll := TRUE;				{ default }
  	IF translen < (18000 DIV 4) THEN	{ is pic smaller than 1/4 screen? }
		BEGIN
	{ enough room to process in one pass? }
	{ allow one extra line for unEOR, plus 128 bytes max for unRun spillover }
	{ FOR STIPS, USE RAW LINE WIDTH HERE }
		decompRows := (decompLen - (ge.x + 128)) DIV ge.x;	{ max poss value }
		IF decompRows >= ge.y THEN	{ don't exceed actual pic height }
			BEGIN
			decompRows := ge.y;
			decompR75 := ge.dy;	{ use previously-calc'ed value }
			
			unroll := FALSE;
			END;
		END;
		
	IF unroll THEN
		BEGIN
		decompRows := 2;		{ n=2 looks quite smooth }
		decompR75 := 2;
		
		IF shrink THEN
			BEGIN
			decompRows := 4;	{ SHRINK75 fails if smaller! }
			decompR75 := 3;
			END;
		END;  
	decompLen1 := LongMult (decompRows, ge.x);
	
	pTrans := altPtr;
	pDecomp := Ptr (ORD4(altPtr) + transLen);

{ calc rects (for stips, always scaled 1.00) }

	SetRect (r1, 0, 0, sx, sy);
	r2 := r1;
	OffsetRect (r2, wMarg + xpos, ypos);

{ if Transparent, blit in (entire) old pic first }
	IF ge.trans THEN
		CopyPic (@thePort^.portBits, BitMapPtr(@gfxPM), r2, r1);

	IF unroll THEN
		BEGIN
		r1.bottom := r1.top;	{ setup for unroll }
		r2.bottom := r2.top;
		END;

	WITH ucr DO
		BEGIN
		inbuf	:= gd.pMin;
		outbuf	:= pDecomp  {altPtr};
		huff_tree  := @gt.hufftree;
		pic_x	:= ge.rx;
		midlen	:= gd.midSize {gd.minSize};
		outbuflen  := decompLen1  {altlen};	{ interrupt point }
		firstCall  := TRUE;
		lastCall   := FALSE;
		END;

	WITH mpr DO		{ for Stips, these are final display vals }
		BEGIN
		srcX := sx;
		srcY := decompR75  {sy};
		dstRB := srb;
		trans := ge.trans;
		psrc := Ptr (ORD4(pDecomp) + ge.rx);	{ 1st row is unEOR hack }
		pdst := pTrans;		{ [returns updated ptr] }
		END;

	stipLen := LongMult (sy, srb);

(**	rawlen {maxSize} := LongMult (ty, tx);		{ calc (shrunk) buflen; rows rounded DOWN }
	IF (rawlen + ge.rx) + stipLen > altLen THEN	{ too big! }
		BEGIN
		IF gfxDebug			{ signal an error ... }
			THEN SysBeep (4);
		EXIT (ShowGFXS);		{ avoid a crash }
		END;   **)

	{ set gfxPM base to 2nd buffer }
	pStips := Ptr (ORD4(altPtr) + (altLen - stipLen));

	WITH gfxPM DO
		BEGIN
		baseAddr := pStips;	{ set gfxPM base to Stips base }
		rowBytes := srb;
		bounds := r1;
		END;

	max2 := 0;
	WHILE NOT ucr.lastCall DO
		BEGIN
		decompRet := UncompH (@ucr);
		IF shrink THEN
			BEGIN
			Shrink75 (mpr.psrc {pMax}, ge.rx, tx, decompR75 {ty});  { works w/ BYTES }
			{ Clip75 ... alternate call }
			END;
		max2 := max2 + decompRet;
		
		IF ucr.lastCall THEN	{ final chunk probably has fewer rows ... }
			mpr.srcY := decompRet DIV ge.x;

		{ stipple/pad to 2nd buffer }	      
		StipPic (tx, decompR75 {ty}, trb DIV 2, 
			mpr.psrc {pMax}, mpr.pdst {pStips}, @gp.exstips, ge.trans);
	     (* MonoPic (@mpr); *)

		IF unroll THEN 		{ blit new pic to screen incrementally }
			BEGIN
			r1.top := r1.bottom;
			r1.bottom := r1.bottom + mpr.srcY {decompRows};
			r2.top := r2.bottom;
			r2.bottom := r2.bottom + mpr.srcY;

			CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
			END;
		END;
		
	IF max2 <> gd.maxSize THEN		{ /should/ be exact pic size }
		BEGIN
		IF gfxDebug
			THEN SysBeep (4);
		{ EXIT (ReadGFXData); }		{ [but don't abort for this one] }
		END;

	IF NOT unroll THEN	{ blit new pic to screen, all at once }
		CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);

END;

{-------------------------------}
{ oShowGFXS			}
{-------------------------------}

(** PROCEDURE Shrink75 (base: Ptr; cols100, cols75, rows75: INTEGER);
	EXTERNAL;  {68K}
PROCEDURE StipPic (srcX, srcY, 		{ color, 1 byte/pixel, [clipped if necessary] }
	dstRB: INTEGER;			{ mono rowBytes -- SRCX/4, ROUNDED UP & EVEN }
	src, dst, maps: Ptr;
	trans: BOOLEAN);
	EXTERNAL;  {68K}  **)

{ display in Mono/Stipples (a section of) a picture, data in altbuf;
  position given in pixel units }
{ MAY need to shrink (0.75 scaling), MUST stipple (auto 2.00 scaling) }

PROCEDURE oShowGFXS (ypos, xpos: INTEGER);
VAR
	r1, r2: rect;
	pMax, pStips: Ptr;
	shrink: BOOLEAN;
	tx, ty, trb: INTEGER;	{ intermediate vals }
	rawlen,
	stiplen: LONGINT;	{ length of stipple buffer }
	sx, sy, srb: INTEGER;	{ final stipple vals }
BEGIN
	pMax := Ptr (ORD4(altPtr) + ge.rx);	{ start of byte data }

{ If stippling AND 1.50 scaling, reduce to 0.75 FIRST (so QD won't mung stips) 
  [if color, (would) never scale /down/, even for 1.50, just let QD expand]. }
	shrink := {ge.fstip AND} NOT myBig;
	
	ty := ge.y;
	tx := ge.x;
	trb := ge.rowBytes;

	IF shrink THEN
	{ recalc "internal" pic size, based on previously calc'ed display size }
		BEGIN
		ty := ge.dy DIV 2;
		tx := ge.dx DIV 2;
	{ new rowBytes must also be multiple of /4/, for in-place stippling }
		trb := CalcRow4Bytes (tx, GFXAM_DEPTH);
		END;

{ we now write into these 's' fields instead of the 'ge.' globals.  This is because
  if ShowGFXS is called twice in a row for the same picture, the globals got mashed. 
  [Removal of the (dead) 0.75 option would simplify things a lot.] }

	sy := ge.dy;
	sx := ge.dx;
	srb := trb DIV 2;
    (*  sdepth := 1; *)

(**	ge.y := ge.dy;			{ and set final (post-stip) vals }
	ge.x := ge.dx;
	ge.depth := 1;  		{ adjust for depth=1 }
	ge.rowbytes := trb DIV 2; **)

{ calc rects (for stips, always scaled 1.00) }

	SetRect (r1, 0, 0, sx, sy);
	r2 := r1;
	OffsetRect (r2, wMarg + xpos, ypos);

	(* IF ge.fstip THEN *)

	IF shrink THEN
		BEGIN
		Shrink75 (pMax, ge.rx, tx, ty);  { works w/ BYTES }
		{ Clip75 ... alternate call }
		END;
		
	rawlen {maxSize} := LongMult (ty, tx);		{ calc (shrunk) buflen; rows rounded DOWN }
	stipLen := LongMult (sy, srb);

	IF (rawlen + ge.rx) + stipLen > altLen THEN	{ too big! }
		BEGIN
		IF gfxDebug			{ signal an error ... }
			THEN SysBeep (4);
		EXIT (oShowGFXS);		{ avoid a crash }
		END;

	{ set gfxPM base to 2nd buffer }
	pStips := Ptr (ORD4(altPtr) + (altLen - stipLen));

	WITH gfxPM DO
		BEGIN
		baseAddr := pStips;	{ set gfxPM base to Stips base }
		rowBytes := srb;
		bounds := r1;
		END;

	{ if Transparent, first blit in old pic }
	IF ge.trans THEN
		CopyPic (@thePort^.portBits, BitMapPtr(@gfxPM), r2, r1);

	{ stipple/pad to 2nd buffer }	      
	StipPic (tx, ty, trb DIV 2, 
		pMax, pStips, @gp.exstips, ge.trans);
		
	(* END  { IF ge.fstip THEN } *)

	{ blit new pic out to screen }
	CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
END;

{-------------------------------}
{ ShowGFX			}
{-------------------------------}

PROCEDURE SqzRow (src, dst: Ptr; rowPix {,rowBytes, rows}: INTEGER); 
	EXTERNAL;  {68K}

{ display in Color (a section of) a picture, data in altbuf;
  position given in pixel units }

PROCEDURE ShowGFX{Color} (ypos, xpos: INTEGER);
VAR
	r1, r2: rect;
	pMax, pFinal: Ptr;
	n: INTEGER;
	p1, p2: Ptr;
BEGIN
	IF ge.mono THEN			{ neither stipple NOR scale }
		BEGIN
		ShowGFXM (ypos, xpos);
		EXIT (ShowGFX);
		END;

	IF ge.fstip THEN		{ handle the stipple case separately, too }
		BEGIN
		ShowGFXS (ypos, xpos);
		EXIT (ShowGFX);
		END;

	pMax := Ptr (ORD4(altPtr) + ge.rx);	{ start of byte data }

	SetRect (r1, 0, 0, ge.x, ge.y);
	SetRect (r2, 0, 0, ge.dx, ge.dy);	{ always scaled up 2.00 }
	OffsetRect (r2, wMarg + xpos, ypos);

	IF TRUE {NOT ge.fstip} THEN
		BEGIN
		pFinal := altptr;
		p1 := pMax;			{ start of byte data }
		p2 := pFinal;			{ start of nibble data }

{ Squeeze bytes (16-color pixels) into nibbles, throwing away the high nibbles.
  also shift picture to base of buffer, and pad each row }

		FOR n := 1 TO ge.ry DO
			BEGIN
			SqzRow (p1, p2, ge.rx);
			p1 := Ptr (ORD4(p1) + ge.rx);		{ advance ptrs }		
			p2 := Ptr (ORD4(p2) + ge.rowBytes);	{ skip/pad }
			END;
		END;

	WITH gfxPM DO
		BEGIN
		baseAddr := pFinal;	{ set gfxPM base to buffer base }
		rowBytes := ge.rowBytes;
		bounds := r1;

		IF TRUE {NOT ge.fstip} THEN	
		{ adjust some additional fields }
			BEGIN
			{ mark as a pixmap  [otherwise, /always/ 1 plane] }
			rowBytes := rowBytes + $8000;  
			END;
		END;

	CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
	
(**	IF n = -999 THEN	{ for testing only -- see if this munges colors ... }
		BEGIN
		CopyPic (@thePort^.portBits, BitMapPtr(@gfxPM), r2, r1);
		CopyPic (BitMapPtr(@gfxPM), @thePort^.portBits, r1, r2);
		END;  **)
END;

{-------------------------------}
{ EraseGFX			}
{-------------------------------}

{ clear a picture, position given in pixel units }

PROCEDURE EraseGFX (ypos, xpos: INTEGER);
VAR
	r: rect;
BEGIN
	SetRect (r, 0, 0, ge.dx, ge.dy);
	OffsetRect (r, wMarg + xpos, ypos);
	EraseRect (r);
END;

{-------------------------------}
{ opDisplay			}
{-------------------------------}

{ position given in pixel units, relative to current window, 0-origin }

PROCEDURE opDisplay (id, ypos, xpos, erase: INTEGER);
VAR
	r: rect;
	clip: Boolean;
BEGIN
	IF ReadGFXEntry (id) <> noErr THEN	{ call BEFORE erase or dy/dx refs! }
		EXIT (opDisplay);

	IF (ypos < -2) OR (xpos < -2) THEN	{ ERROR, force inbounds }
		BEGIN
		ypos := 0;
		xpos := 0;
		IF gfxDebug		{ signal an error ... }
			THEN SysBeep (4);
		END;

{ -2 (after 0-origining) means only read palette/stipple info }
	IF (ypos = -2) OR (xpos = -2) THEN 
		BEGIN
		{ to be implemented }
		
		EXIT (opDisplay);
		END;

{ -1 (after 0-origining) means use current position }
	IF ypos = -1
		THEN ypos := curRow
		ELSE ypos := firstRow + ypos;	{ relative to current window }
	IF xpos = -1
		THEN xpos := curCol
		ELSE xpos := firstCol + xpos;

(** IF id > NPICS THEN
	    EXIT (opDisplay);  **)	{ no errs for now }

{ check size, clip to logical window (YZIP) if too big }
	IF (ge.dy > lastRow - ypos {AFTER relativizing})
	OR (ge.dx > lastCol - xpos {AFTER relativizing}) THEN
		BEGIN
		GetClip (clipRgn1);	{ save old }

		r.top := firstRow;
		r.left := firstCol;
		r.bottom := lastRow;
		r.right := lastCol;
		ClipRect (r);
		
		clip := TRUE;
		IF gfxDebug		{ signal an error ... }
			THEN SysBeep (4);
		END
	ELSE	clip := FALSE;

	IF erase <> 0 THEN		{ clear the (current) pic }
		EraseGFX (ypos, xpos)
	ELSE
		BEGIN
		IF ReadGFXPalette = noErr THEN 
			BEGIN
		(*	CheckMyUpdate;	*)		
			IF ReadGFXData = noErr THEN 
				ShowGFX (ypos, xpos);
			END;
		END;

	IF clip THEN
		SetClip (clipRgn1);	{ undo the above }
END;

{-------------------------------}
{ opPicinf			}
{-------------------------------}

{ Fill in given table with size (in pixels) of the given picture.
  If invalid id, return negative result }

TYPE
	ARRAY2 = ARRAY[1..2] OF INTEGER;

FUNCTION opPicinf (id: INTEGER; VAR tbl: ARRAY2): INTEGER;
BEGIN
	opPicinf := 0;			{ assume no error }

	IF id = 0 THEN		{ >> DEAD IN YZIP (supposedly) << }
		BEGIN
		tbl[1] := gh.npics;	{ [should be /highest/ id, actually] }
		EXIT (opPicinf);
		END;

	{ otherwise, read just enough to return size }
	IF ReadGFXEntry (id) = noErr THEN
		BEGIN
		tbl[1] := ge.dy {DIV lineheight};
		tbl[2] := ge.dx {DIV colWidth};
		END
	ELSE
		BEGIN	
		opPicinf := -1;

	{ provide some defaults for games that don't trap errors ... }
		tbl[1] := 2*lineHeight;
		tbl[2] := 2*colWidth;
		END;
END;

{-------------------------------}
{ getPicset			}
{-------------------------------}

{ Preload the requested set of pics (or as many as will fit)

  Note: PICSET currently caches /compressed/ picture data, trading off space against
  display speed (would reverse this to do animation, for example).  Also, palette data 
  is not touched by the caching.  This is okay for most small pics since they share 
  someone else's palette.

  The format of the info at psPtr is as follows (all word entries -- must change off/len 
  to longs if PSDEFLEN ever exceeds 32K!):
	- #ids in picset
	- 1st id
	- starting offset (in bytes, relative to psData)
	- length (in bytes)
	- 2nd id, etc ... }

PROCEDURE getPicset (tbl: WPtr; setCount: INTEGER);
VAR
	p1: WPtr;
	p2: Ptr;
	curCount, id: INTEGER;
	headerLen, off,
	avail,		{ space remaining in picset buffer }
	curSize: LONGINT;

FUNCTION psRead: osErr;
BEGIN
	psRead := -1;	{ default result }
	IF avail <= 0
		THEN EXIT (psRead);

	{ get dataOff (write new routine? this one does some extra work) }
	IF ReadGFXEntry (id) <> noErr
		THEN EXIT (psRead);

	{ read in the length of the (compressed) data }
	IF FSReadClr (ge.dataOff, gh.LBYTES, @curSize, 4) <> noErr
		THEN EXIT (psRead);
	
	IF curSize > MAXITEMLEN THEN	{ exceeds our absolute max }
		EXIT (psRead);		{   [picset isn't for full screens] }

	IF curSize > avail THEN		{ not enough room left! }
		BEGIN
		IF avail < 256 THEN	{ avoid further disk hits; }
			avail := 0;	{ return errs for remaining pics in set }
		EXIT (psRead);
		END;

	{ read (compressed) data into the picset buf }
	IF FSReadGFX (-1, curSize, p2) <> noErr 
		THEN EXIT (psRead);
		
	psRead := 0;	{ all okay }
END;

BEGIN
	IF ps.psInited <> PSINITMARK		{ valid buffer? }
		THEN EXIT (getPicset);

	p1 := WPtr (ORD4(ps.psPtr) + 2);		{ reserve 1 word for #ids }
	headerLen := 2 + (PSENTRYLEN * setCount);
	p2 := Ptr (ORD4(ps.psPtr) + headerLen);
	ps.psData := p2;				{ data starts here, save ptr }

	avail := ps.psLen - headerLen;
	IF avail <= 0
		THEN EXIT (getPicset);

	curCount := 0;
	WHILE setCount > 0 DO
		BEGIN
		{ get next id in set }
		id := GetWord {GetWordB} (tbl);		{ now guaranteed even-aligned ... }	

		IF psRead = noErr THEN
			BEGIN
			PutWord (p1, id);

			off := ORD4(p2) - ORD4(ps.psData);
			PutWord (p1, off);		{ picbuf offset }
		{	PutLong (LPtr(p1), off);  }

			PutWord (p1, curSize);		{ length (in bytes) }
		{	PutLong (LPtr(p1), off);  }
			
			curSize := RoundUp (curSize);	{ KEEP p2 EVEN ALIGNED, FOR I/O SPEED }
			p2 := Ptr (ORD4(p2) + curSize);
			avail := avail - curSize;
			curCount := curCount + 1;
			END
		ELSE
		{ if any error, write nothing to header }
			BEGIN
			{ if debug then Sysbeep (4); }
			END;
		setCount := setCount - 1;
		END;

	p1 := WPtr (ps.psPtr);		{ [use local var, don't bump global!] }
	PutWord (p1, curCount);		{ store the success count }
END;

{-------------------------------}
{ searchPicset			}
{-------------------------------}

{ Check the current picset for the given id; 
  return zero if found (and return ptr/len in globals) }
  
FUNCTION searchPicset (id: INTEGER): osErr;
VAR
	p1: WPtr;
	setCount: INTEGER;
	temp: INTEGER;
BEGIN
	searchPicset := -1;			{ default }
	
	IF ps.psInited <> PSINITMARK
		THEN EXIT (searchPicset);
		
	p1 := WPtr (ps.psPtr);
	setCount := GetWord (p1);			{ id/off/len triples follow }

	WHILE setCount > 0 DO
		BEGIN
		IF GetWord (p1) = id THEN		{ found it, return info }
			BEGIN
		{ CAUTION: the following two items get sign-extended.
		  Could save off/len as LONGS instead }
			ps.p := Ptr (GetWord (p1) + ORD4(ps.psData));
			ps.len := GetWord (p1);
			
			searchPicset := 0;		{ success }
			EXIT (searchPicset);
			END
		ELSE
			BEGIN
			p1 := WPtr (ORD4(p1) + (PSENTRYLEN-2));	{ skip to next triple }
			setCount := setCount - 1;
			END;
		END;
END;

{-------------------------------}
{ InitGFX			}
{-------------------------------}

{ open gfx megafile, also alloc mem and read in directory }

FUNCTION InitGFX (start: BOOLEAN): OSErr;
VAR
	err: osErr;
	gfxName: STR255;
BEGIN
	InitGFX := -1;		{ default }
	gh.inited := FALSE;
	dirPtr := NIL;
	
	IF start THEN
		BEGIN
		gh.npics := 0;		{ [default val, for PICINF] }
	
	{ for a small window use mono gfx, for a big window use color gfx (if big mono, 
	  we currently stipple --  should probably open a small window instead ) }
		IF myBig 
			THEN gfxName := 'CPic.Data'
			ELSE gfxName := 'Pic.Data';

		IF SearchOpen ('', gfxName, gfxChan) <> 0 THEN
				EXIT (InitGFX);

	{ read [id and] global flags }
		IF FSReadGFX (-1, 2, @gh.gflags) <> noErr 
			THEN EXIT (InitGFX);

	{ read global huff ptr (if any) }
		IF FSReadClr (-1, 2, @gh.ghuffOff, 4) <> noErr
			THEN EXIT (InitGFX);
		gh.ghuffOff := 2 * gh.ghuffOff;	{ make byte offset }

	{ read local [and extern] pic counts }
		IF FSReadGFX (-1, 4, @gh.npics) <> noErr
			THEN EXIT (InitGFX);

	{ read length of each directory entry (12-14-16, etc) }
		IF FSReadClr (-1, 1, @gh.entryLen, 2) <> noErr
			THEN EXIT (InitGFX);

(**	{ read graphics checksum }	{ NEED TO SKIP A BYTE AFTER ENTRYLEN }
		IF FSReadGFX (-1, 2, @gh.cksum) <> noErr 
			THEN EXIT (InitGFX);
**)
	{ read graphics version byte }
		IF FSReadClr (12, 1, @gh.version, 2) <> noErr
			THEN EXIT (InitGFX);
		IF gh.version >= 1
			THEN gh.LBYTES := 3
			ELSE gh.LBYTES := 2;	{ this case just for backward compatibility }

	{ alloc directory buffer, and read in directory
		pro: avoids disk hits for PICINF (and PICDISP prep)
			300 pics/file => 8 hits/pic (binary search)
			30 pics/set => 30*8 hits
		con: 300 pics/file * 16 bytes => 4.8K mem
			disk hits reduced by caching (newer Systems), 30*8 -> ~8
	}
		gh.dirLen := gh.npics * gh.entryLen;
		dirPtr := NewPtr (gh.dirLen); 
		IF dirPtr = NIL
			THEN EXIT (InitGFX);

		IF FSReadGFX (GHEADLEN, gh.dirLen, dirPtr) <> 0 
			THEN EXIT (InitGFX);

		gp.pOff := 0;		{ mark these recs as empty }
		gt.hOff := 0;

		gh.inited := TRUE;	{ init successful }

	{ and init picset stuff (a failure here is not currently reported) }
		WITH ps DO
			BEGIN
			psLen := PSDEFLEN;
			psPtr := NewPtr (psLen);	{ separate buffer }

			IF psPtr <> NIL THEN
				BEGIN
				WPtr(psPtr)^ := 0;	{ start with an empty set }
				psInited := PSINITMARK;	{ success }
				END;
			END;

	{ finally, show a boot screen, if requested & exists  [NOT IN SPEC] }

	(**	IF BAND (gh.gflags, GF_SPLASH) <> 0	{ check flag }
			THEN opDisplay (1, 0, 0, 0);	{ show 1st pic (center it?) }
	**)	END

	ELSE { cleanup } 	
		BEGIN
		IF gfxChan <> 0 THEN	{ this IS necessary under MultiFinder }
			BEGIN
			InitGFX := FSClose (gfxChan);
			gfxChan := 0;
			END;

(**		IF dirPtr <> NIL THEN	{ NOT NECESSARY? [nor for 68K allocs] }
			BEGIN
			DisPosPtr (dirPtr);
			dirPtr := NIL;
			END;

		IF ps.psPtr <> NIL THEN
			BEGIN
			DisPosPtr (ps.psPtr);
			ps.psPtr := NIL;
			END;
**)
		END;

	InitGFX := 0;		{ all okay }
END;

