
/* "The Amiga sound routines are compiled independently, then linked to
   the rest of the program" 

   By Don Harlow; hacked by Duncan Blanchard 
*/

#define SDEADCODE 0		/* exclude unused code */

#include "exec/types.h"
#include "exec/memory.h"

/* The SOUND switch can be turned off (in the batch file) at compile time
   for games that don't use special sounds.  Takes a 10K bite from the space
   occupied by the interpreter on disk and in memory. */

#if SOUND
		
#include "lattice/stdio.h"	/* (Amiga-format pathnames) */
#include "lattice/fcntl.h"
#include "lattice/math.h"
#include "devices/audio.h"
#include "devices/timer.h"
#include "sound.h"

#define NFILES		16		/* max # midi sound files */
#define SCHANS		4		/* max # Amiga audio channels */
#define	S_ON		on_off
#define	S_OFF		!on_off
#define SOUNDDIR	"Sound/"

extern 	VOID 	*AllocMem();		/* APTR?  CHAR *? */
extern 	VOID 	FreeMem();
extern	LONG 	OpenDevice();
extern 	VOID 	CloseDevice();
extern 	VOID 	BeginIO();
extern	LONG	CheckIO();
extern 	double 	pow();

extern	UBYTE *soundbuf;		/* data buffer, 64K preallocated */

UBYTE	*sndfile[NFILES+1] = { 0 };	/* sound file buffer ptrs */
UWORD	sflen[NFILES+1];		/* and lengths */
UBYTE	*midifile = 0;			/* midi file buffer ptr */
UWORD	mflen;				/* and length */

/* Use of sound synchronization flags (one per channel):
	- set by audion
	- cleared by game interrupt
*/
UBYTE sf[SCHANS] = {0};

/******************************************************

(From AUDONOFF.C)

Wednesday 10-Jun-87 13:02:04

*******************************************************/

#define		SOUNDPREC	75	/* precedence = "medium high" */

struct 	IOAudio *IOA[SCHANS];
WORD	dev_error;

UWORD 	percalc();
void	clear_ioBlock();

/*------------------------------*/
/*	audion			*/
/*------------------------------*/

/* vol = 0..63, or -1 for "hardwired" value from midi file */
/* repeat = 1..n, 0 for indefinite, or -1 for "hardwired" val */

WORD 	audion(on_off, vol, rep)
WORD	on_off, vol, rep;
{
	struct 	IOAudio	*curIOA; 
static 	UBYTE	chan_req[SCHANS] =  {1,2,4,8};
static 	UWORD	old_mcp[SCHANS] = {0,0,0,0};	/* midi channel/pitch */
	UWORD 	cur_mcp;
	WORD	i,j;
	UBYTE	m8chn,m4chn,m2chn;		/* n bits of channel data */
	UBYTE	oldstat = 0;
	UBYTE	mpitch, spitch;			/* added - dbb */
	UWORD	sr;
	UBYTE	*cursnd;			/* sound file buffer ptr */
	WORD	vol2, rep2;
	int	err;			/* for ADCMD_ALLOCATE */

/** (unused) **/
/*	UBYTE	flag, status = 0;  */		/* dead */
/*	WORD	n;
	ULONG 	cur_mcp1;
	ULONG 	timey;	
	UBYTE	oneflg = 0;
	UWORD 	pval 	= 400;
	UWORD 	psav 	= pval;
static 	UWORD 	pvalue[SCHANS] = {300,280,200,180};
	ULONG 	Timex;
	UBYTE 	sound_data[128];
*/
	if (S_ON) {
		for(i=0; i<SCHANS; i++) {
			IOA[i] = curIOA = (struct IOAudio *) AllocMem (
				(LONG)sizeof(struct IOAudio),
		  		(MEMF_PUBLIC | MEMF_CLEAR));
			if (!curIOA) {
				clear_ioBlock();
				return (NO_IO_REQUEST_BLOCK);
			}
		}
		dev_error = OpenDevice(AUDIONAME, 0L, IOA[0], 0L);
		if (dev_error) {
			clear_ioBlock();
			return (NO_AUDIO_DEVICE);
		}
		for (j = 1; j < SCHANS; j++)
			*IOA[j] = *IOA[0];	/* copy //contents// */

/* Allocation of channels should be less inflexible, especially for IOA[0].
   curIOA->ioa_Length = (ULONG)i;  (?) dbb  */

		for (i = 0; i < SCHANS; i++) {
			curIOA = IOA[i];
			curIOA->ioa_Data = &chan_req[i];
			curIOA->ioa_Length = (ULONG)sizeof(chan_req[i]);
			curIOA->ioa_Request.io_Message.mn_Node.ln_Pri = SOUNDPREC;
			curIOA->ioa_Request.io_Command = ADCMD_ALLOCATE;

		/* (following is from Peck p285) */

			/* if chan not avail don't wait, return err */
			curIOA->ioa_Request.io_Flags = ADIOF_NOWAIT | IOF_QUICK;
			BeginIO(curIOA);

			err = WaitIO(curIOA);
			if (!(curIOA->ioa_Request.io_Flags & IOF_QUICK))
				/* was not quick I/O after all */
				GetMsg (curIOA->ioa_Request.io_Message.mn_ReplyPort);
			if (err) {
				clear_ioBlock();
				return (-1);	/* "chan alloc err" */
			}
		}
		for (i = 1; i < SCHANS; i++)
			IOA[i]->ioa_AllocKey = IOA[0]->ioa_AllocKey;
	}

	if (S_OFF) {
		midifile += 6;		/* from init(); undone below [dbb] */
/**		mflen -= 6;  **/ /* already hacked in init [dbb] */
		}

	for (j = 0; j < mflen; j += 3) {
		m8chn = midifile[j];

/* "this section generally ignored" */
		if (m8chn <= 0x80) {
			if (j >= mflen)		/* [mflen not mult of 3?] */
				break;		/* [gone] */
			/* use 0 for last file byte */
			j -= 1;			/* retrieve last byte */
			m8chn = oldstat;	/* process last midi channel */
		}
		else if (m8chn != 0xff)
			oldstat = m8chn;

		cur_mcp = (m8chn << 8) + midifile[j+1];
		m4chn = (m8chn & 0xf);		/* mask out high nibble */
		m2chn = (m4chn & 3);
		curIOA = IOA[m2chn];	

		if ((m8chn >= 0x80) && (m8chn <= 0x9f)
				&& (midifile[j+2] /*volume*/ > 0)) {

			/* bug fixed - following used '&&' */
			if ((old_mcp[m2chn] & 0xff00) == (cur_mcp & 0xff00)) {
				do_Finish (curIOA, m2chn);
				old_mcp[m2chn] = 0;	/* but next line ?? **/
			}
/* old_mcp contains the channel assignment and the midi pitch */
			old_mcp[m2chn] = cur_mcp;

/* setup the paramblock for a new start */
			cursnd = sndfile[m4chn];

			if (rep == -1)		/* use val from data file */
				rep2 = (UWORD) *cursnd;
			else rep2 = rep;
			curIOA->ioa_Cycles = rep2;

/* make mystery adjustment to pitch-request byte.  */
/* But DON'T change value in original buffer.  Otherwise, when sound is
   started and stopped more than once (without reloading from disk in between),
   it "drawls" out slowly the second time.   [dbb].  */

			mpitch = midifile[j+1];
			if (m8chn == 0x90)
				mpitch -= 24;	/* was +=12 */
			else if (m8chn == 0x91)
				mpitch -= 24;   
			else if (m8chn == 0x92)
				mpitch -= 12;  

			spitch = *(cursnd + 1);
			sr = *(UWORD *)(cursnd + 2);
			curIOA->ioa_Period = percalc (sr, mpitch, spitch);
		
			if (vol == -1)		/* use val from data file */
				vol2 = midifile[j+2];
			else vol2 = vol;
			curIOA->ioa_Volume = vol2;

			curIOA->ioa_Length = (ULONG) *(UWORD *)(cursnd + 6);
			curIOA->ioa_Data = cursnd + 8;

/* make sure node type is NT_MESSAGE (set also by system at start of sound, 
   changed at end).  Our end-of-sound detector depends on this.  */
			curIOA->ioa_Request.io_Message.mn_Node.ln_Type 
				= NT_MESSAGE;

 			curIOA->ioa_Request.io_Flags = ADIOF_PERVOL | IOF_QUICK;
			curIOA->ioa_Request.io_Command = CMD_WRITE;
			BeginIO((struct IORequest *)curIOA);

			sf[m2chn] = 1;		/* set STARTED flag */
		} 
		else  {
			for (i = 0; i < SCHANS; i++) {		
				if (old_mcp[m2chn] == cur_mcp) {
					old_mcp[m2chn] = 0;
					do_Finish (curIOA, i);
				/* [exit loop after 1st match] */
					break;
				}
			} 	
		}	
	}

/* above code demonstrates how to change period value once sound started */

	if (S_OFF) {
		for (i = 0; i < SCHANS; i++) {
			curIOA = IOA[i];
			do_Finish (curIOA, i);

			/* bug: must ADCMD_FREE also ??? */
		}

	/* must messages be ack'd before releasing the mem?  [dbb] */
		clear_ioBlock();
		midifile -= 6;	  /* undo hack above, before FreeMem  [dbb] */
	/**	mflen += 6;  **/  /* already hacked in init [dbb] */
	}
	return(ALL_OK);
}

/*------------------------------*/
/*	do_Finish		*/
/*------------------------------*/

/* send ADCMD_FINISH message (separate routine since multiple calls)  */

do_Finish (curIOA, chan)
struct IOAudio *curIOA;
UBYTE chan;
{
	sf[chan] = 0;		/* turn off STARTED flag */

	curIOA->ioa_Request.io_Flags = ADIOF_PERVOL | IOF_QUICK;
	curIOA->ioa_Request.io_Command = ADCMD_FINISH;
	BeginIO ((struct IORequest *)curIOA);
}

/*------------------------------*/
/*	percalc			*/
/*------------------------------*/

/* percalc - all args/val now integers; floating point used internally */

#define SCONST  3579.49
#define CHROMA  1.05946309

UWORD percalc (samp, midval, refval)
UWORD samp; UBYTE midval, refval;
/* samp = initial (original) sampling rate;
   midval = desired midi pitch;  refval = initial midi frequency;
   per = (3579.49/(samp/1000)) / (CHROMA ^ delta)  */ 
{
	BYTE delta;
	float per;
	double pitch; 

	per = (float)SCONST / ((float)samp / (float)1000);
	delta = midval - refval;
	if (delta) {
		pitch = pow ((double)CHROMA, (double)delta);
		per /= pitch;
		}
	return ((UWORD)per);
}

/*------------------------------*/
/*	clear_ioBlock		*/
/*------------------------------*/

void clear_ioBlock()
{
	struct 	IOAudio	*curIOA; 
	WORD	i;

	if (!dev_error)
		CloseDevice((struct IORequest *)IOA[0]);
	for (i = 0; i < SCHANS; i++)
		if (curIOA = IOA[i]) {
			FreeMem((char *)curIOA, (LONG)sizeof(struct IOAudio));
			IOA[i] = 0;	/* mark it free  [dbb] */
			}
}

/******************************************************

(From INITONOF.C)

Wednesday 10-Jun-87 13:00:31

*******************************************************/

WORD	fdr[NFILES+1], fdr1 = -1;
UBYTE	fname[NFILES+1][15];	/* sound file names */
UBYTE	chan[NFILES+1];		/* sound channel to use */

void	process();
WORD	get_name();
void	get_all_names();
void	get_midi_name();
WORD	get_file();
WORD	get_all_files();
WORD	get_midi_file();
void	cleanup(), close_files();

/*------------------------------*/
/*	init			*/
/*------------------------------*/

/* allocate memory, read files, setup pointers */

WORD	init (effect, on_off)
UBYTE 	*effect;
UWORD 	on_off;
{    
	static UWORD store;
	WORD n, i, err;
	UBYTE ch, fullname[32];
	WORD num_snd;

/** (unused) **/
/*	WORD  synth_num, sound_error;
	UBYTE  *file_1;
*/
	if (S_ON) {
		for (i = 0; i <= NFILES; i++)
			fdr[i] = -1;		/* all files start unopened */

		strcpy (fullname, SOUNDDIR);	/* always look in subdir */
		strcat (fullname, effect);

		fdr1 = open(fullname, O_RDONLY);	/* open master file */
		if (fdr1 < 0)	/* no files open, no buffers allocated */
			return(MAIN_FILE_ERROR);

	/* get file names */

		n = read (fdr1, &ch, 1);	/* 1st byte is # sound files */
		num_snd = (WORD)ch;
		get_all_names (num_snd);	/* get names of sound files */

		n = read (fdr1, &ch, 1);	/* check for synthesis file */
		if (ch)				/* 1=yes 0=no */
			process();
		get_midi_name();		/* get name of midi file */

	/* get file data */

		err = get_all_files(num_snd);	/* read the sound data */
		if (err) {
			close_files();
			cleanup();
			return (SOUND_FILE_ERROR);
			}

		err = get_midi_file();		/* read the midi data */
		if (err) {
			close_files();
			cleanup();
			return (MIDI_FILE_ERROR);
			}

		store = mflen;		/* save orig length of midi file */
		mflen = 3;		/* "only 3 bytes in this incarnation" */
		close_files();		/* close all files */
	}

/** want only last three bytes  [moved to audion() -- dbb]  **/
/**	if (S_OFF)  midifile += 6;	**/
/** now called separately -- [dbb]  **/
/**	sound_error = audion(on_off);
	if (sound_error)  return(sound_error);	**/

	if (S_OFF) {
		mflen = store;		/* (first) restore actual len */
		cleanup();		/* deallocate buffers */
	}
	return (ALL_OK);
}

/*  subroutines :::::::::::::::::::::::::::: */

void process()
{
}

/*------------------------------*/
/*	get_name		*/
/*------------------------------*/

/* get_name()
	Gets a file name into memory
	Parameters: location where name is to be stored
	Returns: length of name
*/

WORD get_name (buf)
UBYTE *buf;
{
	WORD	n, j=0;
	UBYTE	ch;

	do {
		n = read (fdr1, &ch, 1);
		buf[j++] = ch;
		}
		while (ch != '\0');
	return ((WORD)(j-1));
}

/* Get names of all data files into an array in memory */

void get_all_names (tot)
WORD tot;		/* number of names to get */
{
	WORD	i,n;

	for (i = 0; i < tot; i++) {
		n = read (fdr1, &chan[i], 1);	/* sound channel to use */
		n = get_name (&fname[i]);
	}
}

/* Get midi data file name into memory */

void get_midi_name()
{
	get_name (&fname[NFILES]);
}

/*------------------------------*/
/*	get_file		*/
/*------------------------------*/

/* get_file()
	open a data file, allocate memory, read it in
	Parameters: index of the file
	Returns: error, or 0 if none
*/

WORD get_file(k)
WORD k;
{
	UWORD n;
	UBYTE *loc, fullname[32];

	strcpy (fullname, SOUNDDIR);	/* always look in subdir */
	strcat (fullname, fname[k]);
	fdr[k] = open (fullname, O_RDONLY);
	if (fdr[k] == -1)
		return(-1);		/* "file not found" */
	n = read (fdr[k], &sflen[k], 2);	/* the "defined length" */

	if (k == 0)			/* use standard buffer */
		loc = soundbuf;
	else loc = (UBYTE *)AllocMem ((LONG)sflen[k], (MEMF_CHIP | MEMF_CLEAR));
	if (!loc) {
		return (-2);		/* "alloc error" */
	}

	sndfile[k] = (UBYTE *)loc;
/***	sndfile[chan[k]] = (UBYTE *)loc;  ***/	/* [correct?] */

	n = read (fdr[k], loc, (UWORD)sflen[k]);
	return (0);
}

/* get_all_files()
	Loads all apposite sound data files into memory
	Parameters: number of files to load
	Returns: error, or 0 if none
*/

WORD get_all_files (tot)
WORD tot;
{
	WORD i, err;

	for (i = 0; i < tot; i++) {
		err = get_file (i);
		if (err)  break;
	}
	return(err);
}

/* get_midi_file()
	Load the midi data file into memory
	Parameters: none
	Returns: error, or 0 if none
*/

WORD get_midi_file()
{
	WORD err;

	err = get_file (NFILES);
	if (!err) {
		midifile = sndfile[NFILES];
		mflen = sflen[NFILES];
		}
	return (err);
}

/*------------------------------*/
/*	close_files		*/
/*------------------------------*/

/* Close all open files */

void close_files()
{
	WORD	i, n;

	for (i = 0; i <= NFILES; i++)
		if (fdr[i] != -1) {
			n = close (fdr[i]);
			fdr[i] = -1;
		}
	if (fdr1 != -1) {
		n = close (fdr1);
		fdr1 = -1;
	}
}

/* Free up memory allocated for buffers */

void cleanup()
{
	WORD	i;

	/* i=0 uses global "soundbuf" (64K preallocated) */
	sndfile[0] = 0;

	for (i=1; i < NFILES; i++)
		if (sndfile[i]) {
			FreeMem ((char *)sndfile[i], (LONG)sflen[i]);
			sndfile[i] = 0;		/* [dbb] */
			}
	if (midifile) {
		FreeMem ((char *)midifile, (LONG)mflen);
		midifile = 0;		/* [dbb] */
		}
}

#endif  /* SOUND */

/******************************************************

	Game Interface

*******************************************************/

extern void bell_sound();

#define S_INIT 1	/* load data from disk */
#define S_START 2
#define S_STOP 3	/* stopped but ready to restart */
#define S_CLEANUP 4	/* release the memory used */

#define S_BEEP 1
#define S_BOOP 2

#define	M_ON 1
#define	M_OFF 0

#if SOUND

/*------------------------------*/
/*	md_sound		*/
/*------------------------------*/

/* id=1-n or 0(mru), action=1-4, 
   volume=0-64 or -1(midi), repeat=1-n, 0(infinite) or -1(midi) */

WORD md_sound (id, action, vol, rep)
WORD id, action, vol, rep;
{
	static WORD last_id = 0;
	static WORD inited = 0, started = 0;
	char name[32];
	WORD i, err;

	if (id == 1 || id == 2) {
		bell_sound(id);		/* 1=beep, 2=boop */
		return(0);
		}
	if (id == 0) {			/* use MRU effect */
		if (last_id == 0)  return (0);		/* none, exit */
		else  id = last_id;
		}
	else if (id != last_id) {	/* first get rid of old, if any */
		md_sound (last_id, S_CLEANUP, vol, rep);  /* (recurse) */
		last_id = id;
		}

/* Use id to make generic names (don't want names to give away plot!) */

	i = stccpy (name, "S", 32);	/* make "[SOUND/] Sx.NAM" */
/**	stcu_d (&name[i], id, 16);  **/ /* Lattice bug: i is one too high */
	stcu_d (&name[1], id, 16);
	strcat (name, ".NAM");

	if (action == S_INIT || action == S_START) {
		if (!inited) {
/***			showdbg (S_INIT, name, vol, rep);  ***/
			err = init (name, M_ON);
			if (err != ALL_OK) {
				showerr (err);	/* inform user */
				return (err);
				}
			inited = 1;
			}
		}
	if (action == S_START) {
		md_sound (id, S_STOP, vol, rep);  /* reset (if restarting) */
/***		showdbg (S_START, name, vol, rep);  ***/
		err = audion (M_ON, vol, rep);
		if (err != ALL_OK) {
			showerr (err);		/* inform user */
			return (err);
			}
		started = 1;
		}
	if (action == S_STOP || action == S_CLEANUP) {
		if (started) {
			audion (M_OFF, vol, rep);	/* stop sound */
			started = 0;
			}
		}
	if (action == S_CLEANUP) {		/* free resources */
		if (inited) {
			init (name, M_OFF);
			inited = 0;
			}
		last_id = 0;		/* back to original state */
		}
	return (ALL_OK);
}

/*------------------------------*/
/*	showerr			*/
/*------------------------------*/

showerr (err)		/* inform user of problem */
WORD err;
{

}

/*------------------------------*/
/*	end_sound		*/
/*------------------------------*/

/* This "pseudo-interrupt" is called from the 68K kernel and returns TRUE
   to report the end of a sound (on ANY channel...) */

BOOL end_sound ()
{
	register UBYTE *p;
	register int i;
	BOOL result = FALSE;

/* When sound ends, system changes node type to either NT_FREEMSG or
   to NT_REPLYMSG.  We use this to detect end-of-sound.

   (A CheckIO call merely checks for node type NT_REPLYMSG.  But a
   reply port must have been attached to the message structure, I think,
   for that type to ever be returned.)  */

	p = (UBYTE *) &sf[0];
	for (i=0; i<SCHANS; i++) {
	    if (*p) {				/* started? */
		if (IOA[i]->ioa_Request.io_Message.mn_Node.ln_Type 
		!= NT_MESSAGE) {		/* stopped? */
		    *p = 0;			/* turn off flag */
		    result = TRUE;
		}
	    }
	    p++;
	}
	return (result);
}

#else /* if !SOUND */		/* stub routines */

WORD md_sound (id)
WORD id;
{
	bell_sound(id);		/* 1=beep, 2=boop */
	return(0);
}

BOOL end_sound ()
{
	return (FALSE);
}

#endif  /* SOUND */

#if SDEADCODE			/* routines currently unused */

/*------------------------------*/
/*	showdbg			*/
/*------------------------------*/

showdbg (action, name, vol, rep)		/* show debug info */
WORD action, vol, rep;
char *name;
{
	char s[80], sn[16];

	if (action == S_INIT) {
		strcpy (s, "S_INIT (");
		strcat (s, name);		/* show filename */
		strcat (s, ")  ");
		line_out (s, strlen(s));
		}
	if (action == S_START) {
		strcpy (s, "S_START (");
		stci_d (sn, vol, 16);		/* show volume */
		strcat (s, sn);
		strcat (s, ", ");
		stci_d (sn, rep, 16);		/* show reps */
		strcat (s, sn);
		strcat (s, ")");
		line_out (s, strlen(s));
		char_out (13);
		}
}

/*------------------------------*/
/*	change_vol		*/
/*------------------------------*/

/* THIS ROUTINE NOT CURRENTLY USED; VOLUME SET ONLY AT START/STOP.
   IN PRACTICE I THINK THE AUDIBLE DIFFERENCE WILL BE MINOR. */

/* Change sound volume (under game control).  On Amiga the volume 
   may range from 0-63 (64?).  */

VOID change_vol (soundvol)	/* called only /after/ sound is started */
WORD soundvol;
{
	register struct IOAudio *curIOA;
/*	WORD i;  */

/* Change applies to channel 0 only (until I figure out how audion()
    extracts channel info from the midi file). */

	curIOA = IOA[0];
	if (curIOA) {
		curIOA->ioa_Request.io_Flags = ADIOF_PERVOL;
		curIOA->ioa_Volume = soundvol;
		BeginIO((struct IORequest *)curIOA);
		}
}

/*------------------------------*/
/*	int_sound		*/
/*------------------------------*/

/* Machine interrupt - check for end of sound.
   This interrupt routine is active at all times, for simplicity.

   If a sound has been started (on any channel) and we have NOT previously
   detected its completion, we check now.  If we detect its completion,
   we set the appropriate global "stop" flag.  
*/

void int_sound()
{
}
#endif  /* SDEADCODE */

