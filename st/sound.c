
/*------------------------------------------------------*/
/*	ST Sound routines (high level)			*/
/*------------------------------------------------------*/

/*  Modification history
    01 Feb 88	dbb	broken out into separate file
*/

#include "portab.h"
#include "obdefs.h"
#include "osbind.h"

#define VOID /**/
#define DEADCODE 1

extern void _pump_sound();	/* 68K */
extern void _stop_sound();
extern char *_zalloc();		/* C */

/* defined below (exports):
    VOID _init_sound();
    VOID _zsound();
*/

#define	SNDLEN 64*1024		/* max sound file length */

unsigned char *sbuf = 0;
long s_chan = 0;		/* pos = handle, zero/neg = error */

/*------------------------------*/
/*	_init_sound		*/
/*------------------------------*/

_init_sound ()			/* once only, during startup */
{
	sbuf = (char *) _zalloc (SNDLEN);
}

/*------------------------------*/
/*	load_sound		*/
/*------------------------------*/

int load_sound (id)
WORD id;
{
	char idstr[16], fname[32];
	long flen;
	WORD *p;

	if (!sbuf)			/* should already be inited ... */
		return (-1);

	idstr[0] = 'S';			/* use id to make generic name */
/**	stccpy (idstr, "S", 16);  **/
	stcu_d (&idstr[1], id, 15);

/* search path:
    1. a 'SOUND' subdirectory (if game disk has enough space)
    2. a second disk, in drive A (careful, OS will prompt if no disk) */

	stccpy (fname, "SOUND\\", 32);
	strcat (fname, idstr);
/**	strcat (fname, ".NAM");  **/
	s_chan = Fopen (fname, 0);		/* first try path 1 */
	if (s_chan <= 0) {
		stccpy (fname, "A:\\", 32);
		strcat (fname, idstr);
		s_chan = Fopen (fname, 0);	/* if err, try path 2 */
		}
	if (s_chan <= 0)
		return (-2);

	flen = Fread (s_chan, SNDLEN, sbuf);
	if (flen < 0)
		return (-3);

/* to make sure we have a valid sound file, compare the two lengths.
   [sign extensions by the compiler don't affect this] */
	p = (WORD *) sbuf;
	if (p[0] != p[4] + 8)
		return (-4);

	return (0);
}

/*------------------------------*/
/*	unload_sound		*/
/*------------------------------*/

unload_sound ()
{
	if (s_chan > 0) {		/* successful open? */
		Fclose (s_chan);	/* close and mark */
		s_chan = 0;
		}
}

/*------------------------------*/
/*	_zsound			*/
/*------------------------------*/

#define S_INIT 1	/* load data from disk */
#define S_START 2
#define S_STOP 3	/* stopped, but ready to restart */
#define S_CLEANUP 4	/* release the memory used */

#define S_BEEP 1
#define S_BOOP 2

/* id=1..n or 0(mru), action=1..4, 
   volume=0..64 or -1(midi), repeat=1..n, 0(infinite) or -1(midi) */

WORD _zsound (id, action, vol, reps)
WORD id, action, vol, reps;
{
	static WORD last_id = 0;
	static BOOL loaded = 0, started = 0;
	WORD err;

/**	if (id == S_BEEP || id == S_BOOP) {	/-* handled in 68K *-/
		bell_sound(id);
		return(0);
		}	**/

	if (id == 0) {			/* use MRU effect */
		if (last_id == 0)  return (0);		/* none, exit */
		else  id = last_id;
		}
	else if (id != last_id) {	/* first get rid of old, if any */
		_zsound (last_id, S_CLEANUP, vol, reps);  /* (recurse) */
		last_id = id;
		}

	if (reps < 0)  reps = 1;
/**	if (vol < 0)  vol = 8;  **/	/* ST: no volume, ignored */

	if (action == S_INIT || action == S_START) {
		if (!loaded) {
			err = load_sound (id);
			if (err) {
			/**	showerr (err);  **/  /* inform user */
				return (err);
				}
			loaded = 1;
			}
		}
	if (action == S_START) {
/***		if (started)			/-* reset if 2nd start *-/
			_stop_sound ();		/-* [now checked in subr] *-/
***/
		_pump_sound(sbuf, reps);
		started = 1;
		}
	if (action == S_STOP || action == S_CLEANUP) {
		if (started) {
			_stop_sound();
			started = 0;
			}
		}
	if (action == S_CLEANUP) {		/* free resources */
		if (loaded) {
			unload_sound ();
			loaded = 0;
			}
		last_id = 0;		/* back to original state */
		}
	return (0);
}

/*------------------------------*/
/*	showerr			*/
/*------------------------------*/

/* inform user of audio problem (currently a silent error) */
showerr (err)
WORD err;
{
}
