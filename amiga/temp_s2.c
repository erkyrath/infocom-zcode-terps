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

/* This "pseudo-interrupt" is called from the 68K kernel. 
   Check for end-of-sound flag; if detected, reset it and return TRUE. */

BOOL end_sound ()
{
}

