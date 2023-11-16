
  if (md_prnready() != 0)	/* print device not ready? */
    if (md_prnalert() != 0)	/* CANCEL chosen by user? */
      {
      /* [error] */
      }
  /* [okay] */

/*----------------------------------------------------------------------*/
/*	ST Printer Functions						*/
/*----------------------------------------------------------------------*/

/* The "standard print device" addressed by these routines can be connected,
   I think, to either the parallel or serial port.  The default can be changed
   using the printer desk accessory -- dbb */

#define PRNERR -1	/* Note: Gemdos has these exactly reversed! */
#define PRNOK 0

int md_prnready()
{
/* Unlike Cprnout(), this call returns immediately (no timeout), regardless
   of the printer status.  */

	if (Cprnos() == -1)	/* check printer status -- available? */
		return(PRNOK);	/* yes */
	else return(PRNERR);	/* no */
}

int md_prnalert()	/* called after a printer error */
{
/* Pop up an alert box, using the tasteful AES form_alert() call, and give
   the user a chance to retry or abort.
   (This probably should have been handled by the Gemdos printer code, as
   with disk errors.) */

	int exitbutton, status = PRNERR;
	char *prnerr = "[2][The printer is not responding.|Check the cable, printer|switches, etc.|Click OK to continue, Cancel|to terminate printing.][OK|Cancel]";
	while (status != PRNOK)
		{
		v_show_c (handle, 0);		/* show mouse */
		exitbutton = form_alert(1, prnerr);
		v_hide_c (handle);		/* hide mouse */
		if (exitbutton == 2)
			return(PRNERR);		/* user chose CANCEL */

/* Check immediately whether the printer is now really available.
   This avoids another 30 second timeout if the user clicked OK but didn't
   connect the printer. */
		status = md_prnready();
		}
	return(PRNOK);
}

int md_prnc(ch)		/* print one character */
unsigned char ch;	/* unsigned since might use high bit */
{
	int result;

/* Attempt to print a char.  If a problem exists with the printer, Cprnout
   will wait (time out) until the problem is corrected or for 30 seconds,
   whichever comes first.

   Although Cprnout is listed as VOID, it seems to return a result similar
   to Cprnos.  We must depend on this for our error handling.

   Since this routine is usually called repeatedly from a tight loop, a brief
   to moderate wait may be required for the previous output to complete 
   (especially if it was Return or Form Feed).  Calling the printer status 
   function here would be tricky for this reason.
*/
	do {

/* Check the printer status FIRST.  This should prevent ... hmm ...
   (will this return "temporarily busy (with last CR), not ready" errors? )

		if (md_prnready() != PRNOK)


		result = Cprnout(ch); 	/* output the char */
		if (result != -1)			/* something wrong? */
			if (md_prnalert() != PRNOK)	/* yes, abort? */
				return(PRNERR);		/* yes */
		}
		while (result != -1);	/* keep trying until success */
	return(PRNOK);
}

int md_prns(p, nbytes)			/* print a string */
register unsigned char *p; int nbytes;
{
/* [The gemdos printer interface handles only single chars] */
	int i;
	for (i=0; i<nbytes; i++)
		if (md_prnc(*p++) != PRNOK)
			return(PRNERR);	/* error, abort */
	return(nbytes);			/* success, return #bytes written */
}

