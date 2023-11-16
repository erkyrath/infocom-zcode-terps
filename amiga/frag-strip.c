/*------------------------------*/
/*	strip_spaces		*/
/*------------------------------*/

/* Remove any leading and/or trailing spaces from savenames, before
   the file is created.  AmigaDOS permits them, but they lead to confusing
   RESTORE errors.  (We continue to allow embedded spaces.)  */

VOID
strip_spaces (filename)
register CHAR *filename;
{
	register CHAR *p;

	p = filename;
	while (*p == 32)	/* skip over any leading spaces */
		p++;
	if (filename != p)
		strcpy (filename, p);	/* move the string up */

	p = filename + strlen (filename);
	while ((p != filename) && (*(p-1) == 32))
		p--;
	*p = 0;			/* chop off any trailing spaces */
}

