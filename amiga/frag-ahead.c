
CHAR	ahead_buffer[64];	/* our type-ahead buffer */
INT	aheads = 0;		/* number of chars currently in buffer */

/* A buffer to save typed-ahead chars, in addition to the OS facility for 
   this purpose, is desirable.  This is because writing and reading escape
   sequences to the Amiga console device (for display control) interferes 
   with normal type-ahead.

   Without our own buffer, type-ahead between escape seqs would be lost.
*/

/*------------------------------*/
/*	queue_ahead		*/
/*------------------------------*/

/* Temporarily save a typed-ahead char.  Useful whenever the char would
   otherwise be throw away, such as when reading escape seqs from the console
   device, or after a [MORE] prompt.
*/

VOID
queue_ahead (the_char)

CHAR	the_char;
{
	if (aheads < sizeof (ahead_buffer))	/* only if there's room */
		{
		ahead_buffer[aheads] = the_char;
		aheads++;
		}
}

/*------------------------------*/
/*	deque_ahead		*/
/*------------------------------*/
CHAR
deque_ahead ()		/* return the oldest typed-ahead char, zero if none */
{
	CHAR	the_char;
	INT	i;

	if (aheads == 0)
		the_char = 0;		/* empty buffer, return null */
	else
		{
		the_char = ahead_buffer[0];	/* get first-in char */
		aheads--;

		for (i=0; i<aheads; i++)	/* shift up the rest */
			ahead_buffer[i] = ahead_buffer[i+1];
		}

	return (the_char);
}

/*----------------------------------------------------------------------*/
/*	High-level "Queue Users"					*/
/*----------------------------------------------------------------------*/

/*------------------------------*/
/*	get_control_seq		*/	/* read chars into given buffer */
/*------------------------------*/
VOID
get_control_seq (buffer, end_mark)

CHAR	*buffer;
CHAR	end_mark;
{
	CHAR	the_char;
	INT	len;

/* We are waiting for a certain sequence, which always begins with $9B and
   ends with the given end_mark.  Must ignore (1) other sequences which may be
   generated at any time by pressing special keys, and (2) normal keys.

   Other seqs also start with $9B but (hopefully) don't contain end_mark.
   Known examples are cursor keys (A-D), functions (0~-9~), and Help key (?~).

   Save any ignored keys in the type-ahead buffer.
*/

	while ((the_char = read_console ()) != begin_mark)
		{
		queue_ahead (the_char);
		}
	len = 0;		/* note that we DO throw away the begin mark */

	while ((the_char = read_console ()) != end_mark)
		{
		if (the_char != begin_mark)
			{
			buffer[len] = the_char;
			len++;
			}
		else		/* starting new sequence */
			{
/* At this point we have read a second begin mark, without finding the end 
   mark we want.  The first seq must have been garbage.

   Save the keys we got in the type-ahead buffer, and wait for new seq.
*/
			for (i=0; i<len; i++)
				queue_ahead (buffer[i]);
			len = 0;
			}
		}

	buffer[len] = end_mark;		/* end with the ending mark */
	buffer[len+1] = 0;		/* make it ASCIZ */
}

/*------------------------------*/
/*	char_in			*/
/*------------------------------*/
CHAR
char_in ()	/* wait for keyboard input */
{
	CHAR	the_char;

	the_char = deque_ahead ();	/* first check for old type-ahead */

	if (the_char == 0)
		the_char = read_console ();	/* none, get normal char */

	return (the_char);
}

/*------------------------------*/
/*	char_wait		*/
/*------------------------------*/

/* If the type-ahead buffer already has something in it, just return.
   Otherwise, wait for a single char, and save it in buffer.

   Call this routine to pause screen output (e.g., after a MORE).
*/

VOID
char_wait ()
{
	if (!aheads)
		queue_ahead (read_console ());
}
