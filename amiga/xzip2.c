
/************************************************************************/
/*	misc file functions 						*/
/************************************************************************/

/*------------------------------*/
/*	getcwd			*/
/*------------------------------*/

/* Find the pathname of the current working directory.  Uses a recursive
   call to the followpath function.  Its job is to continue to call the 
   ParentDir function until it obtains a lock value of zero, and to append
   (to the given string) the name of the directory it pops into at each step.
   In the root directory, the lock gives us the volume name of the disk
   (how do we get the drive name instead?).  The program appends a colon
   after the volume name and a slash after each subdirectory along the way
   to the one we're in currently.  [Debugged from example in Peck p36.]
*/

extern struct FileLock *Lock(), *ParentDir();

getcwd (path)
char *path;
{
    struct FileLock *cwdlock;
    path[0] = 0;		/* start with null string */

    /* get a Read lock on the current directory.  NOTE: followpath 
       unlocks the lock */

    cwdlock = Lock ("", ACCESS_READ);
    if (cwdlock != 0)
	followpath (cwdlock, path, 1);	/* add a final slash */
}

int followpath (lock, path, showslash)
struct FileLock *lock;
char *path;
int showslash;
{
    struct FileInfoBlock *myinfo;
    struct FileLock *newlock;
    int success, error;

    /* if we reach the end of the road, just exit */
    if (!lock) return (0);

    /* see if this dir has a parent; might fail because of an I/O error
       or bacause somebody took out the disk  */
    newlock = ParentDir (lock);
    error = IoErr ();

    /* Gross bug: error #205 (object not found?) seems to get reported 
       about half the time for no apparent reason.  Works correctly if ignored, 
       so do so ... */
    if (error == 205)  error = 0;

    /* recursively call this same function to follow path up to the root */
    if (!error)
	error = followpath (newlock, path, 1);

    if (!error) {
	/* alloc memory AFTER the recursion, to minimize total usage */
	myinfo = (struct FileInfoBlock *)
	    AllocMem (sizeof(struct FileInfoBlock), MEMF_CLEAR);

	if (myinfo == 0) error = -1;
	else {
	    success = Examine (lock, myinfo);

	    if (!success) error = -1;
	    else {
		strcat (path, myinfo->fib_FileName);	/* dir name */

		if (newlock == 0) 		/* (actually vol name) */
		    strcat (path, ":");
		else if (showslash)
		    strcat (path, "/");
		}
	    FreeMem (myinfo, sizeof(struct FileInfoBlock));
	    }
	}
    UnLock (lock);

    if (error) 		/* make null the global string */
	path[0] = 0;
    return (error);	/* (zero if ok) */
}

/*------------------------------*/
/*	geterr			*/
/*------------------------------*/

/* This is a front end for IoErr.  It should be called /only/ when we know 
   an error occurred, and so expect a nonzero result.  But IoErr sometimes 
   returns a zero anyway (e.g., after Create with a bad pathname).  In 
   this case we return -1, so 68K stuff will know something went wrong. */

LONG geterr ()
{
	LONG err = IoErr ();

	if (!err)  err = -1;	/* make sure it's nonzero */
	return (err);
}

/************************************************************************/
/*	File Selection							*/
/************************************************************************/

/*------------------------------*/
/*	file_select		*/
/*------------------------------*/
WORD
file_select (opsave, drive)	/* get file and pathspec from user, */
				/*   combine and leave in SAVENAME  */

WORD	opsave;		/* zero if restore, otherwise save */
CHAR	drive;		/* drive to use, zero for default */
{
	/* AMIGA LACKS AN OS ROUTINE - IMPLEMENTED IN KERNEL */
}

/*------------------------------*/
/*	new_default		*/
/*------------------------------*/

VOID
new_default (okay)	/* called at end of each SAVE/RESTORE */

BOOL	okay;
{
	if (okay)	/* dest <- source */
		{
		strcpy (&saveback, &savename);	/* update old defaults */
		}
	else
		{
		strcpy (&savename, &saveback);	/* retrieve old defaults */
		}
}

/*------------------------------*/
/*	strip_spaces		*/
/*------------------------------*/

/* Remove any leading and/or trailing spaces from savenames, before
   the file is created.  AmigaDOS permits them, but they lead to confusing
   RESTORE errors.

   Note that we continue to allow embedded spaces.
*/

VOID
strip_spaces (filename)

CHAR	*filename;
{
	CHAR	*leading;
	WORD	len;

	leading = filename;
	while (*leading++ == 32)	/* skip over a leading space */
		{
		strcpy (filename, leading);	/* move the string up */
		--leading;
		}

	len = strlen (filename);
	while (len && (filename[len-1] == 32))
		{
		filename[len-1] = 0;	/* chop off any trailing spaces */
		--len;
		}
}

/*------------------------------*/
/*	divide_pathname		*/
/*------------------------------*/

/* Check whether a given filename is prefixed with a drive or directory name.
   If so, return a pointer to the bare filename, otherwise return NULL.

   Path syntax for Amiga is "DFn:PATH1/PATH2/PATHn/filename".
*/

CHAR *
divide_pathname (pathname)

CHAR	*pathname;
{
	CHAR	*special;	/* <:> and </> are the special chars */

/* if one or more directories were given, find the last one */

	special = (CHAR *) strrchr (pathname, '/');

/* otherwise, see if a drive was given */

	if (special == NULL)
		special = (CHAR *) stpchr (pathname, ':');

	if (special != NULL)	/* found one, skip over the delimiter */
		special++;

	return (special);	/* pointer to bare filename, or NULL */
}

/*------------------------------*/
/*	drive_default		*/
/*------------------------------*/

/* If savename lacks a drive/directory spec but saveback includes one 
   (the default), prefix it to savename.

   Also, take this opportunity to remove any leading/trailing spaces from
   savename.
*/

VOID
drive_default ()
{
	CHAR	*barename;
	CHAR	temp[64];

/* check whether savename includes a drive/directory */

	barename = (CHAR *) divide_pathname (&savename);

	if (barename == NULL)		/* Nope */
		{
		strip_spaces (&savename);

/* check whether a default drive/directory exists */

		strcpy (&temp, &saveback);

		barename = (CHAR *) divide_pathname (&temp);

		if (barename != NULL)		/* Yep */
			{
/* and prefix the default drive/directory onto savename */

			*barename = 0;		/* chop off default filename */
			strcat (&temp, &savename);
			strcpy (&savename, &temp);
			}
		}

/* user DID supply a drive/directory, just check for spaces in filename */

	else
		{
		strip_spaces (barename);
		}
}

#if bug_DiskFull	/* BUG GONE, THIS IS DEAD CODE */

/*------------------------------*/
/*	disk_bytes_avail	*/
/*------------------------------*/

LONG
disk_bytes_avail ()	/* returns number of available bytes on save disk, 
			   -1 if error. Must call AFTER save file is created */
{
	struct FileLock *filelock;
	LONG	avail;

	WORD	pad1;
struct	InfoData    ZInfoData;		/* information about the save disk */
	WORD	pad2;

struct	InfoData    *ZInfoDataPtr;	/* pointer to the above */
	LONG	ptr;


/* Must first obtain a "lock" for a valid file on the desired disk.
   We conveniently use the save file (thus it must already exist). */

	filelock = Lock (&savename, ACCESS_READ);

	if (!filelock)			/* if error, exit now */
		return (-1);

/* The InfoData structure must be longword aligned (for AmigaDOS).  
   Adjust alignment with the pad bytes, if necessary. */

	pad1 = pad2 = 0;	/* make the Compiler stop whining */

	ptr = (LONG) &ZInfoData;
	ptr = ptr>>2;		ptr = ptr<<2;
	ZInfoDataPtr = (struct InfoData *) ptr;

/* Determine how much space remains on the save disk. */

	if (Info (filelock, ZInfoDataPtr))
		{
		avail = 
			(ZInfoDataPtr->id_NumBlocks
			- ZInfoDataPtr->id_NumBlocksUsed)	/* free blks */
			* ZInfoDataPtr->id_BytesPerBlock;
		}
	else	avail = -1;	/* something wrong, fail */

	UnLock (filelock);	/* make sure to undo this lock */

	return (avail);
}

#endif	/* bug_DiskFull */

/************************************************************************/
/*	Disk I/O							*/
/************************************************************************/

/*------------------------------*/
/*	read_file		*/
/*------------------------------*/

LONG
read_file (channel, offset, length, buffer)

LONG	channel;
LONG	offset, length;
CHAR	*buffer;
{
	LONG	former_pos;
	LONG	actual_len;
	LONG	error;

/* disk thrashing is reduced if unnecessary seeks are avoided */

	if ((channel != old_channel) || (offset != old_offset))
		{
		former_pos = Seek (channel, offset, -1);	/* AmigaDOS */
		if (former_pos == -1)
			{
			old_offset = -1;	/* force seek next time */
			return (geterr());
			}
		}

	old_channel = channel;
	old_offset = offset + length;

	actual_len = Read (channel, buffer, length);
	if (actual_len == -1)
		{
		old_offset = -1;	/* force seek next time */
		error = geterr();
		}

/* Check the number of bytes actually read.  This will detect certain 
   invalid save files (for example, files where Amiga crashed during their
   creation, for whatever reason).

   We allow for a special case of actual length being 256 less than expected.
   The kernel does paging in 512 byte blocks, while the Amiga TFTP utility 
   transfers data files in 256 byte blocks.
*/
	else if ((actual_len == length) || (actual_len == (length - 256)))

		error = 0;		/* zero means no error */

	else
		{
		old_offset = -1;
		error = 1;		/* length error */
		}

	actlen = actual_len;		/* save as global (for 68K) */
	return (error);
}


/*------------------------------*/
/*	write_file		*/
/*------------------------------*/

LONG
write_file (channel, offset, length, buffer)

LONG	channel;
LONG	offset, length;
CHAR	*buffer;
{
	LONG	error, actual_len;

/* Errors: zero means none, 999 means disk full, otherwise geterr() */

#if bug_DiskFull

	if (disk_newsave)		/* FALSE if writing over old file */
		{
		disk_free = disk_free - length;
		if (disk_free < 0)
			return (999);		/* not enough room for write */
		}
#endif
	error = Seek (channel, offset, -1);
	if (error == -1)
		return geterr();

	actual_len = Write (channel, buffer, length);
	if (actual_len == -1)
		return geterr();

	else if (actual_len != length)	/* (should have been caught above) */
		return (999);

	actlen = actual_len;	/* save as global (for 68K) */
	return (0);		/* no error */
}


/*------------------------------*/
/*	create_file		*/
/*------------------------------*/

LONG
create_file ()		/* create (if needed), and open, a SAVE file */
{
	register LONG fd;

/* two ways to enter this routine:  if disk_newsave is TRUE, we are creating
   a new save file, otherwise we are writing over an old one */

	fd = Open (&savename, MODE_NEWFILE);

#if bug_DiskFull	/* avoid a write error */

	if  (fd && disk_newsave)
		{
/* we just created the file, but must close it momentarily to get disk info.
   Otherwise the Lock call fails */

		Close (fd);

/* determine how much free space is on the disk, so file_write can fail
   gracefully if necessary */

		disk_free = disk_bytes_avail ();

		fd = Open (&savename, MODE_NEWFILE);
		}
#endif	/* bug_DiskFull */

	save_ref = fd;			/* save global */
	if (!fd)
		return (geterr());
	else
		return (0);		/* zero means OK */
}


/*------------------------------*/
/*	open_file		*/
/*------------------------------*/

LONG
open_file ()		/* open an file to RESTORE from */
{
	LONG fd;

	fd = Open (&savename, MODE_OLDFILE);

	save_ref = fd;
	if (!fd)
		return (geterr());
	else
		return (0);		/* zero means OK */
}

/*------------------------------*/
/*	close_file		*/
/*------------------------------*/

LONG
close_file ()			/* close a SAVE file */
{
	if (save_ref) {
		Close (save_ref);
		save_ref = 0;
		}

	old_channel = -1;	/* make sure to reset this one */
	return (0);		/* no error codes? */
}


/*------------------------------*/
/*	delete_file		*/
/*------------------------------*/

LONG
delete_file ()
{
	LONG	success;	/* actually BOOLEAN */

	success = DeleteFile (&savename);

	if (success)
		return (0);
	else	return (1);
}


/*------------------------------*/
/*	exist_file		*/
/*------------------------------*/

LONG
exist_file ()		/* called before every SAVE */
{
	struct FileLock *fl;

	fl = Lock (&savename, ACCESS_WRITE);
	if (fl)				/* non-zero if already exists */
		{
		disk_newsave = FALSE;	/* set the global */
		UnLock (fl);		/* remove the lock */
		}
	else	disk_newsave = TRUE;

	return ((LONG) fl);
}

/*------------------------------*/
/*	open_game		*/
/*------------------------------*/

LONG
open_game()
{
	game_ref = Open (gamename, MODE_OLDFILE);

	if (game_ref == 0)
		return (geterr());
	else	return (0);		/* zero means OK */
}

/*------------------------------*/
/*	close_game		*/
/*------------------------------*/

LONG
close_game ()
{
	if (game_ref) {
		Close (game_ref);
		game_ref = 0;
		}
	old_channel = -1;	/* make sure to reset this one */
	return (0);
}


/*------------------------------*/
/*	make_icon_file		*/
/*------------------------------*/

LONG
make_icon_file ()	/* create a new save file icon, */
			/*   called only after a successful SAVE */
{
	LONG	error;
	LONG	object;
/*	WORD	len;	*/

/* diskobj (should have been) read into memory during init */

	if (diskobj == NULL)
		error = 1;
	else
		{

#if bug_DiskFull

/* PROBLEM: PutDiskObj is crashing the machine when a disk-full error occurs.
     First, make sure we have enough room for a new icon file.
*/

#define     MaxIconSize     1024	/* actually ours is under 400 */

		if (disk_newsave)	/* if OLD save, space not an issue */
			{
			disk_free = disk_bytes_avail ();
			if (disk_free < MaxIconSize)	return (1);
			}

/* (Alternate fix.  The preceeding code fixes this problem better.)

   Do some preliminary error checking:  Attempt to create a file with the 
   desired name, and put 1K of random data into it.
*/
		strcat (&savename, ".info");	/* desired name */
		error = create_file ();		/* --> save_ref */

		if (!error)
			{
			error = write_file (save_ref, 0, 1024, &savename);

			close_file (save_ref);	/* (no error codes) */

			if (!error)
				error = delete_file ();    /* <-- savename */
			else		delete_file ();
			}

		len = strlen (&savename);
		savename[len-5] = 0;		/* chop off the ".info" */

		if (error)			/* if ANY error, fail now */
			return (error);

#endif	/* bug_diskfull */


/* Everything seems OK, go ahead and make the Save icon */

		diskobj->do_CurrentX = NO_ICON_POSITION;
		diskobj->do_CurrentY = NO_ICON_POSITION;

	/* write icon file via same path as save file */

		object = PutDiskObject (&savename, diskobj);

		if (object == NULL)
			error = geterr();
		else	error = 0;
		}

	return (error);
}

/************************************************************************/
/*	Scripting							*/
/************************************************************************/

/*------------------------------*/
/*	raw_script		*/
/*------------------------------*/

WORD
raw_script (buffer, length)	/* send data to the printer device */

LONG	buffer, length;
{
	WORD	error;
	LONG	seconds1, micros1;
	LONG	seconds2, micros2;
	LONG	actual_len;

/* A problem:  AmigaDOS (rev 1.0) isn't passing back printer error codes.
   Each attempt to print when the printer isn't ready causes a 30 second
   timeout.  A series of these effectively hangs the game.

   To fix, use the delay to infer an error.  If no error code is returned
   but the delay exceeds 30 seconds, consider the printer not ready.
*/

	CurrentTime (&seconds1, &micros1);	/* starting time */

	actual_len = Write (print_file, buffer, length);
	if (actual_len == length)
		error = 0;
	else	error = 1;

/*	SIOStdReq.io_Data = (APTR) buffer;
	SIOStdReq.io_Length = length;

	SIOStdReq.io_Command = CMD_WRITE;
	DoIO (&SIOStdReq);
	error = (SIOStdReq.io_Error);
*/
	CurrentTime (&seconds2, &micros2);

	if (!error)	/* says no error, but is it lying? */
		{
		if ((seconds2 - seconds1) >= (30 - 1))
			error = 99;
		}

	return (error);			/* error code, zero if OK */
}

/*------------------------------*/
/*	script_init		*/
/*------------------------------*/

/*
  Two approaches to printing were tried; neither returns errors.  The hairy 
  one crashes (CloseDevice) when multi-tasked, so use the simple one.
*/

LONG
script_init (open)	/* initialize/uninitialize the printer */

WORD	open;
{
	LONG	error;

	if (open)
		{
		print_file = Open ("PRT:", MODE_OLDFILE);

		if (print_file)		/* zero means error */
			error = 0;
		else	error = 1;

/**		OpenDevice ("printer.device", 0, &SIOStdReq, 0);

	/-* set up the message port in the I/O request  *-/

		SMsgPort.mp_Node.ln_Type	= NT_MSGPORT;
		SMsgPort.mp_Flags		= 0;
		SMsgPort.mp_SigBit		= AllocSignal (-1);
		SMsgPort.mp_SigTask	= (struct Task *) FindTask (NULL);

		AddPort (&SMsgPort);
		SIOStdReq.io_Message.mn_ReplyPort = &SMsgPort;
**/		}

	else
		{
		if (print_file)			/* call only if once opened */
			Close (print_file);

		error = 0;			/* no close errors? */

	/* clean up the various structures we created */

/**		RemPort (&SMsgPort);
		FreeSignal (SMsgPort.mp_SigBit);

		CloseDevice (&SIOStdReq);
**/		}

	return (error);		/* zero if none */
}

/*------------------------------*/
/*	script_open		*/
/*------------------------------*/

WORD
script_open (start)	/* kernel issued request to start/stop scripting,
			   return status, either active or inactive  */
WORD	start;
{
	CHAR	reset_seq[2];
	LONG	error;

/* starting, may need to open/reset the printer */

	if (start)
		{
		if (!prt_inited)	/* device never opened, do so now */
			{
			error = script_init (TRUE);

			if (!error)
				prt_inited = TRUE;	/* okay, opened */
			}

		if (prt_inited && !prt_active)	/* new start, reset printer */
			{

			reset_seq[0] = '\33';	/* ESC ($1B) */
			reset_seq[1] = 'c';	/* 'c' means reset */

			error = raw_script (&reset_seq, 2);

			if (!error)
				prt_active = TRUE;	/* okay, active */
			}
		}

	else			/* if stop, flag it and shut down */
		{
	 	prt_active = FALSE;

		if (prt_inited)
			{
			script_init (FALSE);	prt_inited = FALSE;
			}
		}

	return (prt_active);
}

/*------------------------------*/
/*	script_line		*/
/*------------------------------*/

WORD
script_line (buffer, length)	/* script a line, tack on an EOL,
				     return error code (zero if none) */
LONG	buffer, length;
{
	CHAR	eol_seq[2];
	WORD	error;

	if (length > 0)
		error = raw_script (buffer, length);
	else	error = 0;

/* The EOL seq used to be an "ISO standard" Esc-E (1B45), but under
   AmigaDOS 1.2 this seems to have no effect.  Changing to CRLF (0D0A)
   works under both 1.1 and 1.2 except that the spacing is too wide 
   (resembles triple, used to resemble double).  It appears that AmigaDOS 
   (/not/ the printer) automatically makes the initial CR a CRLF.  So,
   we will output only the CR and hope all printers are happy.

   By the way, a way to check script spacing is to compare it to what
   AmigaDOS does after typing "TYPE textfile TO PRT:".  Also, some
   printer behavior can be controlled from User Preferences, like
   margins and 6- or 8-line spacing.
*/
	if (!error)
		{
		eol_seq[0] = 0x0D;
/**		eol_seq[1] = 0x0A;	**/

		error = raw_script (&eol_seq, 1);
		}
	return (error);
}

