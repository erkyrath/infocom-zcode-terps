
/* *** main.c ***************************************************************
 *
 * File IO Suite  --  Example Main 
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"


BOOL TestFileIO();


struct NewScreen NewFileIOScreen =
	{
	0, 0,	/* LeftEdge, TopEdge */
	320, 200, /* Width, Height */
	2, /* Depth */
	0, 1,	/* Detail/BlockPens */
	NULL,	/* ViewPort Modes (must set/clear HIRES as needed) */
	CUSTOMSCREEN,
	&SafeFont,	/* Font */
	(UBYTE *)"Example FileIO Program's Screen",
	NULL,	/* Gadgets */
	NULL,	/* CustomBitMap */
	};

struct NewWindow NewFileIOWindow =
	{
	0, 12,					/* LeftEdge, TopEdge */
	320, 150, 			/* Width, Height */
	-1, -1,				/* Detail/BlockPens */
	NEWSIZE | MOUSEBUTTONS | VANILLAKEY | MENUPICK | CLOSEWINDOW
			| DISKINSERTED,
							/* IDCMP Flags */
	WINDOWSIZING | WINDOWDRAG | WINDOWDEPTH | WINDOWCLOSE
			| SIZEBRIGHT | SMART_REFRESH | RMBTRAP | ACTIVATE | NOCAREREFRESH,
							/* Window Specification Flags */
	NULL,					/* FirstGadget */
	NULL,					/* Checkmark */
	(UBYTE *)"FileIO Requester Window",  /* WindowTitle */
	NULL,					/* Screen */
	NULL,					/* SuperBitMap */
	96, 30,				/* MinWidth, MinHeight */
	640, 200,			/* MaxWidth, MaxHeight */
	WBENCHSCREEN,
	};



VOID main(argc, argv)
LONG argc;
char **argv;
{
	struct Screen *screen;
	struct Window *window;
	struct FileIOSupport *fileio1, *fileio2;
	BOOL mainswitch, ioswitch, mainsuccess;
	LONG class;
	struct IntuiMessage *message;
	SHORT pick;

	mainsuccess = FALSE;

	screen = NULL;
	window = NULL;
	fileio1 = fileio2 = NULL;

	IntuitionBase = (struct IntuitionBase *)
			OpenLibrary("intuition.library", 0);
	GfxBase = (struct GfxBase *)OpenLibrary("graphics.library", 0);
	DosBase = (struct DosLibrary *)OpenLibrary("dos.library", 0);
	IconBase = (struct IconBase *)OpenLibrary("icon.library", 0);

	if ( (IntuitionBase == NULL)
			|| (GfxBase == NULL)
			|| (DosBase == NULL)
			|| (IconBase == NULL) )
		goto MAIN_DONE;

	if (argv)
		{
		/* OK, we started from CLI */
		if (argc > 1)
			{
			if (screen = OpenScreen(&NewFileIOScreen))
				{
				NewFileIOWindow.Screen = screen;
				NewFileIOWindow.Type = CUSTOMSCREEN;
				}
			}
		}

	window = OpenWindow(&NewFileIOWindow);

	fileio1 = GetFileIOSupport();
	fileio2 = GetFileIOSupport();

	if ((window == NULL) || (fileio1 == NULL) || (fileio2 == NULL))
		goto MAIN_DONE;

	SetFlag(fileio2->Flags, WBENCH_MATCH | MATCH_OBJECTTYPE);
	fileio2->DiskObjectType = WBTOOL;
	fileio2->ReqTitle = (UBYTE *)"- Workbench Tools and Drawers -";

	SetAPen(window->RPort, 1);
	Move(window->RPort, 25, 40);
	Text(window->RPort, "= Click for FileIO Requester =", 30);

	mainswitch = TRUE;
	pick = 0;

	while (mainswitch)
		{
		WaitPort(window->UserPort);

		ioswitch = FALSE;
		while (message = GetMsg(window->UserPort))
			{
			class = message->Class;
			ReplyMsg(message);

			switch (class)
				{
				case CLOSEWINDOW:
					mainswitch = FALSE;
					break;
				case DISKINSERTED:
					/* You should clear the GOOD_FILENAMES flag whenever you
					 * detect that a new disk was inserted.
					 */
					ClearFlag(fileio1->Flags, GOOD_FILENAMES);
					ClearFlag(fileio2->Flags, GOOD_FILENAMES);

					/* While I'm here, I'll demo another feature for you. */
					ToggleFlag(fileio1->Flags, USE_VOLUME_NAMES);
					ToggleFlag(fileio2->Flags, USE_VOLUME_NAMES);

					break;
				default:
					/* If any other event occurs, bring up that old requester! */
					ioswitch = TRUE;
					break;
				}
			}

		if (ioswitch)
			{
			if (pick == 0)
				{
				if (TestFileIO(fileio1, window))
					/* Oops, disk swapped, so restart the other */
					ClearFlag(fileio2->Flags, GOOD_FILENAMES);
				}
			else
				{
				if (TestFileIO(fileio2, window))
					/* Oops, disk swapped, so restart the other */
					ClearFlag(fileio1->Flags, GOOD_FILENAMES);
				}
			pick = 1 - pick;
			}
		}

	mainsuccess = TRUE;

MAIN_DONE:
	if (NOT mainsuccess) Alert(ALERT_NO_MEMORY, NULL);

	if (fileio1) ReleaseFileIO(fileio1);
	if (fileio2) ReleaseFileIO(fileio2);

	if (window) CloseWindow(window);
	if (screen) CloseScreen(screen);

	if (IntuitionBase) CloseLibrary(IntuitionBase);
	if (GfxBase) CloseLibrary(GfxBase);
	if (DosBase) CloseLibrary(DosBase);
	if (IconBase) CloseLibrary(IconBase);
}



BOOL TestFileIO(fileio, window)
struct FileIOSupport *fileio;
struct Window *window;
/* This guy calls GetFileIOName(), displays the file name selected by the
 * user, and returns TRUE if the user swapped disks during GetFileIOName()
 * (else returns FALSE)
 */
{
	UBYTE name[80];

	if (GetFileIOName(fileio, window))
		{
		/* If user was positive, display the name */
		CopyString(&name[0], "[");
		BuildFileIOPathname(fileio, &name[1]);
		ConcatString(&name[0], "]");
		AlertGrunt(&name[0], window);
		}

	if (FlagIsSet(fileio->Flags, DISK_HAS_CHANGED))
		{
		ClearFlag(fileio->Flags, DISK_HAS_CHANGED);
		return(TRUE);
		}
	else return(FALSE);
}


/* *** fileio.c *************************************************************
 *
 * File IO Suite  --  Primary FileIO Requester Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 20 Oct 87    - RJ            Added RENAME_RAMDISK to fix what seems to 
 *                              be a bug in either AmigaDOS or Workbench.
 * 26 Aug 87    RJ              Added test for 0 GadgetID at end of 
 *                              GetFileIOName()
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"


/* These routines can be found in filesupp.c */
extern HandleGadget();
extern StartOpenRequester();
extern DiskInserted();
extern PropMouseMoves();



/* *** GetFileIOSupport() ***************************************************
 * 
 * NAME
 *     GetFileIOSupport  --  Allocate and initialize a FileIOSupport structure
 * 
 * 
 * SYNOPSIS
 *     struct FileIOSupport *GetFileIOSupport();
 * 
 * 
 * FUNCTION
 *     Allocates and initializes a FileIOSupport structure for use with
 *     calls to GetFileIOName().
 * 
 *     You may want to further initialize the structure before calling
 *     GetFileIOName().  Refer to the FileIO documentation for more
 *     information.
 * 
 *     When you're done with the structure, call ReleaseFileIO().
 * 
 * 
 * INPUTS
 *     None
 * 
 * 
 * RESULT
 *     If all goes well, returns the address of a FileIOSupport structure.
 *     If anything goes wrong (usually out of memory), returns NULL.
 * 
 * 
 * EXAMPLE
 *     struct FileIOSupport *fileio;
 *     fileio = GetFileIOSupport();
 *     GetFileIOName(fileio, window);
 *     ReleaseFileIO(fileio);
 * 
 * 
 * BUGS
 *     None known
 * 
 * 
 * SEE ALSO
 *     GetFileIOName(), ReleaseFileIO()
 */
struct FileIOSupport *GetFileIOSupport()
{
	struct FileIOSupport *fileio;

	if (fileio = (struct FileIOSupport *)AllocMem(
			sizeof(struct FileIOSupport), MEMF_CLEAR))
		{
		/* Anything special to initialize? */
		SetFlag(fileio->Flags, USE_VOLUME_NAMES | RENAME_RAMDISK);
		}
	return(fileio);
}



/* *** GetFileIOName() ******************************************************
 * 
 * NAME
 *     GetFileIOName  --  Gets a file name for input/output from the user
 * 
 * 
 * SYNOPSIS
 *     BOOL GetFileIOName(FileIO, Window);
 * 
 * 
 * FUNCTION
 *     This routine creates a filename requester which allows the user
 *     to browse through the AmigaDOS filesystem and select one of
 *     the filenames found there.
 * 
 *     The FileIO argument is a pointer to a FileIOSupport structure,
 *     which is allocated and initialized for you via a call to 
 *     GetFileIOSupport().
 *     You may preset the FileIO parameters before calling this routine, 
 *     or you may leave them set at their default values.  See the FileIO
 *     documentation for complete details.
 * 
 *     The Window argument is the pointer to the window structure returned
 *     by a call to Intuition's OpenWindow() function.  As this routine
 *     opens a requester and requesters open in windows, you must have
 *     already opened a window before calling this routine, even if it's
 *     a window opened for no other purpose than to call this routine.
 * 
 *     This routine returns a BOOL value of TRUE or FALSE, depending on
 *     whether the user chose to accept or cancel the filename selection 
 *     operation.  If TRUE, the filename selected by the user can be
 *     found in the FileIO structure FileName[] field.  This filename
 *     will have all leading and trailing blanks removed (in case the
 *     user typed in a filename with extraneous spaces).  Likewise,
 *     the pathname to the disk and drawer can be found in the text
 *     fields DiskName[] and DrawerName[].  You can construct 
 *     the pathname using these text strings.  Also, you can call 
 *     BuildFileIOPathname() to build the pathname automatically.
 * 
 *     There's a *lot* more to be said about this function.  Please 
 *     read the documentation.
 * 
 *     NOTE:  This routine is not re-entrant.  What this means 
 *     is that if you have created a program that has more than one task,
 *     this routine cannot be called by more than one task at a time.
 *     This is not a problem for the grand majority of programs.
 *     But if you have some application that would require calling this 
 *     routine asynchronously from multiple tasks, you'll have to 
 *     implement some quick semaphore arrangement to avoid collisions.
 *     No big deal, actually.  See Exec semaphores for everything you need.
 * 
 * 
 * INPUTS
 *     FileIO = pointer to a FileIOSupport structure, as allocated
 *         via a call to GetFileIOSupport()
 *     Window = pointer to a Window structure, as created via a call
 *         to Intuition's OpenWindow()
 * 
 * 
 * RESULT
 *     TRUE if the user decided that the filename selection was successful,
 *     FALSE if the user chose to cancel the operation
 * 
 * 
 * EXAMPLE
 *     if (GetFileIOName(fileio, window))
 *         ProcessFileName(&fileio->FileName[0]);
 * 
 * 
 * BUGS
 *     None known, though there could be some, and the disk selection
 *     subsystem logic is not perfectly polished (though it's believed 
 *     to be bug-free).
 * 
 * 
 * SEE ALSO
 *     BuildFileIOPathname(), GetFileIOSupport(), ReleaseFileIO()
 */
BOOL GetFileIOName(fileio, window)
struct FileIOSupport *fileio;
struct Window *window;
{
	UBYTE *newtext;

	if ((fileio == NULL) || (window == NULL)) return(FALSE);

	OpenSaveLock = CurrentDir(NULL);
	CurrentDir(OpenSaveLock);
	fileio->DOSLock = OpenSaveLock;

	/* Get easily-accessible copies of the values that are referenced 
	 * most often
	 */
	OpenReq = &OpenReqSupport.Requester;
	OpenReqWindow = OpenReqSupport.Window = window;
	OpenReqFileIO = fileio;

	/* Set up the DoRequest() handlers */
	OpenReqSupport.GadgetHandler = HandleGadget;
	OpenReqSupport.StartRequest = StartOpenRequester;
	OpenReqSupport.NewDiskHandler = DiskInserted;
	OpenReqSupport.MouseMoveHandler = PropMouseMoves;

	/* Init the string gadget buffers */
	OpenNameTextInfo.Buffer = &fileio->FileName[0];
	OpenDrawerTextInfo.Buffer = &fileio->DrawerName[0];
	OpenDiskTextInfo.Buffer = &fileio->DiskName[0];

	/* Initialize the requester title */
	if ((ReqTitleText.IText = fileio->ReqTitle) == NULL)
		ReqTitleText.IText = DefaultReqTitle;
	ReqTitleText.LeftEdge
		= (OPEN_WIDTH - IntuiTextLength(&ReqTitleText)) >> 1;

	/* If this fileio doesn't have valid filenames,
	 * then refresh the whole thing
	 */
	if (FlagIsClear(fileio->Flags, GOOD_FILENAMES))
		{
		ResetNameText(TRUE);
		ResetDrawerText(TRUE);
		BuildVolumeTable(fileio);
		CopyString(&OpenReqFileIO->DiskName[0], CurrentVolumeName());
		}

	ResetNameText(FALSE);
	ResetDrawerText(FALSE);
	ResetDiskText(FALSE);

	StuffSelectNames(0);
	InitOpenProp(TRUE);

	/* Reset the double-click time variables */
	OpenClickSeconds = OpenClickMicros = 0;


	/* And now, do that requester. */
	DoRequest(&OpenReqSupport);
	/* Back, eh?  Wasn't that easy? */


	if (FlagIsSet(fileio->Flags, LOCK_GOTTEN))
		{
		UnLock(fileio->DOSLock);
		ClearFlag(fileio->Flags, LOCK_GOTTEN);
		}

	CurrentDir(OpenSaveLock);
	OpenReqFileIO = NULL;

	/* Strip any excess leading and trailing blanks off the final name */
	newtext = StripOuterSpace(&fileio->FileName[0], " ");
	CopyString(&fileio->FileName[0], newtext);

	if ((OpenReqSupport.SelectedGadgetID)
			&& (OpenReqSupport.SelectedGadgetID != OPENGADGET_CANCEL))
		return(TRUE);
	return(FALSE);
}



/* *** BuildFileIOPathname() ************************************************
 * 
 * NAME
 *     BuildFileIOPathname  --  Build a file pathname using a FileIO struct
 * 
 * 
 * SYNOPSIS
 *     BuildFileIOPathname(FileIOSupport, Buffer);
 * 
 * 
 * FUNCTION
 *     Builds the text for a pathname using the FileName[], DrawerName[] and
 *     DiskName[] fields of the specified FileIOSupport structure
 *     after the support structure has been used in a successful call
 *     to GetFileIOName().  Writes the text into the Buffer.
 * 
 * 
 * INPUTS
 *     FileIOSupport = the address of a FileIOSupport structure
 *     Buffer = address of the buffer to receive the file pathname
 * 
 * 
 * RESULT
 *     None
 * 
 * 
 * SEE ALSO
 *     GetFileIOName()
 */
VOID BuildFileIOPathname(fileio, buffer)
struct FileIOSupport *fileio;
UBYTE *buffer;
{
	StripOuterSpace(&fileio->DiskName[0], " ");
	StripOuterSpace(&fileio->DrawerName[0], " ");
	StripOuterSpace(&fileio->FileName[0], " ");

	CopyString(buffer, &fileio->DiskName[0]);
	if (StringLength(&fileio->DrawerName[0]))
		{
		ConcatString(buffer, &fileio->DrawerName[0]);
		ConcatString(buffer, "/");
		}
	ConcatString(buffer, &fileio->FileName[0]);
}



/* *** AddFileIOName() ******************************************************
 * 
 * NAME
 *     AddFileIOName  --  Add a file name to the names in a FileIOSupport
 * 
 * 
 * SYNOPSIS
 *     AddFileIOName(FileIOSupport, FileName);
 * 
 * 
 * FUNCTION
 *     This routine adds a file name to the list of file names currently 
 *     in the specified FileIOSupport structure.  The next time the 
 *     FileIOSupport structure is used for a call to GetFileIOName(), the 
 *     new file name will apppear alphabetized in with the other file names.  
 *     
 *     This routine will most often be used after a call to GetFileIOName() 
 *     or some other routine where the user is allowed to specify the name 
 *     of a file to be opened for output.  If the file is opened 
 *     successfully, this routine will make sure that the name of the file 
 *     is in the FileIOSupport structure.  This is important if the output 
 *     file has been newly created; otherwise, without calling this 
 *     routine, the next time the FileIOSupport structure is used the new 
 *     file name would not appear even though the file exists.  If the name 
 *     is already in the list when you call AddFileIOName() then nothing 
 *     happens.  This allows you to call AddFileIOName() without worrying 
 *     about duplicate name redundancy.
 *     
 *     Here's a typical sequence of events leading up to a call to 
 *     AddFileIOName():
 *     
 *         First, get a FileIOSupport structure:
 *             fileio = GetFileIOSupport(...);
 *     
 *         When the user wants to write data, use GetFileIOName()
 *         to provide a convenient and consistent interface to
 *         the filesystem:
 *             goodfile = GetFileIOName(...);
 *     
 *         If the user has selected a name for output (in this example,
 *         goodfile will equal TRUE if the user selected a name), then
 *         open the file (possibly creating it) and then call 
 *         AddFileIOName() to make sure the name is in the FileIOSupport
 *         structure's list:
 *             if (goodfile)
 *                 {
 *                 UBYTE filename[80];
 *                 
 *                 BuildFileIOPathname(fileio, &filename[0]);
 *                 ... open filename, write it, close it ...
 *                 if (filename opened successfully)
 *                     AddFileIOName(fileio, &filename[0]);
 *                 }
 * 
 * 
 * INPUTS
 *     FileIOSupport = the address of a FileIOSupport structure
 *     FileName = the address of null-terminated text that is
 *         either a simple file name or a valid AmigaDOS pathname.
 * 
 * 
 * RESULT
 *     None
 * 
 * 
 * SEE ALSO
 *     GetFileIOName()
 *     GetFileIOSupport()
 */
VOID AddFileIOName(fileio, filename)
struct FileIOSupport *fileio;
UBYTE *filename;
{
	SHORT index, i, length;
	struct Remember *remember, *oldremember;
	UBYTE *nextentry;
	struct Remember *namekey;

	/* Does the filename start with a volume name?  If so, skip over it */
	index = IndexString(filename, ":");
	if (index >= 0) filename += index + 1;

	/* Does the filename start with a directory name?  If so, skip over it */
	do 
		{
		index = IndexString(filename, "/");
		if (index >= 0) filename += index + 1;
		}
	while (index >= 0);

	if (*filename == 0) return;

	/* Here, filename points to what is presumed to be a valid file name.
	 * If it's found among the FileIOSupport's names, exit.
	 * If it's not found in the list, add it.
	 *
	 * The current file names are stored in the fileio's Remember list.
	 */
	remember = fileio->NameKey;
	oldremember = NULL;
	while (remember)
		{
		i = CompareUpperStrings(filename, remember->Memory);
		if (i < 0) goto ADD_FILENAME;
		if (i == 0) return;
		oldremember = remember;
		remember = remember->NextRemember;
		}

ADD_FILENAME:
	/* Name not on list, so add it now */
	length = StringLength(filename);

	namekey = NULL;
	if (nextentry = AllocRemember(&namekey, length + 1 + 1, NULL))
		{
		CopyString(nextentry, filename);
		*(nextentry + length + 1) = 0;
		if (oldremember) oldremember->NextRemember = namekey;
		else fileio->NameKey = namekey;
		/* This assignment is valid whether or not remember is NULL */
		namekey->NextRemember = remember;
		fileio->NameCount++;
		}
}



/* *** ReleaseFileIO() ******************************************************
 * 
 * NAME
 *     ReleaseFileIO  --  Release the FileIO structure and all local memory
 * 
 * 
 * SYNOPSIS
 *     ReleaseFileIO(FileIO);
 * 
 * 
 * FUNCTION
 *     Releases the FileIO structure by freeing all local memory attached
 *     to the structure and then freeing the structure itself.
 * 
 * 
 * INPUTS
 *     FileIO = the address of a FileIO structure
 * 
 * 
 * RESULT
 *     None
 * 
 * 
 * SEE ALSO
 *     GetFileIOSupport()
 */
VOID ReleaseFileIO(fileio)
struct FileIOSupport *fileio;
{
	if (fileio)
		{
		FreeRemember(&fileio->VolumeKey, TRUE);
		FreeRemember(&fileio->NameKey, TRUE);
		FreeMem(fileio, sizeof(struct FileIOSupport));
		}
}



/* *** filesupp.c ***********************************************************
 *
 * File IO Suite  --  Open Requester Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 20 Oct 87    - RJ            Added RENAME_RAMDISK to fix what seems to 
 *                              be a bug in either AmigaDOS or Workbench.
 * 27 Sep 87    RJ              In order to show the correct DiskName during
 *                              WarmStartFileIO(), that routine now 
 *                              refreshes the gadgets both before and after 
 *                              the BuildNameTable() process.
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"


/* I think these are silly, but I put them here anyway to avoid compiler
 * warnings.
 */
VOID InitOpenProp();
VOID StuffSelectNames();



/* ======================================================================= */
/* === ResetText Routines ================================================ */
/* ======================================================================= */

VOID ResetTextGrunt(info, resetbuffer)
struct StringInfo *info;
BOOL resetbuffer;
/* Reset the string position variables, and reset the buffer itself if
 * resetbuffer is TRUE.
 */
{
	info->BufferPos = info->DispPos = 0;
	if (resetbuffer) info->Buffer[0] = '\0';
}

VOID ResetNameText(resetbuffer)
BOOL resetbuffer;
{
	ResetTextGrunt(&OpenNameTextInfo, resetbuffer);
}

VOID ResetDrawerText(resetbuffer)
BOOL resetbuffer;
{
	ResetTextGrunt(&OpenDrawerTextInfo, resetbuffer);
}

VOID ResetDiskText(resetbuffer)
BOOL resetbuffer;
{
	ResetTextGrunt(&OpenDiskTextInfo, resetbuffer);
}



/* ======================================================================= */
/* === WarmStart Initializer ============================================= */
/* ======================================================================= */

VOID WarmStartFileIO(fileio)
struct FileIOSupport *fileio;
/* This routine establishes a lock on the current disk and drawer,
 * resets all of the subsystem control variables,
 * gets the file names for the current disk and drawer,
 * initializes the proportional gadget,
 * and then refreshes the requester.
 */
{
	ULONG lock;

	/* If the fileio already had a lock, release it before proceeding */
	if (FlagIsSet(fileio->Flags, LOCK_GOTTEN))
		{
		UnLock(fileio->DOSLock);
		ClearFlag(fileio->Flags, LOCK_GOTTEN);
		fileio->DOSLock = OpenSaveLock;
		CurrentDir(OpenSaveLock);
		}

	/* Build the lock name using the current disk and drawer names */
	if (StringLength(&fileio->DiskName[0]))
		CopyString(&OpenLockName[0], &fileio->DiskName[0]);
	else
		CopyString(&OpenLockName[0], CurrentVolumeName());

	/* If the programmer wants us to sidestep the "RAM DISK:" bug, 
	 * then by all means oblige her or him.
	 */
	if (FlagIsSet(fileio->Flags, RENAME_RAMDISK) 
			&& CompareUpperStrings(&OpenLockName[0], 
			"RAM DISK:") == 0)
		{
		/* Everyone seems suspicious of "RAM Disk:" */
		OpenLockName[3] = ':';
		OpenLockName[4] = '\0';
		}

	ConcatString(&OpenLockName[0], &fileio->DrawerName[0]);

	/* Can we get a lock on this name? */
	if (lock = Lock(&OpenLockName[0], ACCESS_READ))
		{
		/* Got it! */
		SetFlag(fileio->Flags, LOCK_GOTTEN);
		fileio->DOSLock = lock;
		CurrentDir(lock);
		}
	else
		{
		/* Hey, bad break, this name just won't do.  But the rest of these
		 * routines need a valid directory, so go back home.
		 */
		Alert(ALERT_BAD_DIRECTORY, OpenReqWindow);
		CopyString(&fileio->DiskName[0], &CurrentDiskString[0]);
		ResetDiskText(FALSE);
		ResetDrawerText(TRUE);
		}

	/* Reset the fileio name selection variables */
	fileio->CurrentPick = -1;
	fileio->NameStart = 0;
	fileio->NameCount = 0;

	/* Reset the text and gadgets */
	InitOpenProp(TRUE);				/* Initialize the prop gadget */
	StuffSelectNames(-1);			/* Display all of the names */
	BuildNameTable(OpenReqFileIO);	/* Get the file names */
	StuffSelectNames(1);			/* Display the file names */
}



/* ======================================================================= */
/* === Select Name Routines ============================================== */
/* ======================================================================= */

VOID BlankSelectText(index)
SHORT index;
/* This routine truns the SelectText at index into blanks */
{
	UBYTE *ptr;
	SHORT blanklength;

	ptr = &OpenSelectBuffers[index][0];
	for (blanklength = VISIBLE_SELECT_LENGTH - 1; blanklength; blanklength--)
		*ptr++ = ' ';
	*ptr = '\0';

	OpenSelectText[index].FrontPen = 1;
	OpenSelectText[index].BackPen = 0;
}



VOID DrawSelectNames()
{
	struct Layer *layer;

	Forbid();
	if (layer = OpenReq->ReqLayer)
		if (layer->rp)
			PrintIText(layer->rp,
					&OpenSelectText[NAME_ENTRY_COUNT - 1], 
					OPENSELECT_LEFT, OPENSELECT_TOP);
	Permit();
}



VOID StuffSelectNames(refreshcount)
SHORT refreshcount;
/* This routine stuffs the Open Requester's filename gadgets with
 * names from the fileio structure, starting from the 
 * fileio->NameStart name.  If the refreshcount is nonzero, the gadgets
 * will be refreshed too.
 */
{
	SHORT i, end, bufferpos;
	SHORT length, blanklength;
	UBYTE *ptr, *ptr2;
	struct Remember *remember;
	struct Layer *layer;

	if (OpenReqFileIO->NameCount 
			> OpenReqFileIO->NameStart + NAME_ENTRY_COUNT) 
		end = OpenReqFileIO->NameStart + NAME_ENTRY_COUNT;
	else end = OpenReqFileIO->NameCount;

	bufferpos = 0;

	/* The current file names are stored in the fileio's Remember list */
	remember = OpenReqFileIO->NameKey;
	for (i = 0; i < OpenReqFileIO->NameStart; i++) 
		remember = remember->NextRemember;

	for (i = OpenReqFileIO->NameStart; i < end; i++) 
		{
		ptr = &OpenSelectBuffers[bufferpos][0];
		ptr2 = remember->Memory;

		length = StringLength(ptr2);
		if (length >= VISIBLE_SELECT_LENGTH)
			length = VISIBLE_SELECT_LENGTH - 1;
		blanklength = (VISIBLE_SELECT_LENGTH - 1) - length;

		/* By filling up the IntuiText with blanks after the characters,
		 * the text, when printed, will overstrike any characters that were 
		 * there before.
		 */
		for ( ; length; length--) *ptr++ = *ptr2++;
		for ( ; blanklength; blanklength--) *ptr++ = ' ';
		*ptr = '\0';

		/* If this is the selected text, then use "highlight" pens */
		if (i == OpenReqFileIO->CurrentPick)
			{
			OpenSelectText[bufferpos].FrontPen = -2;
			OpenSelectText[bufferpos].BackPen = -1;
			}
		else
			{
			OpenSelectText[bufferpos].FrontPen = 1;
			OpenSelectText[bufferpos].BackPen = 0;
			}

		bufferpos++;
		remember = remember->NextRemember;
		}

	/* Now, for all lines that have no entries, fill with blanks */
	for ( ; bufferpos < NAME_ENTRY_COUNT; bufferpos++)
		BlankSelectText(bufferpos);

	/* Finally, redraw the lot */
	if (refreshcount)
		{
		Forbid();
		if (layer = OpenReq->ReqLayer)
			if (layer->rp)
				RefreshGList(&OpenSelectNameGadget, OpenReqWindow, OpenReq,
						refreshcount);
		DrawSelectNames();
		Permit();
		}
}



VOID SetNameStart()
/* This little guy sets the NameStart based on the current
 * prop gadget setting.
 */
{
	if (OpenReqFileIO->NameCount <= NAME_ENTRY_COUNT)
		OpenReqFileIO->NameStart = 0;
	else
		OpenReqFileIO->NameStart = (OpenPropInfo.VertPot 
				* (OpenReqFileIO->NameCount - NAME_ENTRY_COUNT + 1)) >> 16;
}



VOID StripLastDrawer()
/* This guy strips the end drawer reference off of the drawer string,
 * which includes nulling out the string if there's only one to strip.
 */
{
	UBYTE *ptr;
	SHORT index;

	ptr = OpenDrawerTextInfo.Buffer;
	index = IndexString(ptr, "/");
	if (index != -1)
		{
		/* OK, there's more than one drawer reference, so get the ptr + index
		 * to the last one.
		 */
		do 
			ptr += (index + 1);
		while ((index = IndexString(ptr, "/")) != -1);
		}
	else index = 0;
	/* Zammo! */
	*(ptr + index) = '\0';
}



BOOL DirectoryName()
/* Returns TRUE if the selected name was a directory-type reference
 * (up or down), else returns FALSE for a normal filename.
 */
{
	struct Remember *nextentry;
	SHORT i;
	UBYTE entryflags;

	if (OpenReqFileIO->NameCount)
		{
		/* Find the selected entry in the key list */
		nextentry = OpenReqFileIO->NameKey;
		for (i = OpenReqFileIO->CurrentPick; i > 0; i--)
			nextentry = nextentry->NextRemember;
		i = StringLength(nextentry->Memory) + 1;
		entryflags = *(nextentry->Memory + i);

		if (FlagIsSet(entryflags, NAMED_DIRECTORY | NAMED_PREVIOUS))
			return(TRUE);

		/* else just a normal file name was selected, so fall out to ... */
		}
	return(FALSE);
}



VOID StuffFileName()
/* If the selected name is a normal filename, stuffs the filename into the
 * Name gadget.  If the selected name is a directory reference,
 * adjusts the drawer gadget accordingly.
 * Returns TRUE if the selected name was a directory-type reference
 * (up or down), else returns FALSE for a normal filename.
 */
{
	struct Remember *nextentry;
	SHORT i;
	UBYTE entryflags;

	if (OpenReqFileIO->NameCount)
		{
		/* Find the selected entry in the key list */
		nextentry = OpenReqFileIO->NameKey;
		for (i = OpenReqFileIO->CurrentPick; i; i--)
			nextentry = nextentry->NextRemember;
		i = StringLength(nextentry->Memory) + 1;
		entryflags = *(nextentry->Memory + i);

		if (FlagIsSet(entryflags, NAMED_DIRECTORY))
			{
			/* If there's already a drawer reference, build a proper
			 * extension before adding the new to the end.
			 */
			if (StringLength(OpenDrawerTextInfo.Buffer))
				ConcatString(OpenDrawerTextInfo.Buffer, "/");

	 		ConcatString(OpenDrawerTextInfo.Buffer,
					nextentry->Memory + DIR_TEXT_SIZE);

			ResetDrawerText(FALSE);
			ResetNameText(TRUE);
			}
		else if (FlagIsSet(entryflags, NAMED_PREVIOUS))
			{
			/* Remove the last drawer reference */
			StripLastDrawer();
			ResetDrawerText(FALSE);
			ResetNameText(TRUE);
			}
		else
			{
			/* Just a normal old file name was selected */
	 		CopyString(OpenNameTextInfo.Buffer, nextentry->Memory);
			ResetNameText(FALSE);
			}
		}
}



/* ======================================================================= */
/* === Proportional Gadget Routines ====================================== */
/* ======================================================================= */

VOID SetOpenPropPot(resetpos)
BOOL resetpos;
/* This routine resets the vertical pot of the proportional gadget
 * with respect to the current number of displayable file names.
 */
{
	LONG slack, result;

	slack = OpenReqFileIO->NameCount - NAME_ENTRY_COUNT;

	if (slack > 0)
		{
		result = ((LONG)OpenReqFileIO->NameStart << 16) / slack;
		if (result > 0xFFFF) result = 0xFFFF;
		OpenPropInfo.VertPot = result;
		}
	else
		OpenPropInfo.VertPot = 0;

	if (resetpos) OpenPropImage.TopEdge = 0;
}



VOID InitOpenProp(resetpos)
BOOL resetpos;
/* This routine initializes the variable imagery of the proportional
 * gadget and then initializes the gadget's vertical pot.
 * The BOOL arg resetpos describes whether you want the call to 
 * SetOpenPropPot() to reset the prop's knob position.
 */
{
	LONG namecount, height;
	SHORT i, i2;

	namecount = OpenReqFileIO->NameCount;

	if (namecount <= NAME_ENTRY_COUNT)
		{
		OpenPropInfo.VertBody = 0xFFFF;
		ClearFlag(OpenPropInfo.Flags, FREEVERT);
		height = OPENPROP_MAXHEIGHT;
		}
	else
		{
		OpenPropInfo.VertBody = ((LONG)NAME_ENTRY_COUNT << 16) / namecount;
		SetFlag(OpenPropInfo.Flags, FREEVERT);
		height = (OPENPROP_MAXHEIGHT * NAME_ENTRY_COUNT)  / namecount;
		if (height < OPENPROP_MINHEIGHT) height = OPENPROP_MINHEIGHT;
		}

	OpenPropImage.Height = height;
	for (i = 0; i < OPENPROP_TOPHEIGHT; i++)
		{
		OpenPropData[i] = OpenPropTop[i];
		OpenPropData[i + height] = OpenPropTop[i + OPENPROP_TOPHEIGHT];
		}

	for (i = OPENPROP_TOPHEIGHT; i < height - OPENPROP_BOTTOMHEIGHT; i++)
		{
		OpenPropData[i] = OpenPropBottom[0];
		OpenPropData[i + height] = OpenPropBottom[OPENPROP_BOTTOMHEIGHT];
		}

	i2 = 0;
	for (i = height - OPENPROP_BOTTOMHEIGHT; i < height; i++)
		{
		OpenPropData[i] = OpenPropBottom[i2];
		OpenPropData[i + height] = OpenPropBottom[i2 + OPENPROP_BOTTOMHEIGHT];
		i2++;
		}

	SetOpenPropPot(resetpos);
}




/* ======================================================================= */
/* === Requester Handler Routines ======================================== */
/* ======================================================================= */

VOID StartOpenRequester()
/* Called after the requester has been opened. */
{
	ActivateGadget(&OpenNameTextGadget, OpenReqWindow, OpenReq);

	if (FlagIsClear(OpenReqFileIO->Flags, GOOD_FILENAMES))
		WarmStartFileIO(OpenReqFileIO);
	else DrawSelectNames();
}



SHORT HandleSelect(y, seconds, micros)
SHORT y;
LONG seconds, micros;
/* This routine accepts that a GADGETDOWN occured at the given 
 * pointer y offset.  This is translated into the ordinal number of the 
 * filename selected by the user, and this is assigned to the 
 * CurrentPick variable of the OpenReqFileIO structure.
 * Returns:
 *     1 = DirectoryName() returned TRUE (selection was directory name)
 *     0 = new name selected, DirectoryName() returned FALSE (normal name)
 *    -1 = same name selected, double-clicked
 *    -2 = same name selected, not double-clicked
 */
{
	SHORT returnvalue, oldy;
	LONG oldseconds, oldmicros;

	y -= (OpenReq->TopEdge + OPENSELECT_TOP);
	y = y / OPEN_LINEHEIGHT;
	y += OpenReqFileIO->NameStart;
	if (y >= OpenReqFileIO->NameCount)
		y = OpenReqFileIO->NameCount - 1;

	oldseconds = OpenClickSeconds;
	oldmicros = OpenClickMicros;
	OpenClickSeconds = seconds;
	OpenClickMicros = micros;

	oldy = OpenReqFileIO->CurrentPick;
	OpenReqFileIO->CurrentPick = y;

	if (DirectoryName())
		{
		returnvalue = 1;
		}
	else
		{
		if (y == oldy)
			{
			/* User has selected the same name again.
			 * Was it done quickly enough to count as a
			 * double-click selection?
			 */
			if (FlagIsClear(OpenReqFileIO->Flags, DOUBLECLICK_OFF)
					&& DoubleClick(oldseconds, oldmicros,
							OpenClickSeconds, OpenClickMicros))
				returnvalue = -1;
			else returnvalue = -2;
			}
		else
			{
			/* Do this work, what there is of it, only if the user 
			 * hasn't reselected an already-selected name.
			 */
			returnvalue = 0;
			}
		}

	return(returnvalue);
}



LONG HandleGadget(gadget, x, y, seconds, micros)
struct Gadget *gadget;
SHORT x, y;
LONG seconds, micros;
/* This routine handles one gadget selection */
{
	BOOL softbuild, hardbuild;
	SHORT count;
	LONG returnvalue;

	/* softbuild causes the file names to be refreshed.
	 * hardbuild causes the entire requester to be reestablished.
	 * either or both can be set.
	 */
	softbuild = hardbuild = FALSE;

	/* count refers to the count that will be sent to StuffSelectNames().
	 * The default is 2, as you can see.
	 */
   count = 2;

	/* If the selection of any gadget causes us to want the requester to
	 * go away, set the returnvalue non-zero.
	 */
	returnvalue = 0;

	switch (gadget->GadgetID)
		{
		case OPENGADGET_SELECTNAME:
			if (OpenReqFileIO->NameCount)
				{
				/* So our big name gadget was selected, eh?  Well, which one 
				 * was the user really pointing at?
				 */
				y = HandleSelect(y, seconds, micros);
				if (y < 0)
					{
					/* The user has reselected the old name */
					if (y == -1) returnvalue = -1;
					}
				else
					{
					/* The user has selected a new name */
					StuffFileName();
					softbuild = TRUE;
					if (y == 1) hardbuild = TRUE;
					count = 5;
					}
				}
			break;
		case OPENGADGET_UPGADGET:
			if (OpenReqFileIO->NameStart)
				{
				OpenReqFileIO->NameStart--;
				softbuild = TRUE;
				}
			break;
		case OPENGADGET_DOWNGADGET:
			if (OpenReqFileIO->NameStart + NAME_ENTRY_COUNT
					< OpenReqFileIO->NameCount)
				{
				OpenReqFileIO->NameStart++;
				softbuild = TRUE;
				}
			break;
		case OPENGADGET_PROPGADGET:
			if (OpenReqFileIO->NameCount > NAME_ENTRY_COUNT)
				{
				SetNameStart();
				softbuild = TRUE;
				}
			break;
		case OPENGADGET_NEXTDISK:
			/* Next disk!  Wholly mackerel!  First, if no "next" then split */
			if (OpenReqFileIO->VolumeCount <= 1) break;

			OpenReqFileIO->VolumeIndex++;
			if (OpenReqFileIO->VolumeIndex >= OpenReqFileIO->VolumeCount)
				OpenReqFileIO->VolumeIndex = 0;
			CopyString(&OpenReqFileIO->DiskName[0], CurrentVolumeName());
			ResetDiskText(FALSE);
			ResetDrawerText(TRUE);

			/* Refresh the display so the user can see what's been done, as
			 * well as having the display refreshed all over again (hardbuild)
			 * below after the new filenames are retrieved.
			 */
			softbuild = TRUE;
			count = 5;

			/* Intentionally fall into DRAWER/DISKTEXT */

		case OPENGADGET_DRAWERTEXT:
		case OPENGADGET_DISKTEXT:
			ResetNameText(TRUE);
			hardbuild = TRUE;
			break;
		default:
			break;
		}

	/* These are split intentionally, because setters of hardbuild might
	 * want a softbuild done first, to show why a hardbuild is being done!
	 */
	if (hardbuild) SetWaitPointer(OpenReqWindow);
	if (softbuild)
		{
		SetOpenPropPot();
		StuffSelectNames(count);
		}
	if (hardbuild)
		WarmStartFileIO(OpenReqFileIO);	/* Restart the lock etc. */

	return(returnvalue);
}



VOID DiskInserted()
/* This routine is called by the RequesterSupport code whenever a new disk
 * has been inserted.  This allows the user to swap disks while the
 * requester is displayed.  Unfortunately, I have no way of knowing which
 * disk swapped, so I have to restart the requester more or less.
 * Not too bad, but a little unpleasant.
 */
{
	SetWaitPointer(OpenReqWindow);
	SetFlag(OpenReqFileIO->Flags, DISK_HAS_CHANGED);
	BuildVolumeTable(OpenReqFileIO);
	CopyString(&OpenReqFileIO->DiskName[0], CurrentVolumeName());
	WarmStartFileIO(OpenReqFileIO);
}



VOID PropMouseMoves()
/* This routine is called by RequesterSupport whenever the mouse moves
 * while a FOLLOWMOUSE gadget is set.  The only FOLLOWMOUSE gadget is
 * the prop gadget, so...
 */
{
	/* ... if there's more names than the number of visible names,
	 * then reset the name start and redisplay the names.
	 */
	if (OpenReqFileIO->NameCount > NAME_ENTRY_COUNT)
		{
		SetNameStart();
		StuffSelectNames(2);
		}
}


/* *** opendata.c ***********************************************************
 *
 * File IO Suite  --  Open Requester Data
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 18 Aug 87    RJ              Removed SELECTED from most string gadgets
 * 18 Aug 87    RJ              Added KeyHandler field to ReqSupport
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"




struct TextAttr SafeFont =
	{
	(UBYTE *)"topaz.font",
	TOPAZ_EIGHTY,
	0,
	0,
	};



/* This is where the Select text is kept!  This array of text will be
 * redrawn every time we want Intuition to redraw the lowly SelectGadget.
 * The strings themselves are kept in OpenSelectBuffers where 
 * the open routines make sure that the buffers are padded with
 * blanks and are always null-terminated.  Ha ha!  Like madmen
 * dancing, like crazy monkeys.
 */
UBYTE OpenSelectBuffers[NAME_ENTRY_COUNT][VISIBLE_SELECT_LENGTH];
struct IntuiText OpenSelectText[NAME_ENTRY_COUNT] =
	{
		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 0),
		&SafeFont,
		&OpenSelectBuffers[0][0],
		NULL,
		},

		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 1),
		&SafeFont,
		&OpenSelectBuffers[1][0],
		&OpenSelectText[0],
		},

		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 2),
		&SafeFont,
		&OpenSelectBuffers[2][0],
		&OpenSelectText[1],
		},

		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 3),
		&SafeFont,
		&OpenSelectBuffers[3][0],
		&OpenSelectText[2],
		},

		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 4),
		&SafeFont,
		&OpenSelectBuffers[4][0],
		&OpenSelectText[3],
		},

		{
		1, 0, 
		JAM2,
		1, 1 + (OPEN_LINEHEIGHT * 5),
		&SafeFont,
		&OpenSelectBuffers[5][0],
		&OpenSelectText[4],
		},

	};




/* === Backdrop Gadget =================================================== */
/* We shouldn't need this gadget.  It's sole purpose is to allow the user 
 * to cancel the filename building operation by clicking outside any of 
 * the regular requester gadgets.  Why, we may ask, don't we just make 
 * the requester a NOISYREQ and detect SELECTDOWN.  Well, we tried that, 
 * and either there's something wrong with our brains or 1.2 Intuition has 
 * a bug that doesn't broadcast SELECTDOWN to requester listeners.  
 * It broadcasts SELECTUP, MENUDOWN and MENUUP jes' fine, but no DOWN.  
 * Bummer.  So anyway, now we got an imageless, highlightless gadget that 
 * fills up the entire requester but, being at the back of the list, won't 
 * be hit unless all other gadgets are missed.  
 * Having a wonderful time, wish you were here.
 */
struct Gadget BackdropGadget =
	{
	NULL,
	0, 0, 
	OPEN_WIDTH, OPEN_HEIGHT, 
	GADGHNONE,				/* Flags */
	GADGIMMEDIATE,			/* Activation */
	REQGADGET | BOOLGADGET,	/* Type */
	NULL, 					/* GadgetRender */
	NULL,					/* SelectRender */
	NULL,
	NULL, NULL, 
	OPENGADGET_BACKDROP, 
	NULL,
	};



/* === OK, Cancel, and NextDisk Gadgets ================================== */
SHORT BoolCluster3Pairs[] =
	{
	0, -3, 
	55, -3,
	57, -1,
	57, 19,
	55, 21,
	-1, 21,
	-3, 19,
	-3, -1,
	-1, -3,
	};

SHORT BoolCluster2Pairs[] =
	{
	-2, -1,
	56, -1,
	56, 19,
	-2, 19,
	-2, -1,
	};

SHORT BoolClusterPairs[] =
	{
	-1, -2,
	55, -2,
	55, 20,
	-1, 20,
	-1, -2,
	};

struct Border BoolCluster3Border =
	{
	1, 1,
	2, 0,
	JAM1,
	9,
	&BoolCluster3Pairs[0],
	NULL,
	};

struct Border BoolCluster2Border =
	{
	1, 1,
	1, 0,
	JAM1,
	5,
	&BoolCluster2Pairs[0],
	&BoolCluster3Border,
	};

struct Border BoolClusterBorder =
	{
	1, 1,
	1, 0,
	JAM1,
	5,
	&BoolClusterPairs[0],
	&BoolCluster2Border,
	};

struct IntuiText NextDisk2Text =
	{
	1, 0, 
	JAM2,
	13, 11,
	&SafeFont,
	(UBYTE *)"DISK",
	NULL,
	};

struct IntuiText NextDiskText =
	{
	1, 0, 
	JAM2,
	13, 3,
	&SafeFont,
	(UBYTE *)"NEXT",
	&NextDisk2Text,
	};

struct IntuiText CancelText =
	{
	1, 0, 
	JAM2,
	5, 7,
	&SafeFont,
	(UBYTE *)"CANCEL",
	NULL,
	};

struct IntuiText OK2Text =
	{
	1, 0, 
	JAM2,
	17, 7,
	&SafeFont,
	(UBYTE *)"OK!",
	NULL,
	};


struct Gadget NextDiskGadget =
	{
	&BackdropGadget,
	197, 82 + REQTITLE_HEIGHT,
	57, 21,
	GADGHCOMP,	/* Flags */
	RELVERIFY,		/* Activation */
	REQGADGET | BOOLGADGET,		/* Type */
	(APTR)&BoolClusterBorder, 			/* GadgetRender */
	NULL,				/* SelectRender */
	&NextDiskText,
	NULL, NULL, 
	OPENGADGET_NEXTDISK, 
	NULL,
	};

struct Gadget CancelGadget =
	{
	&NextDiskGadget,
	114, 82 + REQTITLE_HEIGHT,
	57, 21,
	GADGHCOMP,	/* Flags */
	RELVERIFY | ENDGADGET,		/* Activation */
	REQGADGET | BOOLGADGET,		/* Type */
	(APTR)&BoolClusterBorder, 			/* GadgetRender */
	NULL,				/* SelectRender */
	&CancelText,
	NULL, NULL, 
	OPENGADGET_CANCEL,
	NULL,
	};

struct Gadget OKGadget =
	{
	&CancelGadget,
	32, 82 + REQTITLE_HEIGHT,
	57, 21,
	GADGHCOMP,		/* Flags */
	RELVERIFY | ENDGADGET,		/* Activation */
	REQGADGET | BOOLGADGET,		/* Type */
	(APTR)&BoolClusterBorder, 
	NULL,				/* Renders */
	&OK2Text,
	NULL, NULL, 
	OPENGADGET_OK, 
	NULL,
	};



/* === OpenUp ============================================================= */

struct Image OpenUpImage =
	{
	1, -1,
	15, 14,
	2,
	&OpenUpData[0],
	0x03, 0x00,
	NULL,
	};

struct Gadget OpenUpGadget =
	{
	&OKGadget,
	134, 14 + REQTITLE_HEIGHT,
	15, 12,
	GADGIMAGE | GADGHNONE,			  /* Flags */
	GADGIMMEDIATE,					  /* Activation */
	REQGADGET | BOOLGADGET,			 /* Type */
	(APTR)&OpenUpImage, 
	NULL,							   /* Renders */
	NULL,
	NULL, NULL, 
	OPENGADGET_UPGADGET, 
	NULL,
	};



/* === OpenDown =========================================================== */

struct Image OpenDownImage =
	{
	1, -1,
	15, 14,
	2,
	&OpenDownData[0],
	0x03, 0x00,
	NULL,
	};

struct Gadget OpenDownGadget =
	{
	&OpenUpGadget,
	134, 64 + REQTITLE_HEIGHT,
	15, 12,
	GADGIMAGE | GADGHNONE,			  /* Flags */
	GADGIMMEDIATE,						  /* Activation */
	REQGADGET | BOOLGADGET,			 /* Type */
	(APTR)&OpenDownImage, 
	NULL,							   /* Renders */
	NULL,
	NULL, NULL, 
	OPENGADGET_DOWNGADGET, 
	NULL,
	};


/* === OpenDrawerText ==================================================== */

struct StringInfo OpenDrawerTextInfo =
	{
	NULL, /* Must be supplied from the appropriate FileIOSupport structure */
	&OpenUndoBuffer[0],
	0,
	MAX_NAME_LENGTH,
	0,
	0,0,0,0,0,0,0,0,
	};

struct Gadget OpenDrawerTextGadget =
	{
	&OpenDownGadget,
	157, 41 + REQTITLE_HEIGHT,
	122, 10,
	GADGHCOMP,			   /* Flags */
	RELVERIFY | STRINGCENTER,			   /* Activation */
	REQGADGET | STRGADGET,			  /* Type */
	NULL, NULL,						 /* Renders */
	NULL,
	NULL,
	(APTR)&OpenDrawerTextInfo,
	OPENGADGET_DRAWERTEXT,
	NULL,
	};


/* === OpenDiskText ====================================================== */

struct StringInfo OpenDiskTextInfo =
	{
	NULL, /* Must be supplied from the appropriate FileIOSupport structure */
	&OpenUndoBuffer[0],
	0,
	MAX_NAME_LENGTH,
	0,
	0,0,0,0,0,0,0,0,
	};

struct Gadget OpenDiskTextGadget =
	{
	&OpenDrawerTextGadget,
	157, 65 + REQTITLE_HEIGHT,
	122, 10,
	GADGHCOMP,			   /* Flags */
	RELVERIFY | STRINGCENTER,			   /* Activation */
	REQGADGET | STRGADGET,			  /* Type */
	NULL, NULL,						 /* Renders */
	NULL,
	NULL,
	(APTR)&OpenDiskTextInfo,
	OPENGADGET_DISKTEXT,
	NULL,
	};



/* === OpenNameText ====================================================== */

struct StringInfo OpenNameTextInfo =
	{
	NULL, /* Must be supplied from the appropriate FileIOSupport structure */
	&OpenUndoBuffer[0],
	0,
	MAX_NAME_LENGTH,
	0,
	0,0,0,0,0,0,0,0,
	};

struct Gadget OpenNameTextGadget =
	{
	&OpenDiskTextGadget,
	157, 16 + REQTITLE_HEIGHT,
	122, 10,
	GADGHCOMP | SELECTED,			   /* Flags */
	RELVERIFY | ENDGADGET | STRINGCENTER,			   /* Activation */
	REQGADGET | STRGADGET,			  /* Type */
	NULL, NULL,						 /* Renders */
	NULL,
	NULL,
	(APTR)&OpenNameTextInfo,
	OPENGADGET_NAMETEXT,
	NULL,
	};


/* === OpenProp =========================================================== */
/* OpenPropData[] is in chipdata.c */

struct Image OpenPropImage =
	{
	1, 0,
	11, 8,
	2,
	&OpenPropData[0],
	0x03, 0x00,
	NULL,
	};

struct PropInfo OpenPropInfo =
	{
	FREEVERT | PROPBORDERLESS,
	0, 0,  /* Pots should be reinitialized on the fly */
	0, 0,  /* Bodies should be reinitialized on the fly */
	0, 0, 0, 0, 0, 0,
	};

struct Gadget OpenPropGadget =
	{
	&OpenNameTextGadget,
	136, 30 + REQTITLE_HEIGHT,	/* Left, Top */
	13, 30,		/* Width, Height */
	GADGIMAGE | GADGHNONE,		 /* Flags */
	GADGIMMEDIATE | RELVERIFY | FOLLOWMOUSE,  /* Activation */
	REQGADGET | PROPGADGET,				/* Type */
	(APTR)&OpenPropImage, 
	NULL,						  /* Renders */
	NULL,
	NULL, 
	(APTR)&OpenPropInfo, 
	OPENGADGET_PROPGADGET, 
	NULL,
	};





/* === OpenSelectName ==================================================== */

struct Gadget OpenSelectNameGadget =
	{
	&OpenPropGadget,
	OPENSELECT_LEFT, OPENSELECT_TOP,		/* Left, Top */
	OPENSELECT_WIDTH, OPENSELECT_HEIGHT,	/* Width, Height */
	GADGHNONE,								/* Flags */
	GADGIMMEDIATE,							/* Activation */
	REQGADGET | BOOLGADGET,					/* Type */
	NULL, NULL,								/* Renders */
	NULL,									/* Text */
	NULL, NULL, 
	OPENGADGET_SELECTNAME,
	NULL,
	};



/* === Requester Details ================================================= */
/* This data is used to render the requester more prettily. */

/* === Line Pairs === */
SHORT Req3BorderData[] =
	{
	  1, 0,
	OPEN_WIDTH - 2, 0,
	OPEN_WIDTH - 2, 1,
	OPEN_WIDTH - 1, 1,
	OPEN_WIDTH - 1, 108 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2, 108 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2, 109 + REQTITLE_HEIGHT,
	  1, 109 + REQTITLE_HEIGHT,
	  1, 108 + REQTITLE_HEIGHT,
	  0, 108 + REQTITLE_HEIGHT,
	  0, 1,
	  1, 1,
	  1, 0,
	};

SHORT Req2BorderData[] =
	{
	  1 + 1, 0 + 1,
	OPEN_WIDTH - 2 - 1, 0 + 1,
	OPEN_WIDTH - 2 - 1, 1 + 1,
	OPEN_WIDTH - 1 - 1, 1 + 1,
	OPEN_WIDTH - 1 - 1, 108 - 1 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2 - 1, 108 - 1 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2 - 1, 109 - 1 + REQTITLE_HEIGHT,
	  1 + 1, 109 - 1 + REQTITLE_HEIGHT,
	  1 + 1, 108 - 1 + REQTITLE_HEIGHT,
	  0 + 1, 108 - 1 + REQTITLE_HEIGHT,
	  0 + 1, 1 + 1,
	  1 + 1, 1 + 1,
	  1 + 1, 0 + 1,
	};

SHORT ReqBorderData[] =
	{
	  1 + 2, 0 + 2,
	OPEN_WIDTH - 2 - 2, 0 + 2,
	OPEN_WIDTH - 2 - 2, 1 + 2,
	OPEN_WIDTH - 1 - 2, 1 + 2,
	OPEN_WIDTH - 1 - 2, 108 - 2 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2 - 2, 108 - 2 + REQTITLE_HEIGHT,
	OPEN_WIDTH - 2 - 2, 109 - 2 + REQTITLE_HEIGHT,
	  1 + 2, 109 - 2 + REQTITLE_HEIGHT,
	  1 + 2, 108 - 2 + REQTITLE_HEIGHT,
	  0 + 2, 108 - 2 + REQTITLE_HEIGHT,
	  0 + 2, 1 + 2,
	  1 + 2, 1 + 2,
	  1 + 2, 0 + 2,
	};

SHORT SelectBorderData[] =
	{
	8,   14 + REQTITLE_HEIGHT,
	129, 14 + REQTITLE_HEIGHT,
	130, 15 + REQTITLE_HEIGHT,
	130, 74 + REQTITLE_HEIGHT,
	129, 75 + REQTITLE_HEIGHT,
	8,   75 + REQTITLE_HEIGHT,
	7,   74 + REQTITLE_HEIGHT,
	7,   15 + REQTITLE_HEIGHT,
	8,   14 + REQTITLE_HEIGHT,
	};

SHORT TextBorderData[] =
	{
	-1,  -2,
	120, -2,
	121, -1,
	121, 8,
	120, 9,
	-1,  9,
	-2,  8,
	-2,  -1,
	-1,  -2,
	};

SHORT PropBorderData[] =
	{
	0,  -3,
	16, -3,
	16, 32,
	0,  32,
	0,  -2,
	15, -2,
	15, -1,
	17, -1,
	17, 30,
	15, 30,
	15, 31,
	1,  31,
	1,  30,
	-1, 30,
	-1, -1,
	-1, 1,
	};


/* === Borders === */
struct Border Req3Border =
	{
	0, 0,
	1, 0,
	JAM1,
	13,
	&Req3BorderData[0],
	NULL,
	};

struct Border Req2Border =
	{
	0, 0,
	2, 0,
	JAM1,
	13,
	&Req2BorderData[0],
	&Req3Border,
	};

struct Border ReqBorder =
	{
	0, 0,
	1, 0,
	JAM1,
	13,
	&ReqBorderData[0],
	&Req2Border,
	};

struct Border SelectBorder =
	{
	0, 0,
	1, 0,
	JAM1,
	9,
	&SelectBorderData[0],
	&ReqBorder,
	};

struct Border Text3Border =
	{
	157, 65 + REQTITLE_HEIGHT,
	1, 0,
	JAM1,
	9,
	&TextBorderData[0],
	&SelectBorder,
	};

struct Border Text2Border =
	{
	157, 41 + REQTITLE_HEIGHT,
	1, 0,
	JAM1,
	9,
	&TextBorderData[0],
	&Text3Border,
	};

struct Border TextBorder =
	{
	157, 16 + REQTITLE_HEIGHT,
	1, 0,
	JAM1,
	9,
	&TextBorderData[0],
	&Text2Border,
	};

struct Border PropBorder =
	{
	134, 30 + REQTITLE_HEIGHT,
	1, 0,
	JAM1,
	16,
	&PropBorderData[0],
	&TextBorder,
	};

/* === RJ wants to know:  Some Requester Text For You? === */
UBYTE *DefaultReqTitle = (UBYTE *)"File IO Requester";
struct IntuiText ReqTitleText =
	{
	1, 0, 
	JAM2,
	0, 5,		/* The x-coordinate is initialized to center the title */
	&SafeFont,
	NULL,		/* Points to the title supplied in the FileIOSupport */
	NULL,
	};

struct IntuiText SelectText =
	{
	1, 0, 
	JAM2,
	18, 6 + REQTITLE_HEIGHT,
	&SafeFont,
	(UBYTE *)"Select a Name",
	&ReqTitleText,
	};

struct IntuiText Type3Text =
	{
	1, 0, 
	JAM2,
	201, 55 + REQTITLE_HEIGHT,
	&SafeFont,
	(UBYTE *)"Disk",
	&SelectText,
	};

struct IntuiText Type2Text =
	{
	1, 0, 
	JAM2,
	193, 31 + REQTITLE_HEIGHT,
	&SafeFont,
	(UBYTE *)"Drawer",
	&Type3Text,
	};

struct IntuiText TypeText =
	{
	1, 0, 
	JAM2,
	161, 6 + REQTITLE_HEIGHT,
	&SafeFont,
	(UBYTE *)"Or Type a Name",
	&Type2Text,
	};



/* === Main Data ======================================================== */
struct ReqSupport OpenReqSupport = 
	{

	/* struct Requester Requester; */
		{
		/*	struct Requester *OlderRequest;*/
		NULL,

		/*	SHORT LeftEdge, TopEdge;*/
		OPEN_LEFT, OPEN_TOP,

		/*	SHORT Width, Height;*/
		OPEN_WIDTH, OPEN_HEIGHT, 

		/*	SHORT RelLeft, RelTop;*/
		0, 0,

		/*	struct Gadget *ReqGadget;*/
		&OpenSelectNameGadget,
	
		/*	struct Border *ReqBorder;*/
		&PropBorder,
	
		/*	struct IntuiText *ReqText;*/
		&TypeText,
	
		/*	USHORT Flags;*/
		NOISYREQ,

		/*	UBYTE BackFill;*/
		0,

		/*	struct ClipRect ReqCRect;*/
		{ NULL },

		/*	struct BitMap *ImageBMap;*/
		NULL,

		/*	struct BitMap ReqBMap;*/
		{ NULL },

		},	/* end of Requester structure */

	/* struct Window *Window; */
	NULL,

	/* LONG (*StartRequest)(); */
	NULL,

	/* LONG (*ReqHandler)(); */
	NULL,

	/* LONG (*NewDiskHandler)(); */
	NULL,

	/* LONG (*KeyHandler)(); */
	NULL,

	/* LONG (*MouseMoveHandler)(); */
	NULL,

	/* SHORT SelectedGadgetID; */
	0,
};

struct Requester *OpenReq = NULL;
struct Window *OpenReqWindow = NULL;
struct FileIOSupport *OpenReqFileIO = NULL;
UBYTE OpenUndoBuffer[MAX_NAME_LENGTH];
UBYTE OpenLockName[128] = {0};

ULONG OpenSaveLock = NULL;

UBYTE CurrentDiskString[] = ":";

LONG OpenClickSeconds;
LONG OpenClickMicros;


/* *** chipdata.c ***********************************************************
 *
 * File IO Suite  --  Chip Memory Data Declarations
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
/* This prevents eglobfio.c from being included */
#define EGLOBAL_FILEIO_CANCEL
#include "fileio.h"



/* === WaitPointer Imagery =============================================== */
USHORT ElecArtsWaitPointer[(ELECARTSPOINT_HEIGHT * 2) + 4] =
	{
	0x0000, 0x0000,

	0x6700, 0xC000,
	0xCFA0, 0xC700,
	0xBFF0, 0x0FA0,
	0x70F8, 0x3FF0,
	0x7DFC, 0x3FF8,
	0xFBFC, 0x7FF8,
	0x70FC, 0x3FF8,
	0x7FFE, 0x3FFC,
	0x7F0E, 0x3FFC,
	0x3FDF, 0x1FFE,
	0x7FBE, 0x3FFC,
	0x3F0E, 0x1FFC,
	0x1FFC, 0x07F8,
	0x07F8, 0x01E0,
	0x01E0, 0x0080,
	0x07C0, 0x0340,
	0x0FE0, 0x07C0,
	0x0740, 0x0200,
	0x0000, 0x0000,
	0x0070, 0x0020,
	0x0078, 0x0038,
	0x0038, 0x0010,

	0x0000, 0x0000,
	};



/* === Open Requester Imagery ============================================ */
USHORT OpenPropData[OPENPROP_MAXHEIGHT * 2];

USHORT OpenPropTop[OPENPROP_TOPHEIGHT * 2] =
	{
	/* plane 0 */
	0x7FC0,
	0xE0E0,

	/* plane 1 */
	0x0000,
	0x1F00,
	};


USHORT OpenPropBottom[OPENPROP_BOTTOMHEIGHT * 2] =
	{
	/* plane 0 */
	0xC060,
	0xC060,
	0xC060,
	0xC060,
	0xE0E0,
	0x7FC0,

	/* plane 1 */
	0x3F80,
	0x3F80,
	0x3F80,
	0x3F80,
	0x1F00,
	0x0000,
	};


USHORT OpenUpData[] =
	{
	/* plane 0 */
	0x0100,
	0x739C,
	0x87C2,
	0x8FE2,
	0x9FF2,
	0xBFFA,
	0x8382,
	0x8382,
	0x8382,
	0x8382,
	0x8382,
	0x7BBC,
	0x0380,
	0x0380,

	/* plane 1 */
	0x0000,
	0x0000,
	0x600C,
	0x4004,
	0x0000,
	0x0000,
	0x0000,
	0x783C,
	0x783C,
	0x783C,
	0x783C,
	0x0000,
	0x0000,
	0x0000,
	};

USHORT OpenDownData[] =
	{
	/* plane 0 */
	0x0380,
	0x0380,
	0x7BBC,
	0x8382,
	0x8382,
	0x8382,
	0x8382,
	0x8382,
	0xBFFA,
	0x9FF2,
	0x8FE2,
	0x87C2,
	0x739C,
	0x0100,

	/* plane 1 */
	0x0000,
	0x0000,
	0x0000,
	0x783C,
	0x783C,
	0x783C,
	0x783C,
	0x0000,
	0x0000,
	0x0000,
	0x4004,
	0x600C,
	0x0000,
	0x0000,
	};


/* *** volname.c ************************************************************
 *
 * File IO Suite  --  Volume Name Construction Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 27 Sep 87    RJ              Leave VolumeIndex alone as much as possible.
 *                              Also, after building a new volume table, try 
 *                              to look up the old volume name among the 
 *                              new volume entries and set VolumeIndex 
 *                              if possible.
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"
#include <libraries/dosextens.h>


/* Lattice doesn't accept the ID_DOS_DISK which looks like this:
 * #define ID_DOS_DISK ('DOS\0')
 * so I make up my own.
 */
#define X_ID_DOS_DISK (((LONG)'D'<<24)|((LONG)'O'<<16)|('S'<<8)|('\0'))


BOOL DOSDisk(device)
struct DeviceList *device;
/* This little routine dips its gnarled toes directly into the 
 * scary murky depths of AmigaDOS interior.
 * If it survives, it returns TRUE or FALSE depending on whether or
 * the specified device is a DOS disk.
 * Jeez, Look at the structure names in this routine!  Whew!  You almost
 * gotta be a computer scientist to understand this stuff.
 */
{
	struct MsgPort *port;
	BOOL result;
	struct InfoData *info;
	struct StandardPacket *packet;


	result = FALSE;

	/* Allocate the data structures required to communicate with AmigaDOS */
	info = (struct InfoData *)AllocMem(sizeof(struct InfoData), MEMF_CLEAR);
	packet = (struct StandardPacket *)AllocMem(sizeof(struct StandardPacket),
			MEMF_CLEAR);

	/* Grab us a message port for the reply from DOS */
	port = CreatePort(NULL, 0);

	if (port && info && packet)
		{
		/* OK, everything is set so here we go! */

		/* Set up the StandardPacket's Exec message */
		packet->sp_Msg.mn_Node.ln_Type = NT_MESSAGE;
		packet->sp_Msg.mn_Node.ln_Name = (char *)&packet->sp_Pkt;
		packet->sp_Msg.mn_ReplyPort = port;

		/* Set up the StandardPacket's DOS data */
		packet->sp_Pkt.dp_Link = &packet->sp_Msg;
		packet->sp_Pkt.dp_Type = ACTION_DISK_INFO;
		packet->sp_Pkt.dp_Arg1 = ((LONG)info >> 2);
		packet->sp_Pkt.dp_Port = port;

		/* Now, ask the device whether or not it's a DOS disk */
		PutMsg(device->dl_Task, &packet->sp_Msg);
		/* zzz */
		WaitPort(port);

		/* Well? */
		if (info->id_DiskType == X_ID_DOS_DISK) result = TRUE;
		}

	if (port) DeletePort(port);
	if (info) FreeMem(info, sizeof(struct InfoData));
	if (packet) FreeMem(packet, sizeof(struct StandardPacket));

	return(result);
}



VOID BuildVolumeTable(fileio)
struct FileIOSupport *fileio;
/* This routine builds an alphabetically-sorted list of all active volumes.
 * The names can be either the device names or the volume names, depending
 * on whether you set USE_VOLUME_NAMES.
 */
{
	struct DosInfo *dosinfo;
	struct DeviceList *masterlist, *devlist, *worklist;
	struct MsgPort *handler;
	LONG yuck;
	UBYTE *nameptr, *textptr;
	UBYTE len;
	SHORT i;
	struct Remember **key, *localkey, *keyptr;

	if (DosBase == NULL) return;

	key = &fileio->VolumeKey;
	FreeRemember(key, TRUE);
	localkey = NULL;

/* FORMERLY:  fileio->VolumeCount = fileio->VolumeIndex = 0;
 * Instead, try leaving index where it was, clipping as needed at the end
 */
	fileio->VolumeCount = 0;

	/* First, feel through from DosBase down to the device list, dancing
	 * all the while with the bcpl pointers.
	 */
	yuck = (LONG)((struct RootNode *)DosBase->dl_Root)->rn_Info;
	dosinfo = (struct DosInfo *)(yuck << 2);	/* reality pointer */
	yuck = (LONG)dosinfo->di_DevInfo;
	masterlist = (struct DeviceList *)(yuck << 2);	/* reality pointer */

	devlist = masterlist;


	while (devlist)
		{
		/* First, find each device that's active (dl_Task is not NULL) */
		if ((devlist->dl_Type == DLT_DEVICE) && (devlist->dl_Task))
			{
			/* OK, got a device.  Now ask it if it's a DOS disk. */
			if (NOT DOSDisk(devlist)) goto NEXT_DEVICE;

			/* OK, got a device that's a DOS disk.  Now find the name.
			 * If USE_VOLUME_NAMES is clear, use the device name,
			 * else look up the volume name.
			 */
			nameptr = (UBYTE *)devlist->dl_Name;
			if (FlagIsSet(fileio->Flags, USE_VOLUME_NAMES))
				{
				/* Use the handler for this volume to find the matching device */
				handler = (struct MsgPort *)devlist->dl_Task;

				worklist = masterlist;
				while (worklist)
					{
					if ((worklist->dl_Type == DLT_VOLUME)
							&& (worklist->dl_Task == handler))
						{
						nameptr = (UBYTE *)worklist->dl_Name;
						goto GOT_NAME;
						}

					yuck = worklist->dl_Next;
					worklist = (struct DeviceList *)(yuck << 2);
					}
				/* If we get to the end of this loop and fall out, then what?  
				 * Got an active device but no matching volume?  OK.
				 */
				}

GOT_NAME:
			yuck = (LONG)nameptr;
			nameptr = (UBYTE *)(yuck << 2);
			len = *nameptr++;

			if ((textptr = AllocRemember(&localkey, len + 2, NULL)) == NULL)
				{
				Alert(ALERT_OUTOFMEM, NULL);
				goto DEVICES_DONE;
				}

			fileio->VolumeCount++;
			for (i = 0; i < len; i++) *textptr++ = *nameptr++;
			*textptr++ = ':';
			*textptr = '\0';
			MakeEntry(localkey->Memory, key, NULL);
			}

NEXT_DEVICE:
		yuck = devlist->dl_Next;
		devlist = (struct DeviceList *)(yuck << 2);
		}


DEVICES_DONE:

	FreeRemember(&localkey, TRUE);

	/* Safety measure */
	if (fileio->VolumeIndex >= fileio->VolumeCount)
		fileio->VolumeIndex = fileio->VolumeCount - 1;

	keyptr = fileio->VolumeKey;
	nameptr = &fileio->DiskName[0];
	i = 0;
	while (keyptr)
		{
		if (StringsEqual(keyptr->Memory, nameptr))
			{
			fileio->VolumeIndex = i;
			goto DONE;
			}
		keyptr = keyptr->NextRemember;
		i++;
		}

	fileio->VolumeIndex = 0;

DONE:	;
}


UBYTE *CurrentVolumeName()
/* This routine returns a pointer the name of the current volume */
{
	SHORT i;
	struct Remember *remember;

	/* If there's no active volumes then return the "safe" volume name */
	if (OpenReqFileIO->VolumeCount < 1)
		return(&CurrentDiskString[0]);

	/* Safety measure */
	if (OpenReqFileIO->VolumeIndex >= OpenReqFileIO->VolumeCount)
		OpenReqFileIO->VolumeIndex = 0;

	/* Finally, look up the name */
	remember = OpenReqFileIO->VolumeKey;
	for (i = 0; i < OpenReqFileIO->VolumeIndex; i++)
		remember = remember->NextRemember;
	return(remember->Memory);
}


/* *** filename.c ***********************************************************
 *
 * File IO Suite  --  File Name Construction Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"
#include <libraries/dosextens.h>


#define FRIENDLY_NOT		0
#define FRIENDLY_DRAWER		1
#define FRIENDLY_OTHER		2



VOID BuildNameTable(fileio)
struct FileIOSupport *fileio;
/* This routine searches through the fileio lock for all file entries,
 * and builds a list of the names found.
 * If the user wants Workbench-style pattern matching, filenames are
 * passed through a filter before being added to the list.
 * All directory entries are added to the list.
 */
{
	struct FileInfoBlock *fileinfo;
	UBYTE *ptr;
	UBYTE workname[MAX_NAME_LENGTH + 5]; /* the extra 5 are for the ".info" */
	struct Remember **key;
	ULONG lock;
	SHORT i, flags, type;
	SHORT pick;

	fileio->NameCount = 0;
	lock = fileio->DOSLock;
	key = &fileio->NameKey;
	FreeRemember(key, TRUE);
	ClearFlag(fileio->Flags, GOOD_FILENAMES);

	if ((fileinfo = (struct FileInfoBlock *)AllocMem(
			sizeof(struct FileInfoBlock), MEMF_CLEAR)) == NULL)
		goto BUILD_EXIT;

	SetWaitPointer(OpenReqWindow);

	/* Now, first, before we might be interrupted by MessageInterrupt(),
	 * check whether or not we're looking in a drawer and, if so, add the
	 * entry that allows the user to ascend one drawer.
	 */
	if (StringLength(&OpenReqFileIO->DrawerName[0]))
		{
		MakeEntry("\253\253 PRIOR DRAWER", key, NAMED_PREVIOUS);
		/* bump the master count */
		fileio->NameCount++;
		}

	/* starting from Examine() until ExNext() is NULL */
	if (Examine(lock, fileinfo))
		while (ExNext(lock, fileinfo))
			{
			/* Default:  this entry is a normal file */
			flags = NULL;
			CopyString(&workname[0], fileinfo->fib_FileName);

#ifdef WBENCH_CODE
			/* Now, does the caller want Workbench-style pattern matching? */
			if (FlagIsSet(fileio->Flags, WBENCH_MATCH))
				{
				/* start from location 1 to avoid matching the ".info" file */
				if (ptr = FindSuffix(&workname[1], ".info")) 
					{
					*ptr = '\0';	/* strip the suffix off that baby */

					/* Get the friendliness quotient of this .info file */
					type = FriendlyInfoType(&workname[0], fileio);

					/* If just not friendly, forget about it */
					if (type == FRIENDLY_NOT) goto NEXT_LOCK;

					/* If this was a drawer, set the fileinfo as a directory */
					if (type == FRIENDLY_DRAWER)
						fileinfo->fib_DirEntryType = 1;
					}
				else goto NEXT_LOCK;
				}
#endif /* ... of WBENCH_CODE conditional */

			if (fileinfo->fib_DirEntryType >= 0)
				{
				/* This entry is a directory */
				flags = NAMED_DIRECTORY;

				/* If you change the following text, change DIR_TEXT_SIZE too */
				for (i = StringLength(&workname[0]); i >= 0; i--)
					workname[i + DIR_TEXT_SIZE] = workname[i];
				workname[0] = '\273';
				workname[1] = '\273';
				workname[2] = ' ';
				}

			pick = MakeEntry(&workname[0], key, flags);
			/* bump the master count */
			fileio->NameCount++;

			if (pick <= fileio->CurrentPick)
				fileio->CurrentPick++;

			InitOpenProp(FALSE);
			StuffSelectNames(2);

NEXT_LOCK:
			/* If there's a message pending, split with what we've got */
			if (MessageInterrupt()) goto EXAMINE_DONE;
			}

	SetFlag(fileio->Flags, GOOD_FILENAMES);


EXAMINE_DONE:

	if (OpenReqWindow) ClearPointer(OpenReqWindow);
	FreeMem(fileinfo, sizeof(struct FileInfoBlock));


BUILD_EXIT: ;
}



VOID PropInterrupt()
/* This routine is called by MessageInterrupt() if the prop gadget 
 * is played with while the file name table is being built.  
 * As long as the user is using the proportional gadget, hang around here. 
 */
{
	struct IntuiMessage *message;
	struct Gadget *gadget;
	BOOL mousemove;

	FOREVER
		{
		WaitPort(OpenReqWindow->UserPort);
		mousemove = FALSE;

		while (message = GetMsg(OpenReqWindow->UserPort))
			{
			switch (message->Class)
				{
				case GADGETUP:
					gadget = (struct Gadget *)message->IAddress;
					switch (gadget->GadgetID)
						{
						case OPENGADGET_PROPGADGET:
							ReplyMsg(message);
							HandleGadget(gadget, 0, 0, 0, 0);
							return;

						default:
							goto MESSAGE_RETURN;
						}
					break;

				case MOUSEMOVE:
					ReplyMsg(message);
					mousemove = TRUE;
					break;

				default:
					goto MESSAGE_RETURN;

				}
			}

		if (mousemove) PropMouseMoves();
		}

MESSAGE_RETURN:
	/* Pretend we didn't see this message */
	AddHead(&OpenReqWindow->UserPort->mp_MsgList, message);
}



BOOL MessageInterruptGrunt(message)
struct IntuiMessage *message;
/* Test if there's a gadget type of message at the window port, 
 * react to it if there is one, and return TRUE if the message is 
 * one that should interrupt the building of the file name list.
 */
{
	ULONG class;
	SHORT x, y;
	struct Gadget *gadget;
	LONG seconds, micros;

	class = message->Class;
	if ((class == GADGETDOWN) || (class == GADGETUP))
		{
		gadget = (struct Gadget *)message->IAddress;
		x = message->MouseX;
		y = message->MouseY;
		seconds = message->Seconds;
		micros = message->Micros;
		OpenReqSupport.SelectedGadgetID = gadget->GadgetID;

		switch (gadget->GadgetID)
			{
			case OPENGADGET_SELECTNAME:
				y = HandleSelect(y, seconds, micros);
				if ((y == -1) || (y == 1))
					{
					/* Pretend we didn't see this message */
					AddHead(&OpenReqWindow->UserPort->mp_MsgList, message);
					return(TRUE);
					}

				StuffFileName();
				StuffSelectNames(5);
				goto REPLY_AND_RETURN_FALSE;

			case OPENGADGET_UPGADGET:
			case OPENGADGET_DOWNGADGET:
				goto REPLY_AND_RETURN_FALSE;

			case OPENGADGET_PROPGADGET:
				HandleGadget(gadget, x, y, seconds, micros);
				if (class == GADGETDOWN) PropInterrupt();
				goto REPLY_AND_RETURN_FALSE;

			default:
				/* Do nothing, fall into the message's AddHead() below.
				 * This includes the gadgets OK, CANCEL, NEXTDISK, 
				 * DISKNAME, DRAWERNAME, FILENAME, and BACKDROP.
				 */
				break;
			}
		}

	/* Pretend we didn't see this message */
	AddHead(&OpenReqWindow->UserPort->mp_MsgList, message);
	return(TRUE);

REPLY_AND_RETURN_FALSE:
	ReplyMsg(message);
	return(FALSE);
}



BOOL MessageInterrupt()
/* Call MessageInterruptGrunt() with each message.
 * Return TRUE if the message is one that should interrupt the building 
 * of the file name list, else return FALSE.
 */
{
	struct IntuiMessage *message;

	while (message = GetMsg(OpenReqWindow->UserPort))
		{
		if (MessageInterruptGrunt(message))
			return(TRUE);
		}
	return(FALSE);
}



SHORT MakeEntry(name, startkey, flags)
UBYTE *name;
struct Remember **startkey;
UBYTE flags;
{
	SHORT length, pos;
	struct Remember *localkey, *nextkey, *oldkey;
	UBYTE *ptr;

	/* length equals the length of the text plus one for the 
	 * terminating NULL
	 */
	length = StringLength(name) + 1;
	localkey = NULL;
	/* Alloc one larger than length to make room for the flag byte */
	ptr = AllocRemember(&localkey, length + 1, NULL);
	if (ptr == NULL) return(32767);
	CopyString(ptr, name);
	*(ptr + length) = flags;
	nextkey = *startkey;
	pos = 0;
	oldkey = NULL;
	while (nextkey)
		{
		if (CompareUpperStrings(nextkey->Memory, name) >= 0)
			goto DONE;

		oldkey = nextkey;
		nextkey = nextkey->NextRemember;
		pos++;
		}

DONE:
	if (oldkey) oldkey->NextRemember = localkey;
	else *startkey = localkey;
	localkey->NextRemember = nextkey;
	return(pos);
}




#ifdef WBENCH_CODE

SHORT FriendlyInfoType(infoname, fileio)
UBYTE *infoname;
struct FileIOSupport *fileio;
/* This routine looks at the .info file that's named infoname and
 * tests to see if its object type and tool type match the specifications
 * in the fileio structure.  Returns TRUE if everything matches.
 * If the .info file couldn't be opened or if the requirements don't
 * match, FALSE is returned.
 */
{
	struct DiskObject *object;
	SHORT result;

	result = FRIENDLY_NOT;
	if (object = GetDiskObject(infoname)) 
		{
		if ((object->do_Type == WBDRAWER) || (object->do_Type == WBGARBAGE))
			result = FRIENDLY_DRAWER;
		else if (object->do_Type == WBDISK) 
			result = FRIENDLY_NOT;
		else
			{
			if (FlagIsSet(fileio->Flags, MATCH_OBJECTTYPE))
				if (object->do_Type != fileio->DiskObjectType) goto FRIEND_DONE;

			result = FRIENDLY_OTHER;

			if (FlagIsSet(fileio->Flags, MATCH_TOOLTYPE))
				{
				if (NOT MatchToolValue(
					   FindToolType(object->do_ToolTypes, "FILETYPE"), 
						&fileio->ToolType[0]))
					result = FRIENDLY_NOT;
				}
			}
		}

FRIEND_DONE:
	if (object) FreeDiskObject(object);
	return(result);
}

#endif /* ... of WBENCH_CODE conditional */




/* *** strings.c ************************************************************
 *
 * String Manipulation Routines
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 *
 * CONFIDENTIAL and PROPRIETARY
 *
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 4 Feb 87     RJ              Real release
 * 2 Feb 87     RJ              Added StripOuterSpace()
 * 16 May 86    RJ              Just more stuff
 * 1 June 1985  =RJ Mical=      Created this file from a weird distant echo.
 *
 * *********************************************************************** */


#include "exec/types.h"

#define FOREVER for(;;)


/* copy these and define them as 'extern' in your global include file 
 * in order to use these routines in your code.
 */
SHORT CompareUpperStrings();
UBYTE *FindSuffix();
SHORT GetAtom();
SHORT IndexString();
SHORT StringLength();
BOOL StringsEqual();
UBYTE *StripLeadingSpace();
UBYTE *StripOuterSpace();



SHORT CompareUpperStrings(s, t)
UBYTE *s, *t;
/* Compare s to t, making alphabet characters uppercase.  Return value:
 *  	s < t 		-1
 *  	s == t 		 0
 *  	s > t 		 1
 */
{
	SHORT sc, tc;

	FOREVER
		{
		sc = *s++;
		if ((sc >= 'a') && (sc <= 'z')) sc = sc - 'a' + 'A';
		tc = *t++;
		if ((tc >= 'a') && (tc <= 'z')) tc = tc - 'a' + 'A';

		if (sc < tc) return(-1);
		if (sc > tc) return(1);
		if (sc == '\0') return(0);
		}
	return(0);
}



VOID CopyString(tostring, fromstring)
UBYTE *tostring, *fromstring;
{
	while (*tostring++ = *fromstring++) ;
}



VOID ConcatString(firststring, addstring)
UBYTE *firststring, *addstring;
{
	SHORT length1;

	length1 = StringLength(firststring);
	firststring += length1;
	CopyString(firststring, addstring);
}



UBYTE *FindSuffix(string, suffix)
UBYTE *string;
UBYTE *suffix;
{
	SHORT stringlength, suffixlength;

	if ( (stringlength = StringLength(string))
			>= (suffixlength = StringLength(suffix)) )
		{
		if (StringsEqual(string + stringlength - suffixlength, suffix))
			return (string + stringlength - suffixlength);
		}
	return(NULL);
}



SHORT IndexString(lookin, lookfor)
UBYTE *lookin, *lookfor;
/* This routines looks in the lookin string for the lookfor string.
 * Returns the position of the lookfor string in lookin, or
 * returns -1 if the lookfor string was not found.
 */
{
	SHORT index;
	UBYTE *workin, *workfor;

	index = 0;

	while (*lookin)
		{
		workin = lookin;
		workfor = lookfor;
		while ((*workfor) && (*workin) && (*workfor == *workin))
			{
			workfor++;
			workin++;
			}
		if (*workfor == '\0') return(index);
		lookin++;
		index++;
		}

	return(-1);
}



SHORT StringLength(text)
UBYTE *text;
{
	SHORT length;

	length = 0;
	while (*text++) length++;
	return(length);
}



BOOL StringsEqual(text1, text2)
UBYTE *text1, *text2;
{
	while (*text1 == *text2)
		{
		if (*text1 == '\0') return(TRUE);
		text1++;
		text2++;
		}

	return(FALSE);
}



UBYTE *StripLeadingSpace(text, space)
UBYTE *text, *space;
/* This routine "strips" the leading occurrences of the space string
 * characters from the text string by finding the first character
 * of the text string that is not contained in the space string.
 * The return value is a pointer to the first non-space character in text.
 * If text is the null string or if text consists of nothing
 * but space characters, NULL is returned.
 * If space is the NULL string, text is returned.
 */
{
	if (space == NULL) return(text);
	if (*space == '\0') return(text);

	while (*text && (*text == *space)) text++;

	if (*text) return(text);
	return(NULL);
}



UBYTE *StripOuterSpace(text, space)
UBYTE *text, *space;
/* This routine "strips" the leading and trailing occurrences of the space 
 * string characters from the text string.
 * The return value is a pointer to the first non-space character in text,
 * and a null byte is stored after the text's last non-space character.
 */
{
	UBYTE *workspace, *lastspace;

	if (text == NULL) return(NULL);

	if ((workspace = StripLeadingSpace(text, space)) == NULL)
		{
		*text = '\0';
		return(text);
		}

	lastspace = workspace + StringLength(workspace) - 1;
	while ((lastspace >= workspace) && (*lastspace == *space)) lastspace--;
	*(lastspace + 1) = '\0';
	return(workspace);
}




/* *** pointers.c ***********************************************************
 *
 * Intuition Pointers Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?  Thanks.
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#define FILEIO_SOURCEFILE
#include "fileio.h"


extern USHORT ElecArtsWaitPointer[];


VOID SetWaitPointer(window)
struct Window *window;
{
	/* This one is the default because it's my favorite */
	SetPointer(window, &ElecArtsWaitPointer[0], ELECARTSPOINT_HEIGHT,
		16, ELECARTSPOINT_XOFF, ELECARTSPOINT_YOFF);
}


/* *** alerts.c *************************************************************
 *
 * Alert and Abort AutoRequest Routines
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 27 Sep 87    RJ              Removed reference to alerts.h, brought 
 *                              declarations into this file
 * 4 Feb 87     RJ              Real release
 * 11 Jul 86    RJ >:-{)*       Prepare (clean house) for release
 * 1 Feb 86     =RJ Mical=      Created this file with fond memories
 *
 * *********************************************************************** */


#include <prosuite/prosuite.h>


#define ALERT_TEXT_TOP   6
#define ALERT_TEXT_LEFT  6


extern struct TextAttr SafeFont;


UBYTE *AlertStrings[] =
	{
	/*       "Longest allowed string -----------|" */
	(UBYTE *)"Really broken.  Stop all and reboot",
	(UBYTE *)"Out of memory!  Sheesh.",
	(UBYTE *)"Invalid Disk or Drawer Selection",
	};


struct IntuiText AlertText =
	{
	AUTOFRONTPEN, AUTOBACKPEN, AUTODRAWMODE,
	AUTOLEFTEDGE + ALERT_TEXT_LEFT, AUTOTOPEDGE + ALERT_TEXT_TOP,
	&SafeFont,
	NULL, NULL,
	};


struct IntuiText AlertOKText =
	{
	AUTOFRONTPEN, AUTOBACKPEN, AUTODRAWMODE,
	AUTOLEFTEDGE, AUTOTOPEDGE,
	&SafeFont,
	(UBYTE *)"OK", 
	NULL,
	};



VOID AlertGrunt(text, window)
UBYTE *text;
struct Window *window;
{
	AlertText.IText = text;
	AutoRequest(window, &AlertText, NULL, 
			&AlertOKText, 0, 0, 320, 48 + ALERT_TEXT_TOP);
}
 


VOID Alert(abortNumber, window)
USHORT abortNumber;
struct Window *window;
{
	AlertGrunt(AlertStrings[abortNumber], window);
}
 


VOID Abort(abortNumber, window)
USHORT abortNumber;
struct Window *window;
{
	Alert(abortNumber, window);
/*???	FOREVER Alert(ALERT_ABORT, window);*/
	FOREVER Alert(0, window);
}
 

/* *** reqsupp.c ************************************************************
 *
 * Requester Support Routine
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     =RJ Mical=      Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


#include <prosuite/prosuite.h>
#include <prosuite/reqsupp.h>


struct IntuiMessage *GetMsg();



/* *** DoRequest() **********************************************************
 * 
 * NAME
 *     DoRequest  --  Creates and manages a requester
 * 
 * 
 * SYNOPSIS
 *     DoRequest(ReqSupport);
 * 
 * 
 * FUNCTION
 *     Creates a requester according to the specifications laid out
 *     by you in a ReqSupport structure, and manages the interaction
 *     with the requester for you.  In the end this routine returns control
 *     to you with an identifier describing which gadget the user selected
 *     to terminate the requester; this identifier can be found in the
 *     SelectedGadgetID field of your ReqSupport structure.
 * 
 *     Note that if anything goes wrong while trying to create the 
 *     requester (usually out of memory) this routine returns 
 *     immediately with the SelectedGadgetID field set to zero.
 *     Because of this, you should either avoid GadgetIDs of zero
 *     or at least you should have your Cancel Gadget have an
 *     ID of zero.
 * 
 *     You can specify routines that will be called when certain events
 *     occur while the requester is displayed.  For instance, you can
 *     specify that a particular routine be called every time the
 *     user selects any of the requester gadgets.  See the documentation
 *     and the ReqSupport structure for details about what
 *     routine vectors you can supply.
 * 
 * 
 * INPUTS
 *     ReqSupport = pointer to a ReqSupport structure
 * 
 * 
 * RESULT
 *     Returns the identifier of the gadget that ended the requester
 *     in the ReqSupport's SelectedGadgetID field.
 *     If anything goes wrong (usually out of memory) the SelectedGadgetID
 *     field is set to zero.
 */
VOID DoRequest(reqsupp)
struct ReqSupport *reqsupp;
{
	ULONG class;
	SHORT x, y;
	struct IntuiMessage *message;
	struct Gadget *gadget;
	ULONG saveidcmp;
	BOOL IAintGotNoSatisfaction, mousemoved;
	struct Window *window;
	LONG seconds, micros;

	window = reqsupp->Window;

	saveidcmp = window->IDCMPFlags;
	/* If you change the list of IDCMP flags, check the effect it has 
	 * on the switch / case statements of MessageInterrupt() in filename.c
	 */
	ModifyIDCMP(window, 
			GADGETUP | GADGETDOWN | REQSET | REQCLEAR
			| MOUSEMOVE | DISKINSERTED);

	if (Request(&reqsupp->Requester, window) == FALSE)
		{
		reqsupp->SelectedGadgetID = 0;
		goto JUMP_SHIP;
		}

	IAintGotNoSatisfaction = TRUE;

	while (IAintGotNoSatisfaction)
		{
		Wait(1 << window->UserPort->mp_SigBit);

		mousemoved = FALSE;

		while (message = GetMsg(window->UserPort))
			{
			gadget = (struct Gadget *)message->IAddress;
			class = message->Class;
			x = message->MouseX;
			y = message->MouseY;
			seconds = message->Seconds;
			micros = message->Micros;
			ReplyMsg(message);

			if (IAintGotNoSatisfaction) switch (class)
				{
				case REQSET:
					/* Does the caller have some startup 
					 * stuff to perform now that the
					 * requester has been opened?
					 */
					if (reqsupp->StartRequest)
						(*reqsupp->StartRequest)();
					break;
				case DISKINSERTED:
					if (reqsupp->NewDiskHandler) (*reqsupp->NewDiskHandler)();
					break;
				case MOUSEMOVE:
					mousemoved = TRUE;
					break;
				case GADGETDOWN:
				case GADGETUP:
					reqsupp->SelectedGadgetID = gadget->GadgetID;
					if (reqsupp->GadgetHandler)
						{
						if ((*reqsupp->GadgetHandler)(gadget, 
								x, y, seconds, micros))
							{
							EndRequest(&reqsupp->Requester, window);
							IAintGotNoSatisfaction = FALSE;
							}
						}
					break;
				case REQCLEAR:
					IAintGotNoSatisfaction = FALSE;
					break;
				}
			}

		if (mousemoved && reqsupp->MouseMoveHandler && IAintGotNoSatisfaction)
			(*reqsupp->MouseMoveHandler)();
		}

JUMP_SHIP:
	ModifyIDCMP(window, saveidcmp);
}



/* *** globfio.c ************************************************************
 *
 * File IO Suite  --  Global Variable Definitions
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *         (available soon if not already)
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 27 Sep 87    RJ              Changed name of this file from global.c
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     RJ              Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */


/* This prevents eglobfio.c from being included */
#define EGLOBAL_FILEIO_CANCEL
#define FILEIO_SOURCEFILE
#include "fileio.h"



/* === System Global Variables ========================================== */
struct IntuitionBase *IntuitionBase = NULL;
struct GfxBase *GfxBase = NULL;
struct DosLibrary *DosBase = NULL;
struct IconBase *IconBase = NULL;


#ifndef EGLOBAL_FILEIO_C
#define EGLOBAL_FILEIO_C


/* *** eglobfio.c ***********************************************************
 *
 * File IO Suite  --  External Global Variable Definitions
 *     from Book 1 of the Amiga Programmers' Suite by RJ Mical
 *         (available soon if not already)
 *
 * Copyright (C) 1986, 1987, Robert J. Mical
 * All Rights Reserved.
 *
 * Created for Amiga developers.
 * Any or all of this code can be used in any program as long as this
 * entire copyright notice is retained, ok?
 *
 * The Amiga Programmer's Suite Book 1 is copyrighted but freely distributable.
 * All copyright notices and all file headers must be retained intact.
 * The Amiga Programmer's Suite Book 1 may be compiled and assembled, and the 
 * resultant object code may be included in any software product.  However, no 
 * portion of the source listings or documentation of the Amiga Programmer's 
 * Suite Book 1 may be distributed or sold for profit or in a for-profit 
 * product without the written authorization of the author, RJ Mical.
 * 
 * HISTORY      NAME            DESCRIPTION
 * -----------  --------------  --------------------------------------------
 * 27 Sep 87    RJ              Changed name of this file from eglobal.c
 * 4 Feb 87     RJ              Real release
 * 12 Aug 86    RJ >:-{)*       Prepare (clean house) for release
 * 3 May 86     RJ              Fix prop gadget for both 1.1 and 1.2
 * 1 Feb 86     =RJ Mical=      Created this file.
 *
 * *********************************************************************** */



/* === System Global Variables ========================================== */
extern struct IntuitionBase *IntuitionBase;
extern struct GfxBase *GfxBase;
extern struct DosLibrary *DosBase;
extern struct IconBase *IconBase;

extern struct TextAttr SafeFont;
extern struct Gadget OKGadget, CancelGadget;



/* === Open Requester Declarations ======================================= */
/* The global declaration of these can be found in opendata.c */
extern struct Requester *OpenReq;
extern struct ReqSupport OpenReqSupport;
extern struct Window *OpenReqWindow;
extern struct FileIOSupport *OpenReqFileIO;

extern struct StringInfo OpenNameTextInfo;
extern struct StringInfo OpenDrawerTextInfo;
extern struct StringInfo OpenDiskTextInfo;
extern struct Gadget OpenNameTextGadget;
extern struct Gadget OpenDiskTextGadget;
extern struct Gadget OpenDrawerTextGadget;
extern struct PropInfo OpenPropInfo;
extern struct Image OpenPropImage;
extern struct Gadget OpenSelectNameGadget;

extern UBYTE OpenUndoBuffer[];
extern UBYTE OpenSelectBuffers[NAME_ENTRY_COUNT][VISIBLE_SELECT_LENGTH];
extern struct IntuiText OpenSelectText[NAME_ENTRY_COUNT];
extern UBYTE OpenLockName[];

extern struct IntuiText ReqTitleText;
extern UBYTE *DefaultReqTitle;

extern ULONG OpenSaveLock;

extern UBYTE CurrentDiskString[];

extern LONG OpenClickSeconds;
extern LONG OpenClickMicros;

/* The global declaration of these can be found in chipdata.c */
extern USHORT OpenUpData[];
extern USHORT OpenDownData[];
extern USHORT OpenPropData[];
extern USHORT OpenPropTop[];
extern USHORT OpenPropBottom[];



#endif /* of EGLOBAL_FILEIO_C */

