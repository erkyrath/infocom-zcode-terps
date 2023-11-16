
/* *************************************************************************
 *
 *	yzip-fileio.c 
 *
 * *********************************************************************** */

/* This is YZIP's interface to fileio.  Collecting all YZIP extensions
 * together in this file should make upgrading to a new version of fileio 
 * easier, if one becomes available. 
 */

#define FILEIO_SOURCEFILE
#include "fileio.h"

/* externs (from lattice lib; also used in z-inits) */

extern stpchr ();
extern strrchr ();

/* user globals */

struct FileIOSupport *Zfileio = NULL;


/*------------------------------*/
/*	fsel_init		*/
/*------------------------------*/

/* (returns no result, just leaves Zfileio null if error) */

VOID fsel_init (init, ref_icon, curdir)
BOOL init;			/* False to cleanup */
struct DiskObject *ref_icon;	/* our template */
char *curdir;
{
	char *p1, c;

	if (init) {
		Zfileio = GetFileIOSupport();
		if (Zfileio == NULL)
			return;

		unBuildFileIOPathname (Zfileio, curdir);
		Zfileio->FileName[0] = 0;	/* (but no fname) */

		SetFlag (Zfileio->Flags, USE_VOLUME_NAMES);

	/***	SetFlag (Zfileio->Flags, WBENCH_MATCH | MATCH_OBJECTTYPE);
		Zfileio->DiskObjectType = WBTOOL;	***/

	/* In our preloaded icon template, patch the "type string" 
	   slightly, so the Fileio display will list the user's subsequent 
	   Saves, but /avoid/ including the template file itself. */

		p1 = (char *) FindToolType (
			ref_icon->do_ToolTypes, "FILETYPE");

		if (p1 != NULL)			/* found a valid string? */
		if ((c = *p1) != 0) {
			if (c >= 0x60)
				c -= 0x20;	/* toggle case */
			else	c += 0x20;
			*p1 = c;

		/* copy the patched string into the fileio record, and
		   use it for all saves.  Notice this means we can customize
		   the type-string for each sku we ship by simply 
		   editing the special icon ... */

			CopyString (&Zfileio->ToolType[0], p1);
			SetFlag (Zfileio->Flags, 
				WBENCH_MATCH | MATCH_TOOLTYPE);
			}
	/* if a valid type string wasn't found (i.e. it's been deliberately
	   removed from the special icon by a user), we just avoid setting 
	   the match-flags, so FileIO does no filtering at all.  
	   Might come in handy someday ... */

		}	/* end of init */
	else {		/* cleanup */
		if (Zfileio) 
			ReleaseFileIO (Zfileio);
		}
}

/*------------------------------*/
/*	fsel_do			*/
/*------------------------------*/

/* call GetFileIOName(), leave full path+name in given buffer.
 * return values:
 *	1 if Cancelled
 *	0 (meaning "no error" to YZIP) if name gotten
 *     -1 if FileIO call not available
 */
int fsel_do (opsave, partial, Zfullname, ZWind)
BOOL opsave;		/* if zero, this is a Restore */
BOOL partial;
char *Zfullname;
struct Window *ZWind;
{
	BOOL result;
	struct FileIOSupport tfio;	/* temp */
	BOOL new_fname = FALSE,
	  new_dir = FALSE;

	if (!Zfileio)
		return (-1);

/* [Initially, Zfullname contains the Current Dir]  */

	if (opsave) {
		Zfileio->ReqTitle = (UBYTE *)"Save to file:";

	/* For a save dialog, we want the "Type a Name" field to wake up 
	 * empty, so the user won't have to tediously erase it.  
	 * We don't disturb the Disk/Drawer fields.
	 */
		Zfileio->FileName[0] = 0;
		Zfileio->CurrentPick = -1;	/* "none" */
		}

	else {
		Zfileio->ReqTitle = (UBYTE *)"Restore from file:";

	/* Check whether disk/drawer matches last /successful/ call.
	   If changed, copy fields back into the fileio record as defaults, 
	   and invalidate fileio's display list.  */

		unBuildFileIOPathname (&tfio, Zfullname);
		if (!StringsEqual (&Zfileio->DiskName[0], &tfio.DiskName[0]))
			{
			CopyString (&Zfileio->DiskName[0], &tfio.DiskName[0]);
			new_dir = TRUE;
			}
		if (!StringsEqual (&Zfileio->DrawerName[0], &tfio.DrawerName[0]))
			{
			CopyString (&Zfileio->DrawerName[0], &tfio.DrawerName[0]);
			new_dir = TRUE;
			}

	/* also check for fname match */

		if (!StringsEqual (&Zfileio->FileName[0], &tfio.FileName[0]))
			{
			CopyString (&Zfileio->FileName[0], &tfio.FileName[0]);
			new_fname = TRUE;
			}

		if (new_dir)
			ClearFlag (Zfileio->Flags, GOOD_FILENAMES);
		if (new_dir || new_fname)
			Zfileio->CurrentPick = -1;	/* "none" */
		}

/* go for it ... */

	result = GetFileIOName (Zfileio, ZWind);
	if (result)		/* 1 means not cancelled */
		BuildFileIOPathname (Zfileio, Zfullname);

/* This would only be neccessary only if we had /another/ fileio structure
   besides Zfileio. */

/*	if (FlagIsSet (Zfileio->Flags, DISK_HAS_CHANGED))
 *	/-* Oops, disk swapped, so restart the other *-/
 *		{
 *		ClearFlag (Zfileio->Flags, DISK_HAS_CHANGED);
 *		ClearFlag (Xfileio->Flags, GOOD_FILENAMES);
 *		}
 */
	if (!result)
		return (1);	/* (map False to 1) */
	else	return (0);	/* (and True to 0) */
}

/*------------------------------*/
/*	fsel_written		*/
/*------------------------------*/

/* This is called after a new Save file has been successfully created. */

fsel_written (Zfullname)
char *Zfullname;
{
	if (Zfileio)
		AddFileIOName (Zfileio, Zfullname);
}

/*------------------------------*/
/*	fsel_diskevent		*/
/*------------------------------*/

/* This is called anytime the YZIP main event loop detects that a 
 * new disk was inserted.
 */

fsel_diskevent ()
{
	if (Zfileio)
		ClearFlag (Zfileio->Flags, GOOD_FILENAMES);
}

/*------------------------------*/
/*	unBuildFileIOPathname	*/
/*------------------------------*/

/* given a full path in buffer, 
   split it up and copy it into the disk-drawer-name fields of fileio */

unBuildFileIOPathname (fio, buffer)
struct FileIOSupport *fio;
UBYTE *buffer;
{
	char *p1, *p2, *p3;
	char *base1, *base2, *base3;
	char local[128];

	p1 = &fio->DiskName[0];
	p2 = &fio->DrawerName[0];
	p3 = &fio->FileName[0];

/* To avoid leaving trash in the input buffer, make a local copy of it. */

	base1 = &local[0];
	CopyString (base1, buffer);

	base2 = (char *) stpchr (base1, ':');	/* find first colon */
	if (!base2) {
		base2 = base1;
		*p1 = 0;
		}
	else {
		*base2++ = 0;
		CopyString (p1, base1);		/* copy DiskName */
		ConcatString (p1, ":");		/*   and colon */
		}

	base3 = (char *) strrchr (base2, '/');	/* find LAST slash */
	if (!base3) {
		base3 = base2;
		*p2 = 0;
		}
	else {
		*base3++ = 0;
		CopyString (p2, base2);		/* copy DrawerName */
		}				/*   (but not slash) */

	CopyString (p3, base3);			/* copy FileName */
}
