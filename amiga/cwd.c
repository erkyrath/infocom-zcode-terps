
/* Find the pathname of the current working directory.  Uses a recursive
   call to a function named followpath.  Its job is to continue to call the 
   ParentDir function until it obtains a lock value of zero, and to append
   (to a global string) the name of the directory it pops into at each step.
   In the root directory, the lock gives us the volume name of the disk
   (how do we get the drive name instead?).  The program appends a colon
   after the volume name and a slash after each subdirectory along the way
   to the one we're in currently.  [Taken from example in Peck p36.]
*/

#include "libraries/dos.h"
#include "libraries/dosextens.h"
#include "exec/memory.h"

extern struct FileLock *Lock(), *ParentDir();

char pathname[101];

main()	/* cwd() */
{
    struct FileLock *cwdlock;
    pathname[0] = 0;		/* start with null string */

    /* get a read lock on the current directory */
    cwdlock = Lock ("", ACCESS_READ);

    if (cwdlock != 0)
	followpath (cwdlock, 0);

    /* NOTE: followpath unlocks the lock */

    printf ("path is %1s\n", pathname);	    /* if main(), display result */
}

int followpath (lock, printslash)
struct FileLock *lock;
int printslash;
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

    /* recursively call this same function to follow path up to the root */
    if (!error)
	error = followpath (newlock, 1);

    if (!error) {
	/* alloc memory AFTER the recursion, to reduce chance of error */
	myinfo = (struct FileInfoBlock *)
	    AllocMem (sizeof(struct FileInfoBlock), MEMF_CLEAR);

	if (myinfo == 0) error = -1;
	else {
	    success = Examine (lock, myinfo);

	    if (!success) error = -1;
	    else {
		strcat (pathname, myinfo->fib_FileName);	/* dir name */

		if (newlock == 0) 		/* actually vol name */
		    strcat (pathname, ":");
		else if (printslash)
		    strcat (pathname, "/");
		}

	    FreeMem (myinfo, sizeof(struct FileInfoBlock));
	    }
	}

    UnLock (lock);

    if (error) 		/* make null the global string */
	pathname[0] = 0;

    return (error);	/* (zero if ok) */
}
