
	In the interest of having a general ZIP paging scheme that is faster
	than the time stamping scheme used in the PDP-11 version, a linked
	list LRU scheme was implemented.  The LRU chain is a doubly linked
	list of blkdesc structures.  Each structure contains a previous and
	next pointer, a char ptr to the buffer and a virtual page number for
	the page currently residing in the buffer.  The global variable MRU
	is a pointer to the structure corresponding to the most recently used
	page; consequently, mru->prev points to the LRU buffer.

	The routine Getpag takes a block number and returns a char pointer
	to the start of the corresponding block.  It also performs all the
	necessary pointer manipulation and paging to make that pointer valid.

/*  P A G I N G  */

short curblk = -1,		/* current block (same as last zpc1) */
    curpag = -1;		/* last page gotten (from getpag) */

char *curblkloc,		/* pointer to curblk block */
    *curpagloc;			/* pointer to curpag block */

struct blkdesc {
    struct blkdesc *next,	/* next descriptor ptr */
	*prev;			/* previous descriptor ptr  */
    char *loc;			/* page pointer */
    short vpage;		/* page number */
    }
    pagedesc[MAXBLKS];		/* one descriptor for each virtual page */

struct blkdesc *mru,		/* most recently used blkdesc */
    *pagemap[MAXBLKS];		/* one mapping for each virtual page */

memini()
{ /*	This routine compares memreq with ENDLOD and PLENTH.  It
	determines how much dataspace to allocate, and does so.  It determines
	how much data to preload, and does so.  It also initializes paging.
  */
    char buffer[BLKSIZ];		/* temp space for block 0 */
    ZIPINT maxlod;
    short i;
    char *md_alloc();

/*  Read the first block into a temporary buffer.  We temporarily set
    dataspace to point to this buffer, so that getpre() and the GTV macros
    work.  */

    dataspace = buffer;
    getpre(0, 1);			/* get block 0 */

    endlod = GTVWRD(PENDLD);		/* get endlod pointer */
    if (endlod & BYTEBITS) THEN
      endlod += BLKSIZ;			/* round up to next block */
    endlod >>= CVTBLK;			/* convert to blocks */

    maxlod = GTVWRD(PLENTH);		/* length of program, in words */
    if (maxlod & 0xFF) THEN
      maxlod += BLKSIZ/2;		/* round up to next block */
    maxlod >>= CVTBLK-1;		/* convert to blocks */

/*  Note that our paging scheme normally requires a minimum of 2 pages in 
    the chain, one for the current code page and a second for roving pointers.
    In the freak case where only one page is not preloaded, however, the
    "chain" may contain only one page too.  When all pages are preloaded, 
    paging is never called and no chain at all is required.  Thus an array
    of MAXBLKS paging structures is the most ever needed.
*/
    if (memreq < endlod + 2) THEN
      fatal("Insufficient memory for preload");

    if (memreq >= maxlod) THEN {	/* mucho memory, take advantage */
      endlod = maxlod;		/* hack endlod to force total preload */
      memreq = maxlod;		/* reduce memreq to max needed */
      }

    if ((dataspace = md_alloc(memreq * BLKSIZ)) == NULL) THEN {
      printf("Unable to allocate %d", memreq, "blocks");
      fatal("Memory allocation error");
      }
    getpre(0, endlod);			/* read in preload data */

/*  Currently, an array of blkdescs and a pagemap are declared statically
    [0..255].  Should allocate space dynamically for [endlod..memreq-1] only 
    (number of physical buffers), and a pagemap array for [0..maxlod-1] only
    (number of actual pages).

    IDEA:  call getpre(endlod, memreq) to "prime" the page buffers, and 
    mark each pagedesc and pagemap appropriately.
*/

    if (endlod < maxlod) THEN {		/* if total preload, just skip */

      for (i = 0; i < MAXBLKS; i++)
        pagemap[i] = NOT_IN_CORE;	/* no paged pages in core, yet */

      for (i = endlod; i < memreq; i++) {
        pagedesc[i].next = &pagedesc[i+1];	/* setup pointer chain */
        pagedesc[i].prev = &pagedesc[i-1];
        pagedesc[i].loc = ((char *)(dataspace + (i * BLKSIZ)));
        pagedesc[i].vpage = NO_PAGE;
        }
      i = memreq - 1;
      pagedesc[i].next = &pagedesc[endlod];	/* make the list circular */
      pagedesc[endlod].prev = &pagedesc[i];	/* excluding pre and extra */
      mru = &pagedesc[i];			/* init mru to last page */
      }
}



char *getpag(blk)	/* return a pointer to the requested page */
short blk;
{
   /*	This is the heart of the paging scheme.  It manages a doubly-linked
	list of block descriptors.  Preloaded pages are not included in this
	list so they cannot be paged out.  If the page requested is preloaded,
	a valid pointer is returned immediately.  

	Otherwise, the block is removed from the linked list, spliced into
	the front of the list and made mru.  There are two subroutines, unlink
	and relink, that manage the linked list.

	If the block is not in core, the current mru's->previous (or lru
	block) buffer is used to page in the requested block.  Then the
	information in the corresponding block descriptors is filled to
	indicate the absence of the lru and the presence of the new.  The mru
	pointer is then pointed at this block.
   */

    struct blkdesc *lru;
    short deadpage;

#if _DEBUG
    if (blk >= MAXBLKS) THEN		/* valid block request? */
      fatal("Virtual page number out of range");
#endif

    if (blk == curpag) THEN		/* same page as last time */
      return(curpagloc);		/* return immediately */

    if (blk < endlod) THEN {		/* preloaded, expand the pointer */
      curpag = blk;
      curpagloc = dataspace + (blk << CVTBLK);
      return(curpagloc); 
      }

    if (pagemap[blk] == NOT_IN_CORE) THEN {

/* When choosing the (lru) page to discard, make sure it's not the 
   current code page (where the zpc is), otherwise the newzpc buffer pointer 
   becomes invalid! -- DBB */

      lru = mru->prev;			/* get oldest page */
      deadpage = lru->vpage;
      if (deadpage == curblk) THEN {	/* but avoid using the zpc page */
        lru = lru->prev;		/* get next oldest page */
	deadpage = lru->vpage;
	}
      getblk(blk, lru->loc);		/* read new page over lru page */
      if (deadpage != NO_PAGE) THEN	/* mark old page as gone */
        pagemap[deadpage] = NOT_IN_CORE;
      pagemap[blk] = lru;		/* update map for new page */
      lru->vpage = blk;			/* update desc for new page */
      mru = lru;			/* update mru */
      }

    else				/* page is resident */
      if (pagemap[blk] != mru) THEN {	/* if already mru, do nothing */
	unlinkb(blk);			/* unsplice from wherever it is */
	relinkb(blk);			/* link it in as new mru */
	}

    curpag = blk;			/* update page globals */
    curpagloc = mru->loc;
    return(curpagloc);			/* return pointer */
}

unlinkb(block)
short block;
{  /* 	Unlink removes a block descriptor from the lru chain.
   */
    struct blkdesc *t1, *t2;

    t1 = pagemap[block]->prev;		/* get pointer to one end */
    t2 = pagemap[block]->next;		/* and the other */
    t1->next = t2;			/* swap pointers */
    t2->prev = t1;
}

relinkb(block)
short block;
{  /*  Splice a block back into the lru chain (becomes the new mru).
   */
    struct blkdesc *newblk, *lru;

    newblk = pagemap[block];		/* pointer to the splice block */
    lru = mru->prev;			/* lru pointer */
    newblk->next = mru;			/* update new desc's prev and next */
    newblk->prev = lru;
    mru->prev = newblk;			/* update mru and lru descs */
    lru->next = newblk;
    mru = newblk;			/* new mru */
}

