
/*
Steps in picture compression (undo in reverse order):

1.  Each line of the picture is exclusive-or'ed with the previous line.

2.  A run-length encoding is applied, as follows:  byte values 0
through 15 represent colors; byte values 16 through 127 are repetition
counts (16 will never actually appear)  Thus: 3 occurrences of byte 
value 2 will turn into 2 17 (subtract 15 from the 17 to find that the 
two should be repeated 2 MORE times).

3.  Optionally, the whole thing is Huffman-coded, using an encoding
specified in the header file.	*/

#define ULONG unsigned long		/* 32 bits */
#define UWORD unsigned short		/* 16 */
#define UCHAR unsigned char		/* 8  */

/*------------------------------*/
/* uncompress_huff		*/
/*------------------------------*/

/* Uncompress a huffed picture.
   Does all three steps simultaneously--unhuf, then undo rle and xor steps.
   Stores extra line of 0s at beginning of outbuf, for xor step.
   Returns number of decompressed bytes.  */
   
ULONG /*int*/ uncompress_huff (inbuf, outbuf, huff_tree, inlen, pic_x)
unsigned char *inbuf, *outbuf, *huff_tree;
int pic_x, inlen;
{
	unsigned char *p = &outbuf[pic_x];
	int j, olen;
	int /*UCHAR*/ chr, cnode = 0, cbit = 128, lastpix;
	unsigned char *p_base = p;

	olen = (inbuf[0] << 8) | inbuf[1];	/* Actual number of characters resulting from unhuf */
	inbuf += 2;
	for (j = 0; j < pic_x; j++)
		outbuf[j] = 0;
		
	chr = *inbuf++;				/* Get the first character */
	inlen--;				/* etc */
	while (1) {
		if (chr & cbit)				/* Proceed with huffman */
			cnode = huff_tree[cnode + 1];
		else
			cnode = huff_tree[cnode];
			
		if (cnode < 128)
			cnode *= 2;
		else {
			cnode -= 128;	/* this is a terminal */
			
			/* here we undo both runlength and xor */
			if (cnode < 16) {	/* It's a color id, output/xor it */
				*p++ = cnode ^ *outbuf++;
				lastpix = cnode;
				}
			else {			/* Otherwise, run/xor last color id */
				for (j = 0; j < (cnode - 15); j++) {
					*p++ = lastpix ^ *outbuf++; 
					}
				}
			if (--olen <= 0) break;
			cnode = 0;
			}
		cbit = cbit >> 1;		/* Next bit */
		if (!cbit) {			/* If done with this char, go to next */
			if (inlen-- <= 0) break;
			cbit = 128;
			chr = *inbuf++;
			}
		}
	return (p - p_base);		/* final outlen */
}

/*------------------------------*/
/* uncompress_nohuff		*/
/*------------------------------*/

int uncompress_nohuff (inbuf, outbuf, inlen, pic_x)
unsigned char *inbuf, *outbuf;
int pic_x, inlen;
{
	unsigned char *p = &outbuf[pic_x];
	unsigned char *p_base = p;
	int lastpix, val, j;
	
	for (j = 0; j < pic_x; j++)
		outbuf[j] = 0;

	while (inlen--) {
		val = *inbuf++;
		if (val < 16) {
			lastpix = val;
			*p++ = val ^ *outbuf++;
			}
		else {
			for (j = 0; j < (val - 15); j++) {
				*p++ = lastpix ^ *outbuf++;
				}
			}
		}
	return (p - p_base);		/* final outlen */
}

/*------------------------------*/
/* binary_search		*/
/*------------------------------*/

/* search a directory for a given id  [first word of each entry],
   return ptr, or null if not found */

short *binary_search (dir, e_count, e_size /*words*/, id)
short *dir;
int e_count, e_size, id;
{
	register int top, bot, cur;
	register int cid;
	register short *p;

	bot = 0;
	top = e_count;
	while (top > bot) {
		cur = bot + ((top - bot) >> 1);	  /* DIV 2, round DOWN */
		p = &dir[cur * e_size];
		
		if ((cid = *p) == id)
			return (p);
			
		if (cid < id)
			bot = cur + 1;
		else	top = cur;
		}
	return (0);
}


/** DEAD CALLS **/

#define incl_dead 0
#if incl_dead == 1

/* huf is Huffman tree, from header; len is number of bytes; buf1 is
   actual data; buf2 is output data.  Returns number of decoded bytes. */

int unhuf (huf, len, buf1, buf2)
unsigned char *huf, *buf1, *buf2;
int len;
{
	int olen = 0;
	int chr;
	unsigned char *outbuf;
	int cnode = 0, cbit = 128;		/* Node 0 is always the root */
	
	outbuf = buf2;
	chr = *buf1++;				/* Get a character */
	len--;
	while (1) {
		if (chr & cbit)			/* if the bit is one */
			cnode = huf[cnode + 1];	/* then use the right-hand descendant of this node */
		else
			cnode = huf[cnode];	/* otherwise left */
		if (cnode >= 128) {		/* if descendant >= 128, it's a terminal */
			*outbuf++ = cnode - 128; /* subtract 128, and dump it out. */
			olen++;
			cnode = 0; 
			}
		else				/* otherwise, get the byte offset of the new node */
			cnode = 2 * cnode;			
		cbit = cbit >> 1;		/* and move to the next bit */
		if (!cbit) {			/* If we've used up the whole byte, */
			len--;			/* get another one, if there are any */
			if (!len) break;
			cbit = 128;
			chr = *buf1++; 
			} 
		}
	return(olen);
}

/* Undo the run-length encoding.
   buf1 is input, buf2 is output, len is number of bytes in the input,
   The length of the output is known, since it must be the picture size;
   however, we return it anyway for error checking.

   Currently, we emit 1 byte/pixel, with no row padding.
   [ x is pixels per row, rowBytes is bytes (padded) per row. ]
*/

int uncomp (buf1, buf2, len)  /*, x, rowBytes */
unsigned char *buf1, *buf2;
int len;
/* int x, rowBytes; */
{
	int i, j;
	char chr;
	unsigned char *b2base = buf2;
	
	for (i = 0; i < len; i++) {
		if (buf1[i] < 16)
			/* <16 means this doesn't get changed */
			*buf2++ = chr = buf1[i];
		else {
			/* Otherwise, repeat previous byte n-15 times */
			for (j = 0; j < (buf1[i] - 15); j++)
				*buf2++ = chr;
			} 
		}
	return (buf2 - b2base);
}	

/* Undo the xor step.  
   Buf is input and output; x is picture rowBytes; y is picture y. */
   
void uneor (buf, x, y, step)
unsigned char *buf;
int x,y;
int step;	/* either x or 2x */
{
	int i;

/* xor element from next line with element from this line, 
   store in next line */

	for (i = 0; i < (x * y) - step; i++)
		buf[i + step] ^= buf[i]; 
}	

#endif


