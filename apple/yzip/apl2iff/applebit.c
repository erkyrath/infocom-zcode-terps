/* Read data from a Dazzle Draw file.  The format's kind of strange, and there
   are two different ones:
   If section is true (width and height are supplied),
   this is part of a screen image.  There are two header bytes,
   width and height, followed by data.  The data is in 7-bit bytes (which ignore
   8-bit byte boundaries), stored in reverse order.  Each pixel is four bits.
   One thus gets a 7-bit byte, reverses it, and takes the high four bits.
   The remaining three bits go into the next pixel, along with the high bit
   of the next 7-bit byte after reversing it.  Ain't this just more fun than
   you can count?
   Otherwise, this is interleaved in strange and wondrous ways.  Line 0 starts
   at 0; 1 at 0x400; 2 at 0x800; 3 at 0xc00; etc up through 7.  Line 8 starts at
   0x80, with subsequent numbers up 0x400 from that; line 16 starts at 0x100; and so
   on through line 63.  Line 64 starts at 0x28, and 64-127 are otherwise like 0
   through 63.  Line 128 starts at 0x50.
   Further, there are two sections, of 8K each.  Line 0 actually starts at
   0 and 8192.  The first byte used (where bytes are as above) is from the first
   section; the second from the second section.  The input file will always be
   16384 bytes; a group of three lines uses 120 bytes in each section, leaving
   8 bytes of hole, since groups of three lines physically occupy 128 bytes. */

int reverse_byte(cb)
int cb;
{
  int i, nb = 0, imask = 1, omask = 64;
  for (i = 0; i < 7; i++) {
    if (cb & imask) nb |= omask;
    imask <<= 1;
    omask >>= 1; }
  return(nb); }

parse_apple_file(inbuf, outbuf, width, height, xmin, ymin, xmax, ymax)
unsigned char *inbuf, *outbuf;
int width, height, xmin, ymin, xmax, ymax;
{
  int cbits_left = 0;
  int curbyte, i, j, outbyte, loffs, bank;
  unsigned char *lb1, *lb2;
  if (width > 0) {		/* It's just a file section */
    for (i = 0; i < height; i++) {
      cbits_left = 0;
      for (j = 0; j < width; j++) {
	outbyte = 0;
	if (cbits_left < 4) {	/* Not enough in curbyte for a whole pixel */
	  if (cbits_left > 0)	/* Use what's there */
	    outbyte = curbyte << (4 - cbits_left);
	  curbyte = reverse_byte(*inbuf++);
	  /* Now have a 7-bit byte in curbyte.  Need 4-cbits_left bits
	     from it, leaving 3+cbits_left for the next time. */
	  outbyte = outbyte | (curbyte >> (3 + cbits_left));
	  cbits_left += 3; }
	else {
	  cbits_left -= 4;
	  outbyte = curbyte >> cbits_left; }
	outbyte &= 0xf;
	if ((i >= ymin) && (i <= ymax) && (j >= xmin) && (j <= xmax))
	  *outbuf++ = outbyte; } } }
  else {			/* It's a whole file */
    for (i = 0; i < 192; i++) {
      j = i / 64;
      if (j == 0)
	loffs = 0;
      else if (j == 1)
	loffs = 0x28;
      else
	loffs = 0x50;
      j = i % 64;
      loffs = loffs + ((j / 8) * 0x80);
      loffs = loffs + ((j % 8) * 0x400);
      lb1 = &inbuf[loffs];
      lb2 = &inbuf[loffs + 8192];
      bank = 0;
      cbits_left = 0;
      for (j = 0; j < 140; j++) {
	outbyte = 0;
	if (cbits_left < 4) {
	  if (cbits_left > 0)
	    outbyte = curbyte << (4 - cbits_left);
	  if (bank == 0) {
	    bank = 1;
	    curbyte = reverse_byte(*lb1++); }
	  else {
	    bank = 0;
	    curbyte = reverse_byte(*lb2++); }
	  outbyte = outbyte | (curbyte >> (3 + cbits_left));
	  cbits_left += 3; }
	else {
	  cbits_left -= 4;
	  outbyte = curbyte >> cbits_left; }
	outbyte &= 0xf;
	if ((i >= ymin) && (i <= ymax) && (j >= xmin) && (j <= xmax))
	  *outbuf++ = outbyte; } } } }

