#include <stdio.h> 
#include <ctype.h>
#include <tam.h>		/* contains AT&T 7300 window support */
#include <sys/signal.h>
#include <sys/termio.h>

/*  #include "zipdefs.h"  */

#include <wind.h>
#include <sys/window.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>


extern open();
extern read();
extern fstat();
extern ioctl();

#define THEN

int wfd,			/* file descriptor for graphics window */
    ttyfd,			/* file descriptor for stdio */
    ttysav1, ttysav2;		/* storage of startup tty conditions */

main()
{
    char c, md_getc();
    unsigned short data, xloc, yloc;

    ttyfd = md_ttyini();	/* setup stdio window */
    wfd = md_setup();		/* create a graphics window */

    while ((c = md_getc()) != 'q') {
      data = (c << 8) | c;

      c = md_getc();
      yloc = c - '0';	if (yloc > 9) THEN yloc = 0;	/* row */
      c = md_getc();
      xloc = c - '0';	if (xloc > 9) THEN xloc = 0;	/* col */

      md_showblock(data, yloc, xloc);
      }

    md_ttyres(ttyfd);		/* clean up */
}


#define ORIGIN 0
#define X_CELL 9		/* PC 7300 standard char size */
#define Y_CELL 12

md_showblock(pattern, y, x)
unsigned short pattern, y, x;
{
    unsigned short i, bitmap[3*32];	/* 6 bytes x 32 rows */
    struct urdata ur;
    int err;

    for (i=0; i<3*32; i++)
      bitmap[i] = pattern; 

    ur.ur_srcbase = bitmap;		ur.ur_srcwidth = 4;  /* bytes */
    ur.ur_dstbase = 0;	/* window */	ur.ur_dstwidth = 0;

    ur.ur_srcx = ORIGIN;		ur.ur_srcy = ORIGIN;
    ur.ur_dstx = X_CELL * 4 * x;	ur.ur_dsty = Y_CELL * 2 * y;

    ur.ur_width = X_CELL * 4;		ur.ur_height = Y_CELL * 2; /* pixels */
    ur.ur_srcop = SRCSRC;		ur.ur_dstop = DSTSRC;
    ur.ur_pattern = 0;

    err = ioctl(wfd, WIOCSELECT);	/* switch to graphics window */
    err = ioctl(wfd, WIOCRASTOP, &ur);
    err = ioctl(ttyfd, WIOCSELECT);	/* switch back to text window */
}

char md_getc()		/* read a key */
{
    unsigned char c;
    int err;
    do
      err = read(ttyfd, &c, 1);		/* use the text window */
    while (err <= 0);
    return (c);
}

int md_setup()	/* create a window, return descriptor */
{
    int fd, wn, err;
    struct stat wstat;
    struct uwdata uw;

    fd = open("/dev/window", O_RDWR | O_CREAT);
/*  err = fstat(fd, &wstat);	*/
/*  wn = wstat.st_rdev;		*/	/* window "number", not needed */

    err = ioctl(fd, WIOCGETD, &uw);
    uw.uw_x = 16;			/* initial position & size */
    uw.uw_y = 16;
    uw.uw_width = 512;
    uw.uw_height = 64;
    uw.uw_uflags = BORDRESIZE;		/* make it resizable */

    err = ioctl(fd, WIOCSETD, &uw); 	/* set size & display window */
    err = ioctl(ttyfd, WIOCSELECT);	/* switch back to text window */
    return (fd);
}

int md_ttyini()
{  /* 	This routine performs Unix tty magic.  It sets the input buffer
	length to 0, and turns off canonization and echo. 
   */
    struct termio ttyinfo;
    int fd, err;

    fd = fileno(stdin);		/* get the stdin file descriptor */

    err = ioctl(fd, TCGETA, &ttyinfo);
    if (err == -1) THEN
      printf("\nIOCTL(TCGETA) failed");

    ttysav1 = ttyinfo.c_lflag;
    ttysav2 = ttyinfo.c_cc[VMIN];

    ttyinfo.c_lflag &= ~ICANON;
    ttyinfo.c_lflag &= ~ECHO;	
    ttyinfo.c_cc[VMIN] = 0;

    err = ioctl(fd, TCSETA, &ttyinfo);
    if (err == -1) THEN
      printf("\nIOCTL(TCSETA) failed");    
    return (fd);
}

md_ttyres(fd)
int fd;
{  /* 	This undoes the above magic.
   */
    struct termio ttyinfo;
    int err;

    err = ioctl(fd, TCGETA, &ttyinfo);
    ttyinfo.c_lflag = ttysav1;
    ttyinfo.c_cc[VMIN] = ttysav2;
    err = ioctl(fd, TCSETA, &ttyinfo);

/*  close(fd);	*/
}
