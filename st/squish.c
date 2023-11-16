#include <osbind.h> /* include the important files... */
#include <stdio.h>
#include <fcntl.h>
#include <gemlib.h>

#define OUTLINE_CROSSHAIRS 7 /* the mouse pointer */
#define SCREEN_SIZE 32000    /* size of screen memory in bytes */

/* globals */

char *screenadr;
short *screenbuffer;
short x1 = 320, y1 = 200, x2 = 0, y2 = 0, dummy, mouse_block[ 37 ];
short handle, pxyarray[ 8 ];
FDB sdb, sdb2;
char dumstr[ 20 ];

#include <file_sel.inc> /* include file selector by Duncan Blanchard */

/* if not a screen file */

not_a_screen_file()
{
  printf( "the file you selected is not a screen file." );
  exit();
}

/* file select, then open and read.  display codes if errors occur */

loadscreen()
{
  int fd, mode, error;

  get_path( pathspec );
  error = _file_select();
  if( error ) exit();                           /* if CANCEL clicked, then get out */
  fd = open( _fullname, O_RAW );
  mode = read( fd ,screenbuffer ,128 );         /* get rid of 1st 128 bytes */
  if( mode < 128 ) not_a_screen_file();
  mode = read( fd, screenbuffer, SCREEN_SIZE ); /* now read the screen */
  if( mode < SCREEN_SIZE ) not_a_screen_file();
  mode = close( fd );
}

/* display instructions, wait for <CR> */

instructions()
{
  putch( 0x1b );
  putch( 'E' );
  printf( "First, select a file using the file selector box (use mouse to point at files;\n" );
  printf( "  select path to change drive or path), and click on one of the return buttons:\n" );
  printf( "  OK to use the file selected, or Cancel to abort the program.\n\n" );
  printf( "Then, using the mouse as a pointer, draw a box surrounding the area you\n" );
  printf( "  wish to SQUISH.  Point to the upper left corner, and drag to the lower right\n" );
  printf( "  corner.  It works almost the same as the rubber box in TOS, but you may draw\n" );
  printf( "  as many boxes as you want.\n\n");
  printf( "When the box surrounds the correct area, hold down the right button and tap the\n" );
  printf( "  left button.  This will allow you to save the file.\n\n");
  printf( "Press <CR> to continue..." );
  gets( dumstr );
  putch( 0x1b );
  putch( 'E' );
}

/* save the screen to buffer, then to disk */

savescreen()
{
  short y, i = 2, n;
  char filename[ 200 ];
  int fd, mode;

  screenbuffer[ 0 ] = x2;      /* set 1st word to x (in pixels) */
  screenbuffer[ 1 ] = y2;      /* set 2nd words to y */
  n = x2 >> 3;                 /* # of bytes to move per line (x2/8) */
  if( ( x2 % 8 ) > 0 ) n++;    /* if not on byte boundary, round up */
  if( ( n % 2 ) == 1 ) n++;    /* if not on word boundary, round up */
  for( y = 0; y <= y2; ++y )   /* loop # of lines down */
  {
    movmem( &screenadr[ y * 80 ], &screenbuffer[ i ], n );  /* move a line */
    setmem( &screenadr[ y * 80 ], n, 255 );                 /* clear the line on screen */
    i += ( n / 2 );                                         /* increment i by n/2 words (or n bytes) */
  }
  v_show_c( handle, 0 );                                    /* show the cursor, who cares how many times it's been hidden */
  printf( "X = %d, Y = %d\n\nType the filename to save as: ", x2, y2 );
  gets( filename );
  fd = open( filename, 0x8102 );                            /* raw, create, write only */
  mode = write( fd, screenbuffer, i * 2 );                  /* save the picture (i is words, so i*2 is bytes) */
  if( mode != i * 2 ) printf( "%d bytes written to the file; %d should have been.\n", mode, i * 2 );
  mode = close( fd );
}

/* if error in malloc(), do this... */

die()
{
  printf( "Your computer does not have enough memory to run this program.\n" );
  printf( "Press <CR> to exit..." );
  gets( dumstr );
  exit();
}

/* save the defined block */

save_block()
{
  v_hide_c( handle );                                /* hide the cursor so it won't interfere with the picture */
  sdb.fd_addr = 0;                                   /* screen is 0 */
  sdb2.fd_addr = screenbuffer;                       /* second Screen Definiton Block is the buffer */
  sdb2.fd_w = 640;                                   /* width */
  sdb2.fd_h = 400;                                   /* height */
  sdb2.fd_wdwidth = 40;                              /* width in words */
  sdb2.fd_stand = 0;                                 /* I don't know; it has to be 0 */
  sdb2.fd_nplanes = 1;                               /* only one plane for monochrome */
  pxyarray[ 0 ] = x1;  pxyarray[ 1 ] = y1;           /* upper left corner on screen */
  pxyarray[ 2 ] = x1 + x2;  pxyarray[ 3 ] = y1 + y2; /* lower right */
  pxyarray[ 4 ] = 0;   pxyarray[ 5 ] = 0;            /* upper left corner for buffer */
  pxyarray[ 6 ] = x2;  pxyarray[ 7 ] = y2;           /* lower right */
  setmem( screenbuffer, SCREEN_SIZE , 0 );           /* clear screenbuffer */
  vro_cpyfm( handle, 3, pxyarray, &sdb, &sdb2 );     /* copy the picture */
  movmem( screenbuffer, screenadr, SCREEN_SIZE );    /* copy the whole buffer to the screen */
  savescreen();                                      /* save it to disk */
}

/* have user define a rectangle with the mouse */

get_rect()
{
  short button2, x, y, box_array[ 10 ];

  do
  {
    box_array[ 0 ] = x1; box_array [ 1 ] = y1;                /* upper left */
    box_array[ 2 ] = x1 + x2; box_array [ 3 ] = y1;           /* upper right */
    box_array[ 4 ] = x1 + x2; box_array [ 5 ] = y1 + y2;      /* lower right */
    box_array[ 6 ] = x1; box_array [ 7 ] = y1 + y2;           /* lower left */
    box_array[ 8 ] = x1; box_array [ 9 ] = y1;                /* upper left */
    v_pline( handle, 5, box_array );                          /* draw the box */
    dummy = evnt_button( 1, 1, 1, &x, &y, &button2, &dummy ); /* wait for left button */
    movmem( screenbuffer, screenadr, SCREEN_SIZE );           /* rewrite screen */
    if( ( button2 & 1) && ( ( button2 & 2) != 2 ) )           /* if button 1 was pressed, but not button 2 */
    {
      x1 = x;                                                 /* reset x1 & y1 */
      y1 = y;
      dummy = graf_rubberbox( x1, y1, 5, 5, &x2, &y2 );       /* set x2 & y2 w/ rubberbox function */
    }
  } while( ( button2 & 2 ) != 2 );                            /* while button 2 is NOT hit */
}

/* set up, read screen, display it */

setup_screen()
{
  instructions();                                        /* give instructions to the user */
  loadscreen();                                          /* get a screen from disk - created with PAINTWORKS by Activision */
  dummy = graf_mouse( OUTLINE_CROSSHAIRS, mouse_block ); /* set pointer to crosshairs */
  screenadr = ( char * ) Logbase();                      /* get screen address */
  movmem( screenbuffer, screenadr, SCREEN_SIZE );        /* copy buffer to screen */
}

/* initialize the buffer array */

init()
{
  screenbuffer = ( short * ) malloc( SCREEN_SIZE );       /* grab 32000 bytes */
  if( screenbuffer == NULL ) die();                       /* if error getting 32000 bytes, then die */
  handle = graf_handle( &dummy, &dummy, &dummy, &dummy ); /* get graphics handle (used by VDI) */
}

/* controller loop */

main()
{
  init();         /* set up the buffer and handle */
  setup_screen(); /* also loads the screen from disk */
  get_rect();     /* have user define a block with the mouse */
  save_block();   /* save block to disk (user supplies name) */
}

