#include <osbind.h> /* include the important files... */
#include <stdio.h>
#include <fcntl.h>
#include <gemlib.h>
#include <file_sel.inc>

#define SCREEN_SIZE 32000  /* in bytes */

/* globals */

char dumstr[ 20 ], *screen, *buf, command[ 200 ][ 80 ];
char command12[ 200 ][ 12 ], lib_name[ 80 ];
short i, num, x, y, commands = 0, lib_file, temp_file;

/* clear the screen */

cls()
{
  putch( 0x1b );
  putch( 'E' );
}

/* error trap detection to help debug this piece */

quit()
{
  printf( "\nPress <CR> to exit..." );
  gets( dumstr );
  exit();
}

/* get command file name and read picture file names from it; also get lib file name */

read_command_file()
{
  char ch, n, i = 0;
  int fd;

  cls();
  printf( "Select the command file:\n" );
  get_path( pathspec );
  if( _file_select() ) exit();              /* if CANCEL was clicked on, then dump user to desktop */
  fd = open( _fullname, O_RAW );            /* untranslated read */
  if( fd == -1 )                            /* if a problem opening, then quit() */
  {
    printf( "Error opening %s from read_command_file().", _fullname );
    quit();
  }
  do
  {
    n = read( fd, &ch, 1 );                 /* read one char */
    if( ( n != 0 ) && ( ch != 32 ) )        /* if not end of file, and space was not read, ... */
    {
      if( ch == '\n' )                      /* if the <CR> character, ... */
      {
        command[ commands ][ i - 1 ] = 0;   /* set end of string */
                                            /* if not null string, then tell user what was read */
        if( command[ commands][ 0 ] != 0 ) printf( "Just read picture name %d = '%s'.\n", commands, command[ commands ] );
        commands++;                         /* increment commands */
        i = 0;                              /* and set pointer in string to beginning of next command */
      }
      else                                  /* if the char was NOT <CR>, ... */
      {
        command[ commands ][ i ] = ch;      /* add char to string */
        i++;                                /* increment pointer in string */
      }
    }
  } while( n != 0 );                        /* while not end of file */
  commands--;                               /* because there was one extra added - in the terminating loop */
  printf( "Number of picture files is %d\n", commands );
  close( fd );
}

/* find out if all files actually exist */

do_all_exist()
{
  short n;
  int fd;

  for( n = 0; n < commands; n++ )          /* count from 0 to # of commands-1 (Because it starts at zero, not one) */
  {
    fd = open( command [ n ], 0 );
    if( fd == -1 )                         /* if unopenable ... */
    {
      printf( "From do_all_exist(), there was an error finding file '%s'", command[ n ] );
      quit();
    }
    printf( "%s exists\n", command[ n ] ); /* tell user that it does exist */
    close( fd );                           /* and close it; we didn't need it open, anyway - yet! */
  }
}

/* open lib file, then write # of pictures in lib file */

write_n()
{
  lib_file = open( lib_name, 0x8102 ); /* raw, create, write only */
  if( lib_file == -1 )                 /* if unopenable - which could only occur with a full dir or disk not there - ... */
  {
    printf( "Error from write_n(): could not open Lib file '%s'", lib_name );
    quit();
  }
  write( lib_file, &commands, 2 );     /* write # of pictures */
}

/* write zeros up to start of names, then write names */

skip_to_names()
{
  char ch = 0;
  short i;

  for( i = 1; i < 30; i++) write( lib_file, &ch, 1 );             /* 30 bytes after 'n' are reserved */
  for( i = 0; i <= commands * 4; i++ ) write( lib_file, &ch, 1 ); /* set aside 4 * commands bytes ( 2 bytes each for x & y) */
}

/* write the names of all the picture files to the lib file */

write_names()
{
  short i, j, index1, index2;

  for( i = 0; i < commands; i++ )
  {
    index1 = back_search( command[ i ], '\\' );                               /* search for pathnames etc. */
    index2 = back_search( command[ i ], '\:' );
    j = max( index1, index2 );                                                /* j is the furthest into the string of the 2 above */
    strcpy( command12[ i ], &command[ i ][ j ] );                             /* copy the filename.ext ONLY */
    for( j = strlen( command12[ i ] ); j < 12; j++) command12[ i ][ j ] = 32; /* pad with spaces to position 12 */
    write( lib_file, &command12[ i ], 12 );                                   /* write 12 chars to lib file */
    printf( "Wrote picture #%d as '%s'\n", i, command12[ i ] );
  }
}

/* read the picture from temp file */

read_picture()
{
  short n;

  setmem( buf,SCREEN_SIZE,0 );     /* clear the buffer to zero */
  num = x >> 3;                    /* num is # of bytes */
  if( ( x % 8 ) > 0 ) num++;       /* if not on byte boundary, round up */
  if( ( num % 2 ) > 0 ) num++;     /* if not on word boundary, round up */
  num *= ( y + 1 );                /* because in the loop, there is an extra line */
  n = read( temp_file, buf, num ); /* read bytes into buf */
}

/* display the picture on the screen and save it to lib file simultaneously */

display_and_save()
{
  short i = 0, count, xover, wrote;

  xover = num / ( y + 1 );                             /* restore it to the value it was in the above function */
  for( count = 0; count <= y; ++count )
  {
    movmem( &buf[ i ], &screen[ count * 80 ], xover ); /* display */
    wrote = write( lib_file, &buf[ i ], xover );       /* and save */
    if( wrote != xover )                               /* if disk is full, ... */
    {
      printf("The disk is full.  Delete something or replace disk and start over.");
      quit();
    }
    i += xover;                                        /* increment pos in buf by # of bytes */
  }
  count = 0;                                           /* zero to be written if not quad aligned */
  if( ( num % 4 ) > 0 )                                /* if NOT quad aligned ... */
  {
    printf( "Aligning on QUAD boundary...\n" );        /* tell user so */
    write( lib_file, &count, 2 );                      /* write a word */
    num += 2;                                          /* add 2 to # of bytes */
  }
}

/* transfer the pictures from separate files to the lib file */

transfer_pictures()
{
  long offset;

  lseek( lib_file, 32L, 0 );         /* offset must be LONG; 0 = start from beginning */
  for( i = 0; i < commands; i++ )
  {
    temp_file = open( command[ i ], O_RAW );
    if( temp_file == -1 )            /* if unopenable, ... */
    {
      printf( "Error opening file '%s'.", command[ i ]);
      quit();
    }
    read( temp_file, &x, 2 );        /* read x */
    read( temp_file, &y, 2 );        /* and y  */
    write( lib_file, &x, 2 );        /* write x */
    write( lib_file, &y, 2 );        /* and y   */
    read_picture();
    lseek( lib_file, 0L, 2 );        /* seek to: 2 = end of file, 0 = right there */
    display_and_save();
    offset = 32 + ( ( i + 1 ) * 4 ); /* skip 1st 32 bytes (reserved), then 4*i, which sets pointer to 1st byte of i+1 */
    lseek( lib_file, offset, 0 );    /* seek to offset, from beginning of file */
    close( temp_file );
  }
}

/* the controller loop  */

main()
{
  screen = ( char * ) Logbase();
  buf = ( char * ) malloc( SCREEN_SIZE );
  read_command_file();
  do_all_exist();
  printf( "Enter the file name to output the LIBRARY to: " );
  gets( lib_name );
  write_n();
  skip_to_names();
  write_names();
  transfer_pictures();
  close( lib_file );
  cls();
  printf( "Done.  Press <CR> to exit..." );
  gets( dumstr );
}

