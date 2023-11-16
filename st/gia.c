/*
 * GIA.C
 *   By Alex Leavens
 *
 *  Digitized sound player for the ST
 *
 * Created 10/28/87
 * Revised 10/28/87
 */
 
#include <osbind.h>
#include <xbios.h>

extern void  pump_sound();

#define	BUF_SIZE	64000L

unsigned char input_buffer[BUF_SIZE];
int repeat;

main(argc, argv)
int argc;
char *argv[];
{
    int f_handle;
    unsigned long file_size, i;

    if (argc <= 1)
    {
    	printf("GIA--a digital sound player.  Call with the filename\n");
    	printf("     of the sound to be played.\n");
    	return;
    }
    printf("GIA by Alex Leavens\n(C) Copyright 1987 by Activision\n\n");

    if (argc > 2)
    {
    	repeat = argv[2][0] - '0';
    }
    else
    	repeat = 1;
    
    if ( (f_handle = Fopen(argv[1], 0)) < 0 )
    {
    	printf("Error opening input file: %s\n", argv[1]);
    	exit(-1);
    }

    if ( (file_size = Fread(f_handle, BUF_SIZE, input_buffer)) < 0L )
    {
    	printf("Error on reading input file: %s\n", argv[1]);
    	exit(-2);
    }

    Fclose(f_handle);

	pump_sound( input_buffer );

}
