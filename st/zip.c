
/*------------------------------------------------------*/
/*	ATARI ST ZIP/EZIP INTERFACE			*/
/*------------------------------------------------------*/

#include "define.h"
#include "portab.h"
#include "vdibind.h"
#include "gemdefs.h"
#include "obdefs.h"
#include "osbind.h"

/*	now use gemdos(), xbios(), etc, in osbind.h ...
EXTERN	LONG	trap1 ();	GEMDOS interface
EXTERN	LONG	trap2 ();	DOS extensions (GEM, GSX)
EXTERN	LONG	trap13 ();	BIOS
EXTERN	LONG	trap14 ();	Atari BIOS extensions
*/

/************************************************************************/
/*	Global Variables						*/
/************************************************************************/

/*				now defined in Lattice
WORD	contrl[16];
WORD	intin[256];	/-* must be large for v_gtext *-/
WORD	ptsin[256];
WORD	intout[256];
WORD	ptsout[256];
*/

WORD	handle;
WORD	y_res, y_char;		/* rows = y_res / y_char */
WORD	x_res, x_char;

WORD	rows, cur_row;		/* cur_row in 0 .. rows-1 */
WORD	columns, cur_column;

WORD	split_row;		/* first row in scrolling area */

WORD	v_inverse = 0;		/* inverse video flag */
WORD	v_italic = 0;		/* flag for italics kludge */

MFDB	scr_mfdb, bak_mfdb;
MFDB	*scr_ptr, *bak_ptr;	/* current screen MFDB, backup MFBD */
MFDB	*org_ptr;		/* original screen MFDB */

char	pathspec[64];	/* buffers for file select dialog */
char	filename[16];
char	fullname[80];	/* combined buffer */

char	pathback[64];		/* buffers for old default names */
char	fileback[16];

WORD	w_handle;

WORD	max_index;		/* number of available color indices */
WORD	v_fore = 1;		/* text color */
WORD	v_back = 0;		/* all other colors */
WORD	reversed = 0;		/* reversed video? (MONO ONLY) */

WORD	color_table[48] = 	/* defaults for first 16 color indices */
{
	0x03E8, 0x03E8, 0x03E8, 	/* white */
	0x0000, 0x0000, 0x0000,		/* black */

	0x03E8, 0x0000, 0x0000,		/* red */
	0x0000, 0x03E8, 0x0000,		/* green */
	0x0000, 0x0000, 0x03E8,		/* blue */

	0x0000, 0x03E8, 0x03E8, 	/* cyan */
	0x03E8, 0x03E8, 0x0000, 	/* yellow */
	0x03E8, 0x0000, 0x03E8, 	/* magenta */

	0x02AF, 0x02AF, 0x02AF, 	/* white */
	0x0138, 0x0138, 0x0138, 	/* black */

	0x003E, 0x03A9, 0x03A9, 	/* light red */
	0x01AC, 0x03E8, 0x01AC, 	/* light green */
	0x03A9, 0x003E, 0x03A9, 	/* light blue */

	0x02AF, 0x003E, 0x02AF, 	/* light cyan /
	0x03A9, 0x03A9, 0x003E, 	/* light yellow */
	0x02AF, 0x02AF, 0x003E		/* light magenta */
};

WORD	ms_tick;	/* millisecs per timer tick */
WORD	ms_total;	/* millisecs since cursor was turned on */
WORD	curs_on;	/* set if currently showing */

LONG	curs_addr;	/* our cursor routine (68K) */
LONG	otim_addr;	/* previous interrupt timer routine */

LONG	*hz_200 = (LONG *)0x04BA;	/* 200 hz timer in low RAM */

/************************************************************************/
/*	Utility Routines						*/
/************************************************************************/

char *zalloc(len)
int len;
{	/* [must use GEMDOS allocator, not library malloc(), to match 68K] */

	return(gemdos (0x48, len));
}

/*------------------------------*/
/*	grect_to_array		*/
/*------------------------------*/
VOID 
grect_to_array (area, array)    /* convert x,y,w,h to upper left x,y and */
				/*                    lower right x,y	 */
GRECT	*area;
WORD	*array;
{
	array[0] = area->g_x;
	array[1] = area->g_y;
	array[2] = area->g_x + area->g_w - 1;
	array[3] = area->g_y + area->g_h - 1;
}

/*------------------------------*/
/*	rc_copy			*/
/*------------------------------*/
VOID
rc_copy (psbox, pdbox)		/* copy source to dest rectangle */

GRECT	*psbox;
GRECT	*pdbox;
{
	pdbox->g_x = psbox->g_x;
	pdbox->g_y = psbox->g_y;
	pdbox->g_w = psbox->g_w;
	pdbox->g_h = psbox->g_h;
}

/************************************************************************/
/*	Low-Level VDI and BIOS						*/
/************************************************************************/

/*------------------------------*/
/*	swap_screen		*/
/*------------------------------*/
VOID
swap_screen ()		/* change to alternate screen buffer */
{
	MFDB	*tmp_ptr;

	xbios(5, bak_ptr->mp, bak_ptr->mp, -1);	/* Setscreen */

/* then wait for next vblank interrupt */
/*	xbios(36);	*/		/* BUG */
/*	xbios(37);	*/		/* Vsync! New and correct # */

	tmp_ptr = scr_ptr;
	scr_ptr = bak_ptr;	/* alternate buffer is now primary */
	bak_ptr = tmp_ptr;	/* primary buffer is now alternate */
}

/*------------------------------*/
/*	init_colors		*/
/*------------------------------*/
VOID 
init_colors (read)	/* read system color map into our table,  */
			/*	or write table back to system map */
WORD	read;
{
	WORD	i, actual;
	actual = 0;		/* return color values requested */

	for (i=0; i<max_index; i++)
	{
		if (read)
			vq_color (handle, i, actual, &color_table[3*i]);
		else	vs_color (handle, i, &color_table[3*i]);
	}
}

/*------------------------------*/
/*	remap_colors		*/
/*------------------------------*/
#define	maxcolor  7	/* not too few, not too many */
#define	mincolor  0

VOID 
remap_colors ()		/* re-map the (first two) color representations */
{
	WORD	i;

	if (max_index == 2)	/* mono display? */
		{
		if (reversed)		/* switch it */
			reversed = 0;
		else	reversed = 1;

		v_back = reversed;	/* if reversed, back becomes fore */

		if (v_back)
			v_fore = 0;
		else	v_fore = 1;
		}
	else			/* colors, make them wrap around */
		{
		if (v_fore > maxcolor) v_fore = 0;
		if (v_fore < 0) v_fore = maxcolor;
	
		if (v_back > maxcolor) v_back = 0;
		if (v_back < 0) v_back = maxcolor;
		}

	vs_color (handle, 1, &color_table[3*v_fore]);
	vs_color (handle, 0, &color_table[3*v_back]);

	for (i=2; i<max_index; i++)	/* map everything else to background */
	{
		vs_color (handle, i, &color_table[3*v_back]);
	}
}

/*------------------------------*/
/*	flip_video		*/
/*------------------------------*/
VOID 
flip_video ()		/* reverse the (first two) color representations */
{
	WORD	fore_rgb[3];
	WORD	back_rgb[3];

	vq_color (handle, 1, 0, fore_rgb);	/* get foreground color */
	vq_color (handle, 0, 0, back_rgb);	/* get background color */

	vs_color (handle, 1, back_rgb);		/* switch them */
	vs_color (handle, 0, fore_rgb);
}

/*------------------------------*/
/*	highlight		*/
/*------------------------------*/
VOID
highlight (mode)

WORD	mode;
{
	switch (mode)
	{
	case 0:			/* normal text */
		v_inverse = 0;
		v_italic = 0;
		vst_effects (handle, 0);
		break;
	case 1:			/* inverse video */
		v_inverse = 1;
		break;
	case 2:			/* bold */
		vst_effects (handle, 1);
		break;
	case 4:			/* italic (underline) */
		v_italic = 1;
		vst_effects (handle, 4);
		break;
	}
}

/************************************************************************/
/*	Raster Operations						*/
/************************************************************************/

/*------------------------------*/
/*	rast_op			*/
/*------------------------------*/
VOID
rast_op (mode, s_area, s_mfdb, d_area, d_mfdb)	/* bit block transfer */

WORD	mode;
GRECT	*s_area, *d_area;
MFDB	*s_mfdb, *d_mfdb;
{
	WORD	pxy[8];

	grect_to_array (s_area, &pxy[0]);
	grect_to_array (d_area, &pxy[4]);

	vro_cpyfm (handle, mode, pxy, s_mfdb, d_mfdb);
}

/*------------------------------*/
/*	bit_copy		*/		/* all units in pixels */
/*------------------------------*/
VOID
bit_copy (first, last, delta, old_mfdb, new_mfdb)

WORD	first, last, delta;		/* negative delta means UP */
MFDB	*old_mfdb, *new_mfdb;
{
	GRECT	old_area, new_area;

	old_area.g_x = 0;		old_area.g_w = x_res;
	old_area.g_y = first;		old_area.g_h = last - first;

	new_area.g_x = 0;		new_area.g_w = x_res;
	new_area.g_y = first + delta;	new_area.g_h = last - first;

	rast_op (3, &old_area, old_mfdb,
			&new_area, new_mfdb);
}

/*------------------------------*/
/*	clear_chars		*/
/*------------------------------*/
VOID
clear_chars (row1, row2, column1, column2, dst_mfdb)	/* clear given area */

WORD	row1, row2, column1, column2;
MFDB	*dst_mfdb;
{
	GRECT	clear_area;
	WORD	mode;

	clear_area.g_x = column1 * x_char;
	clear_area.g_w = (column2 - column1) * x_char;

	clear_area.g_y = row1 * y_char;
	clear_area.g_h = (row2 - row1) * y_char;

	if (v_inverse) 
		mode = 15;	/* fill with foreground color */
	else mode = 0;		/* fill with background color */

	rast_op (mode, &clear_area, dst_mfdb,
			&clear_area, dst_mfdb);
}

/************************************************************************/
/*	Screen Handling							*/
/************************************************************************/

/*------------------------------*/
/*	clear_eol		*/
/*------------------------------*/
VOID
clear_eol ()
{
	clear_chars (cur_row, cur_row + 1,
			cur_column, columns, scr_ptr);
}

/*------------------------------*/
/*	clear_lines		*/
/*------------------------------*/
VOID
clear_lines (row1, row2)
{
	clear_chars (row1, row2, 0, columns, scr_ptr);
}

/*------------------------------*/
/*	clear_screen		*/
/*------------------------------*/
VOID
clear_screen ()
{
	clear_lines (0, rows);
}

/*------------------------------*/
/*	line_scroll		*/
/*------------------------------*/
VOID
line_scroll ()
{
	WORD	first, last;	/* actually last +1 */
	WORD	delta;		/* delta is always one line up */

	first = split_row + 1;	last = rows;
	delta = y_char;

/* (for smoother scrolling, decrease delta and loop) */

/*	bit_copy ((first * y_char), (last * y_char), -delta, 
			scr_ptr, bak_ptr);
	clear_chars (last - 1, last, 0, columns, bak_ptr);
*/
	scan_copy ((first * y_char), (last * y_char), -delta, 
			scr_ptr, scr_ptr);

	clear_chars (last - 1, last, 0, columns, scr_ptr);

/* also copy over window 1, with no displacement ... */

/*	bit_copy (0, (split_row * y_char), 0, scr_ptr, bak_ptr);
	swap_screen ();
*/
}

/*------------------------------*/
/*	line_out		*/
/*------------------------------*/

VOID
line_out (string, len)	/* draw a string at current position */
			/* check length and clip if required (no folding) */
char	*string;
WORD	len;
{
	char	old_endmark;
	WORD	x_align, y_align;
	WORD	clip_len;

/* If italics in column 0, must kluge some extra room, or the first char
   gets clipped off completely.  [The 68K screen folding routine routinely
   uses one less column than is available, for this reason.]
*/
	if (v_italic && (!cur_column))		/* start drawing in column 1 */
		cur_column = 1;

/* If the entire string won't fit at the current position, for any reason,
   clip it down to size.
*/
	clip_len = columns - cur_column;	/* max allowed string length */
	if (len < clip_len)
		clip_len = len;			/* length to actually draw */

/* Make the string ASCIZ.  Buffer output is not given in this form, and 
   in fact may be immediately followed by a character that will ultimately 
   appear at the start of the next line.
*/
	old_endmark = string[clip_len];		/* save this byte */
	string[clip_len] = 0;			/* null-terminate the string */

	x_align = x_char * cur_column;
	y_align = y_char * cur_row;

	if (v_inverse)	/* draw string, then background */
		{
		vswr_mode (handle, 2);		/* transparent mode */
		vst_color (handle, 0);		/* background color */
		v_gtext (handle, x_align, y_align, string);

		vswr_mode (handle, 4);		/* reverse transparent mode */
		vst_color (handle, 1);		/* foreground color */
		v_gtext (handle, x_align, y_align, string);

		vswr_mode (handle, 1);		/* replace mode */
		}
	else
		v_gtext (handle, x_align, y_align, string);

	cur_column = cur_column + clip_len;
	string[clip_len] = old_endmark;		/* restore this byte */
}

/*------------------------------*/
/*	char_out		*/
/*------------------------------*/

VOID
char_out (the_char)	/* if CR or BS, special case, else draw the char */

char	the_char;
{
	char	char_buffer[2];

	switch (the_char)
	{
	case 13:	/* zero col, next row, scroll if off bottom */

		cur_column = 0;
		cur_row++;

		if (cur_row == rows)
			{
			line_scroll ();
			cur_row--;
			}
		break;

	case 8:		/* dec col, write a space, dec col */

		if (cur_column == 0)
			{
			cur_column = columns - 1;
			cur_row--;
			}
		else
			cur_column--;

		char_out (32);		/* do the space */

/*		if (cur_column == 0)
			{
			cur_column = columns - 1;
			cur_row--;
			}
		else
*/			cur_column--;
		break;

	default:	/* check col, next line if required, output chr */

		if (cur_column == columns)
			char_out (13);

		char_buffer[0] = the_char;
		line_out (&char_buffer, 1);	/* draw the char */

		break;
	}
}

/*------------------------------*/
/*	blink_cursor		*/
/*------------------------------*/
VOID 
blink_cursor ()		/* duty cycle is 66% */

{
	if (ms_total >= 1000)	/* fall through, start new cycle */
		ms_total = 0;

	if ((ms_total >= 666) && curs_on)	/* remove the cursor */
		{
		curs_on = 0;
		char_out (32); char_out (8);
		}

	if ((ms_total < 666) && !curs_on)	/* show a cursor */
		{
		curs_on = 1;
/*		char_out (95); cur_column--; */	/* UNDERSCORE */

		char_out (32);

		if (v_inverse)
			v_inverse = 0;
		else	v_inverse = 1;

		char_out (8);

		if (v_inverse)
			v_inverse = 0;
		else	v_inverse = 1;
		}

/*	ms_total = ms_total + ms_tick;	*/	/* update timer */
}

/*------------------------------*/
/*	char_in			*/
/*------------------------------*/
char
char_in ()	/* get a char, show blinking cursor while waiting */
{
	LONG	the_char;
	LONG	temp_addr;
	LONG	init_tick, curr_tick;

	WORD	ev_mwhich, junk;

	FOREVER
	{
	ms_total = 0;	curs_on = 0;
	vex_timv (handle, curs_addr, &otim_addr, &ms_tick);

/*	init_tick = *hz_200;	*/	/* read system timer */
	do
		{
/*		curr_tick = *hz_200;
		ms_total = curr_tick - init_tick;
		ms_total = (ms_total % 200) * 5;
*/
		blink_cursor ();
		the_char = gemdos(0x06, 0xFF); /* get raw keyboard input */
		}
	while (the_char == 0); 

/*	do
	{
		ev_mwhich = evnt_multi (1,0,0,0,
					  0,0,0,0,0,
					  0,0,0,0,0,
					  &junk,0,0,

					  &junk,&junk,&junk,&junk,
					  &the_char, &junk);
	}
	while (ev_mwhich != 1); 
*/
/*	the_char = evnt_keybd ();	*/

	vex_timv (handle, otim_addr, &temp_addr, &ms_tick);

/* make sure we leave with cursor off */

	ms_total = 999;   curs_on = 1;
	blink_cursor ();

	if (the_char == 0x4D0036)		/* right arrow */
		v_fore++;
	else if (the_char == 0x4B0034)		/* left arrow */
		v_fore--;
	else if (the_char == 0x480038)		/* up arrow */
		v_back++;
	else if (the_char == 0x500032)		/* down arrow */
		v_back--;
	else
		return (the_char);	/* exit with result */

	remap_colors ();

	} /* end FOREVER */
}

/************************************************************************/
/*	Status Display							*/
/************************************************************************/

/*------------------------------*/
/*	show_status		*/
/*------------------------------*/
VOID 
show_status (location, loc_len, score, score_len)

char	*location;
char	*score;
WORD	loc_len, score_len;
{
	WORD	old_row, old_column;
	WORD	loc_end, score_start;	/* column offsets */
	WORD	old_inverse;

/* switch temporarily to inverse video */

	old_inverse = v_inverse;
	v_inverse = 1;

	/* vst_effects (handle, 8); */	/* underlined text */

	old_row = cur_row;		/* remember cursor */
	old_column = cur_column;

/* draw first string, clear middle area */

	cur_row = cur_column = 0;	/* reset cursor to top */
	line_out (location, loc_len);

	loc_end = loc_len;
	score_start = columns - score_len;
	clear_chars (0, 1, loc_end, score_start, scr_ptr);

/* draw second string */

	cur_column = score_start;
	line_out (score, score_len);

/* turn off inverse video */

	v_inverse = old_inverse;

	/* vst_effects (handle, 0); */	/* no more underline */

	cur_row = old_row;		/* restore former cursor */
	cur_column = old_column;
}


/************************************************************************/
/*	File Selection							*/
/************************************************************************/

/*------------------------------*/
/*	get_path		*/
/*------------------------------*/

VOID
get_path (init_path)		/* return default directory pathname */

char	*init_path;
{
	WORD	init_drive;

	init_drive = gemdos(0x19);	/* Dgetdrv() */

	init_path[0] = init_drive + 'A';
	init_path[1] = ':';
/*	init_path[2] = '\0';	*/

	gemdos(0x47, &init_path[2], init_drive + 1);	/* Dgetpath */
	strcat (init_path, "\\");
}

/*------------------------------*/
/*	back_search		*/
/*------------------------------*/
WORD
back_search (string, target_char)	/* return length of string through
					     given char, zero if not found */
char	*string;
char	target_char;
{
	WORD	index;

	index = strlen (string);
	while (index && (string[index-1] != target_char))
		{
		index--;
		}
	return (index);
}

/*------------------------------*/
/*	add_fname		*/
/*------------------------------*/
VOID
add_fname (pathspec, filename)

char	*pathspec, *filename;
{
	WORD	index1, index2;

	index1 = back_search (pathspec, '\\');	/* any subdirectories? */
	index2 = back_search (pathspec, '\:');

	if (index1 > index2)			/* chop off any filespec */
		pathspec[index1] = '\0';
	else	pathspec[index2] = '\0';

	strcat (pathspec, filename);		/* append this filename */
}

/*------------------------------*/
/*	add_ftype		*/
/*------------------------------*/
VOID
add_ftype (fullname, filetype)

char	*fullname, *filetype;
{
	WORD	index;

	index = back_search (fullname, '\.');	/* any filetype? */
	if (index)
		fullname[index-1] = '\0';	/* if so, chop it off */
	strcat (fullname, filetype);		/* append our filetype */
}

/*------------------------------*/
/*	file_select		*/
/*------------------------------*/
WORD
file_select (opsave, drive)	/* get file and pathspec from user */

WORD	opsave;		/* non-zero if save, not restore */
char	drive;		/* drive to use, zero if default */
{
	WORD	button, error;

/*	if (drive)
		{
		pathspec[0] = drive;	pathspec[1] = ':';
		pathspec[2] = '\\';	pathspec[3] = '\0';
		strcat (pathspec, "*.SAV");
		}
*/
	scan_copy (0, y_res, 0, scr_ptr, bak_ptr);	/* backup screen */

	if (opsave)			/* no default, */
		filename[0] = '\0';	/* type in new name OR click on old */

	v_show_c (handle, 0);
	error = fsel_input (&pathspec, &filename, &button);
	v_hide_c (handle);

	scan_copy (0, y_res, 0, bak_ptr, scr_ptr);	/* restore screen */

	if (((error == 0) || (button == 0)) 
			  || (strlen (filename) == 0))
		return (1);				/* failed */

	add_ftype (filename, ".SAV");		/* force type to be SAV */

	strcpy (fullname, pathspec);		/* dest <- source */
	add_fname (fullname, filename);		/* create full name */

/* if about to SAVE ... */

/*	() check if this file already exists  (query_file)
	() if so, ask user for Abort or Proceed		*/

	return (0);	/* okay */
}

/*------------------------------*/
/*	new_default		*/
/*------------------------------*/

VOID
new_default (okay)	/* called at end of each SAVE/RESTORE */

WORD	okay;
{
	if (okay)	/* dest <- source */
		{
		strcpy (pathback, pathspec);	/* update old defaults */
		strcpy (fileback, filename);
		}
	else
		{
		strcpy (pathspec, pathback);	/* retrieve old defaults */
		strcpy (filename, fileback);
		}
}

/************************************************************************/
/*	Disk I/O							*/
/************************************************************************/

#define	 dos_create	0x3C
#define	 dos_open	0x3D

#define	 dos_lseek	0x42
#define	 dos_read	0x3F
#define	 dos_write	0x40

#define	 dos_close	0x3E
#define	 dos_delete	0x41	/* "unlink" */


/*------------------------------*/
/*	read_file		*/
/*------------------------------*/

LONG
read_file (channel, offset, length, buffer)

WORD	channel;
LONG	offset, length;
char	*buffer;
{
	LONG	result;

	v_show_c (handle, 0);
	result = gemdos (dos_lseek, offset, channel, 0);

	if (result != offset)		/* seek error */ 
		result = -99;
	else	result = gemdos (dos_read, channel, length, buffer);
	v_hide_c (handle);

	if (result < 0) 
		return (result);	/* read error (ignore EOF error) */
	else	return (0);		/* otherwise no error */
}


/*------------------------------*/
/*	write_file		*/
/*------------------------------*/

LONG
write_file (channel, offset, length, buffer)

WORD	channel;
LONG	offset, length;
char	*buffer;
{
	LONG	result;

	v_show_c (handle, 0);
	result = gemdos (dos_lseek, offset, channel, 0);

	if (result != offset) 		/* seek error */
		result = -99;
	else	result = gemdos (dos_write, channel, length, buffer);
	v_hide_c (handle);

	if (result != length)
		return (result);	/* write error, positive means EOF */
	else	return (0);		/* otherwise no error */
}


/*------------------------------*/
/*	create_file		*/
/*------------------------------*/

LONG
create_file ()		/* create AND OPEN file, return channel */
{
	LONG	error;

	v_show_c (handle,0);
	error = gemdos (dos_create, fullname, 0);
	v_hide_c (handle);

	return (error);
}


/*------------------------------*/
/*	open_file		*/
/*------------------------------*/

LONG
open_file ()
{
	LONG	error;

	v_show_c (handle,0);
	error = gemdos (dos_open, fullname, 2);		/* READ/WRITE */
	v_hide_c (handle);

	return (error);
}


/*------------------------------*/
/*	close_file		*/
/*------------------------------*/

LONG
close_file (channel)

WORD	channel;
{
	LONG	error;

	v_show_c (handle,0);
	error = gemdos (dos_close, channel);
	v_hide_c (handle);

	return (error);
}


/*------------------------------*/
/*	delete_file		*/
/*------------------------------*/

LONG
delete_file ()
{
	LONG	error;

	v_show_c (handle,0);
	error = gemdos (dos_delete, fullname);
	v_hide_c (handle);

	return (error);
}


/*------------------------------*/
/*	open_game		*/
/*------------------------------*/

LONG
open_game()
{
	LONG	f_handle;

	strcpy (fullname, "STORY.DAT");		/* always called this */
	f_handle = open_file ();

	fullname[0] = 0;
	return (f_handle);
}

/*------------------------------*/
/*	close_game		*/
/*------------------------------*/

LONG
close_game (f_handle)

WORD	f_handle;
{
	return (close_file (f_handle));
}


/************************************************************************/
/*	Initializations							*/
/************************************************************************/

/*------------------------------*/
/*	z_init			*/
/*------------------------------*/
WORD
z_init ()
{
	WORD	work_in[20], work_out[100], extnd_out[100];
	WORD	attrib[10], gr_cell[4];

	WORD	hor_out, vert_out;	/* dummy */
	WORD	i, apid;
	WORD	planes;			/* color planes */
	LONG	buff_size, buff_loc;

	WORD	wi[4], wo[4];
	WORD	wind_type;

/* Set the system up to do AES calls */

	apid = appl_init();	/* returns ap_id handle, IGNORE FOR NOW */

	if (apid == -1)
		return (4);

/* Get the VDI handle for the currently opened workstation */

	handle = graf_handle (&gr_cell[0],&gr_cell[1],
			      &gr_cell[2],&gr_cell[3]);

	x_char = gr_cell[0];	/* initialize char cell sizes */
	y_char = gr_cell[1];

/* Open the VIRTUAL SCREEN workstation */

	for (i=0; i<10; i++)
	{
		work_in[i] = 1;
	}
	work_in[10] = 2;	/* use RC units -- raster */

	v_opnvwk(work_in, &handle, work_out);	/* returns NEW handle */

	x_res = work_out[0] + 1;
	y_res = work_out[1] + 1;

/*	(( GRECT	scr_area; ))

	scr_area.g_x = 0;	scr_area.g_y = 0;
	scr_area.g_w = x_res;	scr_area.g_h = y_res;
*/

	graf_mouse (0);
	wind_type = 0x002b;	/* name, size, move, close */

/* request size of desktop window */
	wind_get (0,4, &wi[0],&wi[1],&wi[2],&wi[3]);

/* calculate size of work area */
	wind_calc (1,wind_type,wi[0],wi[1],wi[2],wi[3],
				&wo[0],&wo[1],&wo[2],&wo[3]);

/* make window of the max size */
	w_handle = wind_create (wind_type,wi[0],wi[1],wi[2],wi[3]);

/*	wind_set (w_handle,2," SAMPLE",0,0);
	wind_open (w_handle,,wi[0],wi[1],wi[2],wi[3]);	*/

	vq_extnd (handle, 1, extnd_out);	/* extra info */

	planes = extnd_out[4]; 	max_index = 1;
	for (i=0; i<planes; i++)
	{
		max_index = max_index<<1;	/* 2, 4, or 16 */
	}

/* set up the primary MFDB */

	scr_mfdb.fwp = x_res;
	scr_mfdb.fww = x_res>>4;	/* word width */
	scr_mfdb.fh = y_res;

	scr_mfdb.np = extnd_out[4];	/* screen planes */
	scr_mfdb.ff = 0;		/* form format is ATARI specific */

/*	scr_mfdb.mp = 0x0L; */
	scr_mfdb.mp = xbios(2);		/* get screen phys base addr */

	org_ptr = scr_ptr = &scr_mfdb;

/* set up the secondary MFDB */

	bak_mfdb.fwp = x_res;
	bak_mfdb.fww = x_res>>4;	/* word width */
	bak_mfdb.fh = y_res;

	bak_mfdb.np = extnd_out[4];	/* screen planes */
	bak_mfdb.ff = 0;		/* form format is ATARI specific */

	buff_size = (LONG)(x_res>>3) * (LONG)(y_res) * (LONG)(extnd_out[4]);
	buff_size = buff_size + 256;	/* ATARI only */

	buff_loc = zalloc(buff_size);
	if (buff_loc == 0)
		return (2);

	buff_loc = (buff_loc & 0xFFFFFF00) + 256;	/* ATARI - round up */

	bak_mfdb.mp = buff_loc;
	bak_ptr = &bak_mfdb;

	vqt_attributes (handle, attrib);
/*	x_char = attrib[8];		 char cell width of 6 -- WRONG!
	y_char = attrib[9];
*/
	columns = x_res / x_char;
	rows = y_res / y_char;	

	cur_column = 0; 
	split_row = 1;		/* status line is row 0, for now */
	cur_row = rows - 1;

/* set graphic text alignment -- to left and top */

	vst_alignment (handle, 0, 5, &hor_out, &vert_out);

	v_hide_c (handle);	/* hide the cursor */
	clear_screen ();	/*   THEN clear the screen */

	get_path (pathspec);		/* get default pathname */
	strcat (pathspec, "*.SAV");	/* add the spec */

	filename[0] = 0;
	fullname[0] = 0;
	new_default (1);	/* initialize backup default files */

/* Read system color map into our table, 
   THEN map all unused colors to background */

	init_colors (1);

	if (max_index > 2)	/* not needed if mono */
		remap_colors ();

	return (0);
}	

/*------------------------------*/
/*	z_exit			*/
/*------------------------------*/

VOID
z_exit()
{
/*	if (scr_ptr != org_ptr)
		swap_screen ();	*/	/* restore original screen base */

	clear_screen ();	/* clear screen, THEN ... */
	init_colors (0);	/* restore original system color table */

/*	wind_close (w_handle); */
	wind_delete (w_handle);

	v_show_c (handle,0);
	v_clsvwk (handle);	/* close VIRTUAL work station */

	appl_exit ();		/* release AES calls */

/*	_exit (0);	*/	/* return to controlling process */
}

/************************************************************************/
/*	Diagnostics (Temporary)						*/
/************************************************************************/

#define DIAGNOSTIC 0		/* turn on to include this code */
#if DIAGNOSTIC

/*------------------------------*/
/*	link_test		*/
/*------------------------------*/

VOID
link_test()
{
	char	name[100];
	WORD	f_handle;		/* should be LONG ? */
	WORD	dummy;

	strcpy (name, "DBB.TST");

	f_handle = gemdos (0x3C, &name, 0);	/* CREATE */
	f_handle = gemdos (0x3D, &name, 2);	/* OPEN, read/write */

	gemdos (0x40, f_handle, 0x3L, &name);	/* WRITE 3 bytes */
	gemdos (0x3E, f_handle);			/* CLOSE */
}

/*------------------------------*/
/*	z_test			*/
/*------------------------------*/

z_test()
{
	char	mychr;
	char	mystr[100];	/* primitive, must be lower case! */

	FOREVER		/* do input, output, scroll, until 'quit' */
	{
		mychr = char_in ();

		if (mychr == 'q') break;	/* leave the loop */

		else if (mychr == 'z')		/* print a string */
		{
		        strcpy (mystr, "Hello world ..."); 
			line_out (mystr, strlen (mystr));
			char_out (13);
		}
		else char_out (mychr);		/* echo the char */
	}
}
#endif	/* DIAGNOSTIC */

/************************************************************************/
/*	Top Level							*/
/************************************************************************/

/*------------------------------*/
/*	main			*/
/*------------------------------*/

main()
{
	WORD	error;
	error = z_init ();

	if (!error)
/*		START (); */	/* launch the ZIP */
		draw_test();

	z_exit ();		/* Close the workstation and exit */
}
