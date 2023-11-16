
    TTL		XZIP INTERPRETER FOR COMMODORE AMIGA

**  NOLIST			* DON'T LIST THE SYSTEM EQUATES
**	INCLUDE ''
**  LIST

    SECTION ZDATA,DATA		* DEFINE TWO SEGS FOR AMIGA
	DC.W	0
    SECTION ZCODE,CODE
	NOP
    SECTION ZDATA		* AND START IN DATA ...
    
    XREF	_columns
    XREF	_cur_column
    XREF	_rows
    XREF	_cur_row
    XREF	_split_row
    XREF	_wind1
    
**  XREF	_w0font
**  XREF	_w1font
    XREF	_color		* color/mono flag

    XREF	_marg_left
    XREF	_marg_right
    XREF	_xmouse		* mouse position (char units, zero origin)
    XREF	_ymouse

    XREF	_game_ref
    XREF	_save_ref
    XREF	_savename
    XREF	_saveback
    XREF	_actlen
**  XREF	_undoflag	* [no menus for Amiga]

    SECTION ZCODE

    XDEF	_ZSTART
**  XDEF	_mov_mem	* (char *p1, *p2; LONG len)
**  XDEF	_clr_mem	* (char *p1; LONG len)

**  XREF	_write_cursor_pos	* dead, access vars directly
**  XREF	_read_cursor_pos
    XREF	_erase_eol
    XREF	_clear_lines

    XREF	_setup_input
    XREF	_event_in	* was _char_in, MacttyIn
    XREF	_char_out
    XREF	_line_out

    XREF	_highlight
    XREF	_set_color
**  XREF	_op_picinf
**  XREF	_op_display
    XREF	_german_convert

    XREF	_open_game
    XREF	_close_game

**  XREF	_file_select		* [currently in 68K]
    XREF	_new_default
    XREF	_drive_default
    XREF	_make_icon_file

    XREF	_exist_file
    XREF	_create_file
    XREF	_open_file
    XREF	_close_file
    XREF	_delete_file
    XREF	_read_file
    XREF	_write_file
    
    XREF	_script_open	* PrInit
    XREF	_script_line	* PrLine

    XREF	_random_seed
    XREF	_md_sound
    XREF	_window_resized
    XREF	_unhide_screen

    XREF	_c_getmem
    XREF	_c_maxmem
**  XREF	MacInit
**  XREF	QuitGame

    XREF	_end_sound	* END-OF-SOUND CHECK
    XREF	_int_key

* ----------------------------------------------------------------------------
* PROGRAM
* ----------------------------------------------------------------------------

    INCLUDE	"AX1.asm"
    INCLUDE	"AX2.asm"
    INCLUDE	"AX3.asm"
    INCLUDE	"AX4.asm"

    END
