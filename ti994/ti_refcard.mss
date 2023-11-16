@STYLE (SPACING 1, JUSTIFICATION NO)
@STYLE (INDENT 4)

INTERLOGIC [tm] REFERENCE CARD FOR THE TI-99/4A
=======================================================

I.  What You Need

Required
    []  TI-99/4A home computer
    []  Peripheral Expansion Unit
    []  32K Memory Expansion Card
    []  One 5 1/4 inch disk drive
    []  Any one of the following modules: Extended Basic, 
Mini-Memory, or Editor/Assembler

Optional
    []  One or more blank, formatted diskettes (for SAVEs)
    []  Disk Manager module (for formatting SAVE diskettes)
    []  Second disk drive (for convenience with SAVE)
    []  RS232 Interface Card and a compatible printer, 
either parallel or serial (for SCRIPT)


II.  Loading the Game

    1.  Turn on the power to all parts of the computer system.
    
    2.  Insert game disk 1 in Drive 1 and close the drive door.

    3a.  If you are using the Extended Basic module, select "TI EXTENDED
BASIC" from the main menu.  The program will load automatically.

    3b.  If you are using the Mini-Memory or Editor/Assembler module, 
select "TI BASIC" from the main menu.  When the computer responds with
"TI BASIC READY," depress the ALPHA LOCK key and type OLD DSK1.LOAD1 <Enter>.
When the computer responds a moment later with a prompt, type RUN <Enter>.

    4.  The disk drive will spin the diskette and the program will load
in one to three minutes, depending on the module you are using.

    5.  A brief message will appear when the program is finished loading.
Remove disk 1, insert disk 2, and press any key to continue.  You will not
need disk 1 again unless you reload or RESTART the game.

    6.  If nothing appears on your screen (or if you receive a BASIC error
message), something is wrong.  See the Troubleshooting section.


III.  Talking to the Game

    Whenever you see the prompt (>), the game is waiting for you to type
in your instructions.  You may type up to two full lines of text at a time.
If you make a mistake, hold down the FCTN key and press the "S" key to
erase the error one character at a time.  (You can only erase characters
up to the beginning of the current line.)  When you have finished typing 
in your instructions, press the ENTER key.  The game will respond and
then the prompt (>) will reappear.

    If a description will not fit on the screen all at once, the word 
--MORE-- will appear in the bottom left portion of the screen.  After reading 
the part on the screen, you may press any key to see the rest of the
description.


IV.  SCRIPTing 

    If you have a printer you may make a transcript of the game as you play it:

    1.  Connect the printer to either the parallel port or the serial port
of the RS232 Interface Card.  (Refer to the instructions for your particular
printer.)  If your printer is serial, it should be set to operate at 300
baud with odd parity.

    2.  Turn on the printer and load the game as described above.

    3.  To start the transcript at any time, use the SCRIPT command.  The 
first time you use the SCRIPT command, the game will ask to which port
your printer is connected.  Type the letter "P" to indicate the parallel port,
or "S" to indicate the serial port.  Do not press the ENTER key after the
letter.  (Alternately, you may press the ENTER key without typing a letter
to tell the game to use the default, which is the parallel port.)  

    4.  To stop the transcript, use the UNSCRIPT command.

    5.  SCRIPT and UNSCRIPT may be used as often as desired.

    6.  If the printer is not ready for any reason (e.g., power off, paper
out, etc.), the game will halt until you make the necessary correction
to the printer (e.g., turn the printer on, add paper, etc.).


V.  The Status Line

    At the top of the screen, you will see a status line.  This line is
updated after every move to show your current whereabouts in the game.
Depending upon the type of game, it may also show other information.

Score

    In games that keep a score, such as the ZORK [tm] underground 
adventures, the right side of the status line will show something like
this:
        Score: 245/920

    The first number is your score and the second is the total number of
moves you have made.  In the example, you have 245 points in 920 moves.

Time

    In games that keep track of the time (e.g., the mystery thriller
DEADLINE [tm]), the right side of the status line will look something like
the following:

        Time: 9:22 am

    This shows the current time of day in the game.


VI.  Saving a Game Position

    To save the current position, use the SAVE command.  You may SAVE up to
five different game positions on each storage diskette and RESTORE them in any
order.  To keep track of these different positions, each is assigned a number
(from 1 to 5).  Each time you SAVE a game position, it will overwrite any 
position already on your storage diskette which was assigned the number
you are specifying now.  If you want to SAVE more than one position, you must
specify a different position number for each one.

    When you enter the SAVE command, the game will respond:

        Type backspace to abort
        Position (1-5):   (Default = 1)

    1.  Type a number between 1 and 5 to tell the game to use that position
on the diskette.  Do not press the ENTER key after the digit.  
(Alternately, you may press the ENTER key without typing a digit to tell
the game to use the default position, which is 1.)  The game will respond:

        Disk drive (1-3): (Default = 1)

If you have only one disk drive [*], proceed as follows.

    2.  Press ENTER to tell the game to copy to Drive 1.  It will then
respond:

        Insert SAVE disk and strike any key

    3.  Remove game disk 2 from the disk drive.

    4.  Insert the storage diskette and close the drive door.  (To
prepare this diskette, see Initializing Storage Diskettes.)

    5.  Press any key.  The diskette will spin for forty seconds or less,
then the game will respond:

        Insert game disk 2 and strike any key

    6.  Remove the storage diskette from the drive and re-insert the
game diskette.  Close the drive door.

    7.  Press any key.  If all is well, the game will respond:

        OK

If it responds:

        Failed

consult the Troubleshooting section.

    You may now continue playing.  You can use the storage diskette 
and the RESTORE command to return to this position at another time.

    [*] If you have more than one disk drive, follow the above procedure,
but omit the steps numbered 3 and 6.  Use the appropriate drive number
in step 2 for the disk drive that you use in step 4.


VII.  Restoring a Saved Game Position

    To restore a previously saved game position, enter the RESTORE
command.  Then follow the same steps (1 to 7) as for SAVE above.


VIII.  Initializing Storage Diskettes

    Storage diskettes are made with the TI Disk Manager module.  Refer
to the TI Disk Memory System manual for detailed instructions.


IV.  Troubleshooting

    If the game fails to load properly or SAVE/RESTORE fails, check each
of the following items.  If none of these offers a solution, call your
computer dealer for assistance.

    1.  Check to see that your TI-99/4A console, memory expansion card,
and disk drive(s) are plugged in correctly and connected properly.

    2.  Be sure that the system was turned on in the proper order (i.e., disk
drive(s) first, console last).

    3.  Check to see that the diskette(s) was inserted correctly and that
the drive door(s) is closed.

    4.  Inspect the diskette(s) carefully for any visible damage.

    5.  Be sure that the diskette(s) is in the proper drive(s). 
Game diskettes may only be run from Drive 1.  For SAVE/RESTORE, be sure
that you have typed the correct drive number for the storage diskette.

    6.  For SAVE, be sure that the storage diskette is not write-protected
(i.e., there is nothing covering the notch on the side of the diskette).

    7.  Also for SAVE, be certain that the diskette has been initialized 
properly.  As a last resort, try a different diskette.

    8.  Try again: the problem may only be momentary.

