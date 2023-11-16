@style (spacing 1)

HOW TO FORMAT DISKS FOR THE TI-99
========================================================================

    1.  Insert the Disk Manager cartridge into the front of the computer.
Turn on the monitor, the disk drives, and lastly the CPU.

    2.  Insert a new disk in Drive 1.

    3.  Strike any key to get the main menu.  Select (2) DISK MANAGER, wait for
the title screen, then strike any key.

    4.  From the Disk Manager menu, select
(2) DISK COMMANDS <enter>, then (4) INITIALIZE NEW DISK <enter>.

    5.  To copy-protect the disk, at this point type FCTN-X ten times. (!)
The tenth time you type it, a mark (><) should appear at the top of the screen.

The blank disk you will get (when you are finished with this initialization)
may be copied TO, but may not be copied FROM with the standard Disk Manager 
utilities.  Note that only master disks for Dysan should be copy-protected.

    6.  A prompt will ask you to select a drive.  Select Drive 1 and press ENTER.

    7.  A prompt will ask you to name the new disk.  Make sure the ALPHA
LOCK key is depressed, and give it the name (up to 10 characters, e.g. 
PLANETFALL) of the game that will be put on it.

    8.  For all other prompts, use the default values by pressing ENTER.

    9.  The new disk in Drive 1 will be formatted.  It will take a minute 
or two.

    10.  When the initialization is finished, the display should show 
"AVAILABLE = 358  USED = 0."

----------------------------------------------------------------------

If you are formatting more than one disk it will be necessary to exit
the Disk Manager and restart from Step 3, to erase the mark (><) from
the screen.

@newpage

TRANSFERRING A GAME FILE FROM THE DEC-20 TO THE TI-99
========================================================================

    1.  Insert the Editor/Assembler cartridge into the front of the computer.
Turn on the monitor, the disk drives, and then the CPU.

    2.  Insert the TFTP disk in Drive 1.

    3.  Strike any key to get the main menu.  Select (2) EDITOR/ASSEMBLER.

    4.  From the Editor/Assembler menu, select (3) LOAD AND RUN.  When
the display prompts for a file name, make sure the ALPHA LOCK key is depressed 
and type DSK1.TFTP-OBJ <enter>.  (see note below)

    5.  Remove the TFTP disk from Drive 1 and return it to the storage box.

    6.  Insert two blank, initialized disks into Drive 1 and Drive 2.

    7.  Log on to the DEC-20 and call up the TN20 program.  Enter the name of
the file you want to download.  When it asks for a line speed, type 1200.

    8.  Make sure that the TN20 line is connected to the serial port of the TI-99.

    9.  Go back to the TI-99 and press ENTER.  A prompt will appear for a 
program name.  Type TFTP <enter>.  Both drives will spin, and files named GAME1
and GAME2 will be created on the disks in Drive 1 and Drive 2, respectively.
The display will read READY TO RECEIVE.

    10.  Now go to the DEC-20 and press any key to begin the transfer.  If all
is OK, the TI-99 will display CHECKSUM OK.  (It also displays each data block
as it is transferred).  A complete transfer takes 15-25 minutes.

    11.  If the transfer halts before it is finished, there was probably a disk
error.  Make sure that the two disks are blank (except for the GAME1 and GAME2
files) and try again.  As a last resort, re-initialize the two disks.

-------------------------------------------------------------------------------

    Note: If the file you wish to download is a text file containing displayable 
carriage returns, rather than source code for a game, then in Step 4 type
DSK1.TFTP9-OBJ <enter>.  In Step 9 you will need to type "0" to indicate a
text file, and then type a name (starting with "DSK1.").

@newpage

COPYING TI-99 FILES BETWEEN DISKS
=======================================================================

The easiest way to copy several files from one disk to another is by 
using the disk backup utility, as follows.

1.  Insert the Disk Manager cartridge into the front of the computer.
Turn on the monitor, the disk drives, and then the CPU.

2.  Insert the disk containing the master files in Drive 1, and the copy 
disk in Drive 2.

3.  Strike any key to get the main menu.  Select (2) Disk Manager, wait for
the title screen, then strike any key.  

4.  From the Disk Manager menu, select (2) DISK COMMANDS <enter>, then 
(2) BACKUP DISK <enter>.

5.  The display will prompt SELECTIVE (Y/N)?  Normally you will respond
N(O) <enter>.  This tells the program to automatically copy every file from
the master disk.

(To copy only selected files, respond Y(ES) <enter>.  This tells the program 
to display a prompt for each file.)

6.  The display will then prompt MASTER DISK (1-3)?  Type 1 <enter>.

7.  The display will prompt COPY DISK (1-3)?  Type 2 <enter>.

8.  The display will prompt INITIALIZE NEW DISK (Y/N)?  Normally you
should type N(O) <enter>.  If you type YES, all files on the copy disk will
be erased.

9.  Each file on the master disk in Drive 1 will now be copied.  The file
names will be listed sequentially on the screen.

10.  To confirm the copy process when the routine is finished, press BACK 
(FCTN 9), then do a CATALOG DISK on the disk in Drive 2.

--------------------------------------------------------------------

NOTE: Each Interlogic game on the TI-99 requires two disks, which should
ultimately contain the following files:
@style(indent 4)

Disk 1 - GAME1, LOAD, LOAD1, BOOT, BOOT1
@style(indent 0)
@style(indent 4)
Disk 2 - GAME2
@style(indent 0)
@newpage

HOW TO BOOT A GAME ON THE TI-99
========================================================================

    1.  Turn on the power to all parts of the computer system.
    

    2.  Insert game disk #1 in Drive 1 and close the drive door.

    3a.  If you are using the Extended Basic module, select "TI EXTENDED
BASIC" from the main menu.  The program will load automatically.

    3b.  If you are using the Mini-Memory or Editor/Assembler module, 
select "TI BASIC" from the main menu.  When the computer responds with
"TI BASIC READY," depress the ALPHA LOCK key and type OLD DSK1.LOAD1 <Enter>.
When the computer responds a moment later with a prompt, type RUN <Enter>.

    4.  The disk drive will spin the diskette and the program will load
in one to three minutes, depending upon which module you are using.

    5.  A brief message will appear when the program is finished loading.
Remove disk #1, insert game disk #2, and press any key to continue.
You will not need disk #1 again unless you reload or RESTART the game.

