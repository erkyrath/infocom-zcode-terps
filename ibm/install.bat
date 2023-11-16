echo off
cls
rem	INSTALL.BAT ver B (11/15/86) for Cornerstone Demo and IF
if %1x==x install B:
echo	____________________________________________________________________
echo	.
echo	.
echo		            INFOCOM GAME INSTALLATION
echo			      FOR A FLOPPY DISKETTE
echo	.
echo	____________________________________________________________________
echo	.
echo	This program will create a PLAY diskette.  To proceed, you must have 
echo	one (1) diskette that you have formatted with the command FORMAT B:/S
echo	(the PLAY diskette), and your original DOS diskette or a copy.  You 
echo	should also have your reference card handy while running this 
echo	installation.
echo	.
echo	To install the CORNERSTONE DEMO, you will need another blank,
echo	formatted diskette.  After this program is finished, type
echo	INSTALLD at the A: prompt.  
echo	.
a:yes Do you have the formatted diskette ready (Y/N)? 
if errorlevel 1 goto end
echo	.
a:yes Do you have a two floppy system (Y/N)? 
if errorlevel 1 goto one
echo	.
echo	Put your PLAY diskette in drive %1 and
pause
goto two
:one	
echo	.
echo	You will be required to switch diskettes often as this program copies
echo	the STORY files from the original diskette to your PLAY diskette.  
echo	Your computer will prompt you when to switch diskettes.  Whenever you
echo	are prompted to insert a diskette for drive A:, you should insert the
echo	original STORY diskette into the drive; whenever you are prompted to 
echo	insert a diskette for drive B:, you should insert the PLAY diskette 
echo	into the drive.
echo	.
pause
:two
echo	.
echo	... Please Wait ...
if not exist %1command.com goto nodos
if errorlevel 1 copy a:yes.com %1one > nul
rem Used to to copy *.dat here, but loses on clones, so we'll wait till later
copy *.com %1 > nul
copy af.exe %1 > nul
copy setup.* %1 > nul
copy statline.bat %1 > nul
if not exist %1statline.bat goto nospace
%1
%1statline %1
:nospace
echo	.
echo	Insufficient disk space on drive %1
goto end
:nodos
echo	.
echo	You have not properly formatted the diskette in drive %1.
echo	.
:end
echo	.
echo	You are being returned to DOS without completing the installation.
echo	Please find a blank diskette and format it with the command 
echo	FORMAT B:/S.  When you have prepared the diskette as instructed
echo	in the reference card, restart this installation procedure.
