echo off
rem	INSTALLD.BAT installs CS demo on 1 or 2 floppy machine. Ver. A
rem	11/21/86
cls
if %1x==x a:installd b:
echo	--------------------------------------------------------------
echo	.
echo	               CORNERSTONE DEMO INSTALLATION
echo	                FOR A 1 or 2 FLOPPY MACHINE
echo	.
echo	--------------------------------------------------------------
echo	.
echo	This program will install the CORNERSTONE DEMO on a backup
echo	diskette.  To proceed with this installation, you must have 
echo	a formatted, blank diskette.
echo	.
a:yes	Do you have a formatted blank diskette ready?(Y/N)
if errorlevel 1 goto nodisk
echo	.
a:yes	Do you have a two floppy computer?(Y/N)
if errorlevel 1 goto one
echo	.
echo	Insert the blank, formatted diskette in drive %1 and
pause
goto two
:one
echo	.
echo	You will be asked to swap disks several times.  Whenever 
echo	you are prompted to place a diskette in drive A:, put the 
echo	ORIGINAL diskette in the drive.  Whenever you are prompted to  
echo	place a diskette in drive B:, place your formatted diskette 
echo	in the drive.
:two
echo	.
echo	... Please wait ...
cd demo
%1
copy a:*.* 
del demohd.bat > nul
ren demotf.bat csdemo.bat
a:
cd \
echo	.
echo	Installation of the CORNERSTONE DEMO is complete.  To run
echo	the demo, place the backup disk in drive A: and type: CSDEMO.
goto end
:nodisk
echo	.
echo	You are being returned to DOS.  Please format a disk with the
echo	command FORMAT B:.  When you have a disk ready, please restart
echo	this program by typing INSTALLD.
:end


