echo off
cls
rem	INSTALLH.BAT ver. B (11/15/86) for use with CS demo.
if %1x==x installh C:
%1
cd \
echo	____________________________________________________________________
echo	.
echo	.
echo		            INFOCOM GAME INSTALLATION
echo				 FOR A HARD DISK
echo	.
echo	____________________________________________________________________
echo	.
echo	This program will copy the story files onto your hard disk from your
echo	master STORY diskette.  The story files will be copied into a 
echo	directory called \INFOCOM.
echo	.
echo	Included on this disk is the CORNERSTONE demo.  This program will
echo	copy the necessary files into a directory called \INFOCOM\CSDEMO if
echo	you so desire.
echo	.
echo	... Please Wait ...
a:gamedir 
if errorlevel 2 goto nohd
if exist setup.inf ren setup.inf setup.old
copy a:setup.* >nul
copy a:statline.bat >nul
copy a:*.com >nul
copy a:*.dat >nul
copy a:af.exe >nul
if exist setup.old copy setup.old setup.inf >nul
if exist setup.old del setup.old >nul
del yes.com >nul
del gamedir.com >nul
echo	.
a:yes	Do you want to install the CORNERSTONE demo on your hard disk?(Y/N)
if errorlevel 1 goto nodem
echo	.
echo	The directory \INFOCOM\CSDEMO will be created and the necessary
echo	files copied into it.  To run the CORNERSTONE demo, enter the
echo	\INFOCOM\CSDEMO directory and type CSDEMO at the prompt.
md csdemo > nul
cd csdemo > nul
copy a:\demo\*.* > nul
del demotf.bat 
ren demohd.bat csdemo.bat
cd \INFOCOM 
echo	.
echo	CORNERSTONE demo installation complete.
goto stat
:nodem
echo	.
echo	The Cornerstone demo will not be installed.  If you want to
echo	install it at a later date, you will need to run INSTALLH again.
echo	.
:stat
statline %1 hd
:nohd
echo	Drive %1 is not your hard disk.  Please run INSTALLH again.
echo	.
echo			A:INSTALLH [d:]
echo	.
echo	Be sure to specify the hard disk drive letter followed by a colon.
echo	If your hard disk is drive C:, then you do not need to specify 
echo	a drive at all.  If you have a floppy based computer, you should
echo	run INSTALL.

