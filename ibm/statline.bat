echo off
if %1x==x statline B:
if not %2x==x goto again
rem INSTALLH.BAT ver. B (11/15/86)
rem make some space for the one-diskette case
if exist gamedir.com del gamedir.com
if exist yes.com del yes.com
:again
af > ansi
if errorlevel 1 goto noansi
rem del af to make space in the 2-floppy case
if exist pass2 goto delaf
goto doscan
:delaf
if %2x==x del af.exe
:doscan
scan
if errorlevel 2 goto failed
if errorlevel 1 goto append
echo foo > case1
goto signoff
:append
copy \config.sys \config.old > nul
copy \config.sys+ansi \config.sys > nul
:win
echo	.
echo	.
if not %2x==x goto hdmess
echo foo > case2
goto signoff
:hdmess
echo foo > case3
:signoff
if exist ansi del ansi
if exist install*.bat del install*.bat
if exist scan.com del scan.com
if exist af.exe del af.exe
if exist pass2 del pass2
if not %2x==x goto alldone
if exist %1one goto jrdat
goto docopy
:jrdat
echo	.
echo	Once again, you will often have to change diskettess.  When you're
echo	prompted to insert a diskette for drive A:, use the original STORY
echo	diskette; when you're prompted to insert a diskette for drive B:, use
echo	the PLAY diskette.
echo	.
del one
pause
:docopy
copy A:*.DAT %1 > nul
:alldone
echo	.
if not exist case1 goto case2
del case1
echo	Infocom story installation is now complete.
goto	really
:case2
if not exist case2 goto case3
del case2
echo	Infocom story installation is now complete.  
echo	.
echo	IMPORTANT: You must now put the PLAY diskette in drive A: and
echo	press CTRL-ALT-DEL to reboot your system.
goto	really
:case3
del case3
echo	Infocom story installation is now complete.  
echo	.
echo	IMPORTANT: You must now remove the original diskette from drive A:
echo	and press CTRL-ALT-DEL to reboot your system.
echo	.
:really
echo	.
echo	.
echo	See your reference card for an explanation of how to load the story.
goto end
:failed
echo	.
echo	Something is wrong with your config.sys file.  Please press
echo	CTRL-ALT-DEL to reboot your system.  If this operation fails again, 
echo	consult your DOS manual regarding Configuring Your System.
goto end
:noansi
echo	.
if exist %1one goto pcjr
echo	Please find your original DOS diskette and place it in drive A: and
goto dos
:pcjr
echo	Whenever you are prompted to put a diskette into drive B:, you
echo	should insert the PLAY diskette into the drive, and whenever you
echo	are prompted to put the diskette into drive A:, you should insert
echo	the DOS diskette into the drive.
:dos
if %2x==x goto getb
pause
copy a:ansi.sys %1\ /B > nul
if exist %1one goto nomsg1
echo	.
echo	Now replace the original STORY diskette in drive A: and
pause
:nomsg1
if exist %1\ansi.sys goto getansi
goto end
:getb
pause
copy a:ansi.sys %1ansi.sys /V > nul
if exist %1one goto nomsg2
echo	.
echo	Now replace the original STORY diskette in drive A: and
pause
:nomsg2
if exist %1ansi.sys goto getansi
echo foo > case1
goto signoff
:getansi
echo foo > pass2
goto again
:end
if exist ansi del ansi
if exist install*.bat del install*.bat
if exist scan.com del scan.com
if exist af.exe del af.exe
if exist one del one
if exist pass2 del pass2
