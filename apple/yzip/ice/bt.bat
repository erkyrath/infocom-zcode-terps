@echo off
hm\hma6502 boot.asm -e -s -x -p -r
ask "Want to fix boot.bin?",yn
if errorlevel 2 goto no
hm\itob boot
nu boot.bin
:no



