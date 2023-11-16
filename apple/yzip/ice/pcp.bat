@echo off
pcplus /faplyzip
ask "Load yzip.lst?", yn
if errorlevel 2 goto quit
if errorlevel 1 goto loady
:loady
b yzip.prn
:quit
cls
echo on
