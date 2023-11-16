echo off
if not exist path.set    setpath %0 %1
del path.set
echo 	--- Make AMIGAZIP.C to .O
if not %1/==/ goto :Link
lc1m -dEZIP=1 -i\include\ AMIGAZIP
lc2m AMIGAZIP
:Link
