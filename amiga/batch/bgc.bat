echo off
echo 	--- Compile %1.C to .O
echo on
lc1m -d -i\include\ %1
lc2m -s %1
