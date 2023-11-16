echo off
echo 	--- Compile %1.C to .O
echo on
lc1m -i\include\ %1
lc2m %1
