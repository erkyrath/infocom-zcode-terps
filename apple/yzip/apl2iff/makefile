!if $d(REL)
# for "released version"
CFLAGS= -c -d -f- -k- -G -v- -Z 
!else
# for debugging
CFLAGS= -c -d -f- -v
!endif

!if $d(REL)
LFLAGS= -lc 
!else
LFLAGS= -lm -lv -lc
!endif

CC= tcc $(CFLAGS) $(CCFLAGS)

LINK= tcc $(LFLAGS) $(CLFLAGS)

!if $d(NEW)
all: new iff.exe dd.exe

new:
        del *.obj
        del *.exe
!else
all: iff.exe dd.exe
!endif
	
iff.exe: apl2iff.obj palette.obj picfile.obj applebit.obj
	$(LINK) -eiff apl2iff.obj palette.obj picfile.obj applebit.obj

dd.exe: makeappl.obj picfile.obj palette.obj
	$(LINK) -edd makeappl.obj palette.obj picfile.obj

palette.obj: palette.c picfile.h
	$(CC) palette.c

picfile.obj: picfile.c picfile.h
	$(CC) picfile.c

applebit.obj: applebit.c
	$(CC) applebit.c

makeappl.obj: makeappl.c picfile.h
	$(CC) makeappl.c

apl2iff.obj: apl2iff.c
	$(CC) apl2iff.c

