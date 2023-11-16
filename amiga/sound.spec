
	SOUND   id [,action, vol]: INT		EXT:245  (ZIP, XZIP)

    Id specifies the sound.  If zero, the last specified id is used.
BEEP (1) and BOOP (2) are defined for all micros.  

    Action (1-4) is INIT, START (the default), STOP, or CLEANUP.

    Volume can range from 0-8.  The default is the value defined in
each midi data file (normally the maximum).

    INIT
    ------------
    Getting ready to produce a sound takes significant time, mostly to
read the data from disk.  The samples for Lurking Horror take, on
average (for a 37K file), six seconds each to load.

    Once loaded, a sound can be started and stopped multiple times
without any noticeable delay.

    Sometimes it may be desirable to INIT a sound in advance, then
print things on the screen ("The doorbell chimes") and start the
sound without the awkward delay.  Alternately, the sound could begin 
/before/ the relevant description.

    INIT is optional.  If omitted before a START, it will occur
implicitly.

    START
    ------------
    Russell's sounds come in two categories: sounds that play a finite
number of times (usually once) and then turn themselves off, and
sounds that cycle indefinitely.  In either case, control returns to
the game (on the Amiga, anyway) as soon as the sound starts.

    STOP
    ------------
    STOP is useful for "environmental sounds" in the second 
catagory above; for example, a noise that ends only when you leave a
particular room, and resumes when you reenter it.

    STOP is not required for "one-shot" sounds in the first catagory,
but it's desirable to use it anyway.  Otherwise (on the Amiga) a
concurrent process may be blocked from access to the audio hardware.

    CLEANUP
    ------------
    CLEANUP releases the memory buffers that were loaded during INIT.

    CLEANUP is optional.  If omitted, it will occur implicitly before
the next INIT.
