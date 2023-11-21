# Infocom Z-code Interpreters

This is a collection of Infocom's original Z-code interpreters for various home computers of the 1980s.

Most of the interpreters are provided as assembly source code (for the 6502, Z-80, and so on). Some have compiled binaries as well. As with the Infocom game source code, what's available is what's available.

This repo contains a small amount of internal documentation, mostly about how to create working floppy disks for each platform. It contains no personal or private communication, unless you count one footnote about a "straightforward" porting job.

---

Below I have documented all the directories and files, as best I understand them. Corrections and further details welcomed.

Interesting tidbits:

Many directories include a large "assembled printout" file (usually `.prn` or `.lst`). This is a text file containing the assembler output for a complete program, both hex bytes and opcodes. ("prn" because it is notionally formatted for a line printer.)

The `unix` directory contains a C implementation of DIP, the virtual machine used to implement Fooblitzky. The `atari` and `ibm` directories contain assembly implementations of same. `unix/foo.dat` is the game file in question; `unix/ediptst.sum` would be a test suite game.

(Wikipedia refers to this as "GZIP - Graphical Zork Interpreter Program". I'm not sure what the origin of that name is. It doesn't appear in the Infocom source as far as I can tell. A couple of files refer to it as "GRIP" or the "G-machine". I'll stick with "DIP".)

The architecture of DIP is unexplored territory (for my generation at least!) At a glance, it has version byte 1 and a serial number starting at byte 12 (rather than 18, as in the Z-machine). The game file appears to include graphical sprite data in high VM memory, rather than as external resources. The opcodes in the DIP interpreter are different. And so on.

[oci]: https://eblong.com/infocom/

---

## (root directory)

* pdp11.zip: ZIP, PDP-11 assembly (dated 1982-1983), presumably for reference for porting
* maximumsize.doc: Max Z-code size for each platform
* maxoldsize.doc: Old version of same
* fixer.mud: MDL code -- convert linefeeds?
* verify.speeds: Notes on how fast $VERIFY works on Zork 1 on each interpreter. (SEM, 12 Jul 1983)

## create

- *.rno: Docs for creating floppies on various platforms, and then booting them up (all are in [Runoff] format)

[Runoff]: https://en.wikipedia.org/wiki/TYPSET_and_RUNOFF

## ziptest: Z3 test game file

* ziptest.zip: Game file
* ziptest.hex: Hex dump of same
* ziptest.pre: "preload" segment of ziptest.zip
* ziptest.pur: "pure" segment of ziptest.zip

This is the same game file archived [here][oci] as [ziptest-r40-s840613.z3](https://eblong.com/infocom/gamefiles/ziptest-r40-s840613.z3).

The `.pre` and `.pur` files concatenate to the original, except that in ziptest.pre, header byte 1 gets the "story file split" bit set and bytes 4-5 (himem) are $05FF. This is the usual way Z-code files are split for platforms that can't fit the whole game in memory at once.

## 64: Commodore 64

* how-to-make-disks: Text docs for creating a C64 floppy
* zip/*.src: C64 assembly
* zip/c64zip-d.src: Assembled printout
* zip/c64-d.zip: Zip binary
* lzip/*.asm: C64 assembly (note that lzip/fast.lzip is also assembly)

## c-128: Commodore 128

* ezip/_read.me: Changelog (through 11/13/87)
* ezip/*.asm: Source
* ezip/*.cbd: Source for "combined disk" version, which I think supports C64 and C128 on the same disk
* xzip/_read.me: Changelog (through 11/13/87)
* xzip/notes: Compilation docs
* xzip/c128: Implementation notes on color and fonts
* xzip/*.asm: Source
* xzip/*.bat: Batch scripts for build
* split/*: File split utility
* tftp/*: File transfer utility

## acorn: BBC Acorn

* --read.me: Changelog (1.15.85)
* *.asm: Acorn assembly
* tftpsrc.rno: Assembly for TFTP to put files on floppy? (not runoff, despite the name)

## amiga: Commodore Amiga

* --read.me: Docs
* patches.bugs: Patches for Amiga OS include files (for compiling)
* production.info: Docs for making an Amiga game floppy
* production.old: Same, old version
* tftp.info: Docs on using TFTP to move a game file from DEC20 to Amiga
* split.notes: Notes on porting the game-split utility to Atari ST
* ref.txt: Customer docs for running a game
* refcard.txt: Customer reference card for Amiga
* refcard.old: Same, old version
* support.info: Customer FAQs for support
* intuition.bugs: Bugs "as reported to tech support"
* shersound.list: Sherlock sound list
* sound.readme: Docs on using the Amiga audio C code
* kermit.doc: Docs for Amiga Kermit (Jack J. Rouse)
* aterm.doc: Docs for Amica Aterm (Larry Phillips)
* ss48: Batch script to mount a hard drive
* startupii: Batch script, configure environment?
* temp.zil: Fragment of the V6 ZIL parser? (Looks like "Arthur")
* *.c, *.h, *.asm files: XZIP and YZIP interpreter code
* batch/batch.info: Docs on compiling using batch scripts
* batch/*.bat: Build scripts

## apple: Apple 2

* read.me: Changelog (through 11/13/87)
* applezip.sed: SED script (text replacement); not sure what it's for
* apple-g.zip, apple-h.zip: ZIP binaries, two versions
* oldzip/, zip/, ezip/, xzip/, yzip/: Directories containing assembly interpreter source. All files are assembly except .lst which are assembled printouts.
* yzip/apl2iff: Graphics/sound utilities for Apple 2. (Mentions "Magnetic Scrolls format" for images?)
* interp/: Interpreters or config data for V6 games?

## atari: Atari

* atari-d.zip: ZIP binary
* atari-grip.bin: DIP binary
* *.src: Assembly for ZIP interpreter
* *.dip: Assembly for DIP interpreter
* atarizip-d.asm: assembled printouts (ZIP)
* atari-grip.prn: assembled printouts (DIP)

## basic: GW-BASIC

* xzip.bin: ZIP binary (C128 auto-boot disk)
* *.bas: BASIC bytecode for receiving files over a serial connection

Presumably the C128 image is just the last file that was transferred to a micro before the directory was dumped.

Not sure why there are several versions of the BASIC script. They all start by printing "IBM TFTP RECEIVER", but they diverge from there.

## colorcomputer: TRS-80 CoCo 2

* read.me: Changelog (through 6/12/85)
* cocozip.asm: Assembled printout
* coco.zip: Hex dump of ZIP binary
* cocoref.doc: Customer reference card for CoCo
* *.asm: Assembly

This is the same source archived at the [CoCo Archive][cocoarc].

[cocoarc]: https://colorcomputerarchive.com/repo/Programming/Source/Infocom%20Adventure%20Games%20Interpreter/

## cpm80: CP/M

* read.me: Changelog (through Feb 5, 85)
* zorkcpm.asm: ZIP source (Zilog Z80)
* zorkcpm.prn: Assembled printout

## cpm86: CP/M-86

* read.me: Changelog (through 08/18/84)
* zip.a86: ZIP source (8086)
* cpm86.old: Same, older version
* tftp.asm: TFTP source

## ibm: PC

* --read.me: Changelog (May 85)
* notes: Notes on the contents
* dip.asm: DIP source (monochrome)
* lip.asm: DIP source (RGB)
* dump.asm: Additional color-handling code for lip.asm
* ezip.asm: EZIP source main file
* *.ezp: Files included in ezip.asm
* msibmzip.asm: ZIP for IBM IB2?
* ibmzip.asm: ZIP for IBM (older)
* ibmziplist.dan: Assembled printout for ibmzip.asm
* bosszip.asm: ZIP with "boss key"
* iboss.asm: ADditional boss-key code for bosszip.asm
* create.asm: Utility to create a runnable binary (interpreter plus game data)
* boot.asm: Bootstrap loader code
* switches.txt: Interpreter command-line switches
* gol.bat: Batch script to link `lip.com`
* install.bat: Batch script to create a playable floppy
* installh.bat: Batch script to create a playable folder on a hard drive
* installd.bat: Batch script to create a playable Cornerstone demo floppy
* statline.bat: Batch script, ???
* n2.compare: Diff of msibmzip.asm and boszip.asm

## mac: Macintosh

* mx*.a: XZIP (or maybe YZIP) source, assembly
* mx.lst: Assembled printout
* yzip-f3.bin: Binary for YZIP
* xzip.lst: XZIP source (Pascal)
* xzip.r: Resource fork info for XZIP
* gfx.c: C code to compress/uncompress image data
* gfx.p: Same, Pascal
* build.info: Build docs
* build.old: Older docs
* combined.ideas: Thoughts on creating a combined ZIP/EZIP/XZIP interpreter, with the idea of using high-end Macs as primary IF development machines
* cracks.memo: Notes from "a pirate BBS" on cracking copy-protection, and how the `@piracy` ZIP opcode might be kept meaningful
* hfs.memo*: Customer support info for an interpreter bug on the (then-new) Mac HFS filesystem
* iconfix.memo: Customer support info for save files not showing the correct icon
* zork0.bugfix: Bug report for MacOS 5.0, 6.0.0
* production.info: Docs on creating a game floppy
* production.old: Same, older
* st_convert.notes: Notes on adjusting assembly source to MPW syntax
* refcard.doc: Customer reference card for Macintosh
* prepsound.p: Utility to convert Amiga sound files to Mac
* serialize.c: Utility to tweak the serial number of a Z-code file
* signaturize.info: Docs on setting the Mac creator/type codes for a Mac interpreter
* signaturize.p: Utility to set the Mac creator/type codes on a Mac *floppy*
* tftp.p: File transmission utility
* stftp.p: File sending utility (does more?)
* testsound.p: Test playing sounds, I guess
* av_*.asm: Another audio test
* dissbits.a: Sample source for a "copy image with dissolve effect" routine
* temp: Chunk of mx4.a
* ?: ?

## msdos: MS-DOS 2.0

* read.me: Changelog (through 10/29/84)
* mszip.asm: ZIP assembly
* create.asm: Create a floppy
* setup.asm: Configure terminal to run ZIP properly

## ti: TI Professional? (MS-DOS 2.0)

* read.me: Changelog (08/20/84)
* tizip.asm: ZIP assembly
* create.asm: Create a floppy
* setup.asm: Configure terminal to run ZIP properly

## msx: Microsoft MSX platform (Z80-based)

* read.me: Changelog (through 2-5-85)
* zorkmsx.asm: ZIP assembly
* zorkmsx.prn: Assembled printout
* setmsx.asm: Configure your MSX terminal to run ZIP properly

## tandy: Tandy 2000 (MS-DOS)

* read.me: Changelog (08/13/84)
* tzip.asm: ZIP assembly
* create.asm: Create a floppy

## hp: HP PC (HP-DOS)

* read.me: Changelog (blank)
* hpzip.asm: ZIP assembly
* hpcreate.asm: Create a floppy

## st: Atari ST

* stzip.s: ZIP source (assembly)
* stx.s, stx*.s, sound.s: XZIP source (assembly)
* xzip.c: XZIP source (C)
* zip.c: ZIP source (top level only)
* xzipdip.c, xzipdip.h: XZIP/DIP code (not used?)
* squish.c: Utility to pull a user-selectable chunk of a Paintworks image file?
* smash.c: Utility to combine a bunch of images into a library file?
* fcompare.c: File compare utility
* tftp.c: TFTP source
* asm.note: Notes that the (stx*.s) source can compile either ZIP or EZIP depending on compiler defs
* color.note: Implementation note on color usage
* batch.info: Notes on compiling ZIP binaries
* sid.notes: Notes on SID (debugger?)
* graphics.ken: Notes on graphics file requirements
* sound.notes: Notes on sound file requirements
* nxtlin.bug, nxtlin.asm: Note on a display bug
* scrnam.asm: Code used in *.s files
* tftp.info: Docs on using TFTP to move a game file from DEC20 to ST
* font2.dat: Presumably font data
* refcard.txt: Customer reference card for Atari ST
* readme.doc: Docs for GIA sound utility (via Activision)
* gia.c: Sound player utility
* makefile.gia: Makefile for GIA
* pumpsnd.s: Assembly for low-level GIA code
* pumpsnd_av.s: Modified version?
* pumpsnd.old: Older version?

## ted: CBM Plus/4

* plus4-c.zip: ZIP binary
* plus4zip-c.asm: Assembled printout
* *.src: Assembly

## ti994: TI-99/4

* --read.me: Changelog (10/12/84)
* ti_refbook.mss: Docs on creating and booting a game floppy
* ti_refcard.mss: Customer reference card for TI-99
* *.old: ZIP assembly (different parts of a single ZIP program, despite the confusing filenames)

## unix

* --notes: TODO list
* zip.c: Source for ZIP
* zipdefs.h: Header for zip.c
* phg_zip.c: Source for ZIP, modification by Paul H. Gross (5-October-1985)
* phg_zipdefs.h: Modified header (not used)
* dip.c: Source for DIP
* dipdefs.h: Header for dip.c
* folding.c: Code for word-wrapping feature
* page.c: Code for memory-paging feature
* dip.block: A chunk of dip.c having to do with image data
* dip.call: A chunk of dip.c having to do with OPCALL
* dip.init: A chunk of dip.c having to do with app startup
* dip.ops: A chunk of dipdefs.h
* block.c: Utility to display blocks of font or image data? (not part of the DIP interpreter)
* foo.dat: Possibly the Fooblitzky game file?
* foo.hex: Hex dump of foo.dat (referred to as "fooblitz.dat")
* foo.pichex: A slice of foo.hex
* ediptst.sum: DIP test game?
* ediptst.hex: Hex dump of "ediptst.dip" (a chunk of ediptst.sum)
* ediptst.pichex: Hex dump of "ediptst.pic" (the rest of of ediptst.sum)

Both DIP game files appear to include graphical data. Or so I infer from the `.pichex` files, which are hex dumps of the high end of the game file.

I tried to decode the pichex files. `dip.c` implies that images are made of 8-byte (8x8 pixel) blocks, and if you split the pichex files that way you get reasonable blocks. One would have to do additional work to figure out which blocks to tile together to make complete "icons" or sprites.


