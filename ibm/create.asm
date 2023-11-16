	title - create
	comment $ 
	This program replaces create.exe for MS-DOS based and now
	covers IBM/MS-DOS 2.0+ VERSION I ZIP.  It takes a game name
	on the command line and patches the zip to load the game
	specified on the command line.

	A>create zork1

	will produce zork1.com, the zip program to load zork1.

	Written by Paul H. Gross - 26-Feb-85 $
;
cseg	segment
	assume	cs:cseg,ds:cseg,es:cseg,ss:cseg
	org	100H
	public	create
create	proc	
;
	jmp	start
	public	ext,sav,fnf,nocmd,handle,lenth,infile,com
com	db	'.COM',0
ext	db	'.DAT',0
sav	db	'.SAV',0
nozip	db	'Failed to create zip file...',13,10,'$'
fnf	db	'File not found',13,10,'$'
done	db	'Completed...$'
nocmd	db	'No game specified on command line.  Usage: ',13,10,10
	db	9,'create <gamename>',13,10,'$'
handle	dw	?
lenth	dw	?
infile	db	'msibmzip.com',0
fileoff	dw	82H
;
	public	start,load,patch,dump,buffer
start:	call	load
	call	patch
	call	dump
	mov	ah,9
	mov	dx,offset done
	int	21H
	mov	ah,4CH
	int	21H
;
load:	mov	dx,offset infile	; open zip program file
	mov	ax,3D00H		; open zip for read 
	int	21H			; do it
	jc	nofile
	mov	dx,offset buffer	; get end of program area
	mov	cx,0FFFFH		; read in a whole file
	mov	bx,ax			; get handle
	mov	ah,3fH
	int	21H			; read it
	mov	lenth,ax		; save number bytes read
	mov	ah,3EH			; close this file
	int	21H			; thru dos
	ret				; return
nofile:	mov	dx,offset fnf
error:	mov	ah,9
	int 	21H
	mov	ah,4CH
	int	21H
;
patch:	mov	si,80h			; get cmd line
	cld				; auto increment
	lodsb				; number of chars
	dec	al			; count out <cr>
	cmp	al,2			; at least 2 chars
	jge	patch1			; continue
	mov	dx,offset nocmd		; error message
	jmp	error
patch1:	xor 	ah,ah
	mov	cx,ax			; get number
	inc	si
	cmp	byte ptr 1[si],':'	; do we have a drive specifier
	jnz	ptch1a			; nope
	sub	cx,2			; fix the count
	add	si,2			; and the byte pointer
	mov	fileoff,84H		; offset to file name with drive spec
ptch1a:	push	cx			; save this number
	push	cx			; save this number
	push	cx			; save it again
patch2:	lodsb				; get a byte
	cmp	al,'a'			; is it lower
	jl	patch3
	mov	di,si			; back up pointer
	dec	di
	and	al,5FH			; uppercasify
	stosb				; the byte on command line
patch3:	loop	patch2			; and continue
	mov	si,fileoff		; offset to filename in cmd line
	pop	cx			; restore count
	lea	di,buffer+3		; addr of file name in zip
	rep	movsb			; transfer the name
	mov	cx,5			; drop in extension
	mov	si,offset ext		; point to ext
	rep	movsb			; write it
	pop	cx			; get number of bytes again
	mov	si,fileoff		; point to string again
	lea	di,buffer+65+3		; addr of save string
	rep	movsb			; transfer it
	mov	cx,5			; and now the extension
	mov	si,offset sav		; from here
	rep	movsb
	pop	cx			; get number of bytes
	mov	di,cx			; get end of string addr
	add	di,fileoff		; point at <cr>
	mov	si,offset com		; add comfile extension
	mov	cx,5
	rep	movsb
	ret
;
dump:	mov	dx,82H			; point to new zip name
	mov	cx,0			; normal attribute
	mov	ah,3CH			; create a file
	int	21H			;
	jc	crfail			; create failed
	mov	bx,ax			; get the handle
	mov	cx,lenth		; get file length
	mov	ah,40H			; function is write
	mov	dx,offset buffer
	int	21H
	mov	ah,3EH
	int	21H			; close the file
	ret
crfail:	mov	dx,offset nozip		;
	jmp	error
;
buffer	label	near
create	endp
cseg	ends
	end	create
