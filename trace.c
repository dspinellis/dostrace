/*
 * trace - trace DOS system calls
 *
 * (C) Copyright 1991 Diomidis Spinellis.  All rights reserved.
 *
 * $Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.1 1991/01/10 19:13:46 dds Exp $
 *
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h>
#include <process.h>

#ifndef lint
static char rcsid[] = "$Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.1 1991/01/10 19:13:46 dds Exp $";
#endif

static mypsp;

/* Video interrupt */
#define DOS_INT	0x21

/* Old dos handler to chain to */
static void (_interrupt _far _cdecl *old_dos_handler)( void );

static int fd;

#pragma check_stack(off)
#pragma intrinsic(strlen)

/* Used for number conversions */

static char buff[80];

static void
outstring(char *s)
{
	int len = strlen(s);
	int sseg, soff;
	char far *p = s;

	sseg = FP_SEG(p);
	soff = FP_OFF(p);
	_asm {
		mov bx, fd
		mov cx, len
		mov ax, sseg
		mov dx, soff
		push ds
		mov ds, ax
		mov ah, 40h
		int 21h
		pop ds
	}
}

static void
outso(int sseg, int soff)
{
	union {
		struct {
			unsigned off;
			unsigned seg;
		} so;
		void _far *ptr;
	} v;
	int len;
	
	v.so.seg = sseg;
	v.so.off = soff;
	len = _fstrlen(v.ptr);
	_asm {
		mov bx, fd
		mov cx, len
		mov ax, sseg
		mov dx, soff
		push ds
		mov ds, ax
		mov ah, 40h
		int 21h
		pop ds
	}
}

static void
outdec(int v)
{
	itoa(v, buff, 10);
	outstring(buff);
}

static void
outhex(int v)
{
	itoa(v, buff, 16);
	outstring(buff);
}

unsigned
getpsp(void)
{
	_asm mov ah, 51h
	_asm int 21h
	_asm mov ax, bx
}

void
setpsp(unsigned psp)
{
	_asm mov ah, 50h
	_asm mov bx, psp
	_asm int 21h
}

#pragma check_stack()

/* The dos interrupt handler */
static void _cdecl _interrupt _far
dos_handler(
	unsigned _es,
	unsigned _ds,
	unsigned _di,
	unsigned _si,
	unsigned _bp,
	unsigned _sp,
	unsigned _bx,
	unsigned _dx,
	unsigned _cx,
	unsigned _ax,
	unsigned _ip,
	unsigned _cs,
	unsigned _flags)
{
	static recurse;
	int psp;

	if (recurse)
		_chain_intr(old_dos_handler);
	recurse = 1;
	switch (_ax >> 8) {
	case 0x3c:				/* Creat */
		psp = getpsp();
		setpsp(mypsp);
		outstring("creat(\"");
		outso(_ds, _dx);
		outstring("\", 0x");
		outhex(_cx);
		outstring(") = ");
		setpsp(psp);
		_asm {
			push ds
			mov dx, _dx
			mov ax, _ds
			mov ds, ax
			mov ax, _ax
			mov cx, _cx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
		}
		setpsp(mypsp);
		if (_flags & 1)
			outstring("Error ");
		outdec(_ax);
		outstring("\n");
		setpsp(psp);
		break;
	case 0x3d:				/* Open */
		psp = getpsp();
		setpsp(mypsp);
		outstring("open(\"");
		outso(_ds, _dx);
		outstring("\", ");
		outdec(_ax & 0xff);
		outstring(") = ");
		setpsp(psp);
		_asm {
			push ds
			mov dx, _dx
			mov ax, _ds
			mov ds, ax
			mov ax, _ax
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
		}
		setpsp(mypsp);
		if (_flags & 1)
			outstring("Error ");
		outdec(_ax);
		outstring("\n");
		setpsp(psp);
		break;
	case 0x3e:				/* Close */
		psp = getpsp();
		setpsp(mypsp);
		outstring("close(");
		outdec(_bx);
		outstring(") = ");
		setpsp(psp);
		_asm {
			mov ax, _ax
			mov bx, _bx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\n");
		} else
			outstring("ok\n");
		setpsp(psp);
		break;
	case 0x3f:				/* Read */
		psp = getpsp();
		setpsp(mypsp);
		outstring("read(");
		goto readwrite;
	case 0x40:				/* Write */
		psp = getpsp();
		setpsp(mypsp);
		outstring("write(");
	readwrite:
		outdec(_bx);
		outstring(", ");
		outhex(_ds);
		outstring(":");
		outhex(_dx);
		outstring(", ");
		outdec(_cx);
		outstring(") = ");
		setpsp(psp);
		_asm {
			push ds
			mov ax, _ds
			mov ds, ax
			mov ax, _ax
			mov bx, _bx
			mov cx, _cx
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
		}
		setpsp(mypsp);
		if (_flags & 1)
			outstring("Error ");
		outdec(_ax);
		outstring("\n");
		recurse = 0;
		setpsp(psp);
		break;
	default:
		recurse = 0;
		_chain_intr(old_dos_handler);
	}
	recurse = 0;
}

int
main(int argc, char *argv[])
{
	int status = 0;

	if ((fd = open("trace.log", O_CREAT | O_TRUNC | O_TEXT | O_WRONLY, 0666)) == -1) {
		perror("trace.log");
		exit(1);
	}

	mypsp = getpsp();
	/* Save old handler and install new one */
	old_dos_handler = _dos_getvect(DOS_INT);
	_dos_setvect(DOS_INT, dos_handler);
	/* 
	 * XXX If the program is terminated during spawn by a signal
	 * and does not exit normaly the system will crash.
	 */

	status=spawnvp(P_WAIT, argv[1], argv + 1);

	/* Restore old handler */
	_dos_setvect(DOS_INT, old_dos_handler);

	close(fd);
	if (status == -1) {
		perror(argv[1]);
		return 1;
	} else
		return 0;
}


