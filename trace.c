/*
 * trace - trace DOS system calls
 *
 * (C) Copyright 1991 Diomidis Spinellis.  All rights reserved.
 *
 * $Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.10 1991/01/25 02:44:00 dds Exp $
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
#include <ctype.h>

#ifndef lint
static char rcsid[] = "$Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.10 1991/01/25 02:44:00 dds Exp $";
#endif

static int stringprint, hexprint, otherprint, nameprint, regdump, tracechildren;
static unsigned datalen = 15;

static mypsp, tracedpsp;
static char _far *critsectflag;
static char _far *criterrflag;

/* Video interrupt */
#define DOS_INT	0x21

/* Old dos handler to chain to */
static void (_interrupt _far _cdecl *old_dos_handler)( void );

static int fd;

#pragma check_stack(off)
#pragma intrinsic(strlen)

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
outfstring(char _far *s)
{
	int sseg, soff;
	int len = _fstrlen(s);

	sseg = FP_SEG(s);
	soff = FP_OFF(s);
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

static long
makelong(unsigned high, unsigned low)
{
	union {
		struct {
			unsigned low;
			unsigned high;
		} hl;
		long l;
	} v;

	v.hl.high = high;
	v.hl.low = low;
	return v.l;
}


static void _far *
makefptr(unsigned seg, unsigned off)
{
	union {
		struct {
			unsigned off;
			unsigned seg;
		} so;
		void _far *ptr;
	} v;

	v.so.seg = seg;
	v.so.off = off;
	return v.ptr;
}

static void
outso(int sseg, int soff)
{
	int len;
	
	len = _fstrlen(makefptr(sseg, soff));
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

/* Used for number conversions */

static char buff[80];

static void
outdec(long v)
{
	ltoa(v, buff, 10);
	outstring(buff);
}

static void
outhex(int v)
{
	itoa(v, buff + 2, 16);
	buff[0] = '0';
	buff[1] = 'x';
	outstring(buff);
}

static void
outbuff(unsigned sseg, unsigned soff, unsigned len)
{
	char _far *p;
	unsigned l, i;
	char *s = buff;
	int hex = 0;
	static char hexnum[] = "0123456789abcdef";

	p = makefptr(sseg, soff);
	l = min(datalen, len);
	for (i = 0; i < l; i++)
		if (!isascii(p[i]))
			if (hexprint)
				hex = 1;
			else
				return;
	*s++ = '\t';
	if (hex) {
		*s++ = '{';
		for (i = 0; i < l; i++) {
			*s++ = '0';
			*s++ = 'x';
			*s++ = hexnum[(unsigned)p[i]>>4];
			*s++ = hexnum[p[i] & 0xf];
			*s++ = ',';
		}
		if (l < len) { *s++ = '.'; *s++ = '.'; *s++ = '.';}
		*s++ = '}';
	} else {
		*s++ = '"';
		for (i = 0; i < l; i++)
			switch (p[i]) {
			case '\n':
				*s++ = '\\';
				*s++ = 'n';
				break;
			case '\t':
				*s++ = '\\';
				*s++ = 't';
				break;
			case '\r':
				*s++ = '\\';
				*s++ = 'r';
				break;
			default:
				if (iscntrl(p[i])) {
					*s++ = '\\'; *s++ = 'x';
					*s++ = hexnum[(unsigned)p[i]>>4];
					*s++ = hexnum[p[i] & 0xf];
				} else
					*s++ = p[i];
			}
		if (l < len) { *s++ = '.'; *s++ = '.'; *s++ = '.';}
		*s++ = '"';
	}
	*s++ = '\0';
	outfstring(buff);
}

	
static unsigned
getpsp(void)
{
	_asm mov ah, 51h
	_asm int 21h
	_asm mov ax, bx
}

static void
setpsp(unsigned psp)
{
	_asm mov ah, 50h
	_asm mov bx, psp
	_asm int 21h
}

static char _far *
getcritsectflag(void)
{
	unsigned sseg, soff;

	_asm mov ah, 34h
	_asm int 21h
	_asm mov ax, es
	_asm mov sseg, ax
	_asm mov soff, bx
	return makefptr(sseg, soff);
}

static void _far *
getdta(void)
{
	unsigned sseg, soff;

	_asm mov ah, 2fh
	_asm int 21h
	_asm mov ax, es
	_asm mov sseg, ax
	_asm mov soff, bx
	return makefptr(sseg, soff);
}

static void
outmode(unsigned mode)
{
	char *p = buff;

	*p++ = '[';
	if (mode & 0x20) *p++ = 'a'; else *p++ = '-';
	if (mode & 0x10) *p++ = 'd'; else *p++ = '-';
	if (mode & 0x08) *p++ = 'v'; else *p++ = '-';
	if (mode & 0x04) *p++ = 's'; else *p++ = '-';
	if (mode & 0x02) *p++ = 'h'; else *p++ = '-';
	if (mode & 0x01) *p++ = 'r'; else *p++ = '-';
	*p++ = ']';
	*p++ = '\0';
	outstring(buff);
}

/*
 * Print the DTA contents as filled by the find first / find next functions
 */
static void
outdta(void)
{
	struct s_dta {
		char reserve[21];
		unsigned char mode;
		unsigned short time;
		unsigned short date;
		long size;
		char name[13];
	} _far *d;
	int psp;

	psp = getpsp();
	setpsp(tracedpsp);
	d = getdta();
	setpsp(psp);
	outstring("ok\t(");
	outfstring(d->name);
	outstring(", ");
	outdec(d->size);
	outstring(", ");
	outdec((d->date >> 9) + 80);
	outstring("/");
	outdec((d->date >> 5) & 0xf);
	outstring("/");
	outdec(d->date & 0x1f);
	outstring(", ");
	outdec(d->time >> 11);
	outstring(":");
	outdec((d->time >> 5) & 0x3f);
	outstring(".");
	outdec(d->time & 0x1f);
	outstring(", ");
	outmode(d->mode);
	outstring(")\r\n");
}

static void
okerrorproc(unsigned flags, unsigned ax)
{
	setpsp(mypsp);
	if (flags & 1) {
		outstring("Error ");
		outdec(ax);
		outstring("\r\n");
	} else
		outstring("ok\r\n");
}

static void
nerrorproc(unsigned flags, unsigned ax)
{
	setpsp(mypsp);
	if (flags & 1)
		outstring("Error ");
	outdec(ax);
	outstring("\r\n");
}

#pragma check_stack()

char *funcs[] =
{
	"terminate",		/* 00H	DOS1 - TERMINATE PROGRAM */
	"key_in_echo",		/* 01H	DOS1 - KEYBOARD INPUT WITH ECHO */
	"disp_out",		/* 02H	DOS1 - DISPLAY OUTPUT */
	"serial_in",		/* 03H	DOS1 - SERIAL INPUT */
	"serial_out",		/* 04H	DOS1 - SERIAL OUTPUT */
	"printer_out",		/* 05H	DOS1 - PRINTER OUTPUT */
	"console_io",		/* 06H	DOS1 - DIRECT CONSOLE I/O */
	"dir_key_in",		/* 07H	DOS1 - DIRECT KEYBOARD INPUT */
	"key_in",		/* 08H	DOS1 - KEYBOARD INPUT WITHOUT ECHO */
	"disp_string",		/* 09H	DOS1 - DISPLAY STRING */
	"buf_key_in",		/* 0AH	DOS1 - BUFFERED KEYBOARD INPUT */
	"chk_key_stat",		/* 0BH	DOS1 - CHECK KEYBOARD STATUS */
	"clr_key_func",		/* 0CH	DOS1 - CLEAR KEY BUFFER AND PERFORM FUNCTION */
	"reset_disk",		/* 0DH	DOS1 - RESET DISK */
	"sel_drive",		/* 0EH	DOS1 - SELECT CURRENT DRIVE */
	"open_file",		/* 0FH	DOS1 - OPEN FILE */
	"close_file",		/* 10H	DOS1 - CLOSE FILE */
	"search_first",		/* 11H	DOS1 - SEARCH FOR FIRST MATCHING FILE */
	"search_next",		/* 12H	DOS1 - SEARCH FOR NEXT MATCHING FILE */
	"delete_file",		/* 13H	DOS1 - DELETE FILE */
	"rd_seq_rec",		/* 14H	DOS1 - READ SEQUENTIAL RECORD */
	"wr_seq_rec",		/* 15H	DOS1 - WRITE SEQUENTIAL RECORD */
	"create_file",		/* 16H	DOS1 - CREATE FILE */
	"rename_file",		/* 17H	DOS1 - RENAME FILE */
	"reserved 0x18",	/* 18h */
	"rd_cur_drive",		/* 19H	DOS1 - REPORT CURRENT DRIVE */
	"set_dta",		/* 1AH	DOS1 - SET DISK TRANSFER AREA FUCNTION */
	"rd_fat_1",		/* 1BH	DOS1 - READ CURRENT DRIVE'S FAT */
	"rd_fat_2",		/* 1CH	DOS1 - READ ANY DRIVE'S FAT */
	"reserved 0x1d",	/* 1dh */
	"reserved 0x1e",	/* 1eh */
	"reserved 0x1f",	/* 1fh */
	"reserved 0x20",	/* 20h */
	"rd_ran_rec1",		/* 21H	DOS1 - READ RANDOM FILE RECORD */
	"wr_ran_rec1",		/* 22H	DOS1 - WRITE RANDOM FILE RECORD */
	"rd_file_size",		/* 23H	DOS1 - REPORT FILE SIZE */
	"set_ran_rec",		/* 24H	DOS1 - SET RANDOM RECORD FIELD SIZE */
	"set_int_vec",		/* 25H	DOS1 - SET INTERRUPT VECTOR */
	"create_seg",		/* 26H	DOS1 - CREATE PROGRAM SEGMENT FUCNTION */
	"rd_ran_rec2",		/* 27H	DOS1 - READ RANDOM FILE RECORD */
	"wr_ran_rec2",		/* 28H	DOS1 - WRITE RANDOM FILE RECORD FUCNTION */
	"parse_name",		/* 29H	DOS1 - PARSE FILENAME */
	"get_date",		/* 2AH	DOS1 - GET DATE */
	"set_date",		/* 2BH	DOS1 - SET DATE */
	"get_time",		/* 2CH	DOS1 - GET TIME */
	"set_time",		/* 2DH	DOS1 - SET TIME */
	"set_verify",		/* 2EH	DOS1 - SET DISK WRITE VERIFICATION MODE */
	"get_dta",		/* 2FH	DOS2 - GET DISK TRANSFER AREA ADDRESS */
	"get_ver",		/* 30H	DOS2 - GET DOS VERSION NUMBER */
	"keep",			/* 31H	DOS2 - ADVANCED TERMINATE BUT STAY RESIDENT */
	"reserved 0x32",	/* 32h */
	"cntrl_brk",		/* 33H	DOS2 - GET/SET CONTROL BREAK STATUS */
	"critical_flag",	/* 34h */
	"get_int_vec",		/* 35H	DOS2 - GET INTERRUPT VECTOR */
	"get_space",		/* 36H	DOS2 - GET DISK FREE SPACE */
	"switchar",		/* 37h */
	"get_country",		/* 38H	DOS2 - GET COUNTRY INFORMATION */
	"mkdir",		/* 39H	DOS2 - MAKE DIRECTORY */
	"rmdir",		/* 3AH	DOS2 - REMOVE DIRECTORY */
	"chdir",		/* 3BH	DOS2 - CHANGE CURRENT DIRECTORY FUCNTION */
	"create",		/* 3CH	DOS2 - CREATE FILE */
	"open",			/* 3DH	DOS2 - OPEN FILE */
	"close",		/* 3EH	DOS2 - CLOSE FILE */
	"read",			/* 3FH	DOS2 - READ FILE/DEVICE */
	"write",		/* 40H	DOS2 - WRITE FILE/DEVICE */
	"delete",		/* 41H	DOS2 - DELETE FILE */
	"lseek",		/* 42H	DOS2 - MOVE FILE POINTER */
	"chmod",		/* 43H	DOS2 - CHANGE FILE MODE */
	"ioctl",		/* 44H	DOS2 - DEVICE I/O CONTROL */
	"dup",			/* 45H	DOS2 - DUPLICATE FILE HANDLE */
	"cdup",			/* 46H	DOS2 - FORCE FILE HANDLE DUPLICATION */
	"get_dir",		/* 47H	DOS2 - GET CURRENT DIRECTORY */
	"allocate",		/* 48H	DOS2 - ALLOCATE MEMORY */
	"free",			/* 49H	DOS2 - FREE MEMORY */
	"set_block",		/* 4AH	DOS2 - MODIFY ALLOCATED MEMORY BLOCK */
	"exec",			/* 4BH	DOS2 - LOAD/EXECUTE PROGRAM */
	"term_proc",		/* 4CH	DOS2 - TERMINATE PROCESS */
	"get_code",		/* 4DH	DOS2 - GET SUBPROGRAM RETURN CODE */
	"find_first",		/* 4EH	DOS2 - FIND FIRST FILE MATCH */
	"find_next",		/* 4FH	DOS2 - FIND NEXT FILE MATCH */
	"set_psp",		/* 50h */
	"get_psp",		/* 51h */
	"get_handle_addr",	/* 52h */
	"reserved 0x53",	/* 53h */
	"get_verify",		/* 54H	DOS2 - GET FILE WRITE VERIFY STATE */
	"reserved 0x55",	/* 55h */
	"rename",		/* 56H	DOS2 - RENAME FILE */
	"date_time",		/* 57H	DOS2 - GET/SET FILE DATE/TIME */
	"alloc_strategy",	/* 58h */
	"get_err",		/* 59H	DOS3 - GET EXTENDED RETURN CODE */
	"create_temp",		/* 5AH	DOS3 - CREATE TEMPORARY FILE */
	"create_new",		/* 5BH	DOS3 - CREATE NEW FILE */
	"file_lock",		/* 5CH	DOS3 - LOCK/UNLOCK FILE ACCESS */
	"reserved 0x5d",	/* 5dh */
	"machine_name",		/* 5eh */
	"assign_list",		/* 5fh */
	"reserved 0x60",	/* 60h */
	"reserved 0x61",	/* 61h */
	"get_psp",		/* 62H	DOS3 - GET PROGRAM SEGMENT PREFIX ADDRESS */
};

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
	static trace = 1;
	static execed;
	int fun;

	fun = _ax >> 8;
	if (!execed) {
		if (fun == 0x4b)
			execed = 1;
		_chain_intr(old_dos_handler);
	}
	if (!trace || *critsectflag || *criterrflag)
		_chain_intr(old_dos_handler);
	trace = 0;
	tracedpsp = getpsp();
	setpsp(mypsp);
	switch (fun) {
	case 0x25:				/* set_vector */
		outstring("set_vector(");
		outhex(_ax & 0xff);
		outstring(", ");
		outhex(_ds);
		outstring(":");
		outhex(_dx);
		outstring(")\r\n");
		setpsp(tracedpsp);
		trace = 1;
		_chain_intr(old_dos_handler);
		break;
	case 0x30:				/* get_version */
		outstring("get_version() = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			mov _bx, bx
			mov _cx, cx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		outdec(_ax & 0xff);
		outstring(".");
		outdec(_ax >> 8);
		outstring("\r\n");
		break;
	case 0x35:				/* get_vector */
		outstring("get_vector(");
		outhex(_ax & 0xff);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			mov _bx, bx
			mov ax, es
			mov _es, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		outhex(_es);
		outstring(":");
		outhex(_bx);
		outstring("\r\n");
		break;
	case 0x39:				/* Mkdir */
		outstring("mkdir(\"");
		goto stringfun;
	case 0x3a:				/* Rmdir */
		outstring("rmdir(\"");
		goto stringfun;
	case 0x3b:				/* Chdir */
		outstring("chdir(\"");
	stringfun:
		outso(_ds, _dx);
		outstring("\") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			push es
			mov ax, _ds
			mov ds, ax
			mov ax, _es
			mov es, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop es
			pop ds
			popf
		}
		okerrorproc(_flags, _ax);
		break;
	case 0x3c:				/* Creat */
		outstring("creat(\"");
		outso(_ds, _dx);
		outstring("\", ");
		outhex(_cx);
		if (stringprint)
			outmode(_cx);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov dx, _dx
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov cx, _cx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		nerrorproc(_flags, _ax);
		break;
	case 0x3d:				/* Open */
		outstring("open(\"");
		outso(_ds, _dx);
		outstring("\", ");
		outdec(_ax & 0xff);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov dx, _dx
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		nerrorproc(_flags, _ax);
		break;
	case 0x3e:				/* Close */
		outstring("close(");
		outdec(_bx);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		okerrorproc(_flags, _ax);
		break;
	case 0x3f:				/* Read */
		outstring("read(");
		goto readwrite;
	case 0x40:				/* Write */
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
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
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
			popf
		}
		setpsp(mypsp);
		if (_flags & 1)
			outstring("Error ");
		outdec(_ax);
		if (stringprint)
			outbuff(_ds, _dx, _ax);
		outstring("\r\n");
		break;
	case 0x41:				/* Unlink */
		outstring("unlink(\"");
		goto stringfun;
	case 0x42:				/* Lseek */
		outstring("lseek(");
		outdec(_bx);
		outstring(", ");
		outdec(makelong(_cx, _dx));
		outstring(", ");
		outdec(_ax & 0xff);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			mov cx, _cx
			mov dx, _dx
			int 21h
			mov _ax, ax
			mov _dx, dx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
		} else
			outdec(makelong(_dx, _ax));
		outstring("\r\n");
		break;
	case 0x43:				/* Chmod / getmod */
		if ((_ax & 0xff) == 0)
			outstring("getmod(\"");
		else if ((_ax & 0xff) == 1)
			outstring("chmod(\"");
		else
			goto aka_default;
		outso(_ds, _dx);
		if (_ax & 0xff == 1) {
			outstring("\", ");
			outhex(_cx);
			if (stringprint)
				outmode(_cx);
		}
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov dx, _dx
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov cx, _cx
			int 21h
			mov _ax, ax
			mov _cx, cx
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
		} else {
			if (_ax & 0xff == 0) {
				outdec(_cx);
				if (stringprint)
					outmode(_cx);
			} else
				outstring("ok");
		}
		outstring("\r\n");
		break;
	case 0x45:				/* Dup */
		outstring("dup(");
		outdec(_bx);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		nerrorproc(_flags, _ax);
		break;
	case 0x46:				/* Dup2 */
		outstring("dup2(");
		outdec(_bx);
		outstring(", ");
		outdec(_cx);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			mov cx, _cx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		okerrorproc(_flags, _ax);
		break;
	case 0x47:				/* Getcwd */
		outstring("getcwd(");
		outdec(_dx & 0xff);
		outstring(", ");
		outhex(_ds);
		outstring(":");
		outhex(_si);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov si, _si
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\r\n");
		} else {
			if (stringprint) {
				outstring("ok\t\"");
				outfstring(makefptr(_ds, _si));
				outstring("\"\r\n");
			} else
				outstring("ok\r\n");
		}
		break;
	case 0x48:				/* Alloc */
		outstring("alloc(");
		outhex(_bx);
		outstring("0) = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			int 21h
			mov _ax, ax
			mov _bx, bx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\t(free = ");
			outhex(_bx);
			outstring("0 bytes)\r\n");
		} else {
			outhex(_ax);
			outstring(":0\r\n");
		}
		break;
	case 0x49:				/* Free */
		outstring("free(");
		outhex(_es);
		outstring(":0) = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push es
			mov ax, _flags
			push ax
			popf
			mov ax, _es
			mov es, ax
			mov ax, _ax
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop es
			popf
		}
		okerrorproc(_flags, _ax);
		break;
	case 0x4a:				/* Realloc */
		outstring("realloc(");
		outhex(_es);
		outstring(":0, ");
		outhex(_bx);
		outstring("0) = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push es
			mov ax, _flags
			push ax
			popf
			mov ax, _es
			mov es, ax
			mov ax, _ax
			mov bx, _bx
			int 21h
			mov _ax, ax
			mov _bx, bx
			pushf
			pop ax
			mov _flags, ax
			pop es
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\t(free = ");
			outhex(_bx);
			outstring("0 bytes)\r\n");
		} else
			outstring("ok\r\n");
		break;
	case 0x4b:				/* Exec */
		outstring("exec(\"");
		outso(_ds, _dx);
		if (tracechildren)
			outstring("\") = ...\r\n");
		else
			outstring("\") = ");
		setpsp(tracedpsp);
		if (tracechildren) {
			trace = 1;
			_chain_intr(old_dos_handler);
		}
		_asm {
			pushf
			push ds
			push es
			mov ax, _ds
			mov ds, ax
			mov ax, _es
			mov es, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov bx, _bx
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop es
			pop ds
			popf
		}
		okerrorproc(_flags, _ax);
		break;
	case 0x4c:				/* Exit */
		outstring("exit(");
		outdec(_ax & 0xff);
		outstring(")\r\n");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		break;
	case 0x4e:				/* Findfirst */
		outstring("findfirst(\"");
		outso(_ds, _dx);
		outstring("\", ");
		outhex(_cx);
		if (stringprint)
			outmode(_cx);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov cx, _cx
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\r\n");
		} else {
			if (stringprint)
				outdta();
			else
				outstring("ok\r\n");
		}
		break;
	case 0x4f:				/* Findnext */
		outstring("findnext() = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		if (_flags & 1) {
			outstring("Error ");
			outdec(_ax);
			outstring("\r\n");
		} else {
			if (stringprint)
				outdta();
			else
				outstring("ok\r\n");
		}
		break;
	case 0x5a:				/* Tmpfile */
		outstring("tmpfile(\"");
		outso(_ds, _dx);
		outstring("\", ");
		outhex(_cx & 0xff);
		outstring(") = ");
		setpsp(tracedpsp);
		_asm {
			pushf
			push ds
			mov ax, _ds
			mov ds, ax
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov cx, _cx
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			pop ds
			popf
		}
		nerrorproc(_flags, _ax);
		break;
	aka_default:
	default:
		if (otherprint) {
			if (nameprint && fun <= 0x62) {
				outstring(funcs[fun]);
			} else
				outhex(fun);
			if (regdump) {
				outstring(" :ax=");
				outhex(_ax);
				outstring(" bx=");
				outhex(_bx);
				outstring(" cx=");
				outhex(_cx);
				outstring(" dx=");
				outhex(_dx);
				outstring(" si=");
				outhex(_si);
				outstring(" di=");
				outhex(_di);
				outstring(" ds=");
				outhex(_ds);
				outstring(" es=");
				outhex(_es);
			}
			outstring("\r\n");
		}
		setpsp(tracedpsp);
		trace = 1;
		_chain_intr(old_dos_handler);
	}
	setpsp(tracedpsp);
	trace = 1;
}

int getopt(int, char **, char *);

int
main(int argc, char *argv[])
{
	int status = 0;
	extern int optind;
	extern char *optarg;
	char *fname = "trace.log";
	int errflag = 0;
	char *usagestring = "usage: trace [-f fname] [-l len] [-help] [-vrsoxnc] command options ...\n";
	int c;

	while ((c = getopt(argc, argv, "f:h:sxol:nrvc")) != EOF)
		switch (c) {
		case 'c':
			tracechildren = 1;
			break;
		case 'v':
			tracechildren = regdump = stringprint = hexprint = otherprint = nameprint = 1;
			break;
		case 'r':			/* Dump registers */
			regdump = 1;
			break;
		case 'f':			/* Output file */
			fname = optarg ;
			break ;
		case 's':			/* Print strings in I/O */
			stringprint = 1;
			break;
		case 'x':			/* Print binary data in I/O */
			hexprint = 1;
			break;
		case 'o':			/* Print other DOS functions */
			otherprint = 1;
			break;
		case 'n':			/* Print names of other DOS functions */
			nameprint = 1;
			break;
		case 'l':
			datalen = atoi(optarg);
			break;
		case 'h':			/* Help */
			fputs(usagestring, stderr);
			fputs("Trace Version 1.00 (C) Copyright 1991 D. Spinellis.  All rights reserved\n", stderr);
			fputs("-f\tSpecify output file name\n", stderr);
			fputs("-s\tPrint I/O strings\n", stderr);
			fputs("-x\tPrint I/O binary data (needs -s)\n", stderr);
			fputs("-l\tSpecify I/O printed data length\n", stderr);
			fputs("-o\tTrace other functions\n", stderr);
			fputs("-c\tTrace children processes\n", stderr);
			fputs("-n\tPrint other functions by name\n", stderr);
			fputs("-r\tDump registers on other functions\n", stderr);
			fputs("-v\tVerbose (-sxonrc)\n", stderr);
			fputs("-h\tPrint this message\n", stderr);
			return 0;
		case '?':			/* Error */
			errflag = 1;
			break ;
		}
	if (errflag || optind == argc) {
		fputs(usagestring, stderr);
		return 2;
	}
	if ((fd = open(fname, O_CREAT | O_TRUNC | O_TEXT | O_WRONLY, 0666)) == -1) {
		perror(fname);
		return 1;
	}

	mypsp = getpsp();
	critsectflag = getcritsectflag();
	if (_osmajor == 2)
		criterrflag = critsectflag + 1;
	else
		criterrflag = critsectflag - 1;
	/* Save old handler and install new one */
	old_dos_handler = _dos_getvect(DOS_INT);
	_dos_setvect(DOS_INT, dos_handler);
	/* 
	 * XXX If the program is terminated during spawn by a signal
	 * and does not exit normaly the system will crash.
	 */

	status=spawnvp(P_WAIT, argv[optind], argv + optind);

	/* Restore old handler */
	_dos_setvect(DOS_INT, old_dos_handler);

	close(fd);
	if (status == -1) {
		perror(argv[optind]);
		return 1;
	} else
		return 0;
}
