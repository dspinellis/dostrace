/*
 * trace - trace DOS system calls
 *
 * (C) Copyright 1991 Diomidis Spinellis.  All rights reserved.
 *
 * $Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.14 1991/05/07 20:54:19 dds Exp $
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
static char rcsid[] = "$Header: /dds/src/sysutil/trace/RCS/trace.c,v 1.14 1991/05/07 20:54:19 dds Exp $";
#endif

#define MAXBUFF 80

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
tputs(char *s)
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

int _doprnt(char *fmt, va_list marker, FILE *f);

int
tprintf(char *fmt, ...)
{
	va_list marker;
	int result;
	static char msg[200];
	static FILE f;

	f._ptr = f._base = msg;
	f._cnt = 32000;
	f._flag = _IOWRT | _IOFBF;
	va_start(marker, fmt);
#pragma message("Expect warning for address of automatic (local) variable taken")
	result = _doprnt(fmt, marker, &f);
	*f._ptr = '\0';
	va_end(marker);
	tputs(msg);
	return result;
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

static char buff[MAXBUFF];

/*
 * Convert a $ terminated string to a 0 terminated one
 */
static char *
makestring(unsigned sseg, unsigned soff)
{
	char _far *p, *s;
	unsigned count = 0;

	p = makefptr(sseg, soff);
	s = buff;
	while (*p != '$' && count < datalen) {
		*s++ = *p++;
		count++;
	}
	*s = '\0';
	return buff;
}

/*
 * Return day of week
 */
static char *
weekday(int n)
{
	switch (n) {
		case 0: return "Sun";
		case 1: return "Mon";
		case 2: return "Tue";
		case 3: return "Wed";
		case 4: return "Thu";
		case 5: return "Fri";
		case 6: return "Sat";
	}
}

/*
 * Decode device information as for ioctl 0
 */
static void
devinfo(unsigned n)
{
	if (n & 0x80) {
		tputs("\tCHARDEV: ");
		if (n & 0x01) tputs("STDIN ");
		if (n & 0x02) tputs("STDOUT ");
		if (n & 0x04) tputs("NUL ");
		if (n & 0x08) tputs("CLOCK$ ");
		if (n & 0x10) tputs("SPECIAL ");
		if (n & 0x20) tputs("RAW ");
		if (n & 0x40) tputs("NOT_EOF "); else tputs("EOF ");
		if (n & 0x1000) tputs("REMOTE "); else tputs("LOCAL ");
		if (n & 0x4000) tputs("CAN_IOCTL"); else tputs("NO_IOCTL");
	} else {
		tputs("\tFILE: ");
		tprintf("device=%u ", n & 0x1f);
		if (n & 0x40) tputs("NOT_WRITTEN "); else tputs("WRITTEN ");
		if (n & 0x800) tputs("FIXED "); else tputs("REMOVABLE ");
		if (n & 0x4000) tputs("KEEP_DATE "); else tputs("UPDATE_DATE ");
		if (n & 0x8000) tputs("REMOTE"); else tputs("LOCAL");
	}
	tputs("\r\n");
}

static char *
makeonoff(int n)
{
	switch (n) {
	case 0:
		return "off";
	case 1:
		return "on";
	default:
		return itoa(n, buff, 10);
	}
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
			*s++ = hexnum[(unsigned char)p[i]>>4];
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
	tputs(buff);
}

	
static unsigned
getpsp(void)
{
	_asm mov ah, 51h
	_asm int 21h
	_asm mov ax, bx
}
#pragma message("Expect warning for no returned value")

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

static char *
strmode(unsigned mode)
{
	char *p = buff;

	if (!stringprint)
		return "";
	*p++ = '[';
	if (mode & 0x20) *p++ = 'a'; else *p++ = '-';
	if (mode & 0x10) *p++ = 'd'; else *p++ = '-';
	if (mode & 0x08) *p++ = 'v'; else *p++ = '-';
	if (mode & 0x04) *p++ = 's'; else *p++ = '-';
	if (mode & 0x02) *p++ = 'h'; else *p++ = '-';
	if (mode & 0x01) *p++ = 'r'; else *p++ = '-';
	*p++ = ']';
	*p++ = '\0';
	return buff;
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
	tprintf("ok\t(%-12Fs %10lu %2u/%2u/%2u %2u:%02u.%-2u %s)\r\n", d->name, d->size, (d->date >> 9) + 80, (d->date >> 5) & 0xf, d->date & 0x1f, d->time >> 11, (d->time >> 5) & 0x3f, d->time & 0x1f, strmode(d->mode));
}

static void
okerrorproc(unsigned flags, unsigned ax)
{
	setpsp(mypsp);
	if (flags & 1)
		tprintf("Error %u \r\n", ax);
	else
		tputs("ok\r\n");
}

static void
nerrorproc(unsigned flags, unsigned ax)
{
	setpsp(mypsp);
	if (flags & 1)
		tputs("Error ");
	tprintf("%u\r\n", ax);
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
	case 0x02:				/* disp_out */
		tprintf("display_char('%c')\r\n", _dx & 0xff);
		goto norm_proc;
	case 0x09:				/* disp_string */
		if (stringprint)
			tprintf("display(\"%s\")\r\n", makestring(_ds, _dx));
		else
			tprintf("display(%04X:%04X)\r\n", _ds, _dx);
		goto norm_proc;
	case 0x0e:				/* set_current_disk */
		tprintf("set_current_disk(%c:) = ", (_dx & 0xff) + 'A');
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			mov dx, _dx
			int 21h
			mov _ax, ax
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		tprintf("%d\r\n", _ax & 0xff);
		break;
	case 0x19:				/* get_current_disk */
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
		tprintf("get_current_disk() = %c:\r\n", (_ax & 0xff) + 'A');
		break;
	case 0x1a:				/* set_dta */
		tprintf("set_dta(%04X:%04X)\r\n", _ds, _dx);
		goto norm_proc;
	case 0x25:				/* set_vector */
		tprintf("set_vector(%#x, %04X:%04X)\r\n", _ax & 0xff, _ds, _dx);
		goto norm_proc;
	case 0x2a:				/* get_date */
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			mov _cx, cx
			mov _dx, dx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		tprintf("get_date() = %2u/%2u/%2u (%s)\r\n", _cx, _dx >> 8,  _dx & 0xff, weekday(_ax & 0xff));
		break;
	case 0x2c:				/* get_time */
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov _ax, ax
			mov _cx, cx
			mov _dx, dx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		tprintf("get_time() = %02d:%02d:%02d.%d\r\n", _cx >> 8, _cx & 0xff, _dx >> 8, _dx & 0xff);
		break;
	case 0x2d:				/* set_time */
		tprintf("set_time(%02d:%02d:%02d.%d) = ", _cx >> 8, _cx & 0xff, _dx >> 8, _dx & 0xff);
		setpsp(tracedpsp);
		_asm {
			pushf
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
			popf
		}
		setpsp(mypsp);
		if (_ax & 0xff == 0)
			tputs("ok\r\n");
		else
			tputs("invalid\r\n");
		break;
	case 0x2f:				/* get_dta */
		setpsp(tracedpsp);
		_asm {
			pushf
			mov ax, _flags
			push ax
			popf
			mov ax, _ax
			int 21h
			mov ax, es
			mov _es, ax
			mov _bx, bx
			pushf
			pop ax
			mov _flags, ax
			popf
		}
		setpsp(mypsp);
		tprintf("get_dta() = %04X:%04X\r\n", _es, _bx);
		break;
	case 0x30:				/* get_version */
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
		setpsp(mypsp);
		tprintf("get_version() = %u.%u\r\n", _ax & 0xff, _ax >> 8);
		break;
	case 0x33:				/* cntrl_brk */
		switch (_ax & 0xff) {
		case 0:
			setpsp(tracedpsp);
			_asm {
				pushf
				mov ax, _flags
				push ax
				popf
				mov ax, _ax
				mov dx, _dx
				int 21h
				mov _dx, dx
				mov _ax, ax
				pushf
				pop ax
				mov _flags, ax
				popf
			}
			setpsp(mypsp);
			tprintf("get_break() = %s\r\n", makeonoff(_dx & 0xff));
			break;
		case 1:
			tprintf("set_break(%s)\r\n", makeonoff(_dx & 0xff));
			goto norm_proc;
		default:
			goto aka_default;
		}
		break;
	case 0x35:				/* get_vector */
		tprintf("get_vector(%#x) = ", _ax & 0xff);
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
		setpsp(mypsp);
		tprintf("%04X:%04X\r\n", _es, _bx);
		break;
	case 0x39:				/* Mkdir */
		tputs("mkdir(\"");
		goto stringfun;
	case 0x3a:				/* Rmdir */
		tputs("rmdir(\"");
		goto stringfun;
	case 0x3b:				/* Chdir */
		tputs("chdir(\"");
	stringfun:
		tprintf("%Fs\") = ", makefptr(_ds, _dx));
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
		tprintf("creat(\"%Fs\", %02x%s) = ", makefptr(_ds, _dx), _cx, strmode(_cx));
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
		tprintf("open(\"%Fs\", %d) = ", makefptr(_ds, _dx), _ax & 0xff);
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
		tprintf("close(%u) = ", _bx);
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
		tputs("read(");
		goto readwrite;
	case 0x40:				/* Write */
		tputs("write(");
	readwrite:
		tprintf("%u, %04X:%04X, %u) = ", _bx, _ds, _dx, _cx);
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
			tputs("Error ");
		tprintf("%u", _ax);
		if (stringprint)
			outbuff(_ds, _dx, _ax);
		tputs("\r\n");
		break;
	case 0x41:				/* Unlink */
		tputs("unlink(\"");
		goto stringfun;
	case 0x42:				/* Lseek */
		tprintf("lseek(%u, %ld, %d) = ",_bx, makelong(_cx, _dx), _ax & 0xff);
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
		if (_flags & 1)
			tprintf("Error %u\r\n", _ax);
		else
			tprintf("%ld\r\n", makelong(_dx, _ax));
		break;
	case 0x43:				/* Chmod / getmod */
		switch (_ax & 0xff) {
		case 0:
			tputs("getmod(\"");
			break;
		case 1:
			tputs("chmod(\"");
			break;
		default:
			goto aka_default;
		}
		tprintf("%Fs", makefptr(_ds, _dx));
		if (_ax & 0xff == 1)
			tprintf("\", %#x%s", _cx, strmode(_cx));
		tputs(") = ");
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
		if (_flags & 1)
			tprintf("Error %u", _ax);
		else {
			if (_ax & 0xff == 0)
				tprintf("%u%s", _cx, strmode(_cx));
			else
				tputs("ok");
		}
		tputs("\r\n");
		break;
	case 0x44:				/* ioctl */
		switch (_ax & 0xff) {
		default:
			goto aka_default;
		case 0x00:			/* get device info */
			tprintf("ioctl(GET_DEV_INFO, %d) = ", _bx);
			setpsp(tracedpsp);
			_asm {
				pushf
				mov ax, _flags
				push ax
				popf
				mov ax, _ax
				mov bx, _bx
				int 21h
				mov _dx, dx
				pushf
				pop ax
				mov _flags, ax
				popf
			}
			setpsp(mypsp);
			if (_flags & 1)
				tprintf("Error %u\r\n", _ax);
			else {
				if (stringprint)
					devinfo(_dx);
				else
					tprintf("%04X\r\n", _dx);
			}
			break;
		}
		break;
	case 0x45:				/* Dup */
		tprintf("dup(%u) = ", _bx);
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
		tprintf("dup2(%u, %u) = ", _bx, _cx);
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
		tprintf("getcwd(%d, %04X:%04X) = ", _dx & 0xff, _ds, _si);
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
		if (_flags & 1)
			tprintf("Error %u\r\n", _ax);
		else {
			if (stringprint)
				tprintf("ok\t\"%Fs\"\r\n", makefptr(_ds, _si));
			else
				tputs("ok\r\n");
		}
		break;
	case 0x48:				/* Alloc */
		tprintf("alloc(%#x0)= ", _bx);
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
		if (_flags & 1)
			tprintf("Error %u\t(free = %#x0 bytes)\r\n", _ax, _bx);
		else
			tprintf("%04X:0000\r\n", _ax);
		break;
	case 0x49:				/* Free */
		tprintf("free(%04X:0000) = ", _es);
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
		tprintf("realloc(%04X:0000, %#x0) = ", _es, _bx);
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
		if (_flags & 1)
			tprintf("Error %u\t(free = %#x0 bytes)\r\n", _ax, _bx);
		else
			tputs("ok\r\n");
		break;
	case 0x4b:				/* Exec */
		tprintf("exec(\"%Fs", makefptr(_ds, _dx));
		if (tracechildren)
			tputs("\") = ...\r\n");
		else
			tputs("\") = ");
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
		tprintf("exit(%d)\r\n", _ax & 0xff);
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
		tprintf("findfirst(\"%Fs\", %#x%s) = ", makefptr(_ds, _dx), _cx, strmode(_cx));
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
		if (_flags & 1)
			tprintf("Error %u\r\n", _ax);
		else {
			if (stringprint)
				outdta();
			else
				tputs("ok\r\n");
		}
		break;
	case 0x4f:				/* Findnext */
		tputs("findnext() = ");
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
		if (_flags & 1)
			tprintf("Error %u\r\n", _ax);
		else {
			if (stringprint)
				outdta();
			else
				tputs("ok\r\n");
		}
		break;
	case 0x5a:				/* Tmpfile */
		tprintf("tmpfile(\"%Fs\", %#x) = ", makefptr(_ds, _dx), _cx & 0xff);
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
			if (nameprint && fun <= 0x62)
				tputs(funcs[fun]);
			else
				tprintf("%02x", fun);
			if (regdump)
				tprintf(" :ax=%04X bx=%04X cx=%04X dx=%04X si=%04X di=%04X ds=%04X es=%04X\r\n", _ax, _bx, _cx, _dx, _si, _di, _ds, _es);
			else
				tputs("\r\n");
		}
	norm_proc:
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
	static char revstring[] = "$Revision: 1.14 $", revision[30];

	strcpy(revision, strchr(revstring, ':') + 2);
	*strchr(revision, '$') = '\0';
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
			if (datalen >= MAXBUFF)
				datalen = MAXBUFF - 1;
			break;
		case 'h':			/* Help */
			fputs(usagestring, stderr);
			fprintf(stderr, "Trace Version %s (C) Copyright 1991 D. Spinellis.  All rights reserved.\n", revision);
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
