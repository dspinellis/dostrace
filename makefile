OBJ=trace.obj doprnt.obj getopt.obj
CFLAGS=-Gs -ASw -Oi
CC=cl

all: trace.exe trace.lpr trace.man

trace.exe: $(OBJ)
	$(CC) $(CFLAGS) -o trace $(OBJ)

trace.lpr: trace.1
	nroff -Tepson -man trace.1 | sed -e "1,66d;s/_\(.\)/-1\1-0/g;s/-0-1//g;s/j\.J//g" >trace.lpr

trace.man: trace.1
	nroff -man trace.1 | sed -e "1,66d;s/.//g" >trace.man

trace.dvi:
	latex trace

clean:
	rm -f *.obj *.exe *.aux *.log *.dvi

zip:
	z -a trace trace.exe trace.man trace.c doprnt.c getopt.c makefile trace.1 article.ps

install: trace.exe
	exepack trace.exe \usr\local\bin\trace.exe

backup:
	xcopy *.c a:\trace /m
	xcopy *.exe a:\trace /m
	xcopy *.?%v a:\trace /m
	xcopy makefile a:\trace /m
	xcopy todo a:\trace /m
	xcopy trace.1 a:\trace /m
	xcopy article.tex a:\trace /m
	xcopy letter.fw3 a:\trace /m
