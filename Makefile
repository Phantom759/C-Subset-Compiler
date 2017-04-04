CC = gcc
CFLAGS = -Wall -O2 -g

compile : compile.l compile.y
	bison -dv compile.y
	flex -o compile.lex.c compile.l
	cc -o compile compile.tab.c compile.lex.c

.PHONY : clean
clean:
	/bin/rm -f compile lex.yy.c compile.tab.c compile.tab.h a.out

