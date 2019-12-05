all: euler

primgen.o: primgen.c
	gcc -c primgen.c -I primegen-0.97

euler: euler.f90 primgen.o
	ifort -fpp -std08 -g -o euler euler.f90 primgen.o primegen.a
