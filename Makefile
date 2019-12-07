all: euler

euler: euler.f90
	ifort -fpp -std08 -g -o euler euler.f90
