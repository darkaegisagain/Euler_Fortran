all: euler

euler: euler.f90
	ifort -std08 -g -o euler euler.f90

