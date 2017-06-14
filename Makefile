# Makefile

FC = gfortran
FFLAGS = -O2 -fbacktrace -fbounds-check

.SUFFIXES :
.SUFFIXES : .f90 .o

default: dlayxs_io.exe

.f90.o :
	$(FC) $(FFLAGS) -c $*.f90

dlayxs_io.exe: variables.o text_io.o dlayxs_io.o
	$(FC) $(FFLAGS) variables.o text_io.o dlayxs_io.o -o $@

clean:
	rm -f core *.o *.mod *.exe

query:
	cvs -n update -P

update:
	cvs update -P