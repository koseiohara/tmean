EXE = timeMean

OBJS = NaNchecker.o fileio.o calendar.o clock.o globals.o namelist.o tmean.o main.o

FC = ifort
FLAGS = -assume byterecl -convert little_endian -warn all -O0

%.o : %.f90
	${FC} -c $< ${FLAGS}

%.f90~ : %.f90
	cp $< ../backup/$@

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^


.PHONY : clean re run backup

clean :
	@ rm -fv *.o *.mod ${EXE}

re : clean all

run :
	./${EXE}

backup : ${OBJS:.o=.f90~}

