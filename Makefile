PROGRAM=isen_cold_air


INSTALL=~/bin



##### Fujitsu f90 #####
#FC=f90
#OPTION=-Am -X9 -Kfast -Et#                  # optimized
#OPTION=-Am -X9 -O0#                         # no optimized, safe
#OPTION=-Am -X9 -Et -O0 -Haesux#             # for debug

##### Intel ifort #####
#FC=ifort
#OPTION=-assume byterecl -03 -warn all -module src/  #optimized
#OPTION=-assume byterecl -O0 -warn all -heap-arrays -module src/ #no optimized
#OPTION=-assume byterecl -C -warn all -module src/ # for debug

##### gfortran
FC=gfortran
OPTION=-frecord-marker=4 -O3 -Wall -fbacktrace -g -fcheck=all -fbounds-check

OBJS=
OBJS_MODULE=

include src/Makefile
OBJS += ${OBJS_SRC}
OBJS_MODULE += ${OBJS_MODULE_SRC}




${PROGRAM} : ${OBJS_MODULE} ${OBJS}
	${FC} ${OPTION} ${OBJS_MODULE} ${OBJS} -o $@

.SUFFIXES : .o .f90
.f90.o : 
	${FC} ${OPTION} -c $< -o $@

all : ${PROGRAM}


clean:
	rm -rf ${OBJS_MODULE} src/*.mod ${OBJS} ${PROGRAM} src/*genmod* 

