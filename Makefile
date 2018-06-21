#
# Use default rules for .f and .o files
#
.SUFFIXES: .F90 .f90 .F .f .o .mod
#
# Define macros
#
BINDIR = .
FLAGS = -c -m64 -O3
# -mcmodel=medium
# m-fdefault-real-8
#
# Objects
#
GRIBAPIINC = -I/usr/local/include
GRIBAPILIB = -L/usr/local/lib
GRIBAPI = -leccodes #-lgrib_api -lgrib_api_f77 -lgrib_api_f90

NETCDFINC = -I/usr/local/include
NETCDFLIB = -L/usr/local/lib
FTN = gfortran

NETCDF = -lnetcdff

mainobjects = domain.o constants.o T2.o diag.o surface.o prob.o sounding.o main.o
gfsobjects = domain.o constants.o T2.o diag.o gfs.o

#Default Rules
.F90.o:; $(FTN) $(FLAGS) $*.F90 $(NETCDFINC) $(NETCDFLIB) $(NETCDF)
.F90.mod:; $(FTN) $(FLAGS) $*.F90 $(NETCDFINC) $(NETCDFLIB) $(NETCDF)

# Generate executables

main.x: $(mainobjects)
		$(FTN) $(mainobjects) -o $(BINDIR)/$@ $(NETCDFINC) $(NETCDFLIB) $(NETCDF) $(GRIBAPIINC) $(GRIBAPILIB) $(GRIBAPI)

gfs.x: $(gfsobjects)
		$(FTN) $(gfsobjects) -o $(BINDIR)/$@ $(NETCDFINC) $(NETCDFLIB) $(NETCDF) $(GRIBAPIINC) $(GRIBAPILIB) $(GRIBAPI)


domain.mod: domain.o domain.F90
		$(FTN) $(FLAGS) domain.F90
domain.o: domain.F90
		$(FTN) $(FLAGS) domain.F90

constants.mod: constants.o constants.F90
		$(FTN) $(FLAGS) constants.F90
constants.o: constants.F90
		$(FTN) $(FLAGS) constants.F90

T2.mod: T2.o T2.F90
		$(FTN) $(FLAGS) T2.F90
T2.o: T2.F90
		$(FTN) $(FLAGS) T2.F90

diag.mod: diag.o diag.F90
		$(FTN) $(FLAGS) diag.F90
diag.o: diag.F90
		$(FTN) $(FLAGS) diag.F90

surface.mod: surface.o surface.F90
		$(FTN) $(FLAGS) surface.F90
surface.o: surface.F90
		$(FTN) $(FLAGS) surface.F90

prob.mod: prob.o prob.F90
		$(FTN) $(FLAGS) prob.F90
prob.o: prob.F90
		$(FTN) $(FLAGS) prob.F90

sounding.mod: sounding.o sounding.F90
		$(FTN) $(FLAGS) sounding.F90
sounding.o: sounding.F90
		$(FTN) $(FLAGS) sounding.F90

main.o: main.F90
		$(FTN) $(FLAGS) main.F90 $(NETCDFINC) $(NETCDFLIB) $(NETCDF)

gfs.o: gfs.F90
		$(FTN) $(FLAGS) gfs.F90 $(NETCDFINC) $(NETCDFLIB) $(NETCDF) $(GRIBAPIINC) $(GRIBAPILIB)

clean:
	rm -f *.o *.mod *.x
