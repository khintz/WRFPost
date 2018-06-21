### GIT REPO for WRF post-processing


## Compile WPS and WRF

# Dependencies
* WRF forecasting system
* HDF5
* NetCDF
* NetCDF4-python
* GRIB API ECMWF (ECCODES)

# Dependencies to WRF
* zlib
* szip
* m4
* jpeg
* jasper
* hdf5
* udunits2
* netcdf4
* ncview
* mpich (can be replaced with qsub, aprun, openmp etc.)

* Fortran compiler (gfortran / ifort etc.)
* C compiler (gcc etc.)

Dependent libraries must be compiled with the same compiler as the one which is being used to compile WRF and WPS.

# Configure and compiling settings for Intel compiler
## Build dependent libraries
All intel compiled libraries are installed in /opt/local as all GNU libs are installed in /usr/local
## libpng
* ./configure --prefix=/opt/local CC=icc FC=ifort
* make
* make install
* make check
## zlib
* ./configure --prefix=/opt/local
zlibs configuration setup does not know the options CC and FC, so one have to change it in the Makefile manually after ./configure
* make
* make install
* make check
## szip
* ./configure â€”prefix=/opt/local CC=icc FC=ifortâ€¨
* make
* make install
* make check
# jpeg
* ./configure --prefix=/opt/local CC=icc FC=ifort
* make
* make install
* make check
* add /opt/local and /opt/local/lib to the source shell script
* export LD_LIBRARY_PATH=/opt/local:/opt/local/lib:$LD_LIBRARY_PATH
# jasper
* ./configure --prefix=/opt/local CC=icc FC=ifort CFLAGS=-fPIC
* make
* make install
* make check
# hdf5
* ./configure --prefix=/opt/local CC=icc FC=ifort CPPFLAGS=-I/opt/local/include LDFLAGS=-L/opt/local/lib
* make
* make install
* make check
* It can sometimes be necessary to compile hdf5 with lower optimization level than -O3 (maybe even with no optimization).
# netcdf-c
* ./configure --prefix=/opt/local CC=icc FC=ifort CPPFLAGS=-I/opt/local/include LDFLAGS=-L/opt/local/lib CFLAGS=-I/opt/local/lib
* make
* make install
* make check
# netcdf-fortran
* ./configure --prefix=/opt/local CC=icc FC=ifort CPPFLAGS=-I/opt/local/include LDFLAGS=-L/opt/local/lib CFLAGS=-I/opt/local/lib
* make
* make install
* make check

# Configure and compile WRF
* Configuration settings
icc+ifort with dmpar (not cray/xtn but "clean" icc and ifort)
NETCDFPATH changed to /opt/local in configure.wrf because it automatically linked to /usr/local where the GNU version is installed.

# Configure and compile WPS
If metgrid.exe and geogrid.exe is not created when compiling WPS it can help to add "-lgomp" to WRF_LIB after -lnetcdf when using GNU. For Intel it can help to add "-liomp5 -lpthread".
Remember to export NETCDF=path and JASPERLIB=path and JASPERINC=path ("include path") and source before running configure WPS.

# Errors and possible solutions
With GNU the executables are sometimes not build. If using a parallel setup make sure that the ""-DMPI2_SUPPORT" flag is included in "DM_CC" option in configure.wrf

