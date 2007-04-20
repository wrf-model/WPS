OBJS = cio.o plotgrid.o input_module.o misc_definitions_module.o module_debug.o parallel_module.o queue_module.o gridinfo_module.o wrf_debug.o

INCLUDEPATH = -I$(WRF_DIR)/external/ioapi_share -I$(WRF_DIR)/external/io_netcdf -I$(WRF_DIR)/external/io_grib1 -I$(WRF_DIR)/external/io_int

.IGNORE:
.SUFFIXES: .c .f90 .F90 .o

.c.o:
	rm -f $@
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

.f90.o:
	rm -f $@ $*.mod
	cp $< $*.f
	$(FC) $(FFLAGS) -c $*.f $(INCLUDEPATH)
	rm -f $*.f

.F90.o:
	rm -f $@ $*.mod
	cpp -C -P -traditional $(CPPFLAGS) $(INCLUDEPATH) $< > $*.f
	$(FC) $(FFLAGS) -c $*.f $(INCLUDEPATH)
	rm -f $*.f

all: plotgrid.exe

plotgrid.exe: $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(WRF_DIR)/frame/pack_utils.o $(WRF_DIR)/frame/module_machine.o $(WRF_DIR)/frame/module_internal_header_util.o -L$(NETCDF)/lib -I$(NETCDF)/include -I$(WRF_DIR)/external/io_grib1 -I$(WRF_DIR)/external/io_int -I$(WRF_DIR)/external/io_netcdf -L$(WRF_DIR)/external/io_netcdf -L$(WRF_DIR)/external/io_grib1 -L$(WRF_DIR)/external/io_int -lwrfio_nf -lwrfio_int -lio_grib1 -lnetcdf 

plotgrid.o: input_module.o plotgrid.f90

input_module.o: gridinfo_module.o misc_definitions_module.o module_debug.o parallel_module.o queue_module.o input_module.F90

module_debug.o: cio.o parallel_module.o module_debug.F90

gridinfo_module.o: misc_definitions_module.o module_debug.o gridinfo_module.F90

clean:
	rm -f $(OBJS) *.mod

clobber:
	make clean; rm -f plotgrid.exe
