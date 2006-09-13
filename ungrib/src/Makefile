include ../../configure.wps

LIBTARGET	=	pgu
EXE		=	ungrib.exe \
			g1print.exe g2print.exe

OBJS1		=	table.o module_datarray.o gridinfo.o new_storage.o filelist.o \
			ungrib.o output.o rrpr.o \
			rd_grib1.o file_delete.o datint.o rd_grib2.o 

OBJS2		=	build_hdate.o geth_newdate.o geth_idts.o swap.o table.o \
			parse_table.o gbytesys.o gribcode.o read_namelist.o cio.o

all:			lib$(LIBTARGET).a $(EXE)

ungrib.exe:		$(OBJS1) lib$(LIBTARGET).a
			$(RM) $@
			$(FC) -o $@ $(LDFLAGS) $(OBJS1) \
				-L./ngl -lw3 -lg2_4 \
				$(COMPRESSION_LIBS) \
				-L. -l$(LIBTARGET)

lib$(LIBTARGET).a:	$(OBJS2)
			$(RM) lib$(LIBTARGET).a
			$(AR) lib$(LIBTARGET).a $(OBJS2)
			$(RANLIB) lib$(LIBTARGET).a

g1print.exe:		g1print.o gribcode.o lib$(LIBTARGET).a
			$(FC) -o $(@) $(LDFLAGS) g1print.o lib$(LIBTARGET).a

g2print.exe:		gridinfo.o g2print.o
			$(FC) -o $(@) $(LDFLAGS) g2print.o \
				-L. -l$(LIBTARGET) \
				-L./ngl -lw3 -lg2_4 \
				$(COMPRESSION_LIBS)

g2print.o:		table.o gridinfo.o new_storage.o filelist.o module_datarray.o \
				ngl/g2/gribmod.o ngl/g2/params.o g2print.F90
			$(CPP) $(CPPFLAGS) $*.F90 > $*.f
			$(FC) -c $(FFLAGS) $*.f -I. -I./ngl/g2
			$(RM) $*.f

rd_grib2.o:		ngl/g2/gribmod.o table.o gridinfo.o ngl/g2/params.o new_storage.o \
				rd_grib2.F90
			$(CPP) $(CPPFLAGS) $*.F90 > $*.f
			$(FC) -c $(F77FLAGS) $*.f -I. -I./ngl/g2
			$(RM) $*.f

datint.o:		gridinfo.o new_storage.o datint.F90

new_storage.o:		gridinfo.o new_storage.F90

read_met_module.o:	module_debug.o misc_definitions_module.o read_met_module.f90

write_met_module.o:	module_debug.o misc_definitions_module.o write_met_module.f90

module_debug.o:		parallel_module.o mprintf.o module_debug.F90

output.o:		table.o gridinfo.o new_storage.o filelist.o output.F90

parse_table.o:		table.o parse_table.F90

rd_grib1.o:		table.o gridinfo.o module_datarray.o rd_grib1.F90

rrpr.o:			filelist.o gridinfo.o table.o new_storage.o rrpr.F90

.F90.o:
			$(CPP) $(CPPFLAGS) $(FDEFS) $< > $*.f
			$(FC) -c $(FFLAGS) $*.f
			$(RM) $*.f

.f90.o:
			$(CPP) $(CPPFLAGS) $(FDEFS) $< > $*.f
			$(FC) -c $(FFLAGS) $*.f
			$(RM) $*.f
.c.o:
	$(RM) $@
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<


clean:
			$(RM) *.f *.o *.i *.mod *.M lib$(LIBTARGET).a *exe

superclean:		clean
