include ../../configure.wps

LIBTARGET	=	pgu
EXE		=	ungrib.exe \
			g1print.exe g2print.exe

OBJS1		=	misc_definitions_module.o debug_cio.o module_debug.o module_stringutil.o \
			table.o module_datarray.o gridinfo.o new_storage.o filelist.o \
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

g1print.exe:		g1print.o gribcode.o module_debug.o debug_cio.o lib$(LIBTARGET).a
			$(FC) -o $(@) $(LDFLAGS) g1print.o gribcode.o module_debug.o debug_cio.o lib$(LIBTARGET).a

g2print.exe:		filelist.o gridinfo.o g2print.o
			$(FC) -o $(@) $(LDFLAGS) g2print.o \
				filelist.o gridinfo.o \
				-L. -l$(LIBTARGET) \
				-L./ngl -lw3 -lg2_4 \
				$(COMPRESSION_LIBS)

g2print.o:		table.o gridinfo.o new_storage.o filelist.o module_datarray.o \
				ngl/g2/gribmod.o ngl/g2/params.o g2print.F90
			$(CPP) $(CPPFLAGS) $*.F90 > $*.f
			$(FC) -c $(FFLAGS) $*.f -I. -I./ngl/g2
			$(RM) $*.f

rd_grib2.o:		ngl/g2/gribmod.o module_debug.o table.o gridinfo.o ngl/g2/params.o new_storage.o \
				rd_grib2.F90
			$(CPP) $(CPPFLAGS) $*.F90 > $*.f
			$(FC) -c $(F77FLAGS) $*.f -I. -I./ngl/g2
			$(RM) $*.f

datint.o:		misc_definitions_module.o module_debug.o gridinfo.o new_storage.o datint.F90

module_debug.o:		debug_cio.o module_debug.F90

misc_definitions_module.o: misc_definitions_module.f90

file_delete.o: 		misc_definitions_module.o file_delete.F90

new_storage.o:		gridinfo.o new_storage.F90

output.o:		misc_definitions_module.o module_debug.o table.o gridinfo.o module_stringutil.o new_storage.o filelist.o output.F90

parse_table.o:		module_debug.o table.o parse_table.F90

rd_grib1.o:		module_debug.o table.o gridinfo.o module_datarray.o rd_grib1.F90

gribcode.o:		module_debug.o gribcode.F90

rrpr.o:			misc_definitions_module.o module_debug.o filelist.o gridinfo.o module_stringutil.o table.o new_storage.o rrpr.F90

read_namelist.o:	misc_definitions_module.o module_debug.o read_namelist.F90

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
