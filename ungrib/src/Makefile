include $(DEV_TOP)/configure.wps

LIBTARGET	=	pgu
EXE		=	ungrib.exe \
			g1print.exe g2print.exe \
			plotfmt.exe mod_levs.exe rd_intermediate.exe

OBJS1		=	module_datarray.o gridinfo.o new_storage.o filelist.o \
			ungrib.o output.o rrpr.o \
			rd_grib1.o file_delete.o datint.o rd_grib2.o 

OBJS2		=	build_hdate.o geth_newdate.o geth_idts.o swap.o table.o \
			parse_table.o gbytesys.o gribcode.o read_namelist.o cio.o

all:			lib$(LIBTARGET).a $(EXE)

ungrib.exe:		$(OBJS1) ./libpgu.a
			$(RM) $@
			$(FC) -o $@ $(LDFLAGS) $(OBJS1) \
				-L./ngl -lw3 -lg2_4 \
				$(COMPRESSION_LIBS) \
				-L. -lpgu

lib$(LIBTARGET).a:	$(OBJS2)
			$(RM) lib$(LIBTARGET).a
			$(AR) lib$(LIBTARGET).a $(OBJS2)
			$(RANLIB) lib$(LIBTARGET).a

g1print.exe:		g1print.o gribcode.o lib$(LIBTARGET).a
			$(FC) -o $(@) $(LDFLAGS) g1print.o lib$(LIBTARGET).a

plotfmt.exe:		plotfmt.o lib$(LIBTARGET).a
			$(FC) -o $(@) $(LDFLAGS) plotfmt.o \
				lib$(LIBTARGET).a $(NCARG_LIBS) $(NCARG_LIBS2)

plotfmt.o: 		plotfmt.F90
			$(CPP) $(CPPFLAGS) -I. $< > $*.f
			$(FC) $(FNGFLAGS) -c $*.f
			$(RM) $*.f

mod_levs.exe:		mod_levs.o
			$(FC) -o $(@) $(LDFLAGS) mod_levs.o

rd_intermediate.exe:	rd_intermediate.o read_met_module.o module_debug.o
			$(FC) -o $(@) $(LDFLAGS) rd_intermediate.o read_met_module.o module_debug.o misc_definitions_module.o parallel_module.o mprintf.o

g2print.exe:		gridinfo.o g2print.o
			$(FC) -o $(@) $(LDFLAGS) g2print.o \
				-L. -lpgu \
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

rd_intermediate.o:	module_debug.o read_met_module.o rd_intermediate.F90

new_storage.o:		gridinfo.o new_storage.F90

read_met_module.o:	module_debug.o misc_definitions_module.o read_met_module.f90

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
