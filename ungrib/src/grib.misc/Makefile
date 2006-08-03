include $(DEV_TOP)/configure.wps

OBJS	=	module_datarray.o gridinfo.o new_storage.o filelist.o ungrib.o output.o rrpr.o \
		rd_grib.o file_delete.o datint.o rd_grib2.o

EXE	=	ungrib.exe

all:		$(EXE)

$(EXE):		$(OBJS) ../../src/util/libpgu.a
	$(RM) $@
	$(FC) -o $@ $(LDFLAGS) $(OBJS) \
		-L ../../src/util -lpgu \
		-L ../../src/ngl -lw3 -lg2_4 \
		$(COMPRESSION_LIBS) 

.F.o:
	$(CPP) $(CPPFLAGS) $*.F > $*.f
	$(FC) -c $(FFLAGS) $*.f -I. -I../../src/util
	$(RM) $*.f

rd_grib2.o:
	$(CPP) $(CPPFLAGS) $*.F > $*.f
	$(FC) -c $(F77FLAGS) $*.f -I../../src/util -I../../src/ngl/g2
	$(RM) $*.f

clean:
	$(RM) $(EXE) $(OBJS) *.f *.i *.M *.mod

superclean:	clean
