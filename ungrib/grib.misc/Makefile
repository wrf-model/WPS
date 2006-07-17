include $(DEV_TOP)/configure.wps

OBJS	=	gridinfo.o new_storage.o filelist.o ungrib.o output.o rrpr.o \
		rd_grib.o file_delete.o datint.o rd_grib2.o

EXE	=	ungrib.exe

all:		$(EXE)

$(EXE):		$(OBJS) ../util/libpgu.a
	$(RM) $@
	$(FC) -o $@ $(LDFLAGS) $(OBJS) \
		-L ../util -lpgu \
		-L ../ngl -lw3 -lg2_4 \
		$(COMPRESSION_LIBS) 

.F.o:
	$(CPP) $(CPPFLAGS) $*.F >! $*.f
	$(FC) -c $(FFLAGS) $*.f -I. -I../util
	$(RM) $*.f

rd_grib2.o:
	$(CPP) $(CPPFLAGS) $*.F >! $*.f
	$(FC) -c $(F77FLAGS) $*.f -I../util -I../ngl/g2
	$(RM) $*.f

clean:
	$(RM) $(EXE) $(OBJS) *.f *.i *.M *.mod

superclean:	clean
