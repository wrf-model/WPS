include $(DEV_TOP)/configure.wps

OBJS=   build_hdate.o geth_newdate.o geth_idts.o swap.o table.o parse_table.o \
        gbytesys.o gribcode.o read_namelist.o cio.o pregrid_version.o

LIBTARGET = pgu
TARGETDIR = ./
NCARG_LIBS=     -L$(NCARG_ROOT)/lib -L/usr/X11R6/lib \
        -lncarg -lncarg_gks -lncarg_c -lX11 -lm

all :	lib$(LIBTARGET).a gribprint.exe plotfmt.exe mod_levs.exe

lib$(LIBTARGET).a:	$(OBJS)
	$(RM) $(TARGETDIR)lib$(LIBTARGET).a
	$(AR) $(TARGETDIR)lib$(LIBTARGET).a $(OBJS)
	$(RANLIB) $(TARGETDIR)lib$(LIBTARGET).a

gribprint.exe:	gribprint.o lib$(LIBTARGET).a
	$(FC) -o $(@) $(LDFLAGS) gribprint.o lib$(LIBTARGET).a

plotfmt.exe:	plotfmt.o lib$(LIBTARGET).a
	$(FC) -o $(@) $(LDFLAGS) plotfmt.o lib$(LIBTARGET).a $(NCARG_LIBS) $(NCARG_LIBS2)

mod_levs.exe:	mod_levs.o
	$(FC) -o $(@) $(LDFLAGS) mod_levs.o

clean:
	$(RM) *.f *.o *.i *.mod *.M lib$(LIBTARGET).a *exe

superclean: clean
