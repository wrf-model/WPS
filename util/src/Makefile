include ../../configure.wps

OBJS	=	plotgrids.o avg_tsfc.o plotfmt.o rd_intermediate.o mod_levs.o \
		cio.o gridinfo_module.o misc_definitions_module.o module_debug.o \
		read_met_module.o write_met_module.o module_date_pack.o

all:
		clear ;
		@echo " "
		@echo "go up two directories and type './compile util' to build WPS utilities"
		@echo " "
		@echo " "

rd_intermediate.exe: rd_intermediate.o read_met_module.o module_debug.o misc_definitions_module.o
	$(SFC) -o $(@) $(LDFLAGS) rd_intermediate.o read_met_module.o \
		module_debug.o misc_definitions_module.o cio.o

rd_intermediate.o: module_debug.o read_met_module.o rd_intermediate.F90

plotgrids.exe: plotgrids.o
	$(SFC) -o $(@) $(LDFLAGS) module_map_utils.o module_debug.o cio.o constants_module.o misc_definitions_module.o \
		plotgrids.o $(NCARG_LIBS) $(NCARG_LIBS2)

plotgrids.o: plotgrids.f90 module_map_utils.o
	$(CP) $< $*.f
	$(SFC) $(FNGFLAGS) -c $*.f
	$(RM) $*.f

avg_tsfc.exe: avg_tsfc.o cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o
	$(SFC) $(LDFLAGS) -o $@ avg_tsfc.o cio.o module_debug.o gridinfo_module.o \
		read_met_module.o write_met_module.o module_date_pack.o \
		misc_definitions_module.o

avg_tsfc.o: avg_tsfc.F90 cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o

plotfmt.exe: plotfmt.o read_met_module.o
	$(SFC) -o $(@) $(LDFLAGS) plotfmt.o read_met_module.o module_debug.o \
		misc_definitions_module.o cio.o \
		$(NCARG_LIBS) $(NCARG_LIBS2)

plotfmt.o: plotfmt.F90 misc_definitions_module.o read_met_module.o
	$(CP) $< $*.f
	$(SFC) $(FNGFLAGS) -c $*.f
	$(RM) $*.f

mod_levs.exe: mod_levs.o module_debug.o read_met_module.o write_met_module.o misc_definitions_module.o 
	$(SFC) -o $(@) $(LDFLAGS) mod_levs.o module_debug.o read_met_module.o \
		write_met_module.o misc_definitions_module.o cio.o

mod_levs.o: module_debug.o read_met_module.o write_met_module.o mod_levs.F90

module_map_utils.o: module_map_utils.f90 misc_definitions_module.o constants_module.o module_debug.o

cio.o: cio.c

gridinfo_module.o: gridinfo_module.F90 misc_definitions_module.o module_debug.o

misc_definitions_module.o: misc_definitions_module.f90

module_debug.o: module_debug.F90 cio.o

read_met_module.o: read_met_module.F90 constants_module.o module_debug.o misc_definitions_module.o

write_met_module.o: write_met_module.F90 module_debug.o misc_definitions_module.o

module_date_pack.o: module_date_pack.f90

clean:
	$(RM) $(OBJS) *.mod
	$(RM) plotgrids.exe plotfmt.exe avg_tsfc.exe rd_intermediate.exe mod_levs.exe

superclean:	clean
