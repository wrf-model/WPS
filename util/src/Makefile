include ../../configure.wps

OBJS = plotgrids.o avg_tsfc.o plotfmt.o rd_intermediate.o mod_levs.o cio.o gridinfo_module.o misc_definitions_module.o module_debug.o read_met_module.o write_met_module.o module_date_pack.o

all:
	clear ;
	@echo " "
	@echo "go up two directories and type compile to build WPS"
	@echo " "
	@echo " "

rd_intermediate.exe: rd_intermediate.o read_met_module.o module_debug.o
	$(SFC) -o $(@) $(LDFLAGS) rd_intermediate.o read_met_module.o module_debug.o misc_definitions_module.o cio.o

plotgrids.exe: plotgrids.o
	$(SFC) -o $(@) $(LDFLAGS) plotgrids.o $(NCARG_LIBS) $(NCARG_LIBS2)

avg_tsfc.exe: avg_tsfc.o cio.o module_debug.o gridinfo_module.o read_met_module.o write_met_module.o module_date_pack.o misc_definitions_module.o
	$(SFC) $(LDFLAGS) -o $@ avg_tsfc.o cio.o module_debug.o gridinfo_module.o read_met_module.o write_met_module.o module_date_pack.o misc_definitions_module.o

plotfmt.exe: plotfmt.o read_met_module.o
	$(SFC) -o $(@) $(LDFLAGS) plotfmt.o read_met_module.o module_debug.o misc_definitions_module.o cio.o \
	$(NCARG_LIBS) $(NCARG_LIBS2)

mod_levs.exe: mod_levs.o module_debug.o read_met_module.o write_met_module.o
	$(SFC) -o $(@) $(LDFLAGS) mod_levs.o module_debug.o read_met_module.o write_met_module.o misc_definitions_module.o cio.o

mod_levs.o: module_debug.o read_met_module.o write_met_module.o mod_levs.F90

plotfmt.o: plotfmt.F90 misc_definitions_module.o read_met_module.o
	$(CP) $< $*.f
	$(SFC) $(FNGFLAGS) -c $*.f
	$(RM) $*.f

plotgrids.o: plotgrids.f90
	$(CP) $< $*.f
	$(SFC) $(FNGFLAGS) -c $*.f
	$(RM) $*.f

rd_intermediate.o:      module_debug.o read_met_module.o rd_intermediate.F90

avg_tsfc.o: cio.o module_debug.o gridinfo_module.o read_met_module.o write_met_module.o module_date_pack.o misc_definitions_module.o

cio.o:

gridinfo_module.o: misc_definitions_module.o module_debug.o

misc_definitions_module.o:

module_debug.o: cio.o

read_met_module.o: module_debug.o misc_definitions_module.o

write_met_module.o: module_debug.o misc_definitions_module.o

module_date_pack.o:

clean:
	$(RM) $(OBJS) *.mod
	$(RM) plotgrids.exe plotfmt.exe avg_tsfc.exe rd_intermediate.exe mod_levs.exe

superclean:	clean
