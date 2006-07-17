include $(DEV_TOP)/configure.wps

bad_idea:
	clear
	@echo ' *************** '
	@echo " "
	@echo " "
	@echo "Go up a directory and type 'compile'"
	@echo " "
	@echo " "
	@echo ' *************** '

all:
	( cd ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )
	( cd util ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )
	( cd grib.misc ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )

clean:
	( cd ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd util ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd grib.misc ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )

superclean: clean
