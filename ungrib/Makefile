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
	( cd src/ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )
	( cd src/util ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )
	( cd src/grib.misc ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )

clean:
	( cd src/ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd src/util ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd src/grib.misc ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )

superclean: clean
