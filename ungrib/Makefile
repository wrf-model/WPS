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
	( cd src ; $(MAKE) DEV_TOP="$(DEV_TOP)" CC="$(SCC)" FC="$(SFC)" all )

clean:
	( cd src/ngl ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )
	( cd src ; $(MAKE) DEV_TOP="$(DEV_TOP)" clean )

superclean: clean
