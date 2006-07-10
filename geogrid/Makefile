#	WPS component makefile

include ../configure.wps

bad_idea:
	clear ;
	@echo " "
	@echo "go up a directory and type compile to build WPS"
	@echo " "
	@echo " "

all:
	( cd src ; \
	$(MAKE) $(TARGET) \
		WRF_DIR="$(DEV_TOP)/$(WRF_DIR)" \
		FC="$(FC)" \
		CC="$(CC)" \
		CPP="$(CPP)" \
		FFLAGS="$(FFLAGS)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		CPPFLAGS="$(CPPFLAGS) -D_$(CPP_TARGET)" )
	if [ -e $(TARGET) ] ; then \
		$(RM) $(TARGET) ; \
	fi ; \
	if [ -e ../$(TARGET) ] ; then \
		$(RM) ../$(TARGET) ; \
	fi ; \
	if [ -e src/$(TARGET) ] ; then \
		$(LN) src/$(TARGET) . ; \
	fi

clean:
	( cd src ; $(MAKE) clean )

superclean:
	if [ -e $(TARGET) ] ; then \
		$(RM) $(TARGET) ; \
	fi
	if [ -e ../$(TARGET) ] ; then \
		$(RM) ../$(TARGET) ; \
	fi
	( cd src ; $(MAKE) superclean )
