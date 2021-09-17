cwd = $(shell pwd)
override MAKEFLAGS:=

all:
	@echo "CALLING ZLIB CONFIGURE AND MAKE"
	@echo $(MAKEFLAGS)
	( cd zlib-1.2.7 ; CC=$(SCC) CPP=/usr/bin/cpp ./configure --prefix=$(cwd) && $(MAKE) && $(MAKE) install )
	@echo "CALLING LIBPNG CONFIGURE AND MAKE"
	( cd libpng-1.2.50 ; CC=$(SCC) CPP=/usr/bin/cpp CPPFLAGS="-I../zlib-1.2.7" LDFLAGS="-L../zlib-1.2.7" ./configure --prefix=$(cwd) && $(MAKE) && $(MAKE) install )
	@echo "CALLING JASPER CONFIGURE AND MAKE"
	( cd jasper-1.900.1 ; CC=$(SCC) CPP=/usr/bin/cpp ./configure --prefix=$(cwd) && $(MAKE) && $(MAKE) install )

clean:
	( cd zlib-1.2.7 ; $(MAKE) clean )
	( cd libpng-1.2.50 ; $(MAKE) clean )
	( cd jasper-1.900.1 ; $(MAKE) clean )
	( rm -rf ./share ; rm -rf ./include ; rm -rf ./lib ; rm -rf ./bin ; rm -rf ./man )
