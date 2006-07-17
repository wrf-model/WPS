include $(DEV_TOP)/configure.wps

LIB	= libw3.a

# Make sure one of the following options appears in your CFLAGS 
# variable to indicate which system you are on.
# -DLINUX, -DSGI, -DHP, -DCRAY90, -DIBM4, -DIBM8, -DLINUXF90 

# OPTIONS FOR IBM-SP 
#F77	= xlf
#FFLAGS	= -O3 -qnosave -qarch=auto
#CFLAGS	= -DIBM4 -q64
#ARFLAGS	= -X64
#CC	= cc

# OPTIONS FOR LINUX pgi
#F77	= pgf90
#FFLAGS	= 
#CFLAGS	= -O -DLINUX
#ARFLAGS	= 
#CC	= cc

# OPTIONS FOR VPP5000
#F77     = frt
#FFLAGS  = -X9 -Ad -Sw -Am -Of -Wv,-Of
#CFLAGS  = -O -K4 -DVPP5000
#CC      = vcc

OBJ_MOD	=	bacio_module.o

OBJS	=	getgb.o     getgbmh.o   putgbex.o   w3fi73.o \
		getgb1r.o   getgbmp.o   putgbn.o    w3fi74.o \
		getgb1re.o  getgbp.o    r63w72.o    w3fi75.o \
		getgb1s.o   getgi.o     sbyte.o     w3fi76.o \
		getgbe.o    getgir.o    sbytes.o    w3fi82.o \
		getgbeh.o   idsdef.o    skgb.o      w3fi83.o \
		getgbem.o   iw3jdn.o    w3difdat.o  w3fs26.o \
		getgbemh.o  ixgb.o      w3doxdat.o  w3log.o \
		getgbemp.o  lengds.o    w3fi01.o    w3movdat.o \
		getgbens.o  pdsens.o    w3fi58.o    w3reddat.o \
		getgbep.o   pdseup.o    w3fi59.o    w3tagb.o \
		errmsg.o    getgbex.o   putgb.o     w3fi63.o \
		gbytes.o    getgbexm.o  putgbe.o    w3fi68.o \
		gbytes_char.o           getgbh.o    putgben.o \
		getbit.o    getgbm.o    putgbens.o  w3fi72.o \
		fparsei.o   fparser.o   instrument.o \
		start.o     summary.o   w3utcdat.o  w3fs21.o \
		w3locdat.o  w3fi71.o    baciof.o

OBJS_CC	=	bacio.v1.3.o errexit.o

SRC	=	$(OBJ_MOD:.o=.f) $(OBJS:.o=.f) $(OBJS_CC:.o=.c)

all: $(LIB)

$(LIB): $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	$(AR) $(ARFLAGS) $(LIB) $(OBJ_MOD) $(OBJS) $(OBJS_CC)

clean:
	$(RM) $(OBJ_MOD) $(OBJS) $(OBJS_CC) *.mod *.a

.f.o:
	$(RM) $*.o
	$(FC) $(F77FLAGS) -c $< 

.c.o:
	$(RM) $*.o
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< 
