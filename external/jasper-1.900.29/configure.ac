dnl ****************************************************************************
dnl Copyright (c) 2001-2004 Michael David Adams.
dnl All rights reserved.
dnl
dnl The complete licensing terms for this software can be found in the file
dnl named "LICENSE" in this software distribution.
dnl ****************************************************************************

dnl The package name.
define(AC_JAS_NAME, jasper)

dnl ************************************************************
dnl Package version information.
dnl ************************************************************

dnl The major, minor, and micro version numbers of the package.
define(AC_JAS_MAJOR_VERSION, 1)
define(AC_JAS_MINOR_VERSION, 900)
define(AC_JAS_MICRO_VERSION, 29)

dnl The package version.
define(AC_JAS_VERSION,
  AC_JAS_MAJOR_VERSION.AC_JAS_MINOR_VERSION.AC_JAS_MICRO_VERSION)

dnl ************************************************************
dnl RPM version information.
dnl ************************************************************

define(AC_JAS_RPM_RELEASE, 1)

dnl ************************************************************
dnl Dynamically-linked library version information (for Libtool).
dnl ************************************************************

dnl Guidelines for updating these definitions:
dnl 1) If the library source code has changed at all since the last update,
dnl then increment REVISION.
dnl 2) If any interfaces have been added, removed, or changed since the last
dnl update, increment CURRENT, and set REVISION to zero.
dnl 3) If any interfaces have been added since the last public release, then
dnl increment AGE.
dnl 4) If any interfaces have been removed since the last public release, then
dnl set AGE to zero.

dnl The most recent interface number that this library implements.
define(AC_JAS_LT_CURRENT, 4)

dnl The implementation number of the current interface.
define(AC_JAS_LT_REVISION, 0)

dnl The difference between the newest and oldest interfaces that this library
dnl implements.  In other words, the library implements all of the interface
dnl numbers in the range from current - age to current.
define(AC_JAS_LT_AGE, 0)

dnl The release number of the library.
define(AC_JAS_LT_RELEASE, AC_JAS_MAJOR_VERSION.AC_JAS_MINOR_VERSION)

dnl ****************************************************************************
dnl NOTHING BELOW THIS LINE SHOULD REQUIRE MODIFICATION (IDEALLY :-)
dnl ****************************************************************************

dnl Initialize autoconf.
AC_INIT(AC_JAS_NAME, AC_JAS_VERSION)
AC_PREREQ(2.59)
AC_CONFIG_SRCDIR(README)
dnl Note: Avoid the temptation to call the auxiliary directory "aux" because
dnl Microsoft Windows will barf if you try to use "aux" as a directory name. :-(
AC_CONFIG_AUX_DIR([ac_aux])
AC_CONFIG_MACRO_DIR([ac_m4])

dnl Initialize automake.
AM_INIT_AUTOMAKE([1.11 foreign dist-xz dist-bzip2])

m4_include([ac_m4/m4_ax_generate_changelog.m4])
m4_include([ac_m4/m4_ax_cflags_gcc_option.m4])
m4_include([ac_m4/m4_ax_check_compile_flag.m4])
m4_include([ac_m4/m4_ax_append_flag.m4])

AX_GENERATE_CHANGELOG

# Initialize libtool.
LT_INIT([shared static])

# Detect the canonical host and target build environment.
# Note: This precedes automake initialization to avoid warnings.
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

# Initialize version information parameters.
JAS_MAJOR_VERSION=AC_JAS_MAJOR_VERSION
JAS_MINOR_VERSION=AC_JAS_MINOR_VERSION
JAS_MICRO_VERSION=AC_JAS_MICRO_VERSION
JAS_VERSION=AC_JAS_VERSION
AC_SUBST(JAS_MAJOR_VERSION)
AC_SUBST(JAS_MINOR_VERSION)
AC_SUBST(JAS_MICRO_VERSION)
AC_SUBST(JAS_VERSION)
AC_DEFINE_UNQUOTED(JAS_VERSION, "$JAS_VERSION")
JAS_RPM_RELEASE=AC_JAS_RPM_RELEASE
AC_SUBST(JAS_RPM_RELEASE)

# Initialize version information for libtool.
LT_RELEASE=AC_JAS_LT_RELEASE
LT_CURRENT=AC_JAS_LT_CURRENT
LT_REVISION=AC_JAS_LT_REVISION
LT_AGE=AC_JAS_LT_AGE
AC_SUBST(LT_RELEASE)
AC_SUBST(LT_CURRENT)
AC_SUBST(LT_REVISION)
AC_SUBST(LT_AGE)

# Indicate a configure-based build is being used.
AC_DEFINE(JAS_CONFIGURE)

############################################################
# Configuration header file.
############################################################

AH_TEMPLATE([DEBUG_OVERFLOW], [Debugging overflow detection])
AH_TEMPLATE([JAS_CONFIGURE], [JasPer configure])
AH_TEMPLATE([JAS_VERSION], [JasPer version])
AH_TEMPLATE([DEBUG], [Extra debugging support])

AH_TOP([
/* Avoid problems due to multiple inclusion. */
#ifndef JAS_CONFIG_H
#define JAS_CONFIG_H

/* This preprocessor symbol identifies the version of JasPer. */
#undef	JAS_VERSION
/* If configure is being used, this symbol will be defined automatically
  at this point in the configuration header file. */

/* The preprocessor symbol JAS_WIN_MSVC_BUILD should not be defined
  unless the JasPer software is being built under Microsoft Windows
  using Microsoft Visual C. */
#if !defined(JAS_WIN_MSVC_BUILD)
/* A configure-based build is being used. */

])

AH_BOTTOM([
#else
/* A configure-based build is not being used. */
#include <jasper/jas_pconf2.h>
#endif

#endif
])

############################################################
# Checks for programs.
############################################################

AC_PROG_CC

# In the ideal world, AC_PROG_CC_C11 would exist and work.
#AC_PROG_CC_C11

# Attempt to enable C11 support if available.
# If C11 support is not available, attempt to fall back on C99 support
# if available.
AX_CHECK_COMPILE_FLAG([-std=c11], [AX_APPEND_FLAG([-std=c11])], [
  AX_CHECK_COMPILE_FLAG([-std=c99], [AX_APPEND_FLAG([-std=c99])], [])
])

AC_PROG_INSTALL
# AC_PROG_AWK
# AC_PROG_CPP
# AC_PROG_LN_S

AM_DISABLE_SHARED
AC_PROG_LIBTOOL

AC_PATH_XTRA

############################################################
# Add a few other compiler flags.
############################################################

if test "$GCC" = yes; then
	CFLAGS="$CFLAGS -pedantic"
	CFLAGS="$CFLAGS -Wall -Wno-unused"
	#CFLAGS="$CFLAGS -W -Wformat -Wmissing-prototypes -Wstrict-prototypes"
fi

AC_ARG_ENABLE(strict,
[  --disable-strict    Disable pedantic errors.], [
case "${enableval}" in
yes)
	ENABLE_STRICT=yes;;
no)
	ENABLE_STRICT=no;;
*)
	AC_MSG_ERROR(bad value ${enableval} for --disable-strict)
	;;
esac
], [
	ENABLE_STRICT=yes
])
if test $ENABLE_STRICT = yes; then
	if test "$GCC" = yes; then
		AX_APPEND_FLAG([-pedantic-errors])
	fi
fi

############################################################
# Check for the IJG JPEG library.
############################################################

# Allow the user to explicitly specify whether the JPEG library is present.
AC_ARG_ENABLE(libjpeg,
[  --disable-libjpeg    Force the IJG JPEG library to be ignored],
[
case "${enableval}" in
yes)
	ENABLE_LIBJPEG=yes;;
no)
	ENABLE_LIBJPEG=no;;
*)
	AC_MSG_ERROR(bad value ${enableval} for --disable-libjpeg)
	;;
esac
], ENABLE_LIBJPEG=yes)

HAVE_LIBJPEG=no
if test $ENABLE_LIBJPEG = yes; then
	# Check for the JPEG library.
	AC_CHECK_LIB(jpeg, jpeg_destroy, HAVE_LIBJPEG=yes, HAVE_LIBJPEG=no)
	if test $HAVE_LIBJPEG = yes; then
		LIBS="$LIBS -ljpeg"
	fi
fi
AM_CONDITIONAL(HAVE_LIBJPEG, test $HAVE_LIBJPEG = yes)

############################################################
# Check for OpenGL libraries.
############################################################

AC_ARG_WITH(glut-include-dir,
[  --with-glut-include-dir=DIR    Set GLUT include directory.],
[
	case "$withval" in
	/*)
		CFLAGS="$CFLAGS -I$withval"
		CPPFLAGS="$CPPFLAGS -I$withval";;
	*)
		AC_MSG_ERROR(--with-glut-include-dir requires absolute path)
		;;
	esac
],
)
AC_ARG_WITH(glut-lib-dir,
[  --with-glut-lib-dir=DIR        Set GLUT library directory.],
[
	case "$withval" in
	/*)
		LIBS="$LIBS -L$withval";;
	*)
		AC_MSG_ERROR(--with-glut-lib-dir requires absolute path)
		;;
	esac
],
)

AC_ARG_ENABLE(opengl,
[  --disable-opengl    Disable use of OpenGL. ],
[
	case "$enableval" in
	yes)
		ENABLE_OPENGL=yes;;
	no)
		ENABLE_OPENGL=no;;
	*)
		AC_MSG_ERROR(bad value ${enableval} for --disable-libjpeg)
		;;
	esac
],
ENABLE_OPENGL=yes)

HAVE_OPENGL=no
OPENGL_LIBS=""
if test $ENABLE_OPENGL = yes; then
	if test $HAVE_OPENGL = no; then
		TMPLIBS="-lglut -lGL -lGLU"
		AC_CHECK_LIB(glut, glutInit, [HAVE_OPENGL=yes;
		  OPENGL_LIBS=$TMPLIBS], HAVE_OPENGL=no, $TMPLIBS)
	fi
	AC_CHECK_HEADER(GL/glut.h,, [HAVE_OPENGL=no; OPENGL_LIBS="";])
fi

AM_CONDITIONAL(HAVE_OPENGL, test $HAVE_OPENGL = yes)
AC_SUBST(OPENGL_LIBS)
AC_MSG_CHECKING([whether OpenGL libraries and headers were detected])
AC_MSG_RESULT($HAVE_OPENGL)

############################################################
# Check for other libraries.
############################################################

# Check for the math library.
AC_CHECK_LIB(m, main)

############################################################
# Check for header files.
############################################################

dnl AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h io.h unistd.h windows.h sys/time.h sys/types.h)

# Surely, there must be a better way to do this, but
# let's just get something working for now.

if test $ac_cv_header_fcntl_h = yes; then
	AC_SUBST([JAS_HAVE_FCNTL_H], ["#define JAS_HAVE_FCNTL_H 1"])
else
	AC_SUBST([JAS_HAVE_FCNTL_H], ["#undef JAS_HAVE_FCNTL_H"])
fi

if test $ac_cv_header_io_h = yes; then
	AC_SUBST([JAS_HAVE_IO_H], ["#define JAS_HAVE_IO_H 1"])
else
	AC_SUBST([JAS_HAVE_IO_H], ["#undef JAS_HAVE_IO_H"])
fi

if test $ac_cv_header_unistd_h = yes; then
	AC_SUBST([JAS_HAVE_UNISTD_H], ["#define JAS_HAVE_UNISTD_H 1"])
else
	AC_SUBST([JAS_HAVE_UNISTD_H], ["#undef JAS_HAVE_UNISTD_H"])
fi

if test $ac_cv_header_windows_h = yes; then
	AC_SUBST([JAS_HAVE_WINDOWS_H], ["#define JAS_HAVE_WINDOWS_H 1"])
else
	AC_SUBST([JAS_HAVE_WINDOWS_H], ["#undef JAS_HAVE_WINDOWS_H"])
fi

if test $ac_cv_header_sys_time_h = yes; then
	AC_SUBST([JAS_HAVE_SYS_TIME_H], ["#define JAS_HAVE_SYS_TIME_H 1"])
else
	AC_SUBST([JAS_HAVE_SYS_TIME_H], ["#undef JAS_HAVE_SYS_TIME_H"])
fi

if test $ac_cv_header_sys_types_h = yes; then
	AC_SUBST([JAS_HAVE_SYS_TYPES_H], ["#define JAS_HAVE_SYS_TYPES_H 1"])
else
	AC_SUBST([JAS_HAVE_SYS_TYPES_H], ["#undef JAS_HAVE_SYS_TYPES_H"])
fi

############################################################
# Check for typedefs, structures, and compiler characteristics.
############################################################

dnl AC_C_CONST
dnl AC_C_INLINE
dnl AC_TYPE_SIZE_T
dnl AC_CHECK_TYPE(uchar, unsigned char)
dnl AC_CHECK_TYPE(ushort, unsigned short)
dnl AC_CHECK_TYPE(uint, unsigned int)
dnl AC_CHECK_TYPE(ulong, unsigned long)
dnl AC_CHECK_TYPE(longlong, long long)
dnl AC_CHECK_TYPE(ulonglong, unsigned long long)
dnl AC_CHECK_TYPE(ssize_t, int)
AC_CHECK_FUNCS(gettimeofday)
AC_CHECK_FUNCS(getrusage)

if test $ac_cv_func_gettimeofday = yes; then
	AC_SUBST([JAS_HAVE_GETTIMEOFDAY], ["#define JAS_HAVE_GETTIMEOFDAY 1"])
else
	AC_SUBST([JAS_HAVE_GETTIMEOFDAY], ["#undef JAS_HAVE_GETTIMEOFDAY"])
fi

if test $ac_cv_func_getrusage = yes; then
	AC_SUBST([JAS_HAVE_GETRUSAGE], ["#define JAS_HAVE_GETRUSAGE 1"])
else
	AC_SUBST([JAS_HAVE_GETRUSAGE], ["#undef JAS_HAVE_GETRUSAGE"])
fi

############################################################
# Checks for library functions.
############################################################

dnl AC_FUNC_VPRINTF

############################################################
# Enable memory limit feature.
############################################################

AC_ARG_ENABLE(memory-limit,
[  --enable-memory-limit=[ARG]    Set memory limit to ARG (where ARG defaults to SIZE_MAX).],
[
if test "${enableval}" != no; then
	if test "${enableval}" = yes; then
		enableval="SIZE_MAX"
	fi
	AC_DEFINE_UNQUOTED([JAS_DEFAULT_MAX_MEM_USAGE],
	[${enableval}],
	[Maximum memory usage limit])
fi
],
[])

############################################################
# Enable/disable sanitizers.
############################################################

AC_ARG_ENABLE(asan,
[  --enable-asan    Enable Address Sanitizer.],
[
case "${enableval}" in
yes)
	CFLAGS="$CFLAGS -fsanitize=address"
	LDFLAGS="$LDFLAGS -fsanitize=address"
	;;
esac
],
[])

AC_ARG_ENABLE(usan,
[  --enable-usan    Enable Undefined-Behavior Sanitizer.],
[
case "${enableval}" in
yes)
	CFLAGS="$CFLAGS -fsanitize=undefined"
	LDFLAGS="$LDFLAGS -fsanitize=undefined"
esac
],
[])

AC_ARG_ENABLE(leak,
[  --enable-leak    Enable Leak Sanitizer.],
[
case "${enableval}" in
yes)
	CFLAGS="$CFLAGS -fsanitize=leak"
	LDFLAGS="$LDFLAGS -fsanitize=leak"
esac
],
[])

AC_ARG_ENABLE(msan,
[  --enable-msan    Enable Memory Sanitizer.],
[
case "${enableval}" in
yes)
	CFLAGS="$CFLAGS -fsanitize=memory"
	LDFLAGS="$LDFLAGS -fsanitize=memory"
esac
],
[])

############################################################
# Enable/disable extra code for debugging.
############################################################

AC_ARG_ENABLE(debug,
[  --enable-debug    add extra code for debugging],
[
case "${enableval}" in
yes)
	debug=yes
	AC_DEFINE(DEBUG)
	AC_DEFINE(DEBUG_OVERFLOW)
	if test "$GCC" = yes; then
		#CFLAGS=""
		CFLAGS="$CFLAGS -g -O0"
	fi
	;;
no)
	debug=no
	;;
*)
	AC_MSG_ERROR(bad value ${enableval} for --enable-debug)
	;;
esac
], [debug=no])

############################################################
# Extra stuff
############################################################

if test -n "$EXTRA_CFLAGS"; then
	CFLAGS="$CFLAGS $EXTRA_CFLAGS"
fi

############################################################
# Generate the configuration header file.
############################################################

# The private header file generated by AC_CONFIG_HEADERS is not
# currently used.  This just causes less verbose command lines
# when make is run.
AC_CONFIG_HEADERS([src/libjasper/include/jasper/jas_pconf.h])

############################################################
# Generate the makefiles.
############################################################

AC_CONFIG_FILES([
src/libjasper/include/jasper/jas_config.h
Makefile
src/Makefile
src/appl/Makefile
src/libjasper/Makefile
src/libjasper/base/Makefile
src/libjasper/bmp/Makefile
src/libjasper/include/Makefile
src/libjasper/include/jasper/Makefile
src/libjasper/jp2/Makefile
src/libjasper/jpc/Makefile
src/libjasper/jpg/Makefile
src/libjasper/mif/Makefile
src/libjasper/pgx/Makefile
src/libjasper/pnm/Makefile
src/libjasper/ras/Makefile
src/msvc/Makefile
test/Makefile
test/bin/Makefile
pkgconfig/Makefile
pkgconfig/jasper.pc
jasper.spec
])
AC_OUTPUT
