# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(boundfit,02.03,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/boundfit.f])

# Definitions
DEFAULT_NDATAMAX="100000"
NDATAMAX=${NDATAMAX="$DEFAULT_NDATAMAX"}
AC_ARG_VAR(NDATAMAX,[Maximum number of data points])

DEFAULT_LENLINEA="1000"
LENLINEA=${LENLINEA="$DEFAULT_LENLINEA"}
AC_ARG_VAR(LENLINEA,[Maximum number of characters/line in input file])

# Output files
AC_CONFIG_FILES([Makefile
                 src/lenlinea.inc
                 src/ndatamax.inc
                 src/ndegmax.inc
                 src/nknotsmax.inc
                 src/version.inc
                 src/Makefile])
AC_OUTPUT
