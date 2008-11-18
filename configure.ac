# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(boundfit,02.0,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/boundfit.f])

# Output files
AC_CONFIG_FILES([Makefile
                 src/version.inc
                 src/Makefile])
AC_OUTPUT
