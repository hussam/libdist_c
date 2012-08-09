#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(LibDist, 0.01, hussam.al@gmail.com)
AC_CONFIG_SRCDIR([libdist_client.c])
AC_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_LN_S

# Checks for libraries.
# FIXME: Replace `main' with a function in `-lei':
AC_CHECK_LIB([ei], [ei_connect_init])

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC

dnl Erlang-related configuration.
AC_LANG(Erlang)
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_CHECK_LIB(erts)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(erts)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])
AC_ERLANG_SUBST_INSTALL_LIB_DIR
erlang_cv_drv_include="${ERLANG_ROOT_DIR}/usr/include"
AC_SUBST([ERLANG_DRV_INCLUDE], [$erlang_cv_drv_include])

AC_CONFIG_FILES([ Makefile ])

AM_INIT_AUTOMAKE([1.10 no-define foreign])

AC_CANONICAL_HOST
# Check for which host we are on and setup a few things
# specifically based on the host
case $host_os in
  darwin* )
        # Do something specific for mac
		GCC_FLAGS="-undefined dynamic_lookup -dynamiclib -Wall -m32"
        ;;
  linux*)
        # Do something specific for linux
		GCC_FLAGS="-fPIC -shared -Wall"
        ;;
    *)
        #Default Case
        AC_MSG_ERROR([Your platform is not currently supported])
        ;;
esac

AC_SUBST(GCC_FLAGS)

AC_OUTPUT
