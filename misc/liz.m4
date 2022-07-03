# Copyright (C) 2012-2013, Texas A&M University
# Copyright (C) 2014-2016, Gabriel Dos Reis.
# All rights reserved.
# Written by Gabriel Dos Reis
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     - Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     - Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#
#     - Neither the name of Liz, nor the names of its contributors may
#       be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl ----------------------------
dnl -- LIZ_CANONICAL_TRIPLETS --
dnl ----------------------------
dnl Set up the canonical triplets for the build, 
dnl host, and target systems.
AC_DEFUN([LIZ_CANONICAL_TRIPLETS], [
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AC_DEFINE_UNQUOTED([LIZ_HOST_TRIPLET],["$host"],[The host triplet])

])

dnl ----------------------
dnl -- LIZ_SYSTEM_PATHS --
dnl ----------------------
AC_DEFUN([LIZ_SYSTEM_PATHS], [
AC_SUBST([LIZ_SYSDIR],[\${pkglibdir}-${VERSION}])
])

dnl ---------------------------
dnl -- LIZ_CXX_ACCEPT_SWITCH --
dnl ---------------------------
dnl Check whether the booting C++ compiler accepts a given switch.
dnl If yes, augment the list passed as second argument.
AC_DEFUN([LIZ_CXX_ACCEPT_SWITCH],[
flag=$1
switch_list=$2
if test -z $switch_list; then
   switch_list=CXXFLAGS
fi
old_list=\$${switch_list}
liz_saved_cxxflags="$CXXFLAGS"
AC_MSG_CHECKING([whether $CXX supports "$flag"])
CXXFLAGS="$CXXFLAGS $flag"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
	[AC_MSG_RESULT([yes])]
	[CXXFLAGS=$liz_saved_cxxflags]
	[eval "$switch_list=\"$old_list $flag\""],
	[AC_MSG_RESULT([no])]
	[CXXFLAGS="$liz_saved_cxxflags"])
])


dnl ----------------------------
dnl -- LIZ_REQUIRE_MODERN_CXX --
dnl ----------------------------
dnl Require a modern C++ compiler, e.g. at least C++14.
AC_DEFUN([LIZ_REQUIRE_MODERN_CXX],[
# Assume supported compilers understand -std=c++14
liz_saved_cxx_flags="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -std=c++14"
AC_MSG_CHECKING([whether C++ compiler supports C++14])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])]
  [AC_MSG_ERROR([Liz requires a C++ that supports at least C++14])])

# Clang/LLVM insists on stifling effective programming techniques and
# constructs it judges above its imagination.
LIZ_CXX_ACCEPT_SWITCH([-Wno-overloaded-virtual],[liz_nowarn_flags])
LIZ_CXX_ACCEPT_SWITCH([-Wno-mismatched-tags],[liz_nowarn_flags])
LIZ_CXX_ACCEPT_SWITCH([-Wno-non-virtual-dtor],[liz_nowarn_flags])
LIZ_CXX_ACCEPT_SWITCH([-Wno-unused-parameter],[liz_nowarn_flags])
LIZ_CXX_ACCEPT_SWITCH([-Wno-overloaded-virtual],[liz_nowarn_flags])

AC_SUBST([liz_nowarn_flags])
])

dnl --------------------
dnl -- LIZ_CHECK_LLVM --
dnl --------------------
dnl Check that the target has the LLVM framework and set up switches
dnl and library flags appropriately.
AC_DEFUN([LIZ_CHECK_LLVM],[
AC_CHECK_PROGS(LIZ_LLVM_CONFIG,[llvm-config])
if test -n "$LIZ_LLVM_CONFIG"; then
   liz_target_llvm=1
else
   liz_target_llvm=0
fi
AC_DEFINE_UNQUOTED([LIZ_TARGET_LLVM],[$liz_target_llvm],[Use LLVM if nonzero.])
if test $liz_target_llvm -ne 0 ; then
  AC_CHECK_TARGET_TOOL([LIZ_LLVM_AS],[llvm-as])
  AC_CHECK_TARGET_TOOL([LIZ_LLVM_DIS],[llvm-dis])
  AC_CHECK_TARGET_TOOL([LIZ_LLVM_LINK],[llvm-link])
  AC_CHECK_TARGET_TOOL([LIZ_LLVM_AR],[llvm-ar])
  AC_CHECK_TARGET_TOOL([LIZ_LLVM_NM],[llvm-nm])

  # Prune unrecognized warning flags.
  liz_llvm_config_cxxflags=
  for flag in `$LIZ_LLVM_CONFIG --cxxflags`; do
      case $flag in
      	   -W*)
		LIZ_CXX_ACCEPT_SWITCH([$flag],[liz_llvm_config_cxxflags])
		;;
	   -pedantic)
		# Really?
		;;
	   *)
		liz_llvm_config_cxxflags="$liz_llvm_config_cxxflags $flag"
		;;
      esac
  done
fi

AC_SUBST([liz_llvm_config_cxxflags])
])

dnl ---------------------
dnl -- LIZ_BUILD_TOOLS --
dnl ---------------------
dnl Check for various build tools.
AC_DEFUN([LIZ_BUILD_TOOLS], [
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_SED

# Force Clang, the default system compiler, on MacOS X; or else
# suffer misery.
liz_c_compilers=
liz_cxx_compilers=
case $target in
   *apple*)
       liz_c_compilers='clang'
       liz_cxx_compilers='clang++'
       ;;
   *)
       ;;
esac
AC_PROG_CC([$liz_c_compilers])
AC_PROG_CPP
AC_PROG_CXX([$liz_cxx_compilers])
AC_PROG_CXXCPP
])

dnl ------------------
dnl -- LIZ_CHECK_MM --
dnl ------------------
dnl Check for host capability of memory mapping.
AC_DEFUN([LIZ_CHECK_MM],[
AC_CHECK_HEADERS([sys/mman.h fcntl.h])
## We want annonymous mapping for memory allocation.  Unfortunately,
## the flag for anonymous mapping is not standardized.  Popular names
##  are MAP_ANONYMOUS and MAP_ANON.
if test x"$ac_cv_header_sys_mman_h" = xyes; then
   AC_MSG_CHECKING([for flag to request anonymous memory mapping])
   AC_EGREP_CPP([MAP_ANONYMOUS],
                [#include <sys/mman.h>
#ifdef MAP_ANONYMOUS
   "MAP_ANONYMOUS"
#endif],
                [liz_mm_anonymous_flag=MAP_ANONYMOUS])
   if test -z "$liz_mm_anonymous_flag"; then
      AC_EGREP_CPP([MAP_ANON],
                   [#include <sys/mman.h>
#ifdef MAP_ANON
   "MAP_ANON"
#endif],
                   [liz_mm_anonymous_flag=MAP_ANON])
   fi
   ## It would be curious that we don't have an anonymous mapping
   ## capability.  Let that be known loudly.
   if test -n "$liz_mm_anonymous_flag"; then
      AC_MSG_RESULT([$liz_mm_anonymous_flag])
   else
      AC_MSG_ERROR([Could not find flag for anonymous map])
   fi
   AC_DEFINE_UNQUOTED([LIZ_MM_ANONYMOUS_MAP_FLAG],
                      [$liz_mm_anonymous_flag],
                      [mmap anonymous flag])
fi
])

dnl -----------------------
dnl -- LIZ_HOST_CPPFLAGS --
dnl -----------------------
AC_DEFUN([LIZ_HOST_CPPFLAGS],[
case $host in
  *linux*)
     CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
     ;;
  *bsd*|*dragonfly*)
     CPPFLAGS="$CPPFLAGS -D_BSD_SOURCE"
     ;;
  *mingw*)
     CPPFLAGS="$CPPFLAGS -DLIZ_WINDOWS_HOST"
     ;;
esac
])

dnl ----------------------
dnl -- LIZ_TARGET_TOOLS --
dnl ----------------------
dnl Check for the availability of tools needed for
dnl the target.
AC_DEFUN([LIZ_TARGET_TOOLS], [
AC_CHECK_TARGET_TOOL([LIZ_TARGET_AS],[as])
AC_CHECK_TARGET_TOOL([LIZ_TARGET_LD],[ld])
AC_SUBST(LIZ_TARGET_AS)
AC_SUBST(LIZ_TARGET_LD)
])

dnl -------------------
dnl -- LIZ_CHECK_GMP --
dnl -------------------
AC_DEFUN([LIZ_CHECK_GMP], [
## Default to macports locations on macs
case "${target}" in
   *-*-darwin*)
     if test x"${with_gmp}" = x && test -f /opt/local/include/gmp.h; then
        with_gmp=/opt/local
     fi
   ;;
esac
# Take whatever user wants us to use.
AC_ARG_WITH(gmp,
  [AS_HELP_STRING([--with-gmp=PATH],
     [specify prefix path for the installed GMP packag])])
if test x"${with_gmp}" != x; then
  CPPFLAGS="-I${with_gmp}/include ${CPPFLAGS}"
  LIBS="-L${with_gmp}/lib ${LIBS}"
fi
# Check if the whole thing works out.
AC_CHECK_HEADERS([gmp.h], [], 
                [AC_MSG_ERROR([Could not find <gmp.h> header file])])
AC_CHECK_LIB([gmp], [__gmpz_init], [],
             [AC_MSG_ERROR([GMP runtime library not found])])
])

dnl ------------------
dnl -- LIZ_CHECK_QT --
dnl ------------------
AC_DEFUN([LIZ_CHECK_QT],[
# Check for Qt utilities.
liz_has_qt=no
case $host in
     *darwin*)
	if test -d /opt/local/libexec/qt5/bin; then
	   PATH=/opt/local/libexec/qt5/bin:$PATH
	fi
	;;
     *) ;; 
esac
AC_CHECK_PROGS([LIZ_MOC], [moc])
AC_CHECK_PROGS([LIZ_QMAKE], [qmake])
if test -n "$LIZ_MOC"; then
  AC_MSG_CHECKING([Qt version])
  liz_qt_version=`"$LIZ_MOC" -v 2>&1 | sed -e 's/^.*(\(.*\))$/\1/'`
  AC_MSG_RESULT([$liz_qt_version])
  case $liz_qt_version in
    *[1-3]\.[0-9]+\.[0-9]+)
       AC_MSG_WARN([This version of Qt is too old.])
       ;;
    *) 
       liz_has_qt=yes
       ;;
  esac
fi
AC_DEFINE_UNQUOTED([LIZ_USE_GUI], [`expr x$liz_has_qt = xyes`],
                   [Whether to use the QT-based IDE.])
])

dnl ---------------------------
dnl -- LIZ_SUPPORT_LIBRARIES --
dnl ---------------------------
dnl Check for host and target libraries support libraries.
AC_DEFUN([LIZ_SUPPORT_LIBRARIES], [
LIZ_CHECK_GMP
LIZ_CHECK_QT
LIZ_CHECK_LLVM
]) 
