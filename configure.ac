dnl Copyright (C) 2012-2013, Texas A&M University
dnl Copyright (C) 2014-2016, Gabriel Dos Reis.
dnl All rights reserved.
dnl Written by Gabriel Dos Reis.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are
dnl met:
dnl
dnl     - Redistributions of source code must retain the above copyright
dnl       notice, this list of conditions and the following disclaimer.
dnl
dnl     - Redistributions in binary form must reproduce the above copyright
dnl       notice, this list of conditions and the following disclaimer in
dnl       the documentation and/or other materials provided with the
dnl       distribution.
dnl
dnl     - Neither the name of Liz, nor the names of its contributors may
dnl       be used to endorse or promote products derived from this software
dnl       without specific prior written permission.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
dnl IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
dnl TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
dnl PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
dnl OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
dnl EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
dnl PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
dnl PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
dnl LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
dnl NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
dnl SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl Most of the macros used in this configure.ac are defined in files
dnl located in the subdirectory misc/
sinclude(misc/liz.m4)

AC_INIT([liz], [1.0], [liz@axiomatics.org])

AC_CONFIG_AUX_DIR([misc])
AC_CONFIG_MACRO_DIR([misc])

if test -n "$ZSH_VERSION"; then
  emulate sh
fi

LIZ_CANONICAL_TRIPLETS

AM_INIT_AUTOMAKE([foreign subdir-objects])
AC_PREREQ([2.65])

AC_REQUIRE_AUX_FILE(liz.m4)
AC_CONFIG_FILES([Makefile include/Makefile src/Makefile library/Makefile
   library/Basic/Makefile library/Builtin/Makefile])
AC_LANG([C++])

dnl Put all configuration macros here
AC_CONFIG_HEADERS([include/liz/config:misc/config.h.in])
AH_TOP([#ifndef LIZ_CONFIG_included
#define LIZ_CONFIG_included
])
AH_BOTTOM([#endif   // LIZ_CONFIG_included])

LIZ_SYSTEM_PATHS
LIZ_BUILD_TOOLS
LIZ_REQUIRE_MODERN_CXX

LIZ_HOST_CPPFLAGS
LIZ_CHECK_MM
LIZ_TARGET_TOOLS
LIZ_SUPPORT_LIBRARIES
AM_CONDITIONAL([LIZ_TARGET_LLVM], [test -n "$LIZ_LLVM_CONFIG"])
AM_CONDITIONAL([LIZ_BUILD_IDE], [test x$liz_has_qt = xyes])

AC_CONFIG_LINKS([
  include/liz/utility:src/support/utility.H
  include/liz/storage:src/support/storage.H
  include/liz/Character:src/io/Character.H
  include/liz/Input:src/io/Input.H
  include/liz/Token:src/syntax/Token.H
  include/liz/Ast:src/syntax/Ast.H
  include/liz/IOContext:src/overseer/IOContext.h
  include/liz/Invocation:src/overseer/Invocation.h
])

AC_CONFIG_FILES([src/support/tools.cxx src/ide/qt-mk.pro])

AC_OUTPUT

## Set up Makefile for QT-based gui
if test x$liz_has_qt = xyes; then
  liz_qmake_spec=
  case $host,$GXX in
     *darwin*,yes)
         liz_qmake_spec="-spec macx-g++"
         ;;
  esac
  AC_SUBST([liz_qmake_spec])
  (cd src/ide && $LIZ_QMAKE -makefile $liz_qmake_spec qt-mk.pro)
fi

