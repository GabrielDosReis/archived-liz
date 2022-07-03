# Copyright (C) 2012-2013, Texas A&M University
# Copyright (C) 2014-2015, Gabriel Dos Reis.
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

AUTOMAKE_OPTIONS = nostdinc

LIZ_SYSDIR = $(pkglibdir)-$(VERSION)

if LIZ_BUILD_IDE
liz_maybe_ide = ide
endif

SUBDIRS = . $(liz_maybe_ide)

bin_PROGRAMS = liz
noinst_LIBRARIES = libliz.a

liz_cxxwarn_flags = -Wall -Wextra -Werror
liz_cpp_defines = -DLIZ_SYSTEM_DIRECTORY="\"$(LIZ_SYSDIR)\""
liz_incdir = \
	-I$(srcdir) \
	-I$(top_builddir)/include \
	-I$(srcdir)/io \
	-I$(srcdir)/overseer \
	-I$(srcdir)/syntax \
	-I$(srcdir)/semantics \
	-I$(srcdir)/meta

AM_CXXFLAGS = \
	$(liz_incdir) \
	$(liz_cxxwarn_flags) @liz_nowarn_flags@ \
	$(liz_cpp_defines)

libliz_a_SOURCES = \
	support/utility.cxx \
	support/tools.cxx \
	support/storage.cxx \
	io/Character.cxx \
	io/Input.cxx \
	io/Output.cxx \
	io/pretty-print.cxx \
	overseer/IOContext.cxx \
	overseer/Invocation.cxx \
	syntax/Token.cxx \
	syntax/Ast.cxx \
	syntax/ast-io.cxx \
	syntax/Parser.cxx \
	semantics/Data.cxx \
	semantics/Type.cxx \
	semantics/Expression.cxx \
	semantics/expression-io.cxx \
	semantics/pattern.cxx \
	semantics/logic.cxx \
	semantics/elaboration.cxx \
	backend/cxx.cxx \
	backend/cxx-format.cxx \
	meta/Translator.cxx \
	meta/Elaborator.cxx \
	meta/simplify.cxx \
	meta/Evaluator.cxx \
	meta/Interpreter.cxx \
	meta/Compiler.cxx \
	meta/intrinsics.cxx

liz_SOURCES = \
	cli/driver.cxx

liz_CXXFLAGS = $(AM_CXXFLAGS)
liz_LDADD = libliz.a
if LIZ_TARGET_LLVM
liz_SOURCES += backend/liz-llvm.cxx
liz_CXXFLAGS += \
	@liz_llvm_config_cxxflags@ @liz_nowarn_flags@ -fexceptions \
	-I$(srcdir)/backend/
liz_LDFLAGS = \
	`$(LIZ_LLVM_CONFIG) --ldflags --system-libs --libs core orc native` \
	-rdynamic
endif


if LIZ_BUILD_IDE
all-ide: ide/Makefile
ide/qt-mk.pro: $(srcdir)/src/qt-mk.pro.in $(top_builddir)/config.status
	(cd $(top_builddir) && $(SHELL) ./config.status src/ide/qt-mk.pro)

ide/Makefile: ide/qt-mk.pro
	(cd ide && $(LIZ_QMAKE) -makefile $(liz_qmake_spec) qt-mk.pro)
endif
