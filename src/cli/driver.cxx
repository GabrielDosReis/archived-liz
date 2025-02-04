// Copyright (C) 2012-2013, Texas A&M University
// Copyright (C) 2014-2015, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of Liz, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#include <liz/config>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include <algorithm>
#include <memory>

#include <liz/Invocation>
#include "Evaluator.H"
#include "Interpreter.h"
#include "Compiler.H"
#if LIZ_TARGET_LLVM
#  include "liz-llvm.h"
#  include <llvm/Support/TargetSelect.h>
#endif


// Synonpsys:
//


namespace liz {
   // Return true if `arg' (A C-style string) designates a command lie
   // option to the interpreter (they all start with "--").
   inline bool
   is_option(const char* arg) {
      return arg[0] == '-' and arg[1] == '-';
   }

   template<int N>
   const char* is_prefix(const char (&lhs)[N], const char* rhs) {
      for (int i = 0; i < N - 1; ++i)
         if (lhs[i] != rhs[i])
            return 0;
      return rhs + N - 1;
   }

   struct DebugFlagInfo {
      const char* const name;
      const Debug flag;
   };

   static const DebugFlagInfo debug_table[] = {
      { "lexing", Debug::Lexing },
      { "parsing", Debug::Parsing },
      { "scope", Debug::Scope },
      { "overload", Debug::Overload },
      { "eq", Debug::Eq },
      { "logic", Debug::Logic },
      { "codegen", Debug::Codegen },
      { "matching", Debug::Matching },
      { "inst", Debug::Instantiation },
      { "subst", Debug::Substitution },
      { "eval", Debug::Evaluation },
   };

   static Debug
   debug_flag(std::string name) {
      for (int i = 0; i < length(debug_table); ++i)
         if (name == debug_table[i].name)
            return debug_table[i].flag;
      return Debug::Nothing;
   }

   static Debug
   debug_options(const char* s) {
      if (*s++ == '\0')
         return Debug::Everything;
      Debug flags = Debug::Nothing;

      const char* end = s + strlen(s);
      while (true) {
         const char* p = std::find(s, end, ',');
         flags |= debug_flag(std::string(s, p));
         if (p >= end)
            break;
         s = p + 1;
      }
      return flags;
   }

   static Flags process_command_line(char* argv[], int argc, int& argpos) {
      auto flags = Flags();
      // Note: We require that all options appear before non-option
      // arguments to the interpreter.
      for (; argpos < argc && is_option(argv[argpos]); ++argpos) {
         if (const char* opt = is_prefix("--debug", argv[argpos]))
            flags.debug |= liz::debug_options(opt);
         else if (strcmp(argv[argpos], "--compile") == 0)
            flags.action = liz::translation::compile;
         else if (strcmp(argv[argpos], "--llvm") == 0)
            flags.options |= liz::TranslationOption::Llvm;
         else if (const char* opt = is_prefix("--output", argv[argpos]))
            flags.output_file = opt;
         else if (auto dir = is_prefix("--stdlib=", argv[argpos]))
            flags.stdlib = dir;
         else
            std::cerr << "Unrecognized option: " << argv[argpos] << std::endl;
      }

      return flags;
   }

   // Allocate a secondary evaluator dependent on command line.
   static std::unique_ptr<BasicEvaluator>
   select_evaluator(TranslationOption opt) {
      if (has(opt, TranslationOption::Llvm)) {
         if (!LIZ_TARGET_LLVM)
            std::cerr << "LLVM backend requested but not available"
                      << std::endl;
#if LIZ_TARGET_LLVM
         llvm::InitializeNativeTarget();
         return std::unique_ptr<BasicEvaluator>{ new LlvmEvaluator{ } };
#endif
      }
      return std::unique_ptr<BasicEvaluator>{ new Evaluator{ } };
   }

   static Translator*
   make_translator(const Flags& flags, liz::Reader& reader, BasicEvaluator* evaler) {
      if (flags.action == liz::translation::compile)
         return new liz::Compiler(reader, evaler);
      return new liz::Interpreter(reader, evaler);
   }
}


int main(int argc, char *argv[]) {
   liz::Invocation invoke;
   liz::Reader reader { invoke };
   try {
      // current position in the command line argument vector (argv[])
      int argpos = 1;
      liz::Flags flags = liz::process_command_line(argv, argc, argpos);

      auto evaler = liz::select_evaluator(flags.options);
      // When call with no argument, we start the toplevel repl.
      if (argpos == argc)  {
         liz::Interpreter(reader, evaler.get()).toplevel_loop(flags);
         if (not std::cin.good())
            std::cout << std::endl;
      }
      else {
         auto translator = liz::make_translator(flags, reader, evaler.get());
         // Keep track of file names that we could not process to completion.
         int number_of_erreneous_files = 0;
         for(int n = argpos; n < argc; n++) {
            if (not translator->process_file(argv[n], flags))
               ++number_of_erreneous_files;
         }
         return number_of_erreneous_files;
      }
   }
   catch(const liz::BasicError& e) {
      e.issue_on(std::cerr);
      std::cerr << std::endl;
      std::exit(1);
   }
}
