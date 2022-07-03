// Copyright (C) 2015, Gabriel Dos Reis.
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


#include <iostream>
#include <cstring>
#include <string>
#include <algorithm>
#include <iterator>
#include "Interpreter.h"

namespace liz {
   Interpreter::Interpreter(Reader& r, BasicEvaluator* e)
         : Translator(*r.invocation()),
           elab(r, e)
   {
      e->set_elaborator(&elab);
   }

   Interpreter::~Interpreter()
   { }

   static void
   evaluate_toplevel(BasicContext<>& ctx, const Ast* ast) {
      auto e = ctx.elaborator()->elaborate(ast);
      try {
         Object object = ctx.evaluator()->eval(e);
         if (auto t = object.type()) {
            ctx.reader()->output() << "\n~~> " << object
                          << ": " << *t
                          << std::endl;
         }
         else
            ctx.reader()->output() << "? : ?" << std::endl;
      }
      catch (const FreeVariableError&) {
         ctx.reader()->output() << "\n~~> " << pretty(e.code())
                       << ": " << *e.type()
                       << std::endl;
      }
   }
   
   static void
   evaluate_toplevel(BasicContext<>& ctx, const AstSequence& seq) {
      for (std::size_t i = 0; i < length(seq); ++i)
         evaluate_toplevel(ctx, seq.at(i));
   }


   bool
   Interpreter::process_file(const Path& path, const Flags& flags) {
      Invocation::Manager new_flags { invoke, flags };
      auto ctx = elab.basic_context();
      evaluate_toplevel(ctx, ctx.reader()->read_file(path, flags));
      return true;
   }

   // This is the toplevel read-eval-print-loop
   void
   Interpreter::toplevel_loop(const Flags& flags) {
      Fragment fragment;
      Invocation::Manager new_flags { invoke, flags };
      std::string str;
      const std::string prompt = "liz> ";
      auto ctx = elab.basic_context();
      ctx.reader()->output() << prompt;
      while (std::getline(std::cin, str)) {
         if (not str.empty()) {
            try {
               auto tokens = tokenize(fragment.source.append(str));
               auto asts = ctx.reader()->parse(tokens, invoke.current_flags());
               fragment.tokens.append(std::move(tokens));
               evaluate_toplevel(ctx, asts);
               fragment.asts.append(asts);
            }
            catch (const BasicError& e) {
               e.issue_on(ctx.reader()->error());
               ctx.reader()->error() << std::endl;
            }
         }
         else if (not std::cin) {
            ctx.reader()->output() << std::endl;
            break;
         }
         ctx.reader()->output() << prompt;
      }
   }
}
