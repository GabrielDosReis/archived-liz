// -*- C++ -*-
// Copyright (C) 2009-2013, Texas A&M University. 
// Copyright (C) 2013-2015, Gabriel Dos Reis.
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


#ifndef LIZ_EVALUATOR_INCLUDED
#define LIZ_EVALUATOR_INCLUDED


#include <stack>
#include <set>
#include <iostream>
#include "Parser.H"
#include "Elaborator.H"
#include "Translator.H"
#include "Transducer.h"

namespace liz {
   // -- FreeVariableError --
   struct FreeVariableError : BasicError {
      FreeVariableError(const std::string&, Symbol);
   private:
      Symbol sym;
      void format_message(std::ostream&) const;
   };

   // ---------------
   // -- Evaluator --
   // ---------------
   // Object of this type represents C++ evaluators.
   struct Evaluator : BasicEvaluator, Transducer<Object, FunctionElaboration> {
      Evaluator();
      ~Evaluator();

      Object eval(Elaboration) override;
      void set_elaborator(Elaborator*) override;

   private:
      Elaborator* elab;

      // Bind a symbol to a value in a given environment.
      Object* define(Store<Object>*, const std::string&, Object);
   };
}

#endif	//  LIZ_EVALUATOR_INCLUDED
