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

#include <cstdlib>
#include <iterator>
#include <algorithm>
#include <fstream>

#include "Compiler.H"
#include "backend/cxx.H"

namespace liz {

   // Return a output pathname for the intermediate C++ file.
   static Path
   get_output_path(const Flags& flags, const Path& inpath, const Path ext) {
      if (not flags.output_file.empty())
         return flags.output_file;
      auto p = std::find(inpath.rbegin(), inpath.rend(), '.');
      return p == inpath.rend()
         ? inpath + "." + ext 
         : Path(inpath.begin(), p.base()) + ext;
   }

   static bool compile_cxx_file(const Path& path) {
      std::string cmd = std::string(tools::host::cxx_compiler)
         + " -c " + tools::host::cxx_flags + " "
         + path;
      return std::system(cmd.c_str()) == 0;
   }
      
   
   // -- Compiler --
   Compiler::Compiler(Reader& r, BasicEvaluator* e)
         : Translator(*r.invocation()),
           elab(r, e)
   {
      e->set_elaborator(&elab);
   }

   Compiler::~Compiler()
   { }

   bool
   Compiler::process_file(const Path& path, const Flags& flags) {
      Invocation::Manager new_flags { invoke, flags };
      CxxBackend backend { elab };
      Path output_path = get_output_path(flags, path, backend.path_extension());
      std::ofstream output(output_path.c_str());
      const SourceFileAst* src = elab.reader()->read_file(path, flags);
      for (auto ast : src->asts) {
         const Expression* expr = elab.elaborate(ast).code();
         if (expr != nullptr) {
            backend.format(backend.translate_decl(expr), output);
            output << '\n';
         }
      }
      if (not compile_cxx_file(output_path))
         internal_error("intermediate C++ translation failed to compile");
      return true;
   }
}
