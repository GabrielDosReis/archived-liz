// -*- C++ -*-
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

#ifndef LIZ_LLVM_included
#define LIZ_LLVM_included

// -- This header defines the interface for the LLVM backend of the
// -- Liz compiler.  After elaboration of input Liz source program,
// -- this module translate the internal representation to the LLVM IR.

#include <map>
#include <stack>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <liz/storage>
#include "Expression.H"
#include "Compiler.H"
#include "Backend.h"

namespace liz {
   // -----------------
   // -- LlvmBackend --
   // -----------------
   struct LlvmBackend : Backend<llvm::Value*>,
                        Transducer<llvm::Value*, llvm::Function*> {
      struct ValueMode {
         ValueMode(LlvmBackend*, const Type*);
         ~ValueMode();
      private:
         LlvmBackend* be;
      };

      struct State {
         using TypeDict = std::map<const Type*, llvm::Type*>;
         using FunDict = std::map<const Lambda*, llvm::Function*>;

         explicit State(llvm::LLVMContext& c) : ctx(c) { }
         State(const State&) = delete;
         
         llvm::LLVMContext& ctx;
         TypeDict type_dict;
         FunDict fun_dict;
         std::stack<const Type*> modes;
      };

      struct Allocator {
         Factory<llvm::GlobalVariable> globals;
         Factory<llvm::AllocaInst> locals;
         Factory<llvm::StoreInst> stores;
         Factory<llvm::LoadInst> loads;
      };

      Allocator alloc;
      State state;
      std::unique_ptr<llvm::TargetMachine> target_machine;
      llvm::DataLayout data_layout;

      LlvmBackend();
      // Translate a Liz type to an LLVM type.
      llvm::Type* translate_type(const Type*);

      llvm::LLVMContext& llvm_context() { return state.ctx; }
      llvm::Module* module_unit() { return &*unit; }

      void associate(const Lambda*, llvm::Function*);
      llvm::Function* function(const Lambda*);

      bool at_toplevel() { return current_frame() == nullptr; }
      const Type* value_mode() { return state.modes.top(); }

      llvm::Value* lower_stmt(Elaboration) override;
      void format(llvm::Value*, std::ostream&) override;
      const char* path_extension() const override;
      
   protected:
      std::unique_ptr<llvm::Module> unit;
   };

   // -- An LLVM-based interactive evaluator
   struct LlvmEvaluator : BasicEvaluator, LlvmBackend {
      LlvmEvaluator();
      Object eval(Elaboration) override;
      void set_elaborator(Elaborator*) override;
   private:
      using ObjectLinker = llvm::orc::ObjectLinkingLayer<>;
      using IRCompiler = llvm::orc::IRCompileLayer<ObjectLinker>;
      using UnitSetHandle = IRCompiler::ModuleSetHandleT;
      ObjectLinker linker;
      IRCompiler ir_compiler;
      llvm::ExecutionEngine* jit_vm;
      Elaborator* elab;
   };
}

#endif  // LIZ_LLVM_included
