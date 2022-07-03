// -*- C++ -*-
// Copyright (C) 2014-2016, Gabriel Dos Reis.
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

#include <typeinfo>
#include "liz-llvm.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Verifier.h>
// #include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
// #include <llvm/ExecutionEngine/RuntimeDyld.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Support/raw_os_ostream.h>

namespace liz {
   LlvmBackend::ValueMode::ValueMode(LlvmBackend* be, const Type* t)
         : be(be)
   {
      be->state.modes.push(t);
   }

   LlvmBackend::ValueMode::~ValueMode() {
      be->state.modes.pop();
   }

   // Set up the expected return type of a value, and descend into
   // expression associated with the elaboration.  Return the resulting value.
   template<typename V, typename T>
   T accept_visitor(LlvmBackend* be, Elaboration e) {
      LlvmBackend::ValueMode new_mode { be, e.type() };
      V v { be };
      e.code()->accept(v);
      return v.result;
   }

   // Return the LLVM declaration of the current function being translated.
   static llvm::Function* current_function(LlvmBackend* be) {
      auto frame = be->current_frame();
      if (frame == nullptr)
         internal_error("No current function frame");
      return frame->function();
   }
   
   // Forward declarations of general/generic functions.
   static llvm::Value* convert_expr(LlvmBackend*, Elaboration);

   // Helper reference wrapper class for llvm::BasicBlock*.
   // Centralizes the mechanics of generating instructions in
   // a basic block.
   struct BlockRef {
      BlockRef(llvm::BasicBlock* b) : block(b) { }

      llvm::BasicBlock* base() const { return block; }
      llvm::BasicBlock* operator->() const { return base(); }

      llvm::AllocaInst* allocate(llvm::Type* t, const std::string& s = "") {
         return enque(new llvm::AllocaInst(t, s));
      }

      llvm::StoreInst* store(llvm::Value* val, llvm::Value* loc) {
         return enque(new llvm::StoreInst(val, loc));
      }

      llvm::LoadInst* load(llvm::Value* loc) {
         return enque(new llvm::LoadInst(loc));
      }

      llvm::BitCastInst* pretend(llvm::Value* v, llvm::Type* t) {
         return new llvm::BitCastInst(v, t, "", base());
      }

      llvm::PHINode* phi(llvm::Type* t, uint32_t n) {
         return llvm::PHINode::Create(t, n, "", base());
      }

      llvm::ICmpInst* icmp(llvm::CmpInst::Predicate p,
                           llvm::Value* x, llvm::Value* y) {
         return new llvm::ICmpInst(*base(), p, x, y);
      }

      llvm::FCmpInst* fcmp(llvm::CmpInst::Predicate p,
                           llvm::Value* x, llvm::Value* y) {
         return new llvm::FCmpInst(*base(), p, x, y);
      }

      template<typename... Args>
      llvm::BinaryOperator* apply(Args&&... args) {
         return llvm::BinaryOperator::Create(args..., "", base());
      }

      llvm::BranchInst* jump_to(BlockRef b) {
         return llvm::BranchInst::Create(b.base(), base());
      }

      llvm::BranchInst* jump_if(llvm::Value* c, BlockRef t, BlockRef f) {
         return llvm::BranchInst::Create(t.base(), f.base(), c, base());
      }

      llvm::ReturnInst* ret(llvm::Value* v) {
         return llvm::ReturnInst::Create(v->getContext(), v, base());
      }

      llvm::CallInst*
      call(llvm::Value* f, const std::vector<llvm::Value*>& args) {
         return llvm::CallInst::Create(f, args, "", base());
      }

   private:
      llvm::BasicBlock* block;

      template<typename T>
      T* enque(T* t) {
         block->getInstList().push_back(t);
         return t;
      }
   };

   static BlockRef basic_block(LlvmBackend* be, llvm::Function* fun) {
      auto block = llvm::BasicBlock::Create(be->llvm_context(), ".L", fun);
      return { block };
   }

   // Return the most recently added basic block to the body of
   // the current function.
   BlockRef current_block(LlvmBackend* be) {
      auto fun = current_function(be);
      if (fun->back().getTerminator() == nullptr)
         return { &current_function(be)->back() };
      return basic_block(be, fun);
   }
   
   // Apologize for lack of translation of a given Liz feature.
   static void not_yet_implemented(const Expression& x)
   {
      std::string msg = "LLVM translation not yet implemented for: "; 
      evaluation_error(msg + typeid(x).name());
   }

   static std::unique_ptr<llvm::Module>
   make_unit(llvm::LLVMContext& ctx, const char* name)
   {
      auto unit = std::make_unique<llvm::Module>(name, ctx);
      unit->setTargetTriple(llvm::Triple::normalize(LIZ_HOST_TRIPLET));
      return unit;
   }

   using State = LlvmBackend::State;
   
   LlvmBackend::LlvmBackend()
         : state{ llvm::getGlobalContext() },
      target_machine{ llvm::EngineBuilder().selectTarget() },
      data_layout{ target_machine->createDataLayout() },
      unit{ make_unit(llvm_context(), "<stdin>") }
   { }

   // Return the LLVM translation of a Liz type from a cache, if there.
   // Otherwise, return null.
   static llvm::Type* lookup_type(State& s, const Type* t) {
      auto p = s.type_dict.find(t);
      return p != s.type_dict.end() ? p->second : nullptr;
   }

   // Translate a Liz basic type to an LLVM type.
   static llvm::Type*
   to_llvm(llvm::LLVMContext& ctx, const BasicType& t) {
      auto traits = t.data_traits();
      switch (traits->mode) {
      case Data::Mode::Void:
         return llvm::Type::getVoidTy(ctx);
      case Data::Mode::Bool:
         return llvm::Type::getInt1Ty(ctx);
      case Data::Mode::Int8:
      case Data::Mode::Uint8:
         return llvm::Type::getInt8Ty(ctx);
      case Data::Mode::Int16:
      case Data::Mode::Uint16:
         return llvm::Type::getInt16Ty(ctx);
      case Data::Mode::Int:
      case Data::Mode::Int32:
      case Data::Mode::Uint32:
         return llvm::Type::getInt32Ty(ctx);
      case Data::Mode::Int64:
      case Data::Mode::Uint64:
         return llvm::Type::getInt64Ty(ctx);
      case Data::Mode::Dfloat:
         return llvm::Type::getDoubleTy(ctx);
      case Data::Mode::Pointer:
         return llvm::Type::getVoidTy(ctx)->getPointerTo();
      default:
         abort();
         break;
      }

      return nullptr;
   }

   // Trsnalate a Liz array type to an LLVM type expression.
   static llvm::Type* to_llvm(LlvmBackend* be, const ArrowType& t) {
      auto target = be->translate_type(t.target());
      std::vector<llvm::Type*> source;
      for (auto p : t.source())
         source.push_back(be->translate_type(p));
      return llvm::FunctionType::get(target, source, /* isVarArg */ false);
   }

   // Translate a Liz reference type to an LLVM type expression.
   static llvm::Type* to_llvm(LlvmBackend* be, const ReferenceType& t) {
      auto x = be->translate_type(t.referee());
      return x->getPointerTo();
   }

   // Translate a Liz record type to an LLVM type expression.
   static llvm::Type* to_llvm(LlvmBackend* be, const RecordType& t) {
      std::vector<llvm::Type*> fields;
      for (auto p : t.components())
         fields.push_back(be->translate_type(p));
      return llvm::StructType::get(be->llvm_context(), fields, /* isPacked */ false);
   }

   // Entry point for translating a Liz type to an LLVM type expression.
   llvm::Type*
   LlvmBackend::translate_type(const Type* t)
   {
      if (auto x = lookup_type(state, t))
         return x;

      struct V : Type::Visitor {
         LlvmBackend* be;
         llvm::Type* result;
         V(LlvmBackend* b) : be(b), result() { }
         void visit(const Expression& t) override { not_yet_implemented(t); }
         void visit(const BasicType& t) override {
            result = to_llvm(be->state.ctx, t);
         }
         void visit(const ReadonlyType& t) override {
            result = be->translate_type(t.type());
         }
         void visit(const TagType& t) override {
            result = be->translate_type(t.type());
         }
         void visit(const ArrowType& t) override {
            result = to_llvm(be, t);
         }
         void visit(const ReferenceType& t) override {
            result = to_llvm(be, t);
         }
         void visit(const RecordType& t) override {
            result = to_llvm(be, t);
         }
         void visit(const RestrictedType& t) override {
            result = be->translate_type(t.type());
         }
      };

      V v { this };
      t->accept(v);
      return state.type_dict[t] = v.result;
   }

   // Auxilary data structure used for generating a function definition.
   struct FunDef {
      Symbol name;
      const ArrowType* type;
      const Lambda* init;
   };

   // Begin generating the entry basic block of a function definition.
   static BlockRef
   start_function(LlvmBackend* be, llvm::Function* fun, const Lambda* lambda) {
      auto frame = be->current_frame();
      auto entry = basic_block(be, fun);
      auto arity = lambda->arity();
      auto arg = fun->arg_begin();
      for (int i = 0; i < arity; ++i, ++arg) {
         auto parm = lambda->parameter(i);
         if (auto name = parm->name()) {
            auto& txt = name->symbol().string();
            arg->setName(txt);
            auto loc = entry.allocate(arg->getType(), txt);
            frame->bind(name->symbol(), loc);
            entry.store(arg, loc);
         }
      }
      return entry;
   }

   // Ensure that a function body evaluates to a return instruction.
   static llvm::Instruction*
   ensure_return(LlvmBackend* be, Elaboration e) {
      struct V : Expression::Visitor {
         LlvmBackend* be;
         llvm::Instruction* result;
         V(LlvmBackend* b) : be(b), result() { }

         void visit(const Expression& x) {
            auto expr = convert_expr(be, { be->value_mode(), &x });
            auto block = current_block(be);
            result = block.ret(expr);
         }

         void visit(const Return& x) {
            auto expr = convert_expr(be, x.expression());
            auto block = current_block(be);
            result = block.ret(expr);
         }
      };

      return accept_visitor<V, llvm::Instruction*>(be, e);
   }

   static llvm::Function*
   make_internal_function(LlvmBackend* be, llvm::FunctionType* t,
                          const std::string& s = "") {
      auto linkage = llvm::Function::InternalLinkage;
      return llvm::Function::Create(t, linkage, s, be->module_unit());
   }

   // Generate code for a function definition.
   static llvm::Value*
   emit_function_definition(LlvmBackend* be, const FunDef& fun) {
      auto target = be->translate_type(fun.type->target());
      std::vector<llvm::Type*> source;
      for (auto t : fun.type->source()) {
         auto x = be->translate_type(t);
         source.push_back(x);
      }
      auto ftype = llvm::FunctionType::get(target, source, false);
      auto sym = make_internal_function(be, ftype, fun.name.string());
      LlvmBackend::FrameManager new_cxt { be, sym };
      be->associate(fun.init, sym);
      start_function(be, sym, fun.init);
      ensure_return(be, fun.init->body());
      llvm::verifyFunction(*sym);
      return sym;
   }
   
   // Variable definition info.
   struct VarDef {
      Symbol name;
      llvm::Type* type;
      llvm::Value* init;
   };

   // Emit LLVM expression for global variable definition.
   static llvm::Value*
   emit_global_var_definition(LlvmBackend* be, const VarDef& var) {
      auto val = llvm::dyn_cast<llvm::Constant>(var.init);
      if (val == nullptr)
         evaluation_error("LLVM lowring: can't initialize global variable");
      auto linkage = llvm::GlobalVariable::LinkageTypes::InternalLinkage;
      auto sym = new llvm::GlobalVariable(*be->module_unit(), var.type,
                                          /* isConstant= */true, linkage,
                                          val, var.name.string());
      return sym;
   }

   // Translate a Liz symbol binding to an LLVM definition.
   static llvm::Value*
   convert(LlvmBackend* be, const Bind& x) {
      if (auto ftype = is<ArrowType>(x.type())) {
         auto name = x.name()->symbol();
         auto lam = is<Lambda>(x.initializer()); // FIXME: Assert.
         return emit_function_definition(be, { name, ftype, lam });
      }
      auto name = x.name()->symbol();
      auto type = be->translate_type(x.type());
      auto init = convert_expr(be, x.initializer());
      if (be->at_toplevel())
         return emit_global_var_definition(be, { name, type, init });

      auto block = current_block(be);
      auto loc = block.allocate(type);
      return block.store(init, loc);
   }

   // Generate code for reference to a function parameter.
   static llvm::Value* convert(LlvmBackend* be, const Formal& x) {
      auto frame = be->current_frame();
      auto name = x.name()->symbol();
      auto decls = frame->lookup(name);
      return decls.begin()->value();
   }

   // Generate code for a condition expression.
   static llvm::Instruction* convert(LlvmBackend* be, const If& x) {
      auto block = current_block(be);
      auto cond = convert_expr(be, x.condition());
      auto fun = be->current_frame()->function();
      auto type = be->translate_type(be->value_mode());

      auto t_br = basic_block(be, fun);
      auto t_val = convert_expr(be, x.consequence());

      auto f_br = basic_block(be, fun);
      auto f_val = convert_expr(be, x.alternative());

      block.jump_if(cond, t_br, f_br);

      auto merge = basic_block(be, fun);
      t_br.jump_to(merge);
      f_br.jump_to(merge);
      auto phi = merge.phi(type, 2);
      phi->addIncoming(t_val, t_br.base());
      phi->addIncoming(f_val, f_br.base());
      return phi;
   }

   // Translate a Liz boolean constant to an LLVM constant.
   static llvm::Constant* convert(State& s, const Bool& x) {
      if (x)
         return llvm::ConstantInt::getTrue(llvm::Type::getInt1Ty(s.ctx));
      return llvm::ConstantInt::getFalse(llvm::Type::getInt1Ty(s.ctx));
   }

   // Translate a Liz integer constant to an LLVM constant.
   template<typename T>
   static llvm::Constant* convert(LlvmBackend* be, const T& x) {
      auto t = be->translate_type(x.type());
      return llvm::ConstantInt::get(t, x.rep());
   }

   // Translate a Liz character constant to an LLVM constant.
   static llvm::Constant* convert(LlvmBackend* be, const Char& x) {
      auto t = be->translate_type(x.type());
      return llvm::ConstantInt::get(t, CodePoint(x.rep()));
   }

   // Translate a Liz floating point constant to an LLVM constant.
   static llvm::Constant* convert(LlvmBackend* be, const Double& x) {
      auto t = be->translate_type(x.type());
      return llvm::ConstantFP::get(t, x.rep());
   }

   // Translate a Liz string literal to an LLVM constant.
   static llvm::Constant* convert(LlvmBackend* be, const String& x) {
      auto& s = x.rep().string();
      return llvm::ConstantDataArray::getString(be->llvm_context(), { s.c_str(), s.length() + 1 }, /* addNull= */false);
   }

   // -- Component selection.
   static llvm::Value* convert(LlvmBackend* be, const Component& x) {
      auto path = &x;
      Elaboration entity { };
      while (auto ns = is<Namespace>(path->whole())) {
         entity = ns->select(path->link_name())->value();
         path = is<Component>(entity);
         if (path == nullptr)
            break;
      }
      if (!entity)
         internal_error("reference to non-existent entity "
                        + quote(x.name()));
      if (path != nullptr)
         internal_error("reference to non-toplevel component "
                        + quote(x.name()));
      return convert_expr(be, entity);
   }

   // -- Generate code for an lvalue-to-rvalue conversion.
   static llvm::Value* convert(LlvmBackend* be, const Read& x) {
      if (auto ref = is<Component>(x.address()))
         return convert(be, *ref);
      auto address = convert_expr(be, x.address());
      auto block = current_block(be);
      return block.load(address);
   }

   // -- Generate code for storing a value at a given place.
   static llvm::StoreInst* convert(LlvmBackend* be, const Write& x) {
      auto value = convert_expr(be, x.value());
      auto address = convert_expr(be, x.address());
      auto block = current_block(be);
      return block.store(value, address);
   }

   // -- Helper class for relational comparison instruction code generation.
   struct RelInfo {
      std::string name;
      llvm::CmpInst::Predicate fpred;
      llvm::CmpInst::Predicate upred;
      llvm::CmpInst::Predicate spred;
   };

   template<typename T>
   static llvm::Instruction*
   convert_relational(LlvmBackend* be, const T& x, const RelInfo& cmp) {
      if (x.lhs().type() != x.rhs().type())
         evaluation_error("incompatible operands types for " + cmp.name);
      auto lhs = convert_expr(be, x.lhs());
      auto rhs = convert_expr(be, x.rhs());
      auto block = current_block(be);
      if (lhs->getType()->isFloatingPointTy())
         return block.fcmp(cmp.fpred, lhs, rhs);
      return block.icmp(cmp.upred, lhs, rhs);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Langle& x) {
      const RelInfo cmp {
         "<", llvm::FCmpInst::FCMP_OLT,
            llvm::FCmpInst::ICMP_ULT, llvm::FCmpInst::ICMP_SLT
      };
      
      return convert_relational(be, x, cmp);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Langleq& x) {
      const RelInfo cmp {
         "<", llvm::FCmpInst::FCMP_OLE,
            llvm::FCmpInst::ICMP_ULE, llvm::FCmpInst::ICMP_SLE
      };
      
      return convert_relational(be, x, cmp);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Rangle& x) {
      const RelInfo cmp {
         "<", llvm::FCmpInst::FCMP_OGT,
            llvm::FCmpInst::ICMP_UGT, llvm::FCmpInst::ICMP_SGT
      };
      
      return convert_relational(be, x, cmp);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Rangleq& x) {
      const RelInfo cmp {
         "<", llvm::FCmpInst::FCMP_OGE,
            llvm::FCmpInst::ICMP_UGE, llvm::FCmpInst::ICMP_SGE
      };
      
      return convert_relational(be, x, cmp);
   }

   // Helper class for equality comparison instruction code generation.
   namespace {
      struct EqInfo {
         std::string name;
         llvm::CmpInst::Predicate fpred;
         llvm::CmpInst::Predicate ipred;
      };
   }

   template<typename T>
   static llvm::Instruction*
   convert_equality(LlvmBackend* be, const T& x, const EqInfo& cmp) {
      if (x.lhs().type() != x.rhs().type())
         evaluation_error("incompatible operands types for " + cmp.name);
      auto lhs = convert_expr(be, x.lhs());
      auto rhs = convert_expr(be, x.rhs());
      auto block = current_block(be);
      if (lhs->getType()->isFloatingPointTy())
         return block.fcmp(cmp.fpred, lhs, rhs);
      return block.icmp(cmp.ipred, lhs, rhs);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Eqeq& x) {
      const EqInfo cmp {
         "<", llvm::FCmpInst::FCMP_OEQ, llvm::FCmpInst::ICMP_EQ
      };
      
      return convert_equality(be, x, cmp);
   }

   static llvm::Instruction* convert(LlvmBackend* be, const Excleq& x) {
      const EqInfo cmp {
         "<", llvm::FCmpInst::FCMP_ONE, llvm::FCmpInst::ICMP_NE
      };
      
      return convert_equality(be, x, cmp);
   }

   // -- Arithmetic operations.
   namespace {
      struct ArithInfo {
         std::string name;
         llvm::Instruction::BinaryOps fop;
         llvm::Instruction::BinaryOps iop;
      };
   }

   template<typename T>
   static llvm::BinaryOperator*
   convert_arith(LlvmBackend* be, const T& x, const ArithInfo& cmp) {
      if (x.lhs().type() != x.rhs().type())
         evaluation_error("incompatible operands types for " + cmp.name);
      auto lhs = convert_expr(be, x.lhs());
      auto rhs = convert_expr(be, x.rhs());
      auto block = current_block(be);
      if (lhs->getType()->isFloatingPointTy())
         return block.apply(cmp.fop, lhs, rhs);
      return block.apply(cmp.iop, lhs, rhs);
   }

   static llvm::BinaryOperator* convert(LlvmBackend* be, const Plus& x) {
      ArithInfo op {
         "+", llvm::Instruction::FAdd, llvm::Instruction::Add
      };
      return convert_arith(be, x, op);
   }

   static llvm::BinaryOperator* convert(LlvmBackend* be, const Dash& x) {
      ArithInfo op {
         "-", llvm::Instruction::FSub, llvm::Instruction::Sub
      };
      return convert_arith(be, x, op);
   }

   static llvm::BinaryOperator* convert(LlvmBackend* be, const Star& x) {
      ArithInfo op {
         "*", llvm::Instruction::FMul, llvm::Instruction::Mul
      };
      return convert_arith(be, x, op);
   }

   static llvm::BinaryOperator* convert(LlvmBackend* be, const Div& x) {
      ArithInfo op {
         "div", llvm::Instruction::FDiv, llvm::Instruction::SDiv
      };
      return convert_arith(be, x, op);
   }

   static llvm::BinaryOperator* convert(LlvmBackend* be, const Rem& x) {
      ArithInfo op {
         "rem", llvm::Instruction::FRem, llvm::Instruction::SRem
      };
      return convert_arith(be, x, op);
   }

   // -- Lambda
   static llvm::Value* convert(LlvmBackend* be, const Lambda& x) {
      return be->function(&x);
   }

   // -- Function calls.
   static std::vector<llvm::Value*>
   convert_arguments(LlvmBackend* be, const Arguments& args) {
      std::vector<llvm::Value*> values;
      for (auto& x : args)
         values.push_back(convert_expr(be, x));
      return values;
   }

   static llvm::Value* convert(LlvmBackend* be, const Call& x) {
      auto args = convert_arguments(be, x.arguments());
      auto fun = convert_expr(be, x.function());
      auto block = current_block(be);
      return block.call(fun, args);
   }

   static llvm::Value* convert_expr(LlvmBackend* be, Elaboration e) {
      struct V : Expression::Visitor {
         LlvmBackend* be;
         llvm::Value* result;
         V(LlvmBackend* b) : be(b), result() { }

         void visit(const Expression& x) override { not_yet_implemented(x); }
         void visit(const Bool& x) override { result = convert(be->state, x); }
         void visit(const Byte& x) override { result = convert(be, x); }
         void visit(const Char& x) override { result = convert(be, x); }
         void visit(const Int& x) override { result = convert(be, x); }
         void visit(const Double& x) override { result = convert(be, x); }
         void visit(const String& x) override { result = convert(be, x); }

         void visit(const Lambda& x) override { result = convert(be, x); }
         void visit(const Bind& x) override { result = convert(be, x); }
         void visit(const Call& x) override { result = convert(be, x); }

         void visit(const Component& x) override {
            result = convert(be, x);
         }
         void visit(const Formal& x) override { result = convert(be, x); }
         void visit(const Read& x) override { result = convert(be, x); }
         void visit(const Write& x) override { result = convert(be, x); }
         void visit(const If& x) override { result = convert(be, x); }
         void visit(const Plus& x) override { result = convert(be, x); }
         void visit(const Dash& x) override { result = convert(be, x); }
         void visit(const Star& x) override { result = convert(be, x); }
         void visit(const Div& x) override { result = convert(be, x); }
         void visit(const Rem& x) override { result = convert(be, x); }
         
         void visit(const Langle& x) override { result = convert(be, x); }
         void visit(const Langleq& x) override { result = convert(be, x); }
         void visit(const Rangle& x) override { result = convert(be, x); }
         void visit(const Rangleq& x) override { result = convert(be, x); }
         void visit(const Eqeq& x) override { result = convert(be, x); }
         void visit(const Excleq& x) override { result = convert(be, x); }
      };
      
      return accept_visitor<V, llvm::Value*>(be, e);
   }

   // Associate a lambda expression with its LLVM translation.
   void LlvmBackend::associate(const Lambda* lambda, llvm::Function* fun) {
      auto p = state.fun_dict.insert({ lambda, fun });
      if (!p.second and fun != p.first->second)
         internal_error("Function " + quote(lambda->name())
                        + " has an existing associated routine");
   }   

   // Retried the LLVM value assocuated with a lambda expression.
   llvm::Function* LlvmBackend::function(const Lambda* lambda) {
      auto p = state.fun_dict.find(lambda);
      if (p == state.fun_dict.end())
         internal_error("Function " + quote(lambda->name())
                        + " has no associated routine");
      return p->second;
   }

   llvm::Value* LlvmBackend::lower_stmt(Elaboration e) {
      return convert_expr(this, e);
   }

   void LlvmBackend::format(llvm::Value* x, std::ostream& os) {
      llvm::raw_os_ostream out { os };
      x->print(out);
   }

   const char* LlvmBackend::path_extension() const {
      return "ll";              // for now.
   }

   // -- LlvmEvaluator

   static llvm::Function*
   toplevel_wrapper(LlvmBackend* be, llvm::Type* target) {
      auto ftype = llvm::FunctionType::get(target, { }, false);
      return make_internal_function(be, ftype);
   }

   static llvm::Function*
   wrap(LlvmBackend* be, Elaboration e) {
      auto target = be->translate_type(e.type());
      auto fun = toplevel_wrapper(be, target);
      LlvmBackend::FrameManager new_ctx { be, fun };
      auto block = basic_block(be, fun);
      auto body = be->lower_stmt(e);
      body->dump();
      block.ret(body);
      return fun;
   }
   
   LlvmEvaluator::LlvmEvaluator()
         : ir_compiler{ linker, llvm::orc::SimpleCompiler(*target_machine) },
      jit_vm(),
      elab()
   {
      std::string s;
      // FIXME: Next line is most likely wrong
      llvm::EngineBuilder builder { unit };
      builder.setErrorStr(&s);
      jit_vm = builder.create();
      if (jit_vm == nullptr)
         std::cerr << "JIT failed to initialize: " << s << std::endl;
      llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
   }
   
   Object LlvmEvaluator::eval(Elaboration x) {
      auto fun = wrap(this, x);
      if (jit_vm == nullptr)
         return { };
      auto ptr = jit_vm->getPointerToFunction(fun);
      auto wrapper = reinterpret_cast<Data::Value (*)()>(ptr);
      return { x.type(), wrapper() };
   }

   void LlvmEvaluator::set_elaborator(Elaborator* e) { elab = e; }
}
