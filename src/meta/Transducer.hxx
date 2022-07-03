// -*- C++ -*-
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

#ifndef LIZ_TRANSDUCER_included
#define LIZ_TRANSDUCER_included

#include "Expression.H"

namespace liz {
   // -----------
   // -- Frame --
   // -----------
   template<typename T, typename F>
   struct Frame : Store<T> {
      explicit Frame(F f)
            : Store<T>(Contour::dynamic), fun(f)
      { }
      
      F function() const { return fun; }
      
      Store<T>* push_store(Contour c = Contour::local) {
         st.push(c);
         return &st.top();
      }
      
      void pop_store() { st.pop(); }
      
   private:
      const F fun;
      Stack<Store<T>> st;
   };

   template<typename T, typename F> struct FrameManager;

   // -- ControlStack --
   template<typename T, typename F>
   struct ControlStack : Stack<Frame<T, F>> { };

   // -- StoreStack --
   template<typename T>
   struct StoreStack : Stack<StoreRef<T>> { };

   // ----------------
   // -- Transducer --
   // ----------------
   template<typename T, typename F>
   struct Transducer {
      using FrameManager = liz::FrameManager<T, F>;

      Transducer() {
         push_frame({ });
      }
      
      Frame<T, F>* push_frame(F f) {
         ctrl.push(f);
         return &ctrl.top();
      }
      
      Frame<T, F>* current_frame() {
         if (ctrl.empty())
            return nullptr;
         return &ctrl.top();
      }
      
      void pop_frame() { ctrl.pop(); }

      StoreStack<T>* get_store() { return &store; }

   protected:
      StoreStack<T> store;         // global store stack
      ControlStack<T, F> ctrl;     // Control stack
   };

   // -- StoreManager --
   template<typename T, typename F>
   struct StoreManager {
      StoreManager(Transducer<T, F>* e, Store<T>* s)
            : env(e) {
         env->get_store()->push(s);
      }
      ~StoreManager() { env->get_store()->pop(); }
      
      StoreRef<T> operator->() { return env->get_store()->top(); }
   protected:
      Transducer<T, F>* const env;
   };

   // -- LocalStoreManager --
   template<typename T, typename F>
   struct LocalStoreManager : StoreManager<T, F> {
      LocalStoreManager(Transducer<T, F>* e)
            : StoreManager<T, F>(e, e->current_frame()->push_store())
      { }
      
      ~LocalStoreManager() {
         this->env->current_frame()->pop_store();
      }
   };

   template<typename T, typename F>
   struct NonlocalStoreManager : StoreManager<T, F> {
      NonlocalStoreManager(Transducer<T, F>* e)
            : StoreManager<T, F>(e, e->current_frame()->push_store(Contour::nonlocal))
      { }
      
      ~NonlocalStoreManager() {
         this->env->current_frame()->pop_store();
      }
   };

   // -- FrameManager --
   template<typename T, typename F>
   struct FrameManager : StoreManager<T, F> {
      FrameManager(Transducer<T, F>* e, F f)
            : StoreManager<T, F>(e, e->push_frame(f))
      { }
      
      ~FrameManager() {
         this->env->pop_frame();
      }
   };

   // -- Generic expression graph transducer, parameterized by the
   // -- the type of the result of transformation.
   template<typename Env, typename Val>
   struct TransducerVisitor : Expression::Visitor {
      Env* const env;  // current evaluation environment
      Val result;  // result of visiting an expression node
      TransducerVisitor(Env* e)
            : env(e), result() { }

      TransducerVisitor(Env& e)
            : env(&e), result() { }
      
      void visit(const Expression& x) override {
         evaluation_error("unexpected expression of type "
                          + show_cxx_type(&x));
      }
      
      void visit(const Value& x) override { result = x.get(); }
      void visit(const TypeExpression& x) override { result = convert(env, x); }
      void visit(const Postulate& x) override { result = convert(env, x); }
      void visit(const Assertion& x) override { result = convert(env, x); }
      void visit(const DotSelection& x) override { result = convert(env, x); }
      void visit(const Component& x) override { result = convert(env, x); }
      void visit(const Block& x) override { result = convert(env, x); }
      void visit(const LinkName& x) override { result = convert(env, x); }
      void visit(const Formal& x) override { result = convert(env, x); }
      void visit(const Initializer& x) override { result = convert(env, x); }
      void visit(const Let& x) override { result = convert(env, x); }
      void visit(const Read& x) override { result = convert(env, x); }
      void visit(const Write& x) override { result = convert(env, x); }
      void visit(const UnaryExpression& x) override { result = convert(env, x); }
      void visit(const BinaryExpression& x) override { result = convert(env, x); }
      void visit(const BinaryLogical& x) override { result = convert(env, x); }
      void visit(const Call& x) override { result = convert(env, x); }
      void visit(const Return& x) override { result = convert(env, x); }
      void visit(const Throw& x) override { result = convert(env, x); }
      void visit(const If& x) override { result = convert(env, x); }
      void visit(const Bind& x) override { result = convert(env, x); }
      void visit(const Import& x) override { result = convert(env, x); }
   };
}

#endif  // LIZ_TRANSDUCER_included
