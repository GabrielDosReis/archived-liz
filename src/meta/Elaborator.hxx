// -*- C++ -*-
// Copyright (C) 2009-2013, Texas A&M University.
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


#ifndef LIZ_ELABORATOR_INCLUDED
#define LIZ_ELABORATOR_INCLUDED

#include <type_traits>
#include <list>
#include <vector>
#include <map>
#include <stack>
#include <liz/Ast>
#include "Parser.H"
#include "Translator.H"
#include "Expression.H"

namespace liz {
   // --------------
   // -- Evidence --
   // --------------
   // Instantiation evidence; like a substituion, but maps only
   // signatures to functions.
   struct Evidence : std::map<const Signature*, const Function*> {
      Evidence();
      const Function* operator()(const Signature*) const;
      bool good() const { return ok; }
      void set_bad() { ok = false; }
      // convenience conversion function to be used in
      // conditional context to test for well-formedness.
      operator const void*() const { return good() ? this : 0; }

      void send_to(const Signature*, const Function*);
      void send_to(const Constraint*, const Evidence*);

   private:
      using ConstraintMap = std::map<const Constraint*, const Evidence*>;
      bool ok;                  // true if this is a good evidence.
      std::map<const Constraint*, const Evidence*> sub_evidence_map;
   };


   // ------------------------------
   // -- SpecializationRepository --
   // ------------------------------
   // Repository for specializations of a given template.
   struct SpecializationRepository : std::map<Substitution,
                                              const Expression*> {
   };

   struct Elaborator;
   // -- A simpple evaluator interface
   struct BasicEvaluator {
      virtual Object eval(Elaboration) = 0;
      virtual void set_elaborator(Elaborator*) = 0;
   };

   // ----------------
   // -- Elaborator --
   // ----------------
   struct Elaborator: ExpressionFactory, ScopeStack {
      struct DeclContext;

      // -- Source-level location manager
      struct LocationManager {
         LocationManager(Elaborator*, const Token&);
         ~LocationManager();
      private:
         Elaborator* ctx;
      };

      Elaborator(Reader&, BasicEvaluator*);
      ~Elaborator();

      BasicContext<> basic_context();
      // Override Translator::elaborate.
      Elaboration elaborate(const Ast* x, const Type* t = nullptr);

      // Generate code that coeces a value to a target type, otherwise
      // raise an exception.
      Elaboration coerce(Elaboration, const Type*);

      // -- We have an expression that semantically designates a type.
      // -- Return the typeful version representation reflecting that
      // -- knowledge.
      TypeElaboration coerce_to_type(Elaboration);

      // Current source-level location
      const Token& current_location() const;

      // Manage the type-stack of value-producing blocks.
      void push_return_type(const Type*);
      const Type* get_return_type() const;
      void pop_return_type();
      int frame_depth() const;

      // Management of declaration contexts.
      DeclContext* top_decl_context() const;

      // Manage parameter declaration level.
      void increase_parameter_depth() { ++parms_level; }
      void decrease_parameter_depth() { --parms_level; }
      int get_parameter_depth() const { return parms_level; }


      Namespace* global_scope() const { return global_ns; }
      Namespace* liz_scope() const { return liz_ns; }
      const BasicType* get_concept() const { return concept_type; }
      const BasicType* get_prop() const;
      const BasicType* get_axiom() const { return axiom_type; }
      const Constructor* get_Function() const { return Function_ctor; }
      TypeElaboration get_typename() const;
      TypeElaboration get_LoadUnit() const;
      TypeElaboration get_namespace() const;
      TypeElaboration get_address() const;
      TypeElaboration get_void() const;
      TypeElaboration get_bool() const;
      TypeElaboration get_byte() const;
      TypeElaboration get_char() const;
      TypeElaboration get_int() const;
      TypeElaboration get_double() const;
      TypeElaboration get_string() const;

      FunctionElaboration get_arity() const { return arity_fun; }
      FunctionElaboration get_input_type() const { return input_type_fun; }
      FunctionElaboration get_codomain() const { return codomain_fun; }

      // Return the function type of the (builtin) equality
      // operator on types.
      FunctionElaboration get_type_eq() const { return type_eq_fun; }

      const Evidence* register_evidence(const Constraint*, const Evidence&);
      const Evidence* find_evidence(const Constraint*) const;

      // template specialization management management.
      const Expression*
      retrieve_specialization(const Expression*, const Substitution&) const;
      void register_specialization(const Expression*, const Expression*,
                                   const Substitution&);

      void sorry(const std::string&);

      Reader* reader() { return rdr; }
      IOContext* io() { return reader(); }
      bool enabled(const Debug& f) {
         return reader()->invocation()->enabled(f);
      }

      // Return a pointer to a module with a given name, if already loaded.
      LoadUnit* get_unit_if_loaded(const Name*) const;
      
   protected:
      using ModeStack = std::stack<const Type*>;
      using EvidenceRepository = std::map<const Constraint*, Evidence>;
      using AllSpecs = std::map<const Expression*, SpecializationRepository>;
      Reader* rdr;
      BasicEvaluator* evl;
      std::stack<Token> locs;
      std::map<const Name*, LoadUnit*> loaded_modules;
      const BasicType* const concept_type;
      const BasicType* const typename_type;
      const BasicType* const LoadUnit_type;
      const BasicType* const prop_type;
      const BasicType* const axiom_type;
      const Constructor* const Function_ctor;
      const Constructor* const Regular_ctor;
      const BasicType* const namespace_type;
      const BasicType* const void_type;
      const BasicType* const address_type;
      const BasicType* const bool_type;
      const BasicType* const byte_type;
      const BasicType* const char_type;
      const BasicType* const int_type;
      const BasicType* const double_type;
      const BasicType* const string_type;
      Namespace* const global_ns;
      Namespace* const liz_ns;
      ModeStack exit_modes;  // stack of expected return values.
      int parms_level;       // current parameters nesing level;
      int new_formal_counter;
      FunctionElaboration arity_fun; // built-in function Arity
      FunctionElaboration input_type_fun; // built-in function InputType
      FunctionElaboration codomain_fun;   // built-in function Codomain
      FunctionElaboration type_eq_fun; // holds equality operator on types.
      EvidenceRepository evidences;
      std::vector<DeclContext*> decls;
      AllSpecs specs;
      
      void define_binary_builtin(const std::string&, BinaryBuiltinFunction,
                                 const Type*, const Type*, const Type*);
      
      // define all builtin types in the global environment.
      void define_builtin_types();

      // define all intrinsic operations.
      void define_intrinsics();
   };

   // Return default elaboration of a type expression.
   TypeElaboration type_elaboration(Elaborator*, const Type*);

   // Return a function type for a binary predicate taking arguments
   // of type denotated by `t'.
   const ArrowType* get_binary_predicate_type(Elaborator*, TypeElaboration);


   // Return a pointer to the declaration of equality operator
   // predicate with a given argument type.
   const Function* select_equality_operator(Elaborator*, const Type*);

   // Pattern match an expression (first argument) against a pattern
   // (second argument), and return the matching substitution;
   Substitution pattern_match(Elaboration, const Expression*);

   // Attempt to simplify an elaboration as much as it is safe
   // to do.  This routine is most useful during elaboration and
   // and property deduction.
   Elaboration evaluate(BasicContext<>&, Elaboration);

   Elaboration eq_close_term_if_can(EquivalenceQuotient&, Elaboration);

   inline TypeElaboration
   convert_syntax_to_type(Elaborator* context, const char* s) {
      auto expr = context->get_unambiguous_binding(make_identifier(context, s));
      return context->coerce_to_type(*expr);
   }

   // ------------------
   // -- ScopeManager --
   // ------------------
   // This is a helper class automating push/pop of name binding contours.
   struct ScopeManager {
      ScopeManager(Elaborator*, Scope&);
      Elaborator* context() const { return ctx; }
      ~ScopeManager();
   private:
      Elaborator* ctx;
   };

   // -- BasicContext --
   template<typename T>
   struct BasicContext {
      BasicContext(Elaborator* elb, T* evl)
            : elab(elb),
              evaler(evl),
              home(elab->top())
      { }
      Reader* reader() const { return elab->reader(); }
      Elaborator* elaborator() const { return elab; }
      T* evaluator() const { return evaler; }
      ScopeRef scope() const { return home; }
      ScopeStack& scope_stack() { return *elaborator(); }
      template<typename U,
               std::enable_if_t<std::is_base_of<U, T>::value, U>* = nullptr>
      operator BasicContext<U>() { return { elab, evaler }; }
               
   private:
      Elaborator* const elab;
      T* evaler;
      ScopeRef home;
   };

   const Lambda* substitute(BasicContext<>&, const Lambda*, Substitution);
   FunctionElaboration
   substitute(BasicContext<>&, FunctionElaboration, const Substitution&);

   Elaboration
   subst_expr(BasicContext<>&, Elaboration, const Substitution&);

   FunctionElaboration
   substitute(BasicContext<>&, FunctionElaboration, const Evidence&);

   const Type* simplify_type(BasicContext<>&, const Type*);

   template<typename T>
   inline const Type* simplify_type(BasicContext<T>& ctx, const Type* t) {
      BasicContext<> basic = ctx;
      return simplify_type(basic, t);
   }


   // FIXME: To be removed soon.
   const Value* reify(BasicContext<>&, Object);
   template<typename T>
   inline const Value* reify(BasicContext<T>& ctx, Object obj) {
      BasicContext<> basic = ctx;
      return reify(basic, obj);
   }

   inline BasicContext<> Elaborator::basic_context() {
      return { this, evl };
   }
}

#endif  // LIZ_ELABORATOR_INCLUDED
