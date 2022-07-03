// Copyright (C) 2012-2013, Texas A&M University.
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

#include <limits.h>
#include <assert.h>
#include "Elaborator.H"

namespace liz {
   // -- Elaborator::LocationManager
   Elaborator::LocationManager::LocationManager(Elaborator* c, const Token& t)
         : ctx(c) {
      ctx->locs.push(t);
   }

   Elaborator::LocationManager::~LocationManager() {
      ctx->locs.pop();
   }

   const Token&
   Elaborator::current_location() const {
      return locs.top();
   }

   void
   Elaborator::sorry(const std::string& m) {
      throw BasicError(m);
   }

   TypeElaboration
   Elaborator::get_typename() const {
      return TypeElaboration(typename_type, typename_type);
   }

   TypeElaboration
   Elaborator::get_LoadUnit() const {
      return { typename_type, LoadUnit_type };
   }

   TypeElaboration
   Elaborator::get_namespace() const {
      return TypeElaboration(typename_type, namespace_type);
   }

   const BasicType*
   Elaborator::get_prop() const {
      return prop_type;
   }

   TypeElaboration
   Elaborator::get_address() const {
      return TypeElaboration(typename_type, address_type);
   }

   TypeElaboration
   Elaborator::get_void() const {
      return TypeElaboration(typename_type, void_type);
   }

   TypeElaboration
   Elaborator::get_bool() const {
      return TypeElaboration(typename_type, bool_type);
   }

   TypeElaboration
   Elaborator::get_byte() const {
      return TypeElaboration(typename_type, byte_type);
   }

   TypeElaboration
   Elaborator::get_char() const {
      return TypeElaboration(typename_type, char_type);
   }

   TypeElaboration
   Elaborator::get_int() const {
      return TypeElaboration(typename_type, int_type);
   }

   TypeElaboration
   Elaborator::get_double() const {
      return TypeElaboration(typename_type, double_type);
   }

   TypeElaboration
   Elaborator::get_string() const {
      return TypeElaboration(typename_type, string_type);
   }

   const Evidence*
   Elaborator::find_evidence(const Constraint* c) const {
      EvidenceRepository::const_iterator p = evidences.find(c);
      return p == evidences.end() ? nullptr : &p->second;
   }

   const Evidence*
   Elaborator::register_evidence(const Constraint* c, const Evidence& e) {
      auto result = evidences.insert(std::make_pair(c, e));
      if (not result.second)
         internal_error("evidence for constraint " + quote(show_expr(c))
                         + " already in effect");
      return &result.first->second;
   }

   const Expression*
   Elaborator::retrieve_specialization(const Expression* t,
                                       const Substitution& args) const {
      AllSpecs::const_iterator p = specs.find(t);
      if (p == specs.end())
         return nullptr;
      SpecializationRepository::const_iterator q = p->second.find(args);
      if (q == p->second.end())
         return nullptr;
      return q->second;
   }

   void
   Elaborator::register_specialization(const Expression* x,
                                       const Expression* t,
                                       const Substitution& args) {
      specs[t][args] = x;
   }

   void
   Elaborator::push_return_type(const Type* t) {
      exit_modes.push(t);
   }

   const Type*
   Elaborator::get_return_type() const {
      return exit_modes.top();
   }

   void
   Elaborator::pop_return_type() {
      exit_modes.pop();
   }

   int
   Elaborator::frame_depth() const {
      return exit_modes.size();
   }

   Elaborator::DeclContext*
   Elaborator::top_decl_context() const {
      return decls.empty() ? nullptr : decls.back();
   }

   LoadUnit*
   Elaborator::get_unit_if_loaded(const Name* n) const {
      auto p = loaded_modules.find(n);
      if (p == loaded_modules.end())
         return nullptr;
      return p->second;
   }
      

   // -- Builtin constructors.
   namespace {
      struct UnaryConstructor {
         const char* name;
         const char* source;
         const char* target;
      };

      struct BinaryConstructor {
         const char* name;
         const char* source[2];
         const char* target;
      };
   }

   static const UnaryConstructor unary_ctors[] = {
      { "bits", "int", "type" },
      { "buffer", "type", "type" },
      { "list", "type", "type" }
   };
   
   static const BinaryConstructor binary_ctors[] = {
      { "array", { "type", "int" }, "type" }
   };

   static void
   define_builtin_unary_ctors(Elaborator* ctx) {
      for (auto& x : unary_ctors) {
         InputTypes src(1, convert_syntax_to_type(ctx, x.source));
         auto ftype = ctx->make_arrow_type
            (convert_syntax_to_type(ctx, x.target), src);
         auto name = make_identifier(ctx, x.name);
         const Constructor* ctor = ctx->build_constructor({ name, ftype }, { });
         ctx->global_scope()->define(name, ftype, ctor);
      }
   }

   static void
   define_builtin_binary_ctors(Elaborator* ctx) {
      for (auto& x : binary_ctors) {
         InputTypes src(2);
         src[0] = convert_syntax_to_type(ctx, x.source[0]);
         src[1] = convert_syntax_to_type(ctx, x.source[1]);
         auto ftype = ctx->make_arrow_type
            (convert_syntax_to_type(ctx, x.target), src);
         auto name = make_identifier(ctx, x.name);
         const Constructor* ctor = ctx->build_constructor({ name, ftype }, { });
         ctx->global_scope()->define(name, ftype, ctor);
      }
   }

   // Bring to live some builtin constructors.
   static void
   define_builtin_constructors(Elaborator* ctx) {
      define_builtin_unary_ctors(ctx);
      define_builtin_binary_ctors(ctx);
   }

   static void
   define_basic_type(Elaborator* ctx, const BasicType* value) {
      auto name = value->name();
      ctx->global_scope()->define(name, ctx->get_typename(), value);
   }
   
   static void
   define_concept_ctor(Elaborator* ctx, const Constructor* value) {
      auto name = value->name();
      ctx->global_scope()->define(name, value->type(), value);
   }

   void
   Elaborator::define_builtin_types() {
      define_basic_type(this, typename_type);
      define_basic_type(this, LoadUnit_type);
      define_basic_type(this, namespace_type);
      define_basic_type(this, concept_type);
      define_basic_type(this, axiom_type);
      define_basic_type(this, prop_type);
      define_concept_ctor(this, Function_ctor);
      define_concept_ctor(this, Regular_ctor);
      
      define_basic_type(this, void_type);
      define_basic_type(this, address_type);
      define_basic_type(this, bool_type);
      define_basic_type(this, byte_type);
      define_basic_type(this, char_type);
      define_basic_type(this, int_type);
      define_basic_type(this, double_type);
      define_basic_type(this, string_type);
   }


   static void
   define_constant_compile_time_only(Elaborator* ctx, const std::string& name,
                                     const Value* value, const Type* type) {
      ctx->global_scope()->bind(make_identifier(ctx, name), { type, value });
   }

   static void
   define_constant(Elaborator* ctx, const std::string& name,
                   const Value* value, const Type* type) {
      auto scope = ctx->global_scope();
      auto sym = make_identifier(ctx, name);
      if (scope->select(sym, type))
         internal_error("Attempt to redefine constant " + quote(sym));
      scope->define(sym, type, value);
   }
   
   // define all builtin values in the global environment.
   static void
   define_builtin_values(Elaborator* ctx) {
      define_constant_compile_time_only
         (ctx, "false", ctx->build_bool(false, ctx->get_bool()), ctx->get_bool());
      define_constant_compile_time_only
         (ctx, "true", ctx->build_bool(true, ctx->get_bool()), ctx->get_bool());
      define_constant(ctx, "Liz", ctx->liz_scope(), ctx->get_namespace());
   }

   // -- typename data property
   static void
   fmt_typename(Data::Abstract, std::ostream& os, Data::Value v) {
      Data::to_type(v)->print_on(os);
   }

   static const Data::Property typename_prop = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_typename, nullptr }
   };

   // -- LoadUnit data property
   static void
   fmt_LoadUnit(Data::Abstract, std::ostream& os, Data::Value v) {
      os << Data::to_LoadUnit(v)->path();
   }

   static const Data::Property LoadUnit_traits = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_LoadUnit, nullptr }
   };

   // -- Logical proposition
   static void
   fmt_prop(Data::Abstract, std::ostream& os, Data::Value v) {
      os << pretty(Data::to_quote(v));
   }

   static const Data::Property prop_traits = {
      Data::Mode::Pointer,
      sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract),
      { fmt_prop, nullptr }
   };

   // -- Concept data property
   static void
   fmt_concept(Data::Abstract, std::ostream& os, Data::Value v) {
      auto inst = static_cast<const Instance*>(Data::Abstract(v));
      os << *inst;
   }

   static const Data::Property concept_prop = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_concept, nullptr }
   };

   // Return a function type for a binary predicate
   // taking arguments of type denotated by `t'.
   const ArrowType*
   get_binary_predicate_type(Elaborator* context, TypeElaboration t) {
      return context->make_arrow_type(context->get_bool(), InputTypes(2, t));
   }
   
   // -- Return the builtin declaration for type equality operator
   static FunctionElaboration
   type_equality_operator(Elaborator* context) {
      return lookup_function
         (context->bottom(), make_operator(context, "=="),
          get_binary_predicate_type(context, context->get_typename()));
   }
   
   static const BasicType*
   ref_to_type(Elaborator* context, const char* n, const Data::Property* p) {
      return context->make_basic_type(make_identifier(context, n), p);
   }

   static const BasicType*
   ref_to_type(Elaborator* context, const char* n, Data::Mode m) {
      return ref_to_type(context, n, Data::property(m));
   }

   static const Constructor*
   builtin_unary_concept_ctor(Elaborator* ctx, const char* n) {
      InputTypes source { { ctx->get_typename(), ctx->get_typename() } };
      TypeElaboration target { ctx->get_typename(), ctx->get_concept() };
      auto type = ctx->make_arrow_type(target, source);
      LinkName lnk { make_identifier(ctx, n), type };
      return ctx->build_constructor(lnk, { });
   }

   Elaborator::Elaborator(Reader& r, BasicEvaluator* e)
         : rdr(&r),
           evl(e),
           concept_type(ref_to_type(this, "concept", &concept_prop)),
           typename_type(ref_to_type(this, "type", &typename_prop)),
           LoadUnit_type(ref_to_type(this, "LoadUnit", &LoadUnit_traits)),
           prop_type(ref_to_type(this, "prop", &prop_traits)),
           axiom_type(ref_to_type(this, "axiom", Data::Mode::Pointer)),
           Function_ctor(builtin_unary_concept_ctor(this, "Function")),
           Regular_ctor(builtin_unary_concept_ctor(this, "Regular")),
           
           namespace_type(ref_to_type(this, "namespace", Data::Mode::Pointer)),

           void_type(ref_to_type(this, "void", Data::Mode::Void)),
           address_type(ref_to_type(this, "address", Data::Mode::Pointer)),
           bool_type(ref_to_type(this, "bool", Data::Mode::Bool)),
           byte_type(ref_to_type(this, "byte", Data::Mode::Byte)),
           char_type(ref_to_type(this, "char", Data::Mode::Int32)),
           int_type(ref_to_type(this, "int", Data::Mode::Int)),
           double_type(ref_to_type(this, "double", Data::Mode::Dfloat)),
           string_type(ref_to_type(this, "string", Data::Mode::String)),
           global_ns(build_namespace(nullptr, nullptr)),
      liz_ns(build_namespace(make_identifier(this, "Liz"), global_ns)),
           parms_level(-1),
           arity_fun(), input_type_fun(), codomain_fun(), type_eq_fun()
   {
      assert(evl != nullptr);
      push(global_ns);       // install global scope

      define_builtin_types();
      define_builtin_values(this);
      define_builtin_constructors(this);
      define_intrinsics();
      type_eq_fun = type_equality_operator(this);

      // Cache some builtin functions.
      Elaboration* decl = get_unambiguous_binding(make_identifier(this, "Arity"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("Arity") + " failed");
      arity_fun = make_elaboration(to_function(decl->code()));
      decl = get_unambiguous_binding(make_identifier(this, "InputType"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("InputType") + " failed");
      input_type_fun = make_elaboration(to_function(decl->code()));
      decl = get_unambiguous_binding(make_identifier(this, "Codomain"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("Codomain") + " failed");
      codomain_fun = make_elaboration(to_function(decl->code()));
   }

   Elaborator::~Elaborator()
   { }
}
