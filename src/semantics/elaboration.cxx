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


#include <iostream>
#include <algorithm>
#include "Elaborator.H"

namespace liz {
   inline Elaboration
   make_elaboration(const Type* t, const Expression* x) {
      if (t == nullptr)
         abort();
      return Elaboration(t, x);
   }

   // General pattern for elaborating a definition.
   template<typename C>
   static Elaboration
   do_elaborate_definition(C& ctx, const DefinitionAst& def) {
      check_if_redeclaration(ctx);
      auto decl = ctx.commit();
      auto value = elaborate_definiens(ctx, def.initializer());
      return finish_definition(ctx, decl, value);
   }

   // -----------------------------
   // -- Elaborator::DeclContext --
   // -----------------------------
   struct Elaborator::DeclContext : BasicContext<> {
      DeclContext(BasicContext<>& ctx, const Name* n)
            : BasicContext<>(ctx), nm(n)
      {
         elaborator()->decls.push_back(this);
      }
      ~DeclContext() { elaborator()->decls.pop_back(); }
      const Name* name() const { return nm; }
      virtual const Type* type() const = 0;
      Declaration* commit() const { return declare(scope(), { nm, type() }); }
   private:
      const Name* const nm;
      DeclContext(DeclContext&&) = delete;
   };

   using DeclContext = Elaborator::DeclContext;

   // -- Helper functions on fixity forms
   template<typename T>
   static const T*
   is(const FixityForm* f) {
      return dynamic_cast<const T*>(f);
   }

   static inline std::string
   quote(const Token* t) {
      return quote(lexeme(*t));
   }

   static inline std::string
   quote(const Atomic* x) {
      return quote(x->token());
   }

   static std::string
   quote(const BracketAst* x) {
      return quote(lexeme(x->first()) + lexeme(x->second()));
   }

   // -- Format the declarative form of an elaboration.
   static std::ostream&
   format_as_decl(std::ostream& os, Elaboration x) {
      struct V : Expression::Visitor {
         std::ostream& os;
         const Type* type;
         V(std::ostream& o, const Type* t) : os(o), type(t) { }
         void visit(const Expression&) { os << "_ : " << pretty(type); }
         void visit(const Postulate& x) {
            os << x.name()->symbol() << " : " << pretty(x.type());
         }
         void visit(const LinkName& x) {
            os << x.symbol() << " : " << pretty(x.type());
         }
         void visit(const Component& x) {
            os << x.symbol() << " : " << pretty(x.type());
         }
      };
      V v(os, x.type());
      x.code()->accept(v);
      return os;
   }

   // -- Semantics errors --
   namespace {
      template<typename T>
      struct with_decl : T {
         template<typename... Args>
         with_decl(const DeclContext* ctx, const Args&... args)
               : T(args...),
                 nm(ctx ? ctx->name() : nullptr),
                 ty(ctx ? ctx->type() : nullptr)
         { }

      protected:
         void disclose_location_on(std::ostream& os) const {
            if (nm != nullptr) {
               os << "semantics error in the context of " << quote(nm);
               if (ty != nullptr)
                  os << " of type " << quote(show(*ty));
               os << ':' << std::endl;
            }
            T::disclose_location_on(os);
         }

      private:
         const Name* nm;
         const Type* ty;
      };
      
      struct SemanticsErrorAt : with_decl<ErrorAt> {
         SemanticsErrorAt(const DeclContext* x,
                          const std::string& s, const Token& t)
               : with_decl<ErrorAt>(x, s, t)
         { }
      };

      struct UndeclaredError : with_decl<ErrorAt> {
         UndeclaredError(const DeclContext* x, const std::string& s,
                         const Token& t)
               : with_decl<ErrorAt>(x, s, t)
         { }
      };
   }

   static void
   semantics_error(BasicContext<>& ctx, const std::string& msg) {
      auto decl_ctx = ctx.elaborator()->top_decl_context();
      auto loc = ctx.elaborator()->current_location();
      throw SemanticsErrorAt(decl_ctx, msg, loc);
   }

   static void
   semantics_error(BasicContext<>& ctx, const std::string& m, const Token& t) {
      auto decl_ctx = ctx.elaborator()->top_decl_context();
      throw SemanticsErrorAt(decl_ctx, m, t);
   }

   static void
   semantics_error(BasicContext<>& ctx, const std::string& m, const Ast* x) {
      if (const Token* t = anchor(x))
         return semantics_error(ctx, m, *t);
      semantics_error(ctx, m);
   }

   static void
   semantics_error(BasicContext<>& ctx, const std::string& m, const FixityForm* x) {
      if (const Token* t = anchor(x))
         return semantics_error(ctx, m, *t);
      semantics_error(ctx, m);
   }

   static void
   undeclared_error(BasicContext<>& ctx, const std::string& msg, const Ast& x) {
      auto decl_ctx = ctx.elaborator()->top_decl_context();
      throw UndeclaredError(decl_ctx, msg, *anchor(&x));
   }

   // -- Coercion diagnostics --
   namespace {
      struct CoercionError : SemanticsErrorAt {
         CoercionError(const DeclContext* x, const Token& l,
                       Elaboration e, const Type* t)
               : SemanticsErrorAt(x, "", l), expr(e), target(*t)
         { }
      private:
         void format_message(std::ostream&) const;
         Elaboration expr;
         const Type& target;
      };

      void
      CoercionError::format_message(std::ostream& os) const {
         os << "could not convert expression "
            << quote(show(expr.code()))
            << " of type " << quote(show(*expr.type()))
            << " to type " << target;
      }
   }

   static void
   coercion_error(BasicContext<>& ctx, Elaboration expr, const Type* target) {
      auto decl_ctx = ctx.elaborator()->top_decl_context();
      auto loc = ctx.elaborator()->current_location();
      throw CoercionError(decl_ctx, loc, expr, target);
   }

   // Attempt to coerce an expression `e' to type `t'.  Return an invalid
   // elaboration on failure.
   static Elaboration
   successful_coercion(BasicContext<>& ctx, Elaboration e, const Type* t) {
      try {
         return ctx.elaborator()->coerce(e, t);
      }
      catch (const CoercionError&) { }
      return { };
   }

   // Perform an explicit lvalue-to-rvalue conversion.
   static Elaboration
   rvalue(BasicContext<>& ctx, const Elaboration& expr) {
      if (auto t = is<ReferenceType>(expr.type()))
         return { t->referee(), ctx.elaborator()->build_read(expr) };
      return expr;
   }

   // --
   static const Type*
   default_type(BasicContext<>& ctx, const Token* x) {
      auto env = ctx.elaborator();
      switch (x->kind) {
      case token::literal_boolean_tok: return env->get_bool();
      case token::literal_character_tok: return env->get_char();
      case token::literal_integer_tok: return env->get_int();
      case token::literal_real_tok: return env->get_double();
      case token::literal_string_tok: return env->get_string();

      default:
         semantics_error(ctx, "unknown literal " + quote(x));
         return env->get_void();
      }
   }

   static const Literal*
   literal_name(BasicContext<>& ctx, const Token* t) {
      auto env = ctx.elaborator();
      return env->build_literal(env->intern(t), default_type(ctx, t));
   }
   
   static const Literal*
   make_name(BasicContext<>& ctx, const LiteralAst& x) {
      return literal_name(ctx, x.token());
   }

   static const Operator*
   make_name(BasicContext<>& ctx, const OperatorAst& x) {
      return ctx.elaborator()->build_operator(x.token());
   }

   static const Operator*
   make_name(BasicContext<>& ctx, const BracketAst& x) {
      return make_operator(ctx.elaborator(), lexeme(x.first()) + lexeme(x.second()));
   }

   static const Identifier*
   make_name(BasicContext<>& ctx, const IdentifierAst& x) {
      return ctx.elaborator()->build_identifier(x.token());
   }

   // -- default elaboration of a type expression.
   TypeElaboration
   type_elaboration(BasicContext<>& ctx, const Type* t) {
      return { ctx.elaborator()->get_typename(), t };
   }

   // -- Helper function for building reference types.
   static TypeElaboration
   make_reference_type(BasicContext<>& ctx, const Type* t) {
      return type_elaboration
         (ctx, ctx.elaborator()->make_reference_type(type_elaboration(ctx, t)));
   }

   // -- semantics fiber section
   static Fiber
   fiber_section(BasicContext<>& ctx, const Fiber& f, const Type* t) {
      if (t == nullptr)
         return f;
      Fiber s;
      for (auto& e : f)
         if (auto x = successful_coercion(ctx, e, t))
            s.push_back(x);
      return s;
   }

   // Return an elaboration designating a scope, if it is nomable.
   static Elaboration
   resolve_scope(BasicContext<>& ctx, ScopeRef sc) {
      const Expression* x = sc->origin();
      if (auto n = is<Namespace>(x))
         return { ctx.elaborator()->get_namespace(), n };
      return { };
   }

   // Return an expression designating an entity declared in a given scope.
   static Elaboration
   refer_by_name(BasicContext<>& ctx, const Declaration& d, Elaboration scope) {
      Elaboration e = d.value();
      if (is<Postulate>(e))     // Postulates stand for themselves
         return e;
      auto t = make_reference_type(ctx, e.type());
      if (is<Formal>(e))        // Formals are really symbolic references
         return { t, e.code() };
      auto lnk = ctx.elaborator()->build_name(d.name(), e.type());
      if (scope)
         return { t, ctx.elaborator()->build_component(scope, *lnk) };
      return { t, lnk };
   }

   // Compute the semantics fiber of a name `n' in a given scope `s'
   // denotated by the elaboration `scope'.
   static Fiber
   lookup_fiber(BasicContext<>& ctx, ScopeRef sc, const Name* n,
                Elaboration scope) {
      Fiber f;
      for (auto& d : sc->lookup(n))
         f.push_back(refer_by_name(ctx, d, scope));
      return f;
   }

   // Compute the lexical semantics fiber of a simple symbol.
   static Fiber
   lexical_fiber(BasicContext<>& ctx, const Name* n) {
      return select_fiber(ctx.scope_stack(), [&ctx, n](auto& s) {
            return lookup_fiber(ctx, s, n, resolve_scope(ctx, s)); });
   }

   // Like lexical_fiber, except that an empty fiber results in an error.
   template<typename X>
   static Fiber
   lexical_fiber_or_else(BasicContext<>& ctx, const X& x) {
      Fiber f = lexical_fiber(ctx, make_name(ctx, x));
      if (f.empty())
         undeclared_error(ctx, "there is no declaration for this name", x);
      return f;
   }

   // Return a simple name representation of an AST object, if it is one.
   static const Name*
   simple_name(BasicContext<>& ctx, const Ast* x) {
      struct V {
         BasicContext<>& ctx;
         const Name* result;

         void operator()(const LiteralAst& x) { result = make_name(ctx, x);}
         void operator()(const IdentifierAst& x) {
            result = make_name(ctx, x);
         }
         void operator()(const OperatorAst& x) {
            result = make_name(ctx, x);
         }
         void operator()(const BiSectionAst& x) {
            result = make_name(ctx, *x.operation());
         }
         void operator()(const Ast&) { }
      };
      ast_visitor<V> v { ctx, nullptr };
      x->accept(v);
      return v.result;
   }

   // Return the semantics fiber of a name in a specified scope, error
   // if the fiber is empty.
   static Fiber
   lookup_fiber(BasicContext<>& ctx, Namespace* ns, const Name* n,
                const Elaboration& scope) {
      Fiber f = lookup_fiber(ctx, ns->region(), n, scope);
      if (f.empty())
         semantics_error(ctx, "no declaration for " + quote(n->symbol())
                         + " in namespace " + quote(show(*ns)));
      return f;
   }

   static Elaboration
   select_field(BasicContext<>& ctx, const RecordType* rt, const Name* n,
                const Elaboration& obj) {
      for (auto& f : rt->components())
         if (f->tag() == n) {
            auto t = make_reference_type(ctx, f->type());
            return { t, ctx.elaborator()->build_dot(obj, { n, f->type() }) };
         }
      semantics_error(ctx, "expression of record type has no field named "
                      + quote(n->symbol()));
      return { };
   }

   static const Name*
   selector_name(BasicContext<>& ctx, const Ast* x) {
      auto name = simple_name(ctx, x);
      if (name == nullptr)
         semantics_error(ctx, "invalid selector", x);
      return name;
   }

   static Fiber
   semantics_fiber(BasicContext<>& ctx, const DotAst& x) {
      auto name = selector_name(ctx, x.member());
      Elaboration e = ctx.elaborator()->elaborate(x.scope());
      if (auto rt = is<RecordType>(e.type()))
         return Fiber{ rvalue(ctx, select_field(ctx, rt, name, e)) };
      Elaboration f = rvalue(ctx, e);
      if (auto rt = is<RecordType>(f.type()))
         return Fiber{ select_field(ctx, rt, name, f) };
      // FIXME: next cast indicates design bug.
      auto ns = const_cast<Namespace*>(is<Namespace>(evaluate(ctx, f).code()));
      if (ns == nullptr)
         semantics_error(ctx, "invalid use of object of type "
                         + quote(show(*e.type()))
                         + " as scope object");
      return lookup_fiber(ctx, ns, name, f);
   }

   // Return the lexical semantics fiber of an expression.
   static Fiber
   semantics_fiber(BasicContext<>& ctx, const Ast* x) {
      if (auto n = simple_name(ctx, x))
         return lexical_fiber(ctx, n);
      else if (auto y = is<DotAst>(x))
         return semantics_fiber(ctx, *y);
      return Fiber{ ctx.elaborator()->elaborate(x) };
   }

   // -- ScopeManager --

   static void
   print_scope_stack_enter(ScopeStack& st, std::ostream& os) {
      auto n = st.size();
      os << "=============== scope >" << n << "< ===============" << std::endl;
      for (auto& d : st.top()) {
         os << d.name()->symbol() << ": " << *d.value().type() << std::endl;
      }
   }

   static void
   print_scope_stack_leave(ScopeStack& st, std::ostream& os) {
      auto n = st.size();
      os << "=============== scope <" << n << "> ===============" << std::endl;
      for (auto& d : st.top()) {
         os << d.name()->symbol() << ": " << *d.value().type() << std::endl;
      }
   }
   
   ScopeManager::ScopeManager(Elaborator* c, Scope& s)
         : ctx(c) {
      if (context()->enabled(Debug::Scope))
         print_scope_stack_enter(*context(), ctx->io()->debug());
      context()->push(&s);
   }
   
   ScopeManager::~ScopeManager() {
      if (context()->enabled(Debug::Scope))
         print_scope_stack_leave(*context(), ctx->io()->debug());
      context()->pop();
   }

   // -- Function call debug capability --
   namespace {
      struct CallChain {
         explicit CallChain(const char* s) : fun(s) {
            ++nesting;
            std::cerr << '[' << nesting << '>'
                      << fun << ']' << std::endl;
         }
         ~CallChain() {
            std::cerr << '[' << nesting << '<'
                      << fun << ']' << std::endl;
            --nesting;
         }

      private:
         const char* fun;
         static int nesting;
      };

      int CallChain::nesting = 0;

      struct LocalScopeManager : Scope, ScopeManager {
         explicit LocalScopeManager(Elaborator* c)
               : ScopeManager(c, *this) { }

         const Expression* origin() const { return nullptr; }
      };

      struct ParameterScopeManager : LocalScopeManager {
         explicit ParameterScopeManager(Elaborator* c)
               : LocalScopeManager(c) {
            context()->increase_parameter_depth();
         }
         ~ParameterScopeManager() {
            context()->decrease_parameter_depth();
         }
      };
   }

   // Type elaboration may produce colateral artifacts.
   // Indicate what should be filtered out.
   enum class filter {
      nothing, scope
   };

   static TypeElaboration
   elaborate_type(BasicContext<>&, const Ast*, filter = filter::scope);

   // -- FormContext --
   namespace {
      struct FormContext : DeclContext {
         template<typename T>
         FormContext(BasicContext<>& ctx, const T& form, const Ast* t)
               : DeclContext(ctx, get_name(ctx, form)),
                 parms_scope(ctx.elaborator()),
                 parms(elaborate_parameters(ctx, form)),
                 ty(ctx.elaborator()->make_arrow_type(elaborate_type(ctx, t), parms))
         { }
         const ArrowType* type() const override { return ty; }
         const Formals& formals() const { return parms; }
         Declaration* commit() const { return declare(scope(), { name(), ty }); }
      private:
         ParameterScopeManager parms_scope;
         Formals parms;
         const ArrowType* ty;
         FormContext(FormContext&&) = delete;
      };
   }

   // ----------------
   // -- Elaborator --
   // ----------------
   static const Identifier*
   make_identifier(BasicContext<>& ctx, const IdentifierAst* x) {
      if (x == nullptr)
         return nullptr;
      return make_name(ctx, *x);
   }
   
   static TypeElaboration do_elaborate_arrow_type(BasicContext<>&, const LambdaAst&);
   static TypeElaboration
   do_elaborate_quantified_type(BasicContext<>&, const QuantifiedAst&);

   static TypeElaboration
   elaborate_arrow_type(BasicContext<>& ctx, const LambdaAst& x, filter what) {
      if (what == filter::scope) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         return do_elaborate_arrow_type(ctx, x);
      }
      return do_elaborate_arrow_type(ctx, x);
   }

   static TypeElaboration
   elaborate_quantified_type(BasicContext<>& ctx, const QuantifiedAst& x,
                             filter what) {
      if (what == filter::scope) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         return do_elaborate_quantified_type(ctx, x);
      }
      return do_elaborate_quantified_type(ctx, x);
   }

   // Return the type of a predicate with input types `src'.
   static const ArrowType*
   make_predicate_type(BasicContext<>& ctx, const InputTypes& src) {
      auto env = ctx.elaborator();
      return env->make_arrow_type(env->get_bool(), src);
   }

   // We are elaborating a restricted type; find a normal form for the
   // restricting predicate.
   static Elaboration
   elaborate_restriction(BasicContext<>& ctx, const Ast* x, TypeElaboration t) {
      return ctx.elaborator()->elaborate(x, make_predicate_type(ctx, { t }));
   }

   static TypeElaboration
   elaborate_restricted_type(BasicContext<>& ctx, const FilterAst& ast,
                             filter what ) {
      auto t = elaborate_type(ctx, ast.expr(), what);
      Elaboration p = elaborate_restriction(ctx, ast.condition(), t);
      return { t.type(), ctx.elaborator()->make_restricted_type(t, p) };
   }

   static TypeElaboration
   elaborate_maybe_tag_type(BasicContext<>& ctx, const ParameterAst& x) {
      auto env = ctx.elaborator();
      auto t = elaborate_type(ctx, x.type());
      if (auto n = make_identifier(ctx, x.name()))
         return { env->get_typename(), env->make_tag_type(n, t) };
      return t;
   }
   
   // Elaborate `ast' as a type expression.  Raise an exception otherwise.
   static TypeElaboration
   elaborate_type(BasicContext<>& ctx, const Ast* ast, filter what) {
      struct V {
         BasicContext<>& ctx;
         filter what;
         TypeElaboration result;

         void operator()(const LambdaAst& x) {
            result = elaborate_arrow_type(ctx, x, what);
         }
         void operator()(const FilterAst& x) {
            result = elaborate_restricted_type(ctx, x, what);
         }
         void operator()(const QuantifiedAst& x) {
            result = elaborate_quantified_type(ctx, x, what);
         }
         void operator()(const ParameterAst& x) {
            result = elaborate_maybe_tag_type(ctx, x);
         }
         void operator()(const Ast& x) {
            auto env = ctx.elaborator();
            result = env->coerce_to_type(env->elaborate(&x));
         }
      };
      ast_visitor<V> v { ctx, what, TypeElaboration() };
      ast->accept(v);
      return v.result;
   }

   // Return true if `t' has a type value (as opposed to data values.)
   // This is useful to distinguish values that are types
   // (e.g. `int', `bool') from values that are ordinary data
   // (e.g., `2', `false'.)
   static bool
   has_type_values(const Type* t, BasicContext<>& ctx) {
      if (t == ctx.elaborator()->get_typename())
         return true;
      else if (auto rt = is<RestrictedType>(t))
         return has_type_values(rt->type(), ctx);
      return false;
   }

   // Return true if the elaboration `expr' designates a type.
   static bool
   has_type_denotation(const Elaboration& expr, BasicContext<>& ctx) {
      return is<Type>(expr.code()) or has_type_values(expr.type(), ctx);
   }

//    static const Evidence&
//    is_callable(const Constraint* req, BasicContext<>& ctx) {
//       Evidence evidence;
//       if (not is<ArrowType>(req->argument(0).code()))
//          evidence.set_bad();
//       return *ctx->register_evidence(req, evidence);
//    }

   // -- Construct the substitution mapping concept `c''s parameters to
   // -- to the argument list `args'.
   static Substitution
   substitution(const Constraint* req) {
      Substitution subst;
      auto lamb = req->abstraction();
      const int n = lamb->arity();
      for (int i = 0; i < n; ++i)
         subst[lamb->parameter(i)] = req->argument(i);
      return subst;
   }

//    // Return true if the constraint `c1' entail constraint `c2'.
//    static bool
//    entail(const Constraint* c1, const Constraint* c2,  BasicContext<>& ctx) {
//       // 0. No real task to carry out if both trivially agree.
//       if (c1 == c2 or c2 == nullptr)
//          return true;
//       if (c1 == nullptr)
//          return false;
//       // 1. Set up a subtitution for instantiating the concept
//       // associated with `c1'.
//       Substitution subst = substitution(c1);
//       const Concept* c = c1->original_concept();

//       // 2. Traverse instantiated concept and recursive look for entailment.
//       for (auto& f : c->formulae()) {
//          Elaboration inst = subst_expr(ctx.elaborator(), f, subst);
//          if (inst.code() == c2
//              or entail(is<Constraint>(inst.code()), c2, ctx))
//             return true;
//       }
//       // 3. At this point, we give up, as there is no entailment when
//       // contidering nominal constraints.
//       // However, be sure to handle the built-in concept `Function'.
//       return c2->constructor() == ctx->get_Function()
//          and is_callable(c1, ctx);
//    }

   // Return true if `fun' denotes an equality function.
   static bool
   equality_function(FunctionElaboration f, BasicContext<>& ctx) {
      auto ftype = f.type();
      const Function* fun = is<Function>(f);
      return fun != nullptr
         and fun->name() == make_operator(ctx.elaborator(), "==")
         and ftype->target() == ctx.elaborator()->get_bool()
         and ftype->argument(0) == ftype->argument(1);
   }

   // Return a non-null BinaryExpression pointer if `expr' denotes
   // an equality expression.
   static const BinaryExpression*
   has_eq_denotation(Elaboration expr, BasicContext<>& ctx) {
      auto x = is<BinaryExpression>(expr.code());
      return x == nullptr or not equality_function(x->function(), ctx)
         ? nullptr : x;
   }

   using EqClassHandle = EquivalenceQuotient::iterator;

   static void
   load_assumption(BasicContext<>& ctx, Elaboration expr) {
      ctx.elaborator()->assume(expr);
      if (const BinaryExpression* x = has_eq_denotation(expr, ctx))
         ctx.elaborator()->eq_classes().merge_classes(x->lhs(), x->rhs());
   }
   
   struct equal_to {
      const BasicView* const view;
      explicit equal_to(const BasicView* v) : view(v) { }
      bool operator()(const BasicView* v) const {
         return view->constructor() == v->constructor()
            and view->arguments() == v->arguments();
      }
   };

   template<typename T>
   static void
   augment_set(std::set<T>& dst, const std::set<T>& src) {
      dst.insert(src.begin(), src.end());
   }

   // Return true if the expression `lhs' implies the expression `rhs'
   static bool
   surely_subsumes(Elaboration lhs, Elaboration rhs, BasicContext<>& ctx) {
      struct SubsumeVisitor : Expression::Visitor {
         Elaboration goal;
         BasicContext<>& ctx;
         bool result;
         SubsumeVisitor(Elaboration e, BasicContext<>& c)
               : goal(e), ctx(c), result(false) { }

         void visit(const Expression& x) {
            result = pattern_match(goal, &x);
         }
         
         void visit(const Bool& x) { result = bool(x); }
         
         void visit(const Formula& x) {
            // FIXME: we don't handle existential quantifier yet.
            if (x.quantifier() == Quantifier::exists)
               return;
            result = surely_subsumes(x.body(), goal, ctx);
         }

         void visit(const BinaryLogical& x) {
            switch (x.operation()) {
            case logical::conjunction:
               result = surely_subsumes(x.lhs(), goal, ctx)
                  and surely_subsumes(x.rhs(), goal, ctx);
               break;
            case logical::disjunction:
               result = surely_subsumes(x.lhs(), goal, ctx)
                  or surely_subsumes(x.rhs(), goal, ctx);
               break;
            case logical::implication: {
               Substitution subst = pattern_match(goal, x.rhs().code());
               if (subst.failed())
                  return;
               Elaboration cond = subst_expr(ctx, x.lhs(), subst);
               cond = evaluate(ctx, cond);
               // FIXME: for this code to work properly, we probably
               // need some form of reflection.
               // if (const Bool* b = is<Bool>(cond))
               //  result = bool(*b);
               if (is_closed(cond)) {
                  result = bool(ctx.evaluator()->eval(cond).value());
                  if (result)
                     load_assumption(ctx, goal);
               }
            }
               break;
            case logical::equivalence:
               result = surely_subsumes(x.lhs(), x.rhs(), ctx)
                  and surely_subsumes(x.rhs(), x.lhs(), ctx);
               break;
            }
         }
      };

      SubsumeVisitor v(rhs, ctx);
      lhs.code()->accept(v);
      return v.result;
   }

   static bool
   can_prove(BasicContext<>& ctx, Elaboration expr,
             const AssumptionSet& props) {
      for (auto& p : props) 
         if (surely_subsumes(p, expr, ctx))
            return true;
      return false;
   }

   Elaboration
   eq_close_term_if_can(EquivalenceQuotient& c, Elaboration e) {
      EqClassHandle p = c.eq_class(e);
      if (is_closed(p->leader()))
         return p->leader();
      return e;
   }

   static void print_eq_class(EqClassHandle c, std::ostream& os) {
      os << '{';
      for (auto& p : *c)
         os << show(p.code()) << ", ";
      os << '}';
   }
   
   static void
   print_eq_classes(EquivalenceQuotient& classes, std::ostream& os) {
      os << "\n================ Equivalence Classes =================\n";
      os << '{';
      for (EqClassHandle p = classes.begin(); p != classes.end(); ++p) {
         print_eq_class(p, os);
         os << "\n\t";
      }
      os << '}';
      os << "\n======================================================\n\n";
   }

   // ---------------------
   // -- FullApplication --
   // ---------------------
   // Representation of data generated by full application.
   namespace {
      struct FullApplication
         : structure::binary<FunctionElaboration, Arguments> {
         FullApplication()
               : structure::binary<FunctionElaboration, Arguments>({ }, { })
         { }
         FullApplication(FunctionElaboration f, const Arguments& args)
               : structure::binary<FunctionElaboration, Arguments>(f, args)
         { }
         FunctionElaboration function() const { return first(); }
         const Arguments& arguments() const { return second(); }
         // courtesy conversion for use in conditionals.
         explicit operator bool() const { return function() ? true : false; }
      };
   }

   using call_builder = const Call* (Elaborator::*)
            (FunctionElaboration, const Arguments&);

   // We just finished elaboration of a call to `fun' with arguments `args'.
   // Build the resulting expression with `builder'.  This generality is
   // provided so that we can specialize call to user-defined operations
   // and yet retain the abstract form.
   template<typename Builder, typename... Args>
   Elaboration
   call_with(BasicContext<>& ctx, Builder builder, FunctionElaboration fun,
             const Args&... args) {
      auto elab = ctx.elaborator();
      return { fun.type()->target(), (elab->*builder)(fun, args...) };
   }

   template<typename T>
   Elaboration
   call_with(BasicContext<>& ctx, unary_builder<T> builder, FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args[0]);
   }

   template<typename T>
   Elaboration
   call_with(BasicContext<>& ctx, binary_builder<T> builder, FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args[0], args[1]);
   }

   static Elaboration
   call_with(BasicContext<>& ctx, call_builder builder , FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args);
   }

   // Return the elaboration for a function call.
   static Elaboration
   call(BasicContext<>& ctx, FunctionElaboration fun, const Arguments& args) {
      return call_with(ctx, &Elaborator::build_call, fun, args);
   }

   static bool
   valid_eqclass(EqClassHandle h, BasicContext<>& ctx) {
      return h != ctx.elaborator()->eq_classes().end();
   }

   // Return true is `x' is equivalent to a member of `set'.
   static bool
   in_eqclass(const Type* x, EqClassHandle set, BasicContext<>& ctx) {
      if (not valid_eqclass(set, ctx))
         return false;
      for (auto& e : *set)
         if (x == e.code())
            return true;
      return false;
   }

   // Build an elaboration form for equality test between the types
   // respectively designated by `lhs' and `rhs'.
   static Elaboration
   equality_expr(Elaboration lhs, Elaboration rhs, BasicContext<>& ctx) {
      FunctionElaboration eq_fun = ctx.elaborator()->get_type_eq();
      Arguments args { lhs, rhs };
      return call_with(ctx, &Elaborator::build_eqeq, { eq_fun, args} );
   }

   // Return true if the type expressions `lhs' and `rhs' are
   // equivalent in `context'.  This equivalence takes into account
   // any assumption, in form of equation of logical formula, in effect.
   static bool
   are_equivalent(const Type* s, const Type* t, BasicContext<>& ctx) {
      auto env = ctx.elaborator();
      if (env->enabled(Debug::Eq))
         print_eq_classes(env->eq_classes(), env->io()->debug());
      // Don't work too hard.
      if (s == t)
         return true;
      auto lhs = type_elaboration(ctx, s);
      auto rhs = type_elaboration(ctx, t);
      if (is_closed(lhs) and is_closed(rhs))
         return false;

      if (in_eqclass(s, env->eq_class(rhs), ctx)
          or in_eqclass(t, env->eq_class(lhs), ctx))
         return true;

      // Attempt the actual proof.
      return can_prove(ctx, equality_expr(lhs, rhs, ctx), env->assumptions());
   }

//    // Discharge proof obligations to be satisfied by `expr' in order
//    // to meet the requirement `req'.  Returns the evidence accumulated
//    // so far.
//    static const Evidence&
//    discharge_constraint(BasicContext<>& ctx, const Constraint* req) {
//       if (const Evidence* p = ctx->find_evidence(req))
//          return *p;
//       if (req->constructor() == ctx->get_Function())
//          return is_callable(req, ctx);

//       Evidence evidence;
//       const Concept* c = req->original_concept();
//       Substitution subst = substitution(req);
//       for (std::size_t i = 0; i < c->formulae().size() and evidence; ++i) {
//          Elaboration e = subst_expr(ctx.elaborator(), c->formula(i), subst);
//          if (auto f = is<Signature>(e.code())) {
//             auto lnk = f->link_name();
//             if (auto d = ctx->select_if_can(lnk.name(), lnk.type())) {
//                evidence.send_to(f, is<Function>(d->value()));
//             }
//             else
//                evidence.set_bad();
//          }
//          else if (auto r = is<Constraint>(e.code())) {
//             const Evidence& sub =  discharge_constraint(ctx, r);
//             evidence.send_to(r, &sub);
//             if (not sub.good())
//                evidence.set_bad();
//          }
//          // FIXME: check other types of formula as well.
//       }
//       return *ctx->register_evidence(req, evidence);
//    }

   // Return true if the argument `value' sastifies the requirements
   // of `type'.
   static bool
   satisfies(BasicContext<>& ctx, const Elaboration& value, const Type* type) {
      // 1.  If exact match, then we're done.
      if (are_equivalent(type, value.type(), ctx))
         return true;
      // 2.  If it smells like a type then it is a type.
      else if (type == ctx.elaborator()->get_typename())
         return has_type_denotation(value, ctx);
      // 3.  FIXME If we have a concept, we must check its predicates.
      return false;
   }

   // Return true if the type `target' is a read-only version of `source'
   static bool
   is_readonly_variant(const Type* target, const Type* source,
                       BasicContext<>& ctx) {
      if (auto t = is<ReadonlyType>(target))
         return are_equivalent(t->type(), source, ctx);
      auto t = is<ReferenceType>(target);
      auto s = is<ReferenceType>(source);
      if (t != nullptr and s != nullptr)
         return is_readonly_variant(t->referee(), s->referee(), ctx);
      return false;
   }

   // Return an elaboration for a function.
   static FunctionElaboration
   is_functoid(BasicContext<>& ctx, Elaboration e) {
      if (auto t = is<ArrowType>(e.type()))
         return { t, e.code() };
      else if (is<ReferenceType>(e.type()))
         return is_functoid(ctx, rvalue(ctx, e));
      return { };
   }

   // Return the widened elaboration if `e' denotes the restriction
   // of a value of type `t'.
   static Elaboration
   can_widen_to(Elaboration e, const Type* t, BasicContext<>& ctx) {
      if (auto s = is<RestrictedType>(e.type()))
         if (are_equivalent(s->type(), t, ctx))
            return { t, e.code() };
      return { };
   }

   static Elaboration
   symbolic_expression(BasicContext<>& ctx, Elaboration e, const Type* t) {
      auto env = ctx.elaborator();
      if (t == env->get_prop()) {
         if (is<Value>(e))
            return { t, e.code() };
         return { t, env->build_quote(e) };
      }
      return { };
   }

   static bool
   has_logical_values(const Type* t, BasicContext<>& ctx) {
      if (t == ctx.elaborator()->get_bool())
         return true;
      else if (auto qt = is<QuantifiedType>(t))
         return has_logical_values(qt->abstract_instance(), ctx);
      return false;
   }

   Elaboration
   Elaborator::coerce(Elaboration value, const Type* target) {
      BasicContext<> ctx { this, evl };
      if (has_type_denotation(value, ctx))
         value = coerce_to_type(value).code();

      // Just return the value if we are in inference mode, or
      // when we have an exact match.
      const Type* source = value.type();
      if (target == nullptr or are_equivalent(source, target, ctx))
         return value;
      else if (auto x = symbolic_expression(ctx, value, target))
         return x;
      // If the expression is wanted for its side effect, let it be so.
      if (are_equivalent(target, get_void(), ctx))
         return { target, value.code() };

      // Convert to rvalue conversion if the context requires it.
      if (is<ReferenceType>(source) and not is<ReferenceType>(target))
         return coerce(rvalue(ctx, value), target);

      // Implicit const-qualification is free of charge.
      if (is_readonly_variant(target, source, ctx))
         return { target, value.code() };

      // Losing a rvalue top-level const-qualification is OK.
      if (auto s = is<ReadonlyType>(source)) {
         if (are_equivalent(s->type(), target, ctx))
            return { target, value.code() };
      }

      // Widening is free of charge.
      if (auto expr = can_widen_to(value, target, ctx))
         return expr;

      // Binding an rvalue to a const-reference requires a temporary.
      if (auto t = is<ReferenceType>(target)) {
         if (not is<ReferenceType>(source)
             and is_readonly_variant(t->referee(), source, ctx)) {
            // FIXME: run destructor for the temporary.
            return { target, build_bind(fresh_name(), t->referee(), value) };
         }
      }

      if (target == get_axiom() and has_logical_values(value.type(), ctx))
         return value;
      else if (not satisfies(ctx, value, target))
         coercion_error(ctx, value, target);
      return { target, value.code() };
   }


   const Function*
   select_equality_operator(BasicContext<>& ctx, const Type* t) {
      auto argtype = type_elaboration(ctx, t);
      auto ftype = get_binary_predicate_type(ctx.elaborator(), argtype);
      auto sym = make_operator(ctx.elaborator(), "==");
      auto type_eq =
         [=](const Declaration& x) { return x.value().type() == ftype; };
      auto decl = lexical_lookup(*ctx.elaborator(), sym).select_if(type_eq);
      if (decl == nullptr)
         semantics_error(ctx, "Could not find equality operator of type "
                         + quote(show(*ftype)));
      return to_function(decl->value().code());
   }

   static void assume_property(BasicContext<>&, Elaboration);

   // -- Requirements assumptions
   // Make the requirements in `req' available as assumptions in `context'
   static void
   load_requirements(BasicContext<>& ctx, const Constraint* req) {
      // 1. Add the constraint itself first.
      load_assumption(ctx, { ctx.elaborator()->get_bool(), req });
      // 2. Build a substitution to instantiate the body.
      const Concept* c = req->original_concept();
      Substitution subst = substitution(req);
      // 3. Traverse the body and insert assumptions.
      for (auto& f : c->formulae())
         assume_property(ctx, subst_expr(ctx, f, subst));
   }

   static AstSequence
   get_sequence(const Ast* x) {
      if (auto y = is<SequenceAst>(x))
         return y->sequence();
      return { x };
   }
   
   // Determine whether the syntactic object `x' is enclosed
   // in parenthesis.
   static bool
   is_parenthesized(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_paren_tok;
   }

   static bool
   is_braced(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_brace_tok;
   }

   static AstSequence
   get_arguments(const ApplyAst& x) {
      auto y = x.arguments();
      if (y->expr() == nullptr)
         return { };
      else if (is_parenthesized(*y))
         return get_sequence(y->expr());
      return { y };
   }

   // Attempt to elaborate an expression in a given type context.
   // Return the elaboration in case of success.
   static Elaboration
   try_initializer(BasicContext<>& ctx, const Ast* x, const Type* t) {
      try {
         return ctx.elaborator()->elaborate(x, t);
      }
      catch (const SemanticsErrorAt&) { }
      return { };
   }

   // Subroutine of is_viable_call.
   // Return true if 'fun' is a viable candidate function of the
   // list of arguments  `args'.
   static FullApplication
   acceptable_arguments(BasicContext<>& ctx, FunctionElaboration fun,
                        const AstSequence& arglist) {
      const ArrowType* ftype = fun.type();
      const std::size_t nargs = arglist.size();
      if (nargs != ftype->arity())
         return { };
      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         if (auto arg = try_initializer(ctx, arglist[i], ftype->argument(i)))
            args[i] = arg;
         else
            return { };
      }
      return { fun, args };
   }

   static void
   print_matching_problem(BasicContext<>& ctx, const Expression* expr,
                          const Expression* pat) {
      auto env = ctx.elaborator();
      if (not env->enabled(Debug::Matching))
         return;
      auto io = env->io();
      io->debug() << "============= Matching Problem =============" << std::endl;
      prefix_form(io->debug() << "given: ", expr);
      io->debug() << std::endl;
      prefix_form(io->debug() << "pattern: ", pat);
      io->debug() << std::endl;
      io->debug() << "============================================" << std::endl;
   }

   static void
   print_no_matching_solution(BasicContext<>& ctx) {
      auto io = ctx.elaborator()->io();
      io->debug() << "============= No Solution =============" << std::endl;
   }

   static void
   print_substitution(BasicContext<>& ctx, const Substitution& subst) {
      auto io = ctx.elaborator()->io();
      io->debug() << "============= Substitution =============" << std::endl;
      for (auto& e : subst) {
         io->debug() << '\t' << *e.first
                     << " ~> "
                     << show_expr(e.second.code())
                     << '\n';
      }
      io->debug() << "========================================" << std::endl;
   }

   namespace {
      struct InstantiationData {
         Substitution subst;
         Elaboration pattern;

         explicit operator bool() const { return not subst.failed(); }
      };
   }

   namespace {
      struct UniversalArrow {
         const Formals formals;
         const ArrowType* const arrow;
         const Expression* qexpr;
         explicit operator bool() const { return arrow != nullptr; }
      };
   }

   static void
   print_instantiation_request(BasicContext<>& ctx, const UniversalArrow& ua) {
      auto io = ctx.elaborator()->io();
      io->debug() << "========== instantiate_arrow ==========\n";
      io->debug() << "\ttype: " << show_expr(ua.arrow) << '\n';
      io->debug() << "\tqexpr: " << show_expr(ua.qexpr) << '\n';
      io->debug() << "=======================================\n";
   }

   static void
   check_all_formals_have_values(BasicContext<>& ctx, const UniversalArrow& ua,
                                 const UnificationContext& uc) {
      for (auto p : ua.formals)
         if (not uc.subst.has(p))
            semantics_error(ctx, "no substitution value for "
                            + quote(show(p->name())));
   }

   static FunctionElaboration
   instantiate_arrow(BasicContext<>& ctx, const UniversalArrow& ua,
                     const Substitution& subst) {
      auto env = ctx.elaborator();
      if (env->enabled(Debug::Instantiation))
         print_instantiation_request(ctx, ua);
      if (auto lam = is<Lambda>(ua.qexpr)) {
         FunctionElaboration fun { ua.arrow, lam->body().code() };
         return substitute(ctx, fun, subst);
      }
      return substitute(ctx, { ua.arrow, ua.qexpr }, subst);
   }

   static FullApplication
   instantiate_if_viable_call(BasicContext<>& ctx, const UniversalArrow& ua,
                              const AstSequence& argsyn) {
      const auto nargs = argsyn.size();
      if (ua.arrow->arity() != nargs)
         return { };

      Arguments args(nargs);
      UnificationContext uctx { ua.formals, { } };
      auto env = ctx.elaborator();
      for (std::size_t i = 0; i < nargs; ++i) {
         args[i] = env->elaborate(argsyn[i]);
         auto t = ua.arrow->argument(i);
         if (not is<ReferenceType>(t))
            args[i] = rvalue(ctx, args[i]);
         else if (auto rt = is<ReadonlyType>(t))
            t = rt->type();
         print_matching_problem(ctx, args[i].type(), t);
         if (not uctx.match_type({ env->get_typename(), args[i].type() }, t)) {
            if (env->enabled(Debug::Matching))
               print_no_matching_solution(ctx);
            return { };
         }
         print_substitution(ctx, uctx.subst);
      }
      check_all_formals_have_values(ctx, ua, uctx);

      FunctionElaboration fun = instantiate_arrow(ctx, ua, uctx.subst);
      for (std::size_t i = 0; i < nargs; ++i)
         args[i] = env->coerce(args[i], fun.type()->argument(i));
      return { fun, args };
   }

   static UniversalArrow
   universal_arrow(Elaboration e) {
      if (auto qt = is<QuantifiedType>(e.type())) {
         if (auto ftype = is<ArrowType>(qt->abstract_instance()))
            return { qt->formals(), ftype, e.code() };
      }
      return { { }, nullptr, nullptr };  // FIXME: should be { }, silly GCC.
   }

   static FullApplication
   is_viable_call(BasicContext<>& ctx, Elaboration e, const AstSequence& args) {
      if (auto f = is_functoid(ctx, e))
         return acceptable_arguments(ctx, f, args);
      else if (auto ua = universal_arrow(e))
         return instantiate_if_viable_call(ctx, ua, args);
      return { };
   }

   // Used to to report overload resolution diagnostics.
   static void
   no_match(const Fiber& candidates, const Type* target, BasicContext<>& ctx) {
      std::ostringstream os;
      if (candidates.size() > 1) {
         os << "no suitable match amoung the following candidates";
         if (target != nullptr)
            os << " with target type " << quote(show(*target));
         os << ':';
      }
      else
         os << "invalid call to:";
      os << '\n';
      for (auto& x : candidates)
         format_as_decl(os << "   ", x) << std::endl;
      
      semantics_error(ctx, os.str());
   }

   static void
   print_candidates(BasicContext<>& ctx, const Fiber& funs) {
      auto env = ctx.elaborator();
      if (not env->enabled(Debug::Overload))
         return;
      auto io = env->io();
      io->debug() << "========= Overload candidates ==========" << std::endl;
      for (auto& f : funs)
         format_as_decl(io->debug(), f) << std::endl;
      io->debug() << "========================================" << std::endl;
   }

   // Given a sequence of `arguments' and an overload `funs', select
   // the first function whose source type is satisfied by the arguments.
   template<typename Builder>
   static Elaboration
   resolve_overload(BasicContext<>& ctx, Builder builder, const Ast* op,
                    const AstSequence& args, const Type* target) {
      Fiber funs = semantics_fiber(ctx, op);
      // FIXME: do ADL before giving up.
      if (funs.empty())
         semantics_error(ctx, "no such operation in scope", op);
      print_candidates(ctx, funs);
      using CallInfo = std::pair<FunctionElaboration, Elaboration>;
      vector<CallInfo> data;
      for (auto& f : funs)
         if (auto x = is_viable_call(ctx, rvalue(ctx, f), args)) {
            auto e = call_with(ctx, builder, x);
            if (auto y = successful_coercion(ctx, e, target))
               data.push_back({ x.function(), y });
         }
      if (data.empty())
         no_match(funs, target, ctx);
      else if (data.size() > 1)
         semantics_error(ctx, "ambiguous call");
      return data.front().second;
   }

   // An overloaded function is called with argument expressions `args'.
   // Elaborate the arguments, perform overload resolution, then return
   // an elaboration for the whole call.
   static Elaboration
   elaborate_call(BasicContext<>& ctx, const Ast* op, const AstSequence& args,
                  const Type* t) {
      return resolve_overload(ctx, &Elaborator::build_call, op, args, t);
   }

   static Elaboration
   elaborate_call(BasicContext<>& ctx, const ApplyAst& x, const Type* t) {
      return elaborate_call(ctx, x.operation(), get_arguments(x), t);
   }

   static Elaboration
   elaborate_juxtapose(BasicContext<>& ctx, const JuxtaposeAst& x, const Type* t) {
      return elaborate_call(ctx, x.operation(), { x.argument() }, t);
   }

   // Elaborate a prefix expression.
   template<typename T>
   static Elaboration
   elaborate_prefix(BasicContext<>& ctx, const UnaryAst& x,
                    unary_builder<T> builder, const Type* t) {
      return resolve_overload
         (ctx, builder, x.operation(), { x.argument() }, t);
   }

   static Elaboration
   elaborate_reference(BasicContext<>& ctx, const Ast* x) {
      auto type = elaborate_type(ctx, x);
      auto env = ctx.elaborator();
      return { env->get_typename(), env->make_reference_type(type) };
   }

   static Elaboration
   elaborate_readonly(BasicContext<>& ctx, const Ast* x) {
      auto type = elaborate_type(ctx, x);
      auto env = ctx.elaborator();
      return { env->get_typename(), env->make_readonly_type(type) };
   }

   static Elaboration
   elaborate_assumption(BasicContext<>& ctx, const Ast* property) {
      // FIXME: check that we are actually elaborating a previously
      //        defined axiom; not just any arbitrary expression.
      auto env = ctx.elaborator();
      return env->elaborate(property, env->get_bool());
   }

   static Elaboration
   elaborate_unary(BasicContext<>& ctx, const UnaryAst& x, const Type* t) {
      auto env = ctx.elaborator();
      Elaborator::LocationManager push_loc(env, *anchor(x.operation()));
      switch (x.operation()->token()->kind) {
      default:
         return elaborate_call(ctx, x.operation(), { x.argument() }, t);
         
      case token::minus_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_negate, t);
         
      case token::not_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_not, t);
         
      case token::tilde_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_complement, t);
         
      case token::ampersand_tok:
         return env->coerce(elaborate_reference(ctx, x.argument()), t);
         
      case token::const_tok:
         return env->coerce(elaborate_readonly(ctx, x.argument()), t);
         
      case token::assume_tok:
         return env->coerce(elaborate_assumption(ctx, x.argument()), t);
      }
   }

   // Evaluate an infix expression.
   template<typename T>
   static Elaboration
   elaborate_infix(BasicContext<>& ctx, const BinaryAst& x,
                   binary_builder<T> builder, const Type* t) {
      return resolve_overload
         (ctx, builder, x.operation(), { x.lhs(), x.rhs() }, t);
   }

   // Subroutine of Elaborator::coerce_to_type.
   // A constructor is being used as a type, check that can be so.
   static TypeElaboration
   check_ctor_as_type(BasicContext<>& ctx, const Constructor* ctor) {
      auto env = ctx.elaborator();
      auto ctype = is<ArrowType>(ctor->type());
      if (ctype == nullptr or ctype->arity() != 1
          or ctype->target() != env->get_concept())
         semantics_error
            (ctx, "only a unary concept constructor can be used as type");
      auto type = ctype->argument(0);
      return { type.type(), env->make_restricted_type(type, { ctype, ctor }) };
   }

   // -- We have an expression that semantically designates a type.
   // -- Return the typeful version representation reflecting that
   // -- knowledge.
   TypeElaboration
   Elaborator::coerce_to_type(Elaboration expr) {
      BasicContext<> ctx { this, evl };
      expr = evaluate(ctx, rvalue(ctx, expr));
      if (auto type = is<Type>(expr.code()))
         return { expr.type(), type };
      else if (auto ctor = is<Constructor>(expr))
         return check_ctor_as_type(ctx, ctor);
      else if (not has_type_values(expr.type(), ctx))
         semantics_error(ctx, quote(show_expr(expr.code()))
                         + " does not designate a type");
      return { expr.type(), ctx.elaborator()->make_type_expression(expr) };
   }

   // Ensure `expr' is an lvalue expression.
   static Elaboration
   lvalue(BasicContext<>& ctx, Elaboration expr) {
      if (not is<ReferenceType>(expr.type()))
         semantics_error(ctx, quote(show(expr.code()))
                         + " is not an lvalue");
      return expr;
   }
   
   // Elaborate an assignment statement.
   static Elaboration
   elaborate_assignment(BasicContext<>& ctx, const Ast* x, const Ast* y) {
      auto env = ctx.elaborator();
      Elaboration rhs = env->elaborate(y);
      //  1. Elaborate the left hand side to a memory location.
      Elaboration place = lvalue(ctx, env->elaborate(x));
      auto type = is<ReferenceType>(place.type());
      // 2. Convert the right hand side to the appropriate type.
      Elaboration value = env->coerce(rhs, type->referee());
      // FIXME: check that the type is regular. and for classes
      // FIXME: look up assignment operator.
      return { type, env->build_write(place, value) };
   }

//    static Substitution
//    deduce_pattern_formals(TypeElaboration x, const Type* pattern) {
//       auto t = is<ReferenceType>(x.code());
//       auto p = is<ReferenceType>(pattern);
//       if (t != nullptr and p == nullptr)
//          return deduce_pattern_formals(t->referee(), pattern);
//       else if (t == nullptr and p != nullptr)
//          return deduce_pattern_formals(x, p->referee());
//       else if (auto tt = is<ReadonlyType>(x))
//          return deduce_pattern_formals(tt->type(), pattern);
//       else if (auto pp = is<ReadonlyType>(pattern))
//          return deduce_pattern_formals(x, pp->type());
//       return pattern_match(x, pattern);
//    }

   static const Literal*
   get_name(BasicContext<>& ctx, const LiteralForm& x) {
      return literal_name(ctx, x.token());
   }

   static const Identifier*
   get_name(BasicContext<>& ctx, const AlphabethicForm& x) {
      return ctx.elaborator()->build_identifier(x.token());
   }

   static const Operator*
   get_name(BasicContext<>& ctx, const OperatorForm& x) {
      return ctx.elaborator()->build_operator(x.token());
   }

   static const Identifier*
   get_name(BasicContext<>& ctx, const CallForm& head) {
      return ctx.elaborator()->build_identifier(&head.function());
   }
   
   static const Operator*
   get_name(BasicContext<>& ctx, const InfixForm& head) {
      return ctx.elaborator()->build_operator(head.operation()->token());
   }

   static const Operator*
   get_name(BasicContext<>& ctx, const PrefixForm& head) {
      auto env = ctx.elaborator();
      return env->build_operator(env->intern(head.operation()->token()));
   }

   static const Operator*
   get_name(BasicContext<>& ctx, const SuffixForm& head) {
      auto env = ctx.elaborator();
      return env->build_operator(env->intern(head.operation()->token()));
   }

   static const Operator*
   get_name(BasicContext<>& ctx, const ClosedForm& head) {
      return make_name(ctx, *head.bracket());
   }

   static const Name*
   get_name(BasicContext<>& ctx, const FixityForm* f) {
      struct V : FixityForm::Visitor {
         BasicContext<>& ctx;
         const Name* result;
         V(BasicContext<>& c) : ctx(c), result() { }
         void visit(const LiteralForm& x) { result = get_name(ctx, x); }
         void visit(const AlphabethicForm& x) { result = get_name(ctx, x); }
         void visit(const OperatorForm& x) { result = get_name(ctx, x); }
         void visit(const PrefixForm& x) { result = get_name(ctx, x); }
         void visit(const SuffixForm& x) { result = get_name(ctx, x); }
         void visit(const InfixForm& x) { result = get_name(ctx, x); }
         void visit(const ClosedForm& x) { result = get_name(ctx, x); }
         void visit(const CallForm& x) { result = get_name(ctx, x); }
      };
      V v { ctx };
      f->accept(v);
      return v.result;
   }

   // Return true if `x' is appropriate at non-local scopes.
   static bool
   valid_statement_at_nonlocal_scope(const Ast* x) {
      struct V {
         bool result;
         void operator()(const Ast&) { }
         void operator()(const DefinitionAst&) { result = true; }
         void operator()(const SignatureAst&) { result = true; }
         void operator()(const RuleAst&) { result = true; }
         void operator()(const PostulateAst&) { result = true; }
         void operator()(const ProlongAst&) { result = true; }
         void operator()(const ExprStmtAst& x) {
            if (x.expression() == nullptr)
               error_at(x.semicolon(), "extraneous semicolon");
         }
      };
      ast_visitor<V> v{ false };
      x->accept(v);
      return v.result;
   }

   static void
   filter_nonlocal_statement(BasicContext<>& ctx, const Ast* x) {
      if (x != nullptr and not valid_statement_at_nonlocal_scope(x))
         semantics_error(ctx, "invalid statement at non-local scope", x);
   }

   static const Constructor*
   finish_user_ctor(BasicContext<>& ctx, const Name* n, TypeElaboration type,
                    const Ast* x) {
      auto t = is<ArrowType>(type);
      if (t == nullptr)
         semantics_error(ctx, "user-supplied constructors must have "
                         "function type", x);
      auto impl_type = make_predicate_type(ctx, t->source());
      auto env = ctx.elaborator();
      Elaboration impl = env->elaborate(x, impl_type);
      return env->build_constructor({ n, t } , impl);
   }

   // We are about to elaborate an entity (a structure, or a function)
   // the initialzier of which must be a compound statement.  There is
   // a singularity for "{ }" which fits both an empty block and
   // an empty brace-enclosed expression list.
   static const CompoundAst*
   get_block_or_else(BasicContext<>& ctx, const Ast* x) {
      if (x == nullptr)
         return nullptr;
      else if (auto y = is<EnclosureAst>(x)) {
         if (y->expr() == nullptr)
            return nullptr;
      }
      auto y = is<CompoundAst>(x);
      if (y == nullptr)
         semantics_error(ctx, "invalid initializer for scope object", x);
      return y;
   }

   static const TagType*
   elaborate_tag_type(BasicContext<>& ctx, const SignatureAst& x) {
      const Name* n = get_name(ctx, x.form());
      auto env = ctx.elaborator();
      if (not env->top()->lookup(n).empty())
         semantics_error(ctx, "duplicate declaration of this tag", x.form());
      const Type* t = elaborate_type(ctx, x.type());
      return env->make_tag_type(n, { env->get_typename(), t});
   }

   static Elaboration
   elaborate_record(BasicContext<>& ctx, const DatatypeAst& x) {
      Sequence<TagType> fields;
      auto env = ctx.elaborator();
      LocalScopeManager new_scope { ctx.elaborator() };
      for (auto y : x.members()) {
         auto f = elaborate_tag_type(ctx, *y);
         auto rt = make_reference_type(ctx, f->type());
         new_scope.bind(f->tag(), { rt, f });
         fields.push_back(f);
      }
      return { env->get_typename(), env->make_record_type(fields) };
   }

   // Create a fresh namespace scope object to hold the definition
   // of the namespace being defined.
   static Namespace*
   new_namespace(DeclContext& ctx) {
      auto env = ctx.elaborator();
      return env->build_namespace(ctx.name(), env->top().base());
   }

   static void
   elaborate_members(BasicContext<>& ctx, const Ast* x, ToplevelScope* scope) {
      ScopeManager new_scope { ctx.elaborator(), *scope };
      const CompoundAst* block = get_block_or_else(ctx, x);
      const std::size_t nmembers = length(block);
      for (std::size_t i = 0; i < nmembers; ++i) {
         const Ast* y = block->at(i);
         filter_nonlocal_statement(ctx, y);
         scope->add_stmt(ctx.elaborator()->elaborate(y));
      }
   }
   
   static Elaboration
   elaborate_namespace(DeclContext& ctx, const Ast* x) {
      Namespace* value = new_namespace(ctx);
      elaborate_members(ctx, x, value);
      return { ctx.elaborator()->get_namespace(), value };
   }

   static const Signature*
   elaborate_signature(DeclContext& ctx, const PostulateAst* x) {
      auto n = get_name(ctx, x->form());
      auto t = elaborate_type(ctx, x->type());
      auto env = ctx.elaborator();
      auto sig = env->build_signature(n, t);
      declare(env->top(), { n, t }, sig);
      return sig;
   }

   static void
   elaborate_requirement(DeclContext& ctx, Concept* c, const Ast* x) {
      if (auto y = is<PostulateAst>(x))
         c->postulate(elaborate_signature(ctx, y));
      else
         c->require(ctx.elaborator()->elaborate(x));
   }

   static void
   check_concept_ctor_definition(DeclContext& ctx) {
      auto type = is<ArrowType>(ctx.type());
      if (type == nullptr or type->target() != ctx.elaborator()->get_concept())
         semantics_error(ctx, "invalid concept constructor");
   }

   static Elaboration
   elaborate_concept(DeclContext& ctx, const Ast* x) {
      // FIXME: We should be checking this upstream.
      check_concept_ctor_definition(ctx);
      
      LocalScopeManager new_scope { ctx.elaborator() };
      auto env = ctx.elaborator();
      auto value = env->build_concept();
      if (auto stmts = is<CompoundAst>(x))
         for (auto z : *stmts)
            elaborate_requirement(ctx, value, z);
      else
         elaborate_requirement(ctx, value, x);
      return { env->get_concept(), value };
   }

   static Elaboration
   make_ctor_tag(BasicContext<>& ctx, int k) {
      auto env = ctx.elaborator();
      return { env->get_int(), env->build_int(k, env->get_int()) };
   }

   static const Constructor*
   elaborate_associated_ctor(BasicContext<>& ctx, const SignatureAst& x, int k) {
      auto n = get_name(ctx, x.form());
      auto t = elaborate_type(ctx, x.type());
      if (const Ast* i = x.implementation())
         return finish_user_ctor(ctx, n, t, i);
      return ctx.elaborator()->build_constructor({ n, t }, make_ctor_tag(ctx, k));
   }

   static const DatatypeAst*
   is_inductive_definition(const Ast* x) {
      auto y = is<DatatypeAst>(x);
      if (y != nullptr and y->sort().kind == token::inductive_tok)
         return y;
      return nullptr;
   }

   static Elaboration
   elaborate_inductive_body(DeclContext& ctx, const DatatypeAst* x) {
      const std::size_t n = x->members().size();
      Sequence<Constructor> ctors(n);
      for (std::size_t i = 0; i < n; ++i) {
         auto y = x->members()[i];
         auto ctor = elaborate_associated_ctor(ctx, *y, i);
         // FIXME: Check that we have a positive occurance.
         ctors[i] = ctor;
      }
      for (auto c : ctors)
         ctx.scope()->define(c->name(), c->type(), c);
      auto env = ctx.elaborator();
      return { env->get_typename(), env->make_variant_type(ctors) };
   }

   static Elaboration
   elaborate_initializer(DeclContext& ctx, const Type* type, const Ast* x) {
      auto env = ctx.elaborator();
      if (type == env->get_typename()) {
         if (auto y = is_inductive_definition(x))
            return elaborate_inductive_body(ctx, y);
         return elaborate_type(ctx, x);
      }
      else if (type == env->get_namespace())
         return elaborate_namespace(ctx, x);
      else if (type == env->get_concept())
         return elaborate_concept(ctx, x);
      return env->elaborate(x, type);
   }

   static Elaboration
   finish_definition(BasicContext<>& ctx, Declaration* decl, Elaboration init) {
      auto type = decl->value().type();
      auto name = decl->name();
      if (not satisfies(ctx, init, type))
         semantics_error(ctx, "invalid initializer in definition of "
                         + quote(name));
      decl->value() = init.code();
      // The result is an lvalue.
      const Type* t = make_reference_type(ctx, type);
      return { t, ctx.elaborator()->build_bind(name, type, init) };
   }

   static const Formal*
   elaborate_parameter(BasicContext<>&, const ParameterAst*, int);

   static const ParameterAst*
   ensure_unique_parameter(BasicContext<>& ctx, const ParameterAst* parm,
                           const Parameters& parms, int pos) {
      if (auto name = parm->name())
         for (int i = 0; i < pos; ++i)
            if (lexeme(*parms[i]->name()->token()) == lexeme(*name->token()))
               semantics_error(ctx, "parameters at same level must have "
                               "distinct names", name);
      return parm;
   }

   static Formals
   elaborate_parameters(BasicContext<>& ctx, const Parameters& parms) {
      Formals formals(parms.size());
      for (std::size_t i = 0; i < parms.size(); ++i) {
         auto parm = ensure_unique_parameter(ctx, parms[i], parms, i);
         formals[i] = elaborate_parameter(ctx, parm, i);
      }
      return formals;
   }

   namespace {
      // When elaborating a function body, we want a return value
      // to satisfy the constant the function's return type.
      struct ReturnTypeManager {
         BasicContext<>& ctx;
         ReturnTypeManager(BasicContext<>& c, const Type* t)
               : ctx(c) {
            ctx.elaborator()->push_return_type(t);
         }
         ~ReturnTypeManager() {
            ctx.elaborator()->pop_return_type();
         }
      };
   }
   
   static Elaboration
   elaborate_block(BasicContext<>& ctx, const CompoundAst& stmt) {
      const std::size_t size = length(stmt.sequence());
      LocalScopeManager local_scope { ctx.elaborator() };
      auto env = ctx.elaborator();
      std::vector<Elaboration> stmts(size);
       for (std::size_t i = 0; i < size; ++i)
          stmts[i] = env->elaborate(stmt.sequence()[i]);
      return { env->get_void(), env->build_block(stmts) };
   }

   static void
   load_assumptions_on_parameter(BasicContext<>&, const Formal*) {
      // FIXME: build concept constraints.
   }

   static const Formal*
   elaborate_parameter(BasicContext<>& ctx, const ParameterAst* parm, int pos) {
      auto type = elaborate_type(ctx, parm->type());
      auto env = ctx.elaborator();
      const int level = env->get_parameter_depth();
      auto name = make_identifier(ctx, parm->name());
      LinkName lnk { name, type };
      const Formal* formal = env->build_formal(pos, level, type, lnk);
      load_assumptions_on_parameter(ctx, formal);
      if (name != nullptr)
         env->top()->bind(name, make_elaboration(formal));
      return formal;
   }

   static TypeElaboration
   elaborate_target_type(BasicContext<>& ctx, const Ast* x) {
      auto t = elaborate_type(ctx, x);
      if (auto tt = is<TagType>(t.code()))
         return { t.type(), tt->type() };
      return t;
   }

   static TypeElaboration
   do_elaborate_arrow_type(BasicContext<>& ctx, const LambdaAst& x) {
      InputTypes source = elaborate_parameters(ctx, x.parameters());
      auto target = elaborate_target_type(ctx, x.body());
      return type_elaboration(ctx, ctx.elaborator()->make_arrow_type(target, source));
   }

   static Quantifier
   elaborate_quantifier(const QuantifiedAst& x) {
      return x.quantifier()->token()->kind == token::exists_tok
         ? Quantifier::exists
         : Quantifier::forall;
   }

   static TypeElaboration
   do_elaborate_quantified_type(BasicContext<>& ctx, const QuantifiedAst& x) {
      const Quantifier quant = elaborate_quantifier(x);
      Formals formals = elaborate_parameters(ctx, x.parameters());
      auto target = elaborate_type(ctx, x.body());
      return type_elaboration
         (ctx, ctx.elaborator()->make_quantified_type(quant, formals, target));
   }

   // Return true if the entity designated by `e' has generative instances.
   static bool
   has_generative_instances(Elaboration e, BasicContext<>& ctx) {
      if (has_type_denotation(e, ctx))
         return true;
      auto env = ctx.elaborator();
      return are_equivalent(e.type(), env->get_concept(), ctx)
         or are_equivalent(e.type(), env->get_namespace(), ctx);
   }

   // Construct the mapping object of a function definition.
   static const Expression*
   build_mapping(FormContext& ctx, Elaboration body) {
      auto env = ctx.elaborator();
      LinkName lnk { ctx.name(), ctx.type() };
      auto lam = env->build_lambda(lnk, ctx.formals(), body);
      if (has_generative_instances(body, ctx))
         return env->build_constructor(lnk, { lam->type(), lam });
      return lam;
   }

   static Elaboration
   elaborate_definiens(FormContext& ctx, const Ast* body) {
      auto type = ctx.type();
      const Type* target = type->target();
      ReturnTypeManager push_type { ctx, target };
      Elaboration expr = elaborate_initializer(ctx, target, body);
      auto map = build_mapping(ctx, expr);
      return { type, map };
   }

   static void
   check_if_redeclaration(DeclContext& ctx) {
      auto name = ctx.name();
      auto type = ctx.type();
      if (auto decl = ctx.scope()->select(name, type)) {
         // It makes little sense to redeclare a builtin function.
         auto user = is<Lambda>(decl->value());
         if (user == nullptr)
            semantics_error(ctx, "redeclaration of a builtin function");

         // We certainly don't want to divine what to do with
         // multitple definitions.
         else if (user->body().code() != nullptr)
            semantics_error(ctx, "multiple definition of " + quote(name));
      }
   }

   static Formals
   elaborate_parameters(BasicContext<>& ctx, const CallForm& head) {
      return elaborate_parameters(ctx, head.parameters());
   }
   
   static Formals
   elaborate_parameters(BasicContext<>& ctx, const ClosedForm& head) {
      return elaborate_parameters(ctx, head.parameters());
   }
   
   static Formals
   elaborate_parameters(BasicContext<>& ctx, const InfixForm& head) {
      if (head.lhs()->name() != nullptr
          and lexeme(*head.lhs()->name()->token())
          == lexeme(*head.rhs()->name()->token()))
         semantics_error(ctx, "repeated parameter name in definition",
                         head.rhs());
      Formals formals(2);
      formals[0] = elaborate_parameter(ctx, head.lhs(), 0);
      formals[1] = elaborate_parameter(ctx, head.rhs(), 0);
      return formals;
   }

   static Formals
   elaborate_parameters(BasicContext<>& ctx, const PrefixForm& head) {
      return Formals{ elaborate_parameter(ctx, head.parameter(), 0) };
   }

   static Formals
   elaborate_parameters(BasicContext<>& ctx, const SuffixForm& head) {
      return Formals{ elaborate_parameter(ctx, head.parameter(), 0) };
   }

   // Return the elaboration for a read-access of a parameter
   static Elaboration
   read(BasicContext<>& ctx, const Formal* f) {
      auto rt = make_reference_type(ctx, f->type());
      return { f->type(), ctx.elaborator()->build_read({ rt, f }) };
   }

   // Return a fresh name for a sytnthetized parameter of a given
   // type at a given position.
   static const Formal*
   fresh_formal(BasicContext<>& ctx, int i, TypeElaboration t) {
      auto env = ctx.elaborator();
      return env->build_formal(0, i, t, { env->fresh_name(), t });
   }

   static const Lambda*
   make_coerce_function(BasicContext<>& ctx, const Name* name,
                        const Type* s, const Type* t) {
      auto env = ctx.elaborator();
      TypeElaboration target = { env->get_typename(), t };
      InputTypes source = { { env->get_typename(), s} };
      auto ft = env->make_arrow_type(target, source);
      auto parm = fresh_formal(ctx, 0, source.front());
      return env->build_lambda({ name, ft }, Formals{ parm }, read(ctx, parm));
   }

   // Define a canonical coercion named `n' of type `s -> t' in
   // `scope'.
   static void
   define_coercion(BasicContext<>& ctx, ScopeRef scope,
                   const char* n, const Type* s, const Type* t) {
      auto name = make_identifier(ctx.elaborator(), n);
      auto fun = make_coerce_function(ctx, name, s, t);
      scope->define(name, fun->type(), fun);
   }

   // Build a generative type definition named `name' with `rhs' for
   // the right hand side of the definition.  In the process, make
   // available the canonical value isomorphism
   //    per : rhs -> name     and rep : name -> rhs
   static const GenerativeType*
   generative_type_and_consort(BasicContext<>& ctx,
                               const Name* name, const Type* rhs) {
      auto t = ctx.elaborator()->make_generative_type(name, rhs, ctx.scope().base());
      define_coercion(ctx, ctx.scope(), "per", rhs, t);
      define_coercion(ctx, ctx.scope(), "rep", t, rhs);
      return t;
   }

   static bool
   is_literal(const Name* n) {
      return dynamic_cast<const Literal*>(n);
   }

   // Return true if a given declaration cannot be overloaded.
   static bool
   cannot_overload(BasicContext<>& ctx, const Declaration& d) {
      return has_type_denotation(d.value(), ctx)
         or not is_literal(d.name());
   }

   // Ensure that the definition of the form `x' does not introduce
   // an overload for a type value.
   template<typename X>
   static const Name*
   get_name_and_ensure_uniqueness_if_necessary(BasicContext<>& ctx, const X& x) {
      auto name = get_name(ctx, x);
      for (auto& d : ctx.elaborator()->top()->lookup(name))
         if (cannot_overload(ctx, d))
            semantics_error(ctx, "invalid redefinition", &x);
      return name;
   }

   namespace {
      struct SimpleDeclContext : Elaborator::DeclContext {
         template<typename X>
         SimpleDeclContext(BasicContext<>& ctx, const X& x, const Ast* t)
               : DeclContext(ctx, get_name_and_ensure_uniqueness_if_necessary(ctx, x)),
                 ty(elaborate_type(ctx, t, filter::nothing))
         { }
         const Type* type() const override { return ty; }
      private:
         const Type* ty;
         SimpleDeclContext(SimpleDeclContext&&) = delete;
      };
   }

   // Elaborate the definiens of a simple definition.
   static Elaboration
   elaborate_definiens(SimpleDeclContext& ctx, const Ast* body) {
      auto init = elaborate_initializer(ctx, ctx.type(), body);
      // Type definitions give rise to generative types.
      if (has_type_denotation(init, ctx)) {
         auto t = generative_type_and_consort
            (ctx, ctx.name(), is<Type>(init.code()));
         init = { init.type(), t };
      }
      return init;
   }

   template<template<typename> class T, typename C>
   static Elaboration
   elaborate_definition(C& ctx, const DefinitionAst& def) {
      struct V : FixityForm::Visitor {
         C& ctx;
         const DefinitionAst& def;
         Elaboration result;
         V(C& c, const DefinitionAst& d) : ctx(c), def(d) { }

         void visit(const LiteralForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const AlphabethicForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const OperatorForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const PrefixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const SuffixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const InfixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const ClosedForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const CallForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
      };

      V v { ctx, def };
      def.form()->accept(v);
      return v.result;
   }

   namespace {
      struct NameTypePair {
         std::pair<const Name*, const Type*> rep;
         const Name* name() const { return rep.first; }
         const Type* type() const { return rep.second; }
      };
   }

   static NameTypePair
   get_name_type(BasicContext<>& ctx, const FixityForm* f, const Ast* t) {
      struct V : FixityForm::Visitor {
         BasicContext<>& ctx;
         const Ast* tsyn;
         NameTypePair result;
         V(BasicContext<>& c, const Ast* t) : ctx(c), tsyn(t), result() { }
         void visit(const LiteralForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const AlphabethicForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const OperatorForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const PrefixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const SuffixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const InfixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const ClosedForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const CallForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
      };
      V v { ctx, t };
      f->accept(v);
      return v.result;
   }

   namespace {
      struct formal_name_eq {
         const Symbol name;
         formal_name_eq(Symbol n) : name(n) { }
         bool operator()(const Formal* f) const {
            return f->symbol() == name;
         }
      };
   }

   static void assume_property(BasicContext<>& ctx, Elaboration expr) {
      const Expression* code = expr.code();
      if (auto req = is<Constraint>(code))
         load_requirements(ctx, req);
      else if (auto sig = is<Signature>(code)) {
         auto decl = declare(ctx.elaborator()->top(), sig->link_name());
         decl->value() = sig;   // Give it an abstract value
      }
      else
         load_assumption(ctx, expr);
   }
   
   static Elaboration
   elaborate_logical(BasicContext<>& ctx, logical::Operation op,
                     const BinaryAst& ast) {
      auto env = ctx.elaborator();
      Elaboration lhs = env->elaborate(ast.lhs(), env->get_bool());
      Elaboration rhs = env->elaborate(ast.rhs(), env->get_bool());
      return { env->get_bool(), env->build_logical(op, lhs, rhs) };
   }

   // -- QuantContext --
   namespace {
      struct QuantContext : BasicContext<> {
         QuantContext(BasicContext<>& ctx, const QuantifiedAst& x)
               : BasicContext<>(ctx),
                 quant(elaborate_quantifier(x)),
                 parms_scope(ctx.elaborator()),
                 parms(elaborate_parameters(ctx, x.parameters()))
         { }
         Quantifier quantifier() const { return quant; }
         const Formals& formals() const { return parms; }
      private:
         const Quantifier quant;
         ParameterScopeManager parms_scope;
         Formals parms;
      };
   }

   static const QuantifiedType*
   make_quantified_type(QuantContext& ctx, TypeElaboration t) {
      auto env = ctx.elaborator();
      return env->make_quantified_type(ctx.quantifier(), ctx.formals(), t);
   }

   static const QuantifiedType*
   make_quantified_type(QuantContext& ctx, const Type* t) {
      return make_quantified_type(ctx, type_elaboration(ctx, t));
   }

   namespace {
      template<typename C>
      struct QDefContext : Elaborator::DeclContext {
         template<typename F>
         QDefContext(QuantContext& qc, const F& f, const Ast* t)
               : DeclContext(qc, get_name_and_ensure_uniqueness_if_necessary(qc, f)),
                 qctx(qc),
                 ctx(qc, f, t),
                 ty(make_quantified_type(qc, ctx.type()))
         { }

         const QuantifiedType* type() const override { return ty; }
         QuantContext& quantifying_context() { return qctx; }
         C& quantified_context() { return ctx; }
                 
      private:
         QuantContext& qctx;
         C ctx;
         const QuantifiedType* ty;
      };
   }

   template<typename C>
   static Elaboration
   elaborate_definiens(QDefContext<C>& ctx, const Ast* body) {
      auto expr = elaborate_definiens(ctx.quantified_context(), body);
      auto& qctx = ctx.quantifying_context();
      const LinkName lnk { ctx.name(), ctx.type() };
      auto lam = ctx.elaborator()->build_lambda(lnk, qctx.formals(), expr);
      return { ctx.type(), lam };
   }

   static Elaboration
   elaborate_quantified(BasicContext<>& ctx, const QuantifiedAst& ast) {
      QuantContext new_ctx { ctx, ast };
      auto env = new_ctx.elaborator();
      // 1. elaborate body
      Elaboration body;
      if (auto seq = is<SequenceAst>(ast.body())) {
         const int n = length(seq);
         std::vector<Elaboration> exprs(n);
         for (int i = 0; i < n; ++i)
            exprs[i] = env->elaborate(seq->at(i));
         body = { env->get_void(), env->build_block(exprs) };
      }
      else if (auto x = is<DefinitionAst>(ast.body()))
         return elaborate_definition<QDefContext>(new_ctx, *x);
      else {
         const Type* body_type = is<QuantifiedAst>(ast.body())
            ? env->get_bool().code()
            : nullptr;
         body = env->elaborate(ast.body(), body_type);
      }
      // 2. Now build the formula.
      if (auto x = is<Type>(body)) {
         TypeElaboration t = { body.type(), x };
         auto type = make_quantified_type(new_ctx, t);
         return type_elaboration(new_ctx, type);
      }
      const Formula* formula = env->build_formula(new_ctx.quantifier(), new_ctx.formals(), body);
      auto type = make_quantified_type(new_ctx, body.type());
      return { type, formula };
   }

   static Elaboration
   elaborate_conditional(BasicContext<>& ctx, const IfAst& x, const Type* t) {
      // 0. Set up a new scope for a possible declaration in the condition.
      LocalScopeManager local_scope { ctx.elaborator() };
      auto env = ctx.elaborator();
      // 1. Elaborate the condition.
      Elaboration cond = env->elaborate(x.condition(), env->get_bool());
      // 2. Then the consequence.
      Elaboration conseq = env->elaborate(x.consequence(), t);
      // 3. Finally, the alternative.
      Elaboration alt = env->elaborate(x.alternate(), t);
      // The result of common type or void.
      auto s = conseq.type() == alt.type() ? conseq.type() : env->get_void();
      return { s, env->build_if(cond, conseq, alt) };
   }

   static Elaboration
   elaboration_error(BasicContext<>& ctx, const Ast* ast) {
      semantics_error(ctx, "cannot elaborate this expression", ast);
      return { };
   }

   static Elaboration
   literal_integer(BasicContext<>& ctx, const Token* x, const Type* target) {
      int v = 0;
      for (auto c : *x)
         v = 10 * v +  (c - '0');
      auto env = ctx.elaborator();
      auto val = env->build_int(v, env->get_int());
      if (auto e = successful_coercion(ctx, { val->type(), val }, target))
         return e;
      semantics_error(ctx, "cannot interpret literal " + quote(x)
                      + " as a value of type " + quote(show(*target)), *x);
      return { };
   }

   static Elaboration
   elaborate_string(BasicContext<>& ctx, const Token* token) {
      auto env = ctx.elaborator();
      Symbol s = env->intern(token);
      const Type* t = env->get_string();
      return { t, env->build_string(s, t) };
   }

   // Elaborate a character constant.
   static Elaboration
   elaborate_character(BasicContext<>& ctx, const Token* token) {
      const std::size_t size = length(*token);
      if (size == 0)
         semantics_error(ctx, "missing character in character constant");
      else if (size > 1)
         semantics_error(ctx, "too many characters in character constant");
      auto env = ctx.elaborator();
      const Type* t = env->get_char();
      return { t, env->build_char(Character(*begin(*token)), t) };
   }

   static Fiber
   literal_fiber(BasicContext<>& ctx, const Token* x, const Type* t) {
      Fiber f = lexical_fiber(ctx, literal_name(ctx, x));
      return fiber_section(ctx, f, t);
   }

   static Elaboration
   elaborate_integer(BasicContext<>& ctx, const LiteralAst& x, const Type* type) {
      Fiber f = literal_fiber(ctx, x.token(), type);
      if (f.empty())
         return literal_integer(ctx, x.token(), type);
      else if (f.size() > 1)
         semantics_error(ctx, "use of literal " + quote(&x)
                         + " is ambiguous in this context", &x);
      return f.front();
   }

   static Elaboration
   elaborate_double(BasicContext<>& ctx, const Token* x, const Type* t) {
      std::istringstream is(lexeme(*x));
      double v;
      is >> v;
      if (!is)
         semantics_error(ctx, "invalid floating point literal", *x);
      auto env = ctx.elaborator();
      auto cst = env->build_double(v, env->get_double());
      return env->coerce({ cst->type(), cst}, t);
   }

   static Elaboration
   elaborate_boolean(BasicContext<>& ctx, const Token* token) {
      auto env = ctx.elaborator();
      auto name = env->build_identifier(token);
      auto decls = lexical_lookup(*env, name);
      if (decls.empty())
         semantics_error(ctx, "boolean constant " + quote(name)
                         + " is not defined");
      auto type_eq_bool =
         [&](const Declaration& x) { return x.value().type() == env->get_bool(); };
      return decls.select_if(type_eq_bool)->value();
   }
   
   static Elaboration
   elaborate_literal(BasicContext<>& ctx, const LiteralAst& x, const Type* t) {
      auto env = ctx.elaborator();
      switch (x.token()->kind) {
      default:
         return elaboration_error(ctx, &x);

      case token::literal_real_tok:
         return elaborate_double(ctx, x.token(), t);

      case token::literal_boolean_tok:
         return env->coerce(elaborate_boolean(ctx, x.token()),  t);

      case token::literal_character_tok:
         return env->coerce(elaborate_character(ctx, x.token()), t);
         
      case token::literal_string_tok:
         return env->coerce(elaborate_string(ctx, x.token()), t);
         
      case token::literal_integer_tok:
         return elaborate_integer(ctx, x, t);
      }
   }

   // Elaborate brace-enclosed initializer lists.
   static Elaboration
   elaborate_brace_list(BasicContext<>& ctx, const SequenceAst& x, const Type* t) {
      if (length(x.sequence()) != 1)
         semantics_error(ctx, "non-singleton initializer lists are "
                         "not supported yet.");
      return ctx.elaborator()->elaborate(x.sequence().at(0), t);
   }

   template<typename X>
   static Elaboration
   elaborate_name(BasicContext<>& ctx, const X& x, const Type* t) {
      Fiber f = lexical_fiber_or_else(ctx, x);
      Fiber result = fiber_section(ctx, f, t);
      if (result.empty())
         semantics_error(ctx, "no valid interpretation of this name in"
                         " a context requiring expression of type "
                         + quote(show(*t)), &x);
      else if (not result.singleton())
         semantics_error(ctx, "use of " + quote(&x) + " is ambiguous");
      return result[0];
   }

   static Elaboration
   elaborate_dot(BasicContext<>& ctx, const DotAst& x, const Type* t) {
      Fiber f = fiber_section(ctx, semantics_fiber(ctx, x), t);
      if (f.empty())
         semantics_error(ctx, "selection expression is cannot be coerced to "
                         + quote(show(*t)));
      else if (not f.singleton())
         semantics_error(ctx, "ambiguous expression in context of "
                         + quote(show(*t)) + " expression");
      return f[0];
   }

   static Elaboration
   elaborate_return(BasicContext<>& ctx, const ReturnAst& x) {
      auto env = ctx.elaborator();
      if (env->frame_depth() < 1)
         semantics_error(ctx, "return statement cannot appear "
                         "outside function definitions");
      Elaboration expr = env->elaborate
         (x.expression(), env->get_return_type());
      return { expr.type(), env->build_return(expr) };
   }

   static Elaboration
   elaborate_throw(BasicContext<>& ctx, const ThrowAst& x, const Type* t) {
      auto env = ctx.elaborator();
      Elaboration expr = env->elaborate(x.expression());
      // FIXME: the elaboration type match anything.
      return { t, env->build_throw(expr) };
   }

   // Elaborate an assignment expression.
   // FIXME: restricted to variables only.
   static Elaboration
   elaborate_assignment(BasicContext<>& ctx, const AssignmentAst& x) {
      auto id = is<IdentifierAst>(x.lhs());
      auto env = ctx.elaborator();
      if (id == nullptr)
         env->sorry("variable assignment are currently supported");
      auto lhs = elaborate_name(ctx, *id, nullptr);
      auto t = is<ReferenceType>(lhs.type());
      if (t == nullptr or is<ReadonlyType>(t->referee()))
         semantics_error(ctx, "modifiable reference required on "
                         "left hand side of assignment", x.lhs());
      auto rhs = env->elaborate(x.rhs(), t->referee());
      return { t, env->build_write(lhs, rhs) };
   }

   // -- Elaborate an input source file
   static LoadUnit*
   load_source_file(BasicContext<>& ctx, const SourceFileAst& x) {
      auto env = ctx.elaborator();
      LoadUnit* u = env->build_load_unit(x.path);
      for (auto y : x.asts)
         u->add_stmt(env->elaborate(y));
      return u;
   }
   
   static Elaboration
   elaborate_source_file(BasicContext<>& ctx, const SourceFileAst& x) {
      return { ctx.elaborator()->get_LoadUnit(), load_source_file(ctx, x) };
   }

   static Elaboration
   elaborate_binary(BasicContext<>& ctx, const BinaryAst& x, const Type* t) {
      auto env = ctx.elaborator();
      Elaborator::LocationManager push_loc { env, *anchor(x.operation()) };
      switch (x.operation()->token()->kind) {
      default:
         return elaborate_call(ctx, x.operation(), { x.lhs(), x.rhs() }, t);
         
      case token::plus_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_plus, t);
         
      case token::minus_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_dash, t);

      case token::star_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_star, t);

      case token::slash_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_slash, t);
         
      case token::div_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_div, t);
         
      case token::mod_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_mod, t);
         
      case token::rem_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_rem, t);
         
      case token::greater_tok: 
         return elaborate_infix(ctx, x, &Elaborator::build_rangle, t);
         
      case token::greater_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_rangleq, t);
         
      case token::less_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_langle, t);

      case token::less_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_langleq, t);
         
      case token::double_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_eqeq, t);
         
      case token::not_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_excleq, t);

      case token::equal_tok: 
         return env->coerce(elaborate_assignment(ctx, x.lhs(), x.rhs()), t);
         
      case token::and_tok:
         return env->coerce(elaborate_logical(ctx, logical::conjunction, x), t);
         
      case token::or_tok:
         return env->coerce(elaborate_logical(ctx, logical::disjunction, x), t);
         
      case token::implies_tok:
         return env->coerce(elaborate_logical(ctx, logical::implication, x), t);
         
      case token::equiv_tok:
         return env->coerce(elaborate_logical(ctx, logical::equivalence, x), t);
      }
   }

   // -- Elaborate an expression statement.
   static Elaboration
   elaborate_expr_stmt(BasicContext<>& ctx, const ExprStmtAst& x) {
      Elaboration z = ctx.elaborator()->elaborate(x.expression());
      if (z.type() == nullptr)
         semantics_error(ctx, "expression cannot type check");
      return z;
   }

   // If `x' refers to a string literal AST object, returns a pointer
   // to that object; otherwise return null.
   static const LiteralAst*
   is_string_literal(const Ast* x) {
      if (auto s = is<LiteralAst>(x))
         if (s->token()->kind == token::literal_string_tok)
            return s;
      return nullptr;
   }

   // -- LoadUnit imports.
   static const SourceFileAst*
   read_file(BasicContext<>& ctx, const Path& p) {
      auto rdr = ctx.reader();
      return rdr->read_file(p, rdr->invocation()->current_flags());
   }
   
   static std::string
   resolve_load_path(BasicContext<>& ctx, const Ast* x, std::string d = "") {
      if (auto id = is<IdentifierAst>(x))
         return d + lexeme(*id->token());
      else if (auto s = is_string_literal(x))
         return lexeme(*s->token());
      else if (auto p = is<PathAst>(x))
         return resolve_load_path
            (ctx, p->subpath(), d + lexeme(*p->dirname()->token()) + "/");
      semantics_error(ctx, "invalid module name: " + quote(show(x)));
      return d;
   }
   
   static Elaboration
   elaborate_import(BasicContext<>& ctx, const ImportAst& x) {
      auto src = read_file(ctx, resolve_load_path(ctx, x.path()));
      auto unit = load_source_file(ctx, *src);
      auto env = ctx.elaborator();
      return { env->get_LoadUnit(), env->build_import(unit) };
   }

   static Elaboration
   elaborate_restrict(BasicContext<>& ctx, const RestrictAst& x) {
      const Type* type = elaborate_type(ctx, x.type());
      return ctx.elaborator()->elaborate(x.expression(), type);
   }

   static Elaboration
   elaborate_rule(BasicContext<>& ctx, const RuleAst& x) {
      const Type* type = elaborate_type(ctx, x.type());
      auto id = get_name(ctx, x.form());
      LinkName lnk { id, type };
      auto env = ctx.elaborator();
      auto decl = declare(env->top(), lnk);
      Elaboration e = env->elaborate(x.initializer(), type);
      return finish_definition
         (ctx, decl, { type, env->build_macro(lnk, e) });
   }

   static Clause
   elaborate_case(BasicContext<>& ctx, const CaseAst& x, const Type* t) {
      auto env = ctx.elaborator();
      Elaboration cond = env->elaborate(x.label(), t);
      Elaboration expr = env->elaborate(x.statement());
      return { cond, expr };
   }

   static Elaboration
   elaborate_match(BasicContext<>& ctx, const MatchAst& x) {
      LocalScopeManager new_scope { ctx.elaborator() };
      auto env = ctx.elaborator();
      Elaboration subj = rvalue(ctx, env->elaborate(x.scrutinee()));
      const std::size_t n = x.branches().size();
      Clauses cls(n);
      for (std::size_t i = 0; i < n; ++i)
         cls[i] = elaborate_case(ctx, *x.branches()[i], subj.type());
      return { env->get_void(), env->build_match(subj, cls) };
   }

   // Check that the type context `t' is appropriate for a
   // left- or right- section.  On success return the function type.
   static const ArrowType*
   check_section_context(BasicContext<>& ctx, const Type* t) {
      if (t == nullptr)
         return nullptr;
      auto ft = is<ArrowType>(t);
      if (ft == nullptr or ft->arity() != 1)
         semantics_error(ctx, "use of section in non-unary function context");
      return ft;
   }

   // ------------------------
   // -- PartialApplication --
   // ------------------------
   // Representation of data generated by partial application.
   namespace {
      struct PartialApplication : std::pair<FunctionElaboration, Elaboration> {
         PartialApplication() { }
         PartialApplication(FunctionElaboration f, Elaboration e)
               : std::pair<FunctionElaboration, Elaboration>(f, e)
         { }
         // courtesy conversion for use in conditionals.
         explicit operator bool() const { return first ? true : false; }
      };
   }

   // Return true if `s' is a binary function type whose
   // right section matches `t'.  As a special case, the right
   // section of any function type is acceptable if no target is given.
   static bool
   right_section_equal(const ArrowType* s, const ArrowType* t) {
      if (t == nullptr)
         return true;
      return s->target() == t->target()
         and s->argument(0) == t->argument(0);
   }

   // Return partial application data for viable right section.
   static PartialApplication
   viable_for_right_section(BasicContext<>& ctx, Elaboration f, Elaboration y,
                            const ArrowType* t) {
      if (auto ft = is<ArrowType>(f.type())) {
         if (ft->arity() == 2 and right_section_equal(ft, t))
            if (auto arg = successful_coercion(ctx, y, ft->argument(1)))
               return { { ft, f.code() }, arg };
      }
      return { };
   }

   // Return elaboration data resulting from a partial application of
   // an operator and given second argument.
   static PartialApplication
   fix_second_argument(BasicContext<>& ctx, const OperatorAst& op,
                       Elaboration y, const ArrowType* t) {
      std::vector<PartialApplication> result;
      for (auto& e : lexical_fiber_or_else(ctx, op))
         if (auto data = viable_for_right_section(ctx, rvalue(ctx, e), y, t))
            result.push_back(data);
      if (result.empty())
         semantics_error(ctx, "no suitable operator " + quote(&op)
                             + " in right section", &op);
      else if (result.size() > 1)
         semantics_error(ctx, "ambiguous right section", &op);
      return result.front();
   }

   // Return the type of a right section of a binary operation.
   static const ArrowType*
   right_section_type(BasicContext<>& ctx, const ArrowType* t) {
      return ctx.elaborator()->make_arrow_type(t->target(), { t->argument(0) });
   }
   
   // Subroutine of elaborate_right_section.
   // Build the elaboration for the lambda generated from a right section.
   static Elaboration
   lambda_for_right_section(BasicContext<>& ctx, FunctionElaboration fun,
                            Elaboration arg) {
      auto parm = fresh_formal(ctx, 0, fun.type()->argument(0));
      Elaboration body = call(ctx, fun, { read(ctx, parm), arg });
      auto ftype = right_section_type(ctx, fun.type());
      auto env = ctx.elaborator();
      LinkName name = { env->fresh_name(), ftype };
      // FIXME: close over variables from enclosing scopes.
      return { ftype, env->build_lambda(name, Formals{ parm }, body) };
   }

   // Elaborate a right section.
   static Elaboration
   elaborate_right_section(BasicContext<>& ctx, const RightSectionAst& x,
                           const Type* t) {
      auto ft = check_section_context(ctx, t);
      auto env = ctx.elaborator();
      Elaboration arg = env->elaborate(x.rhs());
      auto data = fix_second_argument(ctx, *x.operation(), arg, ft);
      return env->coerce
         (lambda_for_right_section(ctx, data.first, data.second), ft);
   }

   // Elaborate use of an empty brackets.
   // FIXME: Ideally, the semantics of these should be controlled
   // or given by concepts.
   static Elaboration
   elaborate_empty_enclosure(BasicContext<>& ctx, const EnclosureAst& x,
                            const Type* t) {
      auto env = ctx.elaborator();
      if ((is_braced(x) or is_parenthesized(x))
          and (t == nullptr or are_equivalent(t, env->get_void(), ctx)))
         return { env->get_void(), nullptr };
      else if (is_parenthesized(x)
               and are_equivalent(t, env->get_typename(), ctx))
         return { t, env->get_void() };
      else if (is_braced(x) and are_equivalent(t, env->get_concept(), ctx))
         return { env->get_concept(), nullptr };
      return elaborate_name(ctx, *x.bracket(), t);
   }

   // Determine whether the syntactic object `x' is enclosed
   // in brackets.
   static bool
   is_bracketed(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_bracket_tok;
   }

   static Elaboration
   elaborate_record(BasicContext<>& ctx, const Ast* x, const RecordType* t) {
      auto env = ctx.elaborator();
      if (x == nullptr) {
         if (not t->empty())
            semantics_error(ctx, "empty initializer for object of "
                            " record type " + quote(show(*t)));
         return { t, nullptr };
      }
      else if (auto y = is<SequenceAst>(x)) {
         if (length(y) < t->size())
            semantics_error(ctx, "too few initializer for record object", x);
         else if (length(y) > t->size())
            semantics_error(ctx, "too many initializer for record object", x);
         const std::size_t n = length(y);
         AssocArguments inits(n);
         for (std::size_t i = 0; i < n; ++i) {
            auto val = env->elaborate(y->at(i), t->at(i)->type());
            inits[i] = { t->at(i)->tag()->symbol(), val };
         }
         return { t, env->build_initializer(inits) };
      }
      if (t->empty())
         semantics_error(ctx, "missing initializer for record field");
      else if (t->size() > 1)
         semantics_error(ctx, "too many initializer for record object", x);
      auto e = env->elaborate(x, t->front()->type());
      auto n = t->front()->tag()->symbol();
      return { t, env->build_initializer(AssocArguments{ 1, {n, e} }) };
   }

   static bool
   is_paren_expr(const EnclosureAst& x) {
      if (not is_parenthesized(x))
         return false;
      return is<SequenceAst>(x.expr()) == nullptr;
   }
   
   static Elaboration
   elaborate_enclosure(BasicContext<>& ctx, const EnclosureAst& x, const Type* t) {
      if (x.expr() == nullptr)
         return elaborate_empty_enclosure(ctx, x, t);
      else if (auto rt = is<RecordType>(t)) {
         if (is_bracketed(x))
            return elaborate_record(ctx, x.expr(), rt);
      }
      else if (is_paren_expr(x))
         return ctx.elaborator()->elaborate(x.expr(), t);
      return elaborate_call(ctx, x.bracket(), get_sequence(x.expr()), t);
   }

   static Elaboration
   elaborate_assert(BasicContext<>& ctx, const AssertAst& x, const Type* t) {
      auto env = ctx.elaborator();
      auto e = env->elaborate(x.expression(), t);
      TypeElaboration s = { env->get_typename(), e.type() };
      auto ft = make_predicate_type(ctx, { s });
      auto p = env->elaborate(x.predicate(), ft);
      return { e.type(), env->build_assertion(e, { ft, p.code() }) };
   }

   static Elaboration
   elaborate_postulate(BasicContext<>& ctx, const PostulateAst& x) {
      auto nt = get_name_type(ctx, x.form(), x.type());
      LinkName lnk { nt.name(), nt.type() };
      auto env = ctx.elaborator();
      auto val = env->build_postulate(lnk);
      return declare(env->top(), lnk, val)->value();
   }

   // Elaborate the collection of input types for an arrow type.
   static InputTypes
   elaborate_arrow_domain(BasicContext<>& ctx, const SequenceAst& x) {
      InputTypes src;
      for (auto y : x.sequence())
         src.push_back(elaborate_type(ctx, y));
      return src;
   }

   // Subroutine of elaborate_arrow.
   // Elaborate the domain of an arrow type.  It can be a single type,
   // or a multitude of types in a parenthesized comma-separated list.
   static InputTypes
   elaborate_arrow_domain(BasicContext<>& ctx, const Ast* x) {
      // FIXME: the maze of if-statements below reflects an AST
      // design weakness.
      if (auto y = is<EnclosureAst>(x))
         if (is_parenthesized(*y)) {
            if (y->expr() == nullptr)
               return { };
            else if (auto z = is<SequenceAst>(y->expr()))
               return elaborate_arrow_domain(ctx, *z);
            return { elaborate_type(ctx, y) };
         }
      return { elaborate_type(ctx, x) };
   }

   static Elaboration
   elaborate_arrow(BasicContext<>& ctx, const ArrowAst& x, const Type* t) {
      auto env = ctx.elaborator();
      // FIXME: "-> is somewhat polymorphic; default to type elaboration.
      if (t == nullptr or has_type_values(t, ctx)) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         auto source = elaborate_arrow_domain(ctx, x.source());
         auto target = elaborate_type(ctx, x.target());
         // FIXME: Check for dependency.
         auto ft = env->make_arrow_type(target, source);
         return env->coerce({ env->get_typename(), ft }, t);
      }
      semantics_error(ctx, "ambiguous arrow expression", x.source());
      return { };
   }

   static Elaboration
   elaborate_prolong(BasicContext<>& ctx, const ProlongAst& x) {
      auto name = get_name(ctx, x.form());
      auto env = ctx.elaborator();
      auto decls = env->top()->lookup(name);
      if (decls.empty())
         semantics_error(ctx, "cannot prolong inexistent entity", &x);
      auto type = elaborate_type(ctx, x.type());
      if (not are_equivalent(type, env->get_namespace(), ctx))
         semantics_error(ctx, "cannot prolong a non-scope entity", x.type());
      auto is_namespace =
         [&](const Declaration& x) { return x.value().type() == env->get_namespace(); };
      auto decl = decls.select_if(is_namespace);
      if (decl == nullptr)
         semantics_error(ctx, "entity does not designate a scope", x.form());
      auto ns = is<Namespace>(decl->value());
      // FIXME: cast in next statement is a sign of design bug.
      elaborate_members(ctx, x.extension(), const_cast<Namespace*>(ns));
      return { env->get_namespace(), ns };
   }

   static Elaboration
   elaborate_where(BasicContext<>& ctx, const WhereAst& x, const Type* t) {
      LocalScopeManager new_scope { ctx.elaborator() };
      vector<Elaboration> decls;
      auto env = ctx.elaborator();
      for (auto y : x.locals())
         decls.push_back(env->elaborate(y));
      auto body = env->elaborate(x.expression(), t);
      return { body.type(), env->build_let(decls, body) };
   }

   static Elaboration
   elaborate_variant(BasicContext<>& ctx, const DatatypeAst& x, const Type* t) {
      const std::size_t n = x.members().size();
      Sequence<Constructor> ctors(n);
      for (std::size_t i = 0; i < n; ++i)
         ctors[i] = elaborate_associated_ctor(ctx, *x.members()[i], i);
      auto env = ctx.elaborator();
      auto value = env->make_variant_type(ctors);
      return env->coerce({ env->get_typename(), value }, t);
   }

   static void
   sorry_for_unimplemented(BasicContext<>& ctx, const Ast& x) {
      ctx.elaborator()->sorry("unimplemented elaboration for " + quote(show(&x)));
   }

   static Elaboration
   elaborate_datatype(BasicContext<>& ctx, const DatatypeAst& x, const Type* t) {
      switch (x.sort().kind) {
      case token::record_tok:
         return elaborate_record(ctx, x);
      case token::variant_tok:
         return elaborate_variant(ctx, x, t);
      default:
         sorry_for_unimplemented(ctx, x);
      }
      return { };
   }

   static void
   print_ast_elaboration(BasicContext<>& ctx, const Ast* x, const Expression* e,
                         const Type* t) {
      if (ctx.elaborator()->enabled(Debug::Codegen)) {
         auto io = ctx.elaborator()->io();
         io->debug() << "======== Elaborator::elaborate ========" << std::endl;
         io->debug() << show(x) << std::endl;
         if (t == nullptr)
            io->debug() << "\tinference mode" << std::endl;
         else {
            io->debug() << "\tchecking mode, expecting:" << std::endl;
            io->debug() << show_type(t) << std::endl;
         }
         io->debug() << "\t\t-----" << std::endl;
         io->debug() << show(e) << std::endl;
         io->debug() << "=======================================" << std::endl;
      }
   }
   
   template<typename T>
   using Identity = T;

   Elaboration
   Elaborator::elaborate(const Ast* ast, const Type* type) {
      if (ast == nullptr)
         return coerce({ get_void(), nullptr }, type);

      struct V {
         BasicContext<>& ctx;
         const Type* type;
         Elaboration result;
 
         void operator()(const Ast& x) { sorry_for_unimplemented(ctx, x); }

         void operator()(const LiteralAst& x) {
            result = elaborate_literal(ctx, x, type);
         }

         void operator()(const IdentifierAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const OperatorAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const BracketAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const AssertAst& x) {
            result = elaborate_assert(ctx, x, type);
         }

         void operator()(const DatatypeAst& x) {
            result = elaborate_datatype(ctx, x, type);
         }

         void operator()(const RuleAst& x) {
            result = ctx.elaborator()->coerce(elaborate_rule(ctx, x), type);
         }

         void operator()(const UnaryAst& x) {
            result = elaborate_unary(ctx, x, type);
         }

         void operator()(const BinaryAst& x) {
            result = elaborate_binary(ctx, x, type);
         }

         void operator()(const BiSectionAst& x) {
            result = elaborate_name(ctx, *x.operation(), type);
         }

         void operator()(const LeftSectionAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const RightSectionAst& x) {
            result = elaborate_right_section(ctx, x, type);
         }

         void operator()(const RestrictAst& x) {
            result = ctx.elaborator()->coerce(elaborate_restrict(ctx, x), type);
         }

         void operator()(const QuantifiedAst& x) {
            result = ctx.elaborator()->coerce(elaborate_quantified(ctx, x), type);
         }

         void operator()(const ApplyAst& x) {
            result = elaborate_call(ctx, x, type);
         }

         void operator()(const JuxtaposeAst& x) {
            result = elaborate_juxtapose(ctx, x, type);
         }

         void operator()(const SequenceAst& x) {
            result = elaborate_brace_list(ctx, x, type);
         }

         void operator()(const EnclosureAst& x) {
            result = elaborate_enclosure(ctx, x, type);
         }

         void operator()(const DotAst& x) {
            result = elaborate_dot(ctx, x, type);
         }

         void operator()(const IntervalAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const ExprStmtAst& x) {
            result = ctx.elaborator()->coerce(elaborate_expr_stmt(ctx, x), type);
         }

         void operator()(const ReturnAst& x) {
            result = elaborate_return(ctx, x);
         }

         void operator()(const ThrowAst& x) {
            result = elaborate_throw(ctx, x, type);
         }

         void operator()(const LeaveAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const MatchAst& x) {
            result = ctx.elaborator()->coerce(elaborate_match(ctx, x), type);
         }

         void operator()(const CompoundAst& x) {
            result = elaborate_block(ctx, x);
         }

         void operator()(const AssignmentAst& x) {
            result = elaborate_assignment(ctx, x);
         }

         void operator()(const IfAst& x) {
            result = elaborate_conditional(ctx, x, type);
         }

         void operator()(const FilterAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const CaseAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const CollectAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const RepeatAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const LambdaAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const WhereAst& x) {
            result = elaborate_where(ctx, x, type);
         }

         void operator()(const SourceFileAst& x) {
            result = ctx.elaborator()->coerce(elaborate_source_file(ctx, x), type);
         }

         void operator()(const PathAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const ImportAst& x) {
            result = ctx.elaborator()->coerce(elaborate_import(ctx, x), type);
         }

         void operator()(const DefinitionAst& x) {
            result = ctx.elaborator()->coerce(elaborate_definition<Identity>(ctx, x), type);
         }

         void operator()(const ProlongAst& x) {
            result = ctx.elaborator()->coerce(elaborate_prolong(ctx, x), type);
         }

         void operator()(const PostulateAst& x) {
            result = ctx.elaborator()->coerce(elaborate_postulate(ctx, x), type);
         }

         void operator()(const ArrowAst& x) {
            result = elaborate_arrow(ctx, x, type);
         }

         void operator()(const ParameterAst& x) {
            result = ctx.elaborator()->coerce(elaborate_type(ctx, &x), type);
         }

         void operator()(const DescriptionAst&) {
            auto env = ctx.elaborator();
            result = env->coerce({ env->get_void(), nullptr }, type);
         }
      };

      LocationManager push_loc(this, *anchor(ast));
      BasicContext<> ctx { this, evl };
      ast_visitor<V> v { ctx, type, make_elaboration(get_void(), nullptr) };
      ast->accept(v);
      print_ast_elaboration(ctx, ast, v.result.code(), type);
      return v.result;
   }
}
