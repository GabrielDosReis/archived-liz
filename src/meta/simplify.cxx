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

#include "Elaborator.H"

namespace liz {
   // --------------------------------------
   // -- Template instantiation machinery --
   // --------------------------------------
   template<typename Map>
   static const Type* subst_type(BasicContext<>&, const Type*, const Map&);

   template<typename Map>
   static TypeElaboration
   substitute(BasicContext<>& ctx, TypeElaboration e, const Map& map) {
      const Type* x = subst_type(ctx, e.code(), map);
      const Type* t = subst_type(ctx, e.type(), map);
      return { t, x };
   }

   // -- Instantiate the function type `ftype' in a given `context'
   template<typename Map>
   static const ArrowType*
   substitute(BasicContext<>& ctx, const ArrowType* ftype, const Map& map) {
      const int nargs = ftype->arity();
      InputTypes args(nargs);
      for (int i = 0; i < nargs; ++i)
         args[i] = substitute(ctx, ftype->argument(i), map);
      return ctx.elaborator()->make_arrow_type
         (substitute(ctx, ftype->target(), map), args);
   }

   // -- Instantiate generative type
   template<typename Map>
   static const GenerativeType*
   substitute(BasicContext<>& ctx, const GenerativeType& t, const Map& map) {
      auto rhs = subst_type(ctx, t.value(), map);
      if (rhs == t.value())
         return &t;
      // FIXME: instantiate scope, when we support parameterized scopes.
      // FIXME: re-instate support for parameterized names.
      return ctx.elaborator()->make_generative_type(t.name(), rhs, t.scope());
   }

   // -- Instantiate a dependent type.
   template<typename Map>
   static const QuantifiedType*
   substitute(BasicContext<>& ctx, const QuantifiedType* qt, const Map& map) {
      auto t = substitute(ctx, qt->abstract_instance(), map);
      if (t == qt->abstract_instance())
         return qt;
      return ctx.elaborator()->make_quantified_type
         (qt->quantifier(), qt->formals(), t);
   }

   // -- Instantiate an abstract type variable.
   template<typename Map>
   static const Type*
   substitute(BasicContext<>& ctx, const TypeExpression& x, const Map& map) {
      if (auto f = is_type_variable_value(x)) {
         auto e = subst_expr(ctx, { f->type(), f }, map);
         if (e.code() != f)
            return ctx.elaborator()->coerce_to_type(e);
      }
      return ctx.elaborator()->coerce_to_type(subst_expr(ctx, x.expr(), map));
   }

   template<typename Map>
   struct TypeSubtitutionVisitor : Expression::Visitor {
      BasicContext<>& ctx;
      const Map& map;
      const Type* result;
      TypeSubtitutionVisitor(BasicContext<>& c, const Map& m)
            : ctx(c), map(m), result() { }
      
      void visit(const Expression& x) {
         internal_error("alien expression of type " + show_cxx_type(&x));
      }
      void visit(const BasicType& x) { result = &x; }
      void visit(const TagType& x) {
         result = ctx.elaborator()->make_tag_type(x.tag(), substitute(ctx, x.type(), map));
      }
      void visit(const ReferenceType& x) {
         result = ctx.elaborator()->make_reference_type(substitute(ctx, x.referee(), map));
      }
      void visit(const ArrowType& x) {
         result = substitute(ctx, &x, map);
      }
      void visit(const ReadonlyType& x) {
         result = ctx.elaborator()->make_readonly_type(substitute(ctx, x.type(), map));
      }
      void visit(const TypeExpression& x) {
         result = substitute(ctx, x, map);
      }
      void visit(const QuantifiedType& x) {
         result = substitute(ctx, &x, map);
      }
      void visit(const GenerativeType& x) { result = substitute(ctx, x, map); }
   };
   
   template<typename Map>
   static const Type*
   subst_type(BasicContext<>& ctx, const Type* t, const Map& map) {
      if (t == nullptr)
         return nullptr;
      TypeSubtitutionVisitor<Map> v(ctx, map);
      t->accept(v);
      return v.result;
   }

   template<typename Map>
   static Arguments
   substitute(BasicContext<>& ctx, const Arguments& args, const Map& map) {
      Arguments v(args.size());
      for (std::size_t i = 0; i < args.size(); ++i)
         v[i] = subst_expr(ctx, args[i], map);
      return v;
   }

   template<typename Map>
   static const Constraint*
   substitute(BasicContext<>& ctx, const Constraint* c, const Map& map) {
      // FIXME: instantiate concepts.
      return ctx.elaborator()->build_constraint
         (c->constructor(), substitute(ctx, c->arguments(), map));
   }

   // -- Instantiate a name.
   template<typename Map>
   static const Name* subst_name(BasicContext<>&, const Name*, const Map&);

   template<typename Map>
   static const LinkName*
   substitute(BasicContext<>& ctx, const LinkName* lnk, const Map& map) {
      return ctx.elaborator()->build_name
         (lnk->name(), subst_type(ctx, lnk->type(), map));
   }

   template<typename Map>
   static LinkName
   substitute(BasicContext<>& ctx, const LinkName& lnk, const Map& map) {
      return { lnk.name(), subst_type(ctx, lnk.type(), map) };
   }

   // -- Instantiate a parameter by instantiating its type through
   // -- the substitution.  This is different from replacing the
   // -- formal by its image by the substitution.
   template<typename Map>
   static const Formal*
   subst_formal(BasicContext<>& ctx, const Formal* parm, const Map& map) {
      TypeElaboration t = substitute(ctx, parm->type(), map);
      auto lnk = substitute(ctx, parm->link_name(), map);
      if (t == parm->type() and lnk == parm->link_name())
         return parm;
      return ctx.elaborator()->build_formal(parm->position(), parm->level(), t, lnk);
   }

   // -- Instantiate a user-defined function `fun' in a given
   // -- `context' and substitution `subst'.
   const Lambda*
   substitute(BasicContext<>& ctx, const Lambda* fun, Substitution subst) {
      // 1. Don't work too hard on repeating yourself.
      if (subst.empty())
         return fun;
      if (const Expression* x =
          ctx.elaborator()->retrieve_specialization(fun, subst))
         return is<Lambda>(x);
      // 2, Register a declaration for this instance to cope
      //    recursion.
      auto lnk = substitute(ctx, fun->link_name(), subst);
      const std::size_t n = fun->arity();
      Formals formals(n);
      for (std::size_t i = 0; i < n; ++i) {
         formals[i] = subst_formal(ctx, fun->parameter(i), subst);
         if (formals[i] != fun->parameter(i))
            subst[fun->parameter(i)] = { formals[i]->type(), formals[i] };
      }
      Elaboration body = subst_expr(ctx, fun->body(), subst);
      auto inst = ctx.elaborator()->build_lambda(lnk, formals, body);
      ctx.elaborator()->register_specialization(inst, fun, subst);
      return inst;
   }

   template<typename T, typename Map>
   static const T*
   instantiate(BasicContext<>& ctx, const T& x, unary_builder<T> builder,
               const Map& map) {
      auto fun = substitute(ctx, x.function(), map);
      auto arg = subst_expr(ctx, x.argument(), map);
      return (ctx.elaborator()->*builder)(fun, arg);
   }

   template<typename T, typename Map>
   static const T*
   instantiate(BasicContext<>& ctx, const T& x, binary_builder<T> builder,
               const Map& map) {
      auto fun = substitute(ctx, x.function(), map);
      auto lhs = subst_expr(ctx, x.lhs(), map);
      auto rhs = subst_expr(ctx, x.rhs(), map);
      return (ctx.elaborator()->*builder)(fun, lhs, rhs);
   }

   template<typename Map>
   static const Constructor*
   instantiate(BasicContext<>& ctx, const Constructor& c, const Map& map) {
      auto type = subst_type(ctx, c.type(), map);
      auto impl = subst_expr(ctx, c.implementation(), map);
      if (type == c.type() and impl == c.implementation())
         return &c;
      return ctx.elaborator()->build_constructor({ c.name(), type }, impl);
   }

   namespace {
      struct InstantiatorVisitor : Expression::Visitor {
         BasicContext<>& ctx;
         const Substitution& subst;
         Elaboration result;
         InstantiatorVisitor(BasicContext<>& c, const Substitution& s,
                             const Type* t)
               : ctx(c),
                 subst(s),
                 result(t, nullptr)
         { }

         void visit(const Expression& x) {
            internal_error("alien expression of type " + show_cxx_type(&x));
         }
         void visit(const LinkName& x) {
            result = substitute(ctx, &x, subst);
         }
         void visit(const Value& v) { result = &v; }
         void visit(const Constructor& c) {
            result = instantiate(ctx, c, subst);
         }
         void visit(const Type& x) {
            result = subst_type(ctx, &x, subst);
         }
         void visit(const Formula& x) {
            const std::size_t n = x.parameters().size();
            Formals parms(n);
            Substitution renamer;
            for (std::size_t i = 0; i < n; ++i)
               parms[i] = subst_formal(ctx, x.parameter(i), subst);
            Elaboration body = subst_expr(ctx, x.body(), renamer);
            result = ctx.elaborator()->build_formula(x.quantifier(), parms, body);
         }

         void visit(const Formal& x) {
            // FIXME: don't we need to instantiate the type of the formal too?
            result = subst(&x);
         }
         void visit(const NiladicBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const UnaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const BinaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const Lambda& x) {
            result = substitute(ctx, &x, subst);
         }
         void visit(const Signature& x) {
            auto lnk = substitute(ctx, x.link_name(), subst);
            result = ctx.elaborator()->build_signature(lnk);
         }
         void visit(const Read& x) {
            Elaboration s = subst_expr(ctx, x.address(), subst);
            result = ctx.elaborator()->build_read(s);
         }
         void visit(const Write& x) {
            result = ctx.elaborator()->build_write
               (subst_expr(ctx, x.address(), subst),
                subst_expr(ctx, x.value(), subst));
         }
         void visit(const Offset& x) {
            result = ctx.elaborator()->build_offset
               (subst_expr(ctx, x.address(), subst),
                 subst_expr(ctx, x.delta(), subst));
         }
         void visit(const Component& x) {
            auto n = substitute(ctx, x.link_name(), subst);
            result = ctx.elaborator()->build_component
                (subst_expr(ctx, x.whole(), subst), n);
         }
         void visit(const DotSelection& x) {
            auto n = substitute(ctx, x.link_name(), subst);
            result = ctx.elaborator()->build_dot
               (subst_expr(ctx, x.whole(), subst), n);
         }
         void visit(const Negate& x) {
            result = instantiate(ctx, x, &Elaborator::build_negate, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Not& x) {
            result = instantiate(ctx, x, &Elaborator::build_not, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Complement& x) {
            result = instantiate(ctx, x, &Elaborator::build_complement, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Plus& x) {
            result = instantiate(ctx, x, &Elaborator::build_plus, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Dash& x) {
            result = instantiate(ctx, x, &Elaborator::build_dash, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Star& x) {
            result = instantiate(ctx, x, &Elaborator::build_star, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Slash& x) {
            result = instantiate(ctx, x, &Elaborator::build_slash, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Div& x) {
            result = instantiate(ctx, x, &Elaborator::build_div, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Mod& x) {
            result = instantiate(ctx, x, &Elaborator::build_mod, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Rem& x) {
            result = instantiate(ctx, x, &Elaborator::build_rem, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Langle& x) {
            result = instantiate(ctx, x, &Elaborator::build_langle, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Rangle& x) {
            result = instantiate(ctx, x, &Elaborator::build_rangle, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Langleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_langleq, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Rangleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_rangleq, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Eqeq& x) {
            result = instantiate(ctx, x, &Elaborator::build_eqeq, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Excleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_excleq, subst);
            result = evaluate(ctx, result);
         }
         void visit(const And& x) {
            result = instantiate(ctx, x, &Elaborator::build_and, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Or& x) {
            result = instantiate(ctx, x, &Elaborator::build_or, subst);
            result = evaluate(ctx, result);
         }
         void visit(const Call& x) {
            auto fun = substitute(ctx, x.function(), subst);
            result = ctx.elaborator()->build_call
                (fun, substitute(ctx, x.arguments(), subst));
            result = evaluate(ctx, result);
         }
         void visit(const BinaryLogical& x) {
            result = ctx.elaborator()->build_logical
                (x.operation(), subst_expr(ctx, x.lhs(), subst),
                 subst_expr(ctx, x.rhs(), subst));
         }
         void visit(const Constraint& x) {
            // FIXME: Handle parameterized concepts later.
            result = ctx.elaborator()->build_constraint
                (x.constructor(), substitute(ctx, x.arguments(), subst));
         }
         void visit(const Return& x) {
            result = ctx.elaborator()->build_return
                (subst_expr(ctx, x.expression(), subst));
         }
         void visit(const Throw& x) {
            result = ctx.elaborator()->build_throw
                (subst_expr(ctx, x.expression(), subst));
         }
         void visit(const Loop& x) {
            result = ctx.elaborator()->build_loop
                (subst_expr(ctx, x.body(), subst));
         }
         void visit(const If& x) {
            Elaboration c = subst_expr(ctx, x.condition(), subst);
            Elaboration tt = subst_expr(ctx, x.consequence(), subst);
            Elaboration ff = subst_expr(ctx, x.alternative(), subst);
            result = ctx.elaborator()->build_if(c, tt, ff);
         }
         void visit(const Bind& x) {
            auto lnk = substitute(ctx, x.link_name(), subst);
            auto init = subst_expr(ctx, x.initializer(), subst);
            result = ctx.elaborator()->build_bind(lnk, init);
         }
         void visit(const Block& x) {
            result = ctx.elaborator()->build_block
                (substitute(ctx, x.statements(), subst));
         }
      };
   }

   Elaboration
   subst_expr(BasicContext<>& ctx, Elaboration expr, const Substitution& subst) {
      if (subst.empty())
         return expr;
      const Type* t = subst_type(ctx, expr.type(), subst);
      if (expr.code() == nullptr)
         return { t, nullptr };
      InstantiatorVisitor v(ctx, subst, t);
      expr.code()->accept(v);
      return v.result;
   }

   FunctionElaboration
   substitute(BasicContext<>& ctx, FunctionElaboration fun,
              const Substitution& subst) {
      if (subst.empty())
         return fun;
      const ArrowType* t = substitute(ctx, fun.type(), subst);
      if (fun.code() == nullptr)
         return { t, nullptr };
      InstantiatorVisitor v(ctx, subst, t);
      fun.code()->accept(v);
      return { t, v.result.code() };
   }


   // -- substitute evidence into function expressions.
   static Elaboration
   subst_expr(BasicContext<>&, Elaboration, const Evidence&);
   

   static const Lambda*
   substitute(BasicContext<>& ctx, const Lambda* fun, const Evidence& evidence) {
      Elaboration body = subst_expr(ctx, fun->body(), evidence);
      auto inst = ctx.elaborator()->build_lambda
         (substitute(ctx, fun->link_name(), evidence), fun->formals(), body);
      return inst;
   }

   namespace {
      struct SignatureSubstitutionVisitor : Expression::Visitor {
         BasicContext<>& ctx;
         const Evidence& ev;
         Elaboration result;
         SignatureSubstitutionVisitor(BasicContext<>& c, const Type* t,
                                      const Evidence& e)
               : ctx(c), ev(e), result(t, nullptr) { }

         void visit(const Expression& x) {
            internal_error("alien expression of type " + show_cxx_type(&x));
         }
         void visit(const Value& v) { result = &v; }
         void visit(const Constructor& c) {
            result = instantiate(ctx, c, ev);
         }
         void visit(const Formal& f) { result = &f; }
         void visit(const NiladicBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const UnaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const BinaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            result = ctx.elaborator()->build_builtin(x.name(), t, x.code());
         }
         void visit(const Lambda& x) { result = substitute(ctx, &x, ev); }
         void visit(const Signature& x) { result = ev(&x); }
         void visit(const Type& x) { result = subst_type(ctx, &x, ev); }
         // FIXME:
         // void visit(const TypeExpression& x) {
         //    result = coerce_to_type
         //       (ctx, (evaluate(ctx, subst_expr
         //                           (ctx, x.expr, ev))));
         // }
         void visit(const Read& x) {
            Elaboration s = subst_expr(ctx, x.address(), ev);
            result = ctx.elaborator()->build_read(s);
         }
         void visit(const Write& x) {
            result = ctx.elaborator()->build_write
                (subst_expr(ctx, x.address(), ev),
                 subst_expr(ctx, x.value(), ev));
         }
         void visit(const Offset& x) {
            result = ctx.elaborator()->build_offset
                (subst_expr(ctx, x.address(), ev),
                 subst_expr(ctx, x.delta(), ev));
         }
         void visit(const Component& x) {
            auto n = substitute(ctx, x.link_name(), ev);
            result = ctx.elaborator()->build_component
                (subst_expr(ctx, x.whole(), ev), n);
         }
         void visit(const DotSelection& x) {
            auto n = substitute(ctx, x.link_name(), ev);
            result = ctx.elaborator()->build_dot
               (subst_expr(ctx, x.whole(), ev), n);
         }
         void visit(const Negate& x) {
            result = instantiate(ctx, x, &Elaborator::build_negate, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Not& x) {
            result = instantiate(ctx, x, &Elaborator::build_not, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Complement& x) {
            result = instantiate(ctx, x, &Elaborator::build_complement, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Plus& x) {
            result = instantiate(ctx, x, &Elaborator::build_plus, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Dash& x) {
            result = instantiate(ctx, x, &Elaborator::build_dash, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Star& x) {
            result = instantiate(ctx, x, &Elaborator::build_star, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Slash& x) {
            result = instantiate(ctx, x, &Elaborator::build_slash, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Div& x) {
            result = instantiate(ctx, x, &Elaborator::build_div, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Mod& x) {
            result = instantiate(ctx, x, &Elaborator::build_mod, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Rem& x) {
            result = instantiate(ctx, x, &Elaborator::build_rem, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Langle& x) {
            result = instantiate(ctx, x, &Elaborator::build_langle, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Rangle& x) {
            result = instantiate(ctx, x, &Elaborator::build_rangle, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Langleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_langleq, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Rangleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_rangleq, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Eqeq& x) {
            result = instantiate(ctx, x, &Elaborator::build_eqeq, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Excleq& x) {
            result = instantiate(ctx, x, &Elaborator::build_excleq, ev);
            result = evaluate(ctx, result);
         }
         void visit(const And& x) {
            result = instantiate(ctx, x, &Elaborator::build_and, ev);
            result = evaluate(ctx, result);
         }
         void visit(const Or& x) {
            result = instantiate(ctx, x, &Elaborator::build_or, ev);
            result = evaluate(ctx, result);
         }
         void visit(const BinaryLogical& x) {
            result = ctx.elaborator()->build_logical
                (x.operation(),
                 subst_expr(ctx, x.lhs(), ev),
                 subst_expr(ctx, x.rhs(), ev));
         }
         void visit(const Call& x) {
            auto fun = substitute(ctx, x.function(), ev);
            result = ctx.elaborator()->build_call
                (fun, substitute(ctx, x.arguments(), ev));
            result = evaluate(ctx, result);
         }
         void visit(const Constraint& x) {
            // FIXME: Handle parameterized concepts later.
            result = ctx.elaborator()->build_constraint
                (x.constructor(), substitute(ctx, x.arguments(), ev));
         }
         void visit(const Return& x) {
            Elaboration e = subst_expr(ctx, x.expression(), ev);
            result = ctx.elaborator()->build_return(e);
         }
         void visit(const Throw& x) {
            Elaboration e = subst_expr(ctx, x.expression(), ev);
            result = ctx.elaborator()->build_throw(e);
         }
         void visit(const Loop& x) {
            Elaboration e = subst_expr(ctx, x.body(), ev);
            result = ctx.elaborator()->build_loop(e);
         }
         void visit(const If& x) {
            Elaboration cond = subst_expr(ctx, x.condition(), ev);
            Elaboration conseq = subst_expr(ctx, x.consequence(), ev);
            Elaboration alt = subst_expr(ctx, x.alternative(), ev);
            result = ctx.elaborator()->build_if(cond, conseq, alt);
         }
         void visit(const Bind& x) {
            auto lnk = substitute(ctx, x.link_name(), ev);
            auto init = subst_expr(ctx, x.initializer(), ev);
            result = ctx.elaborator()->build_bind(lnk, init);
         }
         void visit(const Block& x) {
            result = ctx.elaborator()->build_block
                (substitute(ctx, x.statements(), ev));
         }
      };
   }

   static Elaboration
   subst_expr(BasicContext<>& ctx, Elaboration expr, const Evidence& evidence) {
      if (expr == Elaboration() or evidence.empty())
         return expr;
      const Type* t = subst_type(ctx, expr.type(), evidence);
      SignatureSubstitutionVisitor v { ctx, t, evidence };
      expr.code()->accept(v);
      return v.result;
   }

   FunctionElaboration
   substitute(BasicContext<>& ctx, FunctionElaboration fun,
              const Evidence& evidence) {
      if (evidence.empty())
         return fun;
      const ArrowType* t = substitute(ctx, fun.type(), evidence);
      SignatureSubstitutionVisitor v { ctx, t, evidence };
      fun.code()->accept(v);
      return { t, v.result.code() };
   }
   

   // --------------------------------
   // -- Evaluation by substitution --
   // --------------------------------

   // -- On occasions, we have closed expressions that we must
   // -- reduce to values.  And those values may be needed in
   // -- in the intermediate language.  This routine is responsible
   // -- reifying objects to their value expression forms.
   const Value*
   reify(BasicContext<>& ctx, Object obj) {
      auto env = ctx.elaborator();
      switch (obj.type()->data_traits()->mode) {
      case Data::Mode::Void:
         return nullptr;
      case Data::Mode::Bool:
         return env->build_bool(bool(obj.value()), obj.type());
      case Data::Mode::Int:
         return env->build_int(intptr_t(obj.value()), obj.type());
      case Data::Mode::Dfloat:
         return env->build_double(double(obj.value()), obj.type());
      case Data::Mode::String:
      case Data::Mode::Symbol:
         return env->build_string(obj.value(), obj.type());
      case Data::Mode::Pointer:
      default:
         if (obj.type() != env->get_typename())
            system_error("cannot reify value of type" + show(*obj.type()));
         return Data::to_type(obj.value());
      }
   }

   // There are situations were we want to evaluate certain calls
   // (notably those producing types) at elaboration time.  However,
   // on occasions some of the arguments are not ground, so normal
   // evaluation should not be attempted.  If the invoked function is simple
   // enough we can still perform evaluation by substitution.
   // This routine determines whether a function is `simple enough'.
   // More generally, it determines whether an expression is simple
   // enough to be evaluated by substitution.
   static bool is_simple(Elaboration);

   static bool
   is_simple(const Arguments& seq) {
      bool result = true;
      for (std::size_t i = 0; result and i < seq.size(); ++i)
         result = is_simple(seq[i]);
      return result;
   }

   namespace {
      struct IsSimpleVisitor : Expression::Visitor {
         bool result;
         IsSimpleVisitor() : result(false) { }

         void visit(const Expression&) { }
         void visit(const Value&) { result = true; }
         void visit(const TagType& x) {
            result = is_simple(x.type());
         }
         void visit(const ReferenceType& x) {
            result = is_simple(x.referee());
         }
         void visit(const ArrowType& x) {
            result = is_simple(x.target());
            for (std::size_t i = 0; result and i < x.arity(); ++i)
               result = is_simple(x.argument(i));
         }
         void visit(const TypeExpression& x) {
            result = is_simple(x.expr());
         }
         void visit(const ReadonlyType& x) {
            result = is_simple(x.type());
         }
         void visit(const Formal&) { result = true; }
         void visit(const LinkName&) { result = true; }
         void visit(const Read& x) { result = is_simple(x.address()); }
         void visit(const UnaryExpression& x) {
            result = is_simple(x.argument());
         }
         void visit(const BinaryExpression& x) {
            result = is_simple(x.lhs()) and is_simple(x.rhs());
         }
         void visit(const BinaryLogical& x) {
            result = is_simple(x.lhs()) and is_simple(x.rhs());
         }
         void visit(const Call& x) {
            result = is_simple(x.function()) and is_simple(x.arguments());
         }
         void visit(const Return& x) {
            result = is_simple(x.expression());
         }

         void visit(const Block& x) {
            // A block is simple only if it has at most one statement.
            result = x.size() < 2;
            for (int i = 0; result and i < x.size(); ++i)
               result = is_simple(x.statement(i));
         }
      };
   }

   static bool
   is_simple(Elaboration expr) {
      if (expr.code() == 0)
         return true;
      IsSimpleVisitor v;
      expr.code()->accept(v);
      return v.result;
   }

   // -- Simple evaluator for type expressions
   namespace {
      struct ParameterMap : std::map<Symbol, const Expression*> {
         bool is_defined_at(Symbol s) const {
            return find(s) != end();
         }

         const Expression* operator()(Symbol s) const {
            const_iterator p = find(s);
            return p == end() ? nullptr : p->second;
         }
      };
   }

   static const Type*
   simplify_type(BasicContext<>&, const Type*, const ParameterMap&);

   static Elaboration
   simplify(BasicContext<>&, Elaboration, const ParameterMap&);

   static TypeElaboration
   simplify(BasicContext<>& ctx, TypeElaboration expr,
            const ParameterMap& map) {
      const Type* t = simplify_type(ctx, expr.type(), map);
      const Type* e = simplify_type(ctx, expr.code(), map);
      return { t, e };
   }

   static const TagType*
   simplify(BasicContext<>& ctx, const TagType& t, const ParameterMap& map) {
      auto x = simplify(ctx, t.type(), map);
      return ctx.elaborator()->make_tag_type(t.tag(), x);
   }

   static const GenerativeType*
   simplify(BasicContext<>& ctx, const GenerativeType& t,
            const ParameterMap& map) {
      auto v = simplify_type(ctx, t.value(), map);
      if (v == t.value())
         return &t;
      return ctx.elaborator()->make_generative_type(t.name(), v, t.scope());
   }

   namespace {
      struct TypeEvaluator : Expression::Visitor {
         BasicContext<>& ctx;
         const ParameterMap& map;
         const Type* result;
         TypeEvaluator(BasicContext<>& c, const ParameterMap& p)
               : ctx(c), map(p), result()
         { }

         void visit(const Expression& x) {
            internal_error("missed evaluation of " + quote(show(&x)));
         }

         void visit(const BasicType& x) { result = &x; }

         void visit(const TagType& x) {
            result = simplify(ctx, x.type(), map);
         }

         void visit(const ReferenceType& x) {
            result = ctx.elaborator()->make_reference_type
               (simplify(ctx, x.referee(), map));
         }
         void visit(const RecordType& x) {
            Sequence<TagType> fields;
            for (auto f : x.components())
               fields.push_back(simplify(ctx, *f, map));
            result = ctx.elaborator()->make_record_type(fields);
         }
         void visit(const VariantType& x) {
            // FIXME: Instantiate.
            result = &x;
         }
         void visit(const ArrowType& x) {
            TypeElaboration ret = simplify(ctx, x.target(), map);
            InputTypes args(x.arity());
            for (std::size_t i = 0; i < x.arity(); ++i)
               args[i] = simplify(ctx, x.argument(i), map);
            result = ctx.elaborator()->make_arrow_type(ret, args);
         }
         void visit(const ReadonlyType& x) {
            result = ctx.elaborator()->make_readonly_type(simplify(ctx, x.type(), map));
         }
         void visit(const TypeExpression& x) {
            Elaboration expr = simplify(ctx, x.expr(), map);
            if (const Type* t = is<Type>(expr.code()))
               result = t;
            else
               result = ctx.elaborator()->make_type_expression(expr);
         }
         void visit(const QuantifiedType& x) {
            result = ctx.elaborator()->make_quantified_type
               (x.quantifier(), x.formals(),
                simplify(ctx, x.abstract_instance(), map));
         }

         void visit(const RestrictedType& x) {
            auto t = simplify(ctx, x.type(), map);
            auto c = simplify(ctx, x.condition(), map);
            if (t != x.type() or c != x.condition())
               result = ctx.elaborator()->make_restricted_type(t, c);
            else
               result = &x;
         }

         void visit(const GenerativeType& x) {
            result = simplify(ctx, x, map);
         }
      };
   }

   static const Type*
   simplify_type(BasicContext<>& ctx, const Type* t, const ParameterMap& map) {
      if (t == nullptr)
         return nullptr;
      TypeEvaluator v { ctx, map };
      t->accept(v);
      return v.result;
   }

   const Type* simplify_type(BasicContext<>& ctx, const Type* t)
   {
      return simplify_type(ctx, t, ParameterMap());
   }
   
   static LinkName
   simplify(BasicContext<>& ctx, const LinkName& n, const ParameterMap& map) {
      return { n.name(), simplify_type(ctx, n.type(), map) };
   }

   static Elaboration
   simplify(BasicContext<>&, Elaboration, const ParameterMap&);

   static const Expression*
   simplify_call(BasicContext<>& ctx, const Constructor* ctor,
                 const Arguments& args, const ParameterMap& map) {
      auto ft = is<ArrowType>(ctor->type());
      if (ft == nullptr)
         internal_error("call to enumerator " + quote(show(ctor->name())));
      const std::size_t nargs = args.size();
      Arguments xargs(nargs);
      Sequence<Value> new_args(nargs);
      bool all_values = true;
      for (std::size_t i = 0; i < nargs; ++i) {
         Elaboration x = simplify(ctx, args[i], map);
         if (auto v = is<Value>(x.code()))
            new_args[i] = v;
         else if (all_values)
            all_values = false;
         xargs[i] = x;
      }
      if (not all_values)
         return ctx.elaborator()->build_call({ ft, ctor }, xargs);
      return ctx.elaborator()->build_instance(ctor, new_args);
   }

   static Elaboration
   read_component(const Component* x) {
      if (auto ns = is<Namespace>(x->whole())) {
         auto d = ns->select(x->name(), x->type());
         if (d != nullptr and d->value().code() != nullptr)
            return d->value();
      }
      return { x->type(), x };
   }
      
   namespace {
   }

   static Elaboration
   simplify_lambda_call(BasicContext<>& ctx, const Lambda* f, const Arguments& x,
                        const ParameterMap& map) {
      ParameterMap new_map;
      const auto nargs = x.size();
      for (std::size_t i = 0; i < nargs; ++i)
         new_map[f->parameter(i)->symbol()] =
            simplify(ctx, x.at(i), map).code();
      try {
         return simplify(ctx, f->body(), new_map);
      }
      catch(ReturnJump<Elaboration> ret) {
         auto result = ret.value;
         if (is_closed(result))
            result = reify(ctx, ctx.evaluator()->eval(result));
         return result;
      }
   }

   namespace {
      struct SimpleEvalVisitor : Expression::Visitor {
         BasicContext<>& ctx;
         const ParameterMap& map;
         Elaboration result;
         SimpleEvalVisitor(BasicContext<>& c, Elaboration e,
                           const ParameterMap& m)
               : ctx(c), map(m), result(e) { }

         void visit(const Expression& x) {
            // Conservatively assume that the result would  be `expr'.
            result = &x;
         }

         void visit(const Type& x) {
            result = simplify_type(ctx, &x, map);
         }

         void visit(const Component& x) {
            auto whole = simplify(ctx, x.whole(), map);
            auto lnk = simplify(ctx, x.link_name(), map);
            result = ctx.elaborator()->build_component(whole, lnk);
         }

         void visit(const Read& x) {
            auto env = ctx.elaborator();
            Elaboration loc = simplify(ctx, x.address(), map);
            if (auto ref = is<Component>(loc))
               result = read_component(ref);
            else if (auto lnk = is<LinkName>(loc)) {
               auto type_eq = [=](const Declaration& x) {
                  return x.value().type() == lnk->type();
               };
               if (map.is_defined_at(lnk->symbol()))
                  result = map(lnk->symbol());
               else if (auto d = lexical_lookup(*env, lnk->name()).select_if(type_eq))
                  result = d->value().code();
            }
            else if (loc != x.address())
               result = env->build_read(loc);
         }
         
         void visit(const Negate& x) {
            result = ctx.elaborator()->build_negate
                (x.function(), simplify(ctx, x.argument(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }
         
         void visit(const Not& x) {
            result = ctx.elaborator()->build_not
                (x.function(), simplify(ctx, x.argument(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }
         
         void visit(const Complement& x) {
            result = ctx.elaborator()->build_complement
                (x.function(), simplify(ctx, x.argument(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }
         
         void visit(const Plus& x) {
            result = ctx.elaborator()->build_plus
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Dash& x) {
            result = ctx.elaborator()->build_dash
               (x.function(), simplify(ctx, x.lhs(), map),
                simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Star& x) {
            result = ctx.elaborator()->build_star
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Slash& x) {
            result = ctx.elaborator()->build_slash
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Div& x) {
            result = ctx.elaborator()->build_div
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Quo& x) {
            result = ctx.elaborator()->build_quo
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Mod& x) {
            result = ctx.elaborator()->build_mod
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Rem& x) {
            result = ctx.elaborator()->build_rem
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Langle& x) {
            result = ctx.elaborator()->build_langle
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Rangle& x) {
            result = ctx.elaborator()->build_rangle
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Langleq& x) {
            result = ctx.elaborator()->build_langleq
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Rangleq& x) {
            result = ctx.elaborator()->build_rangleq
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Eqeq& x) {
            result = ctx.elaborator()->build_eqeq
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Excleq& x) {
            result = ctx.elaborator()->build_excleq
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const And& x) {
            result = ctx.elaborator()->build_and
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const Or& x) {
            result = ctx.elaborator()->build_or
                (x.function(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
            if (is_closed(result))
               result = reify(ctx, ctx.evaluator()->eval(result));
         }

         void visit(const BinaryLogical& x) {
            result = ctx.elaborator()->build_logical
                (x.operation(), simplify(ctx, x.lhs(), map),
                 simplify(ctx, x.rhs(), map));
         }
         
         void visit(const Call& x) {
            auto fun = simplify(ctx, x.function(), map);
            if (auto f = is<Constructor>(fun)) {
               result = simplify_call(ctx, f, x.arguments(), map);
               return;
            }
            auto f = is<Lambda>(fun);
            if (f == nullptr or not is_simple({ f->type(), f })
                or f->body() == Elaboration())
               // FIXME: Maybe we should reduce arguments first.
               return;

            result = simplify_lambda_call(ctx, f, x.arguments(), map);
         }
         
         void visit(const Return& x) {
            throw ReturnJump<Elaboration>
               (simplify(ctx, x.expression(), map));
         }

         void visit(const Loop& x) {
            try {
               result = simplify(ctx, x.body(), map);
            }
            catch(LeaveJump<Elaboration> e) {
               result = e.value;
            }
         }
         
         void visit(const Block& x) {
            // Note: We get here only when `x' has at most one statement.
            for (int i = 0; i < x.size(); ++i)
               result = simplify(ctx, x.statement(i), map);
         }
      };
   }
      
   static Elaboration
   simplify(BasicContext<>& ctx, Elaboration expr,
            const ParameterMap& map) {
      const Type* t = simplify_type(ctx, expr.type(), map);
      if (expr.code() == nullptr)
         return { t, nullptr };
      SimpleEvalVisitor v { ctx, { t, expr.code() }, map };
      expr.code()->accept(v);
      if (not is_closed(v.result))
         v.result = eq_close_term_if_can(ctx.elaborator()->eq_classes(), v.result);
      return v.result;
   }
   
   Elaboration
   evaluate(BasicContext<>& ctx, Elaboration expr) {
      return simplify(ctx, expr, ParameterMap());
   }
      
   // --------------------------------
   // -- Elaboration simplification --
   // --------------------------------

   static bool
   is_closed(const Arguments& seq) {
      bool result = true;
      for (std::size_t i = 0; result and i < seq.size(); ++i)
         result = is_closed(seq[i]);
      return result;
   }
   
   namespace {
      struct IsClosedVisitor : Expression::Visitor {
         bool result;
         IsClosedVisitor() : result(false) { }

         void visit(const Expression&) { }
         void visit(const Value&) { result = true; }
         void visit(const NiladicBuiltinFunction&) { result = true; }
         void visit(const BinaryBuiltinFunction&) { result = true; }
         void visit(const Type&) { result = true; }
         void visit(const ReferenceType& x) {
            result = is_closed(x.referee());
         }
         void visit(const ArrowType& x) {
            result = is_closed(x.target());
            for (std::size_t i = 0; i < x.arity() and result; ++i)
               result = is_closed(x.argument(i));
         }
         void visit(const ReadonlyType& x) {
            result = is_closed(x.type());
         }
         void visit(const TypeExpression& x) {
            result = is_closed(x.expr());
         }
         void visit(const Lambda& x) {
            // We consider an undefined function as not closed.
            // FIXME: do we treat captures?
            result = x.body() != Elaboration();
         }

         void visit(const UnaryExpression& x) {
            result = is_closed(x.argument());
         }

         void visit(const BinaryExpression& x) {
            result = is_closed(x.lhs()) and is_closed(x.rhs());
         }

         void visit(const BinaryLogical& x) {
            result = is_closed(x.lhs()) and is_closed(x.rhs());
         }

         void visit(const Call& x) {
            result = is_closed(x.function()) and is_closed(x.arguments());
         }
      };
   }

   bool
   is_closed(Elaboration expr) {
      if (expr.code() == nullptr)
         return true;
      IsClosedVisitor v;
      expr.code()->accept(v);
//       std::cerr << "\nexpression " << quote(show(expr))
//                 << " is closed: " << std::boolalpha
//                 << v.result;
      return v.result;
   }
}
