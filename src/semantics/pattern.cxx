// Copyright (C) 2010-2012, Texas A&M University.
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

#include "Elaborator.H"

namespace liz {
   // ---------------------------------------------
   // -- Pattern matching of general expressions --
   // ---------------------------------------------
   static bool
   pattern_match(Elaboration, const Expression*, Substitution&);

   namespace {
      template<typename T>
      const T* get(Elaboration expr, Substitution& subst) {
         if (const T* t = is<T>(expr.code()))
            return t;
         subst.set_failed();
         return nullptr;
      }

      template<typename T>
      static bool
      pattern_match_sequence(const std::vector<T>& expr,
                             const std::vector<T>& pat,
                             Substitution& subst) {
         const std::size_t size = expr.size();
         if (size == pat.size()) {
            bool keep_going = true;
            for (std::size_t i = 0; keep_going and i < size; ++i)
               keep_going =
                  pattern_match(expr[i], pat[i].code(), subst);
         }
         else
            subst.set_failed();
         return subst;
      }

      static bool
      pattern_match_arguments(const Call* expr, const Call* pat,
                              Substitution& subst) {
         const int nargs = expr->argument_count();
         if (nargs == pat->argument_count()) {
            bool keep_going = true;
            for (int i = 0; keep_going and i < nargs; ++i)
               keep_going =
                  pattern_match(expr->argument(i), pat->argument(i).code(), subst);
         }
         else
            subst.set_failed();
         return subst;
      }
      
      struct PatternMatchVisitor : Expression::Visitor {
         Substitution& subst;   // matching substitution constructed so far
         Elaboration expr; // expression we are matching against
         PatternMatchVisitor(Substitution& s, Elaboration x)
               : subst(s), expr(x) { }

         void visit(const Expression& p) {
            // Conversatively failed for expressions we don't match yet.
            if (&p != expr.code())
               subst.set_failed();
         }

         void visit(const Formal& p) {
            // FIXME: This is extremely low-level, unstructured
            std::pair<Substitution::iterator, bool> r =
               subst.insert(std::make_pair(&p, expr));
            if (not r.second and r.first->second.code() != expr.code())
               subst.set_failed();
         }

         void visit(const ReferenceType& p) {
            if (auto x = get<ReferenceType>(expr, subst))
               pattern_match(x->referee(), p.referee().code(), subst);
         }

         void visit(const ArrowType& p) {
            if (auto x = get<ArrowType>(expr, subst))
               pattern_match(x->target(), p.target().code(), subst)
                  and pattern_match_sequence(x->source(), p.source(), subst);
         }

         void visit(const ReadonlyType& p) {
            if (auto x = get<ReadonlyType>(expr, subst))
               pattern_match(x->type(), p.type().code(), subst);
         }

         void visit(const TypeExpression& p) {
            if (auto x = is<TypeExpression>(expr.code()))
               pattern_match(x->expr(), p.expr().code(), subst);
            else
               pattern_match(expr, p.expr().code(), subst);
         }

         void visit(const UnaryExpression& p) {
            if (auto x = get<UnaryExpression>(expr, subst)) {
               pattern_match(x->function(), p.function().code(), subst)
                  and pattern_match(x->argument(),
                                    p.argument().code(), subst);
            }
         }

         void visit(const BinaryExpression& p) {
            if (auto x = get<BinaryExpression>(expr, subst)) {
               pattern_match(x->function(), p.function().code(), subst)
                  and pattern_match(x->lhs(), p.lhs().code(), subst)
                  and pattern_match(x->rhs(), p.rhs().code(), subst);
            }
         }

         void visit(const Call& p) {
            if (auto x = get<Call>(expr, subst)) {
               pattern_match(x->function(), p.function().code(), subst)
                  and pattern_match_arguments(x, &p, subst);
            }
         }
      };
   }


   static bool
   pattern_match(Elaboration expr, const Expression* pat,
                 Substitution& subst) {
      // 1. Get out of here if arguments are identical.
      if (expr.code() == pat or subst.failed())
         return subst;

      // 2. Desconstruct the pattern and the expression,
      //    binding formal variables as appropriate.
      PatternMatchVisitor v(subst, expr);
      pat->accept(v);
      return subst;
   }

   Substitution
   pattern_match(Elaboration expr, const Expression* pat) {
      Substitution subst;
      pattern_match(expr, pat, subst);
      return subst;
   }


   // -------------------------------------------
   // -- Template Argument deduction machinery --
   // -------------------------------------------

   // -- Return true if the expression `expr' uses templates
   // -- that are in deducible context.
   static bool has_deducible_formals(Elaboration);

   namespace {
      struct DeducibleFormalVisitor : Expression::Visitor {
         bool result;
         DeducibleFormalVisitor() : result(false) { }

         void visit(const Expression&) { }

         void visit(const ReferenceType& x) {
            result = has_deducible_formals(x.referee());
         }

         void visit(const ArrowType& x) {
            if (has_deducible_formals(x.target()))
               result = true;
            for (std::size_t i = 0; i < x.arity(); ++i)
               if (has_deducible_formals(x.argument(i)))
                  result = true;
         }

         void visit(const ReadonlyType& x) {
            result = has_deducible_formals(x.type());
         }

         void visit(const TypeExpression& x) {
            result = has_deducible_formals(x.expr());
         }

         void visit(const Formal&) { result = true; }
      };
   }

   static bool
   has_deducible_formals(Elaboration expr) {
      DeducibleFormalVisitor v;
      if (expr.code() != nullptr)
         expr.code()->accept(v);
      return v.result;
   }

   // We are in the process of elaborating a call to a function template.
   // We already elaborated the argument list `args'.  We want to
   // determine values for the template parameters for the purpose of
   // instantiation.  'ttype' is the type of the function template.
   // Substitution
   // deduce_template_arguments(Elaborator* context, const GenericType* ttype,
   //                           const std::vector<Elaboration>& args) {
   //    const ArrowType* ftype = is<ArrowType>(ttype->result());
   //    Substitution subst;
   //    const Type* t = context->get_typename();
   //    for (std::size_t i = 0; subst and i < ftype->arity(); ++i) {
   //       TypeElaboration arg_type(t, args[i].type());
   //       TypeElaboration pattern = ftype->argument(i);
   //       // Follow C++'s tradition in stripping out toplevel reference
   //       // form argument type.
   //       if (const ReferenceType* ref = is<ReferenceType>(arg_type.code())) {
   //          if (not is<ReferenceType>(pattern))
   //             arg_type = ref->referee();
   //       }
   //       // Try deduction only in deducible contexts.
   //       if (has_deducible_formals(pattern))
   //          pattern_match(arg_type, pattern.code(), subst);
   //    }

   //    return subst;
   // }

   template<typename T>
   static const T*
   same_sort(Elaboration e, const T&, UnificationContext& uc) {
      struct V : Expression::Visitor {
         UnificationContext& uc;
         const T* result;
         V(UnificationContext& c) : uc(c), result() { }
         void visit(const Expression&) { uc.subst.set_failed(); }
         void visit(const T& x) { result = &x; }
      };

      V  v { uc };
      e.code()->accept(v);
      return v.result;
   }


   static bool match(Elaboration, const Expression*, UnificationContext&);

   template<typename T>
   static bool
   match(const vector<T>& exprs, const vector<T>& pats,
         UnificationContext& uc) {
      const auto n = exprs.size();
      if (n == pats.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match(exprs[i], pats[i], uc);
      }
      else
         uc.subst.set_failed();
      return not uc.subst.failed();
   }

   static bool
   match(const TagType* t, const TagType* x, UnificationContext& uc) {
      if (t->tag() != x->tag())
         return uc.subst.set_failed();
      return match(t->type(), x->type(), uc);
   }

   static bool
   match(Elaboration expr, const Expression* pat, UnificationContext& uc) {
      struct V : Expression::Visitor {
         UnificationContext& uc;
         Elaboration expr;
         V(UnificationContext& c, Elaboration e) : uc(c), expr(e) { }

         void visit(const Expression& x) {
            internal_error("match: " + quote(typeid(x).name()));
         }

         void visit(const Formal& x) {
            auto p = &x;
            if (uc.unifiable(p))
               uc.subst[p] = expr;
         }

         void visit(const TagType& x) {
            if (auto tg = same_sort(expr, x, uc))
               match(tg->type(), x.type(), uc);
         }

         void visit(const ReferenceType& x) {
            if (auto rt = same_sort(expr, x, uc))
               match(rt->referee(), x.referee(), uc);
         }

         void visit(const ArrowType& x) {
            if (auto at = same_sort(expr, x, uc))
               match(at->source(), x.source(), uc)
                  and match(at->target(), x.target(), uc);
         }

         void visit(const RecordType& x) {
            if (auto rt = same_sort(expr, x, uc))
               match(rt->components(), x.components(), uc);
         }

         void visit(const ReadonlyType& x) {
            if (auto rt = same_sort(expr, x, uc))
               match(rt->type(), x.type(), uc);
         }

         void visit(const RestrictedType& x) {
            if (auto rt = same_sort(expr, x, uc))
               match(rt->type(), x.type(), uc)
                  and match(rt->condition(), x.condition().code(), uc);
         }

         void visit(const TypeExpression& x) {
            if (auto e = is<Read>(x.expr()))
               e->address().code()->accept(*this);
            else
               uc.subst.set_failed();
         }

         void visit(const ProductType& x) {
            if (auto pt = same_sort(expr, x, uc))
               match(pt->source(), x.source(), uc)
                  and match(pt->target(), x.target(), uc);
         }
      };

      V v { uc, expr };
      pat->accept(v);
      return bool(v.uc.subst);
   }

   bool
   UnificationContext::match_type(TypeElaboration t, const Type* x) {
      return match(t, x, *this);
   }
}
