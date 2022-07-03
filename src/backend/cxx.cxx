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

#include "cxx.H"

namespace liz {
   namespace cxx {
      // -- Types --
      NamedType::NamedType(Symbol n)
            : structure::unary<Symbol>(n)
      { }

      ExprAsType::ExprAsType(const Expr* e)
            : structure::unary<const Expr*>(e)
      { }

      ConstType::ConstType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      RefType::RefType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      PtrType::PtrType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      ArrayType::ArrayType(const Type* t, offset_type n)
            : structure::binary<const Type*, offset_type>(t, n)
      { }

      FunType::FunType(const Sequence<Type>& s, const Type* t)
            : structure::binary<Sequence<Type>, const Type*>(s, t)
      { }

      AnonType::AnonType(const Type* t, const Sequence<Decl>& s)
            : structure::binary<const Type*, Sequence<Decl>>(t, s)
      { }

      // -- Expressions --
      IdExpr::IdExpr(Symbol n)
            : structure::unary<Symbol>(n)
      { }

      InstExpr::InstExpr(const Expr* t, const Sequence<Expr>& x)
            : structure::binary<const Expr*, Sequence<Expr>>(t, x)
      { }

      DotExpr::DotExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      ArrowExpr::ArrowExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      ScopeExpr::ScopeExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      SubscriptExpr::SubscriptExpr(const Expr* x, const Expr* y)
            : structure::binary<const Expr*>(x, y)
      { }

      CallExpr::CallExpr(const Expr* x, const Sequence<Expr>& y)
            : structure::binary<const Expr*, Sequence<Expr>>(x, y)
      { }

      ObjectExpr::ObjectExpr(const Type* t, const Sequence<Expr>& x)
            : structure::binary<const Type*, Sequence<Expr>>(t, x)
      { }

      PostIncrementExpr::PostIncrementExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      PostDecrementExpr::PostDecrementExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      DerefExpr::DerefExpr(const Expr* e)
            : structure::unary<const Expr*>(e)
      { }

      NegExpr::NegExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      NotExpr::NotExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }
      
      ComplExpr::ComplExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }
      
      // -- Statements --
      LabeledStmt::LabeledStmt(Symbol l, const Stmt* s)
            : structure::binary<Symbol, const Stmt*>(l, s)
      { }

      CaseStmt::CaseStmt(const Expr* x, const Stmt* s)
            : structure::binary<const Expr*, const Stmt*>(x,s)
      { }

      DefaultStmt::DefaultStmt(const Stmt* s)
            : structure::unary<const Stmt*>(s)
      { }

      ExprStmt::ExprStmt(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      CompoundStmt::CompoundStmt(const Sequence<Stmt>& seq)
            : structure::unary<Sequence<Stmt>>(seq)
      { }

      IfStmt::IfStmt(const Expr*x, const Stmt* t, const Stmt* f)
            : structure::ternary<const Expr*, const Stmt*>(x, t, f)
      { }

      SwitchStmt::SwitchStmt(const Expr* x, const Stmt* s)
            : structure::binary<const Expr*, const Stmt*>(x, s)
      { }

      WhileStmt::WhileStmt(const Expr* x, const Stmt* s)
            : structure::binary<const Expr*, const Stmt*>(x, s)
      { }

      ReturnStmt::ReturnStmt(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      GotoStmt::GotoStmt(Symbol l)
            : structure::unary<Symbol>(l)
      { }

      DeclStmt::DeclStmt(const Decl* d)
            : structure::unary<const Decl*>(d)
      { }

      // -- Declarations --
      VarDecl::VarDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      VarDef::VarDef(Symbol n, const Type* t, const Expr* x)
            : structure::ternary<Symbol, const Type*, const Expr*>(n, t, x)
      { }

      FieldDecl::FieldDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      Parm::Parm(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }
      
      FunDecl::FunDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      Abstraction::Abstraction(const Sequence<Parm>& p, const Sequence<Stmt>& s)
            : structure::binary<Sequence<Parm>, CompoundStmt>(p, s)
      { }

      FunDef::FunDef(Symbol n, const Type* t, const Abstraction& x)
            : structure::ternary<Symbol, const Type*, Abstraction>(n, t, x)
      { }

      ScopeDef::ScopeDef(Symbol s) : n(s) { }

      StructDef::StructDef(Symbol s) : n(s) { }

      UnionDef::UnionDef(Symbol s) : n(s) { }

      TmpltDecl::TmpltDecl(const Sequence<Decl>& s, const Decl* d)
            : structure::binary<Sequence<Decl>, const Decl*>(s, d)
      { }

      // -- Decl::Visitor --
      void
      Decl::Visitor::visit(const Parm& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const VarDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const VarDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FieldDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FunDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FunDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const ScopeDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const StructDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const UnionDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const TmpltDecl& d) {
         visit(as<Decl>(d));
      }

      Factories::Factories()
            : false_cst(false),
              true_cst(true),
              brk_stmt(),
              cont_stmt()
      { }
      
   }

   // -- Backend --
   
   // Return the cached C++ translation of a Liz type.
   const cxx::Type*
   CxxBackend::lookup(const liz::Type* t) const {
      Dict::const_iterator p = dict.find(t);
      return p == dict.end() ? 0 : p->second;
   }

   // Translate a Liz type expression to its C++ equivalent.
   // Look first for in the cache; then do the actual translation
   // if this is the first time.
   const cxx::Type*
   CxxBackend::translate(const liz::Type* t) {
      struct V : liz::Expression::Visitor {
         CxxBackend& cg;
         const cxx::Type* result;
         V(CxxBackend& cg) : cg(cg), result() { }
         
         void visit(const liz::Expression& x) {
            internal_error("cxx::translate: unexpected IL "
                            + quote(show(&x)));
         }
         
         void visit(const liz::Type& t) {
            internal_error("cxx::translate: cannot translate Liz type "
                           + quote(show(t)) + " to C++ type");
         }
         
         void visit(const liz::BasicType& t) {
            result = cg.depot.named_type.make(t.name()->symbol());
         }
         
         void visit(const liz::GenerativeType& t) {
            result = cg.depot.named_type.make(t.name()->symbol());
         }
         
         void visit(const liz::TypeExpression& t) {
            result = cg.depot.expr_type.make
               (cg.translate_expr(t.expr().code()));
         }
         
         void visit(const liz::ReferenceType& t) {
            result = cg.depot.ref_type.make(cg.translate(t.referee()));
         }
         
         void visit(const liz::ArrowType& t) {
            Sequence<cxx::Type> src(t.arity());
            for (std::size_t i = 0; i < t.arity(); ++i)
               src[i] = cg.translate(t.argument(i));
            result = cg.depot.fun_type.make(src, cg.translate(t.target()));
         }
         
         void visit(const liz::ReadonlyType& t) {
            result = cg.depot.const_type.make(cg.translate(t.type()));
         }
         
         void visit(const liz::RestrictedType& t) {
            result = cg.translate(t.type());
         }
      };
      
      if (const cxx::Type* x = lookup(t))
         return x;
      V v(*this);
      t->accept(v);
      return dict[t] = v.result;
   }
   
   CxxBackend::CxxBackend(Elaborator& c) : comp(c) { }
   
   const cxx::Expr*
   CxxBackend::translate_name(const liz::Name* x) {
      struct V : liz::Name::Visitor {
         CxxBackend& cg;
         const cxx::Expr* result;
         V(CxxBackend& cg) : cg(cg), result() { }
         
         void visit(const liz::Identifier& x) {
            result = cg.depot.id_expr.make(x.symbol());
         }
         void visit(const liz::Operator& x) {
            result = cg.depot.id_expr.make
               (cg.comp.intern("operator" + x.symbol().string()));
         }
         void visit(const liz::Literal& x) {
            result = cg.depot.id_expr.make
               (cg.comp.intern("_" + x.symbol().string()));
            }
      };
      
      V v(*this);
      x->accept(v);
      return v.result;
   }

   // Build a C++ binary expression from a Liz binary expression.
   // FIXME: find better expression of this mappinng.
   template<typename S, typename T>
   static const cxx::Expr*
   build_binary_expr(CxxBackend& be, Factory<S>& f,
                     const binary_impl<T>& x) {
      const cxx::Expr* op = be.translate_expr(x.function().code());
      const cxx::Expr* a = be.translate_expr(x.lhs().code());
      const cxx::Expr* b = be.translate_expr(x.rhs().code());
      return f.make(op, a, b);
   }
   
   const cxx::Expr*
   CxxBackend::translate_expr(const Expression* x) {
      struct V : Expression::Visitor {
         CxxBackend& cg;
         const cxx::Expr* result;
         V(CxxBackend& cg) : cg(cg), result() { }
         
         void visit(const Expression& x) {
            internal_error("cxx::translate_expr: missed IL "
                           + quote(show(&x)));
         }
         
         void visit(const Bool& x) {
            result = x.rep() ? &cg.depot.true_cst : &cg.depot.false_cst;
         }
         
         void visit(const Char& x) {
            result = cg.depot.char_cst.make(x.rep());
         }
         
         void visit(const Int& x) {
            result = cg.depot.int_cst.make(x.rep());
         }
         
         void visit(const Double& x) {
            result = cg.depot.double_cst.make(x.rep());
         }
         
         void visit(const String& x) {
            result = cg.depot.string_cst.make(x.rep().string());
         }
         
         void visit(const Formal& x) {
            auto n = x.name();
            if (n == nullptr)
               internal_error("cxx::translate_expr: use of unnamed parameter");
            result = cg.depot.id_expr.make(n->symbol());
         }
         
         void visit(const LinkName& x) {
            result = cg.translate_name(x.name());
         }
         
         void visit(const RecordType& x) {
            auto region = cg.depot.region_expr.make();
            for (auto f : x.components())
               region->push_back(cg.translate_decl(f));
            result = region;
         }
         
         void visit(const VariantType& v) {
            auto region = cg.depot.region_expr.make();
            for (auto ctor : v.constructors())
               region->push_back(cg.translate_ctor(ctor));
            result = region;
         }
         
         void visit(const liz::Namespace& x) {
            if (auto n = x.name())
               result = cg.translate_name(n);
            else
               result = cg.depot.id_expr.make(Symbol());
         }
         
         void visit(const liz::TypeExpression& x) {
            result = cg.translate_expr(x.expr().code());
         }
         
         void visit(const Lambda& x) {
            result = cg.translate_name(x.name());
         }
         
         void visit(const liz::Component& x) {
            const cxx::Expr* obj = cg.translate_expr(x.whole().code());
            auto member = x.name()->symbol();
            result = cg.depot.scope_expr.make(obj, member);
         }
         
         void visit(const liz::DotSelection& x) {
            const cxx::Expr* obj = cg.translate_expr(x.whole().code());
            result = cg.depot.dot_expr.make(obj, x.name()->symbol());
         }
         
         void visit(const liz::Call& x) {
            const cxx::Expr* fun = cg.translate_expr(x.function().code());
            Sequence<cxx::Expr> args;
            for (auto& a : x.arguments())
               args.push_back(cg.translate_expr(a.code()));
            result = cg.depot.call_expr.make(fun, args);
         }
         
         void visit(const liz::Read& x) {
            // FIXME: lvalue-to-rvalue conversion is implicit in C++.
            // FIXME: we could attempt to make it explicit via
            // FIXME: static_cast but that will too much verbosity.
            result = cg.translate_expr(x.address().code());
         }

         void visit(const liz::Negate& x) {
            const cxx::Expr* e = cg.translate_expr(x.argument().code());
            result = cg.depot.neg_expr.make(e);
         }
         
         void visit(const liz::Not& x) {
            const cxx::Expr* e = cg.translate_expr(x.argument().code());
            result = cg.depot.not_expr.make(e);
         }
         
         void visit(const liz::Complement& x) {
            const cxx::Expr* e = cg.translate_expr(x.argument().code());
            result = cg.depot.compl_expr.make(e);
         }
         
         void visit(const liz::Plus& x) {
            result = build_binary_expr(cg, cg.depot.plus_expr, x);
         }
         
         void visit(const liz::Dash& x) {
            result = build_binary_expr(cg, cg.depot.minus_expr, x);
         }
         
         void visit(const liz::Star& x) {
            result = build_binary_expr(cg, cg.depot.mult_expr, x);
         }
         
         void visit(const liz::Slash& x) {
            result = build_binary_expr(cg, cg.depot.div_expr, x);
         }
         
         void visit(const liz::Div& x) {
            result = build_binary_expr(cg, cg.depot.div_expr, x);
         }
         
         void visit(const liz::Rem& x) {
            result = build_binary_expr(cg, cg.depot.rem_expr, x);
         }
         
         void visit(const liz::Mod& x) {
            result = build_binary_expr(cg, cg.depot.mod_expr, x);
         }
         
         void visit(const liz::Langle& x) {
            result = build_binary_expr(cg, cg.depot.lt_expr, x);
         }
         
         void visit(const liz::Rangle& x) {
            result = build_binary_expr(cg, cg.depot.gt_expr, x);
         }
         
         void visit(const liz::Langleq& x) {
            result = build_binary_expr(cg, cg.depot.le_expr, x);
         }
         
         void visit(const liz::Rangleq& x) {
            result = build_binary_expr(cg, cg.depot.ge_expr, x);
         }
         
         void visit(const liz::Eqeq& x) {
            result = build_binary_expr(cg, cg.depot.eq_expr, x);
         }
         
         void visit(const liz::Excleq& x) {
            result = build_binary_expr(cg, cg.depot.neq_expr, x);
         }
         
         void visit(const liz::And& x) {
            result = build_binary_expr(cg, cg.depot.and_expr, x);
         }
         
         void visit(const liz::Or& x) {
            result = build_binary_expr(cg, cg.depot.or_expr, x);
         }
      };
      
      if (x == nullptr)
         return nullptr;
      
      V v(*this);
      x->accept(v);
      return v.result;
   }
   
   const cxx::Stmt*
   CxxBackend::translate_stmt(const liz::Expression* x) {
      struct V : liz::Expression::Visitor {
         CxxBackend& cg;
         const cxx::Stmt* result;
         V(CxxBackend& cg) : cg(cg), result() { }
         
         void visit(const liz::Expression& x) {
            const cxx::Expr* e = cg.translate_expr(&x);
            result = cg.depot.expr_stmt.make(e);
         }
         
         void visit(const liz::Return& x) {
            const cxx::Expr* e = cg.translate_expr(x.expression().code());
            result = cg.depot.ret_stmt.make(e);
         }
         
         void visit(const liz::If& x) {
            const cxx::Expr* c = cg.translate_expr(x.condition().code());
            const cxx::Stmt* t = cg.translate_stmt(x.consequence().code());
            const cxx::Stmt* f = cg.translate_stmt(x.alternative().code());
            result = cg.depot.if_stmt.make(c, t, f);
         }
         
         void visit(const liz::Block& x) {
            Sequence<cxx::Stmt> stmts;
            for (int i = 0; i < x.size(); ++i)
               stmts.push_back(cg.translate_stmt(x.statement(i).code()));
            result = cg.depot.cmpd_stmt.make(stmts);
         }
      };
      
      if (x == nullptr)
         return nullptr;
      V v(*this);
      x->accept(v);
      return v.result;
   }
   
   Sequence<cxx::Parm>
   CxxBackend::translate(const liz::Formals& formals) {
      Sequence<cxx::Parm> parms;
      for (std::size_t i = 0; i < formals.size(); ++i) {
         auto f = formals[i];
         const cxx::Type* t = translate(f->type());
         parms.push_back(depot.parm_decl.make(f->name()->symbol(), t));
      }
      return parms;
   }
   
   const cxx::Decl*
   CxxBackend::translate_def(Symbol n, const liz::RecordType* s) {
      cxx::StructDef* def = depot.struct_def.make(n);
      for (auto f : s->components())
         def->push_back(translate_decl(f));
      return def;
   }
   
   const cxx::Decl*
   CxxBackend::translate_scope_def(const liz::Namespace* s) {
      cxx::ScopeDef* ns = depot.scope_def.make(s->name()->symbol());
      for (std::size_t i = 0; i < s->stmt_count(); ++i)
         ns->push_back(translate_decl(s->statement(i).code()));
      return ns;
   }
   
   const cxx::Decl*
   CxxBackend::translate_template(const liz::Lambda* fun) {
      const std::size_t n = fun->arity();
      Sequence<cxx::Decl> parms(n);
      for (std::size_t i = 0; i < n; ++i)
         parms[i] = translate_decl(fun->parameter(i));
      const cxx::Decl* decl = translate_decl(fun->body().code());
      return depot.tmplt_decl.make(parms, decl);
   }
   
   // If `ctor' is a non-enumeration constant, return its
   // C++ translation.
   const cxx::Decl*
   CxxBackend::translate_ctor(const liz::Constructor* ctor) {
      auto ft = is<ArrowType>(ctor->type());
      if (ft == nullptr)
         return nullptr;
      else if (ft->arity() == 1) {
         Symbol name = ctor->name()->symbol();
         const cxx::Type* type = translate(ft->argument(0));
         return depot.fld_decl.make(name, type);
      }
      else {
         const std::size_t n = ft->arity();
         Sequence<cxx::Decl> fields(n);
         for (std::size_t j = 0; j < n; ++j) {
            Symbol name = comp.intern("liz_field_" + show(j));
            const cxx::Type* type = translate(ft->argument(j));
            fields[j] = depot.fld_decl.make(name, type);
         }
         Symbol name = ctor->name()->symbol();
         const cxx::Type* type = depot.anon_type.make
            (translate(comp.get_typename()), fields);
         return depot.fld_decl.make(name, type);
      }
   }
   
   const cxx::Decl*
   CxxBackend::translate_variant_def(Symbol n, const VariantType* v) {
      // 1. Translate all non-constant contructors into fields
      //    of a union type.  These fields are generally structures
      //    unless the construcor it unary, in which case we use the
      //    parameter type directly.
      cxx::UnionDef* fields = depot.union_def.make(Symbol());
      for (auto c : v->constructors())
         if (auto d = translate_ctor(c))
            fields->push_back(d);
      // 2. contruct the tag decl for the whole structure and add
      //    the anonymous union declaration.
      cxx::StructDef* result = depot.struct_def.make(n);
      Symbol tag = comp.intern("liz_tag");
      const cxx::Type* tag_type = translate(comp.get_int());
      result->push_back(depot.fld_decl.make(tag, tag_type));
      result->push_back(fields);
      return result;
   }
   
   const cxx::Decl*
   CxxBackend::translate_def(const liz::GenerativeType* x) {
      struct V : liz::Expression::Visitor {
         CxxBackend* be;
         const Symbol name;
         const cxx::Decl* result;

         V(CxxBackend* b, Symbol n) : be(b), name(n), result() { }
         
         void visit(const liz::Expression& x) {
            internal_error("cxx::translate_def: unexpected IL "
                           + quote(show(&x)));
         }
         
         void visit(const liz::RecordType& t) {
            result = be->translate_def(name, &t);
         }
      };
      
      V v(this, x->name()->symbol());
      x->value()->accept(v);
      return v.result;
   }
   
   // Generate the C++ definition for a Liz function `fun', with
   // equivalent C++ tyep `t'.
   const cxx::Decl*
   CxxBackend::translate_fundef(const liz::Lambda* fun, const cxx::Type* t) {
      const liz::Expression* expr = fun->body().code();
      Sequence<cxx::Parm> parms = translate(fun->formals());
      Sequence<cxx::Stmt> stmts;
      if (auto blk = is<liz::Block>(expr)) {
         for (int i = 0; i < blk->size(); ++i)
            stmts.push_back(translate_stmt(blk->statement(i).code()));
      }
      else {
         const cxx::Expr* e = translate_expr(expr);
         stmts.push_back(depot.ret_stmt.make(e));
      }
      cxx::Abstraction abs(parms, stmts);
      return depot.fun_def.make(fun->name()->symbol(), t, abs);
   }
   
   const cxx::Decl*
   CxxBackend::translate_decl(const liz::TagType* t) {
      return depot.fld_decl.make(t->tag()->symbol(), translate(t->type()));
   }
   
   const cxx::Decl* CxxBackend::lower_stmt(Elaboration e) {
      return translate_decl(e.code());
   }

   void CxxBackend::format(const cxx::Decl* x, std::ostream& os) {
      cxx::format_toplevel(x, os);
   }

   const char* CxxBackend::path_extension() const { return "cxx"; }
   
   const cxx::Decl*
   CxxBackend::translate_decl(const liz::Expression* x) {
      struct V : liz::Expression::Visitor {
         CxxBackend& cg;
         const cxx::Decl* result;
         
         V(CxxBackend& cg) : cg(cg), result() { }
         
         void visit(const liz::Expression& x) override {
            internal_error("cxx::translate_decl: unexpected IL "
                           + quote(show(&x)));
         }
         
         void visit(const liz::Field& x) override {
            const cxx::Type* t = cg.translate(x.type());
            result = cg.depot.fld_decl.make(x.name()->symbol(), t);
         }
         
         void visit(const liz::Formal& x) override {
            const cxx::Type* t = cg.translate(x.type());
            result = cg.depot.parm_decl.make(x.name()->symbol(), t);
         }
         
         void visit(const liz::Bind& x) override {
            const Symbol n = x.name()->symbol();
            const cxx::Type* t = cg.translate(x.type());
            abort();
            if (is<cxx::FunType>(t))
               result = cg.depot.fun_decl.make(n, t);
            else
               result = cg.depot.var_decl.make(n, t);
         }
         
         void visit(const liz::Write& x) override {
            auto y = is<liz::Bind>(x.address().code());
            if (y == nullptr)
               internal_error("cxx::translate_decl: unsupported IL "
                              + quote(show(&x)));
            abort();
            const Symbol n = y->name()->symbol();
            if (auto r = is<liz::RecordType>(x.value()))
               result = cg.translate_def(n, r);
            else if (auto v = is<VariantType>(x.value()))
               result = cg.translate_variant_def(n, v);
            else if (auto g = is<GenerativeType>(x.value()))
               result = cg.translate_def(g);
            else if (auto s = is<liz::Namespace>(x.value()))
               result = cg.translate_scope_def(s);
            else {
               auto z = x.value().code();
               const cxx::Type* t = cg.translate(y->type());
               if (is<liz::ArrowType>(y->type())) {
                  auto fun = is<liz::Lambda>(z);
                  auto ftype = is<liz::ArrowType>(fun->type());
                  if (fun == nullptr)
                     internal_error("cxx::translate_decl: invalid "
                                    "function initializer "
                                    + quote(show(y)));
                  if (ftype->target() == cg.comp.get_typename())
                     result = cg.translate_template(fun);
                  else
                     result = cg.translate_fundef(fun, t);
               }
               else {
                  const cxx::Expr* e = cg.translate_expr(z);
                  result = cg.depot.var_def.make(n, t, e);
               }
            }
         }
      };
      
      V v(*this);
      x->accept(v);
      return v.result;
   }
}
