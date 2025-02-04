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

#include <ctype.h>
#include <cctype>
#include <iostream>
#include <sstream>
#include <typeinfo>
#include <stdlib.h>
#include <algorithm>
#include "Expression.H"

namespace liz {
   // -- Object --
   std::ostream&
   operator<<(std::ostream& os, const Object& o) {
      Data::Formatter formatter = o.type()->formatter();
      if (formatter.fun != 0)
         formatter.fun(formatter.env, os, o.value());
      else
         os << "(type=" << *o.type() << ", ?)";

      return os;
   }

   // -- Fiber --
   Fiber::Fiber() { }

   Fiber::Fiber(const Elaboration& e)
         : vector<Elaboration>(1, e)
   { }

   Fiber&
   Fiber::append(const Fiber& f) {
      for (auto& e : f)
         if (std::find(begin(), end(), e) == end())
            push_back(e);
      return *this;
   }

   // -- Utility functions --
   std::string
   quote(const Name* n) {
      return quote(n->symbol());
   }

   FunctionElaboration
   lookup_function(ScopeRef s, const Name* n, const ArrowType* t) {
      if (auto decl = s->select(n, t)) {
         if (auto fun = is<Function>(decl->value()))
            return { t, fun };
      }
      return { };
   }

   // -- EquivalenceQuotient --
   using EqClassHandle = EquivalenceQuotient::iterator;

   // Return true `whole' uses `part'.
   static bool uses(const Expression*, const Expression*) {
      return false;
   }

   namespace {
      struct UserVisitor : Expression::Visitor {
         bool result;
         const Expression* part;
         explicit UserVisitor(const Expression* x)
               : result(false), part(x)
         { }

         void visit(const Value&) { } // a value uses nothing.
         void visit(const Function& x) {
            auto ftype = x.type();
            result = uses(ftype->target(), part);
            for (int i = 0; i < x.arity() and not result; ++i)
               result = uses(ftype->argument(i), part);
         }

         void visit(const ReferenceType& x) {
            result = part == x.referee();
         }

         void visit(const LinkName& x) {
            result = part == x.type();
         }

         void visit(const Read& x) {
            result = part == x.address().code();
         }

         void visit(const Write& x) {
            result = part == x.address().code()
               or part == x.value().code();
         }

         void visit(const UnaryExpression& x) {
            result = part == x.function().code()
               or part == x.argument().code();
         }
      };
   }

   // Add `t' to the list `l' of users.
   template<typename T>
   static void register_user(std::list<T>& l, T t) {
      if (std::find(l.begin(), l.end(), t) == l.end())
         l.push_back(t);
   }

   template<typename T>
   static inline T previous(T t) {
      return --t;
   }

   // Return a handle to the equality equivalence class of `expr'.
   EquivalenceQuotient::iterator
   EquivalenceQuotient::eq_class(Elaboration expr) {
      for (auto p = begin(); p != end(); ++p) {
         for (auto& e : *p)
            if (e == expr)
               return p;
            else if (uses(expr.code(), e.code()))
               register_user(p->users(), expr);
      }
      push_back(expr);
      return previous(end());
   }

   // Displace the elements of the class pointed to by `src' to
   // the class pointed to by `dst'.
   void EquivalenceQuotient::displace(iterator src, iterator dst) {
      auto* n = dst->rep;
      while (n->next != nullptr)
         n = n->next;
      n->next = src->rep;
      src->rep = nullptr;
      erase(src);
   }

   void EquivalenceQuotient::merge_classes(Elaboration e1, Elaboration e2) {
      iterator c1 = eq_class(e1);
      iterator c2 = eq_class(e2);
      if (c1 != c2) {
         if (is_closed(c1->leader()))
            displace(c2, c1);
         else
            displace(c1, c2);
      }
   }


   // -- AssumptionSet --
   bool
   AssumptionSet::contains(Elaboration e) const {
      return std::find(begin(), end(), e) != end();
   }

   // ----------------
   // -- ScopeStack --
   // ----------------
   ScopeStack::ScopeStack() {
      props.push_back(AssumptionSet());
   }

   void
   ScopeStack::push(Scope* s) {
      rep.push_back(ScopeRef(s));
      props.push_back(props.back());
   }

   void
   ScopeStack::pop() {
      rep.pop_back();
      props.pop_back();
   }

   AssumptionSet&
   ScopeStack::assumptions() {
      return props.back();
   }

   void
   ScopeStack::assume(Elaboration e) {
      assumptions().push_back(e);
   }

   EquivalenceQuotient&
   ScopeStack::eq_classes() {
      return assumptions().eq_classes;
   }

   Scope::Scope()
         : Environment<Declaration>(Contour::local)
   { }

   Elaboration*
   Scope::define(const Name* n, const Type* t, const Expression* x) {
      return &bind(n, { t, x })->value();
   }

   // Return true if `type' is a function type or amenable to
   // a function type.  This notion is relevant for deciding
   // whether an entity of this type can be part of an overload type.
   static bool
   is_almost_function_type(const Type* type) {
      if (is<ArrowType>(type))
         return true;
      return false;
   }

   // Return true if the overload set `decls' can coexist with
   // a new declaration with type `type'
   static bool
   can_coexist_with(const Scope::BindingSet& decls, const Type* type) {
      return decls.empty() or is_almost_function_type(type);
   }
   
   Declaration*
   declare(ScopeRef scope, const LinkName& lnk) {
      auto name = lnk.name();
      const Type* type = lnk.type();
      if (auto decl = scope->select(name, type))
         return decl;
      Scope::BindingSet decls = scope->lookup(name);
      if (not can_coexist_with(decls, type))
         system_error("conflicting redeclaration of " + quote(name));
      return scope->bind(name, { type, nullptr });
   }

   Declaration*
   declare(ScopeRef scope, const LinkName& lnk, const Expression* expr) {
      auto name = lnk.name();
      const Type* type = lnk.type();
      // FIXME: check for previous declaration.
      Scope::BindingSet decls = scope->lookup(name);
      if (not can_coexist_with(decls, type))
         system_error("conflicting redeclaration of " + quote(name));
      return scope->bind(name, { type, expr });
   }

   // -- ToplevelScope --
   ToplevelScope::ToplevelScope(const Scope* p)
         : prnt(p)
   { }

   Elaboration
   ToplevelScope::statement(int i) const {
      return stmts.at(i);
   }

   void
   ToplevelScope::add_stmt(Elaboration x) {
      stmts.push_back(x);
   }

   // -- Namespace --
   Namespace::Namespace(const Name* n, const Scope* p)
         : ToplevelScope(p),
           structure::unary<const Name*>(n),
           s(Contour::nonlocal)
   { }

   Elaboration*
   Namespace::define(const Name* n, const Type* t, const Expression* x) {
      auto def = Scope::define(n, t, x);
      store()->bind(n->symbol(), { t, Data::Value(x) });
      return def;
   }

   // -- Record --
   Record::Record()
         : Store<Object>(Contour::freestanding)
   { }

   // --
   
   std::string
   show_cxx_type(const Expression* expr) {
      return std::string(typeid(*expr).name());
   }

   // -- Quantifier --
   std::ostream& operator<<(std::ostream& os, Quantifier k) {
      return os << (k == Quantifier::exists ? "exists" : "forall");
   }

   // -- Identifier --
   Identifier::Identifier(Symbol s)
         : structure::unary<Symbol>(s) { }

   void
   Identifier::accept(Visitor& v) const {
      v.visit(*this);
   }

   Symbol
   Identifier::symbol() const {
      return operand();
   }
   
   // -- Operator --
   Operator::Operator(Symbol s)
         : structure::unary<Symbol>(s) { }

   void
   Operator::accept(Visitor& v) const {
      v.visit(*this);
   }

   Symbol
   Operator::symbol() const {
      return operand();
   }

   bool Operator::alphabetic() const {
      Symbol s = symbol();
      return not s.string().empty() and std::isalpha(*s.string().begin());
   }

   // -- Literal --
   Literal::Literal(Symbol s, const Type* t)
         : structure::binary<Symbol, const Type*>(s, t)
   { }

   Symbol
   Literal::symbol() const {
      return first();
   }

   void
   Literal::accept(Visitor& v) const {
      v.visit(*this);
   }
   
   // -- LinkName --
   LinkName::LinkName(const Name* n, const Type* t)
         : structure::binary<const Name*, const Type*>(n, t) { }

   // -- Postulate --
   Postulate::Postulate(const LinkName& n)
         : structure::unary<LinkName>(n)
   { }

   // --------------
   // -- Instance --
   // --------------
   Instance::Instance(const Constructor* c, const Sequence<Value>& x)
         : structure::binary<const Constructor*, Sequence<Value>>(c, x)
   { }

   // -- Quote --
   Quote::Quote(Elaboration x)
         : structure::unary<Elaboration>(x)
   { }

   // -- Concept --
   Concept::Concept()
   { }

   // -- LoadUnit --
   LoadUnit::LoadUnit(const Path& p)
         : ToplevelScope(nullptr),
           pth(p)
   { }

   // -- Domain --
   Domain::Domain(const Constructor* c, const Arguments& a)
         : BasicView(c, a),
           ToplevelScope(nullptr)
   { }

   // -- Formal --
   Formal::Formal(int p, int l, TypeElaboration t, const LinkName& n)
         : structure::binary<TypeElaboration, LinkName>(t, n),
           coord(p, l)
   { }

   void Formal::accept(Visitor& v) const {
      v.visit(*this);
   }

   // -- A wildcard is a formal expression with a negative level (-2).
   const int wildcard_level = -2;

   bool is_wildcard(const Formal* f) {
      return f != nullptr and f->level() == wildcard_level;
   }

   Formals::Formals() : Sequence<Formal>() { }

   Formals::Formals(std::size_t n)
         : Sequence<Formal>(n)
   { }

   Formals::Formals(const Formal* f)
         : Sequence<Formal>(1, f)
   { }

   Formals::operator InputTypes() const {
      InputTypes types(size());
      for (std::size_t i = 0; i < size(); ++i)
         types[i] = (*this)[i]->type();
      return types;
   }

   // -- Formula --
   Formula::Formula(Quantifier q, const Formals& p, Elaboration e)
         : structure::ternary<Quantifier, Formals, Elaboration>(q, p, e)
   { }

   const Formal*
   Formula::parameter(int i) const {
      return parameters().at(i);
   }

   // -- Macro --
   Macro::Macro(const LinkName& n, Elaboration x)
         : structure::binary<LinkName, Elaboration>(n, x)
   { }

   // -- Function --
   const ArrowType*
   Function::type() const {
      return is<ArrowType>(link_name().type());
   }

   // -- Constructor --
   Constructor::Constructor(const LinkName& n, Elaboration e)
         : structure::binary<LinkName, Elaboration>(n, e)
   { }

   const Type*
   Constructor::type() const {
      return first().type();
   }

   std::size_t
   Constructor::arity() const {
      if (auto t = is<ArrowType>(type()))
         return t->arity();
      return 0;
   }

   // -- Lambda --
   Lambda::Lambda(const LinkName& n, const Formals& p, Elaboration x)
         : structure::ternary<LinkName, Formals, Elaboration>(n, p, x)
   { }
   
   void Lambda::accept(Visitor& v) const {
      v.visit(*this);
   }

   const Formal*
   Lambda::parameter(int i) const {
      return formals().at(i);
   }

   // -- Field --
   Field::Field(const LinkName& n, const Expression* x)
         : impl(n, x)
   { }

   // -- DeclarativeScope --
   DeclarativeScope::DeclarativeScope()
   { }

   DeclarativeScope::DeclarativeScope(const Sequence<Field>& s)
         : flds(s)
   { }
   
   void
   DeclarativeScope::add_field(const Field* f) {
      flds.push_back(f);
   }

   // -- Signature --
   Signature::Signature(const LinkName& n)
         : structure::unary<LinkName>(n)
   { }

   void Signature::accept(Visitor& v) const {
      v.visit(*this);
   }

   // -- Assumption --
   Assumption::Assumption(Elaboration x)
         : structure::unary<Elaboration>(x)
   { }

   // -- Initializer --
   Initializer::Initializer(const AssocArguments& x)
         : AssocArguments(x)
   { }

   Let::Let(const vector<Elaboration>& ds, Elaboration e)
         : structure::binary<vector<Elaboration>, Elaboration>(ds, e)
   { }

   // -- Read --
   Read::Read(Elaboration s)
         : structure::unary<Elaboration>(s) { }

   // -- Write --
   Write::Write(Elaboration d, Elaboration v)
         : structure::binary<Elaboration>(d, v) { }

   // -- Offset
   Offset::Offset(Elaboration w, Elaboration p)
         : structure::binary<Elaboration>(w, p) { }

   // -- Component
   Component::Component(Elaboration w, const LinkName& n)
         : structure::binary<Elaboration, LinkName>(w, n)
   { }

   // -- DotSelection --
   DotSelection::DotSelection(Elaboration e, const LinkName& n)
         : structure::binary<Elaboration, LinkName>(e, n)
   { }

   // -- UnaryExpression --
   UnaryExpression::UnaryExpression(FunctionElaboration f, Elaboration x)
         : structure::binary<FunctionElaboration, Elaboration>(f, x)
   { }

   Negate::Negate(FunctionElaboration f, Elaboration x)
         : impl(f, x)
   { }

   Not::Not(FunctionElaboration f, Elaboration x)
         : impl(f, x)
   { }

   Complement::Complement(FunctionElaboration f, Elaboration x)
         : impl(f, x)
   { }

   // -- BinaryExpression --
   BinaryExpression::BinaryExpression(FunctionElaboration f,
                                      Elaboration x, Elaboration y)
         : Base(f, x, y)
   { }

   Plus::Plus(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Dash::Dash(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Star::Star(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Slash::Slash(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Div::Div(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Quo::Quo(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Rem::Rem(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Mod::Mod(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Langle::Langle(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Rangle::Rangle(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Langleq::Langleq(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Rangleq::Rangleq(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Eqeq::Eqeq(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Excleq::Excleq(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   And::And(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }

   Or::Or(FunctionElaboration f, Elaboration x, Elaboration y)
         : impl(f, x, y)
   { }


   // -- BinaryLogical --
   BinaryLogical::BinaryLogical(logical::Operation op,
                                Elaboration l, Elaboration r)
         : structure::ternary<logical::Operation, Elaboration>(op, l, r)
   { }

   // -- If --
   If::If(Elaboration c, Elaboration tt, Elaboration ff)
         : structure::ternary<Elaboration>(c, tt, ff)
   { }
   
   // -- Call --
   Call::Call(FunctionElaboration f, const Arguments& a)
         : Base(f, a)
   { }

   // -- Assertion --
   Assertion::Assertion(Elaboration e, FunctionElaboration p)
         : structure::binary<Elaboration, FunctionElaboration>(e, p)
   { }

   // -- Constraint --
   Constraint::Constraint(const Constructor* c, const Arguments& a)
         : BasicView(c, a)
   { }

   // -- Return --
   Return::Return(Elaboration e) : structure::unary<Elaboration>(e)
   { }

   Throw::Throw(Elaboration e) : structure::unary<Elaboration>(e)
   { }

   // -- Loop --
   Loop::Loop(Elaboration e) : structure::unary<Elaboration>(e) { }

   // -- Bind --
   Bind::Bind(const LinkName& n, Elaboration e)
         : structure::binary<LinkName, Elaboration>(n, e) { }

   // -- Clause --
   Clause::Clause() { }

   Clause::Clause(Elaboration p, Elaboration x)
         : std::pair<Elaboration, Elaboration>(p, x)
   { }

   // -- Match --
   Match::Match(Elaboration x, const Clauses& c)
         : structure::binary<Elaboration, Clauses>(x, c)
   { }

   // -- Block --
   Block::Block(const Arguments& s)
         : structure::unary<Arguments>(s)
   { }

   Elaboration
   Block::statement(int i) const {
      return statements().at(i);
   }

   // ------------
   // -- Import --
   // ------------
   Import::Import(LoadUnit* u)
         : structure::unary<LoadUnit*>(u)
   { }

   // -- SubstExpr --
   SubstExpr::SubstExpr(const Substitution& s, Elaboration e)
         : structure::binary<Substitution, Elaboration>(s, e)
   { }

   // ------------------
   // -- Substitution --
   // ------------------
   Elaboration
   Substitution::has(const Formal* var) const {
      auto iter = find(var);
      if (iter == end())
         return { };
      return iter->second;
   }

   Elaboration
   Substitution::operator()(const Formal* var) const {
      if (auto e = has(var))
         return e;
      return { var->type(), var };
   }

   // -- Expression::Visitor --

   void Expression::Visitor::visit(const LinkName& x) {
      visit(as<Expression>(x));
   }
   
   void Expression::Visitor::visit(const Value& x) {
      visit(as<Expression>(x));
   }
   
   void Expression::Visitor::visit(const Bool& x) {
      visit(as<Value>(x));
   }
   
   void Expression::Visitor::visit(const Byte& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Char& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Int& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Double& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const String& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Record& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Postulate& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Instance& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Quote& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Namespace& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Macro& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Function& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Constructor& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const NiladicBuiltinFunction& x) {
      visit(as<Function>(x));
   }

   void Expression::Visitor::visit(const UnaryBuiltinFunction& x) {
      visit(as<Function>(x));
   }

   void Expression::Visitor::visit(const BinaryBuiltinFunction& x) {
      visit(as<Function>(x));
   }

   void Expression::Visitor::visit(const Lambda& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Formula& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Domain& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const LoadUnit& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const Type& x) {
      visit(as<Value>(x));
   }

   void Expression::Visitor::visit(const BasicType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const TagType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const ReferenceType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const ProductType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const ArrowType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const RecordType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const ReadonlyType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const RestrictedType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const TypeExpression& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const QuantifiedType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const VariantType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const GenerativeType& x) {
      visit(as<Type>(x));
   }

   void Expression::Visitor::visit(const Field& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Formal& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Signature& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Concept& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Initializer& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Let& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Read& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Write& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Offset& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Component& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const DotSelection& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const UnaryExpression& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Negate& x) {
      visit(as<UnaryExpression>(x));
   }

   void Expression::Visitor::visit(const Not& x) {
      visit(as<UnaryExpression>(x));
   }

   void Expression::Visitor::visit(const Complement& x) {
      visit(as<UnaryExpression>(x));
   }

   void Expression::Visitor::visit(const BinaryExpression& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Plus& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Dash& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Star& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Slash& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Div& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Quo& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Rem& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Mod& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Langle& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Rangle& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Langleq& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Rangleq& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Eqeq& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Excleq& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const And& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const Or& x) {
      visit(as<BinaryExpression>(x));
   }

   void Expression::Visitor::visit(const BinaryLogical& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Call& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Assertion& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Constraint& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Return& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Throw& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Loop& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const If& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Match& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Bind& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Block& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const SubstExpr& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Assumption& x) {
      visit(as<Expression>(x));
   }

   void Expression::Visitor::visit(const Import& x) {
      visit(as<Expression>(x));
   }

   // -- ExpressionFactory --
   ExpressionFactory::ExpressionFactory()
         : gensym_count(), wildcard_count()
   { }

   Symbol
   ExpressionFactory::intern(const std::string& s) {
      return Symbol(&*syms.insert(s).first);
   }

   Symbol
   ExpressionFactory::intern(const char* s) {
      return intern(std::string(s));
   }

   Symbol
   ExpressionFactory::intern(const Token* t) {
      if (t == nullptr)
         return { };
      return intern(lexeme(*t));
   }
   
   const Bool* ExpressionFactory::build_bool(bool v, const Type* t) {
      return bools.make(v, t);
   }

   const Byte* ExpressionFactory::build_byte(byte v, const Type* t) {
      return bytes.make(v, t);
   }
   
   const Char* ExpressionFactory::build_char(Character v, const Type* t) {
      return chars.make(v, t);
   }
   
   const Int* ExpressionFactory::build_int(int v, const Type* t) {
      return ints.make(v, t);
   }
   
   const Double* ExpressionFactory::build_double(double v, const Type* t) {
      return doubles.make(v, t);
   }
   
   const String* ExpressionFactory::build_string(Symbol v, const Type* t) {
      return strings.make(v, t);
   }

   const Postulate*
   ExpressionFactory::build_postulate(const LinkName& n) {
      return pos.make(n);
   }

   const Instance*
   ExpressionFactory::build_instance(const Constructor* c,
                                     const Sequence<Value>& x) {
      return insts.make(c, x);
   }

   const Quote*
   ExpressionFactory::build_quote(Elaboration x) {
      return quotes.make(x);
   }

   Record*
   ExpressionFactory::build_record() {
      return records.make();
   }

   const Identifier* ExpressionFactory::build_identifier(Symbol s) {
      return ids.make(s);
   }

   const Identifier*
   ExpressionFactory::build_identifier(const Token* t) {
      return build_identifier(intern(t));
   }

   const Identifier*
   ExpressionFactory::fresh_name() {
      std::ostringstream os;
      os << "__t" << gensym_count++;
      return build_identifier(intern(os.str()));
   }

   const Operator* ExpressionFactory::build_operator(Symbol s) {
      return ops.make(s);
   }

   const Operator*
   ExpressionFactory::build_operator(const Token* t) {
      if (t == nullptr)
         return { };
      return build_operator(intern(t));
   }

   const Literal*
   ExpressionFactory::build_literal(Symbol s, const Type* t) {
      return lits.make(s, t);
   }

   const LinkName*
   ExpressionFactory::build_name(const Name* n, const Type* t) {
      return lnks.make(n, t);
   }

   const Field*
   ExpressionFactory::build_field(const LinkName& n, const Expression* x) {
      return flds.make(n, x);
   }

   LoadUnit*
   ExpressionFactory::build_load_unit(const Path& p) {
      return modules.make(p);
   }

   Concept*
   ExpressionFactory::build_concept() {
      return concepts.make();
   }

   const Formal*
   ExpressionFactory::build_formal(int p, int l, TypeElaboration t,
                                   const LinkName& n) {
      return formals.make(p, l, t, n);
   }

   const Let*
   ExpressionFactory::build_let(const vector<Elaboration> ds, Elaboration e) {
      return lets.make(ds, e);
   }

   const Formal*
   ExpressionFactory::build_wildchar(TypeElaboration t) {
      return build_formal(++wildcard_count, wildcard_level, t, { { }, { } });
   }
   
   const Formula*
   ExpressionFactory::build_formula(Quantifier q,
                                    const Formals& p, Elaboration e) {
      return formulae.make(q, p, e);
   }

   const BinaryLogical*
   ExpressionFactory::build_logical(logical::Operation op,
                                    Elaboration lhs, Elaboration rhs) {
      return bls.make(op, lhs, rhs);
   }

   const Block*
   ExpressionFactory::build_block(const Arguments s) {
      return blocks.make(s);
   }

   const Macro*
   ExpressionFactory::build_macro(const LinkName& n, Elaboration e) {
      return macros.make(n, e);
   }

   const NiladicBuiltinFunction*
   ExpressionFactory::build_builtin(const Name* n, const ArrowType* t,
                                    Function::Niladic c) {
      return nfuns.make(LinkName(n, t), c);
   }

   const UnaryBuiltinFunction*
   ExpressionFactory::build_builtin(const Name* n, const ArrowType* t,
                                    Function::Unary c) {
      return ufuns.make(LinkName(n, t), c);
   }

   const BinaryBuiltinFunction*
   ExpressionFactory::build_builtin(const Name* n, const ArrowType* t,
                                    Function::Binary c) {
      return bfuns.make(LinkName(n, t), c);
   }
   
   const Constructor*
   ExpressionFactory::build_constructor(const LinkName& n, Elaboration e) {
      return ctors.make(n, e);
   }

   const Lambda*
   ExpressionFactory::build_lambda(const LinkName& n, const Formals& p,
                                   Elaboration x) {
      return lambdas.make(n, p, x);
   }

   const Signature*
   ExpressionFactory::build_signature(const Name* n, const Type* t) {
      return sigs.make(LinkName(n, t));
   }

   const Signature*
   ExpressionFactory::build_signature(const LinkName& n) {
      return sigs.make(n);
   }

   const Initializer*
   ExpressionFactory::build_initializer(const AssocArguments& x) {
      return inits.make(x);
   }

   const Read*
   ExpressionFactory::build_read(Elaboration s) {
      return rds.make(s);
   }

   const Write*
   ExpressionFactory::build_write(Elaboration d, Elaboration v) {
      return wrs.make(d, v);
   }
   
   const Call*
   ExpressionFactory::build_call(FunctionElaboration fun, const Arguments& args) {
      return calls.make(fun, args);
   }

   const Assertion*
   ExpressionFactory::build_assertion(Elaboration e, FunctionElaboration p) {
      return asserts.make(e, p);
   }
   
   const Constraint*
   ExpressionFactory::build_constraint(const Constructor* c,
                                       const Arguments& args) {
      return constraints.make(c, args);
   }

   const Return*
   ExpressionFactory::build_return(Elaboration e) {
      return rets.make(e);
   }

   const Throw*
   ExpressionFactory::build_throw(Elaboration e) {
      return throws.make(e);
   }

   const Match*
   ExpressionFactory::build_match(Elaboration x, const Clauses& c) {
      return matches.make(x, c);
   }

   const Offset*
   ExpressionFactory::build_offset(Elaboration w, Elaboration p) {
      return offs.make(w, p);
   }
   
   const Component*
   ExpressionFactory::build_component(Elaboration w, const LinkName& n) {
      return cmps.make(w, n);
   }
   
   const DotSelection*
   ExpressionFactory::build_dot(Elaboration w, const LinkName& n) {
      return dots.make(w, n);
   }
   
   const Negate*
   ExpressionFactory::build_negate(FunctionElaboration fun, Elaboration arg) {
      return negs.make(fun, arg);
   }

   const Not*
   ExpressionFactory::build_not(FunctionElaboration fun, Elaboration arg) {
      return nots.make(fun, arg);
   }

   const Complement*
   ExpressionFactory::build_complement(FunctionElaboration fun, Elaboration arg) {
      return compls.make(fun, arg);
   }

   const Plus*
   ExpressionFactory::build_plus(FunctionElaboration fun,
                                 Elaboration arg0, Elaboration arg1) {
      return plss.make(fun, arg0, arg1);
   }

   const Dash*
   ExpressionFactory::build_dash(FunctionElaboration fun,
                                 Elaboration arg0, Elaboration arg1) {
      return dshs.make(fun, arg0, arg1);
   }

   const Star*
   ExpressionFactory::build_star(FunctionElaboration fun,
                                 Elaboration arg0, Elaboration arg1) {
      return strs.make(fun, arg0, arg1);
   }

   const Slash*
   ExpressionFactory::build_slash(FunctionElaboration fun,
                                  Elaboration arg0, Elaboration arg1) {
      return slhs.make(fun, arg0, arg1);
   }

   const Div*
   ExpressionFactory::build_div(FunctionElaboration fun,
                                Elaboration arg0, Elaboration arg1) {
      return divs.make(fun, arg0, arg1);
   }

   const Quo*
   ExpressionFactory::build_quo(FunctionElaboration fun,
                                Elaboration arg0, Elaboration arg1) {
      return quos.make(fun, arg0, arg1);
   }

   const Rem*
   ExpressionFactory::build_rem(FunctionElaboration fun,
                                Elaboration arg0, Elaboration arg1) {
      return rems.make(fun, arg0, arg1);
   }

   const Mod*
   ExpressionFactory::build_mod(FunctionElaboration fun,
                                Elaboration arg0, Elaboration arg1) {
      return mods.make(fun, arg0, arg1);
   }

   const Langle*
   ExpressionFactory::build_langle(FunctionElaboration fun,
                                   Elaboration arg0, Elaboration arg1) {
      return lgls.make(fun, arg0, arg1);
   }

   const Rangle*
   ExpressionFactory::build_rangle(FunctionElaboration fun,
                                   Elaboration arg0, Elaboration arg1) {
      return rgls.make(fun, arg0, arg1);
   }

   const Langleq*
   ExpressionFactory::build_langleq(FunctionElaboration fun,
                                    Elaboration arg0, Elaboration arg1) {
      return lglqs.make(fun, arg0, arg1);
   }

   const Rangleq*
   ExpressionFactory::build_rangleq(FunctionElaboration fun,
                                    Elaboration arg0, Elaboration arg1) {
      return rglqs.make(fun, arg0, arg1);
   }

   const Eqeq*
   ExpressionFactory::build_eqeq(FunctionElaboration fun,
                                 Elaboration arg0, Elaboration arg1) {
      return eqqs.make(fun, arg0, arg1);
   }

   const Excleq*
   ExpressionFactory::build_excleq(FunctionElaboration fun,
                                   Elaboration arg0, Elaboration arg1) {
      return xqs.make(fun, arg0, arg1);
   }

   const And*
   ExpressionFactory::build_and(FunctionElaboration fun,
                                Elaboration arg0, Elaboration arg1) {
      return ands.make(fun, arg0, arg1);
   }

   const Or*
   ExpressionFactory::build_or(FunctionElaboration fun,
                               Elaboration arg0, Elaboration arg1) {
      return ors.make(fun, arg0, arg1);
   }
   
   const If*
   ExpressionFactory::build_if(Elaboration c,
                               Elaboration tt, Elaboration ff) {
      return ifs.make(c, tt, ff);
   }

   const Loop*
   ExpressionFactory::build_loop(Elaboration e) {
      return loops.make(e);
   }
   
   const Bind*
   ExpressionFactory::build_bind(const LinkName& n, Elaboration e) {
      return binds.make(n, e);
   }

   const Bind*
   ExpressionFactory::build_bind(const Name* n, const Type* t, Elaboration e) {
      return binds.make(LinkName(n, t), e);
   }

   const SubstExpr*
   ExpressionFactory::build_subst_expr(const Substitution& s, Elaboration e) {
      return substs.make(s, e);
   }

   const Assumption*
   ExpressionFactory::build_assumption(Elaboration x) {
      return props.make(x);
   }

   Namespace*
   ExpressionFactory::build_namespace(const Name* n, const Scope* p) {
      return scopes.make(n, p);
   }

   const Import*
   ExpressionFactory::build_import(LoadUnit* u) {
      return imports.make(u);
   }
   
   // -- Helper functions --
   const Block* to_block(const Expression* x) {
      return dynamic_cast<const Block*>(x);
   }
   
   const Function* to_function(const Expression* x) {
      return dynamic_cast<const Function*>(x);
   }

   const Type* to_type(const Expression* x) {
      return dynamic_cast<const Type*>(x);
   }

   const Constraint* to_constraint(const Expression* x) {
      return dynamic_cast<const Constraint*>(x);
   }

   std::string show(const Expression* x) {
      std::ostringstream os;
      prefix_form(os, x);
      return os.str();
   }

   // -- UnificationContext
   bool
   UnificationContext::unifiable(const Formal* p) const {
      return std::find(formals.begin(), formals.begin(), p) != formals.end();
   }
}
