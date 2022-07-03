// Copyright (C) 2012-2013, Texas A&M University
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
//     - Neither the name of the Texas A&M University. nor the name Liz,
//       nor the names of its contributors may be used to endorse or promote 
//       products derived from this software without specific prior
//       written permission.
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
#include <cstring>
#include <cstdlib>
#include <liz/utility>
#include "Expression.H"

namespace liz {
   // ----------
   // -- Type --
   // ----------
   Formatter
   Type::formatter() const {
      return Formatter();
   }

   const Data::Property*
   Type::data_traits() const {
      return 0;
   }

   // -- BasicType --
   BasicType::BasicType(const Name* n, const Data::Property* d)
         : structure::unary<const Name*>(n), data_info(d)
   { }

   void
   BasicType::print_on(std::ostream& os) const {
      os << name()->symbol();
   }

   Formatter
   BasicType::formatter() const {
      return data_info == 0 ? Formatter() : data_info->formatter;
   }

   const Data::Property*
   BasicType::data_traits() const {
      return data_info;
   }

   // -- TagType --
   TagType::TagType(const Name* n, TypeElaboration t)
         : structure::binary<const Name*, TypeElaboration>(n, t)
   { }

   void
   TagType::print_on(std::ostream& os) const {
      os << tag()->symbol() << " : ";
      const Type* t = type();
      t->print_on(os);
   }

   const Data::Property*
   TagType::data_traits() const {
      const Type* t = type();
      return t->data_traits();
   }

   // -- GenerativeType --
   GenerativeType::GenerativeType(const Name* n, const Type* t, const Scope* s)
         : structure::ternary<const Name*, const Type*, const Scope*>(n, t, s)
   { }

   void
   GenerativeType::print_on(std::ostream& os) const {
      os << name()->symbol();
   }

   const Data::Property*
   GenerativeType::data_traits() const {
      return value()->data_traits();
   }

   Formatter
   GenerativeType::formatter() const {
      return value()->formatter();
   }

   // -- ReferenceType --

   ReferenceType::ReferenceType(TypeElaboration t)
         : structure::unary<TypeElaboration>(t)
   { }

   void
   ReferenceType::print_on(std::ostream& os) const {
      os << "ref(";
      referee().code()->print_on(os);
      os << ')';
   }

   // Format a value of reference type.  In traditional C++, references
   // are "automatically" dereferenced; we stick to that tradition.
   // Note: on reflection, we should not when printing.
   static void
   format_reference(Data::Location, std::ostream& os, Data::Value v) {
      os << '@' << Data::Location(v);
   }
   
   Formatter
   ReferenceType::formatter() const {
      return Data::closure(&format_reference);
   }

   const Data::Property*
   ReferenceType::data_traits() const {
      return Data::property(Data::Mode::Pointer);
   }

   // -- ProductType --
   ProductType::ProductType(Target t, const Sequence<TagType>& s)
         : function_space<Sequence<TagType>>(t, s)
   { }

   void
   ProductType::print_on(std::ostream& os) const {
      os << "forall" << ' ' << '(';
      for (std::size_t i = 0; i < arity(); ++i) {
         if (i != 0)
            os << ',' << ' ';
         argument(i)->print_on(os);
      }
      os << ')' << ' ' << '.' << ' ';
      target().code()->print_on(os);
   }

   // -- ArrowType --

   ArrowType::ArrowType(TypeElaboration t, const InputTypes& s)
         : function_space<InputTypes>(t, s)
   { }

   void
   ArrowType::print_on(std::ostream& os) const {
      os << '(';
      for (std::size_t i = 0; i < arity(); ++i) {
         if (i != 0)
            os << ", ";
         argument(i).code()->print_on(os);
      }
      os << ')' << " -> ";
      target().code()->print_on(os);
   }

   // Format an object of function type onto an output stream.
   static void
   format_function(Data::Location, std::ostream& os, Data::Value v) {
      const Function* f = static_cast<const Function*>(Data::Location(v));
      if (is<Lambda>(f))
         ;
      else
         os << "builtin ";
      os << f->name()->symbol();
   }

   Formatter
   ArrowType::formatter() const {
      return Data::closure(&format_function);
   }

   // -- RecordType --
   RecordType::RecordType(const Sequence<TagType>& s)
         : Sequence<TagType>(s)
   { }

   void
   RecordType::print_on(std::ostream& os) const {
      os << "record {";
      bool need_comma = false;
      for (auto f : components()) {
         if (need_comma)
            os << ',';
         os << ' ';
         f->print_on(os);
         need_comma = true;
      }
      os << ' ' << '}';
   }

   static void
   format_record(Data::Location, std::ostream& os, Data::Value v) {
      const Record* r = Data::to_record(v);
      os << '{';
      bool need_comma = false;
      for (auto& f : *r) {
         if (need_comma)
            os << ',';
         os << ' ';
         os << f.name() << " = " << f.value();
         need_comma = true;
      }
      os << ' ' << '}';
   }

   Formatter
   RecordType::formatter() const {
      return Data::closure(&format_record);
   }

   // -- ReadonlyType --

   ReadonlyType::ReadonlyType(TypeElaboration t)
         : structure::unary<TypeElaboration>(t)
   { }

   void
   ReadonlyType::print_on(std::ostream& os) const {
      os << "const ";
      type().code()->print_on(os);
   }

   static void
   format_readonly(Data::Location e, std::ostream& os, Data::Value v) {
      const ReadonlyType* t = is<ReadonlyType>(static_cast<const Type*>(e));
      os << make_object(t->type(), v);
   }

   Formatter
   ReadonlyType::formatter() const {
      return Data::closure(&format_readonly, this);
   }

   // -- RestrictedType --
   RestrictedType::RestrictedType(TypeElaboration t, Elaboration c)
         : structure::binary<TypeElaboration, Elaboration>(t, c)
   { }

   void RestrictedType::print_on(std::ostream& os) const {
      if (auto ctor = is<Constructor>(condition()))
         os << ctor->name()->symbol();
      else {
         type().code()->print_on(os);
         os << " | ";
         os << *condition().code();
      }
   }

   Formatter
   RestrictedType::formatter() const {
      return type().code()->formatter();
   }

   // -- TypeExpression --
   TypeExpression::TypeExpression(Elaboration e)
         : structure::unary<Elaboration>(e)
   { }

   void
   TypeExpression::print_on(std::ostream& os) const {
      struct V : Expression::Visitor {
         std::ostream& os;
         V(std::ostream& os) : os(os) { }

         void visit(const Expression& x) { os << x; }
         void visit(const Instance& x) { os << x; }
         void visit(const Postulate& x) { os << x.name()->symbol(); }
         void visit(const Constructor& x) { os << x.name()->symbol(); }
      };

      V v(os);
      expr().code()->accept(v);
   }

   // -- QuantifiedType --
   QuantifiedType::QuantifiedType(Quantifier q, const Formals& fs,
                                  TypeElaboration t)
         : Base(q, fs, t)
   { }

   const Formal* QuantifiedType::formal(int i) const {
      return formals().at(i);
   }

   static void
   format_formals(const Formals& fs, std::ostream& os) {
      os << '(';
      for (std::size_t i = 0; i < fs.size(); ++i) {
         if (i != 0)
            os << ", ";
         const Formal* f = fs[i];
         if (auto n = f->name())
            os << n->symbol() << " : ";
         f->type().code()->print_on(os);
      }
      os << ')';
   }

   void QuantifiedType::print_on(std::ostream& os) const {
      os << quantifier();
      format_formals(formals(), os);
      os << " . ";
      abstract_instance().code()->print_on(os);
   }

   VariantType::VariantType(const Sequence<Constructor>& cs)
         : structure::unary<Sequence<Constructor>>(cs)
   { }

   void
   VariantType::print_on(std::ostream& os) const {
      const auto n = constructors().size();
      os << "variant" << '{' << ' ';
      for (std::size_t i = 0; i < n; ++i) {
         if (i != 0)
            os << ", ";
         os << pretty(constructors()[i])
            << ' ' << ':' << ' '
            << pretty(constructors()[i]->type());
      }
      os << ' ' << '}';
   }

   std::ostream&
   operator<<(std::ostream& os, const Type& type) {
      type.print_on(os);
      return os;
   }

   // -- TypeFactory --
   const BasicType*
   TypeFactory::make_basic_type(const Name* n, const Data::Property* d) {
      return basics.make(n, d);
   }

   const TagType*
   TypeFactory::make_tag_type(const Name* n, TypeElaboration t) {
      return tags.make(n, t);
   }

   const RecordType*
   TypeFactory::make_record_type(const Sequence<TagType>& s) {
      return records.make(s);
   }

   const ReferenceType*
   TypeFactory::make_reference_type(TypeElaboration t) {
      // The reference type constructor is idempotent
      // FIXME: well, after some thought, we don't do that now.
      // For we currenly to define an 'access' of having a reference
      // type.  We don't have the notion of 'automatic' dereference yet
      // so, we naturally get to the notion of reference to a reference.
      return refs.make(t);
   }

   const ProductType*
   TypeFactory::make_product_type(const Sequence<TagType>& s,
                                  TypeElaboration t) {
      return products.make(t, s);
   }
   
   const ArrowType*
   TypeFactory::make_arrow_type(TypeElaboration target,
                                const InputTypes& source) {
      return funs.make(target, source);
   }
   
   const ReadonlyType*
   TypeFactory::make_readonly_type(TypeElaboration t) {
      // the 'const' type constructor is idempotent
      if (auto u = is<ReadonlyType>(t))
         return u;
      return consts.make(t);
   }

   const RestrictedType*
   TypeFactory::make_restricted_type(TypeElaboration t, Elaboration c) {
      return restricts.make(t, c);
   }

   const TypeExpression*
   TypeFactory::make_type_expression(Elaboration e) {
      if (e.code() == 0)
         abort();
      if (const TypeExpression* t = is<TypeExpression>(e.code()))
         return t;
      return exprs.make(e);
   }

   const QuantifiedType*
   TypeFactory::make_quantified_type(Quantifier q, const Formals& fs,
                                     TypeElaboration t) {
      return quants.make(q, fs, t);
   }

   const VariantType*
   TypeFactory::make_variant_type(const Sequence<Constructor>& cs) {
      return variants.make(cs);
   }

   const GenerativeType*
   TypeFactory::make_generative_type(const Name* n,
                                     const Type* t, const Scope* s) {
      return generatives.make(n, t, s);
   }

   namespace {
      struct TypePatternVisitor : Expression::Visitor {
         bool result;
         TypePatternVisitor() : result(false) { }

         void visit(const Expression& x) {
            system_error("don't know whether "
                         + quote(show_cxx_type(&x))
                         + " is a type pattern");
         }

         void visit(const BasicType&) { }

         void visit(const ReferenceType& x) {
            result = is_type_pattern(x.referee());
         }

         void visit(const ArrowType& x) {
            result = is_type_pattern(x.target());
            for (std::size_t i = 0; i < x.arity() and not result; ++i)
               result = is_type_pattern(x.argument(i));
         }

         void visit(const ReadonlyType& x) {
            result = is_type_pattern(x.type());
         }

         void visit(const TypeExpression& x) {
            if (const Formal* f = is<Formal>(x.expr().code()))
               result = is_wildcard(f);
         }
      };
   }

   bool is_type_pattern(const Type* t) {
      TypePatternVisitor v;
      if (t != 0)
         t->accept(v);
      return v.result;
   }
}
