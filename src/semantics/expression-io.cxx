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

#include <iostream>
#include <typeinfo>
#include "Expression.H"
#include "sexpr-io.H"

namespace liz {
   namespace {
      struct VisitedSet : std::set<const Expression*> {
         bool has(const Expression* x) const {
            return find(x) != end();
         }
      };
   }

   using io::Output;
   using structure::unary;
   using structure::binary;
   using structure::ternary;

   static void sexpr(Output&, const Expression*, VisitedSet&);

   static void
   sexpr(Output& out, const Elaboration& x, VisitedSet& seen) {
      sexpr(out, x.code(), seen);
   }

   // static void
   // sexpr(Output& out, offset_type i, VisitedSet&) {
   //    out.stream() << i;
   // }

   static void
   sexpr(Output& out, Symbol n, VisitedSet&) {
      out.stream() << n;
   }

   template<typename T>
   static void
   just_tag(Output& out, const char* tag, const T& x) {
      out << '(' << tag << ' ' << x << ')';
   }

   static void
   sexpr(Output& out, const Name* n, VisitedSet&) {
      struct V : Name::Visitor {
         Output& out;
         V(Output& o) : out(o) { }
         void visit(const Identifier& x) {
            just_tag(out, ":identifier", x.symbol());
         }
         void visit(const Operator& x) {
            just_tag(out, ":operator", x.symbol());
         }
         void visit(const Literal& x) {
            just_tag(out, ":literal", x.symbol());
         }
      };
      V v(out);
      n->accept(v);
   }

   template<typename T>
   static void
   tag_sexpr(Output& out, const char* tag, const T& x, VisitedSet& seen) {
      out << '(' << tag << ' ';
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr(out, x, seen);
      out << ')';
      out.nibble(-io::tab_unit);
   }

   template<typename T>
   static void
   sexpr(Output& out, const char* tag, const unary<T>& x, VisitedSet& seen) {
      tag_sexpr(out, tag, x.operand(), seen);
   }

   template<typename U, typename V>
   static void
   sexpr(Output& out, const char* tag, const binary<U, V>& x,
         VisitedSet& seen) {
      out << '(' << tag;
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr(out, x.first(), seen);
      out << io::newline() << io::indent();
      sexpr(out, x.second(), seen);
      out << ')';
      out.nibble(-io::tab_unit);
   }

   template<typename U, typename V, typename W>
   static void
   sexpr(Output& out, const char* tag, const ternary<U, V, W>& x,
         VisitedSet& seen) {
      out << '(' << tag << ' ';
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr(out, x.first(), seen);
      out << io::newline() << io::indent();
      sexpr(out, x.second(), seen);
      out << io::newline() << io::indent();
      sexpr(out, x.third(), seen);
      out << ')';
      out.nibble(-io::tab_unit);
   }

   template<typename T>
   static void
   sexpr(Output& os, const vector<T>& args, VisitedSet& seen) {
      for (std::size_t i = 0; i < args.size(); ++i) {
         if (i != 0)
            os << io::newline() << io::indent();
         sexpr(os, args[i], seen);
      }
   }

   static void
   sexpr(Output& os, const LinkName& x, VisitedSet& seen) {
      sexpr(os, ":link-name", x, seen);
   }

   template<typename T>
   static void
   sexpr(Output& os, const char* t, const vector<T>& args, VisitedSet& seen) {
      os << '(' << t;
      if (not args.empty())
         sexpr(os, args, seen);
      os << ')';
   }

   static void
   sexpr(Output& os, const std::pair<Symbol, Elaboration>& x, VisitedSet& seen) {
      os << '(';
      sexpr(os, x.first, seen);
      os << ' ';
      sexpr(os, x.second.code(), seen);
      os << ')';
   }

   static void
   sexpr(Output& os, const Clause& c, VisitedSet& seen) {
      os << '(';
      sexpr(os, c.pattern(), seen);
      os << ' ';
      sexpr(os, c.action(), seen);
      os << ')';
      os << io::newline() << io::indent();
   }

   static void
   sexpr(Output& out, const Formal& x, VisitedSet& seen) {
      out << '(' << ":formal"
          << ' ' << x.position() << ' ' << x.level();
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr(out, x.link_name(), seen);
      out.nibble(-io::tab_unit);
      out << ')';
   }

   static void
   sexpr(Output& out, const TypeElaboration& x, VisitedSet& seen) {
      return sexpr(out, x.code(), seen);
   }

   static void
   sexpr(Output& out, const ArrowType& x, VisitedSet& seen) {
      out << '(' << ":arrow-type" << ' ';
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      tag_sexpr(out, ":target", x.target(), seen);
      out << io::newline() << io::indent();
      tag_sexpr(out, ":source", x.source(), seen);
      out << ')';
      out.nibble(-io::tab_unit);
   }

   static void
   sexpr(Output& out, const GenerativeType& x, VisitedSet& seen) {
      out << '(' << ":generative";
      out.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr(out, x.name(), seen);
      out << io::newline() << io::indent();
      sexpr(out, x.value(), seen);
      out << ')';
      out.nibble(-io::tab_unit);
   }

   static void
   sexpr(Output& os, const Namespace& ns, VisitedSet& seen) {
      os << '(' << ":scope" << ' ';
      if (auto n = ns.name())
         sexpr(os, n, seen);
      else
         os << "<global>";
      os << ')';
   }

   static void
   sexpr_quantified_params(Output& os, Quantifier quant, const Formals& parms,
                           VisitedSet& seen) {
      os << '(';
      switch (quant) {
      case Quantifier::forall: os << ":forall"; break;
      case Quantifier::exists: os << ":exists"; break;
      default: os << ":unknown-quantifier"; break;
      }
      os.nibble(io::tab_unit);
      for (auto& p: parms) {
         os << io::newline() << io::indent();
         sexpr(os, p, seen);
      }
      os.nibble(-io::tab_unit);
      os << ')';
   }

   static void
   sexpr(Output& os, const QuantifiedType& qt, VisitedSet& seen) {
      os << '(' << ":quantified-type";
      os.nibble(io::tab_unit) << io::newline() << io::indent();
      sexpr_quantified_params(os, qt.quantifier(), qt.formals(), seen);
      os << io::newline() << io::indent();
      sexpr(os, qt.abstract_instance().code(), seen);
      os.nibble(-io::tab_unit);
      os << ')';
   }

   static void
   sexpr(Output& os, const BasicType& t, VisitedSet& seen) {
      os << '(' << ":basic-type" << ' ';
      sexpr(os, t.name(), seen);
      os << ')';
   }

   static void
   sexpr(Output& os, const Substitution& subst, VisitedSet& seen) {
      os << '(' << ":subst";
      os.nibble(io::tab_unit) << io::newline() << io::indent();
      for (auto& p : subst) {
         os << '(';
         sexpr(os, p.first, seen);
         os << ' ';
         sexpr(os, p.second, seen);
         os << ')';
         os << io::newline() << io::indent();
      }
      os << ')';
      os.nibble(-io::tab_unit) << io::newline();
   }

   namespace {
      struct PrefixFormVisitor : Expression::Visitor {
         Output& os;
         VisitedSet& seen;
         PrefixFormVisitor(Output& os, VisitedSet& v)
               : os(os), seen(v) { }

         void visit(const Expression& x) override {
            os << "<unknown expression of type " << typeid(x).name() << ">";
         }
         void visit(const LinkName& x) override { sexpr(os, x, seen); }
         
         void visit(const Bool& x) override {
            os.stream() << "(:bool "  << std::boolalpha << bool(x) << ')';
         }
         void visit(const Byte& x) override { just_tag(os, ":byte", x.rep()); }
         void visit(const Char& x) override { just_tag(os, ":char", x.rep()); }
         void visit(const Int& x) override { just_tag(os,":integer", x.rep());  }
         void visit(const Double& x) override { just_tag(os, ":double", x.rep()); }
         void visit(const String& x) override { just_tag(os, ":string", x.rep()); }
         void visit(const Postulate& x) override { sexpr(os, ":postulate", x, seen); }
         void visit(const Instance& x) override { sexpr(os, ":instance", x, seen); }
         void visit(const Constructor& x) override { sexpr(os, ":ctor", x, seen); }
         void visit(const Formal& x) override { sexpr(os, x, seen); }
         void visit(const Field& x) override { sexpr(os, ":field", x, seen); }
         void visit(const Macro& x) override { sexpr(os, ":macro", x, seen); }
         
         void visit(const Formula& x) override {
            if (x.quantifier() == Quantifier::forall)
               os << "(:forall ";
            else if (x.quantifier() == Quantifier::exists)
               os << "(:exist ";
            else
               os << "(:formula ";
            os << "(:parameters ";
            for (std::size_t i = 0; i < x.parameters().size(); ++i) {
               os << ' ';
               sexpr(os, x.parameter(i), seen);
            }
            os << ") ";
            sexpr(os, x.body().code(), seen);
            os << ')';
         }
         
         void visit(const Assumption& x) override {
            sexpr(os, ":assumption", x, seen);
         }
         
         void visit(const Block& x) override { sexpr(os, ":block", x, seen); }
         
         void visit(const Function& x) override {
            sexpr(os, ":builtin", x.link_name(), seen);
         }

         void visit(const Lambda& x) override {
            if (seen.has(&x))
               sexpr(os, x.link_name(), seen);
            else {
               seen.insert(&x);
               os << "(:lambda ";
               sexpr(os, x.name(), seen);
               os << ' ' << show(*x.type());
               os.nibble(io::tab_unit) << io::newline() << io::indent();
               sexpr(os, x.body().code(), seen);
               os << ')';
               os.nibble(-io::tab_unit) << io::newline() << io::indent();
            }
         }

         void visit(const Signature& x) override {
            os << "(:sig ";
            sexpr(os, x.link_name(), seen);
            os << ')';
         }

         void visit(const Namespace& x) override { sexpr(os, x, seen); }
         void visit(const LoadUnit& x) override {
            os << "(:load-unit " << x.path() << ')';
         }
         void visit(const Import& x) override { sexpr(os, ":import", x, seen); }
         void visit(const Type& x) override {
            os << "(:type " << typeid(x).name() << ')';
         }
         void visit(const BasicType& x) override { sexpr(os, x, seen); }
         void visit(const TagType& x) override { sexpr(os, ":tag-type", x, seen); }
         void visit(const ReferenceType& x) override {
            sexpr(os, ":reference-type", x, seen);
         }
         void visit(const ProductType& x) override { sexpr(os, ":product-type", x, seen); }
         void visit(const ArrowType& x) override { sexpr(os, x, seen); }
         void visit(const RecordType& x) override {
            os.stream() << "(:record";
            for (auto f : x.components()) {
               os << ' ';
               sexpr(os, f, seen);
            }
            os << ')';
         }
         void visit(const RestrictedType& x) override {
            sexpr(os, ":restricted-type", x, seen);
         }
         void visit(const GenerativeType& x) override { sexpr(os, x, seen); }
         void visit(const ReadonlyType& x) override { sexpr(os, ":const", x, seen); }
         void visit(const TypeExpression& x) override {
            sexpr(os, ":type-expr", x, seen);
         }
         void visit(const VariantType& x) override {
            sexpr(os, ":variant-type", x, seen);
         }
         void visit(const QuantifiedType& x) override { sexpr(os, x, seen); }
         void visit(const Concept&) override {
//            just_tag(os, ":concept", x.name()->symbol());
         }
         void visit(const Initializer& x) override {
            sexpr(os, ":initializer", x, seen);
         }
         void visit(const Quote& x) override { sexpr(os, ":quote", x, seen); }
         void visit(const Let& x) override { sexpr(os, ":let", x, seen); }
         void visit(const Read& x) override { sexpr(os, ":read", x, seen); }
         void visit(const Write& x) override { sexpr(os, ":write", x, seen); }
         void visit(const Offset& x) override { sexpr(os, ":offset", x, seen); }
         void visit(const Component& x) override { sexpr(os, ":component", x, seen); }
         void visit(const DotSelection& x) override { sexpr(os, ":dot", x, seen); }
         void visit(const UnaryExpression& x) override { sexpr(os, ":unary", x, seen); }
         void visit(const BinaryExpression& x) override {
            sexpr(os, ":binary", x, seen);
         }
         void visit(const BinaryLogical& x) override {
            os << '(';
            switch (x.operation()) {
            case logical::conjunction:
               os << "and";
               break;
            case logical::disjunction:
               os << "or";
               break;
            case logical::implication:
               os << "=>";
               break;
            case logical::equivalence:
               os << "<=>";
               break;
            }
            os << ' ';
            sexpr(os, x.lhs().code(), seen);
            os << ' ';
            sexpr(os, x.rhs().code(), seen);
            os << ')';
         }
         void visit(const Call& x) override { sexpr(os, ":call", x,seen); }
         void visit(const Assertion& x) override { sexpr(os, ":assert", x, seen); }
         void visit(const Constraint& x) override { sexpr(os, ":constraint", x, seen); }
         void visit(const Return& x) override { sexpr(os, ":return", x, seen); }
         void visit(const Throw& x) override { sexpr(os, ":throw", x, seen); }
         void visit(const Loop& x) override { sexpr(os, ":loop", x, seen); }
         void visit(const If& x) override { sexpr(os, ":if", x, seen); }
         void visit(const Match& x) override { sexpr(os, ":match", x, seen); }
         void visit(const Bind& x) override { sexpr(os, ":bind", x, seen); }
         void visit(const SubstExpr& x) override { sexpr(os, ":subst-expr", x, seen); }
      };
   }
   
   static void
   sexpr(Output& os, const Expression* expr, VisitedSet& seen) {
      if (expr == nullptr)
         os << ":nil";
      else {
         PrefixFormVisitor v(os, seen);
         expr->accept(v);
      }
   }

   void prefix_form(std::ostream& os, const Expression* expr) {
      Output out(os);
      VisitedSet seen;
      sexpr(out, expr, seen);
   }
}
