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


// This file implements Liz intrinsic operations.

#include <functional>
#include <fstream>
#include <cstdlib>
#include "Evaluator.H"

namespace liz {
   // -- Builtin names.
   struct BuiltinName {
      const char* text;
      const bool is_op;
   };

   static const Name*
   builtin_name(Elaborator* ctx, const BuiltinName& x) {
      if (x.is_op)
         return make_operator(ctx, x.text);
      return make_identifier(ctx, x.text);
   }

   struct UnaryFunctionType {
      const char* target;
      const char* source;
   };

   static const ArrowType*
   make_type(Elaborator* ctx, const UnaryFunctionType& x) {
      InputTypes src(1, convert_syntax_to_type(ctx, x.source));
      auto tgt = convert_syntax_to_type(ctx, x.target);
      return ctx->make_arrow_type(tgt, src);
   }

   struct BinaryFunctionType {
      const char* target;
      const char* source[2];
   };

   static const ArrowType*
   make_type(Elaborator* ctx, const BinaryFunctionType& x) {
      InputTypes src(2);
      src[0] = convert_syntax_to_type(ctx, x.source[0]);
      src[1] = convert_syntax_to_type(ctx, x.source[1]);
      return ctx->make_arrow_type(convert_syntax_to_type(ctx, x.target), src);
   }

   template<typename C, typename T>
   struct Intrinsic {
      BuiltinName name;
      C code;
      T type;
   };

   template<typename C, typename T, int N>
   static void
   define_intrinsics(BasicContext<>& ctx, const Intrinsic<C, T> (&table)[N]) {
      auto env = ctx.elaborator();
      for (auto& op : table) {
         auto n = builtin_name(env, op.name);
         const ArrowType* t = make_type(env, op.type);
         env->global_scope()->define(n, t, env->build_builtin(n, t, op.code));
      }
   }

   // --------------------------------
   // -- Unary intrinsic operations --
   // --------------------------------

   // Evaluate the instantiation of the intrinsic type constructor
   // 'reference' with 't'
   static Data::Value
   intrinsic_reference(BasicContext<>& ctx, Data::Value v) {
      auto t = static_cast<const Type*>(Data::Location(v));
      auto elab = ctx.elaborator();
      TypeElaboration referee(elab->get_typename(), t);
      return Data::Value(elab->make_reference_type(referee));
   }

   template<typename T, typename Op>
   static Data::Value
   builtin_unary(BasicContext<>&, Data::Value v) {
      return Data::Value(Op()(T(v)));
   }

   // -- implement intrisic exit operation
   static Data::Value
   intrinsic_exit(BasicContext<>& ctx, Data::Value value) {
      auto elab = ctx.elaborator();
      elab->io()->output() << std::endl;
      flush(elab->io()->error());
      flush(elab->io()->debug());
      std::exit(intptr_t(value));
      return { };               // never executed.
   }
   
   static Data::Value
   intrinsic_print_string(BasicContext<>& ctx,  Data::Value v) {
      Symbol s = v;
      ctx.elaborator()->io()->output() << s.string();
      return { };
   }
   
   static Data::Value
   intrinsic_print_int(BasicContext<>& ctx, Data::Value value) {
      ctx.elaborator()->io()->output() << intptr_t(value);
      return { };
   }
   
   static Data::Value
   intrinsic_print_double(BasicContext<>& ctx, Data::Value v) {
      ctx.elaborator()->io()->output() << double(v);
      return { };
   }
   
   static Data::Value
   intrinsic_print_char(BasicContext<>& ctx, Data::Value v) {
      ctx.elaborator()->io()->output() << Character(v);
      return { };
   }

   // Implement the `Arity' function
   static Data::Value
   intrinsic_arity(BasicContext<>&, Data::Value v) {
      auto ft = static_cast<const ArrowType*>(Data::Location(v));
      return Data::Value(intptr_t(ft->arity()));
   }

   // Implement the 'Codomain' type function
   static Data::Value
   intrinsic_codomain(BasicContext<>&, Data::Value v) {
      auto ft = static_cast<const ArrowType*>(Data::Location(v));
      return Data::Value(ft->target().code());
   }

   struct complement {
      template<typename T>
      T operator()(T t) const { return ~t; }
   };

   // -- registration of unary intrinsics --

   // This datatype help collect static description about
   // unary intrinsic operations.
   using UnaryIntrinsic = Intrinsic<Function::Unary, UnaryFunctionType>;

   // This table lists all unary intrinsic operations currently
   // implemented as C++ code.
   const UnaryIntrinsic unary_intrinsic_table[] = {
      { { "ref", false }, intrinsic_reference, { "type", "type" } },
      { { "not", true },
        builtin_unary<bool, std::logical_not<bool>>,
        { "bool", "bool" } },
      { { "~", true }, builtin_unary<byte, complement>, { "byte", "byte" } },
      { { "~", true }, builtin_unary<intptr_t, complement>, { "int", "int" } },
      { { "-", true },
        builtin_unary<intptr_t, std::negate<intptr_t>>,
        { "int", "int" } },
      { { "-", true },
        builtin_unary<double, std::negate<double>>,
        { "double", "double" } },
      { { "exit", false }, intrinsic_exit, { "void", "int" } },
      { { "Arity", false }, intrinsic_arity, { "int", "Function" } },
      { { "Codomain", false }, intrinsic_codomain, { "type", "Function" } }, 
      { { "print", false }, intrinsic_print_int, { "void", "int" } }, 
      { { "print", false }, intrinsic_print_double, { "void", "double" } }, 
      { { "print", false }, intrinsic_print_char, { "void", "char" } }, 
      { { "print", false }, intrinsic_print_string, { "void", "string" } }
   };

   // ---------------------------------
   // -- Binary intrinsic operations --
   // ---------------------------------

   // -- builtin functions

   template<typename T, typename Op>
   static Data::Value
   builtin_binary(BasicContext<>&, Data::Value lhs, Data::Value rhs) {
      return Data::Value(Op()(T(lhs), T(rhs)));
   }

   template<typename Op>
   Data::Value
   builtin_div_int(BasicContext<>&, Data::Value lhs, Data::Value rhs) {
      if (intptr_t(rhs) == 0)
         evaluation_error("integer division by zero");
      return Data::Value(Op()(intptr_t(lhs), intptr_t(rhs)));
   }

   // Implement the `InputType' type function
   static Data::Value
   intrinsic_input_type(BasicContext<>&, Data::Value ft, Data::Value idx) {
      auto ftype = Data::to_arrow_type(ft);
      const std::size_t i = intptr_t(idx);
      if (i >= ftype->arity())
         evaluation_error("index greater than arity of function");
      return Data::Value(ftype->argument(i).code());
   }

   // Implement builtin type equality
   static Data::Value
   intrinsic_type_eq(BasicContext<>&, Data::Value lhs, Data::Value rhs) {
      return Data::Value(Data::to_type(lhs) == Data::to_type(rhs));
   }

   static Data::Value
   intrinsic_type_neq(BasicContext<>&, Data::Value lhs, Data::Value rhs) {
      return Data::Value(Data::to_type(lhs) != Data::to_type(rhs));
   }

   // -- Registration of binary intrinsics --

   using BinaryIntrinsic  = Intrinsic<Function::Binary, BinaryFunctionType>;

   const BinaryIntrinsic binary_intrinsic_table[] = {
      { { "/\\", true },
        builtin_binary<bool, std::logical_and<bool>>,
        { "bool", { "bool", "bool" } } },
      { { "\\/", true },
        builtin_binary<bool, std::logical_or<bool>>,
        { "bool", { "bool", "bool" } } },
      { { "/\\", true },
        builtin_binary<byte, std::bit_and<byte>>,
        { "byte", { "byte", "byte" } } }, 
      { { "\\/", true },
        builtin_binary<byte, std::bit_or<byte>>,
        { "byte", { "byte", "byte" } } }, 
      { { "/\\", true },
        builtin_binary<intptr_t, std::bit_and<intptr_t>>,
        { "int", { "int", "int" } } }, 
      { { "\\/", true },
        builtin_binary<intptr_t, std::bit_or<intptr_t>>,
        { "int", { "int", "int" } } }, 
      
      { { "+", true },
        builtin_binary<intptr_t, std::plus<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "+", true },
        builtin_binary<double, std::plus<double>>,
        { "double", { "double", "double" } } },
      { { "-", true },
        builtin_binary<intptr_t, std::minus<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "-", true },
        builtin_binary<double, std::minus<double>>,
        { "double", { "double", "double" } } },
      { { "*", true },
        builtin_binary<intptr_t, std::multiplies<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "*", true },
        builtin_binary<double, std::multiplies<double>>,
        { "double", { "double", "double" } } },
      { { "div", true },
        builtin_div_int<std::divides<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "mod", true },
        builtin_binary<intptr_t, std::modulus<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "rem", true },
        builtin_binary<intptr_t, std::modulus<intptr_t>>,
        { "int", { "int", "int" } } },
      { { "/", true },
        builtin_binary<double, std::divides<double>>,
        { "double", { "double", "double" } } },
      { { ">", true },
        builtin_binary<byte, std::greater<byte>>,
        { "bool", { "byte", "byte" } } },
      { { ">", true },
        builtin_binary<intptr_t, std::greater<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { ">", true },
        builtin_binary<double, std::greater<double>>,
        { "bool", { "double", "double" } } },
      { { ">=", true },
        builtin_binary<byte, std::greater_equal<byte>>,
        { "bool", { "byte", "byte" } } },
      { { ">=", true },
        builtin_binary<intptr_t, std::greater_equal<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { ">=", true },
        builtin_binary<double, std::greater_equal<double>>,
        { "bool", { "double", "double" } } },
      { { "<", true },
        builtin_binary<byte, std::less<byte>>,
        { "bool", { "byte", "byte" } } },
      { { "<", true },
        builtin_binary<intptr_t, std::less<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { "<", true },
        builtin_binary<double, std::less<double>>,
        { "bool", { "double", "double" } } },
      { { "<=", true },
        builtin_binary<byte, std::less_equal<byte>>,
        { "bool", { "byte", "byte" } } },
      { { "<=", true },
        builtin_binary<intptr_t, std::less_equal<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { "<=", true },
        builtin_binary<double, std::less_equal<double>>,
        { "bool", { "double", "double" } } },
      { { "==", true },
        builtin_binary<bool, std::equal_to<bool>>,
        { "bool", { "bool", "bool" } } },
      { { "==", true },
        builtin_binary<byte, std::equal_to<byte>>,
        { "bool", { "byte", "byte" } } },
      { { "==", true },
        builtin_binary<intptr_t, std::equal_to<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { "==", true },
        builtin_binary<double, std::equal_to<double>>,
        { "bool", { "double", "double" } } },
      { { "!=", true },
        builtin_binary<bool, std::not_equal_to<bool>>,
        { "bool", { "bool", "bool" } } },
      { { "!=", true },
        builtin_binary<byte, std::not_equal_to<byte>>,
        { "bool", { "byte", "byte" } } },
      { { "!=", true },
        builtin_binary<intptr_t, std::not_equal_to<intptr_t>>,
        { "bool", { "int", "int" } } },
      { { "!=", true },
        builtin_binary<double, std::not_equal_to<double>>,
        { "bool", { "double", "double" } } },
      { { "InputType", false },
        intrinsic_input_type, { "type", { "Function", "int" } } },
      { { "==", true }, intrinsic_type_eq, { "bool", { "type", "type" } } },
      { { "!=", true }, intrinsic_type_neq, { "bool", { "type", "type" } } },
   };

   void
   Elaborator::define_intrinsics() {
      BasicContext<> ctx { this, evl };
      liz::define_intrinsics(ctx, unary_intrinsic_table);
      liz::define_intrinsics(ctx, binary_intrinsic_table);
   }
}
