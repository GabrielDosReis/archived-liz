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
#include <cstring>
#include <string>
#include <algorithm>
#include <iterator>
#include "Evaluator.H"

namespace liz {
   // -- Evidence --

   Evidence::Evidence() : ok(true) { }

   const Function*
   Evidence::operator()(const Signature* s) const {
      const_iterator p = find(s);
      return p == end() ? nullptr : p->second;
   }

   void
   Evidence::send_to(const Signature* s, const Function* f) {
      if (f == nullptr)
         system_error("no function resolution for signature "
                         + quote(show_expr(s)));
      iterator p = find(s);
      if (p != end() and p->first != s)
         system_error("conflicting resolution for signature "
                      + quote(show_expr(s)));
      insert(std::make_pair(s, f));
   }

   void
   Evidence::send_to(const Constraint* c, const Evidence* e) {
      if (e == nullptr)
         system_error("no evidence for constraint "
                         + quote(show_expr(c)));
      ConstraintMap::iterator p = sub_evidence_map.find(c);
      if (p != sub_evidence_map.end() and p->first != c)
         system_error("conflicting resolution for constraint "
                         + quote(show_expr(c)));
      sub_evidence_map.insert(std::make_pair(c, e));
   }

   // -- FreeVariableError --
   FreeVariableError::FreeVariableError(const std::string& m, Symbol s)
         : BasicError(m), sym(s)
   { }

   void
   FreeVariableError::format_message(std::ostream& os) const {
      os << msg << ": " << sym.string();
   }

   static void
   free_variable_error(const std::string& msg, Symbol s) {
      throw FreeVariableError(msg, s);
   }

   namespace Data {
      inline Namespace*
      to_namespace(Value v) {
         return static_cast<Namespace*>(Abstract(v));
      }
   }
   
   //  -- debugging functions
   void
   print_bindings(const Scope* scope) {
      Scope::const_iterator p = scope->begin();
      Scope::const_iterator last = scope->end();
      for (; p != last; ++p) {
         std::cerr << '\t' << p->name()->symbol()
                   << " +-> ";
         prefix_form(std::cerr, p->value().code());
         std::cerr << '\n';
      }
   }

   // ------------------------
   // -- Runtime evaluation --
   // ------------------------

   // -- stop evaluation and signal a runtime assertion failure.
   static void
   assertion_failure() {
      throw BasicError("runtime assertion failure");
   }

   // -- Evaluator --
   Evaluator::Evaluator()
         : elab()
   {
      // get_store()->push(elab->global_scope()->store());
   }

   Evaluator::~Evaluator()
   { }

   void Evaluator::set_elaborator(Elaborator* e)
   {
      elab = e;
      get_store()->push(elab->global_scope()->store());
   }

   static Data::Value
   convert(BasicContext<Evaluator>*, const Postulate& x) {
      free_variable_error("unrealized postulate", x.name()->symbol());
      return { };
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const TypeExpression& x) {
      return ctx->evaluator()->eval(x.expr()).value();
   }

   // Evaluate a reference to a namespace member.
   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Component& x) {
      auto env = ctx->evaluator();
      auto s = Data::to_namespace(env->eval(x.whole()).value())->store();
      Symbol n = x.symbol();
      auto var = s->select(n, x.type());
      return Data::Value(&var->value().value());
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const DotSelection& x) {
      auto env = ctx->evaluator();
      auto s = Data::to_record(env->eval(x.whole()).value());
      auto var = s->select(x.symbol(), x.type());
      return Data::Value(&var->value().value());
   }
   
   // Evaluate a read expression
   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Read& x) {
      auto env = ctx->evaluator();
      Data::Value loc = env->eval(x.address()).value();
      return *Data::to_value_location(loc);
   }

   // Return the runtime entity, if any, found by lexical
   // lookup of a symbolic reference.
   static Variable<Object>*
   lexical_or_global_var(BasicContext<Evaluator>* ctx, const LinkName& x) {
      auto env = ctx->evaluator();
      auto n = x.symbol();
      auto type_eq = [&](const Variable<Object>& v) {
         return v.value().type() == x.type();
      };
      if (auto var = lexical_lookup(*env->get_store(), n).select_if(type_eq))
         return var;
      return ctx->elaborator()->global_scope()->store()->select(n, x.type());
   }
   
   // Evaluate a symbolic reference to an entity.
   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const LinkName& x) {
      auto var = lexical_or_global_var(ctx, x);
      if (var == 0)
         evaluation_error("there is no binding of symbol "
                          + quote(x.symbol()));
      return Data::Value(&var->value().value());
   }

   // Evaluate argument lists in a call.
   static vector<Object>
   evaluate_arguments(Evaluator* env, const Arguments& args) {
      vector<Object> vals;
      for (auto& arg : args)
         vals.push_back(env->eval(arg));
      return vals;
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const AssocArguments& x) {
      auto env = ctx->evaluator();
      const std::size_t n = x.size();
      Record* v = ctx->elaborator()->build_record();
      for (std::size_t i = 0; i < n; ++i)
         v->bind(x[i].first, env->eval(x[i].second));
      return Data::Value(v);
   }

   static Data::Value
   evaluate(Evaluator* env, Evaluator::FrameManager& frame,
            const Lambda* f, const vector<Object>& args) {
      // 1. Bind parameters to their values.
      const auto nargs = args.size();
      for (std::size_t i = 0; i < nargs; ++i)  {
         auto parm = f->parameter(i);
         if (auto name = parm->name()) {
            if (frame->select(name->symbol(), parm->type()))
               evaluation_error("Existing binding for parameter named "
                                + quote(name->symbol()));
            frame->bind(name->symbol(), args[i]);
         }
      }
      // 2. Evaluate the body of the function.
      try {
         return env->eval(f->body()).value();
      }
      catch (ReturnJump<Data::Value> ret) {
         return ret.value;
      }
   }

   static Data::Value
   evaluate(BasicContext<Evaluator>* ctx, const Constructor* ctor,
            const vector<Object>& args) {
//      auto ftype = is<ArrowType>(ctor->type());
      Sequence<Value> vals(args.size());
      for (std::size_t i = 0; i < args.size(); ++i) {
         vals[i] = reify(*ctx, args[i]);
      }
      auto env = ctx->elaborator();
      return Data::Value(env->build_instance(ctor, vals));
   }

   template<typename T, typename C, typename... Args>
   auto eval_call(BasicContext<T>* ctx, const Builtin<C>* f,
                  const Args... args) {
      BasicContext<> basic = *ctx;
      return f->code()(basic, args.value()...);
   }

   // Evaluate function call.
   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Call& x) {
      auto env = ctx->evaluator();
      // 1. Figure out what to call.
      auto f = Data::to_function(env->eval(x.function()).value());
      // 2. Evaluate arguments in the caller's context.
      vector<Object> args = evaluate_arguments(env, x.arguments());
      // 3. Establish new call frame.
      Evaluator::FrameManager frame(env, x.function());
      if (auto fun = is<NiladicBuiltinFunction>(f))
         return eval_call(ctx, fun);
      else if (auto fun = is<UnaryBuiltinFunction>(f))
         return eval_call(ctx, fun, args[0]);
      else if (auto fun = is<BinaryBuiltinFunction>(f))
         return eval_call(ctx, fun, args[0], args[1]);
      else if (auto fun = is<Lambda>(f))
         return evaluate(env, frame, fun, args);
      else if (auto ctor = is<Constructor>(f))
         return evaluate(ctx, ctor, args);
      internal_error("call to alien function");
      return Data::Value();     // never executed.
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Assertion& x) {
      auto env = ctx->evaluator();
      auto f = Data::to_function(env->eval(x.predicate()).value());
      auto pred = is<Lambda>(f);
      if (pred == nullptr)
         internal_error("predicate in assertion not a lambda");
      auto arg = env->eval(x.expression());
      Evaluator::FrameManager frame(env, x.predicate());
      if (!bool(evaluate(env, frame, pred, { arg })))
         // FIXME: provide more source-level context.
         assertion_failure();
      return arg.value();
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Formal& x) {
      auto env = ctx->evaluator();
      auto frame = env->current_frame();
      auto name = x.symbol();
      auto decls = frame->lookup(name);
      switch (decls.size()) {
      case 1:
         break;
         
      case 0:
         evaluation_error("unbound parameter " + quote(name)
                          + " in function "
                          + quote(show_expr(frame->function().code())));
         break;
         
      default:
         evaluation_error("multiple bindings for parameter " + quote(name)
            + " in function "
            + quote(show_expr(frame->function().code())));
         break;
      }
      auto loc = &decls.begin()->value().value();
      return loc;
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Block& x) {
      auto env = ctx->evaluator();
      LocalStoreManager<Object, FunctionElaboration> new_scope { env };
      Data::Value v { };
      for (auto& s : x.statements())
         v = env->eval(s).value();
      return v;
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Write& x) {
      auto env = ctx->evaluator();
      Object rhs = env->eval(x.value());
      Object place = env->eval(x.address());
      *Data::to_value_location(place.value()) = rhs.value();
      return place.value();
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const UnaryExpression& x) {
      auto env = ctx->evaluator();
      auto f = Data::to_function(env->eval(x.function()).value());
      auto fun = is<UnaryBuiltinFunction>(f);
      if (fun == nullptr)
         internal_error("call to non-builtin unary operator");
      Object arg = env->eval(x.argument());
      return eval_call(ctx, fun, arg);
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const BinaryExpression& x) {
      auto env = ctx->evaluator();
      auto f = Data::to_function(env->eval(x.function()).value());
      auto fun = is<BinaryBuiltinFunction>(f);
      if (fun == nullptr)
         internal_error("call to non-builtin binary operator");
      Object arg0 = env->eval(x.lhs());
      Object arg1 = env->eval(x.rhs());
      return eval_call(ctx, fun, arg0, arg1);
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const BinaryLogical& x) {
      auto env = ctx->evaluator();
      Object lhs = env->eval(x.lhs());
      switch (x.operation()) {
      case logical::conjunction:
         return bool(lhs.value()) ? env->eval(x.rhs()).value() : lhs.value();
         
      case logical::disjunction:
         return bool(lhs.value()) ? lhs.value() : env->eval(x.rhs()).value();
         
      case logical::implication:
         return bool(lhs.value()) ? env->eval(x.rhs()).value() : lhs.value();

      case logical::equivalence:
         return bool(lhs.value()) == bool(env->eval(x.rhs()).value());
      }
      evaluation_error("unknown binary logical operator");
      return { };
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Return& x) {
      auto env = ctx->evaluator();
      throw ReturnJump<Data::Value>(env->eval(x.expression()).value());
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Throw& x) {
      auto env = ctx->evaluator();
      throw ThrowJump<Data::Value>(env->eval(x.expression()).value());
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const If& x) {
      auto env = ctx->evaluator();
      return bool(env->eval(x.condition()).value())
         ? env->eval(x.consequence()).value()
         : env->eval(x.alternative()).value();
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Bind& x) {
      auto env = ctx->evaluator();
      auto n = x.link_name();
      auto undef = make_object(n.type(), Data::Value());
      auto store = env->get_store()->top();
      if (store->select(n.symbol(), n.type()))
         evaluation_error("Attempt to redeclare symbol " + quote(n.symbol()));
      auto place = &store->bind(n.symbol(), undef)->value().value();
      *place = env->eval(x.initializer()).value();
      return Data::Value(place);
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Let& x) {
      auto env = ctx->evaluator();
      NonlocalStoreManager<Object, FunctionElaboration> new_scope { env };
      for (auto& d : x.locals())
         env->eval(d);
      return env->eval(x.body()).value();
   }

   static Data::Value
   convert(BasicContext<Evaluator>* ctx, const Import& x) {
      auto env = ctx->evaluator();
      for (auto& e : x.load_unit()->statements())
         env->eval(e);
      return Data::Value(x.load_unit());
   }

   Object
   Evaluator::eval(Elaboration expr) {
      // Note: We could worry that a null AST may
      // be produced by erroneous parsing or transformation, and
      // attempt to signal a problem here.  However, doing so would
      // force us to duplicate the semantics of null expression
      // statement in various places.  We find it simpler to just
      // pretend that all null AST are null expression-statement.
      BasicContext<Evaluator> ctx { elab, this };
      const Type* t = simplify_type(ctx, expr.type());
      TransducerVisitor<decltype(ctx), Data::Value> v { ctx };
      if (expr.code() != nullptr)
         expr.code()->accept(v);
      return make_object(t, v.result);
   }

}
