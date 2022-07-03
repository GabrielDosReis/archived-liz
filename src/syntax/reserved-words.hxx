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

#ifndef LIZ_RESERVED_WORDS_included
#define LIZ_RESERVED_WORDS_included

namespace liz {
   namespace token {
      enum class Alphabetic : kind_value {
         LowerLimit = 0xffffff,
         Delimiter,                  // delimiter characters
         LiteralBoolean,             // Boolean literal values
         LiteralCharacter,           // Character literal values
         LiteralInteger,             // Integral literal values
         LiteralString,              // String literal values
         LiteralReal,                // Real literal values
         Identifier,                 // identifier
         Div,                        // "div"
         Quo,                        // "quo"
         Mod,                        // "mod"
         Rem,                        // "rem"
         Is,                         // "is"
         Axiom,                      // "axiom"
         Prop,                       // "prop"
         Const,                      // "const"
         If,                         // "if"
         Then,                       // "then"
         Else,                       // "else"
         In,                         // "in"
         Match,                      // "match"
         With,                       // "with"
         Case,                       // "case"
         For,                        // "for"
         While,                      // "while"
         Until,                      // "until"
         Repeat,                     // "repeat"
         Do,                         // "do"
         Return,                     // "return"
         Throw,                      // "throw"
         Leave,                      // "leave"
         Break,                      // "break"
         Variant,                    // "variant"
         Record,                     // "record"
         Requires,                   // "requires"
         Bool,                       // "bool"
         Byte,                       // "byte"
         Char,                       // "char"
         Int,                        // "int"
         Double,                     // "double"
         String,                     // "string"
         Type,                       // "type"
         Not,                        // "not"
         And,                        // "and"
         Or,                         // "or"
         Forall,                     // "forall"
         Exists,                     // "exists"
         Concept,                    // "concept"
         Namespace,                  // "namespace"
         Assert,                     // "assert"
         Assume,                     // "assume"
         Define,                     // "define"
         Where,                      // "where"
         Rule,                       // "rule"
         Import,                     // "import"
         Inductive,                  // "inductive"
         Coinductive,                // "coinductive"
         Prolong,                    // "prolong"
         Postulate,                  // "postulate"
         Realize,                    // "realize"
         Indent,                     // increase indentation level
         Justify,                    // match previous indentation
         Unindent,                   // decrease indentation level
         Count
      };

      constexpr Kind value(AlphabeticWord w)
      {
         return Kind(kind_value(w));
      }


      // -- Literal strings of given length.
      template<int N>
      using text_chunk = const char (&)[N];
      
      template<int N, typename = std::enable_if_t<(N < 5)>>
         constexpr kind_value value(text_chunk<N> s) {
         kind_value v { };
         for (int i = N - 1; i >= 0; --i)
            v = (v << u8_bits) + s[i];
         return v;
      }

      // Token objects are classified by kinds.
      constexpr Kind unknown_tok = value("");        // unknown token
      constexpr Kind hash_tok    = value("#");
      constexpr Kind caret_tok   = value("^");
      constexpr Kind exclamation_tok  = value("!");
      constexpr Kind plus_tok         = value("+");
      constexpr Kind minus_tok        = value("-");
      constexpr Kind equal_tok        = value("=");
      constexpr Kind less_tok         = value("<");
      constexpr Kind greater_tok      = value(">");
      constexpr Kind ampersand_tok    = value("&");
      constexpr Kind bar_tok          = value("|");
      constexpr Kind star_tok         = value("*");
      constexpr Kind slash_tok        = value("/");
      constexpr Kind percent_tok      = value("%");
      constexpr Kind tilde_tok        = value("~");
      constexpr Kind open_paren_tok   = value("(");
      constexpr Kind close_paren_tok  = value(")");
      constexpr Kind open_bracket_tok     = value("[");
      constexpr Kind close_bracket_tok    = value("]");
      constexpr Kind open_brace_tok       = value("{");
      constexpr Kind close_brace_tok      = value("}");
      constexpr Kind comma_tok            = value(",");
      constexpr Kind dot_tok              = value(".");
      constexpr Kind semicolon_tok        = value(";");
      constexpr Kind colon_tok            = value(":");
      constexpr Kind at_tok               = value("@");
      constexpr Kind underscore_tok       = value("_");
      constexpr Kind last_unigraph_tok    = 0xff;
      constexpr Kind description_tok      = value("++");
      constexpr Kind wisecrack_tok        = value("--");
      constexpr Kind right_arrow_tok      = value("->");
      constexpr Kind left_arrow_tok       = value("<-");
      constexpr Kind less_equal_tok       = value("<=");
      constexpr Kind greater_equal_tok    = value(">=");
      constexpr Kind double_equal_tok     = value("==");
      constexpr Kind implies_tok          = value("=>");
      constexpr Kind not_equal_tok        = value("!=");
      constexpr Kind double_colon_tok     = value("::");
      constexpr Kind colon_equal_tok      = value(":=");
      constexpr Kind hash_exclamation_tok = value("#!");
      constexpr Kind dot_dot_tok          = value("..");
      constexpr Kind call_tok             = value("()"); // pseudo, call operator
      constexpr Kind index_tok            = value("[]"); // pseudo, index operator
      constexpr Kind construct_tok        = value("{}"); // pseudo, construction operator
      constexpr Kind open_mbracket_tok    = value("[|");
      constexpr Kind close_mbracket_tok   = value("|]");
      constexpr Kind open_bar_brace_tok   = value("{|");
      constexpr Kind close_bar_brace_tok  = value("|}");
      constexpr Kind meet_tok             = value("/\\");
      constexpr Kind join_tok             = value("\\/");
      constexpr Kind last_digraph_tok     = 0xffff;
      constexpr Kind equiv_tok            = value("<=>");
      constexpr Kind last_trigraph_tok    = 0xffffff;

      constexpr Kind  delimiter_tok = value(Alphabetic::Delimiter);
      constexpr Kind literal_boolean_tok = value(Alphabetic::LiteralBoolean);
      constexpr Kind literal_character_tok = value(Alphabetic::LiteralCharacter);
      constexpr Kind literal_integer_tok = value(Alphabetic::LiteralInteger);
      constexpr Kind literal_string_tok = value(Alphabetic::LiteralString);
      constexpr Kind literal_real_tok = value(Alphabetic::LiteralReal);
         identifier_tok,                  // identifier
         div_tok,                         // "div"
         quo_tok,                         // "quo"
         mod_tok,                         // "mod"
         rem_tok,                         // "rem"
         is_tok,                          // "is"
         axiom_tok,                       // "axiom"
         prop_tok,                        // "prop"
         const_tok,                       // "const"
         if_tok,                          // "if"
         then_tok,                        // "then"
         else_tok,                        // "else"
         in_tok,                          // "in"
         match_tok,                       // "match"
         with_tok,                        // "with"
         case_tok,                        // "case"
         for_tok,                         // "for"
         while_tok,                       // "while"
         until_tok,                       // "until"
         repeat_tok,                      // "repeat"
         do_tok,                          // "do"
         return_tok,                      // "return"
         throw_tok,                       // "throw"
         leave_tok,                       // "leave"
         break_tok,                       // "break"
         variant_tok,                     // "variant"
         record_tok,                      // "record"
         requires_tok,                    // "requires"
         bool_tok,                        // "bool"
         byte_tok,                        // "byte"
         char_tok,                        // "char"
         int_tok,                         // "int"
         double_tok,                      // "double"
         string_tok,                      // "string"
         type_tok,                        // "type"
         not_tok,                         // "not"
         and_tok,                         // "and"
         or_tok,                          // "or"
         forall_tok,                      // "forall"
         exists_tok,                      // "exists"
         concept_tok,                     // "concept"
         namespace_tok,                   // "namespace"
         assert_tok,                      // "assert"
         assume_tok,                      // "assume"
         define_tok,                      // "define"
         where_tok,                       // "where"
         rule_tok,                        // "rule"
         import_tok,                      // "import"
         inductive_tok,                   // "inductive"
         coinductive_tok,                 // "coinductive"
         prolong_tok,                     // "prolong"
         postulate_tok,                   // "postulate"
         realize_tok,                     // "realize"
         indent_tok,                      // increase indentation level
         justify_tok,                     // match previous indentation
         unindent_tok,                    // decrease indentation level
         last_tok
      };
   }
}

#endif  // LIZ_RESERVED_WORDS_included
