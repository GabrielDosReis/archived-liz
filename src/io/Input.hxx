// Copyright (C) 2013, Texas A&M University.
// Copyright (C) 2014, Gabriel Dos Reis.
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

#ifndef LIZ_INPUT_DEFINED
#define LIZ_INPUT_DEFINED

#include <iterator>
#include <string>
#include <vector>
#include <liz/utility>

namespace liz {
   namespace input {
      // -- Text --
      using Text = std::string;

      // -- TextIterator --
      using TextIterator = Text::const_iterator;

      using ColumnNumber = Ordinal;
      using LineNumber = Ordinal;
      
      // -- Location --
      struct Location {
         LineNumber line;
         ColumnNumber column;
      };

      // -- Span --
      struct Span : structure::binary<ColumnNumber, Cardinal> {
         using Column = ColumnNumber;
         constexpr Span(ColumnNumber c, Cardinal l)
               : structure::binary<ColumnNumber, Cardinal>(c, l) { }
         constexpr Column column() const { return first(); }
         constexpr Cardinal length() const { return second(); }
      };

      using Column = Ordinal;
      using SourceRep = std::vector<Text>;
      struct Source;
      
      // -- Line --
      struct Line : defaults::neq<Line> {
         using value_type = Text::value_type;
         using const_reference = Text::const_reference;
         using const_pointer = Text::const_pointer;
         using difference_type = Text::difference_type;
         using iterator = liz::iterator::basic<Line>;
         using Number = LineNumber;
         const Text& text() const { return src->at(idx); }
         Number number() const { return 1 + idx; }
         Column indentation() const;
         iterator begin() const { return { this, 0 }; }
         iterator end() const { return { this, length() }; }
         bool blank() const;
         Cardinal length() const { return text().length(); }
         const_reference at(Ordinal n) const { return text().at(n); }
         const_reference operator[](Ordinal n) const { return at(n); }
         bool valid() const;
         explicit operator bool() const { return valid(); }
         bool operator==(Line that) const {
            return src == that.src and idx == that.idx;
         }
      private:
         friend Source;
         using TextRef = SourceRep::const_iterator;
         Line(const SourceRep* s, SourceRep::size_type i) : src(s), idx(i) { }
         const SourceRep* src;
         SourceRep::size_type idx;
      };

      // -- LineIterator --
      using LineIterator = Line::iterator;

      // -- Source --
      struct Source : private SourceRep {
         using super = SourceRep;
         using IndexType = size_type;
         using super::iterator;
         using super::const_iterator;
         using super::begin;
         using super::end;
         using super::size;
         using super::empty;
         using super::clear;
         using super::at;
         iterator find_text_at(LineNumber);
         Line line(size_type i) const { return { this, i }; }
         Line append(const Text&);
      };
   }
}

#endif  // LIZ_INPUT_DEFINED

