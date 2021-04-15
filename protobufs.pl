/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, Jeffrey Rosenwald
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(protobufs,
          [ protobuf_message/2,   % ?Template ?Codes
            protobuf_message/3,   % ?Template ?Codes ?Rest
            protobuf_segment_message/2,  % ?Segment ?Message
            int32_codes/2,
            float32_codes/2,
            int64_codes/2,
            float64_codes/2,
            integer_zigzag/2
          ]).
:- autoload(library(error),[must_be/2]).
:- autoload(library(lists),[append/3]).
:- autoload(library(utf8),[utf8_codes/3]).

/** <module> Google's Protocol Buffers

Protocol  buffers  are  Google's    language-neutral,  platform-neutral,
extensible mechanism for serializing structured data  --  think XML, but
smaller, faster, and simpler. You define how   you  want your data to be
structured once. This takes the form of   a  template that describes the
data structure. You use this template  to   encode  and decode your data
structure into wire-streams that may be sent-to or read-from your peers.
The underlying wire stream is platform independent, lossless, and may be
used to interwork with a variety of  languages and systems regardless of
word size or endianness. Techniques  exist   to  safely extend your data
structure without breaking deployed programs   that are compiled against
the "old" format.

The idea behind Google's  Protocol  Buffers   is  that  you  define your
structured messages using a domain-specific language   and  tool set. In
SWI-Prolog, you define your message  template   as  a list of predefined
Prolog terms that correspond to production  rules in the Definite Clause
Grammar (DCG) that realizes the interpreter. Each production rule has an
equivalent rule in the  protobuf  grammar.   The  process  is not unlike
specifiying the format of a regular  expression. To encode a template to
a wire-stream, you pass a grounded template, =X=, and  variable, =Y=, to
protobuf_message/2. To decode a wire-stream, =Y=, you pass an ungrounded
template, =X=,  along  with  a   grounded    wire-stream,   =Y=,  to
protobuf_message/2. The interpreter will unify  the unbound variables in
the template with values decoded from the wire-stream.

For an overview and tutorial with examples, see
[[protobufs_overview.md][https://github.com/SWI-Prolog/contrib-protobufs/blob/master/protobufs_overview.md]].
Examples of usage may also be found by inspecting
[[test_protobufs.pl][https://github.com/SWI-Prolog/contrib-protobufs/blob/master/test_protobufs.pl]]
and the
[[demo][https://github.com/SWI-Prolog/contrib-protobufs/tree/master/demo]]
directory.

@see https://developers.google.com/protocol-buffers
@see https://developers.google.com/protocol-buffers/docs/encoding
@author: Jeffrey Rosenwald (JeffRose@acm.org)
@author: Peter Ludemann (peter.ludemann@gmail.org)
@compat: SWI-Prolog
*/

:- use_foreign_library(foreign(protobufs)).

%
% Map wire type (atom) to its encoding (an int)
%
wire_type(varint, 0).  % for int32, int64, uint32, uint64, sint32, sint64, bool, enum
wire_type(fixed64, 1). % for fixed64, sfixed64, double
wire_type(length_delimited, 2). % for string, bytes, embedded messages, packed repeated fields
wire_type(start_group, 3). % for groups (deprecated)
wire_type(end_group, 4). % for groups (deprecated)
wire_type(fixed32, 5). % for fixed32, sfixed32, float

%
%  basic wire-type processing handled by C-support code
%

fixed_int32(X, [A0, A1, A2, A3 | Rest], Rest) :-
    int32_codes(X, [A0, A1, A2, A3]).

fixed_int64(X, [A0, A1, A2, A3, A4, A5, A6, A7 | Rest], Rest) :-
    int64_codes(X, [A0, A1, A2, A3, A4, A5, A6, A7]).

fixed_float64(X, [A0, A1, A2, A3, A4, A5, A6, A7 | Rest], Rest) :-
    float64_codes(X, [A0, A1, A2, A3, A4, A5, A6, A7]).

fixed_float32(X, [A0, A1, A2, A3 | Rest], Rest) :-
    float32_codes(X, [A0, A1, A2, A3]).

%
%   Start of the DCG
%

code_string(N, Codes, Rest, Rest1) :-
    length(Codes, N),
    append(Codes, Rest1, Rest),
    !.
/*
code_string(N, Codes) -->
        { length(Codes, N) },
        Codes, !.
*/

%
% deal with Google's method of packing unsigned integers in variable
% length, modulo 128 strings.
%
% var_int//1 and tag_type//2 productions were rewritten in straight
% Prolog for speed's sake.
%

%! var_int(?A:int, ?Rest:list, ?Rest1:list) is det.
% Conversion between an int A and a list of codes, using the
% "varint" encoding.
% Rest, Rest1 are a DCG difference list
% e.g. var_int(300, S, []) => S = [172,2]
%      var_int(A, [172,2]) -> A = 300
var_int(A, [A | Rest], Rest) :-
    A < 128,
    !.
var_int(X, [A | Rest], Rest1) :-
    nonvar(X),
    X1 is X >> 7,
    A is 128 + (X /\ 0x7f),
    var_int(X1, Rest, Rest1),
    !.
var_int(X, [A | Rest], Rest1) :-
    var_int(X1, Rest, Rest1),
    X is (X1 << 7) + A - 128,
    !.

%! tag_type(?Tag:int, ?Type:atom, ?Rest, Rest1) is det.
% Conversion between Tag (number) + Type and list of codes.
% Tag: The item's tag (field number)
% Type: The item's type (see prolog_type//2)
% Rest, Rest1 are a DCG difference list
tag_type(Tag, Type, Rest, Rest1) :-
    nonvar(Tag), nonvar(Type),
    wire_type(Type, X),
    A is Tag << 3 \/ X,
    var_int(A, Rest, Rest1),
    !.
tag_type(Tag, Type, Rest, Rest1) :-
    var_int(A, Rest, Rest1),
    X is A /\ 0x07,
    wire_type(Type, X),
    Tag is A >> 3.

%! prolog_type(?Tag:int, ?Type:atom) is semidet.
% Convert between Tag (field number) + Type and list of codes.
% When Type is a variable, backtracks through all the possibilities
% for a given wire encoding.
prolog_type(Tag, double) -->     tag_type(Tag, fixed64).
prolog_type(Tag, integer64) -->  tag_type(Tag, fixed64).
prolog_type(Tag, float) -->      tag_type(Tag, fixed32).
prolog_type(Tag, integer32) -->  tag_type(Tag, fixed32).
prolog_type(Tag, integer) -->    tag_type(Tag, varint).
prolog_type(Tag, unsigned) -->   tag_type(Tag, varint).
prolog_type(Tag, boolean) -->    tag_type(Tag, varint).
prolog_type(Tag, enum) -->       tag_type(Tag, varint).
prolog_type(Tag, atom) -->       tag_type(Tag, length_delimited).
prolog_type(Tag, codes) -->      tag_type(Tag, length_delimited).
prolog_type(Tag, utf8_codes) --> tag_type(Tag, length_delimited).
prolog_type(Tag, string) -->     tag_type(Tag, length_delimited).
prolog_type(Tag, embedded) -->   tag_type(Tag, length_delimited).
%
%   The protobuf-2.1.0 grammar allows negative values in enums.
%   But they are encoded as unsigned in the  golden message.
%   Encode as integer and lose. Encode as unsigned and win.
%
:- meta_predicate enumeration(1,*,*).

enumeration(Type) -->
    { call(Type, Value) },
    payload(unsigned, Value).

payload(enum, A) -->
    enumeration(A).
payload(double,  A) -->
    fixed_float64(A).
payload(integer64, A) -->
    fixed_int64(A).
payload(float, A) -->
    fixed_float32(A).
payload(integer32, A) -->
    fixed_int32(A).
payload(integer, A) -->
    { nonvar(A), integer_zigzag(A,X) },
    !,
    var_int(X).
payload(integer, A) -->
    var_int(X),
    { integer_zigzag(A, X) }.
payload(unsigned, A) -->
    {   nonvar(A)
    ->  A >= 0
    ;   true
    },
    var_int(A).
payload(codes, A) -->
    { nonvar(A), !, length(A, Len)},
    var_int(Len),
    code_string(Len, A).
payload(codes, A) -->
    var_int(Len),
    code_string(Len, A).
payload(utf8_codes, A) -->
    { nonvar(A),
      !,
      phrase(utf8_codes(A), B)
    },
    payload(codes, B).
payload(utf8_codes, A) -->
    payload(codes, B),
    { phrase(utf8_codes(A), B) }.
payload(atom, A) -->
    { nonvar(A),
      atom_codes(A, Codes)
    },
    payload(utf8_codes, Codes),
    !.
payload(atom, A) -->
    payload(utf8_codes, Codes),
    { atom_codes(A, Codes) }.
payload(boolean, true) -->
    payload(unsigned, 1).
payload(boolean, false) -->
    payload(unsigned, 0).
payload(string, A) -->
    {   nonvar(A)
    ->  string_codes(A, Codes)
    ;   true
    },
    payload(codes, Codes),
    { string_codes(A, Codes) }.
payload(embedded, protobuf(A)) -->
    { ground(A),
      phrase(protobuf(A), Codes)
    },
    payload(codes, Codes),
    !.
payload(embedded, protobuf(A)) -->
    payload(codes, Codes),
    { phrase(protobuf(A), Codes) }.

start_group(Tag) -->            tag_type(Tag, start_group).

end_group(Tag) -->              tag_type(Tag, end_group).
%
%
nothing([]) --> [], !.

protobuf([A | B]) -->
    { A =.. [ Type, Tag, Payload] },
    message_sequence(Type, Tag, Payload),
    !,
    (   protobuf(B)
    ;   nothing(B)
    ).

repeated_message_sequence(repeated_enum, Tag, Type, [A | B]) -->
    { Compound =.. [Type, A] },
    message_sequence(enum, Tag, Compound),
    (   repeated_message_sequence(repeated_enum, Tag, Type, B)
    ;   nothing(B)
    ).
repeated_message_sequence(Type, Tag, [A | B]) -->
    message_sequence(Type, Tag, A),
    repeated_message_sequence(Type, Tag, B).
repeated_message_sequence(_Type, _Tag, A) -->
    nothing(A).

message_sequence(repeated, Tag, enum(Compound)) -->
    { Compound =.. [ Type, List] },
    repeated_message_sequence(repeated_enum, Tag, Type, List).
message_sequence(repeated, Tag, Compound) -->
    { Compound =.. [Type, A] },
    repeated_message_sequence(Type, Tag, A).
message_sequence(group, Tag, A) -->
    start_group(Tag),
    protobuf(A),
    end_group(Tag),
    !.
message_sequence(PrologType, Tag, Payload) -->
    prolog_type(Tag, PrologType),
    payload(PrologType, Payload).

%!  protobuf_message(?Template, ?Wire_stream) is semidet.
%!  protobuf_message(?Template, ?Wire_stream, ?Rest) is nondet.
%
%   Marshals  and  unmarshals   byte  streams  encoded  using   Google's
%   Protobuf  grammars.  protobuf_message/2  provides  a  bi-directional
%   parser that marshals a Prolog   structure to Wire_stream,  according
%   to rules specified by Template. It   can also unmarshal  Wire_stream
%   into  a  Prolog   structure   according    to   the   same  grammar.
%   protobuf_message/3 provides a difference list version.
%
%   @bug The protobuf specification states that the wire-stream can have
%   the fields in any order and that unknown fields are to be ignored.
%   This implementation assumes that the fields are in the exact order
%   of the definition and match exactly.
%
%   @bug "Packed" repeated fields are not supported.
%
%   @param Template is a  protobuf   grammar  specification.  On decode,
%   unbound variables in the Template are  unified with their respective
%   values in the Wire_stream. On encode, Template must be ground.
%
%   @param Wire_stream is a code list that   was generated by a protobuf
%   encoder using an equivalent template.

protobuf_message(protobuf(Template), Wirestream) :-
    must_be(list, Template),
    phrase(protobuf(Template), Wirestream),
    !.

protobuf_message(protobuf(Template), Wirestream, Residue) :-
    must_be(list, Template),
    phrase(protobuf(Template), Wirestream, Residue).

%!  protobuf_segment_message(+Segments:list, -WireStream:list(int)) is det.
%!  protobuf_segment_message(-Segments:list, +WireStream:list(int)) is det.
%
%  Low level marshalling and unmarshalling of byte streams. The
%  processing is independent of the =|.proto|= description. As such,
%  it cannot decode 32-bit and 64-bit numbers because they could be
%  either integers or floating point; nor variable-length numbers
%  ("varint" in the [[Protocol Buffers encoding
%  documentation][https://developers.google.com/protocol-buffers/docs/encoding#varints]]),
%  because it doesn't know whether or not to apply the "zigzag"
%  conversion. To finish processing a value, use the predicates
%  int32_codes/2, float32_codes/2,
%  int64_codes/2, float64_codes/2,
%  integer_zigzag/2.
%
%  @bug This predicate is preliminary and may change as additional
%  functionality is added.
%
%  @tbd Expansion of this code to allow inputting of wire streams with
%  fields in arbitrary order. (See bugs for protobuf_message/2).
%
%  @param Segment a list is made up of terms of the following form (=Tag= is
%  the field number; =Codes= is a list of integers):
%    * varint(Tag,Varint)
%    * fixed64(Tag,Codes) - =Codes= is of length 8
%    * length_delimited(Tag,Len,Codes) - =Codes= is of length =Len=
%    * start_group(Tag)
%    * end_group(Tag)
%    * fixed32(Tag,Codes) - =Codes= is of length 4
%  Of these, =start_group= and =end_group= are deprecated in the
%  protobuf documentation and shouldn't appear in modern code, having
%  been replaced by nested message types.
%
%  @param WireStream a code list that was generated by a protobuf
%  endoder.
%
%  @see https://developers.google.com/protocol-buffers/docs/encoding

%! int32_codes(?Value, ?Codes) is det.
% Convert between a 32-bit integer value and its wirestream codes.
%
% @param Value an integer
% @param Codes a list of 4 integers

%! float32_codes(?Value, ?Codes) is det.
% Convert between a 32-bit floating point value and its wirestream codes.
%
% @param Value a floating point number
% @param Codes a list of 4 integers

%! int64_codes(?Value, ?Codes) is det.
% Convert between a 64-bit integer value and its wirestream codes.
%
% @param Value an integer
% @param Codes a list of 8 integers

%! float64_codes(?Value, ?Codes) is det.
% Convert between a 64-bit floating point value and its wirestream codes.
%
% @param Value a floating point number
% @param Codes a list of 8 integers

%! integer_zigzag(?Original, ?Encoded) is det.
% Convert between an integer value and its zigzag encoding
%
% @param Original an integer in the original form
% @param Encoded the zigzag encoding of =Original=


protobuf_segment_message(Segments, WireStream) :-
    phrase(segment_message(Segments), WireStream),
    !. % var(Segments) leaves a choicepoint from the DCG

segment_message([]) --> [].
segment_message([Segment|Segments]) -->
    {   nonvar(Segment)
    ->  segment_type_tag(Segment, Type, Tag)
    ;   true
    },
    tag_type(Tag, Type),
    segment(Type, Tag, Segment),
    segment_message(Segments).

segment(varint, Tag, varint(Tag,Value)) -->
    var_int(Value).
segment(fixed64, Tag, fixed64(Tag, [A0,A1,A2,A3,A4,A5,A6,A7])) -->
    [A0, A1, A2, A3, A4, A5, A6, A7].
segment(length_delimited, Tag, length_delimited(Tag,N,Codes)) -->
    var_int(N),
    code_string(N, Codes).
segment(start_group, Tag, start_group(Tag)) --> [].
segment(end_group, Tag, end_group(Tag)) --> [].
segment(fixed32, Tag, fixed32(Tag, [A0,A1,A2,A3])) -->
    [A0, A1, A2, A3].

segment_type_tag(varint(Tag,_Codes),                varint, Tag).
segment_type_tag(fixed64(Tag,_Codes),               fixed64, Tag).
segment_type_tag(length_delimited(Tag,_Len,_Codes), length_delimited, Tag).
segment_type_tag(start_group(Tag),                  start_group, Tag).
segment_type_tag(end_group(Tag),                    end_group, Tag).
segment_type_tag(fixed32(Tag,_Codes),               fixed32, Tag).

