/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, extended by Peter Ludemann
    E-mail:        jeffrose@acm.org, peter.ludemann.gmail.com
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
            protobuf_segment_message/2,  % ?Segments ?Codes
            protobuf_segment_convert/2,  % +Form1 ?Form2
            protobuf_parse_from_codes/3, % +WireCodes, +MessageType, -Term
            int32_codes/2,
            float32_codes/2,
            int64_codes/2,
            float64_codes/2,
            integer_zigzag/2,
            protobuf_var_int//1,
            protobuf_tag_type//2
          ]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(lists), [append/3]).
:- autoload(library(utf8), [utf8_codes/3]).
:- autoload(library(dif), [dif/2]).
:- autoload(library(dcg/high_order), [sequence//2]).

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
[library(protobufs): Google's Protocol Buffers](#protobufs-main)
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
wire_type(varint,            0). % for int32, int64, uint32, uint64, sint32, sint64, bool, enum
wire_type(fixed64,           1). % for fixed64, sfixed64, double
wire_type(length_delimited,  2). % for string, bytes, embedded messages, packed repeated fields
wire_type(start_group,       3). % for groups (deprecated)
wire_type(end_group,         4). % for groups (deprecated)
wire_type(fixed32,           5). % for fixed32, sfixed32, float

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
% protobuf_var_int//1 and protobuf_tag_type//2 productions were rewritten in straight
% Prolog for speed's sake.
%

%! protobuf_var_int(?A:int)// is det.
% Conversion between an int A and a list of codes, using the
% "varint" encoding.
% The behvior is undefined if =A= is negative.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
% e.g. phrase(protobuf_var_int(300), S) => S = [172,2]
%      phrase(protobuf_var_int(A), [172,2]) -> A = 300
protobuf_var_int(A, [A | Rest], Rest) :-
    A < 128,
    !.
protobuf_var_int(X, [A | Rest], Rest1) :-
    nonvar(X),
    X1 is X >> 7,
    A is 128 + (X /\ 0x7f),
    protobuf_var_int(X1, Rest, Rest1),
    !.
protobuf_var_int(X, [A | Rest], Rest1) :-
    protobuf_var_int(X1, Rest, Rest1),
    X is (X1 << 7) + A - 128,
    !.

%! protobuf_tag_type(?Tag:int, ?Type:atom, ?Rest, Rest1) is det.
% Conversion between Tag (number) + Type and list of codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
% @arg Tag The item's tag (field number)
% @arg Type The item's type (see prolog_type//2)
% @arg Rest, @arg Rest1 DCG difference list
protobuf_tag_type(Tag, Type, Rest, Rest1) :-
    nonvar(Tag), nonvar(Type),
    wire_type(Type, X),
    A is Tag << 3 \/ X,
    protobuf_var_int(A, Rest, Rest1),
    !.
protobuf_tag_type(Tag, Type, Rest, Rest1) :-
    protobuf_var_int(A, Rest, Rest1),
    X is A /\ 0x07,
    wire_type(Type, X),
    Tag is A >> 3.

%! prolog_type(?Tag:int, ?Type:atom) is semidet.
% Convert between Tag (field number) + Type and list of codes.
% When Type is a variable, backtracks through all the possibilities
% for a given wire encoding.
% Note that 'repeated' isn't here because it's handled by message_sequence//3.
% See also segment_type_tag/3.
prolog_type(Tag, double) -->     protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, integer64) -->  protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, float) -->      protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, integer32) -->  protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, integer) -->    protobuf_tag_type(Tag, varint).
prolog_type(Tag, unsigned) -->   protobuf_tag_type(Tag, varint).
%                signed32  - omitted for wire-stream compabitility
prolog_type(Tag, signed64) -->   protobuf_tag_type(Tag, varint).
prolog_type(Tag, boolean) -->    protobuf_tag_type(Tag, varint).
prolog_type(Tag, enum) -->       protobuf_tag_type(Tag, varint).
prolog_type(Tag, atom) -->       protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, codes) -->      protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, utf8_codes) --> protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, string) -->     protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, embedded) -->   protobuf_tag_type(Tag, length_delimited).
prolog_type(Tag, packed) -->     protobuf_tag_type(Tag, length_delimited).

%
%   The protobuf-2.1.0 grammar allows negative values in enums.
%   But they are encoded as unsigned in the  golden message.
%   Encode as integer and lose. Encode as unsigned and win.
%
:- meta_predicate enumeration(1,?,?).

enumeration(Type) -->
    { call(Type, Value) },
    payload(unsigned, Value).

% TODO: payload//2 "mode" is sometimes module-sensitive, sometimes not.
%       payload(enum, A)// has A as a callable
%       all other uses of payload//2, the 2nd arg is not callable.
%     - This confuses check/0; it also makes defining an enumeration
%       more difficult because it has to be defined in module protobufs
%       (see vector_demo.pl, which defines protobufs:commands/2)

payload(enum, A) -->
    enumeration(A).
payload(double, A) -->
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
    protobuf_var_int(X).
payload(integer, A) -->
    protobuf_var_int(X),
    { integer_zigzag(A, X) }.
payload(unsigned, A) -->
    % TODO: replace payload(unsigned, A) with this (needs unit tests +
    %       comparison with Python,C++):
    %   payload(unsigned, A) -->
    %       protobuf_var_int(A),
    %       { A >= 0 }.
    {   nonvar(A)
    ->  A >= 0
    ;   true
    },
    protobuf_var_int(A).
payload(signed32, A) --> % signed32 is not defined by prolog_type//2
                         % for wire-stream compatibility reasons.
%     % signed32 ought to write 5 bytes for negative numbers, but both
%     % the C++ and Python implementations write 10 bytes. For
%     % wire-stream compatibility, we follow C++ and Python, even though
%     % protoc decode appears to work just fine with 5 bytes --
%     % presumably there are some issues with decoding 5 bytes and
%     % getting the sign extension correct with some 32/64-bit integer
%     % models.  See CodedOutputStream::WriteVarint32SignExtended(int32
%     % value) in google/protobuf/io/coded_stream.h.  (To write 5 bytes,
%     % change the "A xor % 0xffffffffffffffff" to "A xor 0xffffffff".)
      payload(signed64, A).
payload(signed64, A) -->
    % protobuf_var_int//1 cannot handle negative numbers (note that
    % zig-zag encoding always results in a positive number), so
    % compute the 64-bit 2s complement, which is what is produced
    % form C++ and Python.
    % TODO: \A instead of A xor ...?
    { var(A) },
    !,
    protobuf_var_int(X),
    {   X > 0x7fffffffffffffff
    ->  A is -(X xor 0xffffffffffffffff + 1)
    ;   A = X
    }.
payload(signed64, A) -->
    % See comment in previous clause about negative numbers.
    {   nonvar(A), A < 0
    ->  X is -(A xor 0xffffffffffffffff + 1)
    ;   X = A
    },
    protobuf_var_int(X).
payload(codes, A) -->
    { nonvar(A), !, length(A, Len)},
    protobuf_var_int(Len),
    code_string(Len, A).
payload(codes, A) -->
    protobuf_var_int(Len),
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
    % string_codes produces a list of unicode, not bytes, e.g.
    % string_codes('écran 網目錦蛇', [233,99,114,97,110,32,32178,30446,37670,34503])
    % so don't use payload(codes, Codes):
    payload(utf8_codes, Codes),
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
payload(packed, Compound) -->
    { Compound =.. [Type, A],
      ground(A),
      phrase(packed_payload(Type, A), Codes)
    },
    payload(codes, Codes),
    !.
payload(packed, Compound) -->
    payload(codes, Codes),
    { Compound =.. [Type, A] },
    { phrase(packed_payload(Type, A), Codes) }.

packed_payload(Type, Codes) -->
    sequence(payload(Type), Codes).

start_group(Tag) --> protobuf_tag_type(Tag, start_group).

end_group(Tag) -->   protobuf_tag_type(Tag, end_group).
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
    { Compound =.. [Type, List] },
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

%!  protobuf_message(?Template, ?WireStream) is semidet.
%!  protobuf_message(?Template, ?WireStream, ?Rest) is nondet.
%
%   Marshals  and  unmarshals   byte  streams  encoded  using   Google's
%   Protobuf  grammars.  protobuf_message/2  provides  a  bi-directional
%   parser that marshals a Prolog   structure to WireStream,  according
%   to rules specified by Template. It   can also unmarshal  WireStream
%   into  a  Prolog   structure   according    to   the   same  grammar.
%   protobuf_message/3 provides a difference list version.
%
%   @bug The protobuf specification states that the wire-stream can have
%   the fields in any order and that unknown fields are to be ignored.
%   This implementation assumes that the fields are in the exact order
%   of the definition and match exactly.
%
%   @bug "Packed" repeated enums are not supported.
%
%   @param Template is a  protobuf   grammar  specification.  On decode,
%   unbound variables in the Template are  unified with their respective
%   values in the WireStream. On encode, Template must be ground.
%
%   @param WireStream is a code list that   was generated by a protobuf
%   encoder using an equivalent template.

protobuf_message(protobuf(TemplateList), WireStream) :-
    must_be(list, TemplateList),
    phrase(protobuf(TemplateList), WireStream),
    !.

protobuf_message(protobuf(TemplateList), WireStream, Residue) :-
    must_be(list, TemplateList),
    phrase(protobuf(TemplateList), WireStream, Residue).

%! protobuf_segment_message(+Segments:list, -WireStream:list(int)) is det.
%! protobuf_segment_message(-Segments:list, +WireStream:list(int)) is det.
%
%  Low level marshalling and unmarshalling of byte streams. The
%  processing is independent of the =|.proto|= description, similar
%  to the processing done by =|protoc --decode_raw|=. This means that
%  field names aren't shown: only field numbers.
%
%  For unmarshalling, a simple heuristic is used on length-delimited
%  segments: first interpret it as a message; if that fails, try to
%  interpret as a UTF8 string; otherwise, leave it as a "blob" (if the
%  heuristic was wrong, you can convert to a string or a blob by using
%  protobuf_segment_convert/2).  32-bit and 64-bit numbers are left as
%  codes because they could be either integers or floating point (use
%  int32_codes/2, float32_codes/2, int64_codes/2, float64_codes/2 as
%  appropriate); variable-length numbers ("varint" in the [[Protocol
%  Buffers encoding
%  documentation][https://developers.google.com/protocol-buffers/docs/encoding#varints]]),
%  might require "zigzag" conversion, integer_zigzag/2.
%
%  For marshalling, use the predicates int32_codes/2, float32_codes/2,
%  int64_codes/2, float64_codes/2, integer_zigzag/2 to put integer
%  and floating point values into the appropriate form.
%
%  @bug This predicate is preliminary and may change as additional
%       functionality is added.
%  @bug Does not support [groups](https://developers.google.com/protocol-buffers/docs/proto#groups)
%       (deprecated in the protobuf documentation).
%  @tbd Expansion of this code to allow generalized handling of wire
%       streams with fields in arbitrary order. (See bugs for
%       protobuf_message/2).
%  @tbd Functionality similar to =|protoc --decode|=, which will
%       use field names rather than field numbers and also
%       will not need heuristics to guess at segment types because
%       it will have the correct types from the .proto definition.
%
%  @param Segments a list containing terms of the following form (=Tag= is
%  the field number; =Codes= is a list of integers):
%    * varint(Tag,Varint) - =Varint= may need integer_zigzag/2
%    * fixed64(Tag,Codes) - =Codes= is of length 8, (un)marshalled by int64_codes/2 or float64_codes/2
%    * fixed32(Tag,Codes) - =Codes= is of length 4, (un)marshalled by int32_codes/2 or float32_codes/2
%    * message(Tag,Segments)
%    * string(Tag,String) - =String= is a SWI-Prolog string
%    * packed(Tag,Type(Scalars)) - =Type= is one of
%             =varint=, =fixed64=, =fixed32=; =Scalars=
%             is a list of =Varint= or =Codes=, which should
%             be interpreted as described under those items.
%             Note that the protobuf specification does not
%             allow packed repeated string.
%    * start_group(Tag) - not supported, but included for completeness
%    * end_group(Tag) - not supported, but included for completeness
%    * length_delimited(Tag,Codes)
%  Of these, =start_group= and =end_group= are deprecated in the
%  protobuf documentation and shouldn't appear in modern code, because
%  they have been replaced by nested message types.
%
%  For deciding how to interpret a length-delimited item (when
%  =Segments= is a variable), an attempt is made to parse the item in
%  the following order (although code should not rely on this order):
%    * message
%    * string (it must be of the form of a UTF string)
%    * packed (which can backtrack through the various =Type=s)
%    * length_delimited - which always is possible.
%
%  The most likely interpretation of length-delimited items can
%  sometimes guess wrong; the interpretation can be undone by either
%  backtracking or by using protobuf_segment_convert/2 to convert the
%  incorrect segment to a string or a list of codes. Backtracking
%  through all the possibilities is not recommended, because of
%  combinatoric explosion (there is an example in the unit tests);
%  instead, it is suggested that you take the first result and iterate
%  through its items, calling protobuf_segment_convert/2 as needed
%  to reinterpret incorrectly guessed segments.
%
%  @param WireStream a code list that was generated by a protobuf
%  endoder.
%
%  @see https://developers.google.com/protocol-buffers/docs/encoding
protobuf_segment_message(Segments, WireStream) :-
    phrase(segment_message(Segments), WireStream).

segment_message(Segments) -->
    sequence(segment, Segments).

segment(Segment) -->
    { var(Segment) },
    !,
    protobuf_tag_type(Tag, Type),
    segment(Type, Tag, Segment).
segment(Segment) -->
    % { nonvar(Segment) },
    { segment_type_tag(Segment, Type, Tag) },
    protobuf_tag_type(Tag, Type),
    segment(Type, Tag, Segment).

% See also prolog_type//2
segment_type_tag(varint(Tag,_Codes),           varint,           Tag).
segment_type_tag(fixed64(Tag,_Codes),          fixed64,          Tag).
segment_type_tag(start_group(Tag),             start_group,      Tag).
segment_type_tag(end_group(Tag),               end_group,        Tag).
segment_type_tag(fixed32(Tag,_Codes),          fixed32,          Tag).
segment_type_tag(length_delimited(Tag,_Codes), length_delimited, Tag).
segment_type_tag(message(Tag,_Segments),       length_delimited, Tag).
segment_type_tag(packed(Tag,_Compound),        length_delimited, Tag).
segment_type_tag(string(Tag,_String),          length_delimited, Tag).

segment(varint, Tag, varint(Tag,Value)) -->
    protobuf_var_int(Value).
segment(fixed64, Tag, fixed64(Tag, [A0,A1,A2,A3,A4,A5,A6,A7])) -->
    [A0, A1, A2, A3, A4, A5, A6, A7].
segment(start_group, Tag, start_group(Tag)) --> [].
segment(end_group, Tag, end_group(Tag)) --> [].
segment(fixed32, Tag, fixed32(Tag, [A0,A1,A2,A3])) -->
    [A0, A1, A2, A3].
segment(length_delimited, Tag, Result) -->
    segment_length_delimited(Tag, Result).

segment_length_delimited(Tag, Result) -->
    { nonvar(Result) },
    !,
    { length_delimited_segment(Result, Tag, Codes) },
    { length(Codes, CodesLen) },
    protobuf_var_int(CodesLen),
    code_string(CodesLen, Codes).
segment_length_delimited(Tag, Result) -->
    % { var(Result) },
    protobuf_var_int(CodesLen),
    code_string(CodesLen, Codes),
    { length_delimited_segment(Result, Tag, Codes) }.

length_delimited_segment(message(Tag,Segments), Tag, Codes) :-
    (   var(Segments)
    ->  protobuf_segment_message(Segments, Codes),
        % heuristic: start_group, end_group are deprecated, so a
        % message shouldn't contain them.
        % TODO: A more precise check would be that
        % start_group(Tag)/end_group(Tag) appear properly nested, as
        % in message_sequence(group, Tag, A).
        \+ memberchk(start_group(_), Segments),
        \+ memberchk(end_group(_), Segments)
    ;   protobuf_segment_message(Segments, Codes)
    ).
length_delimited_segment(string(Tag,String), Tag, Codes) :-
    (   nonvar(String)
    ->  string_codes(String, StringCodes),
        phrase(utf8_codes(StringCodes), Codes)
    ;   phrase(utf8_codes(StringCodes), Codes),
        string_codes(String, StringCodes)
    ).
length_delimited_segment(packed(Tag,Compound), Tag, Codes) :-
    % We don't know the type of the fields, so we try the 3
    % possibilities.  This has a problem: an even number of fixed32
    % items can't be distinguished from half the number of fixed64
    % items; but it's all we can do. The good news is that usually
    % varint (possibly with zig-zag encoding) is more common because
    % it's more compact (I don't know whether 32-bit or 64-bit is more
    % common for floating point).
    packed_option(Type, Items, Compound),
    phrase(sequence(payload(Type), Items), Codes).
length_delimited_segment(length_delimited(Tag,Codes), Tag, Codes).

% See also prolog_type//2, but pick only one for each wirestream type
% For varint(Items), use one that doesn't do zigzag
packed_option(integer64, Items, fixed64(Items)).
packed_option(integer32, Items, fixed32(Items)).
packed_option(unsigned,  Items, varint(Items)).
% packed_option(integer,   Items, varint(Items)).
% packed_option(double,    Items, fixed64(Items)).
% packed_option(float,     Items, fixed32(Items)).
% packed_option(signed64,  Items, varint(Items)).
% packed_option(boolean,   Items, varint(Items)).
% packed_option(enum,      Items, varint(Items)).

%! protobuf_segment_convert(+Form1, ?Form2) is multi.
% A convenience predicate for dealing with the situation where
% protobuf_segment_message/2 interprets a segment of the wire stream
% as a form that you don't want (e.g., as a message but it should have
% been a UTF8 string).
%
% =Form1= is converted back to the original wire stream, then the
% predicate non-deterinisticly attempts to convert the wire stream to
% a =|string|= or =|length_delimited|= term (or both: the lattter
% always succeeds).
%
% The possible conversions are:
%   message(Tag,Segments) => string(Tag,String)
%   message(Tag,Segments) => length_delimited(Tag,Codes)
%   string(Tag,String) => length_delimited(Tag,Codes)
%   length_delimited(Tag,Codes) => length_delimited(Tag,Codes)
%
% For example:
% ==
% ?- protobuf_segment_convert(
%        message(10,[fixed64(13,[110,112,117,116,84,121,112,101])]),
%        string(10,"inputType")).
% ?- protobuf_segment_convert(
%        message(10,[fixed64(13,[110,112,117,116,84,121,112,101])]),
%        length_delimited(10,[105,110,112,117,116,84,121,112,101])).
% ?- protobuf_segment_convert(
%        string(10, "inputType"),
%        length_delimited(10,[105,110,112,117,116,84,121,112,101])).
% ==
% These come from:
% ==
% Codes = [82,9,105,110,112,117,116,84,121,112,101],
% protobuf_message(protobuf([embedded(T1, protobuf([integer64(T2, I)]))]), Codes),
% protobuf_message(protobuf([string(T,S)]), Codes).
%    T = 10, T1 = 10, T2 = 13,
%    I = 7309475598860382318,
%    S = "inputType".
% ==
%
%  @bug This predicate is preliminary and may change as additional
%       functionality is added.
%  @bug This predicate will sometimes generate unexpected choice points,
%       Such as from =|protobuf_segment_convert(message(10,...), string(10,...))|=
%  @bug When converting from a =|Form1=string|=, unnecessarily produces =Form2=string|=.
%
% @param Form1 =|message(Tag,Pieces)|= or =|string(Tag,String)|=.
% @param Form2 =|string(Tag,String)|= or =|length_delimited(Tag,Codes)|=.
protobuf_segment_convert(Form, Form). % for efficiency, don't generate codes
protobuf_segment_convert(Form1, Form2) :-
    dif(Form1, Form2), % Form1=Form2 already generated by first clause
    protobuf_segment_message([Form1], WireCodes),
    phrase(tag_and_codes(Tag, Codes), WireCodes),
    length_delimited_segment(Form2, Tag, Codes).

tag_and_codes(Tag, Codes) -->
    protobuf_tag_type(Tag, length_delimited),
    payload(codes, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Documention of the foreign predicates, which are re-exported

%! int32_codes(?Value, ?Codes) is det.
% Convert between a 32-bit integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% @param Value an integer
% @param Codes a list of 4 integers (codes)

%! float32_codes(?Value, ?Codes) is det.
% Convert between a 32-bit floating point value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% @param Value a floating point number
% @param Codes a list of 4 integers (codes)

%! int64_codes(?Value, ?Codes) is det.
% Convert between a 64-bit integer value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% @param Value an integer
% @param Codes a list of 8 integers (codes)

%! float64_codes(?Value, ?Codes) is det.
% Convert between a 64-bit floating point value and its wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% @param Value a floating point number
% @param Codes a list of 8 integers (codes)

%! integer_zigzag(?Original, ?Encoded) is det.
% Convert between an integer value and its zigzag encoding
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
%
% @param Original an integer in the original form
% @param Encoded the zigzag encoding of =Original=


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Use protobufs meta-data (from protoc --swipl_out=DIR) to parse
% protobuf wire format.
%
% TODO: protobuf_seralize_to_codes/3

% The protoc plugin generates these facts:

% TODO: rename to protobufs:protobuf_meta_package/3 etc.  DO NOT SUBMIT

:- multifile
     protobufs:package/3,             %   protobufs:package(Package, FileName, Options)
     protobufs:message_type/3,        %   protobufs:message_type(       Fqn,     Package, Name)
     protobufs:field_name/4,          %   protobufs:field_name(         Fqn,     FieldNumber, FieldName, FqnName),
     protobufs:field_json_name/2,     %   protobufs:field_json_name(    FqnName, JsonName)
     protobufs:field_label/2,         %   protobufs:field_label(        FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     protobufs:field_type/2,          %   protobufs:field_type(         FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     protobufs:field_type_name/2,     %   protobufs:field_type_name(    FqnName, TypeName)
     protobufs:field_default_value/2, %   protobufs:field_default_value(FqnName, DefaultValue)
     protobufs:field_option_packed/1, %   protobufs:field_option_packed(FqnName)
     protobufs:enum_type/3,           %   protobufs:enum_type(          FqnName, Fqn, Name)
     protobufs:enum_value/3.          %   protobufs:enum_value(         FqnName, Name, Number)

%! protobuf_parse_from_codes(+WireCodes:list, +MessageType:atom, -Term) is semidet.
%
% Fails if the message can't be parsed.
%
% @todo: document the generated terms (see library(http/json) and json_read/3
% @todo: add options such as =true= and =value_string_as=.
%
% @param WireCodes Wire format of the message from e.g., read_stream_to_codes/2.
%          (The stream should have options `encoding(octet)` and `type(binary)`,
%          either as options to read_file_to_codes/3 or by calling set_stream/2
%          on the stream to read_stream_to_codes/2.)
% @param MessageType Fully qualified message name (from the .proto file's =package= and =message=).
%        For example, if the =package= is =google.protobuf= and the
%        message is =FileDescriptorSet=, then you would use
%        ='.google.protobuf.FileDescriptorSet'=. You can see the message
%        names by looking at =protobufs:message_type(MessageType, _, _)=.
% @param Term The generated term.
protobuf_parse_from_codes(WireCodes, MessageType, Term) :-
    protobuf_segment_message(Segments, WireCodes),
    % protobuf_segment_message/2 can leave choicepoints, and we don't
    % want to backtrack through all the possibilities because that
    % leads to combinatoric explosion; instead use
    % protobuf_segment_convert/2 to change segments that were guessed
    % incorrectly.
    !, % don't use any other possible Segments - let protobuf_segment_convert/2 do the job
    % See convert_segment('TYPE_MESSAGE, ...):
    maplist(segment_to_term(MessageType), Segments, MsgFields),
    combine_fields(MsgFields, MessageType{}, Term).

% :- det(segment_to_term/3).  % DO NOT SUBMIT
%! segment_to_term(+ContextType:atom, +Segment, -FieldAndValue) is det.
% ContextType is the type (name) of the containing message
% Segment is a segment from protobuf_segment_message/2
% TODO: if performance is an issue, this code can be combined with
%       protobuf_segment_message/2 (and thereby avoid the use of protobuf_segment_convert/2)
segment_to_term(ContextType0, Segment0, FieldAndValue) =>
    segment_type_tag(Segment0, _, Tag),
    field_and_type(ContextType0, Tag, FieldName, _FqnName, ContextType, RepeatOptional, Type),
    (   RepeatOptional = repeat_packed
    ->  convert_segment(packed(Type), ContextType, Segment0, Segment)
    ;   convert_segment(Type, ContextType, Segment0, Segment)
    ),
    !, % remove choicepoint from convert_segment/2
    FieldAndValue = field_and_value(FieldName,RepeatOptional,Segment).

% :- det(convert_segment/4).  % DO NOT SUBMIT
%! convert_segment(+Type:atom, +Segment, -Value) is det.
% Compute an appropriate =Value= from the combination of descriptor
% "type" (in =Type=) and a =Segment=.
convert_segment('TYPE_DOUBLE', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    float64_codes(Value, Codes).
convert_segment(packed('TYPE_DOUBLE'), _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed64(Codes))),
    phrase(packed_payload(double, Values), Codes), !.
convert_segment('TYPE_FLOAT', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    float32_codes(Value, Codes).
convert_segment(packed('TYPE_FLOAT'), _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed32(Codes))),
    phrase(packed_payload(float, Values), Codes), !.
convert_segment('TYPE_INT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment(packed('TYPE_INT64'), _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed64(Codes))),
    phrase(packed_payload(fixed_int64, Values), Codes), !.
convert_segment('TYPE_UINT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment(packed('TYPE_UINT64'), _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Codes))),
    phrase(packed_payload(unsigned, Values), Codes), !.
convert_segment('TYPE_INT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment(packed('TYPE_INT32'), _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values))).
% DO NOT SUBMIT - add packed(...) from here on
convert_segment('TYPE_FIXED64', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int64_codes(Value, Codes).
convert_segment('TYPE_FIXED32', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int32_codes(Value, Codes).
convert_segment('TYPE_BOOL', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    int_bool(Value0, Value).
convert_segment('TYPE_STRING', _ContextType, Segment0, Value) =>
    Segment = string(_,ValueStr),
    protobuf_segment_convert(Segment0, Segment), !,
    (   true     % TODO: control whether atom or string with an option
    ->  atom_string(Value, ValueStr)
    ;   Value = ValueStr
    ).
convert_segment('TYPE_GROUP', _ContextType, _Segment0, _Value) =>
    fail. % TODO - for now, this should throw an exception because of :- det(convert_segment/4).
convert_segment('TYPE_MESSAGE', ContextType, Segment0, Value) =>
    Segment = message(_,MsgSegments),
    protobuf_segment_convert(Segment0, Segment), !,
    maplist(segment_to_term(ContextType), MsgSegments, MsgFields),
    combine_fields(MsgFields, ContextType{}, Value).
convert_segment('TYPE_BYTES', _ContextType, Segment0, Value) =>
    Segment = length_delimited(_,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_ENUM', ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    protobufs:enum_value(ContextType, Value, Value0). % meta data TODO: rename to protobuf_meta_enum_value
convert_segment('TYPE_SFIXED32', _ContextType, Segment0, Value) =>
    Segment = fixed32(_,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int32_codes(Value, Codes).
convert_segment('TYPE_SFIXED64', _ContextType, Segment0, Value) =>
    Segment = fixed64(_,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int64_codes(Value, Codes).
convert_segment('TYPE_SINT32', _ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    integer_zigzag(Value, Value0).
convert_segment('TYPE_SINT64', _ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    integer_zigzag(Value, Value0).

% TODO: use options to translate to/from false, true (see json_read/3)
int_bool(0, false).
int_bool(1, true).


:- det(combine_fields/3).
%! combine_fields(+Fields:list, +MsgDict0, -MsgDict) is det.
% Combines the fields into a dict.
% If the field is marked as 'norepeat' (optional/required), then the last
%    occurrence is kept (as per the protobuf wire spec)
% If the field is marked as 'repeat', then all the occurrences
%    are put into a list, in order.
% Assume that fields normally occur all together, but can handle
% (less efficiently) fields not occurring together, as is allowed
% by the protobuf spec.
combine_fields([], MsgDict0, MsgDict) => MsgDict = MsgDict0.
combine_fields([field_and_value(Field,norepeat,Value)|Fields], MsgDict0, MsgDict) =>
    put_dict(Field, MsgDict0, Value, MsgDict1),
    combine_fields(Fields, MsgDict1, MsgDict).
combine_fields([field_and_value(Field,repeat_packed,Value)|Fields], MsgDict0, MsgDict) =>
    combine_fields([field_and_value(Field,repeat,Value)|Fields], MsgDict0, MsgDict).
combine_fields([field_and_value(Field,repeat,Value)|Fields], MsgDict0, MsgDict) =>
    combine_fields_repeat(Fields, Field, NewValues, RestFields),
    (   get_dict(Field, MsgDict0, ExistingValues)
    ->  append(ExistingValues, [Value|NewValues], Values)
    ;   Values = [Value|NewValues]
    ),
    put_dict(Field, MsgDict0, Values, MsgDict1),
    combine_fields(RestFields, MsgDict1, MsgDict).

:- det(combine_fields_repeat/4).
%! combine_fields_repeat(+Fields:list, Field:atom, -Values:list, RestFields:list) is det.
% Helper for combine_fields/3
% Stops at the first item that doesn't match =Field= - the assumption
% is that all the items for a field will be together and if they're
% not, they would be combined outside this predicate.
%
% @param Fields a list of fields (Field-Repeat-Value)
% @param Field the name of the field that is being combined
% @param Values gets the Value items that match Field
% @param RestFields gets any left-over fields
combine_fields_repeat([], _Field, Values, RestFields) => Values = [], RestFields = [].
combine_fields_repeat([Field-repeat-Value|Fields], Field, Values, RestFields) =>
    Values = [Value|Values2],
    combine_fields_repeat(Fields, Field, Values2, RestFields).
combine_fields_repeat(Fields, _Field, Values, RestFields) => Values = [], RestFields = Fields.

:- det(field_and_type/7).
%! field_and_type(+ContextType:atom, +Tag:int, -FieldName:atom, -FqnName:atom, -ContextType2:atom, -RepeatOptional:atom, -Type:atom) is det.
% Lookup a =ContextType= and =Tag= to get the field name, type, etc.
field_and_type(ContextType, Tag, FieldName, FqnName, ContextType2, RepeatOptional, Type) =>
    protobufs:field_name(ContextType, Tag, FieldName, FqnName),
    protobufs:field_type_name(FqnName, ContextType2),
    fqn_repeat_optional(FqnName, RepeatOptional),
    protobufs:field_type(FqnName, Type).

%! fqn_repeat_optional(+FqnName:atom, -RepeatOptional:atom) is det.
% Lookup up protobufs:field_label(FqnName, _), protobufs:field_option_packed(FqnName)
% and set RepeatOptional to one of
% =norepeat=, =repeat=, =repeat_packed=.
fqn_repeat_optional(FqnName, RepeatOptional) =>
    protobufs:field_label(FqnName, LabelRepeatOptional),
    % protobufs:enum_value('.google.protobuf.FieldDescriptorProto.Label', 'LABEL_REPEATED', _).
    (   LabelRepeatOptional = 'LABEL_REPEATED',
        protobufs:field_option_packed(FqnName)
    ->  RepeatOptional = repeat_packed
    ;   \+ protobufs:field_option_packed(FqnName), % validity check
        fqn_repeat_optional_2(LabelRepeatOptional, RepeatOptional)
    ).

:- det(fqn_repeat_optional_2/2).
%! fqn_repeat_optional_2(+DescriptorLabelEnum:atom, -RepeatOrEmpty:atom) is det.
% Map the descriptor "label" to 'repeat' or 'norepeat'.
% From protobufs:enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
fqn_repeat_optional_2('LABEL_OPTIONAL', norepeat).
fqn_repeat_optional_2('LABEL_REQUIRED', norepeat).
fqn_repeat_optional_2('LABEL_REPEATED', repeat).

%! field_descriptor_label_repeated(+Label:atom) is semidet.
% From protobufs:enum_value('.google.protobuf.FieldDescriptorProto.Label', 'LABEL_REPEATED', _).
field_descriptor_label_repeated('LABEL_REPEATED').

%! field_descriptor_label_single(+Label:atom) is semidet.
% From protobufs:enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
field_descriptor_label_single('LABEL_OPTIONAL').
field_descriptor_label_single('LABEL_REQUIRED').

end_of_file.
