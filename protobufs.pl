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
            protobuf_serialize_to_codes/3, % +Term, +MessageType, -WireCodes
            int32_codes/2,
            float32_codes/2,
            int64_codes/2,
            float64_codes/2,
            integer_zigzag/2,
            protobuf_var_int//1,
            protobuf_tag_type//2
          ]).
:- autoload(library(error), [must_be/2, domain_error/2]).
:- autoload(library(lists), [append/3]).
:- autoload(library(utf8), [utf8_codes/3]).
:- autoload(library(dif), [dif/2]).
:- autoload(library(dcg/high_order), [sequence//2]).
:- autoload(library(apply), [maplist/3]).

/** <module> Google's Protocol Buffers ("protobufs")

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

See https://developers.google.com/protocol-buffers

There are two ways you can use protobufs in Prolog: with a compiled
".proto" file and protobuf_parse_from_codes/3 or with a lower-level
interface protobuf_message/2, which allows you to define your own
domain-specific language for parsing and serliazing protobufs.
(Currently there is no protobuf_serialize_to_codes/3.)

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

%! protobuf_parse_from_codes(+WireCodes:list, +MessageType:atom, -Term) is semidet.
%
% Fails if the message can't be parsed.
%
% @tbd: document the generated terms (see library(http/json) and json_read_dict/3)
% @tbd: add options such as =true= and =value_string_as= (similar to json_read_dict/3)
% @tbd: add option for form of the dict tags (fully qualified or not)
% @tbd: add proto2/proto3 options for processing default values.
% @tbd: add empty lists for missing repeated fields.
%
% @bug Does nothing for "default" values.
% @bug Ignores extensions.
% @bug Doesn't do anything special for =oneof= or =map=.
%
% @param WireCodes Wire format of the message from e.g., read_stream_to_codes/2.
%          (The stream should have options `encoding(octet)` and `type(binary)`,
%          either as options to read_file_to_codes/3 or by calling set_stream/2
%          on the stream to read_stream_to_codes/2.)
% @param MessageType Fully qualified message name (from the .proto file's =package= and =message=).
%        For example, if the =package= is =google.protobuf= and the
%        message is =FileDescriptorSet=, then you would use
%        =|'.google.protobuf.FileDescriptorSet'|=. You can see the message
%        names by looking at =|protobufs:proto_meta_field_name('.google.protobuf.FileDescriptorSet', FieldNumber, FieldName, FqnName)|=.
%        The initial '.' on the message type name is optional.
% @param Term The generated term, as nested dicts.
% @see  (see [library(protobufs): Google's Protocol Buffers](#protobufs-serialize-to-codes)
protobuf_parse_from_codes(WireCodes, MessageType0, Term) :-
    protobufs:proto_meta_normalize(MessageType0, MessageType),
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

%! protobuf_serialize_to_codes(+Term:dict, -MessageType:atom, -WireCodes) is det.
protobuf_serialize_to_codes(Term, MessageType0, WireCodes) :-
    protobufs:proto_meta_normalize(MessageType0, MessageType),
    term_to_segments(Term, MessageType, Segments),
    print_term('SEGMENTS':Segments, []), nl,
    protobuf_segment_message(Segments, WireCodes).

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
    (   nonvar(X), X > 0x7fffffff % work around https://github.com/SWI-Prolog/contrib-protobufs/issues/5
    ->  X2 is -(X xor 0xffffffff + 1)
    ;   X2 = X
    ),
    int32_codes(X2, [A0, A1, A2, A3]).

fixed_int64(X, [A0, A1, A2, A3, A4, A5, A6, A7 | Rest], Rest) :-
    (   nonvar(X), X > 0x7fffffffffffffff % work around https://github.com/SWI-Prolog/contrib-protobufs/issues/5
    ->  X2 is -(X xor 0xffffffffffffffff + 1)
    ;   X2 = X
    ),
    int64_codes(X2, [A0, A1, A2, A3, A4, A5, A6, A7]).

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

%! protobuf_tag_type(?Tag:int, ?WireType:atom)// is det.
% Conversion between Tag (number) + WireType and wirestream codes.
% This is a low-level predicate; normally, you should use
% template_message/2 and the appropriate template term.
% @arg Tag The item's tag (field number)
% @arg WireType The item's wire type (see prolog_type//2 for how to
%               convert this to a Prolog type)
protobuf_tag_type(Tag, WireType, Rest, Rest1) :-
    nonvar(Tag), nonvar(WireType),
    wire_type(WireType, WireTypeEncoding),
    A is Tag << 3 \/ WireTypeEncoding,
    protobuf_var_int(A, Rest, Rest1),
    !.
protobuf_tag_type(Tag, WireType, Rest, Rest1) :-
    protobuf_var_int(A, Rest, Rest1),
    WireTypeEncoding is A /\ 0x07,
    wire_type(WireType, WireTypeEncoding),
    Tag is A >> 3.

%! prolog_type(?Tag:int, ?PrologType:atom)// is semidet.
% Match Tag (field number) + PrologType.
% When Type is a variable, backtracks through all the possibilities
% for a given wire encoding.
% Note that 'repeated' isn't here because it's handled by single_message//3.
% See also segment_type_tag/3.
prolog_type(Tag, double) -->     protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, integer64) -->  protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, unsigned64) --> protobuf_tag_type(Tag, fixed64).
prolog_type(Tag, float) -->      protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, integer32) -->  protobuf_tag_type(Tag, fixed32).
prolog_type(Tag, unsigned32) --> protobuf_tag_type(Tag, fixed32).
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

%! payload(?PrologType, ?Payload) is det.
% Process the codes into =Payload=, according to =PrologType=
% TODO: payload//2 "mode" is sometimes module-sensitive, sometimes not.
%       payload(enum, A)// has A as a callable
%       all other uses of payload//2, the 2nd arg is not callable.
%     - This confuses check/0; it also makes defining an enumeration
%       more difficult because it has to be defined in module protobufs
%       (see vector_demo.pl, which defines protobufs:commands/2)
payload(enum, Payload) -->
    enumeration(Payload).
payload(double, Payload) -->
    fixed_float64(Payload).
payload(integer64, Payload) -->
    fixed_int64(Payload).
payload(unsigned64, Payload) -->
    { var(Payload) },
    !,
    fixed_int64(X),
    {   X < 0
    ->  Payload is -(X xor 0xffffffffffffffff + 1)
    ;   Payload = X
    }.
payload(unsigned64, Payload) -->
    {   Payload < 0
    ->  X is -(Payload xor 0xffffffffffffffff + 1)
    ;   X = Payload
    },
    fixed_int64(X).
payload(float, Payload) -->
    fixed_float32(Payload).
payload(integer32, Payload) -->
    fixed_int32(Payload).
payload(unsigned32, Payload) -->
    { var(Payload) },
    !,
    fixed_int32(X),
    {   X < 0
    ->  Payload is -(X xor 0xffffffff + 1)
    ;   Payload = X
    }.
payload(unsigned32, Payload) -->
    {   Payload < 0
    ->  X is -(Payload xor 0xffffffff + 1)
    ;   X = Payload
    },
    fixed_int32(X).
payload(integer, Payload) -->
    { nonvar(Payload), integer_zigzag(Payload, X) },
    !,
    protobuf_var_int(X).
payload(integer, Payload) -->
    protobuf_var_int(X),
    { integer_zigzag(Payload, X) }.
payload(unsigned, Payload) -->
    % TODO: replace payload(unsigned, Payload) with this (needs unit tests +
    %       comparison with Python,C++):
    %   payload(unsigned, Payload) -->
    %       protobuf_var_int(Payload),
    %       { Payload >= 0 }.
    {   nonvar(Payload)
    ->  Payload >= 0
    ;   true
    },
    protobuf_var_int(Payload).
payload(signed32, Payload) --> % signed32 is not defined by prolog_type//2
                               % for wire-stream compatibility reasons.
%     % signed32 ought to write 5 bytes for negative numbers, but both
%     % the C++ and Python implementations write 10 bytes. For
%     % wire-stream compatibility, we follow C++ and Python, even though
%     % protoc decode appears to work just fine with 5 bytes --
%     % presumably there are some issues with decoding 5 bytes and
%     % getting the sign extension correct with some 32/64-bit integer
%     % models.  See CodedOutputStream::WriteVarint32SignExtended(int32
%     % value) in google/protobuf/io/coded_stream.h.  (To write 5 bytes,
%     % change the "Payload xor % 0xffffffffffffffff" to "Payload xor 0xffffffff".)
      payload(signed64, Payload).
payload(signed64, Payload) -->
    % protobuf_var_int//1 cannot handle negative numbers (note that
    % zig-zag encoding always results in a positive number), so
    % compute the 64-bit 2s complement, which is what is produced
    % form C++ and Python.
    % TODO: \Payload instead of Payload xor ...?
    { var(Payload) },
    !,
    protobuf_var_int(X),
    {   X > 0x7fffffffffffffff
    ->  Payload is -(X xor 0xffffffffffffffff + 1)
    ;   Payload = X
    }.
payload(signed64, Payload) -->
    % See comment in previous clause about negative numbers.
    {   Payload < 0
    ->  X is -(Payload xor 0xffffffffffffffff + 1)
    ;   X = Payload
    },
    protobuf_var_int(X).
payload(codes, Payload) -->
    { nonvar(Payload), !, length(Payload, Len)},
    protobuf_var_int(Len),
    code_string(Len, Payload).
payload(codes, Payload) -->
    protobuf_var_int(Len),
    code_string(Len, Payload).
payload(utf8_codes, Payload) -->
    { nonvar(Payload),
      !,
      phrase(utf8_codes(Payload), B)
    },
    payload(codes, B).
payload(utf8_codes, Payload) -->
    payload(codes, B),
    { phrase(utf8_codes(Payload), B) }.
payload(atom, Payload) -->
    { nonvar(Payload),
      atom_codes(Payload, Codes)
    },
    payload(utf8_codes, Codes),
    !.
payload(atom, Payload) -->
    payload(utf8_codes, Codes),
    { atom_codes(Payload, Codes) }.
payload(boolean, true) -->
    payload(unsigned, 1).
payload(boolean, false) -->
    payload(unsigned, 0).
payload(string, Payload) -->
    {   nonvar(Payload)
    ->  string_codes(Payload, Codes)
    ;   true
    },
    % string_codes produces a list of unicode, not bytes
    payload(utf8_codes, Codes),
    { string_codes(Payload, Codes) }.
payload(embedded, protobuf(PayloadSeq)) -->
    { ground(PayloadSeq),
      phrase(protobuf(PayloadSeq), Codes)
    },
    payload(codes, Codes),
    !.
payload(embedded, protobuf(PayloadSeq)) -->
    payload(codes, Codes),
    { phrase(protobuf(PayloadSeq), Codes) }.
payload(packed, TypedPayloadSeq) -->
    { TypedPayloadSeq =.. [PrologType, PayloadSeq],  % TypedPayloadSeq = PrologType(PayloadSeq)
      ground(PayloadSeq),
      phrase(packed_payload(PrologType, PayloadSeq), Codes)
    },
    payload(codes, Codes),
    !.
payload(packed, enum(EnumSpec)) -->
    !,
    % TODO: combine with next clause
    { EnumSpec =.. [ Enum, Values ] }, % EnumSpec = Enum(Values)
    payload(codes, Codes),
    { phrase(packed_enum(Enum, Values), Codes) }.
payload(packed, TypedPayloadSeq) -->
    payload(codes, Codes),
    { TypedPayloadSeq =.. [PrologType, PayloadSeq] },  % TypedPayloadSeq = PrologType(PayloadSeq)
    { phrase(packed_payload(PrologType, PayloadSeq), Codes) }.

packed_payload(PrologType, PayloadSeq) -->
    sequence(payload(PrologType), PayloadSeq).

packed_enum(Enum, [ A | As ]) -->
    { E =.. [Enum, A] },
    payload(enum, E),
    packed_enum(Enum, As).
packed_enum(_EnumSpec, []) --> [ ].

start_group(Tag) --> protobuf_tag_type(Tag, start_group).

end_group(Tag) -->   protobuf_tag_type(Tag, end_group).
%
%
nothing([]) --> [], !.

protobuf([Field | Fields]) -->
    % TODO: don't use =.. -- move logic to single_message
    (   { Field = repeated_embedded(Tag, protobuf(EmbeddedFields), Items) }
    ->  repeated_embedded_messages(Tag, EmbeddedFields, Items)
    ;   { Field =.. [ PrologType, Tag, Payload] },  % Field = PrologType(Tag, Payload)
        single_message(PrologType, Tag, Payload),
        (   protobuf(Fields)
        ;   nothing(Fields)
        )
    ),
    !.

repeated_message(repeated_enum, Tag, Type, [A | B]) -->
    { TypedPayload =.. [Type, A] },  % TypedPayload = Type(A)
    single_message(enum, Tag, TypedPayload),
    (   repeated_message(repeated_enum, Tag, Type, B)
    ;   nothing(B)
    ).
repeated_message(Type, Tag, [A | B]) -->
    { Type \= repeated_enum },
    single_message(Type, Tag, A),
    repeated_message(Type, Tag, B).
repeated_message(_Type, _Tag, A) -->
    nothing(A).

repeated_embedded_messages(Tag, EmbeddedFields, [protobuf(A) | B]) -->
    { copy_term(EmbeddedFields, A) },
    % TODO: the call to single_message/3 causes analysis of
    %       missing predicates to fail
    single_message(embedded, Tag, protobuf(A)), !,
    repeated_embedded_messages(Tag, EmbeddedFields, B).
repeated_embedded_messages(_Tag, _EmbeddedFields, []) -->
    [ ].

%! single_message(+PrologType, ?Tag, ?Payload)// is det.
% Processes a single messages (e.g., one item in the list in protobuf([...]).
% The PrologType, Tag, Payload are from Field =.. [PrologType, Tag, Payload]
% in the caller
single_message(repeated, Tag, enum(EnumSpec)) -->
    { EnumSpec =.. [EnumType, Values] },  % EnumSpec = EnumType(Values)
    repeated_message(repeated_enum, Tag, EnumType, Values).
single_message(repeated, Tag, Payload) -->
    { Payload =.. [PrologType, A] },  % Payload = PrologType(A)
    { PrologType \= enum },
    repeated_message(PrologType, Tag, A).
single_message(group, Tag, A) -->
    start_group(Tag),
    protobuf(A),
    end_group(Tag).
single_message(PrologType, Tag, Payload) -->
    { PrologType \= repeated, PrologType \= group },
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
%    * group(Tag,Segments)
%    * length_delimited(Tag,Codes)
%  Of these, =group= is deprecated in the protobuf documentation and
%  shouldn't appear in modern code, because they have been replaced by
%  nested message types.
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
segment_type_tag(start_group(Tag),             start_group,      Tag). % TODO: delete?
segment_type_tag(end_group(Tag),               end_group,        Tag). % TODO: delete?
segment_type_tag(group(Tag,_Segments),         start_group,      Tag).
segment_type_tag(fixed32(Tag,_Codes),          fixed32,          Tag).
segment_type_tag(length_delimited(Tag,_Codes), length_delimited, Tag).
segment_type_tag(message(Tag,_Segments),       length_delimited, Tag).
segment_type_tag(packed(Tag,_Payload),         length_delimited, Tag).
segment_type_tag(string(Tag,_String),          length_delimited, Tag).

segment(varint, Tag, varint(Tag,Value)) -->
    protobuf_var_int(Value).
segment(fixed64, Tag, fixed64(Tag, [A0,A1,A2,A3,A4,A5,A6,A7])) -->
    [A0, A1, A2, A3, A4, A5, A6, A7].
segment(start_group, Tag, group(Tag, Segments)) -->
    segment_message(Segments),
    protobuf_tag_type(Tag, end_group).
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
    ->  protobuf_segment_message(Segments, Codes)
        % TODO: A more precise check would be that
        % start_group(Tag)/end_group(Tag) appear properly nested, as
        % in single_message(group, Tag, A).
    ;   protobuf_segment_message(Segments, Codes)
    ).
length_delimited_segment(string(Tag,String), Tag, Codes) :-
    (   nonvar(String)
    ->  string_codes(String, StringCodes),
        phrase(utf8_codes(StringCodes), Codes)
    ;   phrase(utf8_codes(StringCodes), Codes),
        string_codes(String, StringCodes)
    ).
length_delimited_segment(packed(Tag,Payload), Tag, Codes) :-
    % We don't know the type of the fields, so we try the 3
    % possibilities.  This has a problem: an even number of fixed32
    % items can't be distinguished from half the number of fixed64
    % items; but it's all we can do. The good news is that usually
    % varint (possibly with zig-zag encoding) is more common because
    % it's more compact (I don't know whether 32-bit or 64-bit is more
    % common for floating point).
    packed_option(Type, Items, Payload),
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
%
% @param Form1 =|message(Tag,Pieces)|=, =|string(Tag,String)|=, =|length_delimited(Tag,Codes)|=,
%        =|varint(Tag,Value)|=, =|fixed64(Tag,Codes)|=, =|fixed32(Tag,Codes)|=.
% @param Form2 similar to =Form1=.
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
% @bug Throws instantiation error for large values.
%      https://github.com/SWI-Prolog/contrib-protobufs/issues/5
%      e.g., int32_codes(4294967294, Z).
%                        0xfffffffe
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
% @bug Throws instantiation error for large values.
%      https://github.com/SWI-Prolog/contrib-protobufs/issues/5
%      e.g. integer_zigzag(A, 18446744073709551600).
%                             0xfffffffffffffff0
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

:- multifile
     protobufs:proto_meta_normalize/2,           %   protobufs:proto_meta_normalize(Unnormalized, Normalized),
     protobufs:proto_meta_package/3,             %   protobufs:proto_meta_package(Package, FileName, Options)
     protobufs:proto_meta_message_type/3,        %   protobufs:proto_meta_message_type(       Fqn,     Package, Name)
     protobufs:proto_meta_field_name/4,          %   protobufs:proto_meta_field_name(         Fqn,     FieldNumber, FieldName, FqnName),
     protobufs:proto_meta_field_json_name/2,     %   protobufs:proto_meta_field_json_name(    FqnName, JsonName)
     protobufs:proto_meta_field_label/2,         %   protobufs:proto_meta_field_label(        FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     protobufs:proto_meta_field_type/2,          %   protobufs:proto_meta_field_type(         FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     protobufs:proto_meta_field_type_name/2,     %   protobufs:proto_meta_field_type_name(    FqnName, TypeName)
     protobufs:proto_meta_field_default_value/2, %   protobufs:proto_meta_field_default_value(FqnName, DefaultValue)
     protobufs:proto_meta_field_option_packed/1, %   protobufs:proto_meta_field_option_packed(FqnName)
     protobufs:proto_meta_enum_type/3,           %   protobufs:proto_meta_enum_type(          FqnName, Fqn, Name)
     protobufs:proto_meta_enum_value/3.          %   protobufs:proto_meta_enum_value(         FqnName, Name, Number)

% :- det(segment_to_term/3).  % TODO - test scalars1a_parse left choicepoint
%! segment_to_term(+ContextType:atom, +Segment, -FieldAndValue) is det.
% ContextType is the type (name) of the containing message
% Segment is a segment from protobuf_segment_message/2
% TODO: if performance is an issue, this code can be combined with
%       protobuf_segment_message/2 (and thereby avoid the use of protobuf_segment_convert/2)
segment_to_term(ContextType0, Segment, FieldAndValue) =>
    segment_type_tag(Segment, _, Tag),
    field_and_type(ContextType0, Tag, FieldName, _FqnName, ContextType, RepeatOptional, Type),
    (   RepeatOptional = repeat_packed
    ->  convert_segment_packed(Type, ContextType, Segment, Value)
    ;   convert_segment(Type, ContextType, Segment, Value)
    ),
    !, % TODO: get rid of this?
    FieldAndValue = field_and_value(FieldName,RepeatOptional,Value).

% TODO: these are very similar to convert_segment - can they be combined?
convert_segment_packed('TYPE_DOUBLE', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed64(Values0))),
    maplist(convert_fixed64_double, Values0, Values), !.
convert_segment_packed('TYPE_FLOAT', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed32(Values0))),
    maplist(convert_fixed32_float, Values0, Values), !.
convert_segment_packed('TYPE_INT64', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))),
    maplist(convert_varint_int64, Values0, Values), !.
convert_segment_packed('TYPE_UINT64', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values))), !.
convert_segment_packed('TYPE_INT32', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))),
    maplist(convert_varint_int32, Values0, Values), !.
convert_segment_packed('TYPE_FIXED64', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed64(Values0))),
    maplist(convert_fixed64_fixed64, Values0, Values), !.
convert_segment_packed('TYPE_FIXED32', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed32(Values0))),
    maplist(convert_fixed32_fixed32, Values0, Values), !.
convert_segment_packed('TYPE_BOOL', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))),
    maplist(int_bool, Values0, Values), !.
convert_segment_packed('TYPE_UINT32', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values))), !.
convert_segment_packed('TYPE_ENUM', ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))), !,
    maplist(protobufs:proto_meta_enum_value(ContextType), Values, Values0). % meta data
convert_segment_packed('TYPE_SFIXED32', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed32(Values))), !.
convert_segment_packed('TYPE_SFIXED64', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, fixed64(Values))), !.
convert_segment_packed('TYPE_SINT32', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))),
    maplist(convert_varint_sint32, Values0, Values), !.
convert_segment_packed('TYPE_SINT64', _ContextType, Segment0, Values) =>
    protobuf_segment_convert(Segment0, packed(_, varint(Values0))),
    maplist(convert_varint_sint64, Values0, Values), !.

%  :- det(convert_segment/4).  % TODO: test scalars1a_parse: protobufs:proto_meta_enum_value/3 left choicepoint
%! convert_segment(+Type:atom, +ContextType:atom, +Segment, -Value) is det.
% Compute an appropriate =Value= from the combination of descriptor
% "type" (in =Type=) and a =Segment=.
convert_segment('TYPE_DOUBLE', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment),
    float64_codes(Value, Codes), !.
convert_segment('TYPE_FLOAT', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment),
    float32_codes(Value, Codes), !.
convert_segment('TYPE_INT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    (   Value0 > 0x7fffffffffffffff  % TODO: test case
    ->  Value is -(Value0 xor 0xffffffffffffffff + 1)
    ;   Value = Value0
    ).
convert_segment('TYPE_UINT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_INT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    (   Value0 > 0x7fffffff  % TODO: test case
    ->  Value is -(Value0 xor 0xffffffffffffffff + 1)
    ;   Value = Value0
    ).
convert_segment('TYPE_FIXED64', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int64_codes(Value0, Codes),
    (   Value0 < 0
    ->  Value is -(Value0 xor 0xffffffffffffffff + 1)
    ;   Value = Value0
    ).
convert_segment('TYPE_FIXED32', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment),
    int32_codes(Value0, Codes), !,
    (   Value0 < 0
    ->  Value is -(Value0 xor 0xffffffff + 1)
    ;   Value = Value0
    ).
convert_segment('TYPE_BOOL', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value0),
    protobuf_segment_convert(Segment0, Segment),
    int_bool(Value0, Value), !.
convert_segment('TYPE_STRING', _ContextType, Segment0, Value) =>
    Segment = string(_,ValueStr),
    protobuf_segment_convert(Segment0, Segment), !,
    (   false    % TODO: control whether atom or string with an option
    ->  atom_string(Value, ValueStr)
    ;   Value = ValueStr
    ).
convert_segment('TYPE_GROUP', ContextType, Segment0, Value) =>
    Segment = group(_,GroupSegments),
    protobuf_segment_convert(Segment0, Segment),
    maplist(segment_to_term(ContextType), GroupSegments, GroupFields),
    combine_fields(GroupFields, ContextType{}, Value), !.
convert_segment('TYPE_MESSAGE', ContextType, Segment0, Value) =>
    Segment = message(_,MsgSegments),
    protobuf_segment_convert(Segment0, Segment),
    maplist(segment_to_term(ContextType), MsgSegments, MsgFields),
    combine_fields(MsgFields, ContextType{}, Value), !.
convert_segment('TYPE_BYTES', _ContextType, Segment0, Value) =>
    Segment = length_delimited(_,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_ENUM', ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment),
    protobufs:proto_meta_enum_value(ContextType, Value, Value0), % meta data
    !.
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

convert_fixed64_double(V0, V) :-
    int64_codes(V0, Codes),
    float64_codes(V, Codes).

convert_fixed32_float(V0, V) :-
    int32_codes(V0, Codes),
    float32_codes(V, Codes).

convert_varint_int64(V0, V) :-
    (   V0 > 0x7fffffffffffffff
    ->  V is -(V0 xor 0xffffffffffffffff + 1)
    ;   V = V0
    ).

convert_varint_int32(V0, V) :-
    (   V0 > 0x7fffffff
    ->  V is -(V0 xor 0xffffffffffffffff + 1)
    ;   V = V0
    ).

convert_fixed64_fixed64(V0, V) :-
        (   V0 < 0
    ->  V is -(V0 xor 0xffffffffffffffff + 1)
    ;   V = V0
    ).

convert_fixed32_fixed32(V0, V) :-
        (   V0 < 0
    ->  V is -(V0 xor 0xffffffff + 1)
    ;   V = V0
    ).

convert_varint_sint64(V0, V) :-
    integer_zigzag(V, V0).

convert_varint_sint32(V0, V) :-
    integer_zigzag(V, V0).

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
combine_fields([field_and_value(Field,repeat_packed,Values0)|Fields], MsgDict0, MsgDict) =>
    (   get_dict(Field, MsgDict0, ExistingValues)
    ->  append(ExistingValues, Values0, Values)
    ;   Values = Values0
    ),
    put_dict(Field, MsgDict0, Values, MsgDict1),
    combine_fields(Fields, MsgDict1, MsgDict).
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

% :- det(field_and_type/7). % TODO
%! field_and_type(+ContextType:atom, +Tag:int, -FieldName:atom, -FqnName:atom, -ContextType2:atom, -RepeatOptional:atom, -Type:atom) is det.
% Lookup a =ContextType= and =Tag= to get the field name, type, etc.
field_and_type(ContextType, Tag, FieldName, FqnName, ContextType2, RepeatOptional, Type) =>
    protobufs:proto_meta_field_name(ContextType, Tag, FieldName, FqnName),
    protobufs:proto_meta_field_type_name(FqnName, ContextType2),
    fqn_repeat_optional(FqnName, RepeatOptional),
    protobufs:proto_meta_field_type(FqnName, Type).

%! fqn_repeat_optional(+FqnName:atom, -RepeatOptional:atom) is det.
% Lookup up protobufs:proto_meta_field_label(FqnName, _), protobufs:proto_meta_field_option_packed(FqnName)
% and set RepeatOptional to one of
% =norepeat=, =repeat=, =repeat_packed=.
fqn_repeat_optional(FqnName, RepeatOptional) =>
    protobufs:proto_meta_field_label(FqnName, LabelRepeatOptional),
    % protobufs:proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', 'LABEL_REPEATED', _).
    (   LabelRepeatOptional = 'LABEL_REPEATED',
        protobufs:proto_meta_field_option_packed(FqnName)
    ->  RepeatOptional = repeat_packed
    ;   \+ protobufs:proto_meta_field_option_packed(FqnName), % validity check
        fqn_repeat_optional_2(LabelRepeatOptional, RepeatOptional)
    ).

:- det(fqn_repeat_optional_2/2).
%! fqn_repeat_optional_2(+DescriptorLabelEnum:atom, -RepeatOrEmpty:atom) is det.
% Map the descriptor "label" to 'repeat' or 'norepeat'.
% From protobufs:proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
fqn_repeat_optional_2('LABEL_OPTIONAL', norepeat).
fqn_repeat_optional_2('LABEL_REQUIRED', norepeat).
fqn_repeat_optional_2('LABEL_REPEATED', repeat).

%! field_descriptor_label_repeated(+Label:atom) is semidet.
% From protobufs:proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', 'LABEL_REPEATED', _).
field_descriptor_label_repeated('LABEL_REPEATED').

%! field_descriptor_label_single(+Label:atom) is semidet.
% From protobufs:proto_meta_enum_value('.google.protobuf.FieldDescriptorProto.Label', Label, _).
field_descriptor_label_single('LABEL_OPTIONAL').
field_descriptor_label_single('LABEL_REQUIRED').

% :- det(term_to_segments/3).  % TODO
%! term_to_segments(+Term:dict, +MessageType:atom, -Segments:list) is det.
% Recursively traverse a =Term=, generating message segments
term_to_segments(Term, MessageType, Segments) :-
    is_dict(Term),
    dict_pairs(Term, _, FieldValues),
    maplist(term_field(MessageType), FieldValues, Segments).

term_field(MessageType, FieldName-Value, Segment) :-
    protobufs:proto_meta_field_name(MessageType, Tag, FieldName, FieldFqn),
    protobufs:proto_meta_field_type(FieldFqn, FieldType),
    protobufs:proto_meta_field_label(FieldFqn, Label),
    (   protobufs:proto_meta_field_option_packed(FieldFqn)
    ->  Packed = packed
    ;   Packed = not_packed
    ),
    term_field2(FieldType, Label, Packed, Tag, FieldFqn, Value, Segment).

term_field2(FieldType, 'LABEL_OPTIONAL', not_packed, _Tag, FieldFqn, Value, Segment) :- !,
    convert_segment(FieldType, FieldFqn, Segment, Value).

term_field2(FieldType, 'LABEL_REQUIRED', not_packed, _Tag, FieldFqn, Value, Segment) :- !,  % same as LABEL_OPTIONAL
    convert_segment(FieldType, FieldFqn, Segment, Value).

term_field2(FieldType, 'LABEL_REPEATED', packed, Tag, FieldFqn, Values, packed(Tag,FieldValues)) :- !,
    convert_segment_packed(FieldType, FieldFqn, Values, FieldValues).

term_field2(FieldType, 'LABEL_REPEATED', not_packed, _Tag, FieldFqn, Values, _Segment) :- !,
    maplist(convert_segment(FieldType), FieldFqn, Values, repeated(Values)).

term_field2(FieldType, Label, Packed, Tag, FieldFqn, Value, Segment) :-
    % TODO: this is a bit funky:
    domain_error(type(field_type=FieldType, label=Label, packed=Packed), value(tag=Tag, field_fqn=FieldFqn, value=Value, segment=Segment)).


end_of_file.
