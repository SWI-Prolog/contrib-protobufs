/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, Jeffrey Rosenwald

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(protobufs,
	  [
	   protobuf_message/2,   % ?Template ?Codes
	   protobuf_message/3	 % ?Template ?Codes ?Rest
	  ]).

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

For an overview and tutorial  with examples, see protobufs_overview.txt.
Examples of usage may also be found by inspecting test_protobufs.pl.

@see http://code.google.com/apis/protocolbuffers
@author: Jeffrey Rosenwald (JeffRose@acm.org)
@license: LGPL
@compat: SWI-Prolog
*/

:- require([ use_foreign_library/1
	   , atom_codes/2
	   , call/2
	   , float32_codes/2
	   , float64_codes/2
	   , int32_codes/2
	   , int64_codes/2
	   , integer_zigzag/2
	   , string_to_list/2
	   , succ/2
	   , between/3
	   ]).

:- use_foreign_library(foreign(protobufs)).

wire_type(varint, 0).
wire_type(fixed64, 1).
wire_type(length_delimited, 2).
wire_type(start_group, 3).
wire_type(end_group, 4).
wire_type(fixed32, 5).

%
% deal with Google's method of encoding 2's complement integers
% such that packed length is proportional to magnitude. We can handle up
% to 63 bits, plus sign. Essentially moves sign-bit from MSB to LSB.
%
:- if(false).  % now done in the C-support code
zig_zag(Int, X) :-
	integer(Int), !,
	X is (Int << 1) xor (Int >> 63).

zig_zag(Int, X) :-
	integer(X),
	Y is -1 * (X /\ 1),
	Int is (X >> 1) xor Y.
:- endif.
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
	append(Codes, Rest1, Rest), !.
/*
code_string(N, Codes) -->
	{ length(Codes, N)},
	Codes, !.
*/
%
% deal with Google's method of packing unsigned integers in variable
% length, modulo 128 strings.
%
% var_int and tag_type productions were rewritten in straight Prolog for
% speed's sake.
%

var_int(A, [A | Rest], Rest) :-
	A < 128, !.

var_int(X, [A | Rest], Rest1) :-
	nonvar(X),
	X1 is X >> 7,
	A is 128 + (X /\ 0x7f),
	var_int(X1, Rest, Rest1), !.

var_int(X, [A | Rest], Rest1) :-
	var_int(X1, Rest, Rest1),
	X is (X1 << 7) + A - 128, !.
%
%

tag_type(Tag, Type, Rest, Rest1) :-
	nonvar(Tag), nonvar(Type),
	wire_type(Type, X),
	A is Tag << 3 \/ X,
	var_int(A, Rest, Rest1), !.

tag_type(Tag, Type, Rest, Rest1) :-
	var_int(A, Rest, Rest1),
	X is A /\ 0x07,
	wire_type(Type, X),
	Tag is A >> 3.
%
prolog_type(Tag, double) -->     tag_type(Tag, fixed64).
prolog_type(Tag, integer64) -->	 tag_type(Tag, fixed64).
prolog_type(Tag, float) -->      tag_type(Tag, fixed32).
prolog_type(Tag, integer32) -->  tag_type(Tag, fixed32).
prolog_type(Tag, integer) -->    tag_type(Tag, varint).
prolog_type(Tag, unsigned) -->	 tag_type(Tag, varint).
prolog_type(Tag, boolean) -->    tag_type(Tag, varint).
prolog_type(Tag, enum) -->       tag_type(Tag, varint).
prolog_type(Tag, atom) -->       tag_type(Tag, length_delimited).
prolog_type(Tag, codes) -->      tag_type(Tag, length_delimited).
prolog_type(Tag, string) -->     tag_type(Tag, length_delimited).
prolog_type(Tag, embedded) -->   tag_type(Tag, length_delimited).
%
%   The protobuf-2.1.0 grammar allows negative values in enums.
%   But they are encoded as unsigned in the  golden message.
%   Encode as integer and lose. Encode as unsigned and win.
%

payload(enum, Type) -->
	{ must_be(callable, Type),
	  (  call(Type, Value)
	     *-> true
	     ; ( Type =.. [Functor, Arg],
	         existence_error(enum(Functor), Arg)
	       )
	  ) },
	payload(unsigned, Value).

payload(double,  A) -->
	fixed_float64(A).

payload(integer64, A) -->
	fixed_int64(A).

payload(float, A) -->
	fixed_float32(A).

payload(integer32, A) -->
	fixed_int32(A).

payload(integer, A) -->
	{ nonvar(A), integer_zigzag(A,X) }, !,
	var_int(X).

payload(integer, A) -->
	var_int(X),
	{ integer_zigzag(A, X) }.

payload(unsigned, A) -->
	{ nonvar(A) -> A >= 0; true },
	var_int(A).

payload(codes, A) -->
	{ nonvar(A), length(A, Len)},
	var_int(Len),
	code_string(Len, A).

payload(codes, A) -->
	var_int(Len),
	code_string(Len, A).

payload(atom, A) -->
	{ nonvar(A), atom_codes(A, Codes) },
	payload(codes, Codes), !.

payload(atom, A) -->
	payload(codes, Codes),
	{ atom_codes(A, Codes) }.

payload(boolean, true) -->
	payload(unsigned, 1).

payload(boolean, false) -->
	payload(unsigned, 0).

payload(string, A) -->
	{ nonvar(A) -> string_to_list(A, Codes); true },

	payload(codes, Codes),

	{ string_to_list(A, Codes) }.

payload(embedded, protobuf(A)) -->
	{ ground(A), phrase(protobuf(A), Codes) },
	payload(codes, Codes), !.

payload(embedded, protobuf(A)) -->
	payload(codes, Codes),
	{ phrase(protobuf(A), Codes) }.

start_group(Tag) -->		tag_type(Tag, start_group).

end_group(Tag) -->		tag_type(Tag, end_group).
%
%
nothing([]) --> [], !.

protobuf([A | B]) -->
	{ A =.. [ Type, Tag, Payload] },
	message_sequence(Type, Tag, Payload),
	(   protobuf(B); nothing(B)).


repeated_message_sequence(repeated_enum, Tag, Type, [A | B]) -->
	{ Compound =.. [Type, A] },
	message_sequence(enum, Tag, Compound),
	(   repeated_message_sequence(repeated_enum, Tag, Type, B);
	    nothing(B)).

repeated_message_sequence(Type, Tag, [A | B]) -->
	message_sequence(Type, Tag, A),
	repeated_message_sequence(Type, Tag, B).

repeated_message_sequence(_Type, _Tag, A) -->
	nothing(A).
%
%

message_sequence(repeated, Tag, enum(Compound)) -->
	{ Compound =.. [ Type, List] },
	repeated_message_sequence(repeated_enum, Tag, Type, List).

message_sequence(repeated, Tag, Compound) -->
	{ Compound =.. [Type, A] },
	repeated_message_sequence(Type, Tag, A).

message_sequence(group, Tag, A) -->
	start_group(Tag),
	protobuf(A),
	end_group(Tag), !.

message_sequence(PrologType, Tag, Payload) -->
	prolog_type(Tag, PrologType),
        payload(PrologType, Payload).


%%	protobuf_message(?Template, ?Wire_stream) is semidet.
%%	protobuf_message(?Template, ?Wire_stream, ?Rest) is nondet.
%
%  marshalls  and  unmarshalls  byte  streams   encoded  using  Google's
%  Protobuf  grammars.  protobuf_message/2  provides   a  bi-directional
%  parser that marshalls a Prolog structure to Wire_stream, according to
%  rules specified by Template. It can  also unmarshall Wire_stream into
%  a Prolog structure according to  the same grammar. protobuf_message/3
%  provides a difference list version.
%
%  @param Template is  a  protobuf   grammar  specification.  On decode,
%  unbound variables in the Template are   unified with their respective
%  values in the Wire_stream. On encode, Template must be ground.
%
%  @param Wire_stream is a code list that was generated by a protobuf
%  encoder using a equivalent template.
%
protobuf_message(protobuf(Template), Wirestream) :-
	must_be(list, Template),
	phrase(protobuf(Template), Wirestream), !.

protobuf_message(protobuf(Template), Wirestream, Residue) :-
	must_be(list, Template),
	phrase(protobuf(Template), Wirestream, Residue).
