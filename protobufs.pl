/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, Jeffrey Rosenwald

	 Modified by:	Dario Campagna
	 E-mail:			dario.campagna@dmi.unipg.it

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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
	   , integer_zigzag_64/2
	   , integer_zigzag_32/2
	   , string_to_list/2
	   , succ/2
	   , between/3
	   ]).

:- use_foreign_library(foreign('protobufs')).

:- multifile message/2.

wire_type(varint, 0).
wire_type(fixed64, 1).
wire_type(length_delimited, 2).
wire_type(start_group, 3).
wire_type(end_group, 4).
wire_type(fixed32, 5).

% Predicate for computing an unsigned int from an int usign two's complement
unsigned_int(A,UA) :- Aux is abs(A), UA is (Aux xor 18446744073709551615) + 1.

% Predicate for computing the int corresponding to an unsigned int encoding int with two's complement
int_unsigned(A,UA) :-
	(UA >> 63 =:= 1
	 ->	Aux is (UA xor 18446744073709551615) + 1,
			A is -Aux
	 ;		A is UA
	).

%
%  basic wire-type processing handled by C-support code
%
fixed_int32(X) -->
	[A0, A1, A2, A3],
	{ int32_codes(X, [A0, A1, A2, A3]) }.

fixed_int64(X) -->
	[A0, A1, A2, A3, A4, A5, A6, A7],
	{ int64_codes(X, [A0, A1, A2, A3, A4, A5, A6, A7]) }.

fixed_float32(X) -->
	[A0, A1, A2, A3],
	{ float32_codes(X, [A0, A1, A2, A3]) }.
	
fixed_float64(X) -->
	[A0, A1, A2, A3, A4, A5, A6, A7],
	{ float64_codes(X, [A0, A1, A2, A3, A4, A5, A6, A7]) }.

%
%   Start of the DCG
%
code_string(0, A) --> !, nothing(A).

code_string(N, [A | B]) -->
	[A],
	{ N1 is N - 1 },
	code_string(N1, B).
	
%
% deal with Google's method of packing unsigned integers in variable
% length, modulo 128 strings.
%
var_int_encode(X,Len,NLen) -->
	( { X < 128 }
		->	{ NLen is Len + 1 },
			[X]
		;	{ X1 is X // 128,
		     A is 128 + (X - (128 * X1))
			},
			[A],
			var_int_encode(X1,Len,NLen1),
			{ NLen is NLen1 + 1 }
	).

var_int_decode(X,Dec,NDec) -->
	[A],
	{nonvar(Dec) -> NDec1 is Dec - 1 ; NDec1 = Dec},
	( { A < 128 }
		->	{ X = A }
		;	var_int_decode(X1,NDec1,NDec),
			{ X is A - 128 + (128 * X1) }
	).
%
%
tag_type_encode(Tag,Type,Len,NLen) -->
	{ wire_type(Type,X),
	  A is Tag << 3 \/ X
	},
	var_int_encode(A,Len,NLen).
	
tag_type_decode(Tag,Type,Dec,NDec) -->
	var_int_decode(A,Dec,NDec),
	{ X is A /\ 0x07, wire_type(Type, X), Tag is A >> 3 }.
%
%
prolog_type_encode(Tag, double, Len, NLen) --> tag_type_encode(Tag, fixed64, Len, NLen).
prolog_type_encode(Tag, integer64, Len, NLen) -->	 tag_type_encode(Tag, fixed64, Len, NLen).
prolog_type_encode(Tag, float, Len, NLen) -->      tag_type_encode(Tag, fixed32, Len, NLen).
prolog_type_encode(Tag, integer32, Len, NLen) -->  tag_type_encode(Tag, fixed32, Len, NLen).
prolog_type_encode(Tag, integer, Len, NLen) -->    tag_type_encode(Tag, varint, Len, NLen).
prolog_type_encode(Tag, unsigned, Len, NLen) -->	 tag_type_encode(Tag, varint, Len, NLen).
prolog_type_encode(Tag, sinteger64, Len, NLen) --> tag_type_encode(Tag, varint, Len, NLen).						
prolog_type_encode(Tag, sinteger32, Len, NLen) --> tag_type_encode(Tag, varint, Len, NLen).						
prolog_type_encode(Tag, boolean, Len, NLen) -->    tag_type_encode(Tag, varint, Len, NLen).
prolog_type_encode(Tag, enum, Len, NLen) -->       tag_type_encode(Tag, varint, Len, NLen).
prolog_type_encode(Tag, atom, Len, NLen) -->       tag_type_encode(Tag, length_delimited, Len, NLen).
prolog_type_encode(Tag, codes, Len, NLen) -->      tag_type_encode(Tag, length_delimited, Len, NLen).
prolog_type_encode(Tag, string, Len, NLen) -->     tag_type_encode(Tag, length_delimited, Len, NLen).
prolog_type_encode(Tag, embedded, Len, NLen) -->   tag_type_encode(Tag, length_delimited, Len, NLen).
%
prolog_type_decode(Tag, double,Dec,NDec) -->     tag_type_decode(Tag, fixed64,Dec,NDec).
prolog_type_decode(Tag, integer64,Dec,NDec) -->	 tag_type_decode(Tag, fixed64,Dec,NDec).
prolog_type_decode(Tag, float,Dec,NDec) -->      tag_type_decode(Tag, fixed32,Dec,NDec).
prolog_type_decode(Tag, integer32,Dec,NDec) -->  tag_type_decode(Tag, fixed32,Dec,NDec).
prolog_type_decode(Tag, integer,Dec,NDec) -->    tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, unsigned,Dec,NDec) -->	 tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, sinteger64,Dec,NDec) --> tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, sinteger32,Dec,NDec) --> tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, boolean,Dec,NDec) -->    tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, enum,Dec,NDec) -->       tag_type_decode(Tag, varint,Dec,NDec).
prolog_type_decode(Tag, atom,Dec,NDec) -->       tag_type_decode(Tag, length_delimited,Dec,NDec).
prolog_type_decode(Tag, codes,Dec,NDec) -->      tag_type_decode(Tag, length_delimited,Dec,NDec).
prolog_type_decode(Tag, string,Dec,NDec) -->     tag_type_decode(Tag, length_delimited,Dec,NDec).
prolog_type_decode(Tag, embedded,Dec,NDec) -->   tag_type_decode(Tag, length_delimited,Dec,NDec).
%
%
payload_encode(enum(Tag, Type), Len, NLen) -->
	{ call(Type, Value) },
	payload_encode(unsigned(Tag, Value), Len, NLen).

payload_encode(double(_Tag, A), Len, NLen) -->
	fixed_float64(A),
	{ NLen = Len + 8 }.

payload_encode(integer64(_Tag, A), Len, NLen) -->
	fixed_int64(A),
	{ NLen is Len + 8 }.

payload_encode(float(_Tag, A), Len, NLen) -->
	fixed_float32(A),
	{ NLen is Len + 4 }.

payload_encode(integer32(_Tag, A), Len, NLen) -->
	fixed_int32(A),
	{ NLen is Len + 4 }.

payload_encode(integer(_Tag, A), Len, NLen) -->
	{ A >= 0 -> UA = A ; unsigned_int(A,UA) },
	var_int_encode(UA, Len, NLen).
	
payload_encode(unsigned(_Tag, A), Len, NLen) -->
	{ A >= 0 },
	var_int_encode(A, Len, NLen).
	
payload_encode(sinteger64(_Tag, A), Len, NLen) -->
	{ integer_zigzag_64(A,X) },
	var_int_encode(X, Len, NLen).
	
payload_encode(sinteger32(_Tag, A), Len, NLen) -->
	{ integer_zigzag_32(A,X) },
	var_int_encode(X, Len, NLen).

payload_encode(boolean(Tag, true), Len, NLen) -->
	payload_encode(unsigned(Tag, 1), Len, NLen).

payload_encode(boolean(Tag, false), Len, NLen) -->
	payload_encode(unsigned(Tag, 0), Len, NLen).

payload_encode(atom(_Tag, A), Len, NLen) -->
	{ atom_codes(A,Codes), length(Codes,Length) },
	var_int_encode(Length, Len, NLen1),
	Codes,
	{ NLen is NLen1 + Length }.
	
payload_encode(codes(_Tag, A), Len, NLen) -->
	{ length(A,Length) },
	var_int_encode(Length,Len,NLen1),
	A,
	{ NLen is NLen1 + Length }.

payload_encode(string(_Tag,A), Len, NLen) -->
	{ string_to_list(A,Codes), length(Codes,Length) },
	var_int_encode(Length, Len, NLen1),
	Codes,
	{ NLen is NLen1 + Length}.

payload_encode(embedded(_Tag, protobuf(A)), Len, NLen, List, Tail) :-
	phrase(protobuf_encode(A,0,LenA), Codes, CTail),
	var_int_encode(LenA, Len, NLen1, List, Tail1),			
	Tail1 = Codes,
	NLen is NLen1 + LenA,
	CTail = Tail.

payload_encode(embedded(_Tag, message(_Name, A)), Len, NLen, List, Tail) :-
	phrase(protobuf_encode(A,0,LenA), Codes, CTail),
	var_int_encode(LenA, Len, NLen1, List, Tail1),
	Tail1 = Codes,
	NLen is NLen1 + LenA,
	CTail = Tail.
%
payload_decode(enum(Tag, Type),Dec,NDec) -->
	{ call(Type, Value) },
	payload_decode(unsigned(Tag, Value),Dec,NDec).

payload_decode(double(_Tag, A),Dec,NDec) -->
	fixed_float64(A),
	{ nonvar(Dec) -> NDec is Dec - 8 ; NDec = Dec }.
	
payload_decode(integer64(_Tag, A),Dec,NDec) -->
	fixed_int64(A),
	{ nonvar(Dec) -> NDec is Dec - 8 ; NDec = Dec }.
	
payload_decode(float(_Tag, A),Dec,NDec) -->
	fixed_float32(A),
	{ nonvar(Dec) -> NDec is Dec - 4 ; NDec = Dec }.

payload_decode(integer32(_Tag, A),Dec,NDec) -->
	fixed_int32(A),
	{ nonvar(Dec) -> NDec is Dec - 4 ; NDec = Dec }.
	
payload_decode(integer(_Tag, A),Dec,NDec) -->
	var_int_decode(UA,Dec,NDec),
	{int_unsigned(A,UA)}.

payload_decode(unsigned(_Tag, A),Dec,NDec) -->
	var_int_decode(A,Dec,NDec).

payload_decode(sinteger64(_Tag, A),Dec,NDec) -->
	var_int_decode(X,Dec,NDec),
	{ integer_zigzag_64(A,X) }.

payload_decode(sinteger32(_Tag, A),Dec,NDec) -->
	var_int_decode(X,Dec,NDec),
	{ integer_zigzag_32(A,X) }.
	
payload_decode(boolean(Tag, true),Dec,NDec) -->
	payload_decode(unsigned(Tag, 1),Dec,NDec).

payload_decode(boolean(Tag, false),Dec,NDec) -->
	payload_decode(unsigned(Tag, 0),Dec,NDec).

payload_decode(codes(_Tag, A),Dec,NDec) -->	
	var_int_decode(Len,Dec,NDec1),	
	code_string(Len, A),
	{ nonvar(Dec) -> NDec is NDec1 - Len ; NDec = Dec }.

payload_decode(atom(Tag, A),Dec,NDec) -->
	payload_decode(codes(Tag,Codes),Dec,NDec),
	{ atom_codes(A,Codes) }.

payload_decode(string(Tag,A),Dec,NDec) -->
	payload_decode(codes(Tag,Codes),Dec,NDec),
	{ string_to_list(A,Codes) }.

payload_decode(embedded(_Tag, protobuf(A)),Dec,NDec) -->
	var_int_decode(Len,Dec,NDec1),
	protobuf_decode(A,Len,_),
	{ nonvar(Dec) -> NDec is NDec1 - Len ; NDec = Dec }.

payload_decode(embedded(_Tag, message(Name, A)),Dec,NDec) -->
	{ message(Name, A) },
	var_int_decode(Len,Dec,NDec1),
	protobuf_decode(A,Len,_),
	{ nonvar(Dec) -> NDec is NDec1 - Len ; NDec = Dec }.
%
%
start_group_encode(Tag, Len, NLen) --> tag_type_encode(Tag, start_group, Len, NLen).

end_group_encode(Tag, Len, NLen) --> tag_type_encode(Tag, end_group, Len, NLen).
%
start_group_decode(Tag,Dec,NDec) --> tag_type_decode(Tag, start_group, Dec, NDec).

end_group_decode(Tag,Dec,NDec) --> tag_type_decode(Tag, end_group, Dec, NDec).
%
%
nothing([]) --> [], !.
%
%
protobuf_encode([A | B], Len, NLen) -->
	message_sequence_encode(A, Len, LenA),
	( protobuf_encode(B, LenA, NLen)
	 	->	{ true }
		;	nothing(B), { NLen = LenA }
		).

protobuf_decode([A | B],Dec,NDec) -->
	message_sequence_decode(A,Dec,NDec1),
	(protobuf_decode(B,NDec1,NDec)
		-> { true }
		;	nothing(B), { NDec = NDec1 }
   ).
%
%
message_sequence_encode_cons(embedded(Tag,A),repeated(Tag,More),Len,NLen) -->	
	prolog_type_encode(Tag, embedded, Len, NLen1),
   payload_encode(embedded(Tag,A), NLen1, LenEmb),
	message_sequence_encode(repeated(Tag,More),LenEmb,NLen).

message_sequence_encode(repeated(Tag,embedded([A | B], Template)), Len, NLen) -->
	{ More = embedded(B, Template),
	  Proto = embedded(Tag, A)
	},
	message_sequence_encode_cons(Proto,repeated(Tag,More),Len,NLen).

message_sequence_encode(repeated(_Tag, embedded([], _Template)), Len, Len) --> [].

message_sequence_encode(repeated(Tag, double([A | B])), Len, NLen) -->
	{ More = double(B),
	  Proto = double(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, double([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, integer64([A | B])), Len, NLen) -->
	{ More = integer64(B),
	  Proto = integer64(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, integer64([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, float([A | B])), Len, NLen) -->
	{ More = float(B),
	  Proto = float(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, float([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, integer32([A | B])), Len, NLen) -->
	{ More = integer32(B),
	  Proto = integer32(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, integer32([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, integer([A | B])), Len, NLen) -->
	{ More = integer(B),
	  Proto = integer(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, integer([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, unsigned([A | B])), Len, NLen) -->
	{ More = unsigned(B),
	  Proto = unsigned(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, unsigned([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, sinteger64([A | B])), Len, NLen) -->
	{ More = sinteger64(B),
	  Proto = sinteger64(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, sinteger64([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, sinteger32([A | B])), Len, NLen) -->
	{ More = sinteger32(B),
	  Proto = sinteger32(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, sinteger32([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, boolean([A | B])), Len, NLen) -->
	{ More = boolean(B),
	  Proto = boolean(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, boolean([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, atom([A | B])), Len, NLen) -->
	{ More = atom(B),
	  Proto = atom(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, atom([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, codes([A | B])), Len, NLen) -->
	{ More = codes(B),
	  Proto = codes(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, codes([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, string([A | B])), Len, NLen) -->
	{ More = string(B),
	  Proto = string(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, string([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, group([A|B])), Len, NLen) -->
	{ 	More = group(B),
		Proto = group(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, group([])), Len, Len) --> [].

message_sequence_encode(repeated(Tag, enum([A|B])), Len, NLen) -->
	{ 	More = enum(B),
		Proto = enum(Tag, A) },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).
	
message_sequence_encode(repeated(_Tag, enum([])), Len, Len) --> [].
/*	
message_sequence_encode(repeated(Tag, Compound), Len, NLen) -->
	{ Compound =.. [Type, [A | B]],
	  More =.. [Type, B],
	  Proto =.. [Type, Tag, A] },
	message_sequence_encode(Proto, Len, LenProto),
	message_sequence_encode(repeated(Tag, More), LenProto, NLen).

message_sequence_encode(repeated(_Tag, Compound), Len, Len) -->
	{ Compound =.. [ _Type , []] }.
*/

message_sequence_encode(group(Tag, A), Len, NLen) -->
	start_group_encode(Tag, Len, NLen1),
  	protobuf_encode(A, NLen1, NLen2),
	end_group_encode(Tag, NLen2, NLen), !.

message_sequence_encode(optional(Payload,Presence), Len, NLen) -->
	(
		{ Presence = present}, message_sequence_encode(Payload, Len, NLen)
		;
		{ Presence = not_present }, [], {NLen = Len}
	).

message_sequence_encode(double(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, double, Len, NLen1),
   payload_encode(double(Tag,Payload), NLen1, NLen).

message_sequence_encode(integer64(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, integer64, Len, NLen1),
   payload_encode(integer64(Tag,Payload), NLen1, NLen).

message_sequence_encode(float(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, float, Len, NLen1),
   payload_encode(float(Tag,Payload), NLen1, NLen).

message_sequence_encode(integer32(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, integer32, Len, NLen1),
   payload_encode(integer32(Tag,Payload), NLen1, NLen).

message_sequence_encode(integer(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, integer, Len, NLen1),
   payload_encode(integer(Tag,Payload), NLen1, NLen).

message_sequence_encode(unsigned(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, unsigned, Len, NLen1),
   payload_encode(unsigned(Tag,Payload), NLen1, NLen).

message_sequence_encode(sinteger64(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, sinteger64, Len, NLen1),
   payload_encode(sinteger64(Tag,Payload), NLen1, NLen).

message_sequence_encode(sinteger32(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, sinteger32, Len, NLen1),
   payload_encode(sinteger32(Tag,Payload), NLen1, NLen).

message_sequence_encode(boolean(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, boolean, Len, NLen1),
   payload_encode(boolean(Tag,Payload), NLen1, NLen).

message_sequence_encode(enum(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, enum, Len, NLen1),
   payload_encode(enum(Tag,Payload), NLen1, NLen).

message_sequence_encode(atom(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, atom, Len, NLen1),
   payload_encode(atom(Tag,Payload), NLen1, NLen).

message_sequence_encode(string(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, string, Len, NLen1),
   payload_encode(string(Tag,Payload), NLen1, NLen).

message_sequence_encode(codes(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, codes, Len, NLen1),
   payload_encode(codes(Tag,Payload), NLen1, NLen).

message_sequence_encode(embedded(Tag,Payload), Len, NLen) -->
	prolog_type_encode(Tag, embedded, Len, NLen1),
   payload_encode(embedded(Tag,Payload), NLen1, NLen).
/*
message_sequence_encode(Compound, Len, NLen) -->
	{ Compound =.. [PrologType, Tag, _Payload] },
	prolog_type_encode(Tag, PrologType, Len, NLen1),
   payload_encode(Compound, NLen1, NLen).
*/
%
message_sequence_decode_cons(embedded(Tag,A),repeated(Tag,More),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	{ More = embedded(_,Template) },
	prolog_type_decode(Tag, embedded,Dec,NDec1),
	{ copy_term(Template,A) },
   payload_decode(embedded(Tag,A),NDec1,NDec2),
	message_sequence_decode(repeated(Tag,More),NDec2,NDec).

message_sequence_decode(repeated(Tag,embedded([A | B], Template)),Dec,NDec) -->
	{ More = embedded(B, Template),
     Proto = embedded(Tag, A)
	},
	message_sequence_decode_cons(Proto,repeated(Tag,More),Dec,NDec).

message_sequence_decode(repeated(_Tag, embedded([],_Template)),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, integer([A|B])),Dec,NDec) -->
	{ More = integer(B),
	  Proto = integer(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, integer([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, boolean([A|B])),Dec,NDec) -->
	{ More = boolean(B),
	  Proto = boolean(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).

message_sequence_decode(repeated(_Tag, boolean([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, unsigned([A|B])),Dec,NDec) -->
	{ More = unsigned(B),
	  Proto = unsigned(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, unsigned([])),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, atom([A|B])),Dec,NDec) -->
	{ More = atom(B),
	  Proto = atom(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).

message_sequence_decode(repeated(_Tag, atom([])),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, codes([A|B])),Dec,NDec) -->
	{ More = codes(B),
	  Proto = codes(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).

message_sequence_decode(repeated(_Tag, codes([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, string([A|B])),Dec,NDec) -->
	{ More = string(B),
	  Proto = string(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, string([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, double([A|B])),Dec,NDec) -->
	{ More = double(B),
	  Proto = double(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, double([])),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, integer64([A|B])),Dec,NDec) -->
	{ More = integer64(B),
	  Proto = integer64(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, integer64([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, float([A|B])),Dec,NDec) -->
	{ More = float(B),
	  Proto = float(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, float([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, integer32([A|B])),Dec,NDec) -->
	{ More = integer32(B),
	  Proto = integer32(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, integer32([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, sinteger64([A|B])),Dec,NDec) -->
	{ More = sinteger64(B),
	  Proto = sinteger64(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, sinteger32([])),Dec,Dec) --> [].
	
message_sequence_decode(repeated(Tag, sinteger32([A|B])),Dec,NDec) -->
	{ More = sinteger32(B),
	  Proto = sinteger32(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, sinteger64([])),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, group([A|B])),Dec,NDec) -->
	{ More = group(B),
	  Proto = group(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, group([])),Dec,Dec) --> [].

message_sequence_decode(repeated(Tag, enum([A|B])),Dec,NDec) -->
	{ More = enum(B),
	  Proto = enum(Tag, A) },
	message_sequence_decode(Proto,Dec,NDec1),
	message_sequence_decode(repeated(Tag, More),NDec1,NDec).
	
message_sequence_decode(repeated(_Tag, enum([])),Dec,Dec) --> [].
/*	
message_sequence_decode(repeated(Tag, Compound)) -->
	{ Compound =.. [Type, [A | B]],
	  More =.. [Type, B],
	  Proto =.. [Type, Tag, A] },
	message_sequence_decode(Proto),
	message_sequence_decode(repeated(Tag, More)).

message_sequence_decode(repeated(_Tag, Compound)) -->
	{ Compound =.. [ _Type , []] }.
*/
message_sequence_decode(group(Tag, A),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	start_group_decode(Tag,Dec,NDec1),
  	protobuf_decode(A,NDec1,NDec2),
	end_group_decode(Tag,NDec2,NDec), !.

message_sequence_decode(optional(Payload,Presence),Dec,NDec) -->
	( { nonvar(Dec), Dec = 0 }
		->	{ Presence = not_present, NDec = Dec }
		;
			{ Presence = present }, message_sequence_decode(Payload,Dec,NDec)
			;
			{ Presence = not_present }, [], { 	NDec = Dec }
	).

message_sequence_decode(integer(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, integer,Dec,NDec1),
   payload_decode(integer(Tag,Payload),NDec1,NDec).

message_sequence_decode(boolean(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, boolean,Dec,NDec1),
   payload_decode(boolean(Tag,Payload),NDec1,NDec).

message_sequence_decode(string(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, string,Dec,NDec1),
   payload_decode(string(Tag,Payload),NDec1,NDec).

message_sequence_decode(unsigned(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, unsigned,Dec,NDec1),
   payload_decode(unsigned(Tag,Payload),NDec1,NDec).

message_sequence_decode(enum(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, enum,Dec,NDec1),
   payload_decode(enum(Tag,Payload),NDec1,NDec).

message_sequence_decode(atom(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, atom,Dec,NDec1),
   payload_decode(atom(Tag,Payload),NDec1,NDec).

message_sequence_decode(codes(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, codes,Dec,NDec1),
   payload_decode(codes(Tag,Payload),NDec1,NDec).

message_sequence_decode(double(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, double,Dec,NDec1),
   payload_decode(double(Tag,Payload),NDec1,NDec).

message_sequence_decode(integer64(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, integer64,Dec,NDec1),
   payload_decode(integer64(Tag,Payload),NDec1,NDec).

message_sequence_decode(float(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, float,Dec,NDec1),
   payload_decode(float(Tag,Payload),NDec1,NDec).

message_sequence_decode(integer32(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, integer32,Dec,NDec1),
   payload_decode(integer32(Tag,Payload),NDec1,NDec).

message_sequence_decode(sinteger64(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, sinteger64,Dec,NDec1),
   payload_decode(sinteger64(Tag,Payload),NDec1,NDec).

message_sequence_decode(sinteger32(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, sinteger32,Dec,NDec1),
   payload_decode(sinteger32(Tag,Payload),NDec1,NDec).

message_sequence_decode(embedded(Tag,Payload),Dec,NDec) -->
	{ nonvar(Dec) -> Dec > 0 ;  true },
	prolog_type_decode(Tag, embedded,Dec,NDec1),
   payload_decode(embedded(Tag,Payload),NDec1,NDec).
/*
message_sequence_decode(Compound) -->
	{ Compound =.. [PrologType, Tag, Payload] },
	prolog_type_decode(Tag, PrologType),
   payload_decode(Compound).
*/
%
%
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
	( optional_ground(Template)
		->	phrase(protobuf_encode(Template,0,_), Wirestream)
		;	phrase(protobuf_decode(Template,_,_), Wirestream)
	).

protobuf_message(message(Name,Template), Wirestream) :-
   message(Name,Template),
	must_be(list, Template),
	( optional_ground(Template)
		->	phrase(protobuf_encode(Template,0,_), Wirestream)
		;	phrase(protobuf_decode(Template,_,_), Wirestream)
	).
	
protobuf_message(protobuf(Template), Wirestream, Residue) :-
	must_be(list, Template),
	( optional_ground(Template)
		->	phrase(protobuf_encode(Template,0,_), Wirestream, Residue)
		;	phrase(protobuf_decode(Template,_,_), Wirestream, Residue)
	).

protobuf_message(message(Name,Template), Wirestream, Residue) :-
   message(Name,Template),
	must_be(list, Template),
	( optional_ground(Template)
		->	phrase(protobuf_encode(Template,0,_), Wirestream, Residue)
		;	phrase(protobuf_decode(Template,_,_), Wirestream, Residue)
	).
%
%
optional_ground([]).
optional_ground([L|Ls]) :-
	( ground(L) ->
	    optional_ground(Ls)
	;   nonvar(L),% writeln(L), nl,
		 optional_ground_cons(L,Ls)
	).
	
optional_ground_cons(optional(_,not_present),Ls) :-
	optional_ground(Ls).
optional_ground_cons(optional(T,present),Ls) :-
	optional_ground_cons(T,Ls).
optional_ground_cons(embedded(_,E),Ls) :-
	optional_ground_cons(E,Ls).
optional_ground_cons(repeated(_,embedded(L,_)),Ls) :-
	nonvar(L),
	optional_ground(L),
	optional_ground(Ls).
optional_ground_cons(message(_,L),Ls) :-
	nonvar(L),
	optional_ground(L),
	optional_ground(Ls).
optional_ground_cons(protobuf(L),Ls) :-
	nonvar(L),
	optional_ground(L),
	optional_ground(Ls).
