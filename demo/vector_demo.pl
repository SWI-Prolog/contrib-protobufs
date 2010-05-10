:- module(vector_demo,
	  [
	   write_as_proto/1,
	   read_from_proto/1,
	   vector/2,
	   send_command/3,
	   send_precompiled_command/3,
	   protobuf_bag/2,
	   make_tmp99/0
	  ]).

:- use_module(library(protobufs)).

:- meta_predicate ~>(0,0).
:- op(950, xfy, ~>).

~>(P, Q) :-
	setup_call_cleanup(P, (true; fail), assertion(Q)).
%
%    See protobufs_overview.txt for discussion
%

vector_type(double(_List), 2).
vector_type(float(_List), 3).
vector_type(integer(_List), 4).
vector_type(integer64(_List), 5).
vector_type(integer32(_List), 6).
vector_type(unsigned(_List), 7).
vector_type(codes(_List), 8).
vector_type(atom(_List), 9).
vector_type(string(_List), 10).


basic_vector(Type, Template) :-
	vector_type(Type, Tag),

	Template = protobuf([ repeated(Tag, Type) ]).

vector(Type, B):-
	basic_vector(Type, Proto),

	protobuf_message(Proto, B).

write_as_proto(List) :-
	vector(List, Z),

	open('tmp99.tmp', write, S, [type(binary)])
	   ~> close(S),

	format(S, '~s', [Z]), !.


read_from_proto(V) :-
	read_file_to_codes('tmp99.tmp', Codes, [type(binary)]),

	vector(V, Codes).

protobufs:commands(Key, Value) :-
	nth1(Value,
		[ square,
		  decimate,
		  transform,
		  inverse_transform
		],
		Key).

send_command(Command, Vector, Msg) :-

	basic_vector(Vector, Proto1),

	Proto = protobuf([enum(1, commands(Command)), embedded(2, Proto1)]),

	protobuf_message(Proto, Msg).

%
%
:- dynamic precompiled_message/3.

send_precompiled_command(Command, Vector, Msg) :-
	basic_vector(Vector, Proto1),

	precompiled_message(commands(Command), Msg, Msg1),

	protobuf_message(protobuf([embedded(3, Proto1)]), Msg1).

precompile_commands :-
	abolish(precompiled_message/3),
	forall(protobufs:commands(Key, _),
	      (	  Proto = protobuf([atom(1, command),
				    enum(2, commands(Key))]),
		  protobuf_message(Proto, Msg, Tail),
		  assert(precompiled_message(commands(Key), Msg, Tail))
	      )),
	compile_predicates([precompiled_message/3]).

%
%

compound_protobuf(complex(Real, Img), group(12, [double(1, Real), double(2, Img)])).
compound_protobuf(float(Val), float(13, Val)).
compound_protobuf(double(Val), double(14, Val)).
compound_protobuf((Num rdiv Den), group(15, [integer(1, Num), integer(2, Den)])).
compound_protobuf(integer(Val), integer(16, Val)).


protobuf_bag([], []).

protobuf_bag([ Type | More], Msg) :-

	compound_protobuf(Type, Proto),

	protobuf_message(protobuf([Proto]), Msg, Msg1),

	protobuf_bag(More, Msg1), !.

make_tmp99 :-
	X is pi,
	write_as_proto(double([-2.2212, -7.6675, X, 0, 1.77e-9, 2.54e222])),
	halt(0).

:- initialization
      precompile_commands.










