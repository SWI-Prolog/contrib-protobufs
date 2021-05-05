% -*- mode: Prolog -*-

% Test of test1.proto, test1.py
% Assumes it is called from test1.pl

:- module(test1, [test1_main/0]).

:- initialization(test1_main, main).

% :- use_module(library(protobufs)). % DO NOT SUBMIT
:- use_module('../protobufs').

:- set_prolog_flag(optimise_debug, false). % ensure assertion/1 is executed

protobufs:enum(Key, Value) :-
    nth0(Value,
         ['E1',
          'Enum2',
          'AnotherEnum'],
         Key).

template1(Template, Vars) :-
    Template = protobuf([
                         double(1,     V_double),
                         float(2,      V_float),
                         unsigned(3,   V_int32),
                         unsigned(4,   V_int64),
                         unsigned(5,   V_uint32),
                         unsigned(6,   V_uint64),
                         integer(7,    V_sint32),
                         integer(8,    V_sint64),
                         integer32(9,  V_fixed32),
                         integer64(10, V_fixed64),
                         integer32(11, V_sfixed32),
                         integer64(12, V_sfixed64),
                         boolean(13,   V_bool),
                         string(14,    V_string),
                         codes(15,     V_bytes),
                         enum(16,      enum(V_enum))
                        ]),
    Vars = [                           V_double,
                                       V_float,
                                       V_int32,
                                       V_int64,
                                       V_uint32,
                                       V_uint64,
                                       V_sint32,
                                       V_sint64,
                                       V_fixed32,
                                       V_fixed64,
                                       V_sfixed32,
                                       V_sfixed64,
                                       V_bool,
                                       V_string,
                                       V_bytes,
                                       V_enum].


test1_main :-
    template1(Template, Vars),
    Vars = [                           V_double,
                                       V_float,
                                       V_int32,
                                       V_int64,
                                       V_uint32,
                                       V_uint64,
                                       V_sint32,
                                       V_sint64,
                                       V_fixed32,
                                       V_fixed64,
                                       V_sfixed32,
                                       V_sfixed64,
                                       V_bool,
                                       V_string,
                                       V_bytes,
                                       V_enum],
    read_file_to_codes('scalars1a_from_python.wire', Wirestream, [encoding(octet),type(binary)]),
    format(user_error, 'Wirestream=~q~n', [Wirestream]),
    protobuf_message(Template, Wirestream),
    print_term(Template, [output(user_error)]), nl,
    assertion(V_double   == 1.0),
    assertion(V_float    == 2.0),
    assertion(V_int32    == 3),
    assertion(V_int64    == 4),
    assertion(V_uint32   == 5),
    assertion(V_uint64   == 6),
    assertion(V_sint32   == 7),
    assertion(V_sint64   == 8),
    assertion(V_fixed32  == 9),
    assertion(V_fixed64  == 10),
    assertion(V_sfixed32 == 11),
    assertion(V_sfixed64 == 12),
    assertion(V_bool     == false),
    assertion(V_string   == "écran 網目錦蛇"),
    assertion(V_bytes    == [0xc3, 0x28]),
    assertion(V_enum     ==  'E1'),
    protobuf_message(Template, Wirestream2),
    open('scalars1a_from_prolog.wire', write, Stream, [encoding(octet),type(binary)]),
    format(Stream, '~s', [Wirestream2]),
    close(Stream).
