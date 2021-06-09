% -*- mode: Prolog -*-

:- module(test_read, [test_read_main/0]).

:- initialization(test_read_main, main).

:- use_module(library(protobufs)).
:- use_module(test_common).

% Not needed because we're using plunit:
% :- set_prolog_flag(optimise_debug, false). % ensure assertion/1 is executed

t0 :-
    Template = protobuf([repeated(1, enum(my_enum(V_enum)))]),
    read_file_to_codes('m0.wire', WireStream, [encoding(octet),type(binary)]),
    format('m0.wire: ~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    print_term(Template, []),nl,
    print_term(enum=V_enum, []),nl.

t1 :-
    % T1 = protobuf([embedded(12, protobuf([string(15,Key),string(128,Value)]))]),
    Template = protobuf([repeated(12,
                                  embedded(Key-Value,
                                           protobuf([string(15,Key),string(128,Value)]),
                                           KeyValueList)
                                 )]),
    read_file_to_codes('m1.wire', WireStream, [encoding(octet),type(binary)]),
    format('m1.wire: ~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    print_term(Template, []),nl,
    print_term(list=KeyValueList, []),nl.

t2 :-
    Template = protobuf([embedded(5, protobuf([string(15,Key),string(128,Value)]))]),
    read_file_to_codes('m2.wire', WireStream, [encoding(octet),type(binary)]),
    format('m2.wire: ~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    print_term(Template, []),nl,
    print_term([key=Key,value=Value], []),nl.

test_read_main :-
    run_tests.

:- begin_tests(scalar).

test(scalars1a) :-
    scalars1_template(Template, Vars),
    Vars = [V_double,
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
            V_enum,
            V_key,
            V_value],
    read_file_to_codes('scalars1a_from_python.wire', WireStream, [encoding(octet),type(binary)]),
    % format(user_error, 'WireStream=~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    % print_term(Template, [output(user_error)]), nl(user_error),
    assertion(V_double   == 1.5),
    assertion(V_float    == 2.5),
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
    assertion(V_key      == "reticulated python"),
    assertion(V_value    == "網目錦蛇").

test(scalars1b) :-
    scalars1_template(Template, Vars),
    Vars = [V_double,
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
            V_enum,
            V_key,
            V_value],
    read_file_to_codes('scalars1b_from_python.wire', WireStream, [encoding(octet),type(binary)]),
    % format(user_error, 'WireStream=~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    % print_term(Template, [output(user_error)]), nl(user_error),
    assertion(V_double   ==  -1.5),
    assertion(V_float    ==  -2.5),
    assertion(V_int32    ==  -3),
    assertion(V_int64    ==  -4),
    assertion(V_uint32   =:=  5+10000000),
    assertion(V_uint64   =:=  6+10000000),
    assertion(V_sint32   ==  -7),
    assertion(V_sint64   ==  -8),
    assertion(V_fixed32  =:=  9+1000),
    assertion(V_fixed64  =:= 10+1000),
    assertion(V_sfixed32 == -11),
    assertion(V_sfixed64 == -12),
    assertion(V_bool     ==  true),
    assertion(V_string   ==  "[àmímé níshíkíhéꜜbì] reticulated python"),
    assertion(V_bytes    ==  [0xf0, 0x28, 0x8c, 0x28]),
    assertion(V_enum     ==  'AnotherEnum'),
    assertion(V_key      ==  "foo"),
    assertion(V_value    ==  "").


% test(repeated1a, blocked(not_implemented)) :-
%     repeated1a_template(Template, Vars),
%     Vars = [V_double,
%             V_float,
%             V_int32,
%             V_int64,
%             V_uint32,
%             V_uint64,
%             V_sint32,
%             V_sint64,
%             V_fixed32,
%             V_fixed64,
%             V_sfixed32,
%             V_sfixed64,
%             V_bool,
%             V_string,
%             V_bytes,
%             V_enum,
%             V_key1,
%             V_value1,
%             V_key2,
%             V_value2],
%     read_file_to_codes('repeated1a_from_python.wire', WireStream, [encoding(octet),type(binary)]),
%     % format(user_error, 'WireStream=~q~n', [WireStream]),
%     protobuf_message(Template, WireStream),
%     true.

:- end_tests(scalar).
