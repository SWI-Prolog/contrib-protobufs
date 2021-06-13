% -*- mode: Prolog -*-

:- module(test_read, [test_read_main/0]).

:- initialization(test_read_main, main).

:- use_module(library(protobufs)).
:- use_module(test_templates).

% Not needed because we're using plunit:
% :- set_prolog_flag(optimise_debug, false). % ensure assertion/1 is executed

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

:- end_tests(scalar).

:- begin_tests(repeated).

test(repeated1a) :-
    repeated1a_template(Template, Vars),
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
            V_key_values],
    read_file_to_codes('repeated1a_from_python.wire', WireStream, [encoding(octet),type(binary)]),
    % format(user_error, 'WireStream=~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    assertion(V_double     == [1.5, 0.0, -1.5]),
    assertion(V_float      == [2.5, 0.0, -2.5]),
    assertion(V_int32      == [3, -3, 555, 0, 2147483647, -2147483648]),
    assertion(V_int64      == [4, -4, 0, 9223372036854775807, -9223372036854775808]),
    assertion(V_uint32     == [5, 0, 4294967295]),
    assertion(V_uint64     == [6, 7, 8, 9, 0, 18446744073709551615]),
    assertion(V_sint32     == [7, -7, 0, 2147483647, -2147483648]),
    assertion(V_sint64     == [-8, 8, 0, 4611686018427387903]), % TODO: bug in integer_zigzag:  9223372036854775807, -9223372036854775808
    assertion(V_fixed32    == [9, 0, 4294967295]),
    assertion(V_fixed64    == [10, 0, 18446744073709551615]),
    assertion(V_sfixed32   == [-11, 11, 0, 2147483647, -2147483648]),
    assertion(V_sfixed64   == [-12, 12, 0, 9223372036854775807, -9223372036854775808]),
    assertion(V_bool       == [false, true]),
    assertion(V_string     == ["écran 網目錦蛇", "Hello world"]),
    assertion(V_bytes      == [[0xc3, 0x28], [0,1,2]]),
    assertion(V_enum       == ['E1','Enum2','E1']),
    assertion(V_key_values == [protobuf([string(15,"foo"),string(128,"")]),
                               protobuf([string(15,"àmímé níshíkíhéꜜbì"),
                                         string(128,"reticulated python")])]).

test(packed1a) :-
    packed1a_template(Template, Vars),
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
            V_key_values],
    read_file_to_codes('packed1a_from_python.wire', WireStream, [encoding(octet),type(binary)]),
    % format(user_error, 'WireStream=~q~n', [WireStream]),
    protobuf_message(Template, WireStream),
    assertion(V_double     == [1.5, 0.0, -1.5]),
    assertion(V_float      == [2.5, 0.0, -2.5]),
    assertion(V_int32      == [3, -3, 555, 0, 2147483647, -2147483648]),
    assertion(V_int64      == [4, -4, 0, 9223372036854775807, -9223372036854775808]),
    assertion(V_uint32     == [5, 0, 4294967295]),
    assertion(V_uint64     == [6, 7, 8, 9, 0, 18446744073709551615]),
    assertion(V_sint32     == [7, -7, 0, 2147483647, -2147483648]),
    assertion(V_sint64     == [-8, 8, 0, 4611686018427387903]), % TODO: bug in integer_zigzag:  9223372036854775807, -9223372036854775808
    assertion(V_fixed32    == [9, 0, 4294967295]),
    assertion(V_fixed64    == [10, 0, 18446744073709551615]),
    assertion(V_sfixed32   == [-11, 11, 0, 2147483647, -2147483648]),
    assertion(V_sfixed64   == [-12, 12, 0, 9223372036854775807, -9223372036854775808]),
    assertion(V_bool       == [false, true]),
    assertion(V_string     == ["écran 網目錦蛇", "Hello world"]),
    assertion(V_bytes      == [[0xc3, 0x28], [0,1,2]]),
    assertion(V_enum       == ['E1','Enum2','E1']),
    assertion(V_key_values == [protobuf([string(15,"foo"),string(128,"")]),
                               protobuf([string(15,"àmímé níshíkíhéꜜbì"),
                                         string(128,"reticulated python")])]).

:- end_tests(repeated).

end_of_file.
