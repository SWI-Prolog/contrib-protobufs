% -*- mode: Prolog coding:utf-8 -*-

:- module(test_read, [test_read_main/0]).

:- encoding(utf8).

:- initialization(test_read_main, main).

:- use_module(library(protobufs)).
:- use_module(test_templates).
:- use_module(test_pb).
:- use_module(google/protobuf/unittest_pb).
:- use_module(google/protobuf/unittest_import_pb).
:- use_module(google/protobuf/unittest_import_public_pb).

% Not needed because we're using plunit:
% :- set_prolog_flag(optimise_debug, false). % ensure assertion/1 is executed

test_read_main :-
    run_tests.

string_values(S1, S2, S3, S4) :-
    string_codes(S1, [0xe9, 0x63, 0x72, 0x61, 0x6e, 0x20, 0x7db2, 0x76ee, 0x9326, 0x86c7]),  % "écran 網目錦蛇"
    string_codes(S2, [0x7db2, 0x76ee, 0x9326, 0x86c7]),  % "網目錦蛇"
    string_codes(S3, [0x5b, 0xe0, 0x6d, 0xed, 0x6d, 0xe9, 0x20, 0x6e, 0xed, 0x73, 0x68, 0xed, 0x6b, 0xed, 0x68, 0xe9, 0xa71c, 0x62, 0xec, 0x5d, 0x20, 0x72, 0x65, 0x74, 0x69, 0x63, 0x75, 0x6c, 0x61, 0x74, 0x65, 0x64, 0x20, 0x70, 0x79, 0x74, 0x68, 0x6f, 0x6e]), % [àmímé níshíkíhéꜜbì] reticulated python"
    string_codes(S4, [0xe0, 0x6d, 0xed, 0x6d, 0xe9, 0x20, 0x6e, 0xed, 0x73, 0x68, 0xed, 0x6b, 0xed, 0x68, 0xe9, 0xa71c, 0x62, 0xec]). % àmímé níshíkíhéꜜbì"

%! assertion_eq_dict(+D1:dict, +D2:dict) is det.
% Convenience predicate - does assertion/2 check for the two dicts, then
% repeats field by field. This produces nicer error messages.
assertion_eq_dict(D1, D2) :-
    ( assertion(D1 == D2) -> true ; true ),
    dict_pairs(D1, _, D1pairs),
    dict_pairs(D2, _, D2pairs),
    maplist(assertion_eq, D1pairs, D2pairs).

assertion_eq(V1, V2) :-
    ( assertion(V1 == V2) -> true ; true ).

golden_message_codes(WireCodes) :-
    source_file(test_read:test_read_main, MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, '../golden_message.2.5.0'], /, Path),
    read_file_to_codes(Path, WireCodes, [encoding(octet),type(binary)]).

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
    read_file_to_codes('scalars1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    % format(user_error, 'WireCodes=~q~n', [WireCodes]),
    protobuf_message(Template, WireCodes),
    protobuf_message(Template, WireCodes2),
    assertion(WireCodes == WireCodes2),
    protobuf_message(Template, WireCodes2), % once more, with both Template and WireCodes2 fully instantiated
    % print_term(Template, [output(user_error)]), nl(user_error),
    string_values(S1, S2, _S3, _S4),
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
    assertion(V_string   == S1),
    assertion(V_bytes    == [0xc3, 0x28]),
    assertion(V_enum     ==  'E1'),
    assertion(V_key      == "reticulated python"),
    assertion(V_value    == S2).

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
    read_file_to_codes('scalars1b_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    % format(user_error, 'WireCodes=~q~n', [WireCodes]),
    protobuf_message(Template, WireCodes),
    protobuf_message(Template, WireCodes2),
    assertion(WireCodes == WireCodes2),
    protobuf_message(Template, WireCodes2), % once more, with both Template and WireCodes2 fully instantiated
    % print_term(Template, [output(user_error)]), nl(user_error),
    string_values(_S1, _S2, S3, _S4),
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
    assertion(V_string   ==  S3),
    assertion(V_bytes    ==  [0xf0, 0x28, 0x8c, 0x28]),
    assertion(V_enum     ==  'AnotherEnum'),
    assertion(V_key      ==  "foo"),
    assertion(V_value    ==  "").

test(scalars1a_parse) :-
    read_file_to_codes('scalars1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    protobuf_parse_from_codes(WireCodes, 'test.Scalars1', Term), % No leading '.' for the MessageType
    string_values(S1, S2, _S3, _S4),
    assertion_eq_dict(Term,
                      '.test.Scalars1'{
                                       v_double  :1.5,
                                       v_float   :2.5,
                                       v_int32   :3,
                                       v_int64   :4,
                                       v_uint32  :5,
                                       v_uint64  :6,
                                       v_sint32  :7,
                                       v_sint64  :8,
                                       v_fixed32 :9,
                                       v_fixed64 :10,
                                       v_sfixed32:11,
                                       v_sfixed64:12,
                                       v_bool    :false,
                                       v_string  :S1,
                                       v_bytes   :[195,40],
                                       v_enum    :'E1',
                                       v_key_value:'.test.KeyValue'{key:"reticulated python",
                                                                    value:S2}
                                      }).

test(scalars1b_parse) :-
    read_file_to_codes('scalars1b_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    protobuf_parse_from_codes(WireCodes, '.test.Scalars1', Term),
    string_values(_S1, _S2, S3, _S4),
    X5 is 5+10000000,
    X6 is 6+10000000,
    X9 is 9+1000,
    X10 is 10+1000,
    assertion_eq_dict(Term,
                      '.test.Scalars1'{
                                       v_double  : -1.5,
                                       v_float   : -2.5,
                                       v_int32   : -3,
                                       v_int64   : -4,
                                       v_uint32  :  X5,
                                       v_uint64  :  X6,
                                       v_sint32  : -7,
                                       v_sint64  : -8,
                                       v_fixed32 :  X9,
                                       v_fixed64 :  X10,
                                       v_sfixed32: -11,
                                       v_sfixed64: -12,
                                       v_bool    :  true,
                                       v_string  :  S3,
                                       v_bytes   :  [0xf0, 0x28, 0x8c, 0x28],
                                       v_enum    :  'AnotherEnum',
                                       v_key_value:'.test.KeyValue'{key:"foo",
                                                                    value:""}
                                      }).

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
    read_file_to_codes('repeated1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    % format(user_error, 'WireCodes=~q~n', [WireCodes]),
    protobuf_message(Template, WireCodes),
    protobuf_message(Template, WireCodes2),
    assertion(WireCodes == WireCodes2),
    protobuf_message(Template, WireCodes2), % once more, with both Template and WireCodes2 fully instantiated
    string_values(S1, _S2, _S3, S4),
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
    assertion(V_string     == [S1, "Hello world"]),
    assertion(V_bytes      == [[0xc3, 0x28], [0,1,2]]),
    assertion(V_enum       == ['E1','Enum2','E1']),
    assertion(V_key_values == [protobuf([string(15,"foo"),string(128,"")]),
                               protobuf([string(15,S4),
                                         string(128,"reticulated python")])]).

test(repeated1a_parse) :-
    read_file_to_codes('repeated1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    protobuf_parse_from_codes(WireCodes, '.test.Repeated1', Term),
    string_values(S1, _S2, _S3, S4),
    assertion_eq_dict(Term,
                      '.test.Repeated1'{
                                       v_double   : [ 1.5, 0.0, -1.5],
                                       v_float    : [ 2.5, 0.0, -2.5],
                                       v_int32    : [ 3, -3, 555, 0, 2147483647, -2147483648],
                                       v_int64    : [ 4, -4, 0, 9223372036854775807, -9223372036854775808],
                                       v_uint32   : [ 5, 0, 4294967295],
                                       v_uint64   : [ 6, 7, 8, 9, 0, 18446744073709551615],
                                       v_sint32   : [ 7, -7, 0, 2147483647, -2147483648],
                                       v_sint64   : [ -8, 8, 0, 4611686018427387903],
                                       v_fixed32  : [ 9, 0, 4294967295],
                                       v_fixed64  : [10, 0, 18446744073709551615],
                                       v_sfixed32 : [-11, 11, 0, 2147483647, -2147483648],
                                       v_sfixed64 : [-12, 12, 0, 9223372036854775807, -9223372036854775808],
                                       v_bool     : [false, true],
                                       v_string   : [S1, "Hello world"],
                                       v_bytes    : [[0xc3,0x28], [0x00,0x01,0x02]],
                                       v_enum     : ['E1', 'Enum2', 'E1'],
                                       v_key_value: ['.test.KeyValue'{key:"foo", value:""},
                                                     '.test.KeyValue'{key:S4,
                                                                      value:"reticulated python"}]
                                      }).

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
    read_file_to_codes('packed1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    % format(user_error, 'WireCodes=~q~n', [WireCodes]),
    protobuf_message(Template, WireCodes),
    string_values(S1, _S2, _S3, S4),
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
    assertion(V_string     == [S1, "Hello world"]),
    assertion(V_bytes      == [[0xc3, 0x28], [0,1,2]]),
    assertion(V_enum       == ['E1','Enum2','E1']),
    assertion(V_key_values == [protobuf([string(15,"foo"),string(128,"")]),
                               protobuf([string(15,S4),
                                         string(128,"reticulated python")])]).

test(packed1a_parse) :-
    read_file_to_codes('packed1a_from_python.wire', WireCodes, [encoding(octet),type(binary)]),
    % protobuf_segment_message(Segments, WireCodes),
    % print_term(Segments, []), nl(user_output), !,
    protobuf_parse_from_codes(WireCodes, '.test.Packed1', Term),
    string_values(S1, _S2, _S3, S4),
    assertion_eq_dict(Term,
                      '.test.Packed1'{
                                       v_double   : [ 1.5, 0.0, -1.5],
                                       v_float    : [ 2.5, 0.0, -2.5],
                                       v_int32    : [ 3, -3, 555, 0, 2147483647, -2147483648],
                                       v_int64    : [ 4, -4, 0, 9223372036854775807, -9223372036854775808],
                                       v_uint32   : [ 5, 0, 4294967295],
                                       v_uint64   : [ 6, 7, 8, 9, 0, 18446744073709551615],
                                       v_sint32   : [ 7, -7, 0, 2147483647, -2147483648],
                                       v_sint64   : [ -8, 8, 0, 4611686018427387903],
                                       v_fixed32  : [ 9, 0, 4294967295],
                                       v_fixed64  : [10, 0, 18446744073709551615],
                                       v_sfixed32 : [-11, 11, 0, 2147483647, -2147483648],
                                       v_sfixed64 : [-12, 12, 0, 9223372036854775807, -9223372036854775808],
                                       v_bool     : [false, true],
                                       v_string   : [S1, "Hello world"],
                                       v_bytes    : [[0xc3,0x28], [0x00,0x01,0x02]],
                                       v_enum     : ['E1', 'Enum2', 'E1'],
                                       v_key_value: ['.test.KeyValue'{key:"foo", value:""},
                                                     '.test.KeyValue'{key:S4,
                                                                      value:"reticulated python"}]
                                      }),
    true.  % protobuf_serialize_to_codes(Term, '.test.Packed1', WireCodes2).

:- end_tests(repeated).

:- begin_tests(golden).

% Taken from protobuf/src/google/protobuf/unittest.proto and
% protobuf_unittest.TestAllTypes (see also golden_message/1 in
% ../test_protobufs.pl)

test(golden_2_5_0) :- % , fixme(unimplemented:[start_group,end_group])) :-
    % Golden message contains groups, not supported
    golden_message_codes(WireCodes),
    % format('GOLDEN: ~q~n', [WireCodes]),
    % protobuf_segment_message(Segments, WireCodes),
    % print_term(Segments, []), nl(user_output), !,
    protobuf_parse_from_codes(WireCodes, '.protobuf_unittest.TestAllTypes', Term),
    % print_term(Term, []), nl(user_output),
    % To double-check the following:
    %    protoc -I. --decode=protobuf_unittest.TestAllTypes google/protobuf/unittest.proto <../golden_message.2.5.0
    % (the ordering is different because Prolog dicts display items in key order)
    % TODO: re-order these to match unittest.proto
    assertion_eq_dict(Term,
        '.protobuf_unittest.TestAllTypes'{ default_bool:false,
                                           default_bytes:[52,49,54],
                                           default_cord:"425",
                                           default_double:412.0,
                                           default_fixed32:407,
                                           default_fixed64:408,
                                           default_float:411.0,
                                           default_foreign_enum:'FOREIGN_FOO',
                                           default_import_enum:'IMPORT_FOO',
                                           default_int32:401,
                                           default_int64:402,
                                           default_nested_enum:'FOO',
                                           default_sfixed32:409,
                                           default_sfixed64:410,
                                           default_sint32:405,
                                           default_sint64:406,
                                           default_string:"415",
                                           default_string_piece:"424",
                                           default_uint32:403,
                                           default_uint64:404,
                                           optional_bool:true,
                                           optional_bytes:[49,49,54],
                                           optional_cord:"125",
                                           optional_double:112.0,
                                           optional_fixed32:107,
                                           optional_fixed64:108,
                                           optional_float:111.0,
                                           optional_foreign_enum:'FOREIGN_BAZ',
                                           optional_foreign_message:'.protobuf_unittest.ForeignMessage'{ c:119 },
                                           optional_import_enum:'IMPORT_BAZ',
                                           optional_import_message:'.protobuf_unittest_import.ImportMessage'{ d:120 },
                                           optional_int32:101,
                                           optional_int64:102,
                                           optional_lazy_message:'.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:127 },
                                           optional_nested_enum:'BAZ',
                                           optional_nested_message:'.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:118 },
                                           optional_public_import_message:'.protobuf_unittest_import.PublicImportMessage'{ e:126 },
                                           optional_sfixed32:109,
                                           optional_sfixed64:110,
                                           optional_sint32:105,
                                           optional_sint64:106,
                                           optional_string:"115",
                                           optional_string_piece:"124",
                                           optional_uint32:103,
                                           optional_uint64:104,
                                           optionalgroup:'.protobuf_unittest.TestAllTypes.OptionalGroup'{ a:117 },
                                           repeated_bool:[true,false],
                                           repeated_bytes:[ [50,49,54], % repeated_bytes: [b"216", b"316"]
                                                            [51,49,54]
                                                          ],
                                           repeated_cord:["225","325"],
                                           repeated_double:[212.0,312.0],
                                           repeated_fixed32:[207,307],
                                           repeated_fixed64:[208,308],
                                           repeated_float:[211.0,311.0],
                                           repeated_foreign_enum:[ 'FOREIGN_BAR',
                                                                   'FOREIGN_BAZ'
                                                                 ],
                                           repeated_foreign_message:[ '.protobuf_unittest.ForeignMessage'{ c:219 },
                                                                      '.protobuf_unittest.ForeignMessage'{ c:319 }
                                                                    ],
                                           repeated_import_enum:[ 'IMPORT_BAR',
                                                                  'IMPORT_BAZ'
                                                                ],
                                           repeated_import_message:[ '.protobuf_unittest_import.ImportMessage'{ d:220 },
                                                                     '.protobuf_unittest_import.ImportMessage'{ d:320 }
                                                                   ],
                                           repeated_int32:[201,301],
                                           repeated_int64:[202,302],
                                           repeated_lazy_message:[ '.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:227 },
                                                                   '.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:327 }
                                                                 ],
                                           repeated_nested_enum:['BAR','BAZ'],
                                           repeated_nested_message:[ '.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:218 },
                                                                     '.protobuf_unittest.TestAllTypes.NestedMessage'{ bb:318 }
                                                                   ],
                                           repeated_sfixed32:[209,309],
                                           repeated_sfixed64:[210,310],
                                           repeated_sint32:[205,305],
                                           repeated_sint64:[206,306],
                                           repeated_string:["215","315"],
                                           repeated_string_piece:["224","324"],
                                           repeated_uint32:[203,303],
                                           repeated_uint64:[204,304],
                                           repeatedgroup:[ '.protobuf_unittest.TestAllTypes.RepeatedGroup'{ a:217 },
                                                           '.protobuf_unittest.TestAllTypes.RepeatedGroup'{ a:317 }
                                                         ]
                                         }).


:- end_tests(golden).

end_of_file.
