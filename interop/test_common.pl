% -*- mode: Prolog -*-

% Common stuff for test_read.pl, scala_test_write.pl

:- module(test_common, [scalars1_template/2, repeated1a_template/2, packed1a_template/2]).

% Define the my_enum callback:
protobufs:my_enum(Key, Value) :-
    nth0(Value,
         ['E1',
          'Enum2',
          'AnotherEnum'],
         Key).

scalars1_template(Template, Vars) :-
    % See test.Scalars1
    Template = protobuf([
                         double(      1, V_double),
                         float(       2, V_float),
                         signed64(  103, V_int32),  % not signed32, for wire-format compatibility
                         signed64(  127, V_int64),
                         unsigned(  128, V_uint32),
                         unsigned(  666, V_uint64),
                         integer(   777, V_sint32),
                         integer(   888, V_sint64),
                         integer32( 999, V_fixed32),
                         integer64(1010, V_fixed64),
                         integer32(1011, V_sfixed32),
                         integer64(1012, V_sfixed64),
                         boolean(  1013, V_bool),
                         string(   1014, V_string),
                         codes(    1015, V_bytes),
                         enum(     1016, my_enum(V_enum)),
                         embedded( 9999, protobuf([string(15,  V_key),
                                                   string(128, V_value)]))
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
                                       V_enum,
                                       V_key,
                                       V_value].

repeated1a_template(Template, Vars) :-
    % See test.Scalars1, test_write.py - repeated1a
    Template = protobuf([
                         repeated(    1, double(V_double)),
                         repeated(   12, float(V_float)),
                         repeated( 1103, signed64(V_int32)), % not signed32, for wire-format compatibility
                         repeated( 1127, signed64(V_int64)),
                         repeated( 1128, unsigned(V_uint32)),
                         repeated( 1666, unsigned(V_uint64)),
                         repeated( 1777, integer(V_sint32)),
                         repeated( 1888, integer(V_sint64)),
                         repeated( 1999, unsigned32(V_fixed32)),
                         repeated(11010, unsigned64(V_fixed64)),
                         repeated(11011, integer32(V_sfixed32)),
                         repeated(11012, integer64(V_sfixed64)),
                         repeated(11013, boolean(V_bool)),
                         repeated(11014, string(V_string)),
                         repeated(11015, codes(V_bytes)),
                         repeated(11016, enum(my_enum(V_enum))),
                         repeated_embedded(99999,
                                           protobuf([string(15, _Key),
                                                     string(128, _Value)]),
                                           V_key_values)
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
                                       V_enum,
                                       V_key_values
                                       ].

packed1a_template(Template, Vars) :-
    % repeated1a_template/2, using packed where possible
    Template = protobuf([
                         packed(    1, double(V_double)),
                         packed(   12, float(V_float)),
                         packed( 1103, signed64(V_int32)), % not signed32, for wire-format compatibility
                         packed( 1127, signed64(V_int64)),
                         packed( 1128, unsigned(V_uint32)),
                         packed( 1666, unsigned(V_uint64)),
                         packed( 1777, integer(V_sint32)),
                         packed( 1888, integer(V_sint64)),
                         packed( 1999, unsigned32(V_fixed32)),
                         packed(11010, unsigned64(V_fixed64)),
                         packed(11011, integer32(V_sfixed32)),
                         packed(11012, integer64(V_sfixed64)),
                         packed(11013, boolean(V_bool)),
                         repeated(11014, string(V_string)),
                         repeated(11015, codes(V_bytes)),
                         packed(11016, enum(my_enum(V_enum))),
                         repeated_embedded(99999,
                                           protobuf([string(15, _Key),
                                                     string(128, _Value)]),
                                           V_key_values)
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
                                       V_enum,
                                       V_key_values
                                       ].

