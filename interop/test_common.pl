% -*- mode: Prolog -*-

% Common stuff for test_read.pl, scala_test_write.pl

:- module(test_common, [scalars1_template/2]).

% Define the enum callback:
protobufs:enum(Key, Value) :-
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
                         enum(     1016, enum(V_enum))
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
