/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, extended by Peter Ludemann
    E-mail:        jeffrose@acm.org, peter.ludemann.gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2018, Jeffrey Rosenwald
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

:- module(test_protobufs,
        [ test_protobufs/0
        ]).

% Trap syntax errors and halt. See https://github.com/SWI-Prolog/swipl-devel/issues/826
% TODO: Not needed any more? (As of swil 8.3.29, it is needed)
:- multifile user:message_hook/3.
:- dynamic user:message_hook/3.
user:message_hook(Term, error, Lines) :-
    Term = error(syntax_error(_Msg), _Details),
    print_message_lines(user_error, 'ERROR: ', Lines),
    halt(1).

:- use_module(library(plunit)).

:- asserta(user:file_search_path(library, .)).
:- asserta(user:file_search_path(foreign, .)).

:- use_module(library(protobufs)).

gmp :-
    current_prolog_flag(bounded, false).

protobufs:nested_enum(Key, Value) :-
    nested_enum(Key, Value).

nested_enum(foo, 1).
nested_enum(bar, 2).
nested_enum(baz, 3).

protobufs:foreign_enum(Key, Value) :-
    foreign_enum(Key, Value).

foreign_enum(foo, 4).
foreign_enum(bar, 5).
foreign_enum(baz, 6).

protobufs:import_enum(Key, Value) :-
    import_enum(Key, Value).

import_enum(foo, 7).
import_enum(bar, 8).
import_enum(baz, 9).

:- meta_predicate sorted_findall(?, 0, ?).
%! sorted_findall(+Template, :Goal, -Bag) is semidet.
% Call findall and msort - the result is similar to
% setof/3 except it always succeeds and it can contain duplicates.
% This is useful for testing where "all results" where we don't care
% about the order but we do care about duplicates.
sorted_findall(Template, Goal, Bag) :-
    findall(Template, Goal, Bag0),
    msort(Bag0, Bag).

test_numbers(Numbers) :-
    Numbers =
    numbers{
            max_signed32:          MaxSigned32,
            max_signed32_plus1:    MaxSigned32Plus1,
            max_signed64:          MaxSigned64,
            max_signed64_plus1:    MaxSigned64Plus1,
            max_unsigned32:        MaxUnsigned32,
            max_unsigned32_plus1:  MaxUnsigned32Plus1,
            max_unsigned64:        MaxUnsigned64,
            max_unsigned64_plus1:  MaxUnsigned64Plus1,
            min_signed32:          MinSigned32,
            min_signed32_minus1:   MinSigned32Minus1,
            min_signed64:          MinSigned64,
            min_signed64_minus1:   MinSigned64Minus1,
            min_unsigned32:        MinUnsigned32,
            min_unsigned32_minus1: MinUnsigned32Minus1,
            min_unsigned64:        MinUnsigned64,
            min_unsigned64_minus1: MinUnsigned64Minus1
           },
    % SWI-Prolog doesn't distinguish between int32, int64 and GNU
    % Multi-precision arithmetic integers. However, protobufs do, so
    % the following values get min and max 32- and 64-bit values.
    assertion(gmp),

    MaxSigned32         is 0x7fffffff, % 2147483647
    MaxSigned32Plus1    is MaxSigned32 + 1,
    MaxUnsigned32       is 0xffffffff,              % 4294967295
    MaxUnsigned32Plus1  is MaxUnsigned32 + 1,
    MinSigned32         is -0x7fffffff - 1,         % -2147483648
    MinSigned32Minus1   is MinSigned32 - 1,
    MinUnsigned32       is 0,
    MinUnsigned32Minus1 is MinUnsigned32 - 1,

    MaxSigned64         is 0x7fffffffffffffff,      %  9223372036854775807
    MaxSigned64Plus1    is MaxSigned64 + 1,
    MaxUnsigned64       is 0xffffffffffffffff,      % 18446744073709551615
    MaxUnsigned64Plus1  is MaxUnsigned64 + 1,
    MinSigned64         is -0x7fffffffffffffff - 1, % -9223372036854775808
    MinSigned64Minus1   is MinSigned64 - 1,
    MinUnsigned64       is 0,
    MinUnsigned64Minus1 is MinUnsigned64 - 1,

    % The values were hand-checked against limits.h on a 64-bit
    % machine. The following might fail on some weird architecture.
    assertion(Numbers ==
             numbers{
                     max_signed32:          2147483647,
                     max_signed32_plus1:    2147483648,
                     max_signed64:          9223372036854775807,
                     max_signed64_plus1:    9223372036854775808,
                     max_unsigned32:        4294967295,
                     max_unsigned32_plus1:  4294967296,
                     max_unsigned64:        18446744073709551615,
                     max_unsigned64_plus1:  18446744073709551616,
                     min_signed32:          -2147483648,
                     min_signed32_minus1:   -2147483649,
                     min_signed64:          -9223372036854775808,
                     min_signed64_minus1:   -9223372036854775809,
                     min_unsigned32:        0,
                     min_unsigned32_minus1: -1,
                     min_unsigned64:        0,
                     min_unsigned64_minus1: -1
                    }).


golden_message(Proto) :-
    % This corresponds to protobuf_unittest.TestAllTypes
    % defined in google/protobuf/unittest.proto
    % (from git@github.com:protocolbuffers/protobuf.git).
    % There's a similar test in interop.test_interop.pl,
    % which uses the unittest.prot definition.

    string_codes("116", Codes116),
    string_codes("216", Codes216),
    string_codes("316", Codes316),
    string_codes("225", Codes225),
    string_codes("325", Codes325),
    string_codes("416", Codes416),
    string_codes("424", Codes424),
    string_codes("425", Codes425),
    string_codes(String124, "124"),
    string_codes(String125, "125"),
    string_codes(String224, "224"),
    string_codes(String324, "324"),
    string_codes(String415, "415"),

    Proto = protobuf([ unsigned(1, 101),
                       unsigned(2, 102),
                       unsigned(3, 103),
                       unsigned(4, 104),
                       integer(5,105),
                       integer(6, 106),
                       integer32(7, 107),
                       integer64(8, 108),
                       integer32(9, 109),
                       integer64(10, 110),
                       float(11, 111.0),
                       double(12, 112.0),
                       boolean(13, true),
                       atom(14, '115'),
                       codes(15, Codes116),
                       group(16, [unsigned(17, 117)]),
                       embedded(18, protobuf([unsigned(1, 118)])),             % nested_message
                       embedded(19, protobuf([unsigned(1, 119)])),             % foreign_message
                       embedded(20, protobuf([unsigned(1, 120)])),             % import message
                       enum(21, nested_enum(baz)),                             % nested_enum  BAZ
                       enum(22, foreign_enum(baz)),                            % nested_enum  FOREIGN_BAZ
                       enum(23, import_enum(baz)),                             %  nested_enum IMPORT_BAZ
                       string(24, String124),                                  % string_piece
                       string(25, String125),                                  % cord
                       codes(26, [8, 126]),                                    % public_import_message
                       codes(27, [8, 127]),                                    % lazy message
                       repeated(31, unsigned([201, 301])),
                       repeated(32, unsigned([202, 302])),
                       repeated(33, unsigned([203, 303])),
                       repeated(34, unsigned([204, 304])),
                       repeated(35, integer([205, 305])),
                       repeated(36, integer([206, 306])),
                       repeated(37, integer32([207, 307])),
                       repeated(38, integer64([208, 308])),
                       repeated(39, integer32([209, 309])),
                       repeated(40, integer64([210, 310])),
                       repeated(41, float([211.0, 311.0])),
                       repeated(42, double([212.0, 312.0])),
                       repeated(43, boolean([true, false])),
                       repeated(44, atom(['215', '315'])),
                       repeated(45, codes([Codes216, Codes316])),
                       repeated(46, group([[unsigned(47, 217)], [unsigned(47, 317)]])),
                       repeated(48, embedded([protobuf([unsigned(1, 218)]),
                                              protobuf([unsigned(1,318)])])),  % nested
                       repeated(49, embedded([protobuf([unsigned(1, 219)]),
                                              protobuf([unsigned(1, 319)])])), % foreign
                       repeated(50, embedded([protobuf([unsigned(1, 220)]),
                                              protobuf([unsigned(1, 320)])])), % import
                       repeated(51, enum(nested_enum([bar, baz]))),
                       repeated(52, enum(foreign_enum([bar, baz]))),
                       repeated(53, enum(import_enum([bar, baz]))),
                       repeated(54, string([String224, String324])),           % string_piece
                       repeated(55, codes([Codes225, Codes325])),              % cord
                       repeated(57, embedded([protobuf([unsigned(1,227)]),     % lazy msg
                                              protobuf([unsigned(1,327)])])),
                       unsigned(61, 401),                                      % default_int32
                       unsigned(62, 402),
                       unsigned(63, 403),
                       unsigned(64, 404),
                       integer(65, 405),
                       integer(66, 406),
                       integer32(67, 407),
                       integer64(68, 408),
                       integer32(69, 409),
                       integer64(70, 410),
                       float(71, 411.0),
                       double(72, 412.0),
                       boolean(73, false),
                       string(74, String415),
                       codes(75, Codes416),
                       enum(81, nested_enum(foo)),
                       enum(82, foreign_enum(foo)),
                       enum(83, import_enum(foo)),
                       codes(84, Codes424),
                       codes(85, Codes425)
                     ]).

golden_message_template(Proto) :-
    Proto = protobuf([ unsigned(_, _),
                       unsigned(_, _),
                       unsigned(_, _),
                       unsigned(_, _),
                       integer(_,_),
                       integer(_, _),
                       integer32(_, _),
                       integer64(_, _),
                       integer32(_, _),
                       integer64(_, _),
                       float(_, _),
                       double(_, _),
                       boolean(_, _),
                       atom(_, _),
                       codes(_, _),
                       group(_, [unsigned(_, _)]),
                       embedded(_, protobuf([unsigned(_, _)])),             % nested_message
                       embedded(_, protobuf([unsigned(_, _)])),             % foreign_message
                       embedded(_, protobuf([unsigned(_, _)])),             % import message
                       enum(_, nested_enum(_)),                             % nested_enum  BAZ
                       enum(_, foreign_enum(_)),                            % nested_enum  FOREIGN_BAZ
                       enum(_, import_enum(_)),                             %  nested_enum IMPORT_BAZ
                       string(_, _),                                        % string_piece
                       string(_, _),                                        % cord
                       codes(_, _),                                         % public_import_message
                       codes(_, _),                                         %lazy message
                       repeated(_, unsigned(_)),
                       repeated(_, unsigned(_)),
                       repeated(_, unsigned(_)),
                       repeated(_, unsigned(_)),
                       repeated(_, integer(_)),
                       repeated(_, integer(_)),
                       repeated(_, integer32(_)),
                       repeated(_, integer64(_)),
                       repeated(_, integer32(_)),
                       repeated(_, integer64(_)),
                       repeated(_, float(_)),
                       repeated(_, double(_)),
                       repeated(_, boolean(_)),
                       repeated(_, atom(_)),
                       repeated(_, codes(_)),
                       repeated(_, group([[unsigned(_, _)], [unsigned(_, _)]])),
                       repeated(_, embedded([protobuf([unsigned(_, _)]),
                                             protobuf([unsigned(_,_)])])),  % nested
                       repeated(_, embedded([protobuf([unsigned(_, _)]),
                                             protobuf([unsigned(_, _)])])), % foreign
                       repeated(_, embedded([protobuf([unsigned(_, _)]),
                                             protobuf([unsigned(_, _)])])), % import
                       repeated(_, enum(nested_enum(_))),
                       repeated(_, enum(foreign_enum(_))),
                       repeated(_, enum(import_enum(_))),
                       repeated(_, string(_)),                              % string_piece
                       repeated(_, codes(_)),                               % cord
                       repeated(_, embedded([protobuf([unsigned(_,_)]),
                                             protobuf([unsigned(_,_)])])),
                       unsigned(_, _),                                      % default_int_
                       unsigned(_, _),
                       unsigned(_, _),
                       unsigned(_, _),
                       integer(_, _),
                       integer(_, _),
                       integer32(_, _),
                       integer64(_, _),
                       integer32(_, _),
                       integer64(_, _),
                       float(_, _),
                       double(_, _),
                       boolean(_, _),
                       string(_, _),
                       codes(_, _),
                       enum(_, nested_enum(_)),
                       enum(_, foreign_enum(_)),
                       enum(_, import_enum(_)),
                       codes(_, _),
                       codes(_, _)
                     ]).

% Define the my_enum callback -- see interop/test.proto MyEnum:
protobufs:my_enum(Key, Value) :-
    nth0(Value,
         ['E1',
          'Enum2',
          'AnotherEnum'],
         Key).

% For testing C implementation of protobufs:uint64_codes/2.
pl_uint64_codes(X, Codes) :-
    Codes = [C0,C1,C2,C3,C4,C5,C6,C7],
    (   integer(X)
    ->  X =< 18446744073709551615,  % 0xff ff ff ff ff ff ff ff
        X >= -9223372036854775808,  % ( 2s complement) 0x80.....
        C0 is  X        /\ 0xff,
        C1 is (X >>  8) /\ 0xff,
        C2 is (X >> 16) /\ 0xff,
        C3 is (X >> 24) /\ 0xff,
        C4 is (X >> 32) /\ 0xff,
        C5 is (X >> 40) /\ 0xff,
        C6 is (X >> 48) /\ 0xff,
        C7 is (X >> 56) /\ 0xff
    ;   X is C0 + (C1 << 8) + (C2 << 16) + (C3 << 24) +
             (C4 << 32) + (C5 << 40) + (C6 << 48) + (C7 << 56)
    ).

% For testing C implementation of protobufs:uint32_codes/2.
pl_uint32_codes(X, Codes) :-
    Codes = [C0,C1,C2,C3],
    (   integer(X)
    ->  X =<  4294967295,   % 0xff ff ff ff
        X >= -2147483648,   % ( 2s complement) 0x80.....
        C0 is  X        /\ 0xff,
        C1 is (X >>  8) /\ 0xff,
        C2 is (X >> 16) /\ 0xff,
        C3 is (X >> 24) /\ 0xff
    ;   X is C0 + (C1 << 8) + (C2 << 16) + (C3 << 24)
    ).

% For testing uint64_int64/2:
pl_uint64_int64(Uint64, Int64) :-
    (   nonvar(Uint64)
    ->  (   Uint64 > 0x7fffffffffffffff
        ->  Int64 is -(Uint64 xor 0xffffffffffffffff + 1) % 2s complement
        ;   Int64 = Uint64
        )
    ;   (   Int64 < 0
        ->  Uint64 is -(Int64 xor 0xffffffffffffffff + 1) % 2s complement
        ;   Uint64 = Int64
        )
    ).

% For testing uint64_int32/2:
pl_uint64_int32(Uint64, Int32) :-
    (   nonvar(Uint64)
    ->  (   Uint64 > 0x7fffffff
        ->  Int32 is -(Uint64 xor 0xffffffffffffffff + 1) % 2s complement
        ;   Int32 = Uint64
        )
    ;   (   Int32 < 0
        ->  Uint64 is -(Int32 xor 0xffffffffffffffff + 1) % 2s complement
        ;   Uint64 = Int32
        )
    ).

% For testing uint64_zigzag/2:
pl_uint64_zigzag(Uint64, ZigZag) :-
    (   integer(Uint64)
    ->  ZigZag is (Uint64 << 1) xor (Uint64 >> 63)
    ;   Uint64 is (ZigZag >> 1) xor (-1 * (ZigZag /\ 1))
    ).


test_protobufs :- run_tests. % ([ protobuf_message,
                             %    protobuf_message_2,
                             %    some_message_example,
                             %    repeated_fields,
                             %    protobuf_segment_convert,
                             %    codes,
                             %    zigzag,
                             %    coerce
                             %  ]).

test_input(Name, Path) :-
    source_file(test_protobufs, MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, Name], /, Path).

golden_message_codes(Wirestream) :-
    test_input('./golden_message.2.5.0', Gold250),
    read_file_to_codes(Gold250, Wirestream, [encoding(octet),type(binary)]).

:- begin_tests(protobuf_message).

% The original test suite had a series of tests that built on each
% other.  The tests below have taken those tests and separated them
% out, so there's some duplication on setup between tests.

% The "Test...-" at the beginning of a test name references the
% original test that was written before the tests were converted to
% use plunit.

test(original) :-
    % These are the executable parts from the original test. It is
    % preserved here, in case there was a mistake in defining the
    % indvidual tests.
    golden_message(Message),
    golden_message_template(Template),
    copy_term(Template, Template1),
    copy_term(Template, Template2),
    test_input('./golden_message.2.5.0', Gold250),
    read_file_to_codes(Gold250, Wirestream, [type(binary)]), % Test1a - Loading Google''s Golden Wirestream (2.5.0)
    (Message = Template, Message == Template),               % Test1  - Unifying canned Golden Message with canned Golden Template
    protobuf_message(Message, Wirestream),                   % Test2  - Unifying canned Golden Message with Google''s Golden Wirestream
    protobuf_message(Template2, Wirestream),                 % Test3  - Parsing Google''s Golden Wirestream to canned Golden Template
    Message == Template2,                                    % Test3a - Comparing canned Golden Message to parsed Golden Template
    protobuf_message(Message, Codes ),                       % Test4  - Serializing canned Golden Message to Codes
    (Wirestream == Codes),                                   % Test4a - Comparing Google''s Golden Wirestream to Codes
    protobuf_message(Template1, Codes),                      % Test5  - Parsing Codes to canned Golden Template
    (Message == Template1).                                  % Test6  - Comparing canned Golden Message to parsed Golden Template

test("Test1a,Test1 - test set-up check: Unifying canned Golden Message with canned Golden Template") :-
    golden_message(Message),
    golden_message_template(Template),
    % golden_message_template/1, golden_message/1 have same "shape":
    assertion(subsumes_term(Template, Message)),
    Message = Template,
    % TODO: Why the following test? (It was in the original test file.)
    Message == Template.

test("Test2 - Unifying canned Golden Message with Google's Golden Wirestream") :-
    golden_message_codes(Wirestream),
    golden_message(Message),
    protobuf_message(Message, Wirestream).

test("Test3,Test3a - Parsing Google's Golden Wirestream to canned Golden Template, Comparing canned Golden Message to parsed Golden Template") :-
    golden_message_codes(Wirestream),
    golden_message_template(Template2),
    protobuf_message(Template2, Wirestream),
    golden_message(Message),
    assertion(Message == Template2).

test("Test4,Test4a - Serializing canned Golden Message to Codes, Comparing Google's Golden Wirestream to Codes") :-
    golden_message(Message),
    protobuf_message(Message, Codes),
    golden_message_codes(Wirestream),
    assertion(Wirestream == Codes).

test("Test5,Test6 - Parsing Codes to canned Golden Template, Comparing canned Golden Message to parsed Golden Template") :-
    golden_message(Message),
    golden_message_template(Template1),
    protobuf_message(Message, Codes),
    protobuf_message(Template1, Codes),
    assertion(Message == Template1).

:- end_tests(protobuf_message).

:- begin_tests(protobuf_message_2).

% Some additional tests that aren't fully exercised by the original
% tests or that are useful as examples.

test(my_enum_msg) :-
    % include interop/test.proto MyEnum
    % message M0 {
    %   repeated MyEnum v_enum = 1 [packed=false];
    % }
    % M0(v_enum=[MyEnum.E1, MyEnum.Enum2, MyEnum.E1])
    Wire = [8,0,8,1,8,0],
    Template = protobuf([repeated(1, enum(my_enum(V_enum)))]),
    protobuf_message(Template, Wire),
    assertion(V_enum == ['E1','Enum2','E1']).

test(embedded_key_value) :-
    % message KeyValue {
    %   optional string key = 15;
    %   optional string value = 128;
    % }
    % message M2 {
    %   optional KeyValue v_key_value = 5;
    % }
    % M2(v_key_value=KeyValue(key="foo", value="bar"))
    Wire = [42,11,122,3,102,111,111,130,8,3,98,97,114],
    Template = protobuf([embedded(5, protobuf([string(15,Key),string(128,Value)]))]),
    protobuf_message(Template, Wire),
    assertion(Key == "foo"),
    assertion(Value == "bar").

:- end_tests(protobuf_message_2).

:- begin_tests(some_message_example).

some_message_wire(Wire) :-
    % This is output generated by some_message.py
    Wire = [8, 100, 18, 4, 97, 98, 99, 100, 26, 3, 102, 111, 111, 26, 3, 98, 97, 114, 32, 1, 42, 17, 8, 179, 10, 18, 12, 110, 101, 103, 97, 116, 105, 118, 101, 32, 54, 54, 54, 50, 20, 8, 164, 19, 18, 15, 111, 110, 101, 116, 119, 111, 116, 104, 114, 101, 101, 102, 111, 117, 114, 50, 14, 8, 220, 34, 18, 9, 102, 111, 117, 114, 32, 116, 119, 111, 115, 56, 2, 56, 4, 56, 6, 56, 8, 66, 6, 200, 1, 143, 3, 208, 15].

some_message_template(Template) :-
    Template = protobuf([
        unsigned(1, 100),
        string(2, "abcd"),
        repeated(3, atom([foo, bar])),
        boolean(4, true),
        embedded(5, protobuf([integer(1, -666), string(2, "negative 666")])),
        repeated(6, embedded([protobuf([integer(1, 1234), string(2, "onetwothreefour")]),
                              protobuf([integer(1, 2222), string(2, "four twos")])])),
        repeated(7, integer([1,2,3,4])),
        packed(8, integer([100,-200,1000]))
       ]).

test(some_message_wire) :-
    some_message_template(Template),
    some_message_wire(ExpectedWire),
    protobuf_message(Template, WireStream),
    assertion(WireStream == ExpectedWire).

:- end_tests(some_message_example).

:- begin_tests(repeated_fields).

% Taken from https://developers.google.com/protocol-buffers/docs/encoding#packed
%
% message Test4 {
%  repeated int32 d = 4 [packed=true];
% }

% Construct a Test4, providing the values 3, 270, and 86942 for the
% repeated field d. Then, the encoded form would be:
%   22        // key (field number 4, wire type 2)
%   06        // payload size (6 bytes)
%   03        // first element (varint 3)
%   8E 02     // second element (varint 270)
%   9E A7 05  // third element (varint 86942)

test(packed1) :-
    % WireCodes = [250,163,232,3,8,34,6,3,142,2,158,167,5].
    % verified by writing WireCodes to file ww.wire and defining ww.proto:
    %   message WW1 { repeated sfixed32 m1 = 999999 [packed=true]; }
    %   message WW2 { repeated sfixed64 m2 = 999999 [packed=true]; }
    % protoc --decode=WW1 ww.proto <ww.wire
    Segment = message(999999,[packed(4,varint([3,270,86942]))]),
    sorted_findall(S, protobufs:protobuf_segment_convert(Segment, S), Ss),
    assertion(Ss == [length_delimited(999999,[34,6,3,142,2,158,167,5]),
                     message(999999,[length_delimited(4,[3,142,2,158,167,5])]),
                     message(999999,[packed(4,varint([3,270,86942]))]),
                     packed(999999,fixed32([-1912404446,94871042])),
                     packed(999999,fixed64([407468025110005282])),
                     packed(999999,varint([34,6,3,270,86942]))
                    ]).

test(not_packed_repeated) :-
    Message = protobuf([repeated(4, unsigned([3, 270, 86942]))]),
    Template = protobuf([repeated(Tag, unsigned(Ints))]),
    protobuf_message(Message, WireStream),
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, WireStream), AllSegments),
    % Note: this leaves a choicepoint even though there's
    %       no alternative (it's not a length_delimited segment,
    %       so there's no "Codes" to backtrack over).
    assertion(AllSegments == [[varint(4,3), varint(4,270), varint(4,86942)]]),
    protobuf_message(Template, WireStream),
    assertion(Template == Message),
    assertion(Tag == 4),
    assertion(Ints == [3, 270, 86942]),
    assertion(WireStream == [32,3,32,142,2,32,158,167,5]).

test(not_packed_repeated2) :-
    % Same as not_packed_repeated, but wrapped in a length_delimited
    % segment, so it backtracks.
    % The protobuf_segment_message/2 test has been moved to
    % not_packed_repeated2_gmp.
    Message = protobuf([embedded(666,
                                 protobuf([repeated(4, unsigned([3, 270, 86942]))]))]),
    Template = protobuf([embedded(_Tag0,
                                  protobuf([repeated(_Tag, unsigned(_Ints))]))]),
    protobuf_message(Message, WireStream),
    protobuf_message(Template, WireStream),
    assertion(Template == Message),
    assertion(WireStream == [210,41,9,32,3,32,142,2,32,158,167,5]).

test(not_packed_repeated2_gmp, condition(gmp)) :-
    % The protobuf_segment_message/2 part of not_packed_repeated2.
    % TODO: why does protobuf_segment_message/2 need GMP?
    WireStream = [210,41,9,32,3,32,142,2,32,158,167,5], % See not_packed_repeated2.
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, WireStream), AllSegments),
    assertion(AllSegments == [[length_delimited(666,[32,3,32,142,2,32,158,167,5])],
                              [message(666,[varint(4,3), varint(4,270), varint(4,86942)])],
                              [packed(666,varint([32,3,32,270,32,86942]))]
                             ]).

test(packed_repeated) :-
    % Example from https://developers.google.com/protocol-buffers/docs/encoding#packed
    %   message Test 4 { repeated int32 d = 4 [packed=true]; }
    %   22        // key (field number 4, wire type 2)
    %   06        // payload size   (6 bytes)
    %   03        // first element  (varint 3)
    %   8E 02     // second element (varint 270)
    %   9E A7 05  // third element  (varint 86942)
    Message = protobuf([packed(4, unsigned([3, 270, 86942]))]),
    Template = protobuf([packed(_Tag, unsigned([_I0, _I1, _I2]))]),
    protobuf_message(Message, WireStream),
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, WireStream), AllSegments),
    protobuf_message(Template, WireStream),
    assertion(AllSegments == [[length_delimited(4,[3,142,2,158,167,5])],
                              [packed(4, varint([3, 270, 86942]))]
                             ]),
    %                       [  34,    6,    3,  142,    2,  158,  167,    5]
    assertion(WireStream == [0x22, 0x06, 0x03, 0x8E, 0x02, 0x9E, 0xA7, 0x05]),
    assertion(Template == Message).

test(packed_repeated2) :-
    % Same as packed_repeated, but wrapped in a length_delimited segment,
    % so it backtracks.
    Message = protobuf([embedded(999999,
                                 protobuf([packed(4, unsigned([3, 270, 86942]))]))]),
    Template = protobuf([embedded(_Tag0,
                                  protobuf([packed(_Tag, unsigned(_Ints))]))]),
    protobuf_message(Message, WireStream),
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, WireStream), AllSegments),
    protobuf_message(Template, WireStream),
    assertion(AllSegments == [[length_delimited(999999,[34,6,3,142,2,158,167,5])],
                              [message(999999,[length_delimited(4,[3,142,2,158,167,5])])],
                              [message(999999,[packed(4,varint([3,270,86942]))])],
                              [packed(999999,fixed32([-1912404446,94871042]))],
                              [packed(999999,fixed64([407468025110005282]))],
                              [packed(999999,varint([34,6,3,270,86942]))]
                             ]),
    assertion(WireStream == [250,163,232,3,8,34,6,3,142,2,158,167,5]),
    assertion(Template == Message).

test(packed_and_unpacked_repeated) :- % , condition(gmp)) :-
    % Combines not_packed_repeated2 and packed_repeated2
    % The protobuf_segment_message/2 part has been moved to packed_and_unpacked_repeated_gmp.
    Message = protobuf([embedded(666,
                                 protobuf([repeated(4, unsigned([3, 270, 86942]))])),
                        embedded(999999,
                                 protobuf([packed(4, unsigned([3, 270, 86942]))]))]),
    Template = protobuf([embedded(_Tag0_a,
                                  protobuf([repeated(_Tag1_a, unsigned(_Ints0_a))])),
                         embedded(_Tag0_b,
                                  protobuf([packed(_Tag1_b, unsigned(_Ints0_b))]))]),
    protobuf_message(Message, WireStream),
    assertion(WireStream == [210,41,9,32,3,32,142,2,32,158,167,5,250,163,232,3,8,34,6,3,142,2,158,167,5]),
    protobuf_message(Template, WireStream),
    assertion(Template == Message).

test(packed_and_unpacked_repeated_gmp, condition(gmp)) :-
    % The protobuf_segment_message/2 part of packed_and_unpacked_repeated.
    % TODO: why does protobuf_segment_message/2 need GMP?
    WireStream = [210,41,9,32,3,32,142,2,32,158,167,5,250,163,232,3,8,34,6,3,142,2,158,167,5], % see packed_and_unpacked_repeated.
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, WireStream), AllSegments),
    % The result is combinatoric explosion:
    sorted_findall([S1,S2], ( member(S1, [ length_delimited(666,[32,3,32,142,2,32,158,167,5]),
                                           message(666,[varint(4,3),varint(4,270),varint(4,86942)]),
                                           packed(666,varint([32,3,32,270,32,86942]))
                                  ]),
                              member(S2, [ length_delimited(999999,[34,6,3,142,2,158,167,5]),
                                           message(999999,[packed(4,varint([3,270,86942]))]),
                                           message(999999,[length_delimited(4,[3,142,2,158,167,5])]),
                                           packed(999999,varint([34,6,3,270,86942])),
                                           packed(999999,fixed64([407468025110005282])),
                                           packed(999999,fixed32([-1912404446,94871042]))
                                         ]) ),
                   ExpectedSegments),
    assertion(AllSegments == ExpectedSegments).

test(repeated_key_value) :-
    % KeyValue as in embedded_key_value
    % message M1 {
    %   repeated KeyValue v_key_value = 12 [packed=false];
    % }
    % M1(v_key_value=[KeyValue(key="foo", value="bar"), KeyValue(key="x", value="y")])
    % --decode_raw:
    %   12 { 15: "foo", 128: "bar" }
    %   12 { 15: "x",   128: "y" }
    Wire = [98,11,122,3,102,111,111,130,8,3,98,97,114,98,7,122,1,120,130,8,1,121],
    Template = protobuf([repeated_embedded(12,
                                           protobuf([string(15,Key),string(128,Value)]),
                                           KeyValueList)]),
    protobuf_message(Template, Wire),
    assertion(var(Key)),    % "template" variable shouldn't get instantiated
    assertion(var(Value)),  % "template" variable shouldn't get instantiated
    assertion(KeyValueList == [protobuf([string(15,"foo"),string(128,"bar")]),
			       protobuf([string(15,"x"),string(128,"y")])]).

:- end_tests(repeated_fields).

:- begin_tests(protobuf_segment_convert).

test_data(Ld, Msg, Packed, Str, Codes) :-
    Ld  = length_delimited(10,[105,110,112,117,116,84,121,112,101]),
    Msg = message(10,[fixed64(13,7309475598860382318)]),
    Str = string(10,"inputType"),
    Packed = packed(10,varint([105,110,112,117,116,84,121,112,101])),
    Codes = [82,9,105,110,112,117,116,84,121,112,101].

test(protobuf_message) :-
    test_data(Ld, Msg, Packed, Str, Codes),
    sorted_findall(Segments, protobufs:protobuf_segment_message(Segments, Codes), AllSegments),
    assertion(AllSegments == [[Ld], [Msg], [Packed], [Str]]),
    protobufs:protobuf_segment_message([Msg], CodesFromMsg),
    assertion(CodesFromMsg == Codes).

test(protobuf_message2) :-
    test_data(Ld, Msg, Packed, Str, _Codes),
    % Check that we can reinterpret a segment that comes out
    % in an unexpected form:
    sorted_findall(S, protobufs:protobuf_segment_convert(Msg, S), Ss),
    assertion(Ss == [Ld, Msg, Packed, Str]).

test(message_string1,
     [true(Strs == [Ld, Msg, Packed, Str])]) :-
    test_data(Ld, Msg, Packed, Str, _Codes),
    sorted_findall(S, protobufs:protobuf_segment_convert(Msg, S), Strs).

test(message_string2,
     [true(Strs == [Str])]) :-
    test_data(_, Msg, _, Str, _),
    % protobufs:protobuf_segment_convert/2 leaves a choicepoint - ensure that
    % there's only one result
    sorted_findall(Str, protobufs:protobuf_segment_convert(Msg, Str), Strs).

test(message_string3,
     [true(Strs == [Str])]) :-
    test_data(_, Msg, _, Str, _),
    % protobufs:protobuf_segment_convert/2 leaves a choicepoint - ensure that
    % there's only one result
    sorted_findall(S,
                   ( S = string(_,_), protobufs:protobuf_segment_convert(Msg, S ) ),
                   Strs).

test(message_length_delimited1) :-
    test_data(Ld, Msg, _, _, _),
    protobufs:protobuf_segment_convert(Msg, Ld).

test(message_length_delimited2,
     [true(Ld == Ld2)]) :-
    test_data(Ld, Msg, _, _, _),
    Ld2 = length_delimited(_,_),
    protobufs:protobuf_segment_convert(Msg, Ld2).

test(string_length_delimited1) :-
    test_data(Ld, _, _,  Str, _),
    protobufs:protobuf_segment_convert(Str, Ld).

test(string_length_delimited2,
     [true(Xs == [Ld, Msg, Packed, Str])]) :-
    test_data(Ld, Msg, Packed, Str,_),
    sorted_findall(X, protobufs:protobuf_segment_convert(Str, X), Xs).

:- end_tests(protobuf_segment_convert).

:- begin_tests(codes).

% See https://github.com/SWI-Prolog/contrib-protobufs/issues/5

round_trip_uint32(In, Codes, Out) :-
    assertion(ground([In,Codes,Out])),
    protobufs:uint32_codes(In, Codes0),
    assertion(Codes0 == Codes),
    protobufs:uint32_codes(I0, Codes),
    assertion(I0 == Out),
    % repeat above, for pl_...
    pl_uint32_codes(In, Codes1),
    assertion(Codes1 == Codes),
    pl_uint32_codes(I1, Codes),
    assertion(I1 == Out).

round_trip_uint64(In, Codes, Out) :-
    assertion(ground([In,Codes,Out])),
    protobufs:uint64_codes(In, Codes0),
    assertion(Codes0 == Codes),
    protobufs:uint64_codes(I0, Codes),
    assertion(I0 == Out),
    % repeat above, for pl_...
    pl_uint64_codes(In, Codes1),
    assertion(Codes1 == Codes),
    pl_uint64_codes(I1, Codes),
    assertion(I1 == Out).

round_trip_uint32_int32(Uint, Int) :-
    protobufs:uint32_int32(Uint, ComputedInt),
    assertion(ComputedInt == Int),
    protobufs:uint32_int32(ComputedUint, Int),
    assertion(ComputedUint = Uint).

round_trip_int64_float64(Float0) :-
    must_be(ground, Float0),
    protobufs:float64_codes(Float0, Codes0),
    protobufs:uint64_codes(Uint0, Codes0),
    protobufs:uint64_int64(Uint0, Int0),
    protobufs:int64_float64(Int0, Float1),
    protobufs:int64_float64(Int1, Float0),
    assertion(Int0 == Int1),
    assertion(Float0 == Float1),
    protobufs:uint64_int64(Uint1, Int1),
    protobufs:uint64_codes(Uint1, Codes2),
    protobufs:float64_codes(Float1, Codes3),
    assertion(Codes2 == Codes3).

round_trip_int32_float32(Float0) :-
    must_be(ground, Float0),
    protobufs:float32_codes(Float0, Codes0),
    protobufs:uint32_codes(Uint0, Codes0),
    protobufs:uint32_int32(Uint0, Int0),
    protobufs:int32_float32(Int0, Float1),
    protobufs:int32_float32(Int1, Float0),
    assertion(Int0 == Int1),
    assertion(Float0 == Float1),
    protobufs:uint32_int32(Uint1, Int1),
    protobufs:uint32_codes(Uint1, Codes2),
    protobufs:float32_codes(Float1, Codes3),
    assertion(Codes2 == Codes3).

test(uint32_codes, condition(gmp)) :-
    test_numbers(Numbers),
    round_trip_uint32(Numbers.max_unsigned32, [0xff,0xff,0xff,0xff], Numbers.max_unsigned32),
    round_trip_uint32(2147483648, [0,0,0,0x80], 2147483648).

test(uint32_codes_e1, [error(instantiation_error,context(protobufs:uint32_codes/2,_))]) :-
    protobufs:uint32_codes(_, _).

test(uint32_codes_e2,
     [ condition(gmp),
       error(representation_error(uint),context(protobufs:uint32_codes/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint32_codes(Numbers.max_unsigned32_plus1, _).

test(uint32_codes_e3, [error(type_error(list,[1,2,3,4,5]),context(protobufs:uint32_codes/2,_))]) :-
    protobufs:uint32_codes(_, [1,2,3,4,5]).

test(uint32_codes_e4, [error(representation_error(uint),context(protobufs:uint32_codes/2,_))]) :-
    protobufs:uint32_codes(-1, _).

test(uint64_codes, condition(gmp)) :-
    test_numbers(Numbers),
    round_trip_uint64(Numbers.max_unsigned64, [0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff], Numbers.max_unsigned64),
    protobufs:uint64_int64(MinSigned64, Numbers.min_signed64),
    round_trip_uint64(MinSigned64, [0,0,0,0,0,0,0,0x80], Numbers.max_signed64_plus1).

test(uint32_int32, condition(gmp)) :-
    test_numbers(Numbers),
    round_trip_uint32_int32(0, 0),
    round_trip_uint32_int32(1, 1),
    round_trip_uint32_int32(Numbers.max_signed32, Numbers.max_signed32),
    round_trip_uint32_int32(0x80000000, -2147483648),
    round_trip_uint32_int32(0xffffffff, -1),
    round_trip_uint32_int32(0xfffffffe, -2).

test(uint32_int32_e1,
     [ condition(gmp),
       error(domain_error('32_bit_integer',4294967296),context(protobufs:uint32_int32/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint32_int32(Numbers.max_unsigned32_plus1, _).

test(uint32_int32_e2,
     [ condition(gmp),
       error(representation_error(int),context(protobufs:uint32_int32/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint32_int32(_, Numbers.max_signed32_plus1).

test(uint32_int32_e3,
     [ condition(gmp),
       error(representation_error(int),context(protobufs:uint32_int32/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint32_int32(_, Numbers.min_signed32_minus1).

test(int64_float64, condition(gmp)) :-
    % Not all bit patterns are normalized floating point, so we can't test, e.g. 0xffffffffffffffff;
    % instead, we use the min and max floating point values, plus a few others.
    prolog_flag(float_max, PosFloatMax),
    prolog_flag(float_min, PosFloatMin),
    NegFloatMax is - PosFloatMax,
    NegFloatMin is - PosFloatMin,
    % PosFloatMax:  1.7976931348623157e+308: [255, 255, 255, 255, 255, 255, 239, 127]
    % NegFloatMax: -1.7976931348623157e+308: [255, 255, 255, 255, 255, 255, 239, 255]
    % PosFloatMin:  2.2250738585072014e-308: [0, 0, 0, 0, 0, 0, 16, 0]
    % NegFloatMin: -2.2250738585072014e-308: [0, 0, 0, 0, 0, 0, 16, 128]
    round_trip_int64_float64(0.0),
    round_trip_int64_float64(1.0),
    round_trip_int64_float64(-1.0),
    round_trip_int64_float64(1234567890.0),
    round_trip_int64_float64(-987654321.5),
    round_trip_int64_float64(PosFloatMax),
    round_trip_int64_float64(PosFloatMin),
    round_trip_int64_float64(NegFloatMax),
    round_trip_int64_float64(NegFloatMin).

test(int32_float32) :- % , [blocked("equality tests fail due to float32 precision")]) :-
    % See comments with test(int64_float64).
    % https://softwareengineering.stackexchange.com/questions/294269/how-to-calculate-min-max-values-of-floating-point-numbers
    % PosFloatMax = 1.8e38,  % TODO: get the real value - ? protobufs:float32_codes(F, [255,255,255,126]).
    % PosFloatMin = 1.8e-38, % TODO: get the real value - ?                            [255,255,255,0]
    % NegFloatMax is - PosFloatMax,
    % NegFloatMin is - PosFloatMin,
    % TODO: the following require an "epsilon" test for the round-trip.
    %       round_trip_int32_float32(PosFloatMax),
    %       round_trip_int32_float32(PosFloatMin),
    %       round_trip_int32_float32(NegFloatMax),
    %       round_trip_int32_float32(NegFloatMin),
    round_trip_int32_float32(0.0),
    round_trip_int32_float32(1.0),
    round_trip_int32_float32(-1.0),
    round_trip_int32_float32(12345.0),
    round_trip_int32_float32(-98765.5),
    round_trip_int32_float32(1.0e10).

:- end_tests(codes).

:- begin_tests(zigzag).

round_trip_zigzag(Original, Encoded) :-
    protobufs:int64_zigzag(Original, ComputedEncoded),
    assertion(ComputedEncoded == Encoded),
    protobufs:int64_zigzag(ComputedOriginal, Encoded),
    assertion(ComputedOriginal == Original).

% Examples from https://developers.google.com/protocol-buffers/docs/encoding#types
test(zigzag, condition(gmp)) :-
    test_numbers(Numbers),
    round_trip_zigzag(0, 0),
    round_trip_zigzag(-1, 1),
    round_trip_zigzag(1, 2),
    round_trip_zigzag(2147483647, 4294967294),
    round_trip_zigzag(-2147483648, 4294967295),
    % 64-bit: 0x7fffffffffffffff
    round_trip_zigzag(Numbers.max_signed64, 18446744073709551614),
    round_trip_zigzag(Numbers.min_signed64, 18446744073709551615).

test(zigzag_e1, [error(instantiation_error,context(protobufs:int64_zigzag/2,_))]) :-
    protobufs:int64_zigzag(_, _).

test(zigzag_e2,
     [ condition(gmp),
       error(representation_error(int64_t),context(protobufs:int64_zigzag/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:int64_zigzag(Numbers.max_signed64_plus1, _).

test(zigzag_e3,
     [ condition(gmp),
       error(representation_error(uint64_t),context(protobufs:int64_zigzag/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:int64_zigzag(_, Numbers.max_unsigned64_plus1).

test(zigzag_e4, [error(type_error(integer,'123'),context(protobufs:int64_zigzag/2,_))]) :-
    protobufs:int64_zigzag('123', _).

test(zigzag_e4, [error(type_error(integer,'666'),context(protobufs:int64_zigzag/2,_))]) :-
    protobufs:int64_zigzag(_, '666').

:- end_tests(zigzag).

:- begin_tests(coerce).

round_trip_uint64_int64(Uint64, Int64) :-
    protobufs:uint64_int64(Uint64, ComputedInt64),
    assertion(ComputedInt64 == Int64),
    protobufs:uint64_int64(ComputedUint64, Int64),
    assertion(ComputedUint64 == Uint64).

test(coerce, condition(gmp)) :-
    test_numbers(Numbers),
    round_trip_uint64_int64(0, 0),
    round_trip_uint64_int64(Numbers.max_unsigned64, -1),
    round_trip_uint64_int64(Numbers.max_signed64, Numbers.max_signed64),
    round_trip_uint64_int64(12345, 12345),
    round_trip_uint64_int64(12345678, 12345678),
    round_trip_uint64_int64(18446744073621897295, -87654321). % 0xfffffffffac6804f

test(coerce_e1, [error(instantiation_error,context(protobufs:uint64_int64/2,_))]) :-
    protobufs:uint64_int64(_, _).

test(coerce_e2,
     [ condition(gmp),
       error(domain_error(not_less_than_zero,-1),context(protobufs:uint64_int64/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint64_int64(Numbers.min_unsigned64_minus1, _).

test(coerce_e3,
     [ condition(gmp),
       error(representation_error(uint64_t),context(protobufs:uint64_int64/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint64_int64(Numbers.max_unsigned64_plus1, _).

test(coerce_e4,
     [ condition(gmp),
       error(representation_error(int64_t),context(protobufs:uint64_int64/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint64_int64(_, Numbers.max_signed64_plus1).

test(coerce_e5,
     [ condition(gmp),
       error(representation_error(int64_t),context(protobufs:uint64_int64/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint64_int64(_, Numbers.min_signed64_minus1).

test(coerce_e6,
     [ condition(gmp),
       error(representation_error(int64_t),context(protobufs:uint64_int64/2,_))
     ]) :-
    test_numbers(Numbers),
    protobufs:uint64_int64(_, Numbers.max_signed64_plus1).

test(coerce_e7, [error(type_error(integer,'666'),context(protobufs:uint64_int64/2,_))]) :-
    protobufs:uint64_int64('666', _).

test(coerce_e8, [error(type_error(integer,'999'),context(protobufs:uint64_int64/2,_))]) :-
    protobufs:uint64_int64(_, '999').

:- end_tests(coerce).

end_of_file.
