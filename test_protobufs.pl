/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, Jeffrey Rosenwald
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

:- asserta(user:file_search_path(library, .)).
:- asserta(user:file_search_path(foreign, .)).

:- use_module(library(protobufs)).

:- use_module(eventually_implies).   % ~> operator

protobufs:nested_enum(Key, Value) :-
    nested_enum(Key, Value).

nested_enum(foo,1).
nested_enum(bar,2).
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

golden_message(Proto) :-
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

    Proto = protobuf([ unsigned(1 , 101),
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
                       embedded(18, protobuf([unsigned(1, 118)])), % nested_message
                       embedded(19, protobuf([unsigned(1, 119)])), % foreign_message
                       embedded(20, protobuf([unsigned(1, 120)])),  % import message
                       enum(21, nested_enum(baz)),    % nested_enum  BAZ
                       enum(22, foreign_enum(baz)),     % nested_enum  FOREIGN_BAZ
                       enum(23, import_enum(baz)),     %  nested_enum IMPORT_BAZ
                       string(24, String124),   % string_piece
                       string(25, String125),   % cord
                       codes(26, [8, 126]),        % public_import_message
                       codes(27, [8, 127]),        % lazy message
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
                                              protobuf([unsigned(1, 320)])])),  % import
                       repeated(51, enum(nested_enum([bar, baz]))),
                       repeated(52, enum(foreign_enum([bar, baz]))),
                       repeated(53, enum(import_enum([bar, baz]))),
                       repeated(54, string([String224, String324])),   % string_piece
                       repeated(55, codes([Codes225, Codes325])),    % cord
                       repeated(57, embedded([protobuf([unsigned(1,227)]), % lazy msg
                                              protobuf([unsigned(1,327)])])),
                       unsigned(61, 401),  % default_int32
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
    Proto = protobuf([ unsigned(_ , _),
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
                       embedded(_, protobuf([unsigned(_, _)])), % nested_message
                       embedded(_, protobuf([unsigned(_, _)])), % foreign_message
                       embedded(_, protobuf([unsigned(_, _)])),  % import message
                       enum(_, nested_enum(_)),    % nested_enum  BAZ
                       enum(_, foreign_enum(_)),     % nested_enum  FOREIGN_BAZ
                       enum(_, import_enum(_)),     %  nested_enum IMPORT_BAZ
                       string(_, _),   % string_piece
                       string(_, _),   % cord
                       codes(_, _),        % public_import_message
                       codes(_, _), %lazy message
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
                                             protobuf([unsigned(_, _)])])),  % import
                       repeated(_, enum(nested_enum(_))),
                       repeated(_, enum(foreign_enum(_))),
                       repeated(_, enum(import_enum(_))),
                       repeated(_, string(_)),   % string_piece
                       repeated(_, codes(_)),    % cord
                       repeated(_, embedded([protobuf([unsigned(_,_)]),
                                             protobuf([unsigned(_,_)])])),
                       unsigned(_, _),  % default_int_
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

announce(Announcement, Test) :-
    write(Announcement)
      ~> (Test == ok -> writeln('OK'); writeln('FAILED')).

test_protobufs :-
    golden_message(Message),
    golden_message_template(Template),
    copy_term(Template, Template1),
    copy_term(Template, Template2),

    announce('Loading Google''s Golden Wirestream (2.5.0)...', Test1a),
    (   read_file_to_codes('./golden_message.2.5.0', Wirestream, [type(binary)])
    ->  Test1a = ok
    ),
    !,
    announce('Unifying canned Golden Message with canned Golden Template...',
             Test1),
    (   (Message = Template, Message == Template)
    ->  Test1 = ok
    ),
    !,

    announce('Unifying canned Golden Message with Google''s Golden Wirestream...',
             Test2),
    (   protobuf_message(Message, Wirestream)
    ->  Test2 = ok
    ),
    !,

    announce('Parsing Google''s Golden Wirestream to canned Golden Template...',
             Test3),
    (   protobuf_message(Template2, Wirestream)
    ->  Test3 = ok
    ),
    !,

    announce('Comparing canned Golden Message to parsed Golden Template...',
             Test3a),
    (   Message == Template2
    ->  Test3a = ok
    ),
    !,

    announce('Serializing canned Golden Message to Codes...', Test4),
    (   protobuf_message(Message, Codes )
    ->  Test4 = ok
    ),
    !,

    announce('Comparing Google''s Golden Wirestream to Codes...', Test4a),
    (   (Wirestream == Codes)
    ->  Test4a = ok
    ),
    !,

    announce('Parsing Codes to canned Golden Template...', Test5),
    (   protobuf_message(Template1, Codes)
    ->  Test5 = ok
    ),
    !,

    announce('Comparing canned Golden Message to parsed Golden Template...',
             Test6),
    (   (Message == Template1)
    ->  Test6 = ok
    ),
    !,

    writeln('All tests passed.').
