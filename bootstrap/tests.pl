% Miscellaneous tests.

% TODO: use library(plunit)

:- module(bootstrap_tests,
          [
           test_segment_messages/1
          ]).

:- use_module(library(protobufs)).

%! test_segment_messages is det.
% Tests round-trip of segment_protobuf_segment_message/2,
% using the protobuf wire form of descriptor.proto.
% You may wish to compare the contents of =Segments= with
% the output from protoc --decode_raw
test_segment_messages(Path) :- % Path = 'descriptor.proto.wire'
    read_file_to_codes(Path, WireStream, [encoding(octet),type(binary)]),
    protobufs:protobuf_segment_message(Segments, WireStream),
    % Check that it reverses:
    protobufs:protobuf_segment_message(Segments, WireStream2),
    assertion(WireStream == WireStream2),
    Segments = [message(1, MessageSegments)],
    length(MessageSegments, MessageSegmentsLen),
    format('test_segment_messages succeeded with ~d segment(s) in the message.~n', [MessageSegmentsLen]),
    % Don't print it out in all its glory because it's rather long.
    true. % print_term(Segments, [indent_arguments(4), tab_width(0), right_margin(88)]), nl.
