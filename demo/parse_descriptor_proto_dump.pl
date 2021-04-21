% -*- mode: Prolog -*-

%% Parse the output of protoc --decode=FileDescriptorSet
%% It does not handle the general output of protoc --decode ... for
%% that, use protoc --descriptor_set_out [--include_imports] and
%% process that protobuf.

% This code is reversible -- that is, you can do this, where
% `Term` is ground but `Codes` isn't:
%    `phrase(file(Term), Codes), string_codes(String, Codes)`
% However, it doesn't do nice indentation.
% TODO: pass around an indentation level (optional) for pretty-printing.

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% Use one of these to process the file:

parse_descriptor :-
    parse_descriptor(Term),
    print_term(Term, []), nl.

parse_descriptor(Term) :-
    parse_file('descriptor.proto.dump', Term).

parse_file(File, Term) :-
    read_file_to_codes(File, Codes, [type(binary)]),
    phrase(file(Term), Codes).


% The parser:

file(file{name:Name,
          package:Package,
          message_type:MessageType,
          options:Options}) -->
    left_brace("file"), !,
    tag_colon_string("name", Name),
    optional_tag_colon_string("package", Package),
    sequence(message_type, MessageType),
    options_file(Options),
    right_brace, !.

message_type(message_type{name:Name, field:Field, nested_type:NestedType,
                          enum_type:EnumType, extension_range:ExtensionRange,
                          reserved_range:ReservedRange}) -->
    left_brace("message_type"), !,
    tag_colon_string("name", Name),
    sequence(field, Field),
    sequence(nested_type, NestedType),
    sequence(enum_type, EnumType),
    sequence(extension_range, ExtensionRange),
    sequence(reserved_range, ReservedRange),
    right_brace, !.

field(field{name:Name, number:Number, label:Label, type:Type, type_name:TypeName,
            default_value:DefaultValue, options:Options, json_name:JsonName}) -->
    left_brace("field"), !,
    optional_tag_colon_string("name", Name),
    optional_tag_colon_number("number", Number),
    optional_tag_colon_id("label", Label),
    optional_tag_colon_id("type", Type),
    optional_tag_colon_string("type_name", TypeName),
    optional_tag_colon_string("default_value", DefaultValue),
    optional(options(Options), {Options=[]}),
    optional_tag_colon_string("json_name", JsonName),
    right_brace, !.

nested_type(nested_type{name:Name, field:Field}) -->
    left_brace("nested_type"), !,
    optional_tag_colon_string("name", Name),
    sequence(field, Field),
    right_brace, !.

enum_type(enum_type{name:Name, value:Value}) -->
    left_brace("enum_type"), !,
    tag_colon_string("name", Name),
    sequence(value, Value),
    right_brace, !.

value(value{name:Name, number:Number}) -->
    left_brace("value"), !,
    tag_colon_string("name", Name),
    tag_colon_number("number", Number),
    right_brace, !.

extension_range(extension_range{start:Start, end:End}) -->
    left_brace("extension_range"), !,
    tag_colon_number("start", Start),
    tag_colon_number("end", End),
    right_brace, !.

reserved_range(reserved_range{start:Start, end:End}) -->
    left_brace("reserved_range"), !,
    tag_colon_number("start", Start),
    tag_colon_number("end", End),
    right_brace, !.

options(options{deprecated:Deprecated, packed:Packed}) -->
    left_brace("options"), !,
    optional_tag_colon_id("deprecated", Deprecated),
    optional_tag_colon_id("packed", Packed),
    right_brace, !.

options_file(options{java_package: V_java_package,
                     java_outer_classname: V_java_outer_classname,
                     optimize_for: V_optimize_for,
                     go_package: V_go_package,
                     cc_enable_arenas: V_cc_enable_arenas,
                     objc_class_prefix: V_objc_class_prefix,
                     csharp_namespace: V_csharp_namespace}) -->
    left_brace("options"), !,
    optional_tag_colon_string("java_package", V_java_package),
    optional_tag_colon_string("java_outer_classname", V_java_outer_classname),
    optional_tag_colon_id("optimize_for", V_optimize_for),
    optional_tag_colon_string("go_package", V_go_package),
    optional_tag_colon_id("cc_enable_arenas", V_cc_enable_arenas),
    optional_tag_colon_string("objc_class_prefix", V_objc_class_prefix),
    optional_tag_colon_string("csharp_namespace", V_csharp_namespace),
    right_brace, !.


left_brace(Type) -->
    whites,
    bidi_nonblanks(Type),
    whites, "{", blanks_to_nl, !,
    { trace_format('~q {~n', [Type]) }.

right_brace -->
    whites, "}", blanks_to_nl, !.

tag_colon_string(Tag, StringAsAtom) -->
    whites,
    bidi_nonblanks(Tag),
    colon_string(StringAsAtom),
    { trace_format('  ~q : ~q~n', [Tag, StringAsAtom]) }.

tag_colon_number(Tag, Number) -->
    whites,
    bidi_nonblanks(Tag),
    colon_number(Number),
    { trace_format('  ~q : ~q~n', [Tag, Number]) }.

tag_colon_id(Tag, IdAsAtom) -->
    whites,
    bidi_nonblanks(Tag),
    colon_id(IdAsAtom),
    { trace_format('  ~q : ~q~n', [Tag, IdAsAtom]) }.

optional_tag_colon_string(Tag, StringAsAtom) -->
    optional(tag_colon_string(Tag, StringAsAtom), {StringAsAtom=''}).

optional_tag_colon_number(Tag, Number) -->
    optional(tag_colon_number(Tag, Number), {Number=0}).

optional_tag_colon_id(Tag, IdAsAtom) -->
    optional(tag_colon_id(Tag, IdAsAtom), {IdAsAtom=''}).

bidi_nonblanks(Atom) -->
    { when( (ground(Atom) ; ground(AtomCodes) ),
            atom_codes(Atom, AtomCodes) ) },
    nonblanks(AtomCodes).

colon_string(StringAsAtom) -->
    { when(( ground(StringAsAtom) ; ground(StringCodes) ),
           atom_codes(StringAsAtom, StringCodes) ) },
    whites, ":", whites,
    "\"",
    string_without("\"", StringCodes),
    "\"",
    blanks_to_nl, !.

colon_number(Number) -->
    { when( (ground(Number) ; ground(Digits) ),
            number_codes(Number, Digits) ) },
    whites, ":", whites,
    digits(Digits),
    blanks_to_nl, !.

colon_id(IdAsAtom) -->
    { when( (ground(IdAsAtom) ; ground(IdCodes) ),
            atom_codes(IdAsAtom, IdCodes) ) },
    whites, ":", whites,
    nonblanks(IdCodes),
    blanks_to_nl, !.

% For figuring out where a parse fails:
% trace_format(Format, Args) :- format(Format, Args).
trace_format(_Format, _Args).

end_of_file.
