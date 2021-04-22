% -*- mode: Prolog -*-

%% Parse the output of protoc --decode=FileDescriptorSet
%% It does not handle the general output of protoc --decode ... for
%% that, use protoc --descriptor_set_out [--include_imports] and
%% process that protobuf.

% This code is reversible -- that is, you can do this, where
% `Term` is ground but `Codes` isn't, e.g., to round-trip descriptor.proto.dump:
%     parse_descriptor(_Term),
%     phrase(file(_Term), _Codes),
%     string_codes(_String, _Codes),
%     write(_String), nl.
% However, it doesn't do nice indentation.
% TODO: pass around an indentation level (optional) for pretty-printing.

:- module(parse_descriptor_proto_dump,
          [parse_descriptor/0,
           parse_descriptor/1,
           parse_file/2,
           file//1]).

:- use_module(library(dcg/basics),
              [whites//0,
               digits//1,
               nonblanks//1,
               string_without//2,
               blanks_to_nl//0]).
:- use_module(library(dcg/high_order),
              [sequence//2,
               optional//2]).
:- use_module(library(debug), [debug/3, debug/1]).

:- meta_predicate bidi(3, 2, ?, ?, ?).

% :- debug(dcg_trace).

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
    bidi(nonblanks, atom_codes, Type),
    whites, "{", blanks_to_nl, !,
    { debug(dcg_trace, '~q {~n', [Type]) }.

right_brace -->
    whites, "}", blanks_to_nl, !.

tag_colon_string(Tag, StringAsAtom) -->
    whites,
    bidi(nonblanks, atom_codes, Tag),
    colon_string(StringAsAtom),
    { debug(dcg_trace, '  ~q : ~q~n', [Tag, StringAsAtom]) }.

tag_colon_number(Tag, Number) -->
    whites,
    bidi(nonblanks, atom_codes, Tag),
    colon_number(Number),
    { debug(dcg_trace, '  ~q : ~q~n', [Tag, Number]) }.

tag_colon_id(Tag, IdAsAtom) -->
    whites,
    bidi(nonblanks, atom_codes, Tag),
    colon_id(IdAsAtom),
    { debug(dcg_trace, '  ~q : ~q~n', [Tag, IdAsAtom]) }.

optional_tag_colon_string(Tag, StringAsAtom) -->
    optional(tag_colon_string(Tag, StringAsAtom), {StringAsAtom=''}).

optional_tag_colon_number(Tag, Number) -->
    optional(tag_colon_number(Tag, Number), {Number=0}).

optional_tag_colon_id(Tag, IdAsAtom) -->
    optional(tag_colon_id(Tag, IdAsAtom), {IdAsAtom=''}).

colon_string(StringAsAtom) -->
    whites, ":", whites,
    "\"",
    bidi(string_without("\""), atom_codes, StringAsAtom),
    "\"",
    blanks_to_nl, !.

colon_number(Number) -->
    whites, ":", whites,
    bidi(digits, number_codes, Number),
    blanks_to_nl, !.

colon_id(IdAsAtom) -->
    whites, ":", whites,
    bidi(nonblanks, atom_codes, IdAsAtom),
    blanks_to_nl, !.

%! bidi(Element, Conversion, Converted)//2 is det.
% Does call(Element, Codes), { call(Conversion, Converted, Codes) }
% in appropriate order, depending on how the arguments are instantiated.
% ("bidi" for "bidrectional")
bidi(Element, Conversion, Converted) -->
    % Could also be done:
    %    { when( (ground(Converted) ; ground(Codes) ),
    %        call(Convert, Converted, Codes) ) },
    %    call(Element, Codes)
    (   { var(Converted) }
    ->  call(Element, Codes),
        { call(Conversion, Converted, Codes) }
    ;   { call(Conversion, Converted, Codes) },
        call(Element, Codes)
    ).

end_of_file.
