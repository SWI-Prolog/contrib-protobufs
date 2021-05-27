% -*- mode: Prolog -*-

%% Parse the output of protoc --decode=FileDescriptorSet
%%
%% To generate the file plugin.proto.wiredump
%%    protoc --include_imports --descriptor_set_out=plugin.proto.wire \
%%           -I. -I$(SRC_PROTOBUF)/src -I$(SRC_PROTOBUF)/src/google/protobuf -I$(SRC_PROTOBUF)/src/google/protobuf/compiler \
%%           plugin.proto
%%    protoc -I. -I$(SRC_PROTOBUF)/src -I$(SRC_PROTOBUF)/src/google/protobuf -I$(SRC_PROTOBUF)/src/google/protobuf/compiler \
%%           --decode=google.protobuf.FileDescriptorSet \
%%           descriptor.proto \
%%           <plugin.proto.wire >plugin.proto.wiredump

%%
%% This code is used to bootstrap the protoc plugin.
%% TODO: document bootstrap.

% This code is reversible -- see test_parse_round_trip/0.

:- module(parse_descriptor_proto_dump,
          [parse_wiredump/1,
           parse_wiredump/2,
           test_parse_round_trip/1]).

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

parse_wiredump(File) :-
    parse_wiredump(File, Term),
    print_term_cleaned(Term, [indent_arguments(4)]),
    writeln('.').

parse_wiredump(File, Term) :-
    read_file_to_codes(File, Codes, [encoding(octet),type(binary)]),
    phrase(parse_FileDescriptorSet([], Term), Codes).

test_parse_round_trip(File) :-  % e.g., File = 'plugin.proto.wiredump'
    phrase(parse_FileDescriptorSet([], File), Term),
    phrase(parse_FileDescriptorSet([], Term), Codes),
    string_codes(String, Codes),
    write(String), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser:
% All the rules have an Indent parameter. For parsing, this is ignored.
% For output, it's a list of 32 (ascii blank) that's put at the beginning
% of each line, to produce nicely indented output.

parse_FileDescriptorSet(Indent, 'FileDescriptorSet'{file: File}) -->
    sequence(name_braces(Indent, "file", parse_FileDescriptorProto), File).

parse_FileDescriptorProto(Indent,
                          'FileDescriptorProto'{name:Name,
                                                package:Package,
                                                dependency:Dependency,
                                                message_type:MessageType,
                                                options:Options}) -->
    tag_colon_string(Indent, "name", Name),
    optional_tag_colon_string(Indent, "package", Package),
    optional_tag_colon_string(Indent, "dependency", Dependency),
    sequence(name_braces(Indent, "message_type", parse_DescriptorProto), MessageType),
    optional_name_braces(Indent, "options", parse_FileOptions,
                         'FileOptions', Options).

parse_DescriptorProto(Indent,
                      'DescriptorProto'{name:Name,
                                        field:Field,
                                        nested_type:NestedType,
                                        enum_type:EnumType,
                                        extension_range:ExtensionRange,
                                        reserved_range:ReservedRange}) -->
    tag_colon_string(Indent, "name", Name),
    sequence(name_braces(Indent, "field", parse_FieldDescriptorProto), Field),
    sequence(name_braces(Indent, "nested_type", parse_DescriptorProto), NestedType),
    sequence(name_braces(Indent, "enum_type", parse_EnumDescriptorProto), EnumType),
    sequence(name_braces(Indent, "extension_range", parse_ExtensionRange), ExtensionRange),
    sequence(name_braces(Indent, "reserved_range", parse_EnumReservedRange), ReservedRange).

parse_FieldDescriptorProto(Indent,
                           'FieldDescriptorProto'{name:Name,
                                                  number:Number,
                                                  label:Label,
                                                  type:Type,
                                                  type_name:TypeName,
                                                  default_value:DefaultValue,
                                                  options:Options,
                                                  json_name:JsonName}) -->
     optional_tag_colon_string(Indent, "name", Name),
     optional_tag_colon_number(Indent, "number", Number),
     optional_tag_colon_id(Indent, "label", Label),
     optional_tag_colon_id(Indent, "type", Type),
     optional_tag_colon_string(Indent, "type_name", TypeName),
     optional_tag_colon_string(Indent, "default_value", DefaultValue),
     optional_name_braces(Indent, "options", parse_FieldOptions,
                          'FieldOptions', Options),
     optional_tag_colon_string(Indent, "json_name", JsonName).

parse_EnumDescriptorProto(Indent, 'EnumDescriptorProto'{name:Name,
                                                        value:Value}) -->
    tag_colon_string(Indent, "name", Name),
    sequence(name_braces(Indent, "value", parse_EnumValueDescriptorProto), Value).

parse_EnumValueDescriptorProto(Indent, 'EnumValueDescriptorProto'{name:Name,
                                                                  number:Number}) -->
    tag_colon_string(Indent, "name", Name),
    tag_colon_number(Indent, "number", Number).

parse_ExtensionRange(Indent, 'ExtensionRange'{start:Start,
                                              end:End}) -->
    tag_colon_number(Indent, "start", Start),
    tag_colon_number(Indent, "end", End).

parse_EnumReservedRange(Indent, 'EnumReservedRange'{start:Start,
                                                     end:End}) -->
    tag_colon_number(Indent, "start", Start),
    tag_colon_number(Indent, "end", End).

parse_FieldOptions(Indent, 'FieldOptions'{deprecated:Deprecated,
                                          packed:Packed}) -->
    optional_tag_colon_id(Indent, "deprecated", Deprecated),
    optional_tag_colon_id(Indent, "packed", Packed).

parse_FileOptions(Indent, 'FileOptions'{java_package: V_java_package,
                                         java_outer_classname: V_java_outer_classname,
                                         optimize_for: V_optimize_for,
                                         go_package: V_go_package,
                                         cc_enable_arenas: V_cc_enable_arenas,
                                         objc_class_prefix: V_objc_class_prefix,
                                         csharp_namespace: V_csharp_namespace}) -->
    optional_tag_colon_string(Indent, "java_package", V_java_package),
    optional_tag_colon_string(Indent, "java_outer_classname", V_java_outer_classname),
    optional_tag_colon_id(Indent, "optimize_for", V_optimize_for),
    optional_tag_colon_string(Indent, "go_package", V_go_package),
    optional_tag_colon_id(Indent, "cc_enable_arenas", V_cc_enable_arenas),
    optional_tag_colon_string(Indent, "objc_class_prefix", V_objc_class_prefix),
    optional_tag_colon_string(Indent, "csharp_namespace", V_csharp_namespace), !.

name_braces(Indent, Name, ParseType, Value) -->
    indent(Indent, Indent1),
    bidi(nonblanks, atom_codes, Name),
    white_1, whites, "{", blanks_to_nl, !,
    { debug(dcg_trace, '~q ~q{', [Name, ParseType]) },
    call(ParseType, Indent1, Value),
    whites(Indent), "}", blanks_to_nl, !,
    { debug(dcg_trace, '~q }~q', [Name, ParseType]) }.

optional_name_braces(Indent, Name, ParseType, Type, Value) -->
    optional(name_braces(Indent, Name, ParseType, Value), {Value=Type{}}).

tag_colon_string(Indent, Tag, String) -->
    whites(Indent),
    bidi(nonblanks, atom_codes, Tag),
    colon_string(Indent, String),
    { debug(dcg_trace, '  ~q : ~q', [Tag, String]) }.

tag_colon_number(Indent, Tag, Number) -->
    whites(Indent),
    bidi(nonblanks, atom_codes, Tag),
    colon_number(Indent, Number),
    { debug(dcg_trace, '  ~q : ~q', [Tag, Number]) }.

tag_colon_id(Indent, Tag, IdAsAtom) -->
    whites(Indent),
    bidi(nonblanks, atom_codes, Tag),
    colon_id(Indent, IdAsAtom),
    { debug(dcg_trace, '  ~q : ~q', [Tag, IdAsAtom]) }.

optional_tag_colon_string(Indent, Tag, String) -->
    optional(tag_colon_string(Indent, Tag, String), {String=''}).

optional_tag_colon_number(Indent, Tag, Number) -->
    optional(tag_colon_number(Indent, Tag, Number), {Number=0}).

optional_tag_colon_id(Indent, Tag, IdAsAtom) -->
    optional(tag_colon_id(Indent, Tag, IdAsAtom), {IdAsAtom=''}).

colon_string(_Indent, String) -->
    whites, ":", white_1, whites,
    "\"",
    bidi(string_without("\""), atom_codes, String),
    "\"",
    blanks_to_nl, !.

colon_number(_Indent, Number) -->
    whites, ":", white_1, whites,
    bidi(digits, number_codes, Number),
    blanks_to_nl, !.

colon_id(_Indent, IdAsAtom) -->
    whites, ":", white_1, whites,
    bidi(nonblanks, atom_codes, IdAsAtom),
    blanks_to_nl, !.

%! indent(Indent0, Indent)// is det.
%! For output, indent by the codes in Indent0; new indent in Indent
indent(Indent, [0' ,0' |Indent], S0, S) :-
    (   var(S0)
    ->  whites(Indent, S0, S)
    ;   whites(S0, S)
    ).

%! white_1// is det.
% For output, add one whitespace.
white_1(S0, S) :-
    (   var(S)
    ->  whites([0' ], S0, S)  % 0' is character code for space (blank).
    ;   S0 = S
    ).

whites([]) --> [].
whites([W|Ws]) -->
    [W],
    whites(Ws).

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

:- det(print_term_cleaned/2).
print_term_cleaned(Term, Options) =>
    print_term_cleaned(Term, Options, TermStr),
    write(TermStr).

:- det(print_term_cleaned/3).
%! print_term_cleaned(+Term, +Options, -TermStr) is det.
% print_term, cleaned up
print_term_cleaned(Term, Options, TermStr) =>
    % print_term leaves trailing whitespace, so remove it
    with_output_to(
            string(TermStr0),
            (current_output(TermStream),
             print_term(Term, [output(TermStream)|Options]))),
    re_replace(" +\n"/g, "\n", TermStr0, TermStr1),
    re_replace("\t"/g, "        ", TermStr1, TermStr).

end_of_file.
