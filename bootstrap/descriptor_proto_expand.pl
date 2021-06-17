% -*- mode: Prolog -*-

% TODO: delete this file when all the logic has been moved to protoc-gen-swipl
%       and everything has been completely bootstrapped.

%% Term expansion for descriptor_proto.pl
%% (which is used by descriptor_proto.pl to expand a
%% descriptor_proto/1 fact -- see the documentation of
%% descriptor_proto.pl for how this term is created.)

:- module(descriptor_proto_expand, [descriptor_proto_expand_FileDescriptorSet/2,
                                    descriptor_proto_expand_FileDescriptorSet//1,
                                    descriptor_proto_expand_FileDescriptorSet_preds/1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(debug), [assertion/1]).

:- det(descriptor_proto_expand_FileDescriptorSet_preds/1).
descriptor_proto_expand_FileDescriptorSet_preds(Preds) :-
    Preds = [
     protobufs:proto_meta_normalize/2,               %   protobufs:proto_meta_package(Unnormalized, Normalized)
     protobufs:proto_meta_package/3,                 %   protobufs:proto_meta_package(Package, FileName, Options)
     protobufs:proto_meta_message_type/3,            %   protobufs:proto_meta_message_type(       Fqn,     Package, Name)
     protobufs:proto_meta_field_name/4,              %   protobufs:proto_meta_field_name(         Fqn,     FieldNumber, FieldName, FqnName),
     protobufs:proto_meta_field_json_name/2,         %   protobufs:proto_meta_field_json_name(    FqnName, JsonName)
     protobufs:proto_meta_field_label/2,             %   protobufs:proto_meta_field_label(        FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     protobufs:proto_meta_field_type/2,              %   protobufs:proto_meta_field_type(         FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     protobufs:proto_meta_field_type_name/2,         %   protobufs:proto_meta_field_type_name(    FqnName, TypeName)
     protobufs:proto_meta_field_default_value/2,     %   protobufs:proto_meta_field_default_value(FqnName, DefaultValue)
     protobufs:proto_meta_field_option_packed/1,     %   protobufs:proto_meta_field_option_packed(FqnName)
     protobufs:proto_meta_enum_type/3,               %   protobufs:proto_meta_enum_type(          FqnName, Fqn, Name)
     protobufs:proto_meta_enum_value/3               %   protobufs:proto_meta_enum_value(         FqnName, Name, Number)
            ].

:- det(descriptor_proto_expand_FileDescriptorSet/2).
descriptor_proto_expand_FileDescriptorSet(Set,
                                          [(:- discontiguous CommaPreds) | Expansion]) :-
    phrase(descriptor_proto_expand_FileDescriptorSet(Set), Expansion),
    (   false  % for debugging
    ->  writeln('***expansion'),
        print_term(Expansion, [right_margin(160)]),
        nl, writeln('***end expansion'), nl
    ;   true
    ),
    descriptor_proto_expand_FileDescriptorSet_preds(Preds),
    list_commalist(Preds, CommaPreds).

:- det(list_commalist/2).
list_commalist([Pred], Pred) :- !.
list_commalist([Pred|Preds], (Pred,CommaList)) :-
    list_commalist(Preds, CommaList).

:- det(lookup_pieces/3).
%! lookup_pieces(+Tag, +DataDict, ?LookupDict) is det.
% Given a =DataDict=, look up the items in =LookupDict= If =DataDict=
% contains any keys that aren't in =LookupDict=, this predicate
% fails. This is to catch typos. For example: =|lookup_pieces(d,
% d{a:1,b:2}, _{a:0-A,bb:0-B,c:[]-C})|= will fail but
% =|lookup_pieces(d, d{a:1,b:2}, _{a:0-A,b:0-B,c:[]-C})|= will succeed
% with =|A=1,B=2,C=[]|=. In other words, =LookupDict= must contain all
% the possible keys in =DataDict= (with suitable defaults, of course).
% @param Tag the tag for =DataDict=
% @param DataDict items in =LookupDict= are looked up in here.
%        Its tag must unify with =Tag= (i.e., =|is_dict(DataDict,Tag)|=).
% @param LookupDict a dict where each entry is of the form =Default-Value=.
%        Each key is looked up in =DataDict= - if it's there, the value
%        from =DataDict= is unified with =Value=; if it's not there,
%        =Value= is unified with =Default=.
lookup_pieces(Tag, DataDict, LookupDict) :-
    is_dict(DataDict, Tag),
    dict_pairs(LookupDict, _, LookupPairs),
    lookup_piece_pairs(LookupPairs, DataDict).

lookup_piece_pairs([], RemainderDict) =>
    RemainderDict = _{}. % For debugging: assertion(RemainderDict = _{})
lookup_piece_pairs([Key-(Default-Value)|KDVs], DataDict0) =>
    dict_create(D0, _, [Key-Value]),
    (   select_dict(D0, DataDict0, DataDict)
    ->  true
    ;   Value = Default,
        DataDict = DataDict0
    ),
    lookup_piece_pairs(KDVs, DataDict).

:- det(descriptor_proto_expand_FileDescriptorSet//1).
descriptor_proto_expand_FileDescriptorSet(Set) -->
    { lookup_pieces('FileDescriptorSet', Set,
                    _{
                      file: []-File
                     }) },
    sequence(descriptor_proto_expand_FileDescriptorProto, File).

:- det(descriptor_proto_expand_FileDescriptorProto//1).
descriptor_proto_expand_FileDescriptorProto(File) -->
    { lookup_pieces('FileDescriptorProto', File,
                    _{
                      name:              ''              -File_name,
                      package:           ''              -File_package,
                      dependency:        []              -_,
                      public_dependency: []              -_,
                      weak_dependency:   []              -_,
                      message_type:      []              -File_message_type,
                      enum_type:         []              -File_enum_type,
                      service:           []               -_,
                      extension:         []              -File_extension,
                      options:           'FileOptions'{} -File_options,
                      source_code_info:  _               -_,
                      syntax:            ''              -_
                     }) },
    % TODO: The following is a quick hack - see protoc-gen-swipl for the correct
    %       way to define proto_meta_normalize/2.
    %       No need to fix this if we do a full bootstrap.
    [ (protobufs:proto_meta_normalize(X, X) :- !) ],
    { assertion(File_extension == []) }, % TODO: handle this?
    { add_to_fqn('', File_package, Package) },
    [ protobufs:proto_meta_package(Package, File_name, File_options) ],
    sequence(expand_DescriptorProto(Package), File_message_type),
    sequence(expand_EnumDescriptorProto(Package), File_enum_type).

:- det(expand_DescriptorProto//2).
expand_DescriptorProto(Fqn, MessageType) -->
    { lookup_pieces('DescriptorProto', MessageType,
                    _{
                      name:            ''      -MessageType_name,
                      field:           []      -MessageType_field,
                      extension:       []      -_,
                      nested_type:     []      -MessageType_nested_type,
                      enum_type:       []      -MessageType_enum_type,
                      extension_range: []      -_,
                      oneof_decl:      []      -_,
                      options:         []      -_,
                      reserved_range:  []      -_,
                      reserved_name:   []      -_
                     }) },
    { add_to_fqn(Fqn, MessageType_name, FqnName) },
    [ protobufs:proto_meta_message_type(FqnName, Fqn, MessageType_name) ],
    sequence(expand_FieldDescriptorProto(FqnName), MessageType_field),
    sequence(expand_DescriptorProto(FqnName), MessageType_nested_type),
    sequence(expand_EnumDescriptorProto(FqnName), MessageType_enum_type).

:- det(expand_FieldDescriptorProto//2).
expand_FieldDescriptorProto(Fqn, Field) -->
    { lookup_pieces('FieldDescriptorProto', Field,
                    _{
                      name:            ''               -Field_name,
                      number:          0                -Field_number,
                      label:           0                -Field_label,  % enum Label
                      type:            0                -Field_type,  % enum Type
                      type_name:       ''               -Field_type_name,
                      extendee:        _                -_,
                      default_value:   ''               -Field_default_value,
                      oneof_index:     _                -_,
                      json_name:       ''               -Field_json_name,
                      options:         'FieldOptions'{} -Field_options,
                      proto3_optional: _                -_
                     }) },
    { add_to_fqn(Fqn, Field_name, FqnName) },
    [ protobufs:proto_meta_field_name(Fqn, Field_number, Field_name, FqnName) ],
    [ protobufs:proto_meta_field_json_name(FqnName, Field_json_name) ],
    [ protobufs:proto_meta_field_label(FqnName, Field_label) ],
    [ protobufs:proto_meta_field_type(FqnName, Field_type) ],
    [ protobufs:proto_meta_field_type_name(FqnName, Field_type_name) ],
    [ protobufs:proto_meta_field_default_value(FqnName, Field_default_value) ],
    expand_FieldOptions(FqnName, Field_options).

:- det(expand_FieldOptions//2).
expand_FieldOptions(FqnName, Options) -->
    { lookup_pieces('FieldOptions', Options,
                    _{
                      ctype:                _     -_,
                      packed:               false -Option_packed,
                      jstype:               _     -_,
                      lazy:                 false -_,
                      deprecated:           false -_, % TODO: output warning if a deprecated field is used
                      weak:                 false -_,
                      uninterpreted_option: _     -_
                     }) },
    (   { Option_packed = true }
    ->  [ protobufs:proto_meta_field_option_packed(FqnName) ]
    ;   [ ]
    ).

:- det(expand_EnumDescriptorProto//2).
expand_EnumDescriptorProto(Fqn, EnumType) -->
    { lookup_pieces('EnumDescriptorProto', EnumType,
                    _{
                      name:           '' -EnumType_name,
                      value:          [] -EnumType_value,
                      options:        _  -_,
                      reserved_range: _  -_,
                      reserved_name:  _  -_
                      }) },
    { add_to_fqn(Fqn, EnumType_name, FqnName) },
    [ protobufs:proto_meta_enum_type(FqnName, Fqn, EnumType_name) ],
    sequence(expand_EnumValueDescriptorProto(FqnName), EnumType_value).

:- det(expand_EnumValueDescriptorProto//2).
expand_EnumValueDescriptorProto(Fqn, Value) -->
    { lookup_pieces('EnumValueDescriptorProto', Value,
                    _{
                      name:    ''-Value_name,
                      number:  0-Value_number,
                      options: _-_
                      }) },
    [ protobufs:proto_meta_enum_value(Fqn, Value_name, Value_number) ].

add_to_fqn(Fqn, Name, FqnName) :-
    atomic_list_concat([Fqn, Name], '.', FqnName).

end_of_file.
