% -*- mode: Prolog -*-

%% Term expansion for descriptor_proto.pl
%% (which is used by descriptor_proto.pl to expand a
%% descriptor_proto/1 fact -- see the documentation of
%% descriptor_proto.pl for how this term is created.)

:- module(descriptor_proto_expand, [descriptor_proto_expand_file/2,
                                    descriptor_proto_expand_file//1,
                                    descriptor_proto_expand_file_preds/1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(debug), [assertion/1]).

:- det(descriptor_proto_expand_file_preds/1).
descriptor_proto_expand_file_preds(Preds) :-
    Preds = [
     proto_package/3,                 %   proto_package(Package, FileName, Options)
     proto_message_type/3,            %   proto_message_type(           Fqn, Package, Name)
     proto_field_name/4,              %   proto_field_name(             Fqn, FieldNumber, FieldName, FqnName),
     proto_field_json_name/2,         %   proto_field_json_name(        FqnName, JsonName)
     proto_field_label/2,             %   proto_field_label(            FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     proto_field_type/2,              %   proto_field_type(             FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     proto_field_type_name/2,         %   proto_field_type_name(        FqnName, TypeName)
     proto_field_default_value/2,     %   proto_field_default_value(    FqnName, DefaultValue)
     proto_field_option_packed/1,     %   proto_field_option_packed(    FqnName)
     proto_nested_type/3,             %   proto_nested_type(            FqnName, Fqn, Name)
     proto_enum_type/3,               %   proto_enum_type(              FqnName, Fqn, Name)
     proto_enum_value/3               %   proto_enum_value(             FqnName, Name, Number)
            ].

:- det(descriptor_proto_expand_file/2).
descriptor_proto_expand_file(Proto,
                             [(:- discontiguous CommaPreds) | Expansion]) :-
    phrase(descriptor_proto_expand_file(Proto), Expansion),
    descriptor_proto_expand_file_preds(Preds),
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
    RemainderDict = _{}.
lookup_piece_pairs([Key-(Default-Value)|KDVs], DataDict0) =>
    dict_create(D0, _, [Key-Value]),
    (   select_dict(D0, DataDict0, DataDict)
    ->  true
    ;   Value = Default,
        DataDict = DataDict0
    ),
    lookup_piece_pairs(KDVs, DataDict).

:- det(descriptor_proto_expand_file//1).
descriptor_proto_expand_file(File) -->
    % message FileDescriptorProto
    { lookup_pieces(file, File,
                    _{
                      name:              ''        -File_name,
                      package:           ''        -File_package,
                      message_type:      []        -File_message_type,
                      enum_type:         []        -File_enum_type,
                      extension:         []        -File_extension,
                      options:           options{} -File_options,
                      dependency:        _         -_,
                      public_dependency: _         -_,
                      weak_dependency:   _         -_,
                      service:           _         -_,
                      source_code_info:  _         -_,
                      syntax:            _         -_
                     }) },
    { assertion(File_extension == []) }, % TODO: handle this?
    [ proto_package(File_package, File_name, File_options) ],
    sequence(expand_message_type(File_package), File_message_type),
    sequence(expand_enum_type(File_package), File_enum_type).

:- det(expand_message_type//2).
expand_message_type(Package, MessageType) -->
    % message DescriptorProto
    { lookup_pieces(message_type,MessageType,
                    _{
                      name:            '' - MessageType_name,
                      field:           [] - MessageType_field,
                      nested_type:     [] - MessageType_nested_type,
                      enum_type:       [] - MessageType_enum_type,
                      extension:       _  -_,
                      extension_range: _  -_,
                      oneof_decl:      _  -_,
                      options:         _  -_,
                      reserved_range:  _  -_,
                      reserved_name:   _  -_
                     }) },
    { atomic_list_concat(['',Package,  MessageType_name], '.', Fqn) },
    [ proto_message_type(Fqn, Package, MessageType_name) ],
    sequence(expand_field(Fqn), MessageType_field),
    sequence(expand_nested_type(Fqn), MessageType_nested_type),
    sequence(expand_enum_type(Fqn), MessageType_enum_type).

:- det(expand_field//2).
expand_field(Fqn, Field) -->
    % message FieldDescriptorProto
    { lookup_pieces(field, Field,
                    _{
                      name:            ''              -Field_name,
                      number:          0               -Field_number,
                      label:           0               -Field_label,  % enum Label
                      type:            0               -Field_type,  % enum Type
                      type_name:       ''              -Field_type_name,
                      default_value:   ''              -Field_default_value,
                      json_name:       ''              -Field_json_name,
                      options:         field_options{} -Field_options,
                      extendee:        _               -_,
                      oneof_index:     _               -_,
                      proto3_optional: _               -_
                     }) },
    { atomic_list_concat([Fqn, Field_name], '.', FqnName) },
    [ proto_field_name(Fqn, Field_number, Field_name, FqnName) ],
    [ proto_field_json_name(FqnName, Field_json_name) ],
    [ proto_field_label(FqnName, Field_label) ],
    expand_field_options(FqnName, Field_options),
    [ proto_field_type(FqnName, Field_type) ],
    [ proto_field_type_name(FqnName, Field_type_name) ],
    [ proto_field_default_value(FqnName, Field_default_value) ].

:- det(expand_field_options//2).
expand_field_options(FqnName, Options) -->
    % message FieldOptions
    { lookup_pieces(options, Options,
                    _{
                      ctype:                _     -_,
                      packed:               false -Option_packed,
                      jstype:               _     -_,
                      lazy:                 _     -_,
                      deprecated:           _     -_,
                      weak:                 _     -_,
                      uninterpreted_option: _     -_
                     }) },
    (   { Option_packed = true }
    ->  [ proto_field_option_packed(FqnName) ]
    ;   [ ]
    ).

:- det(expand_nested_type//2).
expand_nested_type(Fqn, NestedType) -->
    % message DescriptorProto
    { lookup_pieces(nested_type, NestedType,
                    _{
                      name:            ''      -NestedType_name,
                      field:           field{} -NestedType_field,
                      extension:       []      -_,
                      nested_type:     []      -NestedType_nested_type,
                      enum_type:       []      -NestedType_enum_type,
                      extension_range: []      -_,
                      oneof_decl:      []      -_,
                      options:         []      -_,
                      reserved_range:  []      -_
                     }) },
    { atomic_list_concat([Fqn, NestedType_name], '.', FqnName) },
    [ proto_nested_type(FqnName, Fqn, NestedType_name) ],
    sequence(expand_field(FqnName), NestedType_field),
    sequence(expand_nested_type(FqnName), NestedType_nested_type),
    sequence(expand_enum_type(FqnName), NestedType_enum_type).

:- det(expand_enum_type//2).
expand_enum_type(Fqn, EnumType) -->
    % message EnumDescriptorProto
    { lookup_pieces(enum_type, EnumType,
                    _{
                      name:           '' -EnumType_name,
                      value:          [] -EnumType_value,
                      options:        [] -_,
                      reserved_range: _  -_,
                      reserved_name:  _  -_
                      }) },

    { atomic_list_concat([Fqn, EnumType_name], '.', FqnName) },
    [ proto_enum_type(FqnName, Fqn, EnumType_name) ],
    sequence(expand_enum_value(FqnName), EnumType_value).

:- det(expand_enum_value//2).
expand_enum_value(Fqn, Value) -->
    % message EnumValueDescriptorProto
    { lookup_pieces(value, Value,
                    _{
                      name:    ''-Value_name,
                      number:  0-Value_number,
                      options: _-_
                      }) },
    { is_dict(Value, value) },
    [ proto_enum_value(Fqn, Value_name, Value_number) ].
