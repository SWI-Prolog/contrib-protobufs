% -*- mode: Prolog -*-

%% Term expansion for descriptor_proto.pl

:- module(descriptor_proto_expand, [x_file//1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- det(x_file//1).
x_file(file{name:FileName,
            package:Package,
            options:Options,
            message_type:MessageType}) -->
    [ proto_package(Package, FileName, Options) ],
    sequence(x_message_type(Package), MessageType).

:- det(x_message_type//2).
x_message_type(Package,
               message_type{name:Name, field:Field, nested_type:NestedType,
                            enum_type:EnumType, extension_range:ExtensionRange,
                            reserved_range:ReservedRange}) -->
    { atomic_list_concat(['',Package,Name], '.', Fqn) },
    [ proto_message_type(Fqn, Package, Name) ],
    sequence(x_field(Fqn), Field),
    sequence(x_nested_type(Fqn), NestedType),
    sequence(x_enum_type(Fqn), EnumType),
    sequence(x_extension_range(Fqn), ExtensionRange),
    sequence(x_reserved_range(Fqn), ReservedRange).

:- det(x_field//2).
x_field(Fqn, field{name:Name, number:Number, label:Label, type:Type, type_name:TypeName,
                   default_value:DefaultValue, options:Options, json_name:JsonName}) -->
    { atomic_list_concat([Fqn, Name], '.', FqnName) },
    [ proto_field_name(FqnName, Name) ],
    [ proto_field_number(FqnName, Number) ],
    [ proto_field_json_name(FqnName, JsonName) ],
    [ proto_field_label(FqnName, Label) ],
    x_field_options(FqnName, Options),
    [ proto_field_type(FqnName, Type) ],
    [ proto_field_type_name(FqnName, TypeName) ],
    [ proto_field_default_value(FqnName, DefaultValue) ].

:- det(x_field_options//2).
x_field_options(_, []) --> !, [].
x_field_options(FqnName, options{deprecated:Deprecated, packed:Packed}) -->
    ( { Deprecated = '' } -> [] ; [ proto_field_option_deprecated(FqnName) ] ),
    ( { Packed = 'true' } -> [] ; [ proto_field_option_packed(FqnName)]  ).

:- det(x_nested_type//2).
x_nested_type(Fqn, nested_type{name:Name, field:Field}) -->
    { atomic_list_concat([Fqn, Name], '.', FqnName) },
    [ proto_nested_type(FqnName, Fqn, Name) ],
    sequence(x_field(FqnName), Field).

:- det(x_enum_type//2).
x_enum_type(Fqn, enum_type{name:Name, value:Value}) -->
    {atomic_list_concat(['', Fqn, Name], '.', FqnName) },
    [ proto_enum_type(FqnName, Fqn, Name) ],
    sequence(x_enum_value(FqnName), Value).

:- det(x_enum_value//2).
x_enum_value(Fqn, value{name:Name, number:Number}) -->
    [ proto_enum_value(Fqn, Name, Number) ].

:- det(x_extension_range//2).
x_extension_range(Fqn, extension_range{start:Start, end:End}) -->
    [ proto_extension_range(Fqn, Start, End) ].

:- det(x_reserved_range//2).
x_reserved_range(Fqn, reserved_range{start:Start, end:End}) -->
    [ proto_reserved_range(Fqn, Start, End) ].
