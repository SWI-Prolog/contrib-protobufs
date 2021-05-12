% -*- mode: Prolog -*-

% Inputs a .proto.wire file (as output by protoc
% --descriptor_set_out=...) and outputs a Prolog term that represents
% the same data.

% The descriptor_proto/1 contents is derived from descriptor.proto (libprotoc 3.6.1):
%   protoc --include_imports --descriptor_set_out=descriptor.proto.wire \
%     -I$HOME/src/protobuf/src/google/protobuf \
%     descriptor.proto
%   protoc -I. -I$HOME/src/protobuf/src/google/protobuf \
%     --decode=google.protobuf.FileDescriptorSet \
%     descriptor.proto \
%     <descriptor.proto.wire
%     >descriptor.proto.wiredump
% And then run descriptor.proto.wiredump through parse_descriptor_proto_dump.pl
% parse_descriptor('descriptor.proto.wiredump').

:- module(descriptor_proto,
    [                                 % Term expansion of descriptor_proto/1 creates the following facts:
     proto_package/3,                 %   proto_package(Package, FileName, Options)
     proto_message_type/3,            %   proto_message_type(           Fqn, Package, Name)
     proto_field_name/4,              %   proto_field_name(             Fqn, FieldNumber, FieldName, FqnName),
     proto_field_json_name/2,         %   proto_field_json_name(        FqnName, JsonName)
     proto_field_label/2,             %   proto_field_label(            FqnName, LabelRepeatOptional) % LABEL_OPTIONAL, LABEL_REQUIRED, LABEL_REPEATED
     proto_field_type/2,              %   proto_field_type(             FqnName, Type) % TYPE_INT32, TYPE_MESSAGE, etc
     proto_field_type_name/2,         %   proto_field_type_name(        FqnName, TypeName)
     proto_field_default_value/2,     %   proto_field_default_value(    FqnName, DefaultValue)
     proto_field_option_deprecated/1, %   proto_field_option_deprecated(FqnName)
     proto_field_option_packed/1,     %   proto_field_option_packed(    FqnName)
     proto_nested_type/3,             %   proto_nested_type(            FqnName, Fqn, Name)
     proto_enum_type/3,               %   proto_enum_type(              FqnName, Fqn, Name)
     proto_enum_value/3,              %   proto_enum_value(             FqnName, Name, Number)
     proto_extension_range/3,         %   proto_extension_range(        FqnName, Start, End)
     proto_reserved_range/3,          %   proto_reserved_range(         FqnName, Start, End)
     main/0
    ]).

:- use_module(descriptor_proto_expand, [descriptor_proto_expand_file//1]).
:- use_module(library(readutil), [read_stream_to_codes/3]).
:- use_module(library(protobufs)).
:- use_module(library(debug)).

:- initialization(main, main).

main :-
    sanity_check,
    set_stream(user_input, encoding(octet)),
    set_stream(user_input, type(binary)),
    read_stream_to_codes(user_input, WireFormat),
    protobuf_segment_message(Segments, WireFormat),
    % protobuf_segment_message/2 can leave choicepoints, but we don't
    % want to backtrack through all the possibilities because that
    % leads to combinatoric explosion; instead use
    % protobuf_segment_convert/2 to change segments that were guessed
    % incorrectly.
    !, % don't use any other possible Segments - let protobuf_segment_convert/2 do the job
    maplist(segment_to_term('.google.protobuf.FileDescriptorSet'), Segments, Msg),
    print_term(Msg, [indent_arguments(4), output(user_output)]),
    writeln(user_output, '.').

:- det(sanity_check/0).
sanity_check :-
    forall(proto_field_name(Fqn, Num, FN, FqnN), proto_field_label(FqnN, _LabelRepeatOptional)),
    forall(proto_field_name(Fqn, Num, FN, FqnN), proto_field_type_name(FqnN, _Type)).

:- det(segment_to_term/3).
%! segment_to_term(+ContextType:atom, +Segment, -FieldAndValue) is det.
% ContextType is the type (name) of the containing message
% Segment is a segment from protobuf_segment_message/2
% TODO: if performance is an issue, this code can be combined with
%       protobuf_segment_message/2 (and thereby avoid the use of protobuf_segment_convert/2)
segment_to_term(ContextType0, Segment0, FieldAndValue) =>
    segment_type_tag(Segment0, _, Tag),
    field_and_type(ContextType0, Tag, FieldName, _FqnName, ContextType, RepeatOptional, Type),
    convert_segment(Type, ContextType, Segment0, Segment),
    FieldAndValue = field_and_value(FieldName,RepeatOptional,Segment).

% TODO: protobufs:segment_type_tag/3
segment_type_tag(varint(Tag,_Codes),           varint,           Tag).
segment_type_tag(fixed64(Tag,_Codes),          fixed64,          Tag).
segment_type_tag(start_group(Tag),             start_group,      Tag).
segment_type_tag(end_group(Tag),               end_group,        Tag).
segment_type_tag(fixed32(Tag,_Codes),          fixed32,          Tag).
segment_type_tag(length_delimited(Tag,_Codes), length_delimited, Tag).
segment_type_tag(message(Tag,_Segments),       length_delimited, Tag).
segment_type_tag(packed(Tag,_Compound),        length_delimited, Tag).
segment_type_tag(string(Tag,_String),          length_delimited, Tag).

:- det(convert_segment/4).
%! convert_segment(+Type:atom, +Segment, -Value) is det.
% Compute an appropriate =Value= from the combination of descriptor
% "type" (in =Type=) and a =Segment=.
convert_segment('TYPE_DOUBLE', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    float64_codes(Value, Codes).
convert_segment('TYPE_FLOAT', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    float32_codes(Value, Codes).
convert_segment('TYPE_INT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT64', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_INT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_FIXED64', _ContextType, Segment0, Value) =>
    Segment = fixed64(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int64_codes(Value, Codes).
convert_segment('TYPE_FIXED32', _ContextType, Segment0, Value) =>
    Segment = fixed32(_Tag,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int32_codes(Value, Codes).
convert_segment('TYPE_BOOL', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    int_bool(Value0, Value).
convert_segment('TYPE_STRING', _ContextType, Segment0, Value) =>
    Segment = string(_,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_GROUP', _ContextType, _Segment0, _Value) =>
    fail. % TODO - for now, this will throw an exception because of :- det(convert_segment/4).
convert_segment('TYPE_MESSAGE', ContextType, Segment0, Value) =>
    Segment = message(_,MsgSegments),
    protobuf_segment_convert(Segment0, Segment), !,
    maplist(segment_to_term(ContextType), MsgSegments, MsgFields),
    combine_fields(MsgFields, ContextType{}, Value).
convert_segment('TYPE_BYTES', _ContextType, Segment0, Value) =>
    Segment = length_delimited(_,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_UINT32', _ContextType, Segment0, Value) =>
    Segment = varint(_Tag,Value),
    protobuf_segment_convert(Segment0, Segment), !.
convert_segment('TYPE_ENUM', ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    proto_enum_value(ContextType, Value, Value0).
convert_segment('TYPE_SFIXED32', _ContextType, Segment0, Value) =>
    Segment = fixed32(_,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int32_codes(Value, Codes).
convert_segment('TYPE_SFIXED64', _ContextType, Segment0, Value) =>
    Segment = fixed64(_,Codes),
    protobuf_segment_convert(Segment0, Segment), !,
    int64_codes(Value, Codes).
convert_segment('TYPE_SINT32', _ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    integer_zigzag(Value, Value0).
convert_segment('TYPE_SINT64', _ContextType, Segment0, Value) =>
    Segment = varint(_,Value0),
    protobuf_segment_convert(Segment0, Segment), !,
    integer_zigzag(Value, Value0).

int_bool(0, false).
int_bool(1, true).

:- det(combine_fields/3).
%! combine_fields(+Fields:list, +MsgDict0, -MsgDict) is det.
% Combines the fields into a dict.
% If the field is marked as 'norepeat' (optional/required), then the last
%    occurrence is kept (as per the protobuf wire spec)
% If the field is marked as 'repeat', then all the occurrences
%    are put into a list, in order.
% Assume that fields normally occur all together, but can handle
% (less efficiently) fields not occurring togeter, as is allowed
% by the protobuf spec.
combine_fields([], MsgDict0, MsgDict) => MsgDict = MsgDict0.
combine_fields([field_and_value(Field,norepeat,Value)|Fields], MsgDict0, MsgDict) =>
    put_dict(Field, MsgDict0, Value, MsgDict1),
    combine_fields(Fields, MsgDict1, MsgDict).
combine_fields([field_and_value(Field,repeat,Value)|Fields], MsgDict0, MsgDict) =>
    combine_fields_repeat(Fields, Field, NewValues, RestFields),
    (   get_dict(Field, MsgDict0, ExistingValues)
    ->  append(ExistingValues, [Value|NewValues], Values)
    ;   Values = [Value|NewValues]
    ),
    put_dict(Field, MsgDict0, Values, MsgDict1),
    combine_fields(RestFields, MsgDict1, MsgDict).

:- det(combine_fields_repeat/4).
%! combine_fields_repeat(+Fields:list, Field:atom, -Values:list, RestFields:list) is det.
% Helper for combine_fields/3
% Stops at the first item that doesn't match =Field= - the assumption
% is that all the items for a field will be together and if they're
% not, they would be combined outside this predicate.
%
% @param Fields a list of fields (Field-Repeat-Value)
% @param Field the name of the field that is being combined
% @param Values gets the Value items that match Field
% @param RestFields gets any left-over fields
combine_fields_repeat([], _Field, Values, RestFields) => Values = [], RestFields = [].
combine_fields_repeat([Field-repeat-Value|Fields], Field, Values, RestFields) =>
    Values = [Value|Values2],
    combine_fields_repeat(Fields, Field, Values2, RestFields).
combine_fields_repeat(Fields, _Field, Values, RestFields) => Values = [], RestFields = Fields.

:- det(field_and_type/7).
%! field_and_type(+ContextType:atom, +Tag:int, -FieldName:atom, -FqnName:atom, -ContextType2:atom, -RepeatOptional:atom, -Type:atom) is det.
% Lookup a =ContextType= and =Tag= to get the field name, type, etc.
field_and_type(ContextType, Tag, FieldName, FqnName, ContextType2, RepeatOptional, Type) =>
    proto_field_name(ContextType, Tag, FieldName, FqnName),
    proto_field_type_name(FqnName, ContextType2),
    fqn_repeat_optional(FqnName, RepeatOptional),
    proto_field_type(FqnName, Type).

%! fqn_repeat_optional(+FqnName:atom, -RepeatOptional:atom) is det.
% Lookup up proto_field_label(FqnName, _), proto_field_option_packed(FqnName)
% and set RepeatOptional to one of
% =norepeat=, =repeat=, =repeat_packed=.
fqn_repeat_optional(FqnName, RepeatOptional) =>
    proto_field_label(FqnName, LabelRepeatOptional),
    (   LabelRepeatOptional = 'LABEL_REPEATED',
        proto_field_option_packed(FqnName)
    ->  RepeatOptional = repeat_packed
    ;   \+ proto_field_option_packed(FqnName), % validity check
        fqn_repeat_optional_2(LabelRepeatOptional, RepeatOptional)
    ).

:- det(fqn_repeat_optional_2/2).
%! fqn_repeat_optional_2(+DescriptorLabelEnum:atom, -RepeatOrEmpty:atom) is det.
% Map the descriptor "label" to 'repeat' or 'norepeat'.
fqn_repeat_optional_2('LABEL_OPTIONAL', norepeat).
fqn_repeat_optional_2('LABEL_REQUIRED', norepeat).
fqn_repeat_optional_2('LABEL_REPEATED', repeat).

%! field_descriptor_label_repeated(+Label:atom) is semidet.
% From message FieldDescriptorProto enum Label
field_descriptor_label_repeated('LABEL_REPEATED').

%! field_descriptor_label_single(+Label:atom) is semidet.
% From message FieldDescriptorProto enum Label
field_descriptor_label_single('LABEL_OPTIONAL').
field_descriptor_label_single('LABEL_REQUIRED').

%! term_expansion(+Term, -Expansion) is semidet.
% Term expansion for =|descriptor_proto(Proto)|=.
term_expansion(descriptor_proto(Proto), Expansion) :-
    phrase(descriptor_proto_expand_file(Proto), ExpansionRaw),
    sort(ExpansionRaw, Expansion). % prevent warning messages about discontiguous messages

%! descriptor_proto(-Proto) is det.
% descriptor_proto/1 is expanded using
% descriptor_proto_expand:descriptor_proto_expand_file//1.
%
% It was generated by running parse_descriptor_proto_dump.pl
% (parse_descriptor/0) over the output of protoc --decode=FileDescriptorSet
% (see Makefile rule descriptor.proto.parse) and then doing some
% minor reformatting and rearrangement of the fields (e.g., putting
% the =name= field first).
descriptor_proto(
  file{
    name:'descriptor.proto',
    package:'google.protobuf',
    message_type:
    [
     message_type{ name:'FileDescriptorSet',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:file,
                                  label:'LABEL_REPEATED',
                                  name:file,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FileDescriptorProto'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'FileDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:package,
                                  label:'LABEL_OPTIONAL',
                                  name:package,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:dependency,
                                  label:'LABEL_REPEATED',
                                  name:dependency,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:publicDependency,
                                  label:'LABEL_REPEATED',
                                  name:public_dependency,
                                  number:10,
                                  options:options{},
                                  type:'TYPE_INT32',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:weakDependency,
                                  label:'LABEL_REPEATED',
                                  name:weak_dependency,
                                  number:11,
                                  options:options{},
                                  type:'TYPE_INT32',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:messageType,
                                  label:'LABEL_REPEATED',
                                  name:message_type,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.DescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:enumType,
                                  label:'LABEL_REPEATED',
                                  name:enum_type,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:service,
                                  label:'LABEL_REPEATED',
                                  name:service,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.ServiceDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:extension,
                                  label:'LABEL_REPEATED',
                                  name:extension,
                                  number:7,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FieldDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:8,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FileOptions'
                                },
                           field{ default_value:'',
                                  json_name:sourceCodeInfo,
                                  label:'LABEL_OPTIONAL',
                                  name:source_code_info,
                                  number:9,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.SourceCodeInfo'
                                },
                           field{ default_value:'',
                                  json_name:syntax,
                                  label:'LABEL_OPTIONAL',
                                  name:syntax,
                                  number:12,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'DescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:field,
                                  label:'LABEL_REPEATED',
                                  name:field,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FieldDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:extension,
                                  label:'LABEL_REPEATED',
                                  name:extension,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FieldDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:nestedType,
                                  label:'LABEL_REPEATED',
                                  name:nested_type,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.DescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:enumType,
                                  label:'LABEL_REPEATED',
                                  name:enum_type,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:extensionRange,
                                  label:'LABEL_REPEATED',
                                  name:extension_range,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.DescriptorProto.ExtensionRange'
                                },
                           field{ default_value:'',
                                  json_name:oneofDecl,
                                  label:'LABEL_REPEATED',
                                  name:oneof_decl,
                                  number:8,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.OneofDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:7,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.MessageOptions'
                                },
                           field{ default_value:'',
                                  json_name:reservedRange,
                                  label:'LABEL_REPEATED',
                                  name:reserved_range,
                                  number:9,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.DescriptorProto.ReservedRange'
                                },
                           field{ default_value:'',
                                  json_name:reservedName,
                                  label:'LABEL_REPEATED',
                                  name:reserved_name,
                                  number:10,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                }
                         ],
                   nested_type:[ nested_type{ name:'ExtensionRange',
                                              field:[ field{ default_value:'',
                                                           json_name:start,
                                                             label:'LABEL_OPTIONAL',
                                                             name:start,
                                                             number:1,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:end,
                                                             label:'LABEL_OPTIONAL',
                                                             name:end,
                                                             number:2,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:options,
                                                             label:'LABEL_OPTIONAL',
                                                             name:options,
                                                             number:3,
                                                             options:options{},
                                                             type:'TYPE_MESSAGE',
                                                             type_name:'.google.protobuf.ExtensionRangeOptions'
                                                           }
                                                    ]
                                            },
                                 nested_type{ name:'ReservedRange',
                                              field:[ field{ default_value:'',
                                                             json_name:start,
                                                             label:'LABEL_OPTIONAL',
                                                             name:start,
                                                             number:1,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:end,
                                                             label:'LABEL_OPTIONAL',
                                                             name:end,
                                                             number:2,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           }
                                                    ]
                                            }
                               ],
                   reserved_range:[]
                 },
     message_type{ name:'ExtensionRangeOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'FieldDescriptorProto',
                   enum_type:[ enum_type{ name:'Type',
                                          value:[ value{ name:'TYPE_DOUBLE',
                                                         number:1
                                                       },
                                                  value{ name:'TYPE_FLOAT',
                                                         number:2
                                                       },
                                                  value{ name:'TYPE_INT64',
                                                         number:3
                                                       },
                                                  value{ name:'TYPE_UINT64',
                                                         number:4
                                                       },
                                                  value{ name:'TYPE_INT32',
                                                         number:5
                                                       },
                                                  value{ name:'TYPE_FIXED64',
                                                         number:6
                                                       },
                                                  value{ name:'TYPE_FIXED32',
                                                         number:7
                                                       },
                                                  value{ name:'TYPE_BOOL',
                                                         number:8
                                                       },
                                                  value{ name:'TYPE_STRING',
                                                         number:9
                                                       },
                                                  value{ name:'TYPE_GROUP',
                                                         number:10
                                                       },
                                                  value{ name:'TYPE_MESSAGE',
                                                         number:11
                                                       },
                                                  value{ name:'TYPE_BYTES',
                                                         number:12
                                                       },
                                                  value{ name:'TYPE_UINT32',
                                                         number:13
                                                       },
                                                  value{ name:'TYPE_ENUM',
                                                         number:14
                                                       },
                                                  value{ name:'TYPE_SFIXED32',
                                                         number:15
                                                       },
                                                  value{ name:'TYPE_SFIXED64',
                                                         number:16
                                                       },
                                                  value{ name:'TYPE_SINT32',
                                                         number:17
                                                       },
                                                  value{ name:'TYPE_SINT64',
                                                         number:18
                                                       }
                                                ]
                                        },
                               enum_type{ name:'Label',
                                          value:[ value{ name:'LABEL_OPTIONAL',
                                                         number:1
                                                       },
                                                  value{ name:'LABEL_REQUIRED',
                                                         number:2
                                                       },
                                                  value{ name:'LABEL_REPEATED',
                                                         number:3
                                                       }
                                                ]
                                        }
                             ],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:number,
                                  label:'LABEL_OPTIONAL',
                                  name:number,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_INT32',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:label,
                                  label:'LABEL_OPTIONAL',
                                  name:label,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.FieldDescriptorProto.Label'
                                },
                           field{ default_value:'',
                                  json_name:type,
                                  label:'LABEL_OPTIONAL',
                                  name:type,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.FieldDescriptorProto.Type'
                                },
                           field{ default_value:'',
                                  json_name:typeName,
                                  label:'LABEL_OPTIONAL',
                                  name:type_name,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:extendee,
                                  label:'LABEL_OPTIONAL',
                                  name:extendee,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:defaultValue,
                                  label:'LABEL_OPTIONAL',
                                  name:default_value,
                                  number:7,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:oneofIndex,
                                  label:'LABEL_OPTIONAL',
                                  name:oneof_index,
                                  number:9,
                                  options:options{},
                                  type:'TYPE_INT32',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:jsonName,
                                  label:'LABEL_OPTIONAL',
                                  name:json_name,
                                  number:10,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:8,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.FieldOptions'
                                },
                           field{ default_value:'',
                                  json_name:proto3Optional,
                                  label:'LABEL_OPTIONAL',
                                  name:proto3_optional,
                                  number:17,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'OneofDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.OneofOptions'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'EnumDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:value,
                                  label:'LABEL_REPEATED',
                                  name:value,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumValueDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumOptions'
                                },
                           field{ default_value:'',
                                  json_name:reservedRange,
                                  label:'LABEL_REPEATED',
                                  name:reserved_range,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumDescriptorProto.EnumReservedRange'
                                },
                           field{ default_value:'',
                                  json_name:reservedName,
                                  label:'LABEL_REPEATED',
                                  name:reserved_name,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                }
                         ],
                   nested_type:[ nested_type{ name:'EnumReservedRange',
                                              field:[ field{ default_value:'',
                                                             json_name:start,
                                                             label:'LABEL_OPTIONAL',
                                                             name:start,
                                                             number:1,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:end,
                                                             label:'LABEL_OPTIONAL',
                                                             name:end,
                                                             number:2,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           }
                                                    ]
                                            }
                               ],
                   reserved_range:[]
                 },
     message_type{ name:'EnumValueDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:number,
                                  label:'LABEL_OPTIONAL',
                                  name:number,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_INT32',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.EnumValueOptions'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'ServiceDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:method,
                                  label:'LABEL_REPEATED',
                                  name:method,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.MethodDescriptorProto'
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.ServiceOptions'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'MethodDescriptorProto',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_OPTIONAL',
                                  name:name,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:inputType,
                                  label:'LABEL_OPTIONAL',
                                  name:input_type,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:outputType,
                                  label:'LABEL_OPTIONAL',
                                  name:output_type,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:options,
                                  label:'LABEL_OPTIONAL',
                                  name:options,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.MethodOptions'
                                },
                           field{ default_value:false,
                                  json_name:clientStreaming,
                                  label:'LABEL_OPTIONAL',
                                  name:client_streaming,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:serverStreaming,
                                  label:'LABEL_OPTIONAL',
                                  name:server_streaming,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'FileOptions',
                   enum_type:[ enum_type{ name:'OptimizeMode',
                                          value:[ value{ name:'SPEED',
                                                         number:1
                                                       },
                                                  value{ name:'CODE_SIZE',
                                                         number:2
                                                       },
                                                  value{ name:'LITE_RUNTIME',
                                                         number:3
                                                       }
                                                ]
                                        }
                             ],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:'',
                                  json_name:javaPackage,
                                  label:'LABEL_OPTIONAL',
                                  name:java_package,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:javaOuterClassname,
                                  label:'LABEL_OPTIONAL',
                                  name:java_outer_classname,
                                  number:8,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:javaMultipleFiles,
                                  label:'LABEL_OPTIONAL',
                                  name:java_multiple_files,
                                  number:10,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:javaGenerateEqualsAndHash,
                                  label:'LABEL_OPTIONAL',
                                  name:java_generate_equals_and_hash,
                                  number:20,
                                  options:options{ deprecated:true,
                                                   packed:''
                                                 },
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:javaStringCheckUtf8,
                                  label:'LABEL_OPTIONAL',
                                  name:java_string_check_utf8,
                                  number:27,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'SPEED',
                                  json_name:optimizeFor,
                                  label:'LABEL_OPTIONAL',
                                  name:optimize_for,
                                  number:9,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.FileOptions.OptimizeMode'
                                },
                           field{ default_value:'',
                                  json_name:goPackage,
                                  label:'LABEL_OPTIONAL',
                                  name:go_package,
                                  number:11,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:ccGenericServices,
                                  label:'LABEL_OPTIONAL',
                                  name:cc_generic_services,
                                  number:16,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:javaGenericServices,
                                  label:'LABEL_OPTIONAL',
                                  name:java_generic_services,
                                  number:17,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:pyGenericServices,
                                  label:'LABEL_OPTIONAL',
                                  name:py_generic_services,
                                  number:18,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:phpGenericServices,
                                  label:'LABEL_OPTIONAL',
                                  name:php_generic_services,
                                  number:42,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:23,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:true,
                                  json_name:ccEnableArenas,
                                  label:'LABEL_OPTIONAL',
                                  name:cc_enable_arenas,
                                  number:31,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:objcClassPrefix,
                                  label:'LABEL_OPTIONAL',
                                  name:objc_class_prefix,
                                  number:36,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:csharpNamespace,
                                  label:'LABEL_OPTIONAL',
                                  name:csharp_namespace,
                                  number:37,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:swiftPrefix,
                                  label:'LABEL_OPTIONAL',
                                  name:swift_prefix,
                                  number:39,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:phpClassPrefix,
                                  label:'LABEL_OPTIONAL',
                                  name:php_class_prefix,
                                  number:40,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:phpNamespace,
                                  label:'LABEL_OPTIONAL',
                                  name:php_namespace,
                                  number:41,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:phpMetadataNamespace,
                                  label:'LABEL_OPTIONAL',
                                  name:php_metadata_namespace,
                                  number:44,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:rubyPackage,
                                  label:'LABEL_OPTIONAL',
                                  name:ruby_package,
                                  number:45,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[ reserved_range{ end:39,
                                                    start:38
                                                  }
                                  ]
                 },
     message_type{ name:'MessageOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:false,
                                  json_name:messageSetWireFormat,
                                  label:'LABEL_OPTIONAL',
                                  name:message_set_wire_format,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:noStandardDescriptorAccessor,
                                  label:'LABEL_OPTIONAL',
                                  name:no_standard_descriptor_accessor,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:mapEntry,
                                  label:'LABEL_OPTIONAL',
                                  name:map_entry,
                                  number:7,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[ reserved_range{ end:5,
                                                    start:4
                                                  },
                                    reserved_range{ end:6,
                                                    start:5
                                                  },
                                    reserved_range{ end:7,
                                                    start:6
                                                  },
                                    reserved_range{ end:9,
                                                    start:8
                                                  },
                                    reserved_range{ end:10,
                                                    start:9
                                                  }
                                  ]
                 },
     message_type{ name:'FieldOptions',
                   enum_type:[ enum_type{ name:'CType',
                                          value:[ value{ name:'STRING',
                                                         number:0
                                                       },
                                                  value{ name:'CORD',
                                                         number:1
                                                       },
                                                  value{ name:'STRING_PIECE',
                                                         number:2
                                                       }
                                                ]
                                        },
                               enum_type{ name:'JSType',
                                          value:[ value{ name:'JS_NORMAL',
                                                         number:0
                                                       },
                                                  value{ name:'JS_STRING',
                                                         number:1
                                                       },
                                                  value{ name:'JS_NUMBER',
                                                         number:2
                                                       }
                                                ]
                                        }
                             ],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:'STRING',
                                  json_name:ctype,
                                  label:'LABEL_OPTIONAL',
                                  name:ctype,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.FieldOptions.CType'
                                },
                           field{ default_value:'',
                                  json_name:packed,
                                  label:'LABEL_OPTIONAL',
                                  name:packed,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'JS_NORMAL',
                                  json_name:jstype,
                                  label:'LABEL_OPTIONAL',
                                  name:jstype,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.FieldOptions.JSType'
                                },
                           field{ default_value:false,
                                  json_name:lazy,
                                  label:'LABEL_OPTIONAL',
                                  name:lazy,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:weak,
                                  label:'LABEL_OPTIONAL',
                                  name:weak,
                                  number:10,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[ reserved_range{ end:5,
                                                    start:4
                                                  }
                                  ]
                 },
     message_type{ name:'OneofOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'EnumOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:'',
                                  json_name:allowAlias,
                                  label:'LABEL_OPTIONAL',
                                  name:allow_alias,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[ reserved_range{ end:6,
                                                    start:5
                                                  }
                                  ]
                 },
     message_type{ name:'EnumValueOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'ServiceOptions',
                   enum_type:[],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:33,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'MethodOptions',
                   enum_type:[ enum_type{ name:'IdempotencyLevel',
                                          value:[ value{ name:'IDEMPOTENCY_UNKNOWN',
                                                         number:0
                                                       },
                                                  value{ name:'NO_SIDE_EFFECTS',
                                                         number:1
                                                       },
                                                  value{ name:'IDEMPOTENT',
                                                         number:2
                                                       }
                                                ]
                                        }
                             ],
                   extension_range:[ extension_range{ end:536870912,
                                                      start:1000
                                                    }
                                   ],
                   field:[ field{ default_value:false,
                                  json_name:deprecated,
                                  label:'LABEL_OPTIONAL',
                                  name:deprecated,
                                  number:33,
                                  options:options{},
                                  type:'TYPE_BOOL',
                                  type_name:''
                                },
                           field{ default_value:'IDEMPOTENCY_UNKNOWN',
                                  json_name:idempotencyLevel,
                                  label:'LABEL_OPTIONAL',
                                  name:idempotency_level,
                                  number:34,
                                  options:options{},
                                  type:'TYPE_ENUM',
                                  type_name:'.google.protobuf.MethodOptions.IdempotencyLevel'
                                },
                           field{ default_value:'',
                                  json_name:uninterpretedOption,
                                  label:'LABEL_REPEATED',
                                  name:uninterpreted_option,
                                  number:999,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption'
                                }
                         ],
                   nested_type:[],
                   reserved_range:[]
                 },
     message_type{ name:'UninterpretedOption',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:name,
                                  label:'LABEL_REPEATED',
                                  name:name,
                                  number:2,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.UninterpretedOption.NamePart'
                                },
                           field{ default_value:'',
                                  json_name:identifierValue,
                                  label:'LABEL_OPTIONAL',
                                  name:identifier_value,
                                  number:3,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:positiveIntValue,
                                  label:'LABEL_OPTIONAL',
                                  name:positive_int_value,
                                  number:4,
                                  options:options{},
                                  type:'TYPE_UINT64',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:negativeIntValue,
                                  label:'LABEL_OPTIONAL',
                                  name:negative_int_value,
                                  number:5,
                                  options:options{},
                                  type:'TYPE_INT64',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:doubleValue,
                                  label:'LABEL_OPTIONAL',
                                  name:double_value,
                                  number:6,
                                  options:options{},
                                  type:'TYPE_DOUBLE',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:stringValue,
                                  label:'LABEL_OPTIONAL',
                                  name:string_value,
                                  number:7,
                                  options:options{},
                                  type:'TYPE_BYTES',
                                  type_name:''
                                },
                           field{ default_value:'',
                                  json_name:aggregateValue,
                                  label:'LABEL_OPTIONAL',
                                  name:aggregate_value,
                                  number:8,
                                  options:options{},
                                  type:'TYPE_STRING',
                                  type_name:''
                                }
                         ],
                   nested_type:[ nested_type{ name:'NamePart',
                                              field:[ field{ default_value:'',
                                                             json_name:namePart,
                                                             label:'LABEL_REQUIRED',
                                                             name:name_part,
                                                             number:1,
                                                             options:options{},
                                                             type:'TYPE_STRING',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:isExtension,
                                                             label:'LABEL_REQUIRED',
                                                             name:is_extension,
                                                             number:2,
                                                             options:options{},
                                                             type:'TYPE_BOOL',
                                                             type_name:''
                                                           }
                                                    ]
                                            }
                               ],
                   reserved_range:[]
                 },
     message_type{ name:'SourceCodeInfo',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:location,
                                  label:'LABEL_REPEATED',
                                  name:location,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.SourceCodeInfo.Location'
                                }
                         ],
                   nested_type:[ nested_type{ name:'Location',
                                              field:[ field{ default_value:'',
                                                             json_name:path,
                                                             label:'LABEL_REPEATED',
                                                             name:path,
                                                             number:1,
                                                             options:options{ deprecated:'',
                                                                              packed:true
                                                                            },
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:span,
                                                             label:'LABEL_REPEATED',
                                                             name:span,
                                                             number:2,
                                                             options:options{ deprecated:'',
                                                                              packed:true
                                                                            },
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:leadingComments,
                                                             label:'LABEL_OPTIONAL',
                                                             name:leading_comments,
                                                             number:3,
                                                             options:options{},
                                                             type:'TYPE_STRING',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:trailingComments,
                                                             label:'LABEL_OPTIONAL',
                                                             name:trailing_comments,
                                                             number:4,
                                                             options:options{},
                                                             type:'TYPE_STRING',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:leadingDetachedComments,
                                                             label:'LABEL_REPEATED',
                                                             name:leading_detached_comments,
                                                             number:6,
                                                             options:options{},
                                                             type:'TYPE_STRING',
                                                             type_name:''
                                                           }
                                                    ]
                                            }
                               ],
                   reserved_range:[]
                 },
     message_type{ name:'GeneratedCodeInfo',
                   enum_type:[],
                   extension_range:[],
                   field:[ field{ default_value:'',
                                  json_name:annotation,
                                  label:'LABEL_REPEATED',
                                  name:annotation,
                                  number:1,
                                  options:options{},
                                  type:'TYPE_MESSAGE',
                                  type_name:'.google.protobuf.GeneratedCodeInfo.Annotation'
                                }
                         ],
                   nested_type:[ nested_type{ name:'Annotation',
                                              field:[ field{ default_value:'',
                                                             json_name:path,
                                                             label:'LABEL_REPEATED',
                                                             name:path,
                                                             number:1,
                                                             options:options{ deprecated:'',
                                                                              packed:true
                                                                            },
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:sourceFile,
                                                             label:'LABEL_OPTIONAL',
                                                             name:source_file,
                                                             number:2,
                                                             options:options{},
                                                             type:'TYPE_STRING',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:begin,
                                                             label:'LABEL_OPTIONAL',
                                                             name:begin,
                                                             number:3,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           },
                                                      field{ default_value:'',
                                                             json_name:end,
                                                             label:'LABEL_OPTIONAL',
                                                             name:end,
                                                             number:4,
                                                             options:options{},
                                                             type:'TYPE_INT32',
                                                             type_name:''
                                                           }
                                                    ]
                                            }
                               ],
                   reserved_range:[]
                 }
    ],
    options:options{ cc_enable_arenas:true,
                     csharp_namespace:'Google.Protobuf.Reflection',
                     go_package:'google.golang.org/protobuf/types/descriptorpb',
                     java_outer_classname:'DescriptorProtos',
                     java_package:'com.google.protobuf',
                     objc_class_prefix:'GPB',
                     optimize_for:'SPEED'
                   }
  }).

end_of_file.
