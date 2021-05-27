% -*- mode: Prolog -*-

% Inputs a .proto.wire file and outputs a Prolog term that represents
% the same data. The .proto.wire file can be created by:
%     protoc --descriptor_set_out=___.proto.wire

% The protobuf metadata is in descriptor_proto/1, which is derived
% from descriptor.proto (libprotoc 3.6.1). Eventually, this will be bootstrapped
% to use the .proto.wire data; but for now, the process is:
%     protoc --include_imports --descriptor_set_out=descriptor.proto.wire \
%       -I$HOME/src/protobuf/src/google/protobuf \
%       descriptor.proto
%     protoc -I. -I$HOME/src/protobuf/src/google/protobuf \
%       --decode=google.protobuf.FileDescriptorSet \
%       descriptor.proto \
%       < descriptor.proto.wire
%       > descriptor.proto.wiredump
%   And then run use parse_descriptor_proto_dump.pl:
%     ?- parse_descriptor('descriptor.proto.wiredump').

:- module(descriptor_proto,
    [ % Term expansion of descriptor_proto/1 creates the following facts
      % see descriptor_proto_expand.pl
     proto_package/3,
     proto_message_type/3,
     proto_field_name/4,
     proto_field_json_name/2,
     proto_field_label/2,
     proto_field_type/2,
     proto_field_type_name/2,
     proto_field_default_value/2,
     proto_field_option_packed/1,
     proto_enum_type/3,
     proto_enum_value/3,
     main/0
    ]).

:- use_module(descriptor_proto_expand, [descriptor_proto_expand_FileDescriptorSet/2]).
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
    % protobuf_segment_message/2 can leave choicepoints, and we don't
    % want to backtrack through all the possibilities because that
    % leads to combinatoric explosion; instead use
    % protobuf_segment_convert/2 to change segments that were guessed
    % incorrectly.
    !, % don't use any other possible Segments - let protobuf_segment_convert/2 do the job
    maplist(segment_to_term('.google.protobuf.FileDescriptorSet'), Segments, Msg),
    maplist(write_metadata, Msg).

write_metadata(field_and_value(file,repeat,FileDescriptor)) =>
    FileDescriptor >:< '.google.protobuf.FileDescriptorProto'{name:FileName},
    print_term_cleaned(protobuf_metadata(FileName, FileDescriptor), [indent_arguments(4)], MsgStr),
    write(user_output, MsgStr),
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
    Segment = string(_,ValueStr),
    protobuf_segment_convert(Segment0, Segment), !,
    (   true     % TODO: control whether atom or string with an option
    ->  atom_string(Value, ValueStr)
    ;   Value = ValueStr
    ).
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

%! term_expansion(+Term, -Expansion) is semidet.
% Term expansion for =|descriptor_set(Set)|=.
term_expansion(descriptor_set(Set), Expansion) :-
    descriptor_proto_expand_FileDescriptorSet(Set, Expansion).

%! descriptor_set(-Set) is det.
% descriptor_set/1 is expanded using
% descriptor_set_expand:descriptor_proto_expand_FileDescriptorSet//1.
%
% It was generated by running parse_descriptor_proto_dump.pl
% (parse_descriptor/0) over the output of protoc --decode=FileDescriptorSet
% (see Makefile rule plugin.proto.parse).
descriptor_set(
'FileDescriptorSet'{
    file:[ 'FileDescriptorProto'{
               dependency:'',
               message_type:[ 'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:file,
                                              label:'LABEL_REPEATED',
                                              name:file,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FileDescriptorProto'
                                            }
                                        ],
                                  name:'FileDescriptorSet',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:package,
                                              label:'LABEL_OPTIONAL',
                                              name:package,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:dependency,
                                              label:'LABEL_REPEATED',
                                              name:dependency,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:publicDependency,
                                              label:'LABEL_REPEATED',
                                              name:public_dependency,
                                              number:10,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:weakDependency,
                                              label:'LABEL_REPEATED',
                                              name:weak_dependency,
                                              number:11,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:messageType,
                                              label:'LABEL_REPEATED',
                                              name:message_type,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.DescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:enumType,
                                              label:'LABEL_REPEATED',
                                              name:enum_type,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:service,
                                              label:'LABEL_REPEATED',
                                              name:service,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.ServiceDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:extension,
                                              label:'LABEL_REPEATED',
                                              name:extension,
                                              number:7,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FieldDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:8,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FileOptions'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:sourceCodeInfo,
                                              label:'LABEL_OPTIONAL',
                                              name:source_code_info,
                                              number:9,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.SourceCodeInfo'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:syntax,
                                              label:'LABEL_OPTIONAL',
                                              name:syntax,
                                              number:12,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            }
                                        ],
                                  name:'FileDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:field,
                                              label:'LABEL_REPEATED',
                                              name:field,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FieldDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:extension,
                                              label:'LABEL_REPEATED',
                                              name:extension,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FieldDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:nestedType,
                                              label:'LABEL_REPEATED',
                                              name:nested_type,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.DescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:enumType,
                                              label:'LABEL_REPEATED',
                                              name:enum_type,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:extensionRange,
                                              label:'LABEL_REPEATED',
                                              name:extension_range,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.DescriptorProto.ExtensionRange'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:oneofDecl,
                                              label:'LABEL_REPEATED',
                                              name:oneof_decl,
                                              number:8,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.OneofDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:7,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.MessageOptions'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:reservedRange,
                                              label:'LABEL_REPEATED',
                                              name:reserved_range,
                                              number:9,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.DescriptorProto.ReservedRange'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:reservedName,
                                              label:'LABEL_REPEATED',
                                              name:reserved_name,
                                              number:10,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            }
                                        ],
                                  name:'DescriptorProto',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:start,
                                                                label:'LABEL_OPTIONAL',
                                                                name:start,
                                                                number:1,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:end,
                                                                label:'LABEL_OPTIONAL',
                                                                name:end,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:options,
                                                                label:'LABEL_OPTIONAL',
                                                                name:options,
                                                                number:3,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_MESSAGE',
                                                                type_name:'.google.protobuf.ExtensionRangeOptions'
                                                              }
                                                          ],
                                                    name:'ExtensionRange',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  },
                                                'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:start,
                                                                label:'LABEL_OPTIONAL',
                                                                name:start,
                                                                number:1,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:end,
                                                                label:'LABEL_OPTIONAL',
                                                                name:end,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              }
                                                          ],
                                                    name:'ReservedRange',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'ExtensionRangeOptions',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[ 'EnumDescriptorProto'{
                                                  name:'Type',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'TYPE_DOUBLE',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_FLOAT',
                                                              number:2
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_INT64',
                                                              number:3
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_UINT64',
                                                              number:4
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_INT32',
                                                              number:5
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_FIXED64',
                                                              number:6
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_FIXED32',
                                                              number:7
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_BOOL',
                                                              number:8
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_STRING',
                                                              number:9
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_GROUP',
                                                              number:10
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_MESSAGE',
                                                              number:11
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_BYTES',
                                                              number:12
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_UINT32',
                                                              number:13
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_ENUM',
                                                              number:14
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_SFIXED32',
                                                              number:15
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_SFIXED64',
                                                              number:16
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_SINT32',
                                                              number:17
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'TYPE_SINT64',
                                                              number:18
                                                            }
                                                        ]
                                                },
                                              'EnumDescriptorProto'{
                                                  name:'Label',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'LABEL_OPTIONAL',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'LABEL_REQUIRED',
                                                              number:2
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'LABEL_REPEATED',
                                                              number:3
                                                            }
                                                        ]
                                                }
                                            ],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:number,
                                              label:'LABEL_OPTIONAL',
                                              name:number,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:label,
                                              label:'LABEL_OPTIONAL',
                                              name:label,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.FieldDescriptorProto.Label'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:type,
                                              label:'LABEL_OPTIONAL',
                                              name:type,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.FieldDescriptorProto.Type'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:typeName,
                                              label:'LABEL_OPTIONAL',
                                              name:type_name,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:extendee,
                                              label:'LABEL_OPTIONAL',
                                              name:extendee,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:defaultValue,
                                              label:'LABEL_OPTIONAL',
                                              name:default_value,
                                              number:7,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:oneofIndex,
                                              label:'LABEL_OPTIONAL',
                                              name:oneof_index,
                                              number:9,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:jsonName,
                                              label:'LABEL_OPTIONAL',
                                              name:json_name,
                                              number:10,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:8,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FieldOptions'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:proto3Optional,
                                              label:'LABEL_OPTIONAL',
                                              name:proto3_optional,
                                              number:17,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            }
                                        ],
                                  name:'FieldDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.OneofOptions'
                                            }
                                        ],
                                  name:'OneofDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:value,
                                              label:'LABEL_REPEATED',
                                              name:value,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumValueDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumOptions'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:reservedRange,
                                              label:'LABEL_REPEATED',
                                              name:reserved_range,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumDescriptorProto.EnumReservedRange'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:reservedName,
                                              label:'LABEL_REPEATED',
                                              name:reserved_name,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            }
                                        ],
                                  name:'EnumDescriptorProto',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:start,
                                                                label:'LABEL_OPTIONAL',
                                                                name:start,
                                                                number:1,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:end,
                                                                label:'LABEL_OPTIONAL',
                                                                name:end,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              }
                                                          ],
                                                    name:'EnumReservedRange',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:number,
                                              label:'LABEL_OPTIONAL',
                                              name:number,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.EnumValueOptions'
                                            }
                                        ],
                                  name:'EnumValueDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:method,
                                              label:'LABEL_REPEATED',
                                              name:method,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.MethodDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.ServiceOptions'
                                            }
                                        ],
                                  name:'ServiceDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_OPTIONAL',
                                              name:name,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:inputType,
                                              label:'LABEL_OPTIONAL',
                                              name:input_type,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:outputType,
                                              label:'LABEL_OPTIONAL',
                                              name:output_type,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:options,
                                              label:'LABEL_OPTIONAL',
                                              name:options,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.MethodOptions'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:clientStreaming,
                                              label:'LABEL_OPTIONAL',
                                              name:client_streaming,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:serverStreaming,
                                              label:'LABEL_OPTIONAL',
                                              name:server_streaming,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            }
                                        ],
                                  name:'MethodDescriptorProto',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[ 'EnumDescriptorProto'{
                                                  name:'OptimizeMode',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'SPEED',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'CODE_SIZE',
                                                              number:2
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'LITE_RUNTIME',
                                                              number:3
                                                            }
                                                        ]
                                                }
                                            ],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:javaPackage,
                                              label:'LABEL_OPTIONAL',
                                              name:java_package,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:javaOuterClassname,
                                              label:'LABEL_OPTIONAL',
                                              name:java_outer_classname,
                                              number:8,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:javaMultipleFiles,
                                              label:'LABEL_OPTIONAL',
                                              name:java_multiple_files,
                                              number:10,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:javaGenerateEqualsAndHash,
                                              label:'LABEL_OPTIONAL',
                                              name:java_generate_equals_and_hash,
                                              number:20,
                                              options:'FieldOptions'{
                                                          deprecated:true,
                                                          packed:''
                                                        },
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:javaStringCheckUtf8,
                                              label:'LABEL_OPTIONAL',
                                              name:java_string_check_utf8,
                                              number:27,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'SPEED',
                                              json_name:optimizeFor,
                                              label:'LABEL_OPTIONAL',
                                              name:optimize_for,
                                              number:9,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.FileOptions.OptimizeMode'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:goPackage,
                                              label:'LABEL_OPTIONAL',
                                              name:go_package,
                                              number:11,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:ccGenericServices,
                                              label:'LABEL_OPTIONAL',
                                              name:cc_generic_services,
                                              number:16,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:javaGenericServices,
                                              label:'LABEL_OPTIONAL',
                                              name:java_generic_services,
                                              number:17,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:pyGenericServices,
                                              label:'LABEL_OPTIONAL',
                                              name:py_generic_services,
                                              number:18,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:phpGenericServices,
                                              label:'LABEL_OPTIONAL',
                                              name:php_generic_services,
                                              number:42,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:23,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:true,
                                              json_name:ccEnableArenas,
                                              label:'LABEL_OPTIONAL',
                                              name:cc_enable_arenas,
                                              number:31,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:objcClassPrefix,
                                              label:'LABEL_OPTIONAL',
                                              name:objc_class_prefix,
                                              number:36,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:csharpNamespace,
                                              label:'LABEL_OPTIONAL',
                                              name:csharp_namespace,
                                              number:37,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:swiftPrefix,
                                              label:'LABEL_OPTIONAL',
                                              name:swift_prefix,
                                              number:39,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:phpClassPrefix,
                                              label:'LABEL_OPTIONAL',
                                              name:php_class_prefix,
                                              number:40,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:phpNamespace,
                                              label:'LABEL_OPTIONAL',
                                              name:php_namespace,
                                              number:41,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:phpMetadataNamespace,
                                              label:'LABEL_OPTIONAL',
                                              name:php_metadata_namespace,
                                              number:44,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:rubyPackage,
                                              label:'LABEL_OPTIONAL',
                                              name:ruby_package,
                                              number:45,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'FileOptions',
                                  nested_type:[],
                                  reserved_range:[ 'EnumReservedRange'{
                                                       end:39,
                                                       start:38
                                                     }
                                                 ]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:messageSetWireFormat,
                                              label:'LABEL_OPTIONAL',
                                              name:message_set_wire_format,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:noStandardDescriptorAccessor,
                                              label:'LABEL_OPTIONAL',
                                              name:no_standard_descriptor_accessor,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:mapEntry,
                                              label:'LABEL_OPTIONAL',
                                              name:map_entry,
                                              number:7,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'MessageOptions',
                                  nested_type:[],
                                  reserved_range:[ 'EnumReservedRange'{
                                                       end:5,
                                                       start:4
                                                     },
                                                   'EnumReservedRange'{
                                                       end:6,
                                                       start:5
                                                     },
                                                   'EnumReservedRange'{
                                                       end:7,
                                                       start:6
                                                     },
                                                   'EnumReservedRange'{
                                                       end:9,
                                                       start:8
                                                     },
                                                   'EnumReservedRange'{
                                                       end:10,
                                                       start:9
                                                     }
                                                 ]
                                },
                              'DescriptorProto'{
                                  enum_type:[ 'EnumDescriptorProto'{
                                                  name:'CType',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'STRING',
                                                              number:0
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'CORD',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'STRING_PIECE',
                                                              number:2
                                                            }
                                                        ]
                                                },
                                              'EnumDescriptorProto'{
                                                  name:'JSType',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'JS_NORMAL',
                                                              number:0
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'JS_STRING',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'JS_NUMBER',
                                                              number:2
                                                            }
                                                        ]
                                                }
                                            ],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'STRING',
                                              json_name:ctype,
                                              label:'LABEL_OPTIONAL',
                                              name:ctype,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.FieldOptions.CType'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:packed,
                                              label:'LABEL_OPTIONAL',
                                              name:packed,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'JS_NORMAL',
                                              json_name:jstype,
                                              label:'LABEL_OPTIONAL',
                                              name:jstype,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.FieldOptions.JSType'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:lazy,
                                              label:'LABEL_OPTIONAL',
                                              name:lazy,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:weak,
                                              label:'LABEL_OPTIONAL',
                                              name:weak,
                                              number:10,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'FieldOptions',
                                  nested_type:[],
                                  reserved_range:[ 'EnumReservedRange'{
                                                       end:5,
                                                       start:4
                                                     }
                                                 ]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'OneofOptions',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:allowAlias,
                                              label:'LABEL_OPTIONAL',
                                              name:allow_alias,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'EnumOptions',
                                  nested_type:[],
                                  reserved_range:[ 'EnumReservedRange'{
                                                       end:6,
                                                       start:5
                                                     }
                                                 ]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'EnumValueOptions',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:33,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'ServiceOptions',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[ 'EnumDescriptorProto'{
                                                  name:'IdempotencyLevel',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'IDEMPOTENCY_UNKNOWN',
                                                              number:0
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'NO_SIDE_EFFECTS',
                                                              number:1
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'IDEMPOTENT',
                                                              number:2
                                                            }
                                                        ]
                                                }
                                            ],
                                  extension_range:[ 'ExtensionRange'{
                                                        end:536870912,
                                                        start:1000
                                                      }
                                                  ],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:false,
                                              json_name:deprecated,
                                              label:'LABEL_OPTIONAL',
                                              name:deprecated,
                                              number:33,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BOOL',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'IDEMPOTENCY_UNKNOWN',
                                              json_name:idempotencyLevel,
                                              label:'LABEL_OPTIONAL',
                                              name:idempotency_level,
                                              number:34,
                                              options:'FieldOptions'{},
                                              type:'TYPE_ENUM',
                                              type_name:'.google.protobuf.MethodOptions.IdempotencyLevel'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:uninterpretedOption,
                                              label:'LABEL_REPEATED',
                                              name:uninterpreted_option,
                                              number:999,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption'
                                            }
                                        ],
                                  name:'MethodOptions',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:name,
                                              label:'LABEL_REPEATED',
                                              name:name,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.UninterpretedOption.NamePart'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:identifierValue,
                                              label:'LABEL_OPTIONAL',
                                              name:identifier_value,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:positiveIntValue,
                                              label:'LABEL_OPTIONAL',
                                              name:positive_int_value,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_UINT64',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:negativeIntValue,
                                              label:'LABEL_OPTIONAL',
                                              name:negative_int_value,
                                              number:5,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT64',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:doubleValue,
                                              label:'LABEL_OPTIONAL',
                                              name:double_value,
                                              number:6,
                                              options:'FieldOptions'{},
                                              type:'TYPE_DOUBLE',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:stringValue,
                                              label:'LABEL_OPTIONAL',
                                              name:string_value,
                                              number:7,
                                              options:'FieldOptions'{},
                                              type:'TYPE_BYTES',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:aggregateValue,
                                              label:'LABEL_OPTIONAL',
                                              name:aggregate_value,
                                              number:8,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            }
                                        ],
                                  name:'UninterpretedOption',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:namePart,
                                                                label:'LABEL_REQUIRED',
                                                                name:name_part,
                                                                number:1,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:isExtension,
                                                                label:'LABEL_REQUIRED',
                                                                name:is_extension,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_BOOL',
                                                                type_name:''
                                                              }
                                                          ],
                                                    name:'NamePart',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:location,
                                              label:'LABEL_REPEATED',
                                              name:location,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.SourceCodeInfo.Location'
                                            }
                                        ],
                                  name:'SourceCodeInfo',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:path,
                                                                label:'LABEL_REPEATED',
                                                                name:path,
                                                                number:1,
                                                                options:'FieldOptions'{
                                                                            deprecated:'',
                                                                            packed:true
                                                                          },
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:span,
                                                                label:'LABEL_REPEATED',
                                                                name:span,
                                                                number:2,
                                                                options:'FieldOptions'{
                                                                            deprecated:'',
                                                                            packed:true
                                                                          },
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:leadingComments,
                                                                label:'LABEL_OPTIONAL',
                                                                name:leading_comments,
                                                                number:3,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:trailingComments,
                                                                label:'LABEL_OPTIONAL',
                                                                name:trailing_comments,
                                                                number:4,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:leadingDetachedComments,
                                                                label:'LABEL_REPEATED',
                                                                name:leading_detached_comments,
                                                                number:6,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              }
                                                          ],
                                                    name:'Location',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:annotation,
                                              label:'LABEL_REPEATED',
                                              name:annotation,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.GeneratedCodeInfo.Annotation'
                                            }
                                        ],
                                  name:'GeneratedCodeInfo',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:path,
                                                                label:'LABEL_REPEATED',
                                                                name:path,
                                                                number:1,
                                                                options:'FieldOptions'{
                                                                            deprecated:'',
                                                                            packed:true
                                                                          },
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:sourceFile,
                                                                label:'LABEL_OPTIONAL',
                                                                name:source_file,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:begin,
                                                                label:'LABEL_OPTIONAL',
                                                                name:begin,
                                                                number:3,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:end,
                                                                label:'LABEL_OPTIONAL',
                                                                name:end,
                                                                number:4,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_INT32',
                                                                type_name:''
                                                              }
                                                          ],
                                                    name:'Annotation',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                }
                            ],
               name:'google/protobuf/descriptor.proto',
               options:'FileOptions'{
                           cc_enable_arenas:true,
                           csharp_namespace:'Google.Protobuf.Reflection',
                           go_package:'google.golang.org/protobuf/types/descriptorpb',
                           java_outer_classname:'DescriptorProtos',
                           java_package:'com.google.protobuf',
                           objc_class_prefix:'GPB',
                           optimize_for:'SPEED'
                         },
               package:'google.protobuf'
             },
           'FileDescriptorProto'{
               dependency:'google/protobuf/descriptor.proto',
               message_type:[ 'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:major,
                                              label:'LABEL_OPTIONAL',
                                              name:major,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:minor,
                                              label:'LABEL_OPTIONAL',
                                              name:minor,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:patch,
                                              label:'LABEL_OPTIONAL',
                                              name:patch,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_INT32',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:suffix,
                                              label:'LABEL_OPTIONAL',
                                              name:suffix,
                                              number:4,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            }
                                        ],
                                  name:'Version',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:fileToGenerate,
                                              label:'LABEL_REPEATED',
                                              name:file_to_generate,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:parameter,
                                              label:'LABEL_OPTIONAL',
                                              name:parameter,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:protoFile,
                                              label:'LABEL_REPEATED',
                                              name:proto_file,
                                              number:15,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.FileDescriptorProto'
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:compilerVersion,
                                              label:'LABEL_OPTIONAL',
                                              name:compiler_version,
                                              number:3,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.compiler.Version'
                                            }
                                        ],
                                  name:'CodeGeneratorRequest',
                                  nested_type:[],
                                  reserved_range:[]
                                },
                              'DescriptorProto'{
                                  enum_type:[ 'EnumDescriptorProto'{
                                                  name:'Feature',
                                                  value:[ 'EnumValueDescriptorProto'{
                                                              name:'FEATURE_NONE',
                                                              number:0
                                                            },
                                                          'EnumValueDescriptorProto'{
                                                              name:'FEATURE_PROTO3_OPTIONAL',
                                                              number:1
                                                            }
                                                        ]
                                                }
                                            ],
                                  extension_range:[],
                                  field:[ 'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:error,
                                              label:'LABEL_OPTIONAL',
                                              name:error,
                                              number:1,
                                              options:'FieldOptions'{},
                                              type:'TYPE_STRING',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:supportedFeatures,
                                              label:'LABEL_OPTIONAL',
                                              name:supported_features,
                                              number:2,
                                              options:'FieldOptions'{},
                                              type:'TYPE_UINT64',
                                              type_name:''
                                            },
                                          'FieldDescriptorProto'{
                                              default_value:'',
                                              json_name:file,
                                              label:'LABEL_REPEATED',
                                              name:file,
                                              number:15,
                                              options:'FieldOptions'{},
                                              type:'TYPE_MESSAGE',
                                              type_name:'.google.protobuf.compiler.CodeGeneratorResponse.File'
                                            }
                                        ],
                                  name:'CodeGeneratorResponse',
                                  nested_type:[ 'DescriptorProto'{
                                                    enum_type:[],
                                                    extension_range:[],
                                                    field:[ 'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:name,
                                                                label:'LABEL_OPTIONAL',
                                                                name:name,
                                                                number:1,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:insertionPoint,
                                                                label:'LABEL_OPTIONAL',
                                                                name:insertion_point,
                                                                number:2,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:content,
                                                                label:'LABEL_OPTIONAL',
                                                                name:content,
                                                                number:15,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_STRING',
                                                                type_name:''
                                                              },
                                                            'FieldDescriptorProto'{
                                                                default_value:'',
                                                                json_name:generatedCodeInfo,
                                                                label:'LABEL_OPTIONAL',
                                                                name:generated_code_info,
                                                                number:16,
                                                                options:'FieldOptions'{},
                                                                type:'TYPE_MESSAGE',
                                                                type_name:'.google.protobuf.GeneratedCodeInfo'
                                                              }
                                                          ],
                                                    name:'File',
                                                    nested_type:[],
                                                    reserved_range:[]
                                                  }
                                              ],
                                  reserved_range:[]
                                }
                            ],
               name:'plugin.proto',
               options:'FileOptions'{
                           cc_enable_arenas:'',
                           csharp_namespace:'',
                           go_package:'google.golang.org/protobuf/types/pluginpb',
                           java_outer_classname:'PluginProtos',
                           java_package:'com.google.protobuf.compiler',
                           objc_class_prefix:'',
                           optimize_for:''
                         },
               package:'google.protobuf.compiler'
             }
         ]
  }).

end_of_file.

  % Result of expansion of the above:

:- discontiguous
    proto_package/3,
    proto_message_type/3,
    proto_field_name/4,
    proto_field_json_name/2,
    proto_field_label/2,
    proto_field_type/2,
    proto_field_type_name/2,
    proto_field_default_value/2,
    proto_field_option_packed/1,
    proto_enum_type/3,
    proto_enum_value/3.

proto_package('.google.protobuf',
              'google/protobuf/descriptor.proto',
              'FileOptions'{ cc_enable_arenas:true,
                             csharp_namespace:'Google.Protobuf.Reflection',
                             go_package:'google.golang.org/protobuf/types/descriptorpb',
                             java_outer_classname:'DescriptorProtos',
                             java_package:'com.google.protobuf',
                             objc_class_prefix:'GPB',
                             optimize_for:'SPEED'
                           }).

proto_message_type(          '.google.protobuf.FileDescriptorSet'                                 ,'.google.protobuf','FileDescriptorSet').

  proto_field_name(          '.google.protobuf.FileDescriptorSet'                                 ,1,file,'.google.protobuf.FileDescriptorSet.file').
  proto_field_json_name(     '.google.protobuf.FileDescriptorSet.file'                            ,file).
  proto_field_label(         '.google.protobuf.FileDescriptorSet.file'                            ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorSet.file'                            ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorSet.file'                            ,'.google.protobuf.FileDescriptorProto').
  proto_field_default_value( '.google.protobuf.FileDescriptorSet.file'                            ,'').

proto_message_type(          '.google.protobuf.FileDescriptorProto'                               ,'.google.protobuf','FileDescriptorProto').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,1,name,'.google.protobuf.FileDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.name'                          ,name).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.name'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.name'                          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.name'                          ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.name'                          ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,2,package,'.google.protobuf.FileDescriptorProto.package').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.package'                       ,package).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.package'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.package'                       ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.package'                       ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.package'                       ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,3,dependency,'.google.protobuf.FileDescriptorProto.dependency').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.dependency'                    ,dependency).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.dependency'                    ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.dependency'                    ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.dependency'                    ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.dependency'                    ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,10,public_dependency,'.google.protobuf.FileDescriptorProto.public_dependency').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.public_dependency'             ,publicDependency).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.public_dependency'             ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.public_dependency'             ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.public_dependency'             ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.public_dependency'             ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,11,weak_dependency,'.google.protobuf.FileDescriptorProto.weak_dependency').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.weak_dependency'               ,weakDependency).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.weak_dependency'               ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.weak_dependency'               ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.weak_dependency'               ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.weak_dependency'               ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,4,message_type,'.google.protobuf.FileDescriptorProto.message_type').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.message_type'                  ,messageType).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.message_type'                  ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.message_type'                  ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.message_type'                  ,'.google.protobuf.DescriptorProto').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.message_type'                  ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,5,enum_type,'.google.protobuf.FileDescriptorProto.enum_type').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.enum_type'                     ,enumType).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.enum_type'                     ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.enum_type'                     ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.enum_type'                     ,'.google.protobuf.EnumDescriptorProto').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.enum_type'                     ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,6,service,'.google.protobuf.FileDescriptorProto.service').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.service'                       ,service).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.service'                       ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.service'                       ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.service'                       ,'.google.protobuf.ServiceDescriptorProto').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.service'                       ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,7,extension,'.google.protobuf.FileDescriptorProto.extension').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.extension'                     ,extension).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.extension'                     ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.extension'                     ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.extension'                     ,'.google.protobuf.FieldDescriptorProto').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.extension'                     ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,8,options,'.google.protobuf.FileDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.options'                       ,options).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.options'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.options'                       ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.options'                       ,'.google.protobuf.FileOptions').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.options'                       ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,9,source_code_info,'.google.protobuf.FileDescriptorProto.source_code_info').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.source_code_info'              ,sourceCodeInfo).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.source_code_info'              ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.source_code_info'              ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.source_code_info'              ,'.google.protobuf.SourceCodeInfo').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.source_code_info'              ,'').

  proto_field_name(          '.google.protobuf.FileDescriptorProto'                               ,12,syntax,'.google.protobuf.FileDescriptorProto.syntax').
  proto_field_json_name(     '.google.protobuf.FileDescriptorProto.syntax'                        ,syntax).
  proto_field_label(         '.google.protobuf.FileDescriptorProto.syntax'                        ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileDescriptorProto.syntax'                        ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileDescriptorProto.syntax'                        ,'').
  proto_field_default_value( '.google.protobuf.FileDescriptorProto.syntax'                        ,'').

proto_message_type(          '.google.protobuf.DescriptorProto'                                   ,'.google.protobuf','DescriptorProto').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,1,name,'.google.protobuf.DescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.name'                              ,name).
  proto_field_label(         '.google.protobuf.DescriptorProto.name'                              ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.name'                              ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.name'                              ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.name'                              ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,2,field,'.google.protobuf.DescriptorProto.field').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.field'                             ,field).
  proto_field_label(         '.google.protobuf.DescriptorProto.field'                             ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.field'                             ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.field'                             ,'.google.protobuf.FieldDescriptorProto').
  proto_field_default_value( '.google.protobuf.DescriptorProto.field'                             ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,6,extension,'.google.protobuf.DescriptorProto.extension').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.extension'                         ,extension).
  proto_field_label(         '.google.protobuf.DescriptorProto.extension'                         ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.extension'                         ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.extension'                         ,'.google.protobuf.FieldDescriptorProto').
  proto_field_default_value( '.google.protobuf.DescriptorProto.extension'                         ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,3,nested_type,'.google.protobuf.DescriptorProto.nested_type').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.nested_type'                       ,nestedType).
  proto_field_label(         '.google.protobuf.DescriptorProto.nested_type'                       ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.nested_type'                       ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.nested_type'                       ,'.google.protobuf.DescriptorProto').
  proto_field_default_value( '.google.protobuf.DescriptorProto.nested_type'                       ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,4,enum_type,'.google.protobuf.DescriptorProto.enum_type').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.enum_type'                         ,enumType).
  proto_field_label(         '.google.protobuf.DescriptorProto.enum_type'                         ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.enum_type'                         ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.enum_type'                         ,'.google.protobuf.EnumDescriptorProto').
  proto_field_default_value( '.google.protobuf.DescriptorProto.enum_type'                         ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,5,extension_range,'.google.protobuf.DescriptorProto.extension_range').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.extension_range'                   ,extensionRange).
  proto_field_label(         '.google.protobuf.DescriptorProto.extension_range'                   ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.extension_range'                   ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.extension_range'                   ,'.google.protobuf.DescriptorProto.ExtensionRange').
  proto_field_default_value( '.google.protobuf.DescriptorProto.extension_range'                   ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,8,oneof_decl,'.google.protobuf.DescriptorProto.oneof_decl').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.oneof_decl'                        ,oneofDecl).
  proto_field_label(         '.google.protobuf.DescriptorProto.oneof_decl'                        ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.oneof_decl'                        ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.oneof_decl'                        ,'.google.protobuf.OneofDescriptorProto').
  proto_field_default_value( '.google.protobuf.DescriptorProto.oneof_decl'                        ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,7,options,'.google.protobuf.DescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.options'                           ,options).
  proto_field_label(         '.google.protobuf.DescriptorProto.options'                           ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.options'                           ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.options'                           ,'.google.protobuf.MessageOptions').
  proto_field_default_value( '.google.protobuf.DescriptorProto.options'                           ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,9,reserved_range,'.google.protobuf.DescriptorProto.reserved_range').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.reserved_range'                    ,reservedRange).
  proto_field_label(         '.google.protobuf.DescriptorProto.reserved_range'                    ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.reserved_range'                    ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.reserved_range'                    ,'.google.protobuf.DescriptorProto.ReservedRange').
  proto_field_default_value( '.google.protobuf.DescriptorProto.reserved_range'                    ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto'                                   ,10,reserved_name,'.google.protobuf.DescriptorProto.reserved_name').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.reserved_name'                     ,reservedName).
  proto_field_label(         '.google.protobuf.DescriptorProto.reserved_name'                     ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.DescriptorProto.reserved_name'                     ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.reserved_name'                     ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.reserved_name'                     ,'').

proto_message_type(          '.google.protobuf.DescriptorProto.ExtensionRange'                    ,'.google.protobuf.DescriptorProto','ExtensionRange').

  proto_field_name(          '.google.protobuf.DescriptorProto.ExtensionRange'                    ,1,start,'.google.protobuf.DescriptorProto.ExtensionRange.start').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.ExtensionRange.start'              ,start).
  proto_field_label(         '.google.protobuf.DescriptorProto.ExtensionRange.start'              ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.ExtensionRange.start'              ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.ExtensionRange.start'              ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.ExtensionRange.start'              ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto.ExtensionRange'                    ,2,end,'.google.protobuf.DescriptorProto.ExtensionRange.end').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.ExtensionRange.end'                ,end).
  proto_field_label(         '.google.protobuf.DescriptorProto.ExtensionRange.end'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.ExtensionRange.end'                ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.ExtensionRange.end'                ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.ExtensionRange.end'                ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto.ExtensionRange'                    ,3,options,'.google.protobuf.DescriptorProto.ExtensionRange.options').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.ExtensionRange.options'            ,options).
  proto_field_label(         '.google.protobuf.DescriptorProto.ExtensionRange.options'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.ExtensionRange.options'            ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.ExtensionRange.options'            ,'.google.protobuf.ExtensionRangeOptions').
  proto_field_default_value( '.google.protobuf.DescriptorProto.ExtensionRange.options'            ,'').

proto_message_type(          '.google.protobuf.DescriptorProto.ReservedRange'                     ,'.google.protobuf.DescriptorProto','ReservedRange').

  proto_field_name(          '.google.protobuf.DescriptorProto.ReservedRange'                     ,1,start,'.google.protobuf.DescriptorProto.ReservedRange.start').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.ReservedRange.start'               ,start).
  proto_field_label(         '.google.protobuf.DescriptorProto.ReservedRange.start'               ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.ReservedRange.start'               ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.ReservedRange.start'               ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.ReservedRange.start'               ,'').

  proto_field_name(          '.google.protobuf.DescriptorProto.ReservedRange'                     ,2,end,'.google.protobuf.DescriptorProto.ReservedRange.end').
  proto_field_json_name(     '.google.protobuf.DescriptorProto.ReservedRange.end'                 ,end).
  proto_field_label(         '.google.protobuf.DescriptorProto.ReservedRange.end'                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.DescriptorProto.ReservedRange.end'                 ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.DescriptorProto.ReservedRange.end'                 ,'').
  proto_field_default_value( '.google.protobuf.DescriptorProto.ReservedRange.end'                 ,'').

proto_message_type(          '.google.protobuf.ExtensionRangeOptions'                             ,'.google.protobuf','ExtensionRangeOptions').

  proto_field_name(          '.google.protobuf.ExtensionRangeOptions'                             ,999,uninterpreted_option,'.google.protobuf.ExtensionRangeOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.ExtensionRangeOptions.uninterpreted_option'        ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.ExtensionRangeOptions.uninterpreted_option'        ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.ExtensionRangeOptions.uninterpreted_option'        ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.ExtensionRangeOptions.uninterpreted_option'        ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.ExtensionRangeOptions.uninterpreted_option'        ,'').

proto_message_type(          '.google.protobuf.FieldDescriptorProto'                              ,'.google.protobuf','FieldDescriptorProto').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,1,name,'.google.protobuf.FieldDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.name'                         ,name).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.name'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.name'                         ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.name'                         ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.name'                         ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,3,number,'.google.protobuf.FieldDescriptorProto.number').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.number'                       ,number).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.number'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.number'                       ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.number'                       ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.number'                       ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,4,label,'.google.protobuf.FieldDescriptorProto.label').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.label'                        ,label).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.label'                        ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.label'                        ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.label'                        ,'.google.protobuf.FieldDescriptorProto.Label').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.label'                        ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,5,type,'.google.protobuf.FieldDescriptorProto.type').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.type'                         ,type).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.type'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.type'                         ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.type'                         ,'.google.protobuf.FieldDescriptorProto.Type').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.type'                         ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,6,type_name,'.google.protobuf.FieldDescriptorProto.type_name').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.type_name'                    ,typeName).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.type_name'                    ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.type_name'                    ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.type_name'                    ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.type_name'                    ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,2,extendee,'.google.protobuf.FieldDescriptorProto.extendee').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.extendee'                     ,extendee).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.extendee'                     ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.extendee'                     ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.extendee'                     ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.extendee'                     ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,7,default_value,'.google.protobuf.FieldDescriptorProto.default_value').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.default_value'                ,defaultValue).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.default_value'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.default_value'                ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.default_value'                ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.default_value'                ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,9,oneof_index,'.google.protobuf.FieldDescriptorProto.oneof_index').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.oneof_index'                  ,oneofIndex).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.oneof_index'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.oneof_index'                  ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.oneof_index'                  ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.oneof_index'                  ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,10,json_name,'.google.protobuf.FieldDescriptorProto.json_name').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.json_name'                    ,jsonName).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.json_name'                    ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.json_name'                    ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.json_name'                    ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.json_name'                    ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,8,options,'.google.protobuf.FieldDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.options'                      ,options).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.options'                      ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.options'                      ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.options'                      ,'.google.protobuf.FieldOptions').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.options'                      ,'').

  proto_field_name(          '.google.protobuf.FieldDescriptorProto'                              ,17,proto3_optional,'.google.protobuf.FieldDescriptorProto.proto3_optional').
  proto_field_json_name(     '.google.protobuf.FieldDescriptorProto.proto3_optional'              ,proto3Optional).
  proto_field_label(         '.google.protobuf.FieldDescriptorProto.proto3_optional'              ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldDescriptorProto.proto3_optional'              ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FieldDescriptorProto.proto3_optional'              ,'').
  proto_field_default_value( '.google.protobuf.FieldDescriptorProto.proto3_optional'              ,'').
  proto_enum_type(           '.google.protobuf.FieldDescriptorProto.Type'                         ,'.google.protobuf.FieldDescriptorProto','Type').
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_DOUBLE',1).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_FLOAT',2).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_INT64',3).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_UINT64',4).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_INT32',5).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_FIXED64',6).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_FIXED32',7).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_BOOL',8).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_STRING',9).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_GROUP',10).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_MESSAGE',11).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_BYTES',12).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_UINT32',13).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_ENUM',14).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_SFIXED32',15).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_SFIXED64',16).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_SINT32',17).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Type'                         ,'TYPE_SINT64',18).
  proto_enum_type(           '.google.protobuf.FieldDescriptorProto.Label'                        ,'.google.protobuf.FieldDescriptorProto','Label').
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Label'                        ,'LABEL_OPTIONAL',1).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Label'                        ,'LABEL_REQUIRED',2).
  proto_enum_value(          '.google.protobuf.FieldDescriptorProto.Label'                        ,'LABEL_REPEATED',3).

proto_message_type(          '.google.protobuf.OneofDescriptorProto'                              ,'.google.protobuf','OneofDescriptorProto').

  proto_field_name(          '.google.protobuf.OneofDescriptorProto'                              ,1,name,'.google.protobuf.OneofDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.OneofDescriptorProto.name'                         ,name).
  proto_field_label(         '.google.protobuf.OneofDescriptorProto.name'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.OneofDescriptorProto.name'                         ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.OneofDescriptorProto.name'                         ,'').
  proto_field_default_value( '.google.protobuf.OneofDescriptorProto.name'                         ,'').

  proto_field_name(          '.google.protobuf.OneofDescriptorProto'                              ,2,options,'.google.protobuf.OneofDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.OneofDescriptorProto.options'                      ,options).
  proto_field_label(         '.google.protobuf.OneofDescriptorProto.options'                      ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.OneofDescriptorProto.options'                      ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.OneofDescriptorProto.options'                      ,'.google.protobuf.OneofOptions').
  proto_field_default_value( '.google.protobuf.OneofDescriptorProto.options'                      ,'').

proto_message_type(          '.google.protobuf.EnumDescriptorProto'                               ,'.google.protobuf','EnumDescriptorProto').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto'                               ,1,name,'.google.protobuf.EnumDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.name'                          ,name).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.name'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.name'                          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.name'                          ,'').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.name'                          ,'').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto'                               ,2,value,'.google.protobuf.EnumDescriptorProto.value').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.value'                         ,value).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.value'                         ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.value'                         ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.value'                         ,'.google.protobuf.EnumValueDescriptorProto').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.value'                         ,'').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto'                               ,3,options,'.google.protobuf.EnumDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.options'                       ,options).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.options'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.options'                       ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.options'                       ,'.google.protobuf.EnumOptions').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.options'                       ,'').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto'                               ,4,reserved_range,'.google.protobuf.EnumDescriptorProto.reserved_range').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.reserved_range'                ,reservedRange).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.reserved_range'                ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.reserved_range'                ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.reserved_range'                ,'.google.protobuf.EnumDescriptorProto.EnumReservedRange').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.reserved_range'                ,'').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto'                               ,5,reserved_name,'.google.protobuf.EnumDescriptorProto.reserved_name').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.reserved_name'                 ,reservedName).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.reserved_name'                 ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.reserved_name'                 ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.reserved_name'                 ,'').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.reserved_name'                 ,'').

proto_message_type(          '.google.protobuf.EnumDescriptorProto.EnumReservedRange'             ,'.google.protobuf.EnumDescriptorProto','EnumReservedRange').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto.EnumReservedRange'             ,1,start,'.google.protobuf.EnumDescriptorProto.EnumReservedRange.start').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.EnumReservedRange.start'       ,start).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.EnumReservedRange.start'       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.EnumReservedRange.start'       ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.EnumReservedRange.start'       ,'').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.EnumReservedRange.start'       ,'').

  proto_field_name(          '.google.protobuf.EnumDescriptorProto.EnumReservedRange'             ,2,end,'.google.protobuf.EnumDescriptorProto.EnumReservedRange.end').
  proto_field_json_name(     '.google.protobuf.EnumDescriptorProto.EnumReservedRange.end'         ,end).
  proto_field_label(         '.google.protobuf.EnumDescriptorProto.EnumReservedRange.end'         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumDescriptorProto.EnumReservedRange.end'         ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.EnumDescriptorProto.EnumReservedRange.end'         ,'').
  proto_field_default_value( '.google.protobuf.EnumDescriptorProto.EnumReservedRange.end'         ,'').

proto_message_type(          '.google.protobuf.EnumValueDescriptorProto'                          ,'.google.protobuf','EnumValueDescriptorProto').

  proto_field_name(          '.google.protobuf.EnumValueDescriptorProto'                          ,1,name,'.google.protobuf.EnumValueDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.EnumValueDescriptorProto.name'                     ,name).
  proto_field_label(         '.google.protobuf.EnumValueDescriptorProto.name'                     ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumValueDescriptorProto.name'                     ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.EnumValueDescriptorProto.name'                     ,'').
  proto_field_default_value( '.google.protobuf.EnumValueDescriptorProto.name'                     ,'').

  proto_field_name(          '.google.protobuf.EnumValueDescriptorProto'                          ,2,number,'.google.protobuf.EnumValueDescriptorProto.number').
  proto_field_json_name(     '.google.protobuf.EnumValueDescriptorProto.number'                   ,number).
  proto_field_label(         '.google.protobuf.EnumValueDescriptorProto.number'                   ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumValueDescriptorProto.number'                   ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.EnumValueDescriptorProto.number'                   ,'').
  proto_field_default_value( '.google.protobuf.EnumValueDescriptorProto.number'                   ,'').

  proto_field_name(          '.google.protobuf.EnumValueDescriptorProto'                          ,3,options,'.google.protobuf.EnumValueDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.EnumValueDescriptorProto.options'                  ,options).
  proto_field_label(         '.google.protobuf.EnumValueDescriptorProto.options'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumValueDescriptorProto.options'                  ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumValueDescriptorProto.options'                  ,'.google.protobuf.EnumValueOptions').
  proto_field_default_value( '.google.protobuf.EnumValueDescriptorProto.options'                  ,'').

proto_message_type(          '.google.protobuf.ServiceDescriptorProto'                            ,'.google.protobuf','ServiceDescriptorProto').

  proto_field_name(          '.google.protobuf.ServiceDescriptorProto'                            ,1,name,'.google.protobuf.ServiceDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.ServiceDescriptorProto.name'                       ,name).
  proto_field_label(         '.google.protobuf.ServiceDescriptorProto.name'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.ServiceDescriptorProto.name'                       ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.ServiceDescriptorProto.name'                       ,'').
  proto_field_default_value( '.google.protobuf.ServiceDescriptorProto.name'                       ,'').

  proto_field_name(          '.google.protobuf.ServiceDescriptorProto'                            ,2,method,'.google.protobuf.ServiceDescriptorProto.method').
  proto_field_json_name(     '.google.protobuf.ServiceDescriptorProto.method'                     ,method).
  proto_field_label(         '.google.protobuf.ServiceDescriptorProto.method'                     ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.ServiceDescriptorProto.method'                     ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.ServiceDescriptorProto.method'                     ,'.google.protobuf.MethodDescriptorProto').
  proto_field_default_value( '.google.protobuf.ServiceDescriptorProto.method'                     ,'').

  proto_field_name(          '.google.protobuf.ServiceDescriptorProto'                            ,3,options,'.google.protobuf.ServiceDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.ServiceDescriptorProto.options'                    ,options).
  proto_field_label(         '.google.protobuf.ServiceDescriptorProto.options'                    ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.ServiceDescriptorProto.options'                    ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.ServiceDescriptorProto.options'                    ,'.google.protobuf.ServiceOptions').
  proto_field_default_value( '.google.protobuf.ServiceDescriptorProto.options'                    ,'').

proto_message_type(          '.google.protobuf.MethodDescriptorProto'                             ,'.google.protobuf','MethodDescriptorProto').

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,1,name,'.google.protobuf.MethodDescriptorProto.name').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.name'                        ,name).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.name'                        ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.name'                        ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.name'                        ,'').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.name'                        ,'').

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,2,input_type,'.google.protobuf.MethodDescriptorProto.input_type').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.input_type'                  ,inputType).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.input_type'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.input_type'                  ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.input_type'                  ,'').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.input_type'                  ,'').

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,3,output_type,'.google.protobuf.MethodDescriptorProto.output_type').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.output_type'                 ,outputType).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.output_type'                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.output_type'                 ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.output_type'                 ,'').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.output_type'                 ,'').

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,4,options,'.google.protobuf.MethodDescriptorProto.options').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.options'                     ,options).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.options'                     ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.options'                     ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.options'                     ,'.google.protobuf.MethodOptions').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.options'                     ,'').

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,5,client_streaming,'.google.protobuf.MethodDescriptorProto.client_streaming').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.client_streaming'            ,clientStreaming).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.client_streaming'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.client_streaming'            ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.client_streaming'            ,'').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.client_streaming'            ,false).

  proto_field_name(          '.google.protobuf.MethodDescriptorProto'                             ,6,server_streaming,'.google.protobuf.MethodDescriptorProto.server_streaming').
  proto_field_json_name(     '.google.protobuf.MethodDescriptorProto.server_streaming'            ,serverStreaming).
  proto_field_label(         '.google.protobuf.MethodDescriptorProto.server_streaming'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodDescriptorProto.server_streaming'            ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MethodDescriptorProto.server_streaming'            ,'').
  proto_field_default_value( '.google.protobuf.MethodDescriptorProto.server_streaming'            ,false).

proto_message_type(          '.google.protobuf.FileOptions'                                       ,'.google.protobuf','FileOptions').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,1,java_package,'.google.protobuf.FileOptions.java_package').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_package'                          ,javaPackage).
  proto_field_label(         '.google.protobuf.FileOptions.java_package'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_package'                          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_package'                          ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_package'                          ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,8,java_outer_classname,'.google.protobuf.FileOptions.java_outer_classname').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_outer_classname'                  ,javaOuterClassname).
  proto_field_label(         '.google.protobuf.FileOptions.java_outer_classname'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_outer_classname'                  ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_outer_classname'                  ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_outer_classname'                  ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,10,java_multiple_files,'.google.protobuf.FileOptions.java_multiple_files').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_multiple_files'                   ,javaMultipleFiles).
  proto_field_label(         '.google.protobuf.FileOptions.java_multiple_files'                   ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_multiple_files'                   ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_multiple_files'                   ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_multiple_files'                   ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,20,java_generate_equals_and_hash,'.google.protobuf.FileOptions.java_generate_equals_and_hash').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_generate_equals_and_hash'         ,javaGenerateEqualsAndHash).
  proto_field_label(         '.google.protobuf.FileOptions.java_generate_equals_and_hash'         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_generate_equals_and_hash'         ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_generate_equals_and_hash'         ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_generate_equals_and_hash'         ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,27,java_string_check_utf8,'.google.protobuf.FileOptions.java_string_check_utf8').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_string_check_utf8'                ,javaStringCheckUtf8).
  proto_field_label(         '.google.protobuf.FileOptions.java_string_check_utf8'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_string_check_utf8'                ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_string_check_utf8'                ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_string_check_utf8'                ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,9,optimize_for,'.google.protobuf.FileOptions.optimize_for').
  proto_field_json_name(     '.google.protobuf.FileOptions.optimize_for'                          ,optimizeFor).
  proto_field_label(         '.google.protobuf.FileOptions.optimize_for'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.optimize_for'                          ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.FileOptions.optimize_for'                          ,'.google.protobuf.FileOptions.OptimizeMode').
  proto_field_default_value( '.google.protobuf.FileOptions.optimize_for'                          ,'SPEED').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,11,go_package,'.google.protobuf.FileOptions.go_package').
  proto_field_json_name(     '.google.protobuf.FileOptions.go_package'                            ,goPackage).
  proto_field_label(         '.google.protobuf.FileOptions.go_package'                            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.go_package'                            ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.go_package'                            ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.go_package'                            ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,16,cc_generic_services,'.google.protobuf.FileOptions.cc_generic_services').
  proto_field_json_name(     '.google.protobuf.FileOptions.cc_generic_services'                   ,ccGenericServices).
  proto_field_label(         '.google.protobuf.FileOptions.cc_generic_services'                   ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.cc_generic_services'                   ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.cc_generic_services'                   ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.cc_generic_services'                   ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,17,java_generic_services,'.google.protobuf.FileOptions.java_generic_services').
  proto_field_json_name(     '.google.protobuf.FileOptions.java_generic_services'                 ,javaGenericServices).
  proto_field_label(         '.google.protobuf.FileOptions.java_generic_services'                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.java_generic_services'                 ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.java_generic_services'                 ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.java_generic_services'                 ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,18,py_generic_services,'.google.protobuf.FileOptions.py_generic_services').
  proto_field_json_name(     '.google.protobuf.FileOptions.py_generic_services'                   ,pyGenericServices).
  proto_field_label(         '.google.protobuf.FileOptions.py_generic_services'                   ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.py_generic_services'                   ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.py_generic_services'                   ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.py_generic_services'                   ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,42,php_generic_services,'.google.protobuf.FileOptions.php_generic_services').
  proto_field_json_name(     '.google.protobuf.FileOptions.php_generic_services'                  ,phpGenericServices).
  proto_field_label(         '.google.protobuf.FileOptions.php_generic_services'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.php_generic_services'                  ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.php_generic_services'                  ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.php_generic_services'                  ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,23,deprecated,'.google.protobuf.FileOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.FileOptions.deprecated'                            ,deprecated).
  proto_field_label(         '.google.protobuf.FileOptions.deprecated'                            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.deprecated'                            ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.deprecated'                            ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.deprecated'                            ,false).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,31,cc_enable_arenas,'.google.protobuf.FileOptions.cc_enable_arenas').
  proto_field_json_name(     '.google.protobuf.FileOptions.cc_enable_arenas'                      ,ccEnableArenas).
  proto_field_label(         '.google.protobuf.FileOptions.cc_enable_arenas'                      ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.cc_enable_arenas'                      ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FileOptions.cc_enable_arenas'                      ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.cc_enable_arenas'                      ,true).

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,36,objc_class_prefix,'.google.protobuf.FileOptions.objc_class_prefix').
  proto_field_json_name(     '.google.protobuf.FileOptions.objc_class_prefix'                     ,objcClassPrefix).
  proto_field_label(         '.google.protobuf.FileOptions.objc_class_prefix'                     ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.objc_class_prefix'                     ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.objc_class_prefix'                     ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.objc_class_prefix'                     ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,37,csharp_namespace,'.google.protobuf.FileOptions.csharp_namespace').
  proto_field_json_name(     '.google.protobuf.FileOptions.csharp_namespace'                      ,csharpNamespace).
  proto_field_label(         '.google.protobuf.FileOptions.csharp_namespace'                      ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.csharp_namespace'                      ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.csharp_namespace'                      ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.csharp_namespace'                      ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,39,swift_prefix,'.google.protobuf.FileOptions.swift_prefix').
  proto_field_json_name(     '.google.protobuf.FileOptions.swift_prefix'                          ,swiftPrefix).
  proto_field_label(         '.google.protobuf.FileOptions.swift_prefix'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.swift_prefix'                          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.swift_prefix'                          ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.swift_prefix'                          ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,40,php_class_prefix,'.google.protobuf.FileOptions.php_class_prefix').
  proto_field_json_name(     '.google.protobuf.FileOptions.php_class_prefix'                      ,phpClassPrefix).
  proto_field_label(         '.google.protobuf.FileOptions.php_class_prefix'                      ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.php_class_prefix'                      ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.php_class_prefix'                      ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.php_class_prefix'                      ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,41,php_namespace,'.google.protobuf.FileOptions.php_namespace').
  proto_field_json_name(     '.google.protobuf.FileOptions.php_namespace'                         ,phpNamespace).
  proto_field_label(         '.google.protobuf.FileOptions.php_namespace'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.php_namespace'                         ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.php_namespace'                         ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.php_namespace'                         ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,44,php_metadata_namespace,'.google.protobuf.FileOptions.php_metadata_namespace').
  proto_field_json_name(     '.google.protobuf.FileOptions.php_metadata_namespace'                ,phpMetadataNamespace).
  proto_field_label(         '.google.protobuf.FileOptions.php_metadata_namespace'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.php_metadata_namespace'                ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.php_metadata_namespace'                ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.php_metadata_namespace'                ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,45,ruby_package,'.google.protobuf.FileOptions.ruby_package').
  proto_field_json_name(     '.google.protobuf.FileOptions.ruby_package'                          ,rubyPackage).
  proto_field_label(         '.google.protobuf.FileOptions.ruby_package'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FileOptions.ruby_package'                          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.FileOptions.ruby_package'                          ,'').
  proto_field_default_value( '.google.protobuf.FileOptions.ruby_package'                          ,'').

  proto_field_name(          '.google.protobuf.FileOptions'                                       ,999,uninterpreted_option,'.google.protobuf.FileOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.FileOptions.uninterpreted_option'                  ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.FileOptions.uninterpreted_option'                  ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FileOptions.uninterpreted_option'                  ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FileOptions.uninterpreted_option'                  ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.FileOptions.uninterpreted_option'                  ,'').
  proto_enum_type(           '.google.protobuf.FileOptions.OptimizeMode'                          ,'.google.protobuf.FileOptions','OptimizeMode').
  proto_enum_value(          '.google.protobuf.FileOptions.OptimizeMode'                          ,'SPEED',1).
  proto_enum_value(          '.google.protobuf.FileOptions.OptimizeMode'                          ,'CODE_SIZE',2).
  proto_enum_value(          '.google.protobuf.FileOptions.OptimizeMode'                          ,'LITE_RUNTIME',3).

proto_message_type(          '.google.protobuf.MessageOptions'                                    ,'.google.protobuf','MessageOptions').

  proto_field_name(          '.google.protobuf.MessageOptions'                                    ,1,message_set_wire_format,'.google.protobuf.MessageOptions.message_set_wire_format').
  proto_field_json_name(     '.google.protobuf.MessageOptions.message_set_wire_format'            ,messageSetWireFormat).
  proto_field_label(         '.google.protobuf.MessageOptions.message_set_wire_format'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MessageOptions.message_set_wire_format'            ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MessageOptions.message_set_wire_format'            ,'').
  proto_field_default_value( '.google.protobuf.MessageOptions.message_set_wire_format'            ,false).

  proto_field_name(          '.google.protobuf.MessageOptions'                                    ,2,no_standard_descriptor_accessor,'.google.protobuf.MessageOptions.no_standard_descriptor_accessor').
  proto_field_json_name(     '.google.protobuf.MessageOptions.no_standard_descriptor_accessor'    ,noStandardDescriptorAccessor).
  proto_field_label(         '.google.protobuf.MessageOptions.no_standard_descriptor_accessor'    ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MessageOptions.no_standard_descriptor_accessor'    ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MessageOptions.no_standard_descriptor_accessor'    ,'').
  proto_field_default_value( '.google.protobuf.MessageOptions.no_standard_descriptor_accessor'    ,false).

  proto_field_name(          '.google.protobuf.MessageOptions'                                    ,3,deprecated,'.google.protobuf.MessageOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.MessageOptions.deprecated'                         ,deprecated).
  proto_field_label(         '.google.protobuf.MessageOptions.deprecated'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MessageOptions.deprecated'                         ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MessageOptions.deprecated'                         ,'').
  proto_field_default_value( '.google.protobuf.MessageOptions.deprecated'                         ,false).

  proto_field_name(          '.google.protobuf.MessageOptions'                                    ,7,map_entry,'.google.protobuf.MessageOptions.map_entry').
  proto_field_json_name(     '.google.protobuf.MessageOptions.map_entry'                          ,mapEntry).
  proto_field_label(         '.google.protobuf.MessageOptions.map_entry'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MessageOptions.map_entry'                          ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MessageOptions.map_entry'                          ,'').
  proto_field_default_value( '.google.protobuf.MessageOptions.map_entry'                          ,'').

  proto_field_name(          '.google.protobuf.MessageOptions'                                    ,999,uninterpreted_option,'.google.protobuf.MessageOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.MessageOptions.uninterpreted_option'               ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.MessageOptions.uninterpreted_option'               ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.MessageOptions.uninterpreted_option'               ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.MessageOptions.uninterpreted_option'               ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.MessageOptions.uninterpreted_option'               ,'').

proto_message_type(          '.google.protobuf.FieldOptions'                                      ,'.google.protobuf','FieldOptions').

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,1,ctype,'.google.protobuf.FieldOptions.ctype').
  proto_field_json_name(     '.google.protobuf.FieldOptions.ctype'                                ,ctype).
  proto_field_label(         '.google.protobuf.FieldOptions.ctype'                                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.ctype'                                ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.FieldOptions.ctype'                                ,'.google.protobuf.FieldOptions.CType').
  proto_field_default_value( '.google.protobuf.FieldOptions.ctype'                                ,'STRING').

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,2,packed,'.google.protobuf.FieldOptions.packed').
  proto_field_json_name(     '.google.protobuf.FieldOptions.packed'                               ,packed).
  proto_field_label(         '.google.protobuf.FieldOptions.packed'                               ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.packed'                               ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FieldOptions.packed'                               ,'').
  proto_field_default_value( '.google.protobuf.FieldOptions.packed'                               ,'').

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,6,jstype,'.google.protobuf.FieldOptions.jstype').
  proto_field_json_name(     '.google.protobuf.FieldOptions.jstype'                               ,jstype).
  proto_field_label(         '.google.protobuf.FieldOptions.jstype'                               ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.jstype'                               ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.FieldOptions.jstype'                               ,'.google.protobuf.FieldOptions.JSType').
  proto_field_default_value( '.google.protobuf.FieldOptions.jstype'                               ,'JS_NORMAL').

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,5,lazy,'.google.protobuf.FieldOptions.lazy').
  proto_field_json_name(     '.google.protobuf.FieldOptions.lazy'                                 ,lazy).
  proto_field_label(         '.google.protobuf.FieldOptions.lazy'                                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.lazy'                                 ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FieldOptions.lazy'                                 ,'').
  proto_field_default_value( '.google.protobuf.FieldOptions.lazy'                                 ,false).

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,3,deprecated,'.google.protobuf.FieldOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.FieldOptions.deprecated'                           ,deprecated).
  proto_field_label(         '.google.protobuf.FieldOptions.deprecated'                           ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.deprecated'                           ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FieldOptions.deprecated'                           ,'').
  proto_field_default_value( '.google.protobuf.FieldOptions.deprecated'                           ,false).

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,10,weak,'.google.protobuf.FieldOptions.weak').
  proto_field_json_name(     '.google.protobuf.FieldOptions.weak'                                 ,weak).
  proto_field_label(         '.google.protobuf.FieldOptions.weak'                                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.FieldOptions.weak'                                 ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.FieldOptions.weak'                                 ,'').
  proto_field_default_value( '.google.protobuf.FieldOptions.weak'                                 ,false).

  proto_field_name(          '.google.protobuf.FieldOptions'                                      ,999,uninterpreted_option,'.google.protobuf.FieldOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.FieldOptions.uninterpreted_option'                 ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.FieldOptions.uninterpreted_option'                 ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.FieldOptions.uninterpreted_option'                 ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.FieldOptions.uninterpreted_option'                 ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.FieldOptions.uninterpreted_option'                 ,'').
  proto_enum_type(           '.google.protobuf.FieldOptions.CType'                                ,'.google.protobuf.FieldOptions','CType').
  proto_enum_value(          '.google.protobuf.FieldOptions.CType'                                ,'STRING',0).
  proto_enum_value(          '.google.protobuf.FieldOptions.CType'                                ,'CORD',1).
  proto_enum_value(          '.google.protobuf.FieldOptions.CType'                                ,'STRING_PIECE',2).
  proto_enum_type(           '.google.protobuf.FieldOptions.JSType'                               ,'.google.protobuf.FieldOptions','JSType').
  proto_enum_value(          '.google.protobuf.FieldOptions.JSType'                               ,'JS_NORMAL',0).
  proto_enum_value(          '.google.protobuf.FieldOptions.JSType'                               ,'JS_STRING',1).
  proto_enum_value(          '.google.protobuf.FieldOptions.JSType'                               ,'JS_NUMBER',2).

proto_message_type(          '.google.protobuf.OneofOptions'                                      ,'.google.protobuf','OneofOptions').

  proto_field_name(          '.google.protobuf.OneofOptions'                                      ,999,uninterpreted_option,'.google.protobuf.OneofOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.OneofOptions.uninterpreted_option'                 ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.OneofOptions.uninterpreted_option'                 ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.OneofOptions.uninterpreted_option'                 ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.OneofOptions.uninterpreted_option'                 ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.OneofOptions.uninterpreted_option'                 ,'').

proto_message_type(          '.google.protobuf.EnumOptions'                                       ,'.google.protobuf','EnumOptions').

  proto_field_name(          '.google.protobuf.EnumOptions'                                       ,2,allow_alias,'.google.protobuf.EnumOptions.allow_alias').
  proto_field_json_name(     '.google.protobuf.EnumOptions.allow_alias'                           ,allowAlias).
  proto_field_label(         '.google.protobuf.EnumOptions.allow_alias'                           ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumOptions.allow_alias'                           ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.EnumOptions.allow_alias'                           ,'').
  proto_field_default_value( '.google.protobuf.EnumOptions.allow_alias'                           ,'').

  proto_field_name(          '.google.protobuf.EnumOptions'                                       ,3,deprecated,'.google.protobuf.EnumOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.EnumOptions.deprecated'                            ,deprecated).
  proto_field_label(         '.google.protobuf.EnumOptions.deprecated'                            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumOptions.deprecated'                            ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.EnumOptions.deprecated'                            ,'').
  proto_field_default_value( '.google.protobuf.EnumOptions.deprecated'                            ,false).

  proto_field_name(          '.google.protobuf.EnumOptions'                                       ,999,uninterpreted_option,'.google.protobuf.EnumOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.EnumOptions.uninterpreted_option'                  ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.EnumOptions.uninterpreted_option'                  ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.EnumOptions.uninterpreted_option'                  ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumOptions.uninterpreted_option'                  ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.EnumOptions.uninterpreted_option'                  ,'').

proto_message_type(          '.google.protobuf.EnumValueOptions'                                  ,'.google.protobuf','EnumValueOptions').

  proto_field_name(          '.google.protobuf.EnumValueOptions'                                  ,1,deprecated,'.google.protobuf.EnumValueOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.EnumValueOptions.deprecated'                       ,deprecated).
  proto_field_label(         '.google.protobuf.EnumValueOptions.deprecated'                       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.EnumValueOptions.deprecated'                       ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.EnumValueOptions.deprecated'                       ,'').
  proto_field_default_value( '.google.protobuf.EnumValueOptions.deprecated'                       ,false).

  proto_field_name(          '.google.protobuf.EnumValueOptions'                                  ,999,uninterpreted_option,'.google.protobuf.EnumValueOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.EnumValueOptions.uninterpreted_option'             ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.EnumValueOptions.uninterpreted_option'             ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.EnumValueOptions.uninterpreted_option'             ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.EnumValueOptions.uninterpreted_option'             ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.EnumValueOptions.uninterpreted_option'             ,'').

proto_message_type(          '.google.protobuf.ServiceOptions'                                    ,'.google.protobuf','ServiceOptions').

  proto_field_name(          '.google.protobuf.ServiceOptions'                                    ,33,deprecated,'.google.protobuf.ServiceOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.ServiceOptions.deprecated'                         ,deprecated).
  proto_field_label(         '.google.protobuf.ServiceOptions.deprecated'                         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.ServiceOptions.deprecated'                         ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.ServiceOptions.deprecated'                         ,'').
  proto_field_default_value( '.google.protobuf.ServiceOptions.deprecated'                         ,false).

  proto_field_name(          '.google.protobuf.ServiceOptions'                                    ,999,uninterpreted_option,'.google.protobuf.ServiceOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.ServiceOptions.uninterpreted_option'               ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.ServiceOptions.uninterpreted_option'               ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.ServiceOptions.uninterpreted_option'               ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.ServiceOptions.uninterpreted_option'               ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.ServiceOptions.uninterpreted_option'               ,'').

proto_message_type(          '.google.protobuf.MethodOptions'                                     ,'.google.protobuf','MethodOptions').

  proto_field_name(          '.google.protobuf.MethodOptions'                                     ,33,deprecated,'.google.protobuf.MethodOptions.deprecated').
  proto_field_json_name(     '.google.protobuf.MethodOptions.deprecated'                          ,deprecated).
  proto_field_label(         '.google.protobuf.MethodOptions.deprecated'                          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodOptions.deprecated'                          ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.MethodOptions.deprecated'                          ,'').
  proto_field_default_value( '.google.protobuf.MethodOptions.deprecated'                          ,false).

  proto_field_name(          '.google.protobuf.MethodOptions'                                     ,34,idempotency_level,'.google.protobuf.MethodOptions.idempotency_level').
  proto_field_json_name(     '.google.protobuf.MethodOptions.idempotency_level'                   ,idempotencyLevel).
  proto_field_label(         '.google.protobuf.MethodOptions.idempotency_level'                   ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.MethodOptions.idempotency_level'                   ,'TYPE_ENUM').
  proto_field_type_name(     '.google.protobuf.MethodOptions.idempotency_level'                   ,'.google.protobuf.MethodOptions.IdempotencyLevel').
  proto_field_default_value( '.google.protobuf.MethodOptions.idempotency_level'                   ,'IDEMPOTENCY_UNKNOWN').

  proto_field_name(          '.google.protobuf.MethodOptions'                                     ,999,uninterpreted_option,'.google.protobuf.MethodOptions.uninterpreted_option').
  proto_field_json_name(     '.google.protobuf.MethodOptions.uninterpreted_option'                ,uninterpretedOption).
  proto_field_label(         '.google.protobuf.MethodOptions.uninterpreted_option'                ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.MethodOptions.uninterpreted_option'                ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.MethodOptions.uninterpreted_option'                ,'.google.protobuf.UninterpretedOption').
  proto_field_default_value( '.google.protobuf.MethodOptions.uninterpreted_option'                ,'').
  proto_enum_type(           '.google.protobuf.MethodOptions.IdempotencyLevel'                    ,'.google.protobuf.MethodOptions','IdempotencyLevel').
  proto_enum_value(          '.google.protobuf.MethodOptions.IdempotencyLevel'                    ,'IDEMPOTENCY_UNKNOWN',0).
  proto_enum_value(          '.google.protobuf.MethodOptions.IdempotencyLevel'                    ,'NO_SIDE_EFFECTS',1).
  proto_enum_value(          '.google.protobuf.MethodOptions.IdempotencyLevel'                    ,'IDEMPOTENT',2).

proto_message_type(          '.google.protobuf.UninterpretedOption'                               ,'.google.protobuf','UninterpretedOption').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,2,name,'.google.protobuf.UninterpretedOption.name').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.name'                          ,name).
  proto_field_label(         '.google.protobuf.UninterpretedOption.name'                          ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.UninterpretedOption.name'                          ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.name'                          ,'.google.protobuf.UninterpretedOption.NamePart').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.name'                          ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,3,identifier_value,'.google.protobuf.UninterpretedOption.identifier_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.identifier_value'              ,identifierValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.identifier_value'              ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.identifier_value'              ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.identifier_value'              ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.identifier_value'              ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,4,positive_int_value,'.google.protobuf.UninterpretedOption.positive_int_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.positive_int_value'            ,positiveIntValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.positive_int_value'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.positive_int_value'            ,'TYPE_UINT64').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.positive_int_value'            ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.positive_int_value'            ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,5,negative_int_value,'.google.protobuf.UninterpretedOption.negative_int_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.negative_int_value'            ,negativeIntValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.negative_int_value'            ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.negative_int_value'            ,'TYPE_INT64').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.negative_int_value'            ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.negative_int_value'            ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,6,double_value,'.google.protobuf.UninterpretedOption.double_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.double_value'                  ,doubleValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.double_value'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.double_value'                  ,'TYPE_DOUBLE').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.double_value'                  ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.double_value'                  ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,7,string_value,'.google.protobuf.UninterpretedOption.string_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.string_value'                  ,stringValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.string_value'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.string_value'                  ,'TYPE_BYTES').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.string_value'                  ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.string_value'                  ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption'                               ,8,aggregate_value,'.google.protobuf.UninterpretedOption.aggregate_value').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.aggregate_value'               ,aggregateValue).
  proto_field_label(         '.google.protobuf.UninterpretedOption.aggregate_value'               ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.UninterpretedOption.aggregate_value'               ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.aggregate_value'               ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.aggregate_value'               ,'').

proto_message_type(          '.google.protobuf.UninterpretedOption.NamePart'                      ,'.google.protobuf.UninterpretedOption','NamePart').

  proto_field_name(          '.google.protobuf.UninterpretedOption.NamePart'                      ,1,name_part,'.google.protobuf.UninterpretedOption.NamePart.name_part').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.NamePart.name_part'            ,namePart).
  proto_field_label(         '.google.protobuf.UninterpretedOption.NamePart.name_part'            ,'LABEL_REQUIRED').
  proto_field_type(          '.google.protobuf.UninterpretedOption.NamePart.name_part'            ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.NamePart.name_part'            ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.NamePart.name_part'            ,'').

  proto_field_name(          '.google.protobuf.UninterpretedOption.NamePart'                      ,2,is_extension,'.google.protobuf.UninterpretedOption.NamePart.is_extension').
  proto_field_json_name(     '.google.protobuf.UninterpretedOption.NamePart.is_extension'         ,isExtension).
  proto_field_label(         '.google.protobuf.UninterpretedOption.NamePart.is_extension'         ,'LABEL_REQUIRED').
  proto_field_type(          '.google.protobuf.UninterpretedOption.NamePart.is_extension'         ,'TYPE_BOOL').
  proto_field_type_name(     '.google.protobuf.UninterpretedOption.NamePart.is_extension'         ,'').
  proto_field_default_value( '.google.protobuf.UninterpretedOption.NamePart.is_extension'         ,'').

proto_message_type(          '.google.protobuf.SourceCodeInfo'                                    ,'.google.protobuf','SourceCodeInfo').

  proto_field_name(          '.google.protobuf.SourceCodeInfo'                                    ,1,location,'.google.protobuf.SourceCodeInfo.location').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.location'                           ,location).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.location'                           ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.location'                           ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.location'                           ,'.google.protobuf.SourceCodeInfo.Location').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.location'                           ,'').

proto_message_type(          '.google.protobuf.SourceCodeInfo.Location'                           ,'.google.protobuf.SourceCodeInfo','Location').

  proto_field_name(          '.google.protobuf.SourceCodeInfo.Location'                           ,1,path,'.google.protobuf.SourceCodeInfo.Location.path').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.Location.path'                      ,path).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.Location.path'                      ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.Location.path'                      ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.Location.path'                      ,'').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.Location.path'                      ,'').
  proto_field_option_packed( '.google.protobuf.SourceCodeInfo.Location.path').

  proto_field_name(          '.google.protobuf.SourceCodeInfo.Location'                           ,2,span,'.google.protobuf.SourceCodeInfo.Location.span').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.Location.span'                      ,span).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.Location.span'                      ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.Location.span'                      ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.Location.span'                      ,'').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.Location.span'                      ,'').
  proto_field_option_packed( '.google.protobuf.SourceCodeInfo.Location.span').

  proto_field_name(          '.google.protobuf.SourceCodeInfo.Location'                           ,3,leading_comments,'.google.protobuf.SourceCodeInfo.Location.leading_comments').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.Location.leading_comments'          ,leadingComments).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.Location.leading_comments'          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.Location.leading_comments'          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.Location.leading_comments'          ,'').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.Location.leading_comments'          ,'').

  proto_field_name(          '.google.protobuf.SourceCodeInfo.Location'                           ,4,trailing_comments,'.google.protobuf.SourceCodeInfo.Location.trailing_comments').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.Location.trailing_comments'         ,trailingComments).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.Location.trailing_comments'         ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.Location.trailing_comments'         ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.Location.trailing_comments'         ,'').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.Location.trailing_comments'         ,'').

  proto_field_name(          '.google.protobuf.SourceCodeInfo.Location'                           ,6,leading_detached_comments,'.google.protobuf.SourceCodeInfo.Location.leading_detached_comments').
  proto_field_json_name(     '.google.protobuf.SourceCodeInfo.Location.leading_detached_comments' ,leadingDetachedComments).
  proto_field_label(         '.google.protobuf.SourceCodeInfo.Location.leading_detached_comments' ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.SourceCodeInfo.Location.leading_detached_comments' ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.SourceCodeInfo.Location.leading_detached_comments' ,'').
  proto_field_default_value( '.google.protobuf.SourceCodeInfo.Location.leading_detached_comments' ,'').

proto_message_type(          '.google.protobuf.GeneratedCodeInfo'                                 ,'.google.protobuf','GeneratedCodeInfo').

  proto_field_name(          '.google.protobuf.GeneratedCodeInfo'                                 ,1,annotation,'.google.protobuf.GeneratedCodeInfo.annotation').
  proto_field_json_name(     '.google.protobuf.GeneratedCodeInfo.annotation'                      ,annotation).
  proto_field_label(         '.google.protobuf.GeneratedCodeInfo.annotation'                      ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.GeneratedCodeInfo.annotation'                      ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.GeneratedCodeInfo.annotation'                      ,'.google.protobuf.GeneratedCodeInfo.Annotation').
  proto_field_default_value( '.google.protobuf.GeneratedCodeInfo.annotation'                      ,'').

proto_message_type(          '.google.protobuf.GeneratedCodeInfo.Annotation'                      ,'.google.protobuf.GeneratedCodeInfo','Annotation').

  proto_field_name(          '.google.protobuf.GeneratedCodeInfo.Annotation'                      ,1,path,'.google.protobuf.GeneratedCodeInfo.Annotation.path').
  proto_field_json_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.path'                 ,path).
  proto_field_label(         '.google.protobuf.GeneratedCodeInfo.Annotation.path'                 ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.GeneratedCodeInfo.Annotation.path'                 ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.path'                 ,'').
  proto_field_default_value( '.google.protobuf.GeneratedCodeInfo.Annotation.path'                 ,'').
  proto_field_option_packed( '.google.protobuf.GeneratedCodeInfo.Annotation.path').

  proto_field_name(          '.google.protobuf.GeneratedCodeInfo.Annotation'                      ,2,source_file,'.google.protobuf.GeneratedCodeInfo.Annotation.source_file').
  proto_field_json_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.source_file'          ,sourceFile).
  proto_field_label(         '.google.protobuf.GeneratedCodeInfo.Annotation.source_file'          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.GeneratedCodeInfo.Annotation.source_file'          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.source_file'          ,'').
  proto_field_default_value( '.google.protobuf.GeneratedCodeInfo.Annotation.source_file'          ,'').

  proto_field_name(          '.google.protobuf.GeneratedCodeInfo.Annotation'                      ,3,begin,'.google.protobuf.GeneratedCodeInfo.Annotation.begin').
  proto_field_json_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.begin'                ,begin).
  proto_field_label(         '.google.protobuf.GeneratedCodeInfo.Annotation.begin'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.GeneratedCodeInfo.Annotation.begin'                ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.begin'                ,'').
  proto_field_default_value( '.google.protobuf.GeneratedCodeInfo.Annotation.begin'                ,'').

  proto_field_name(          '.google.protobuf.GeneratedCodeInfo.Annotation'                      ,4,end,'.google.protobuf.GeneratedCodeInfo.Annotation.end').
  proto_field_json_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.end'                  ,end).
  proto_field_label(         '.google.protobuf.GeneratedCodeInfo.Annotation.end'                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.GeneratedCodeInfo.Annotation.end'                  ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.GeneratedCodeInfo.Annotation.end'                  ,'').
  proto_field_default_value( '.google.protobuf.GeneratedCodeInfo.Annotation.end'                  ,'').

proto_package('.google.protobuf.compiler',
              'plugin.proto',
              'FileOptions'{ cc_enable_arenas:'',
                             csharp_namespace:'',
                             go_package:'google.golang.org/protobuf/types/pluginpb',
                             java_outer_classname:'PluginProtos',
                             java_package:'com.google.protobuf.compiler',
                             objc_class_prefix:'',
                             optimize_for:''
                           }).

proto_message_type(          '.google.protobuf.compiler.Version'                                        ,'.google.protobuf.compiler','Version').

  proto_field_name(          '.google.protobuf.compiler.Version'                                        ,1,major,'.google.protobuf.compiler.Version.major').
  proto_field_json_name(     '.google.protobuf.compiler.Version.major'                                  ,major).
  proto_field_label(         '.google.protobuf.compiler.Version.major'                                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.Version.major'                                  ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.compiler.Version.major'                                  ,'').
  proto_field_default_value( '.google.protobuf.compiler.Version.major'                                  ,'').

  proto_field_name(          '.google.protobuf.compiler.Version'                                        ,2,minor,'.google.protobuf.compiler.Version.minor').
  proto_field_json_name(     '.google.protobuf.compiler.Version.minor'                                  ,minor).
  proto_field_label(         '.google.protobuf.compiler.Version.minor'                                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.Version.minor'                                  ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.compiler.Version.minor'                                  ,'').
  proto_field_default_value( '.google.protobuf.compiler.Version.minor'                                  ,'').

  proto_field_name(          '.google.protobuf.compiler.Version'                                        ,3,patch,'.google.protobuf.compiler.Version.patch').
  proto_field_json_name(     '.google.protobuf.compiler.Version.patch'                                  ,patch).
  proto_field_label(         '.google.protobuf.compiler.Version.patch'                                  ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.Version.patch'                                  ,'TYPE_INT32').
  proto_field_type_name(     '.google.protobuf.compiler.Version.patch'                                  ,'').
  proto_field_default_value( '.google.protobuf.compiler.Version.patch'                                  ,'').

  proto_field_name(          '.google.protobuf.compiler.Version'                                        ,4,suffix,'.google.protobuf.compiler.Version.suffix').
  proto_field_json_name(     '.google.protobuf.compiler.Version.suffix'                                 ,suffix).
  proto_field_label(         '.google.protobuf.compiler.Version.suffix'                                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.Version.suffix'                                 ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.Version.suffix'                                 ,'').
  proto_field_default_value( '.google.protobuf.compiler.Version.suffix'                                 ,'').

proto_message_type(          '.google.protobuf.compiler.CodeGeneratorRequest'                           ,'.google.protobuf.compiler','CodeGeneratorRequest').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorRequest'                           ,1,file_to_generate,'.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate'          ,fileToGenerate).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate'          ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate'          ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate'          ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorRequest.file_to_generate'          ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorRequest'                           ,2,parameter,'.google.protobuf.compiler.CodeGeneratorRequest.parameter').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorRequest.parameter'                 ,parameter).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorRequest.parameter'                 ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorRequest.parameter'                 ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorRequest.parameter'                 ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorRequest.parameter'                 ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorRequest'                           ,15,proto_file,'.google.protobuf.compiler.CodeGeneratorRequest.proto_file').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorRequest.proto_file'                ,protoFile).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorRequest.proto_file'                ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorRequest.proto_file'                ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorRequest.proto_file'                ,'.google.protobuf.FileDescriptorProto').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorRequest.proto_file'                ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorRequest'                           ,3,compiler_version,'.google.protobuf.compiler.CodeGeneratorRequest.compiler_version').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorRequest.compiler_version'          ,compilerVersion).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorRequest.compiler_version'          ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorRequest.compiler_version'          ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorRequest.compiler_version'          ,'.google.protobuf.compiler.Version').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorRequest.compiler_version'          ,'').

proto_message_type(          '.google.protobuf.compiler.CodeGeneratorResponse'                          ,'.google.protobuf.compiler','CodeGeneratorResponse').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse'                          ,1,error,'.google.protobuf.compiler.CodeGeneratorResponse.error').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.error'                    ,error).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.error'                    ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.error'                    ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.error'                    ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.error'                    ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse'                          ,2,supported_features,'.google.protobuf.compiler.CodeGeneratorResponse.supported_features').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.supported_features'       ,supportedFeatures).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.supported_features'       ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.supported_features'       ,'TYPE_UINT64').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.supported_features'       ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.supported_features'       ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse'                          ,15,file,'.google.protobuf.compiler.CodeGeneratorResponse.file').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.file'                     ,file).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.file'                     ,'LABEL_REPEATED').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.file'                     ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.file'                     ,'.google.protobuf.compiler.CodeGeneratorResponse.File').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.file'                     ,'').

proto_message_type(          '.google.protobuf.compiler.CodeGeneratorResponse.File'                     ,'.google.protobuf.compiler.CodeGeneratorResponse','File').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse.File'                     ,1,name,'.google.protobuf.compiler.CodeGeneratorResponse.File.name').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.name'                ,name).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.File.name'                ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.File.name'                ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.name'                ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.File.name'                ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse.File'                     ,2,insertion_point,'.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point'     ,insertionPoint).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point'     ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point'     ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point'     ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point'     ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse.File'                     ,15,content,'.google.protobuf.compiler.CodeGeneratorResponse.File.content').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.content'             ,content).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.File.content'             ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.File.content'             ,'TYPE_STRING').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.content'             ,'').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.File.content'             ,'').

  proto_field_name(          '.google.protobuf.compiler.CodeGeneratorResponse.File'                     ,
		   16                                                                                   ,
		   generated_code_info                                                                  ,
                             '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info').
  proto_field_json_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info' ,generatedCodeInfo).
  proto_field_label(         '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info' ,'LABEL_OPTIONAL').
  proto_field_type(          '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info' ,'TYPE_MESSAGE').
  proto_field_type_name(     '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info' ,'.google.protobuf.GeneratedCodeInfo').
  proto_field_default_value( '.google.protobuf.compiler.CodeGeneratorResponse.File.generated_code_info' ,'').
  proto_enum_type(           '.google.protobuf.compiler.CodeGeneratorResponse.Feature'                  ,'.google.protobuf.compiler.CodeGeneratorResponse','Feature').
  proto_enum_value(          '.google.protobuf.compiler.CodeGeneratorResponse.Feature'                  ,'FEATURE_NONE',0).
  proto_enum_value(          '.google.protobuf.compiler.CodeGeneratorResponse.Feature'                  ,'FEATURE_PROTO3_OPTIONAL',1).

end_of_file.

