# Google's Protocol Buffers {#protobufs-main}

## Overview {#protobufs-overview}

Protocol  Buffers ("protobufs")
are  Google's    language-neutral,  platform-neutral,
extensible mechanism for serializing structured data   -- think XML, but
smaller, faster, and simpler. You define how   you  want your data to be
structured once. This takes the form of   a  template that describes the
data structure. You use this template to encode your data structure into
wire-streams that may be sent-to or read-from your peers. The underlying
wire stream is platform  independent,  lossless,   and  may  be  used to
interwork with a variety of  languages   and  systems regardless of word
size  or  endianness.  Techniques  exist  to  safely  extend  your  data
structure without breaking deployed programs   that are compiled against
the "old" format.

See https://developers.google.com/protocol-buffers

The idea behind Google's  Protocol  Buffers   is  that  you  define your
structured messages using a  domain-specific   language.  This takes the
form of a ".proto" source file. You   pass  this file through a Google
provided tool that generates source code for a target language, creating
an interpreter that can encode/decode  your   structured  data. You then
compile and build  this  interpreter   into  your  application  program.
Depending on the platform, the underlying runtime support is provided by
a Google supplied library that is also bound into your program.

## Processing protobufs with Prolog {#protobufs-processing-with-prolog}

There are two ways you can use protobufs in Prolog: with a compiled
".proto" file and protobuf_parse_from_codes/3 and
protobuf_serialize_to_codes/3; or with a lower-level interface
protobuf_message/2, which allows you to define your own
domain-specific language for parsing and serliazing protobufs.

## protoc {#protobufs-protoc}

A protobuf ".proto" file can be processed by the protobuf compiler
(=protoc=), using a Prolog specific plugin. You can do this by either
adding =|/usr/lib/swi-prolog/library/protobufs|= to your =PATH= or by
specifying the option
=|--plugin=protoc-gen-swipl=/usr/lib/swi-prolog/library/protobufs/protoc-gen-swipl|=.
You specify where the generated files go with the =|--swipl_out|=
option, which must be an existing directory.

When using =protoc=, it's important to specify the =|--protopath|= (or
=|-I|=) and files correctly. The idea of =protopath= is that it
gives a list of source "roots", and the files are specified relative
to that. If you want to include the current directory, you must also
specify it (e.g., =|protoc -I. swipl_out=. foo.proto|=).
For example, when bootstrapping the "swipl" plugin, these are
used:
~~~{.sh}
protoc -I/usr/include --swipl_out=gen_pb google/include/descriptor.proto google/include/compiler/plugin.proto
~~~
which creates these files:
~~~
gen_pb/google/protobuf/descriptor_pb.pl
gen_pb/google/protobuf/compiler/plugin_pb.pl
~~~

The =|plugin_pb|= is used by:
~~~{.pl}
:- use_module(gen_pb/google/protobuf/compiler/plugin_pb)
~~~
which has this (import is relative to the current module):
~~~{.pl}
:- use_module('../descriptor_pb').
~~~

Each =X.proto= file generates a =X_pb.pl= file in the directory
specified by =|--swipl_out|=. The file contains a module name =X=,
some debugging information, and meta-data facts that go into the
=protobufs= module (all the facts start with "=|proto_meta_|=") --
protobuf_parse_from_codes/3 uses these facts to parse the wire form of
the message into a Prolog term and protobuf_serialize_to_codes/3 uses
them to serialize the data to wire form.

The generated code does not rely on any Google-supplied code.

You must compile all the ".proto" files separately but you only need
to load the top-level generated file -- it contains the necessary load
directives for things that it uses.
You can find out the dependencies for a .proto file by running
=|PATH="$PATH:/usr/lib/swipl/library/protobufs" protoc -I... --dependency_out=FILE --swipl_out=. SRC.proto|=

### protobuf_serialize_to_codes/3 {#protobufs-serialize-to-codes}

The Prolog term corresponding to a protobuf =message= is a
[dict](</pldoc/man?section=bidicts>), with the keys corresponding to
the field names in the =message= (the dict tag is treated as a comment).
Repeated fields are represented as lists;
enums are looked up and converted to atoms; bools are represented by
=false= and =true=; strings are represented by Prolog strings or atoms;
bytes are represented by lists of codes.

TODO: Add an option to omit default values (this is the =proto3=
behavior).

When serializing, the dict tag is treated as a comment and is ignored.
So, you can use any dict tags when creating data for output. For
example, both of these will generate the same output:
~~~{.pl}
protobuf_serialize_to_codes(_{people:[_{id:1234,name:"John Doe"}]}, 'tutorial.AddressBook', WireCodes).
protobuf_serialize_to_codes('tutorial.AddressBook'{people:['tutorial.Person'{name:"John Doe",id:1234}]}, 'tutorial.AddressBook', WireCodes).
~~~

NOTE: if the wire codes can't be parsed, protobuf_parse_from_codes/3
fails.  One common cause is if you give an incorrect field name. Typically, this
shows up in a call to protobufs:field_segment/3, when
protobufs:proto_meta_field_name/4 fails.

### protobuf_parse_from_codes/3 {#protobufs-parse-from-codes}

This is the inverse of protobuf_serialize_to_codes/3 -- it takes a
wire stream (list of codes) and creates a
[dict](</pldoc/man?section=bidicts>).  The dict tags are the fully
qualified names of the messages.  Repeated fields that aren't in the
wire stream get set to the value =|[]|=; other fields that aren't in
the wire stream get their default value (typically the empty string or
zero, depending on type). Embedded messages and groups are omitted if
not in the wire stream; you can test for their presence using
get_dict/3.
Enums are looked up and converted to atoms; bools are represented by
=false= and =true=; strings are represented by Prolog strings (not atoms);
bytes are represented by lists of codes.

There is no mechanism for determining whether a field was
in the wire stream or not (that is, there is no equivalent of the
Python implementation's =|HasField|=).

The "oneof" feature causes a slightly different behavior.
Only the field that's in the wire stream gets set; the other fields
are omitted. And if none of the fields in the "oneof" are set, then
none of the fields appears. You can check which field is set by
using get_dict/3.

Currently, there is no special support for the protobuf "map" feature.
It is treated as an ordinary message field.
The convenience predicates protobuf_field_is_map/3 and protobuf_map_pairs/3
can be used to convert between a "map" field and a key-value list, which
gives you the freedom to use any kind of association list for the map.
See also [Issue #12](https://github.com/SWI-Prolog/contrib-protobufs/issues/12)
For example:
~~~{.c}
message MapMessage {
  map<string, sint64> number_ints = 5;
}
~~~
is treated as if it is
~~~{.c}
message MapMessage {
  message KeyValue {
    optional string  Key = 1;
    optional sint64  Value = 2;
  }
  repeated KeyValue number_ints = 5;
}
~~~
You can handle this on input by
~~~{.pl}
protobuf_parse_from_codes(WireCodes, 'MapMessage', Term),
protobuf_map_pairs(Term.number_ints, _, Pairs).
~~~
and on output by
~~~{.pl}
protobuf_map_pairs(TermNnumberInts, _, Pairs),
protobuf_serialize_to_codes(_{number_ints:TermNumberInts}, WireCodes).
~~~

### addressbook example {#protobufs-addressbook-example}

The Google documentation has a tutorial example of a simple
addressbook:
https://developers.google.com/protocol-buffers/docs/tutorials The
Prolog equivalent is in
=|/usr/lib/swi-prolog/oc/packages/examples/protobufs/interop/addressbook.pl|=
and you can run it by =|make run_addressbook|=, which will run =|protoc|=
to generate the _pb.pl files and then run the example. The resulting file
is =|addressbook.wire|=.

## The low-level SWI-Prolog Implementation {#protobufs-swipl}

For most users, protobuf_serialize_to_codes/3 and
protobuf_parse_from_codes/3 suffice. However, if you need greater
control, or wish to define your own domain-specific language that maps
to protobufs, you can use protobuf_message/2.

The wire stream interpreter is  embodied in the form of a
Definite Clause Grammar (DCG).  It  has   a  small  underlying C-support
library that loads when the  Prolog   module  loads. This implementation
does not depend on any code that is  provided by Google and thus is not
bound by its license terms.

On the Prolog side, you  define  your   message  template  as  a list of
predefined Prolog terms that correspond to  production rules in the DCG.
The  process  is  not  unlike  specifiying   the  format  of  a  regular
expression. To encode a message, =X=, to   wire-stream,  =Y=, you pass a
grounded template, =X=, and a variable,   =Y=, to protobuf_message/2. To
decode a wire-stream, =Y=, to  template,   =X=,  you  pass an ungrounded
template,  =X=,  along   with   a    grounded   wire-stream,   =Y=,   to
protobuf_message/2. The interpreter will unify  the unbound variables in
the template with values decoded from the wire-stream.

An example template is:
```prolog
protobuf([
        unsigned(1, 100),
        string(2, "abcd"),
        repeated(3, atom([foo, bar])),
        boolean(4, true),
        embedded(5, protobuf([integer(1, -666), string(2, "negative 666")])),
        repeated(6, embedded([
            protobuf([integer(1, 1234), string(2, "onetwothreefour")]),
            protobuf([integer(1, 2222), string(2, "four twos")])])),
        repeated(7, integer([1,2,3,4])),
        packed(8, integer([5,6,7,8]))
    ])
```

This corresponds to a message created with this .proto definition
(using proto2 syntax):

```
syntax = "proto2";
package my.protobuf;
message SomeMessage {
  optional int32 first = 1;  // example template also works with int64, uint32, uint64
  optional string second = 2;
  repeated string third = 3;
  optional bool fourth = 4;
  message NestedMessage {
    optional sint32 value = 1;
    optional string text = 2;
  }
  optional NestedMessage fifth = 5;
  repeated NestedMessage sixth = 6;
  repeated sint32 seventh = 7;
  repeated sint32 eighth = 8 [packed=true];
}
```

The wire format message can be displayed:

```
$ protoc --decode=my.protobuf.SomeMessage some_message.proto <some_message.wire
first: 100
second: "abcd"
third: "foo"
third: "bar"
fourth: true
fifth {
  value: -666
  text: "negative 666"
}
sixth {
  value: 1234
  text: "onetwothreefour"
}
sixth {
  value: 2222
  text: "four twos"
}
seventh: 1
seventh: 2
seventh: 3
seventh: 4
eighth: 100
eighth: -200
eighth: 1000
```

and the actual message would be created in Python by code similar to this:

```python
import some_message_pb2

msg = some_message_pb2.SomeMessage()
msg.first = 100
msg.second = "abcd"
msg.third[:] = ["foo", "bar"]
msg.fourth = True
msg.fifth.value = -666
msg.fifth.text = "negative 666"

m1 = msg.sixth.add()
m1.value = 1234
m1.text = "onetwothreefour"
msg.sixth.append(msg.NestedMessage(value=2222, text="four twos"))
msg.seventh.extend([1,2,3,4])
msg.eighth.extend([100,-200,1000])
```

or

```python
msg2 = some_message_pb2.SomeMessage(
    first = 100,
    second = "abcd",
    third = ["foo", "bar"],
    fourth = True,
    fifth = some_message_pb2.SomeMessage.NestedMessage(value=-666, text="negative 666"),
    sixth = [some_message_pb2.SomeMessage.NestedMessage(value=1234, text="onetwothreefour"),
             some_message_pb2.SomeMessage.NestedMessage(value=2222, text="four twos")],
    seventh = [1,2,3,4],
    eighth = [100,-200,1000],
    )
```

Note that the fields can be in any order (they are disambiguated by
their tags) and if there is no value for a field, it would be simply
omitted in the template. The field names and message names can be
changed without any change to the wire format.

### Wiretypes {#protobufs-wire-types}

The wire-stream consists of six primitive   payload  types, two of which
have been deprecated. A primitive  in   the  wire-stream is a multi-byte
string that provides  three  pieces  of   information:  a  wire-type,  a
user-specified tag (field number), and the raw payload.   Except for the
tag and its
wire-type,  protobuf  payloads  are   not  instantaneously  recognizable
because the wire-stream  contains  no   payload  type  information.  The
interpreter uses the tag to associate the  raw payload with a local host
type specified by the template. Hence, the  message can only be properly
decoded using  the template that was used to encode it. Note also that
the primitive is interpreted according to  the   needs  of a local host.
Local word-size and endianness are dealt with at this level.

The following table shows the association between the types in the .proto file
and the primitives used in the
wire-stream.  For how these correspond to other programming languages,
such as C++, Java, etc. see [[Protocol Buffers Scalar Value
Types][https://developers.google.com/protocol-buffers/docs/overview#scalar]],
which also has advice on how to choose between the various integer
types. (Python3 types are also given here, because Python is used
in some of the interoperability tests.)

| Prolog     | Wirestream       | .proto file       | C++      | Python3  | Notes   |
| ---------- | ---------------- | ----------------- | -------- | -------- | ------- |
| double     | fixed64          | double            | double   | float    |         |
| unsigned64 | fixed64          | fixed64           | uint64   | int      |         |
| integer64  | fixed64          | sfixed64          | int64    |          |         |
| float      | fixed32          | float             | float    | float    |         |
| unsigned32 | fixed32          | fixed32           | uint32   | int      |         |
| integer32  | fixed32          | sfixed32          | int32    |          |         |
| integer    | varint           | sint32            | int32    | int      | 1, 2, 9 |
| integer    | varint           | sint64            | int64    | int      | 1, 2, 9 |
| signed32   | varint           | int32             | int32    | int      | 2, 3, 10 |
| signed64   | varint           | int64             | int64    | int      | 2, 3, 10 |
| unsigned   | varint           | uint32            | uint32   | int      | 2, 3    |
| unsigned   | varint           | uint64            | uint64   | int      | 2, 3    |
| boolean    | varint           | bool              | bool     | bool     | 2, 8    |
| enum       | varint           | (enum)            | (enum)   | (enum)   |         |
| atom       | length delimited | string            |          | str (unicode) |    |
| codes      | length delimited | bytes             |          | bytes    |         |
| utf8_codes | length delimited | string            |          | str (unicode) |    |
| string     | length delimited | string            | string   | str (unicode) |    |
| embedded   | length delimited | message           |          | (class)  | 5       |
| repeated   | length delimited | repeated          |          | (list)   | 6       |
| repeated_embedded | length delimited | repeated   |          | (list)   | 11      |
| packed     | length delimited | packed repeated   |          | (list)   |         |

*|Notes:|*

    1. Encoded using a compression technique known as zig-zagging,
       which is more efficient for negative values, but which is
       slightly less efficient if you know the values will always
       be non-negative.
    2. Encoded as a modulo 128 string. Its length is proportional to
       its magnitude. The intrinsic word length is decoupled between
       parties. If zig-zagging is not used (see note 1), negative
       numbers become maximum length.
    3. SWI-Prolog has unbounded integers, so an unsigned integer isn't
       a special case (it is range-checked and an exception thrown if
       its representation would require more than 32 or 64 bits).
    4. Encoded as UTF8 in the wire-stream.
    5. Specified as =|embedded(Tag,protobuf([...]))|=.
    6. Specified as =|repeated(Tag,Type([...,...]))|=, where
       Type is =unsigned, =integer=, =string=, etc.
    7. =|repeated ... [packed=true]|= in proto2.
       Can not contain "length delimited" types.
    8. Prolog =|boolean(Tag,false)|= maps to 0 and
       =|boolean(Tag,true)|= maps to 1.
    9. Uses "zig-zag" encoding, which is more space-efficient for
       negative numbers.
    0. The documentation says that this doesn't use "zig-zag"
       encoding, so it's less space-efficient for negative numbers.
       In particular, both C++ and Python encode negative numbers as
       10 bytes, and this implementation does the same for wire-stream compatibility
       (note that SWI-Prolog typically uses 64-bit integers anyway).
       Therefore, signed64 is used for both .proto types =int32= and
       =int64=.
    1. Specified as =|repeated_embedded(Tag,protobuf([...]),Fields)|=

### Tags (field numbers) {#protobufs-tags}

A tag (or field number) is a small integer that is present in every wire-stream primitive.
The tag is the only means that  the interpreter has to synchronize the
wire-stream with its template. Tags are user   defined  for each term in
each message of the wire-stream. The protobuf specification requires that
each field within a message has a unique field number; the protobuf compiler
(=protoc=) will produce an error if a field number is reused (field numbers
are unique only within a message; an embedded message can use the same
field numbers without ambigituity).

### Basic Usage {#protobufs-basic-usage}

A protobuf wire-stream is a byte  string   that  is comprised of zero or
more of the above multi-byte wire-stream primitives. Templates are lists
of Prolog terms. Each term corresponds to  a production rule in the DCG.
The purpose of the template is to  provide   a  recipe and value set for
encoding and decoding a particular message.   Each  term in the template
has an arity of two.  The  term's   functor  is  the  local "host type".
Argument 1 is its tag (field number), which must always  be  ground, and
argument 2 is its associated value, which may or may not be ground.

A protobuf "message" is a list of fields and is encoded in the template as
=|protobufs([Field1, Field2, ...])|=, where each field is of the
form =|Type(Tag,Value|= and =Type= can be any scalar or compound
type.

Scalar fields are encoded as =|Type(Tag,Value)|=.  For example, if a
field is defined in a .proto file by
=|optional string some_field = 10|=, then it could be encoded by
=|string(10,"some field's contents")|= or by
=|atom(10, 'some field\'s contents')|=.

Repeated fields are done by
=|repeated(Tag,Type([Value1,Value2,...])|=, where =|Type|= is any type.

Embedded messages are done by
=|embedded(Tag,protobuf([Field1,Field2,...]))|= (this is the same
=protobuf(...)= as is used at the top level).

Repeated embedded messages are done by
=|repeated_embedded(Tag,protobuf([Field1,Field2,...]),Fields)|=,
which gets repeated items and combines them into a list.
For example,
=|repeated_embedded(Tag, protobuf([string(1,_Key),string(2,_Value)]), Fields)|=
could unify =Fields= to =|[protobuf([string(1,"key1"),string(2,"value1")]), protobuf([string(1,"key2"),string(2,"value2")])]|=.
Note that the variables in the =protobuf= part of the term do not get instantiated: they are
similar to the =Template= in findall/3 and similar.

*|Note:|* It is an error to attempt to encode a message using a template
that is not ground. Decoding a message  into a template that has unbound
variables  has  the  effect  of  unifying    the  variables  with  their
corresponding values in the wire-stream.

Assume a .proto definition:

```prolog
message Command {
  optional string msg_type = 1;
  optional string command  = 2;
  optional int32  x        = 3;
  optional int32  y        = 4;
}
```

Map a Prolog structure to a Protocol Buffer:

```prolog
%! command(+Term, -Proto) is det.
% Map a Prolog term to a corresponding protobuf term.
command(add(X,Y), Proto) :-
   freeze(X, must_be(integer, X)),  % for debugging
   freeze(Y, must_be(integer, Y)),  % for debugging
   Proto = protobuf([atom(1, command),
                     atom(2, add),
                     integer(3, X),
                     integer(4, Y)
                    ]).
```

Later on:

```prolog
   ... prepare X, Y for command/2 ...

   command(add(X,Y), Proto),
   protobuf_message(Proto, WireCodes),

   % send the message
   open('filename', write, Stream, [encoding(octet),type(binary)]),
   format(Stream, '~s', [WireCodes]),
   close(Stream)
```

=Proto= is the protobuf template.  Each   template  describes  exactly one
message. =WireCodes= is the wire-stream, which encodes bytes (values between 0 and 255 inclusive).
If   you  are  interworking with other
systems and languages, then the protobuf   templates  that you supply to
protobuf_message/2  must  be  equivalent  to   those  described  in  the
.proto file that is used on the other side.

### Alternation, Aggregation, Encapsulation, and Enumeration {#protobufs-aaee}

#### Alternation {#protobufs-alternation}

The  protobuf  grammar  provides  a   reserved  word,  =optional=,  that
indicates that the production rule that it  refers to may appear once or
not at all in a protobuf message.  Since   Prolog  has  its own means of
alternation, this reserved word is not supported  on the Prolog side. It
is anticipated that customary Prolog mechanisms for nondeterminism (e.g.
backtracking) will be used to generate and test alternatives.

Note that =required= and =optional= have been removed from the proto3
specification, making all fields optional. This has been partially
revised in releases 3.12 and later. In general, you should not expect
any field to exist, nor can you expect a repeated field to have at
least one item.

Also note that the handling of missing fields is slightly different in
proto2 and proto3 -- proto2 allows specifying a default value but proto3
uses 0 and =""= as defaults for numbers and strings and omits encoding
any field that has one of those default values.

TODO: determine correct behvaior for =oneof= with a default field value.

#### Aggregation {#protobufs-aggregation}

It is possible to specify homogeneous vectors   of things (e.g. lists of
numbers) using the =repeated= attribute. You specify a repeated field as
follows:

```prolog
    repeated(22, float([1,2,3,4])),
    repeated(23, enum(tank_state([empty, half_full, full]))).
```

The first clause above  will cause all  four   items  in  the list to be
encoded in the wire-stream as IEEE-754   32-bit  floating point numbers,
all with tag 22. The decoder will aggregate all items in the wire-stream
with tag 22 into a list as above.  Likewise, all the items listed in the
second clause will be  encoded  in   the  wire-stream  according  to the
mapping defined in an enumeration   (described below) tank_state/2, each
with tag 23.

You can also encode vectors of embedded messages using =repeated_embedded=.
This uses a "template" for the individual messages and a list of messages
in the wire stream.
For example: =|repeated_embedded(Tag, protobuf([string(1,_Key),string(2,_Value)]), Fields)|=
where =Fields= gets a list (possibly empty), with each item of the form
=|protobuf([string(1,_Key),string(2,_Value)])|=.


*|Notes:|*

 Beware that there is no explicit means  to encode an empty set. The
 protobuf specification provides that a =repeated= field may match a
 tag zero or more times. The empty set, while legal, produces no output
 on encode. While decoding a =repeated= term, failure to match the
 specified tag will yield an empty set of the specified host type.

 An omitted =optional= field is handled the same way as a =repeated=
 field with an empty set.

 The protobuf grammar provides a variant   of the =repeated= field known
 as "packed." This is represented similar to =repeated=, e.g.:

```prolog
    packed(22, float([1,2,3,4])),
    packed(23, enum(tank_state([empty, half_full, full]))).
```

#### Handling missing fields {#protobufs-missing}

For input, you can wrap fields in `repeated`, so that if a field is there,
it gets a length-1 list and if it's missing, an empty list:

```prolog
?- Codes = [82,9,105,110,112,117,116,84,121,112,101],
   protobuf_message(protobuf([embedded(10, protobuf([repeated(13, integer64(I))]))]),  Codes),
   protobuf_message(protobuf([embedded(10, protobuf([repeated(13, double(D))]))]),  Codes),
   protobuf_message(protobuf([repeated(10, string(S))]), Codes).
I = [7309475598860382318],
D = [4.272430685433854e+180],
S = ["inputType"].
```

```
?- Codes = [82,9,105,110,112,117,116,84,121,112,101],
      protobuf_message(protobuf([repeated(10, string(S)),
                                 repeated(11, integer64(I))]), Codes).
S = ["inputType"],
I = [].
```

This technique can also be used for output - a missing field simply
produces nothing in the wire format:
```
?- protobuf_message(protobuf([repeated(10, string([]))]), Codes).
Codes = [].
?- protobuf_message(protobuf([repeated(10, string(S))]), []).
S = [].
```

#### Encapsulation and Enumeration {#protobufs-encapsulation}

It is possible to embed one protocol buffer specification inside
another using the =embedded= term.  The following example shows a
vector of numbers being placed in an envelope that contains a command
enumeration.

Enumerations are a compact method of sending   tokens from one system to
another. Most occupy only two bytes   in the wire-stream. An enumeration
requires that you specify a callable   predicate like commands/2, below.
The first argument is an atom  specifying   the  name  of token, and the
second is an integer  that   specifies  the  token's value.
These  must  of  course,  match  a   corresponding  enumeration  in  the
.proto file.

*|Note:|* You must expose this predicate to the protobufs module
by assigning it explicitly.

```prolog
protobufs:commands(Key, Value) :-
    commands(Key, Value).

commands(square, 1).
commands(decimate, 2).
commands(transform, 3).
commands(inverse_transform, 4).

basic_vector(Type, Proto) :-
    vector_type(Type, Tag),
    Proto = protobuf([ repeated(Tag, Type) ]).

send_command(Command, Vector, WireCodes) :-
    basic_vector(Vector, Proto1),
    Proto = protobuf([enum(1, commands(Command)),
                      embedded(2, Proto1)]),
    protobuf_message(Proto, WireCodes).
```

Use it as follows:

```prolog
?- send_command(square, double([1,22,3,4]), WireCodes).
WireCodes = [8, 1, 18, 36, 17, 0, 0, 0, 0, 0, 0, 240, 63, 17, 0, 0, 0, 0, 0,
0, 54, 64, 17, 0, 0, 0, 0, 0, 0, 8, 64, 17, 0, 0, 0, 0, 0, 0, 16, 64].

?- send_command(Cmd, V, $WireCodes).
Cmd = square,
V = double([1.0, 22.0, 3.0, 4.0]).
```

*|Compatibility Note:|* The protobuf   grammar  (protobuf-2.1.0) permits
enumerations to assume negative values. This requires them to be encoded
as integers. Google's own  Golden   Message  unit-test framework has
enumerations encoded as regular integers, without the "zigzag" encoding.
Therefore, negative values are space-inefficient, but they are allowed.

An earlier version of protobuf_message/2 assumed that enumeration values
could not be zero, and there might still be incorrect assumptions in the code,
resulting in either exceptions or silent failure.

#### Heterogeneous Collections {#protobufs-heterogeneous}

Using Protocol Buffers, it is          easy        to specify fixed data
structures and homogeneous vectors like one might find in languages like
C++ and Java. It is however,  quite   another  matter  to interwork with
these  languages  when  requirements  call  for  working  with  compound
structures, arrays of compound structures,   or unstructured collections
(e.g. bags) of data.

At bottom, a wire-stream is nothing more   than a concatenated stream of
primitive wire type strings. As long as you can associate a tag with its
host type in advance, you  will  have   no  difficulty  in  decoding the
message. You do this by supplying the  _structure_. Tell the parser what
is possible and let the parser figure it  out on its own, one production
at a time. An example may be found in the appendix.

### Groups (deprecated) {#protobufs-groups}

Protocol Buffer Groups provide a means for constructing unitary messages
consisting of ad-hoc lists of  terms.   The  following protobuf fragment
shows the definition of a group carrying a complex number.

```prolog
     Proto = group(2, [ double(1, Real_part), double(2, Img_part) ]).
```

Groups have been replaced by _embedded_ messages, which are slightly
less expensive to encode.

### Advanced Topics {#protobufs-advanced}

#### Precompiled Messages {#protobufs-precompiled}

Performance  can  be                 improved    using   a  strategy  of
precompiling the constant portions  of   your  message. Enumerations for
example,   are   excellent   candidates    for   precompilation.   Using
protobuf_message/3, the precompiled portion of   the message is inserted
directly in the wire-stream on encode, and   is unified with, and removed
from the wire-stream on decode. The  following  shows how the
"send_command" example above, can be converted to precompiled form:

```prolog
send_precompiled_command(Command, Vector, WireCodes) :-
    basic_vector(Vector, Proto1),
    % precompiled_message/3 is created by term_expansion
    precompiled_message(commands(Command), WireCodes, Tail),
    protobuf_message(protobuf([embedded(3, Proto1)]), Tail).

term_expansion(precompile_commands, Clauses) :-
    findall(precompiled_message(commands(Key), WireCodes, Tail),
            (   protobufs:commands(Key, _),
                Proto = protobuf([atom(1, command),
                                  enum(2, commands(Key))]),
                protobuf_message(Proto, WireCodes, Tail)
            ),
            Clauses).

*
*
*
precompile_commands.  % Trigger the term-expansion precompilation
```

#### Supplying Your Own Host Type Message Sequences {#protobufs-user-types}

You can extend the parser to support your own compound host types. These
are treated as first class entities by the   parser. That is they can be
used either by themselves, or in  =repeated= and =embedded= clauses just
as any other host type would be. You  do this by hooking into the parser
and adding your own =message_sequence= productions. Your hook eventually
calls back into the parser   with  your substitution/expansion protobuf,
which is then embedded in the wire   stream. Recursive structures can be
defined this way. A simple example of  a recursive XML like structure is
shown in the appendix.

# Appendix {#protobufs-appendix}

## Example: A Simple XML Like Structure {#protobufs-ex-xml}

This is an example of using the low-level interface for implementing
a domain-specific language that maps to protobufs.

In this example we demonstrate managing  a recursive structure like XML.
The structure shown in xml_proto/1 below,   is  similar to the structure
returned by load_xml_file/2, which  is  part   of  the  SGML library. We
supply three =message_sequence= decorators: =kv_pair=, =xml_element=,
and =aux_xml_element=. These are treated as first class host types.

```prolog
:- multifile protobufs:message_sequence//3.

protobufs:message_sequence(Type, Tag, Value)  -->
    { my_message_sequence(Type, Value, Proto) },
    protobufs:message_sequence(embedded, Tag, Proto), !.
%
% On encode, the value type determines the tag. And on decode
% the tag to determines the value type.
%

guard(Type, Value) :-
    ( nonvar(Value) -> is_of_type(Type, Value); true ).

my_message_sequence(kv_pair, Key=Value, Proto) :-
    Proto = protobuf([atom(30, Key), X]),
    ( ( guard(integer, Value), X = integer(31, Value) )
    ; ( guard(float, Value),   X = double(32,  Value) )
    ; ( guard(atom, Value),    X = atom(33,    Value)) ).

my_message_sequence(xml_element,
                    element(Name, Attributes, Contents), Proto) :-
    Proto = protobuf([ atom(21, Name),
                       repeated(22, kv_pair(Attributes)),
                       repeated(23, aux_xml_element(Contents))]).

my_message_sequence(aux_xml_element, Contents, Proto) :-
    Contents = element(_Name, _Attributes, _ElementContents),
    Proto = protobuf([xml_element(40, Contents)]).

my_message_sequence(aux_xml_element, Contents, Proto) :-
    Proto = protobuf([atom(43, Contents)]).

xml_proto([element(space1,
                   [foo='1', bar='2'],
                   [fum,
                    bar,
                    element(space2,
                            [fum=3.1415, bum= -14],
                            ['more stuff for you']),
                    element(space2b,
                            [],
                            [this, is, embedded, also]),
                    to,
                    you])]).

test_xml(X, Y) :-
    Proto = protobuf([repeated(20, xml_element(X))]),

    protobuf_message(Proto, Y).

% And test it:

?- xml_proto(X), test_xml(X,Y), test_xml(Z,Y), Z == X.
X = Z,
Z = [element(space1,
             [foo='1', bar='2'],
             [fum,
              bar,
              element(space2,
                      [fum=3.1415, bum= -14],
                      ['more stuff for you']
                    ),
              element(space2b,
                      [],
                      [this, is|...]
                     ),
              to,
              you])],
Y = [162, 1, 193, 1, 170, 1, 6, 115, 112|...],
```

A protobuf description that is compatible with the above wire stream
follows:

```
message kv_pair {
  required string key = 30;
  optional sint64  int_value = 31;
  optional double float_value  = 32;
  optional string atom_value = 33;
}

message aux_xml_element {
  optional string atom = 43;
  optional xml_element element = 40;
}

message xml_element {
  required string name = 21;
  repeated kv_pair attributes = 22;
  repeated aux_xml_element contents = 23;
}

message XMLFile {
  repeated xml_element elements = 20;
}
```

Verify the wire stream using the protobuf compiler's decoder:

```
$ protoc --decode=XMLFile pb_vector.proto <tmp98.tmp
elements {
  name: "space1"
  attributes {
    key: "foo"
    atom_value: "1"
  }
  attributes {
    key: "bar"
    atom_value: "2"
  }
  contents {
    atom: "fum"
  }
  contents {
    atom: "bar"
  }
  contents {
    element {
      name: "space2"
      attributes {
        key: "fum"
        float_value: 3.1415
      }
      attributes {
        key: "bum"
        int_value: -14
      }
      contents {
        atom: "more stuff for you"
      }
    }
  }
  contents {
    element {
      name: "space2b"
      contents {
        atom: "this"
      }
      contents {
        atom: "is"
      }
      contents {
        atom: "embedded"
      }
      contents {
        atom: "also"
      }
    }
  }
  contents {
    atom: "to"
  }
  contents {
    atom: "you"
  }
}
```

## Example: Vectors of Numbers {#protobufs-ex-vector-of-numbers}

This is an example of using the low-level interface.

In the Prolog client:

```prolog
vector_type(double(_List), 2).
vector_type(float(_List), 3).
vector_type(integer(_List), 4).
vector_type(integer64(_List), 5).
vector_type(integer32(_List), 6).
vector_type(unsigned(_List), 7).
vector_type(codes(_List), 8).
vector_type(atom(_List), 9).
vector_type(string(_List), 10).

vector(Type, B):-
    vector_type(Type, Tag),
    Proto = protobuf([ repeated(Tag, Type) ]),
    protobuf_message(Proto, B).
```

A protobuf description that is compatible with the above wire stream
follows:

```prolog
  message Vector {
  repeated double double_values     = 2;
  repeated float float_values       = 3;
  repeated sint32 integer_values    = 4;
  repeated fixed64 integer64_values = 5;
  repeated fixed32 integer32_values = 6;
  repeated uint32 unsigned_values   = 7;
  repeated bytes bytes_values       = 8;
  repeated string atom_values       = 9;
  repeated string string_values     = 10;
  }
```

A typical application might consist of   an abstract adapter class along
with a collection  of  concrete  subclasses   that  refine  an  abstract
behavior in order to hide the   interaction with the underlying protobuf
interpreter. An example of such a class written in C++ may be found in
the demos.

On the Prolog side:

```prolog
  :- meta_predicate ~>(0,0).
  :- op(950, xfy, ~>).

  ~>(P, Q) :-
    setup_call_cleanup(P, (true; fail), assertion(Q)).

  write_as_proto(Vector) :-
    vector(Vector, WireStream),
    open('tmp99.tmp', write, S, [encoding(octet),type(binary)])
      ~> close(S),
    format(S, '~s', [WireStream]), !.

  testv1(V) :-
    read_file_to_codes('tmp99.tmp', Codes, [encoding(octet),type(binary)]),
    vector(V, Codes).
```

Run the Prolog side:

```prolog
?- X is pi,
   write_as_proto(double([-2.2212, -7.6675, X, 0, 1.77e-9, 2.54e222])).
X = 3.14159.

?- testv1(Vector).
Vector = double([-2.2212, -7.6675, 3.14159, 0.0, 1.77e-09, 2.54e+222])
?-
```

Verify the wire stream using the protobuf compiler's decoder:

```
$ protoc --decode=Vector pb_vector.proto <tmp99.tmp
double_values: -2.2212
double_values: -7.6675
double_values: 3.1415926535897931
double_values: 0
double_values: 1.77e-09
double_values: 2.5400000000000002e+222
```

## Example: Heterogeneous Collections {#protobufs-ex-heterogeneous}

This is an example of using the low-level interface.

The following example shows  how  one   can  specify  a  Protocol Buffer
message  that  can  deal  with  variable-length,  unstructured  bags  of
numbers:

```prolog
compound_protobuf(complex(Real, Img), group(12, [double(1, Real), double(2, Img)])).
compound_protobuf(float(Val), float(13, Val)).
compound_protobuf(double(Val), double(14, Val)).
compound_protobuf((Num rdiv Den), group(15, [integer(1, Num), integer(2, Den)])).
compound_protobuf(integer(Val), integer(16, Val)).

protobuf_bag([], []).

protobuf_bag([ Type | More], WireCodes) :-
    compound_protobuf(Type, X),
    Proto = protobuf([embedded(1, protobuf([X]))]),
    protobuf_message(Proto, WireCodes, WireCodes1),
    protobuf_bag(More, WireCodes1), !.
```

Use it as follows:

```prolog
?- protobuf_bag([complex(2,3), complex(4,5),
                 complex(6,7), 355 rdiv -113, integer(11)], X).

X = [10, 20, 99, 9, 0, 0, 0, 0, 0|...].

?- protobuf_bag(Y, $X).
Y = [complex(2.0, 3.0), complex(4.0, 5.0),
     complex(6.0, 7.0), 355 rdiv -113, integer(11)].
```

A protobuf description that is compatible with the above wire stream
follows:

```prolog
message compound_protobuf {
optional group Complex = 12 {
    required double real = 1;
    required double img = 2;
};
optional group Fraction = 15 {
    required sint64 num = 1;
    required sint64 den = 2;
};
optional float float = 13;
optional double double = 14;
optional sint32 integer = 16;
}

message protobuf_bag {
    repeated compound_protobuf bag = 1;
```

Verify the wire stream using the protobuf compiler's decoder:

```
$ protoc --decode=protobuf_bag pb_vector.proto <tmp96.tmp
bag {
  Complex {
    real: 2
    img: 3
  }
}
bag {
  Complex {
    real: 4
    img: 5
  }
}
bag {
  Complex {
    real: 6
    img: 7
  }
}
bag {
  Fraction {
    num: 355
    den: -113
  }
}
bag {
  integer: 11
}
```
