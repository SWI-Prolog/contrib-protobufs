% -*- mode: Prolog coding:utf-8 -*-

% Addressbook example, derived from
% https://developers.google.com/protocol-buffers/docs/pythontutorial

:- module(addressbook, [test_write/0, test_read/0]).

:- use_module(library(protobufs)).
:- use_module(addressbook_pb).
:- use_module(addressbook2_pb).
:- use_module(timestamp_pb).

write_message(Path, Person) :-
    message_file(Path, FullPath),
    (   exists_file(FullPath)
    ->  read_file_to_codes(FullPath, WireCodes, [encoding(octet),type(binary)]),
        protobuf_parse_from_codes(WireCodes, 'tutorial.AddressBook', AddressBook)
    ;   AddressBook = 'tutorial.AddressBook'{people: []}
    ),
    get_time(TimeStamp),
    TimeSec is integer(floor(TimeStamp)),
    TimeNano is integer((TimeStamp-TimeSec)*1000000000),
    put_dict(timestamps, Person,
             'Timestamp'{last_updated:u{seconds:TimeSec, nanos:TimeNano}}, % 'Timestamp', 'u' are comments
             Person2),
    % The Python example simply appends to the new entry: the following
    % code replaces the entry if it's there:
    (   select(APerson, AddressBook.people, OtherPersons),
        APerson.id == Person2.id
    ->  append(OtherPersons, [Person2], People2)
    ;   append(AddressBook.people, [Person2], People2)
    ),
    protobuf_serialize_to_codes('tutorial.AddressBook'{people: People2},
                                'tutorial.AddressBook', WireCodesOut),
    open(FullPath, write, OutStream, [encoding(octet),type(binary)]),
    format(OutStream, '~s', [WireCodesOut]),
    close(OutStream).

test_write :-
    test_write('addressbook.wire').

test_write(Path) :-
    write_message(Path,
                  _{id:1234,
                    name:"John Doe",
                    email:"jdoe@example.com",
                    phones:[_{number:"555-4321", type:'HOME'}]}),
    write_message(Path,
                  _{id:666,
                    name:"Satan",
                    email:"satan@fb.com",
                    phones:[_{number:"555-1212", type:'WORK'},
                            _{number:"555-1234", type:'HOME'}]}).

test_read :-
    test_read('addressbook.wire').

test_read(Path) :-
    message_file(Path, FullPath),
    read_file_to_codes(FullPath, WireCodes, [encoding(octet),type(binary)]),
    protobuf_parse_from_codes(WireCodes, 'tutorial.AddressBook', AddressBook),
    format('=== address book ===~n', []),
    maplist(print_entry, AddressBook.people),
    format('=== address book (end ===~n', []).

print_entry(Person) :-
    % In Python, you test for the email existing by
    % person.HasField('email'); but in SWI-Prolog, it's a regular
    % dict, so use get_dict(email, Person, Email)

    % You can print the entire term for debugging by:
    %    print_term(Person, [indent_arguments(4), right_margin(80), output(current_output)]),

    format('Person ID: ~w~n', [Person.id]),
    format('  Name: ~w~n', [Person.name]),
    (   get_dict(email, Person, Email)
    ->  format('  Email: ~w~n', [Email])
    ;   true
    ),
    (   get_dict(phones, Person, Phones)
    ->  maplist(print_phone_number, Phones)
    ;   true
    ),
    % TODO: print last update timestamp
    nl.

print_phone_number(Phone) :-
    phone_type(Phone.type, PhoneType),
    format('  ~w #: ~w~n', [PhoneType, Phone.number]).

phone_type('MOBILE', 'Mobile phone').
phone_type('HOME', 'Home phone').
phone_type('WORK', 'Work phone').

%! message_file(+LocalPath:atom, -Path:atom) is det.
% Given a =LocalPath=, determine its path relative to the current module.
message_file(LocalPath, Path) :-
    source_file(addressbook:message_file(_,_), MyFile),
    file_directory_name(MyFile, MyDir),
    atomic_list_concat([MyDir, LocalPath], /, Path).

end_of_file.
