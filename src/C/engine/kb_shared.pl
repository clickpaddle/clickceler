:- module(kb_shared, [thread_goal_kb_shared/1,start_kb_shared_loop/0,assert_json_event/2, event/2, print_all_events/1, log_event/1, is_subtype/2]).
:- use_module(library(http/json)).
:- use_module('../types/types.pl',[subtype/2, valid_severity/1, valid_status/1]).
:- use_module(utils).

:- dynamic event/2.
:- multifile event/2.
:- mutex_create(event_id_mutex).

thread_goal_kb_shared(ClientID) :-
    log_trace(info,'[Kb_shared ~w  ] Thread started', [ClientID]).

% Principal Loop
start_kb_shared_loop :-
    thread_self(_Main),
    format("[Kb_shared] Starting kb_shared loop", []),
    loop.



% Get and Handle messages
loop :-
    thread_get_message(Message),
    handle_message(Message),
    loop.

% Store Events safely
handle_message(json_event(Id, Json)) :-
    catch(
        assertz(event(Id, Json)),
        Err,
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w", [Id, Err]), fail)
    ),
    format("[Main] Event asserted: ~w => ~w", [Id, Json]).


% Public interface to assert event safely
assert_json_event(Id, Json) :-
    catch(
        assertz(event(Id, Json)),
        Err,
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w", [Id, Err]), fail)
    ).

print_all_events(ResultString) :-
    findall(EventString,
            ( event(Type, Attrs),
              event_to_string(event(Type, Attrs), EventString)
            ),
            EventsStrings),
    atomic_list_concat(EventsStrings, "", ResultString).

event_to_string(event(Type, Attrs), EventString) :-
    ( string(Type) -> atom_string(AtomType, Type) ; AtomType = Type ),
    with_output_to(string(EventString), (
        format("event(~q, [", [AtomType]),
        print_attrs(Attrs),
        format("]).")
    )).

print_attrs([]).
print_attrs([Key-Value]) :-
    !,
    format("~q-~q", [Key, Value]).
print_attrs([Key-Value | Rest]) :-
    format("~q-~q, ", [Key, Value]),
    print_attrs(Rest).


%% log_event(+EventTerm) EventTerm is normalized
log_event(EventTerm) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        (
            open('../logs/eventdb.log', append, Stream, [encoding(utf8)]),
            write_term(Stream, EventTerm, [quoted(true), fullstop(true), nl(true)]),
            close(Stream)
        )
    ).

is_type(Type, Type).
is_type(SubType, SuperType) :-
    subtype(SubType, Parent),
    is_type(Parent, SuperType).

event_type(Type, E) :-
    event(EventType, E),
    is_type(EventType, Type).


generate_type_predicates :-
    findall(Type, subtype(Type, _), SubTypes),
    list_to_set([log|SubTypes], Types),
    maplist(generate_type_predicate, Types).

generate_type_predicate(Type) :-
    Head =.. [Type, E],
    Body = (event_type(Type, E)),
    Clause = (Head :- Body),
    (   current_predicate(Type/1)
    ->  true
    ;   assertz(Clause)
    ).

load_type :-
    % load Types of event file and inheritance predicates. 
    absolute_file_name('../types.pl', Path, [access(read), file_errors(fail)]),
    (   exists_file(Path)
    ->  consult(Path)
    ;   format(user_error, 'Warning: engine.pl not found at ~w', [Path])
    ),
    % (Re)generate hierarchy of event types. 
    generate_type_predicates.

% A type is always a subtype of itself (reflexivity)
is_subtype(Type, Type).

% If Type is a direct subtype of SuperType
is_subtype(Type, SuperType) :-
    subtype(Type, SuperType).

% If Type is an indirect subtype via a chain of subtypes
is_subtype(Type, SuperType) :-
    subtype(Type, Intermediate),
    is_subtype(Intermediate, SuperType).

