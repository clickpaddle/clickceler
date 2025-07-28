:- module(kb_shared, [thread_goal_kb_shared/1,start_kb_shared_loop/0, assert_json_event/2, event/2, print_all_events/1, eventlog_mutex/1, log_event/1]).
:- use_module(library(http/json)).
:- dynamic event/2.
:- multifile event/2.
:- dynamic eventlog_mutex/1.
:- mutex_create(event_id_mutex).

thread_goal_kb_shared(ClientID) :-
    format('[kb_shared ~w] Thread started~n', [ClientID]).

% Principal Loop
start_kb_shared_loop :-
    thread_self(Main),
    format("[Main ~w] Starting kb_shared loop~n", [Main]),
    loop.

%% Mutex pour la gestion des événements et log
eventlog_mutex(mutex_event).


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
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w~n", [Id, Err]), fail)
    ),
    format("[Main] Event asserted: ~w => ~w~n", [Id, Json]).

handle_message(stop) :-
    format("[Main] Stopping kb_shared~n"),
    !.

% Public interface to assert event safely
assert_json_event(Id, Json) :-
    catch(
        assertz(event(Id, Json)),
        Err,
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w~n", [Id, Err]), fail)
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
        format("]).~n")
    )).

print_attrs([]).
print_attrs([Key-Value]) :-
    !,
    format("~q-~q", [Key, Value]).
print_attrs([Key-Value | Rest]) :-
    format("~q-~q, ", [Key, Value]),
    print_attrs(Rest).

%% log_event(+EventTerm)
% Ajoute une ligne JSON dans le fichier de log
log_event(EventTerm) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        ( open('../logs/eventdb.log', append, Stream, [encoding(utf8)]),
          write_term(Stream, EventTerm, [quoted(true), fullstop(true), nl(true)]), 
          close(Stream)
        )
    ).

