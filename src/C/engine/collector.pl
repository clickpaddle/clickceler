:- module(collector, [thread_goal_collector/1, collect_events/1, ensure_id_and_store/1]).
:- use_module(library(http/json)).
:- use_module(library(gensym)).
:- use_module(library(thread)).
:- use_module(library(lists)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/0]).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.

:- dynamic event_id_counter/1.

init_event_id_counter :-
    retractall(event_id_counter(_)),
    assertz(event_id_counter(0)).

generate_unique_event_id(Id) :-
    get_time(TS),
    TSint is floor(TS * 1000),  % ms depuis epoch
    mutex_lock(event_id_mutex),
    (   retract(event_id_counter(Count))
    ->  NewCount is Count + 1
    ;   NewCount = 1
    ),
    assertz(event_id_counter(NewCount)),
    mutex_unlock(event_id_mutex),
    % Compose un entier long : timestamp * 10000 + compteur (4 chiffres)
    Id is TSint * 10000 + NewCount.



thread_goal_collector(ClientID) :-
    format('[Collector ~w] Thread started~n', [ClientID]).


%% Mutex pour la gestion des événements et log


%% collect_events(+JsonString)
% Parse la chaîne JSON, liste d’événements, ajoute un id si absent,
% puis insert chaque événement en mémoire et log.
collect_events(JsonString) :-
    catch(
        atom_json_dict(JsonString, EventsList, [as(list)]),
        E,
        ( format(user_error, '[collector collect_events] JSON parse error: ~w~n', [E]), fail)
    ),
    is_list(EventsList), !,
    maplist(ensure_id_and_store, EventsList).
collect_events(_) :-
    format(user_error, '[collector collect_events] Error: Expected JSON list of events.~n'),
    fail.

ensure_id_and_store(EventIn) :-
    ( get_dict(id, EventIn, _Id) ->
        EventWithId = EventIn
    ; generate_unique_event_id(Id),
      put_dict(id, EventIn, Id, EventWithId)
    ),

    dict_to_event(EventWithId, EventTerm),
    assert_event(EventTerm),
    format('[collector] Received ~q~n', [EventTerm]),  % log en forme normalisée
    log_event(EventTerm),
    thread_send_message(refine, EventTerm).


% Normalize event Dict to event(Type, Attrs) 
dict_to_event(Dict, event(Type, Attrs)) :-
    get_dict(type, Dict, Type),
    del_dict(type, Dict, _, Rest),
    dict_pairs(Rest, _, Pairs),
    Attrs = Pairs.


%% assert_event(+Type, +EventDict)
%  Protected by Mutex
assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        assertz(kb_shared:event(Type, Dict))
    ).
