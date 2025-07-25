:- module(collector, [thread_goal_collector/1, collect_events/1, ensure_id_and_store/1]).

:- use_module(library(http/json)).
:- use_module(library(gensym)).
:- use_module(library(thread)).
:- use_module(library(lists)).

:- dynamic event/2.
:- dynamic collector_log_mutex/1.

:- dynamic event_id_counter/1.
:- mutex_create(event_id_mutex).

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
collector_log_mutex(mutex_collector).


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

%% ensure_id_and_store(+EventDict)
% Ajoute un id s'il manque, insère dynamiquement et loggue.
ensure_id_and_store(EventIn) :-
    ( get_dict(id, EventIn, Id) ->
        EventWithId = EventIn
    ; generate_unique_event_id(Id),
      put_dict(id, EventIn, Id, EventWithId)
    ),
    assert_event(Id, EventWithId),
    format('[~w] ~q~n', [Id, EventWithId]),
    log_event(Id, EventWithId).

%% assert_event(+Id, +EventDict)
% Protegé par mutex pour éviter conflits entre threads.
assert_event(Id, EventDict) :-
    collector_log_mutex(Mutex),
    with_mutex(Mutex,
        ( retractall(event(Id, _)),
          assertz(event(Id, EventDict))
        )
    ).

%% log_event(+Id, +EventDict)
% Ajoute une ligne JSON dans le fichier de log
log_event(_Id, EventDict) :-
    collector_log_mutex(Mutex),
    with_mutex(Mutex,
        ( open('../logs/collector.log', append, Stream, [encoding(utf8)]),
          % Convertir le dict en JSON string
          atom_json_dict(JsonAtom, EventDict, []),
          format(Stream, '~w~n', [JsonAtom]),
          close(Stream)
        )
    ).

