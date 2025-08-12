:- module(collector, [thread_goal_collector/1, collect_events/1, ensure_default_event/1,ensure_field/4]).
:- use_module(library(http/json)).
:- use_module(library(gensym)).
:- use_module(library(thread)).
:- use_module(library(lists)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).

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
    log_trace(info,'[Collector ~w] Thread started~n', [ClientID]).



collect_events(JsonString) :-
    catch(
        atom_json_dict(JsonString, DictList, [as(list)]),
        E,
        ( log_trace(error, '[collector collect_events] JSON parse error: ~w~n', [E],[]), fail)
    ),
    is_list(DictList), !,
    maplist(ensure_default_event, DictList).

collect_events(_) :-
    log_trace(warning, '[collector collect_events] Expected JSON list of events.~n',[]),
    fail.

ensure_default_event(DictIn) :-
    ( get_dict(id, DictIn, _) ->
        DictWithId = DictIn
    ; generate_unique_event_id(Id),
      put_dict(id, DictIn, Id, DictWithId) 
    ),
    get_time(TimeStamp),
    ensure_field(timestamp,TimeStamp,DictWithId, DictWithTimeStamp),
    ensure_field(status, open, DictWithTimeStamp, DictWithStatus),
    ensure_field(counter, 0, DictWithStatus, DictWithCounter),
    ( get_dict(type, DictWithCounter, TypeRaw) ->
        atom_string(Type, TypeRaw),
        del_dict(type, DictWithCounter, _, DictFinal)
    ; Type = unknown,
      DictFinal = DictWithCounter
    ),
    Event = event(Type, DictFinal),
    %assert_event(Event),
    log_event(Event),
    safe_thread_send_message(refine_queue, Event).

ensure_field(Key, Default, DictIn, DictOut) :-
    ( get_dict(Key, DictIn, _) ->
        DictOut = DictIn
    ; put_dict(Key, DictIn, Default, DictOut)
    ).
dict_to_event(Dict, event(Type, Attrs)) :-
    get_dict(type, Dict, Type),
    del_dict(type, Dict, _, Rest),
    dict_pairs(Rest, _, Pairs),
    Attrs = Pairs.


assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        assertz(kb_shared:event(Type, Dict))
    ).


queue_exists(QueueName) :-
    catch(message_queue_property(QueueName, _), _, fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; log_trace(error, '[Collector] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

