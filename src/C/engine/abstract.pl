:- module(abstract, [thread_goal_abstract/1, start_abstract_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/5.

% Initialize abstract queue
init_queue :-
    ( catch(message_queue_property(abstract_queue,_),_,fail) -> true
    ; message_queue_create(abstract_queue),
      log_trace(info,'[Abstract] Created message queue abstract',[])
    ).

% Thread entry
thread_goal_abstract(ClientID) :-
    log_trace(info,'[Abstract ~w] Thread started', [ClientID]).

% Load rules
load_abstract_rules :-
    RuleFile = '../rules/abstract.pl',
    exists_file(RuleFile),
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Abstract] Rules loaded from ~w', [RuleFile]).
load_abstract_rules :-
    log_trace(info,'[Abstract] Warning: Rules file not found.', []).

% Main loop
start_abstract_loop :-
    load_abstract_rules,
    init_queue,
    abstract_loop.

abstract_loop :-
    thread_get_message(abstract_queue, EventTerm),
    ( catch(handle_event(EventTerm), E,
            (log_trace(error,'[Abstract] Error: ~w', [E]), fail))
    -> true
    ; log_trace(warning,'[Abstract] Warning: EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Abstract] Received: ~q', [EventTerm]),
    abstract_loop.

% Handle event
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] Normalized DictIn: ~q', [DictIn]),
    findall(rule(Priority, RuleID, Conditions, Transformations),
            abstract_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
            RuleList),
    log_trace(info,'[Abstract] Matched rules: ~q', [RuleList]),
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Abstract] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, event(EventType, DictIn), DictOut),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_event(EventOut),
    safe_thread_send_message(abstract_queue, EventOut).

% abstract_rule_match
abstract_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    abstract_rule(EventPattern, Priority, RuleID, Conditions, Transformations),
    EventPattern =.. [RuleType|Args],
    EventType = RuleType,
    bind_event_args(Args, DictIn),
    match_all_conditions(Conditions, DictIn).

bind_event_args([], _Dict).
bind_event_args([Arg|Rest], Dict) :-
    ( var(Arg) -> Arg = Dict ; true ),
    bind_event_args(Rest, Dict).

% Apply matching rules
apply_matching_rules([], _Event, DictOut) :- DictOut = _{}.
apply_matching_rules([rule(_P,_ID,_C,Transforms)|Rest], event(EventType, DictIn), DictOut) :-
    prepare_transforms(Transforms, DictIn, BoundTransforms),
    apply_transformations(BoundTransforms, DictIn, UpdatedDict),
    apply_matching_rules(Rest, event(EventType, UpdatedDict), DictOut).

prepare_transforms([], _Dict, []).
prepare_transforms([Transform|Rest], DictIn, [NewTransform|RestOut]) :-
    Transform =.. [Functor, Var],
    ( var(Var) ->
        ( existing_abstract(Functor, AbsID) -> NewVar = AbsID
        ; generate_abstract(Functor, DictIn, AbsID),
          NewVar = AbsID
        )
    ; NewVar = Var
    ),
    NewTransform =.. [Functor, NewVar],
    prepare_transforms(Rest, DictIn, RestOut).

% Assert event safely
assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      ( retractall(kb_shared:event(Type,Dict)),
        assertz(kb_shared:event(Type,Dict))
      )
    ).

% Mutex-protected add event to abstract contrib
add_to_abstract_contrib(AbstractID, EventDict) :-
    kb_shared:event(Type, AbsDict),
    get_dict(is_abstract, AbsDict, true),
    get_dict(id, AbsDict, AbstractID),
    get_dict(abstract_contrib, AbsDict, Contrib),
    \+ memberchk(EventDict, Contrib),
    NewContrib = [EventDict|Contrib],
    NewDict = AbsDict.put(abstract_contrib, NewContrib),
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        ( retract(kb_shared:event(Type,AbsDict)),
          assertz(kb_shared:event(Type,NewDict))
        )
    ).

% Check if abstract exists
existing_abstract(EventType, AbstractID) :-
    kb_shared:event(_Type, AbsDict),
    get_dict(is_abstract, AbsDict, true),
    get_dict(type, AbsDict, EventType),
    get_dict(abstract_contrib, AbsDict, Contrib),
    Contrib \= [],
    get_dict(id, AbsDict, AbstractID).

% Generate new abstract
generate_abstract(EventType, EventDict, AbstractID) :-
    gensym(abs_, AbstractID),
    AbsDict = EventDict.put(_{
        id: AbstractID,
        type: EventType,
        is_abstract: true,
        abstract_contrib: []
    }),
    assert_event(event(AbstractID, AbsDict)),
    log_trace(info,'[Abstract] Generated new abstract: ~w', [AbsDict]).

% Thread-safe message send
queue_exists(QueueName) :-
    catch(message_queue_property(QueueName,_),_,fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; log(info,'[ERROR] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

% List events / abstracts
list_all_events :-
    kb_shared:event(Type, Dict),
    log(info,'Event Type: ~w~n', [Type]),
    log(info,'Event Dict: ~w~n', [Dict]),
    fail.
list_all_events.

list_all_abstracts :-
    kb_shared:event(Type, Dict),
    get_dict(is_abstract, Dict, true),
    log(info,'Abstract Type: ~w, ID: ~w~n', [Type, Dict.id]),
    ( get_dict(abstract_contrib, Dict, Contrib) ->
        log(info,'Contributors:~w ~n',[Contrib])
    ;   log(info,'No contributors.~n',[])
    ),
    fail.
list_all_abstracts.

