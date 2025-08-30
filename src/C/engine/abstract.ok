:- module(abstract, [thread_goal_abstract/1, start_abstract_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/6.

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
    log_trace(info,'[Abstract] Rules file not found.', []).

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
    ; log_trace(warning,'[Abstract] EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Abstract] Received: ~q', [EventTerm]),
    abstract_loop.

% Handle event
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] Normalized event: ~q', [event(EventType,DictIn)]),

    % On essaye de trouver toutes les règles qui matchent
    findall(
        abstract_rule(RuleID, Priority,[Pattern], Conditions,[Abstract], Transformations),
        (
            catch(
                (
                    abstract_rule_match(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations, event(EventType,DictIn)),
                    log_trace(info, '[Abstract] Rule matched: ~w (Priority: ~w)', [RuleID, Priority])
                ),
                E,
                log_trace(warning,'[Abstract] Rule ~w raised error: ~w', [RuleID, E])
            )
        ),
        RuleList
    ),

    ( RuleList \= [] ->
        log_trace(info,'[Abstract] Matched rules: ~q', [RuleList]),
        sort(1, @>=, RuleList, SortedRules),
        log_trace(info,'[Abstract] Sorted rules by priority: ~q', [SortedRules]),
        apply_matching_rules(SortedRules, event(EventType, DictIn), DictOut),
        log_trace(info,'[Abstract] After apply_matching_rules: ~q', [DictOut]),   
        EventOut = event(EventType, DictOut),
        log_event(EventOut),
        safe_thread_send_message(update_queue, EventOut)
    ; log_trace(warning,'[Abstract] No rules matched for EventTerm: ~w', [event(EventType, DictIn)])
    ).

% abstract_rule_match(+RuleID, +Priority, +EventType, +Conditions, -Abstract, -Transformations, +DictIn)
abstract_rule_match(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations, event(EventType,DictIn)) :-
    % Retrieve the abstract rule
    abstract_rule(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations),
    Pattern =.. [RuleEventType,E],
    DictIn = E,
    is_subtype(EventType, RuleEventType),
    log_trace(info, '[Abstract] EventType ~w  matches RuleEventType: ~w', [EventType,RuleEventType]),

    % Vérification des conditions
    ( match_all_conditions(Conditions, E) ->
        log_trace(info, '[Abstract] Conditions matched for RuleID ~w', [RuleID])
    ;
        log_trace(info, '[Abstract] Conditions FAILED for RuleID ~w', [RuleID]),
        fail
    ).



% get_or_create_abstract(event(EventType,EventDict), event(AbsType, AbsDict))
% Returns the existing abstract or creates a new one, applying its initial transforms
get_or_create_abstract(event(EventType,EventDict), event(AbsType, AbsDict),Conditions,Transforms) :-
       
    ( existing_abstract(event(EventType, EventDictIn), Conditions, event(AbsType, AbsDict)),
        add_to_abstract_contrib(event(EventType, EventDict),event(AbsType,AbsDict)),
        log_trace(info, '[Abstract] Found existing abstract ~q for event ~q', [event(EventType,EventDict),event(AbsType,AbsDict)])
    ;   with_mutex(abstract_lock,
        (
            generate_abstract(event(EventType,EventDict),event(AbsType,AbsDict)),
            log_trace(info, '[Abstract] generated new abstract ~w', [event(AbsType,AbsDict)]),
            % Apply linked_abstract to event
            update_linked_abstract(event(EventType, EventDict),event( AbsType, AbsDict)),
            log_trace(info, '[Abstract]  Updated Event linked_abstracts event ~w', [event(AbsType,AbsDict)]),
            apply_transformations(Transforms,AbsDict,AbsDictNext),
            replace_event(AbsType,AbsDictNext)
        )
    ),
    EventOut = event(AbsType,AbsDictNext),
   log_event(EventOut

).

% update_linked_abstract(+EventType, +AbstractID)
% Adds AbstractID to the field_linked_abstract of the source event(EventType, Dict)
update_linked_abstract(event(EventType, EventDict),event(AbsType,AbsDict)) :-
    with_mutex(abstract_lock,
        (
            event(AbsType,AbsDict),
            ( get_dict(linked_abstract, EventDict, Linked0) -> true ; Linked0 = [] ),
            ( memberchk(AbsDict.id, Linked0) -> Linked = Linked0 ; Linked = [AbsDict.id | Linked0] ),
            NewEventDict = EventDict.put(linked_abstract, Linked),
            replace_event(EventType, NewEventDict)
        )
    ),
    EventOut = event(EventType, NewEventDict)),
    log_event(EventType, NewEventDict).

% apply_matching_rules(+RuleList, +Event, -DictOut)
% Applies rules to the event, handling abstracts, transformations, and linked abstracts
apply_matching_rules([], event(_, DictIn), DictIn).  % nothing to do

apply_matching_rules([abstract_rule(RuleID, Priority, [_Pattern], Conditions, [Abstract], Transforms)|Rest],
                     event(EventType, DictIn)) :-
    log_trace(info, '[Abstract] Applying abstract_rule ~w (Priority: ~w)', [RuleID, Priority]),
    Abstract =.. [AbsType,AbsDict],
    % Ensure abstract exists and get its ID
    get_or_create_abstract(event(EventType, DictIn), event(AbsType, AbsDict),Conditions,Transforms),

    % Continue with the rest of the rules, passing updated linked abstracts
    apply_matching_rules(Rest, event(EventType,DictIn),DictOut).

% Helper predicate to call each condition on the pattern
call_condition(Pattern, Condition) :-
    call(Condition, Pattern).


% Assert event safely
assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      ( retractall(event(Type,Dict)),
        assertz(event(Type,Dict))
      )
    ).


% Check if abstract exists or not

% existing_abstract(+Event, +Conditions, -AbstractEvent)
existing_abstract(event(EventType, DictIn), Conditions, event(AbsType, AbsDict)) :-
    match_all_conditions(Conditions, DictIn),
    event(AbsType, AbsDict),
    get_dict(is_abstract, AbsDict, true),
    log_trace(info, '[Abstract] Found existing abstract ~w matching EventType ~w', [AbsDict.id, EventType]).

% Generate new abstract
generate_abstract(event(EventType,EventDict), event(AbsType,Absdict)) :-
    gensym(abs_, AbstractID),
    AbsDict = EventDict.put(_{
        id: AbstractID,
        type: EventType,
        is_abstract: true,
        abstract_contrib: [],
        status: "open"
    }),
    add_to_abstract_contrib(event(EventType, EventDict),event(AbsType,AbsDict)),
    assert_event(event(AbsType,AbsDict)),
    log_trace(info,'[Abstract] Generated new abstract: ~w', [event(AbsType,AbsDict)]).

% replace_event(+Type, +NewDict, +Mutex)
replace_event(Type, NewDict) :-
    eventlog_mutex(Mutex),
    get_dict(id, NewDict, Id),
    with_mutex(Mutex,
        (
            ( retract(event(Type, OldDict)), OldDict.id == Id -> true ; true ),
            assertz(event(Type, NewDict))
        )
    ).

% Thread-safe message send
queue_exists(QueueName) :-
    catch(message_queue_property(QueueName,_),_,fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; log_trace(error,' [Abstract] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

% List events / abstracts
list_all_events :-
    kb_shared:event(Type, Dict),
    log_trace(info,'[Abstract] Event Type: ~w~n', [Type]),
    log_trace(info,'[Abstract] Event Dict: ~w~n', [Dict]),
    fail.

list_all_events.

list_all_abstracts :-
    kb_shared:event(Type, Dict),
    get_dict(is_abstract, Dict, true),
    log_trace(info,'[Abstract] Type: ~w, ID: ~w~n', [Type, Dict.id]),
    ( get_dict(abstract_contrib, Dict, Contrib) ->
        log_trace(info,'[Abstract] Contributors:~w ~n',[Contrib])
    ;   log_trace(info,'[Abstract] No contributors.~n',[])
    ),
    fail.
list_all_abstracts.

