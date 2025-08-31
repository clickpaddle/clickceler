:- module(abs, [thread_goal_abstract/1, start_abstract_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/6.
:- dynamic abstract_queue/1.

init_queue :-
    ( catch(message_queue_property(abstract_queue,_),_,fail) -> true
    ; message_queue_create(abstract_queue),
      log_trace(info,'[Abstract] Created message queue abstract',[])
    ).

thread_goal_abstract(ClientID) :-
    log_trace(info,'[Abstract ~w] Thread started', [ClientID]).

load_abstract_rules :-
    RuleFile = '../rules/abstract.pl',
    exists_file(RuleFile),
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Abstract] Rules loaded from ~w', [RuleFile]).
load_abstract_rules :-
    log_trace(info,'[Abstract] Rules file not found.', []).

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

handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] Received event: ~q', [event(EventType,DictIn)]),
    % Save the incoming event immediately

    Pattern =.. [EventType, DictIn],
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
    log_trace(info,'[Abstract] RuleList: ~q', [RuleList]),

    ( RuleList \= [] ->
        log_trace(info,'[Abstract] Matched rules: ~q', [RuleList]),
        sort(2, @>=, RuleList, SortedRules),
        log_trace(info,'[Abstract] Sorted rules by priority: ~q', [SortedRules]),
        apply_matching_rules(SortedRules, event(EventType, DictIn), DictOut, DictEnd),
        log_trace(info,'[Abstract] After apply_matching_rules: ~q', [DictOut]),
        EventOut = event(EventType, DictEnd),
        log_event(EventOut)
    ;   log_trace(warning,'[Abstract] No rules matched for EventTerm: ~w', [event(EventType, DictIn)])
    ).

abstract_rule_match(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations,
                    event(EventType, DictIn)) :-
    log_trace(info,'[Abstract]  Entering abstract_rule_match ',[]),
    abstract_rule(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations),
    Pattern =.. [EventType, F],
    F = DictIn,
    log_trace(info, '[Abstract] EventType ~w matches Pattern type ~w', [EventType, EventType]),
    ( match_all_conditions(Conditions, F) ->
        log_trace(info, '[Abstract] Conditions matched for RuleID ~w', [RuleID])
    ;   log_trace(info, '[Abstract] Conditions FAILED for RuleID ~w', [RuleID]),
        fail
    ).

get_or_create_abstract(event(EventType, EventDict), event(AbsType, AbsDict), Conditions, Transforms) :-
    log_trace(info, '[Abstract] get_or_create_abstract for type ~w', [AbsType]),
    ( existing_abstract(event(EventType, EventDict), Conditions, event(AbsType, AbsDict)) ->
        add_to_abstract_contrib(event(EventType, EventDict), event(AbsType, AbsDict)),
        log_trace(info, '[Abstract] Found existing abstract ~q for event ~q', [event(AbsType, AbsDict), event(EventType, EventDict)])
    ;   with_mutex(abstract_lock,
            generate_abstract(event(EventType, EventDict), event(AbsType, AbsDict), Transforms)
        )
    ),
    EventOut = event(AbsType, AbsDict),
    log_event(EventOut).

apply_matching_rules([], _, DictIn, DictIn).

apply_matching_rules([abstract_rule(RuleID, Priority, Patterns, Conditions, Abstracts, Transforms)|Rest],
                     event(EventType, DictIn),
                     DictIn, DictOut) :-
    log_trace(info, '[Abstract] Applying abstract_rule ~w (Priority: ~w)', [RuleID, Priority]),
    Patterns = [Pattern | _],
    Abstracts = [Abstract | _],
    log_trace(info, '[Debug] Pattern: ~q, Abstract: ~q', [Pattern, Abstract]),
    Abstract =.. [AbsType, _],
    ( get_or_create_abstract(event(EventType, DictIn), event(AbsType, UpdatedDict), Conditions, Transforms)
      -> log_trace(info, '[Debug] Rule applied successfully: ~w', [RuleID])
      ;  log_trace(warn, '[Debug] Rule FAILED: ~w', [RuleID]), UpdatedDict = DictIn
    ),
    apply_matching_rules(Rest, event(EventType, UpdatedDict), UpdatedDict, DictOut).


existing_abstract(event(EventType, DictIn), Conditions, event(AbsType, AbsDict)) :-
    log_trace(info,'[Abstract] Enter existing_abstract',[]),
    match_all_conditions(Conditions, DictIn),
    find_event(AbsType, (functor(DictCandidate) :- match_all_conditions(Conditions, DictCandidate)), AbsDict),
    nonvar(AbsDict),
    get_dict(is_abstract, AbsDict, true),
    log_trace(info, '[Abstract] Found existing abstract ~w matching EventType ~w', [AbsDict.id, EventType]).

generate_abstract(event(EventType, EventDict), event(AbsType, AbsDictNext), Transforms) :-
    log_trace(info, '[Abstract] Generating Dictionary of new Abstract: ~w', [AbsType]),
    generate_unique_event_id(AbstractID),
    AbsDict0 = EventDict.put(_{id: AbstractID, is_abstract:true, abstract_contrib:[], status:"open"}),
    apply_transformations(Transforms, AbsDict0, AbsDictNext),
    add_to_abstract_contrib(event(EventType, EventDict), event(AbsType, AbsDictNext)),
    assert_event(event(AbsType, AbsDictNext)),
    log_trace(info, '[Abstract] Generated new abstract: ~w', [event(AbsType, AbsDictNext)]).

% Les autres helpers (queue, listing, safe_thread_send_message) restent inchangés

