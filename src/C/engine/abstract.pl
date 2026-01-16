:- module(abstract, [thread_goal_abstract/1, start_abstract_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared).
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
            (log_trace(error,'[Abstract] abstract_loop Error: ~w', [E]), fail))
    -> true
    ; log_trace(warning,'[Abstract] abstract_loop EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Abstract] abstract_loop Received: ~q', [EventTerm]),
    abstract_loop.

handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] handle_event Received event: ~q', [event(EventType,DictIn)]),
    % Save the incoming event immediately

    Pattern =.. [EventType, DictIn],
    findall( abstract_rule(RuleID, Priority,[Pattern], Conditions,[Abstract], Settings, Keys, Transformations),
        (
            catch(
                (
                    abstract_rule_match(RuleID, Priority, [Pattern], Conditions, [Abstract], Settings, Keys, Transformations, event(EventType,DictIn)),
                    log_trace(info, '[Abstract] handle_event  Rule matched: ~w Priority: ~w Keys: ~w Settings: ~w ', [RuleID, Priority, Keys, Settings])
                ),
                E,
                log_trace(warning,'[Abstract] handle_event Rule ~w raised error: ~w', [RuleID, E])
            )
        ),
        RuleList
    ),
    log_trace(info,'[Abstract] handle_event RuleList: ~q', [RuleList]),

    ( RuleList \= [] ->
        log_trace(info,'[Abstract] handle_event Matched rules: ~q', [RuleList]),
        sort(2, @>=, RuleList, SortedRules),
        log_trace(info,'[Abstract]handle_event Sorted rules by priority: ~q', [SortedRules]),
        apply_matching_rules(SortedRules, event(EventType, DictIn)) 
    ;   log_trace(warning,'[Abstract] handle_event No rules matched for EventTerm: ~w', [event(EventType, DictIn)])
    ).

abstract_rule_match(RuleID, Priority, [Pattern], Conditions, [Abstract], Settings, Keys, Transformations, event(EventType, DictIn)) :-
    abstract_rule(RuleID, Priority, [Pattern], Conditions, [Abstract], Settings, Keys, Transformations),
    %Pattern =.. [SuperType, F],
    %( EventType == SuperType
    % ; is_subtype(EventType, SuperType)
    %),
    %F = DictIn,
    Pattern =.. [EventType, F],
    F = DictIn, 
    log_trace(info, '[Abstract] abstract_rule_match EventType ~w matches Pattern type ~w', [EventType, EventType]),
    ( match_all_conditions(Conditions, F) ->
        log_trace(info, '[Abstract] abstract_rule_match Conditions matched for RuleID ~w', [RuleID])
    ;   log_trace(info, '[Abstract] abstract_rule_match Conditions FAILED for RuleID ~w', [RuleID]),
        fail
    ).

get_or_create_abstract(event(EventType, EventDict), event(AbsType, AbsDict), Settings, Keys, Conditions, Transforms) :-
    log_trace(info, '[Abstract] get_or_create_abstract for type ~w', [AbsType]),
    ( existing_abstract(event(EventType, EventDict), Conditions, event(AbsType, AbsDict), Settings, Keys) ->
        add_to_abstract_contrib(event(EventType, EventDict),AbsType, AbsDict,AbsDictUpdated),
        replace_event(AbsType, AbsDict,AbsDictUpdated),
        log_trace(info, '[Abstract] get_or_create_abstract Found existing abstract ~q for event ~q', [event(AbsType, AbsDictUpdated), event(EventType, EventDict)]),
        update_linked_abstract(EventType, EventDict, AbsDict.id, EventDictUpdated), 
        replace_event(EventType,EventDict,EventDictUpdated)
    ;   eventlog_mutex(Mutex),
        with_mutex(Mutex,
            generate_abstract(event(EventType, EventDict), event(AbsType, AbsDict), Keys, Transforms)
        )
    ),
    EventOut = event(AbsType, AbsDict),
    log_event(EventOut).

apply_matching_rules([], _).

apply_matching_rules([abstract_rule(RuleID, Priority, Patterns, Conditions, Abstracts, Settings, Keys,Transforms)|Rest], event(EventType, DictIn)) :-
    log_trace(info, '[Abstract] apply_matching_rules Applying abstract_rule ~w (Priority: ~w) Abstracts: ~w ', [RuleID, Priority, Abstracts]),
    Patterns = [Pattern | _],
    Abstracts = [Abstract | _],
    log_trace(info, '[Abstract] apply_matching_rules Pattern: ~w, RuleID: ~w', [Pattern, RuleID]),
    Abstract =.. [AbsType, _],
    ( get_or_create_abstract(event(EventType, DictIn), event(AbsType, UpdatedDict), Settings, Keys, Conditions, Transforms)
      -> log_trace(info, '[Abstract] apply_matching_rules Rule applied successfully: ~w', [RuleID])
      ;  log_trace(warn, '[Abstract] apply_matching_rules Rule FAILED: ~w', [RuleID]), UpdatedDict = DictIn
    ),
    apply_matching_rules(Rest, event(EventType, UpdatedDict)).


% Check if an Abstract exist matching event and keys conditions
existing_abstract(event(EventType, DictIn), Conditions, event(AbsType, AbsDict), Settings, Keys) :-
    log_trace(info,'[Abstract] existing_abstract Enter',[]),
    match_all_conditions(Conditions, DictIn),
    enrich_absdict(_{}, DictIn, Keys, AbsDictKeys),
    ( member(settings{window:TWString}, Settings) ->
        duration_to_seconds(TWString, TimeWindow)
    ;   % Valeur par défaut si non précisée
        TimeWindow = 600
    ),
    ( find_event(AbsType, TimeWindow ,AbsDictKeys, Candidate) ->
        AbsDict = Candidate,
        get_dict(is_abstract, AbsDict, true),
        log_trace(info, '[Abstract] existing_abstract Found existing abstract ~w matching EventType ~w', [AbsDict.id, EventType])
    ;   log_trace(warning, '[Abstract] existing_abstract No event found for type ~w', [AbsType]),
        fail
    ).

enrich_absdict(AbsDictIn, EventDict, Keys, AbsDictOut) :-
    foldl(add_key_from_event(EventDict), Keys, AbsDictIn, AbsDictOut).

add_key_from_event(EventDict, Key, DictIn, DictOut) :-
    ( get_dict(Key, EventDict, Val) -> true ; Val = ""),
    DictOut = DictIn.put(Key, Val).

% generate abstract as it does not exist yet
generate_abstract(event(EventType, EventDict), event(AbsType, AbsDictNext), Keys, Transforms) :-
    log_trace(info, '[Abstract] generate_abstract  Generating Dictionary of new Abstract: ~w', [AbsType]),
    generate_unique_event_id(AbstractID),
    AbsDict0 = EventDict.put(_{id: AbstractID, is_abstract:true, abstract_contrib:[], status:"open", timestamp_collected: AbstractID}),
    apply_transformations(Transforms, AbsDict0, AbsDictNext),
    add_to_abstract_contrib(event(EventType, EventDict),AbsType, AbsDictNext,AbsDictUpdated),
    log_trace(info,'[Abstract] generate_abstract AbsDictUpdated: ~w',[AbsDictUpdated]), 
    assert_event(event(AbsType, AbsDictUpdated)),
    log_trace(info, '[Abstract]generate_abstract  Generated new abstract: ~w', [event(AbsType, AbsDictUpdated)]),
    update_linked_abstract(EventType, EventDict, AbsDictUpdated.id, EventDictUpdated),
    log_trace(info, '[Abstract] generate_abstract Updated Dict of Event: ~w', [event(EventType, EventDictUpdated)]).


% add_to_abstract_contrib(+EventDict, +AbsDict, -AbsDictUpdated)
add_to_abstract_contrib(event(_EventType,EventDict),AbsType,AbsDict,AbsDictUpdated) :-
    log_trace(info, "[Abstract] add_to_abstract_contrib Received EventDict: ~w AbsDict: ~w", [EventDict,AbsDict]),
    must_be(dict, EventDict),
    must_be(dict,AbsDict),

    % retrieve existing list or create empty
    ( get_dict(abstract_contrib, AbsDict, ContribList) ->
        log_trace(info, "[Abstract] add_to_abstract_contrib Existing contribution list found: ~w", [ContribList])
    ;
        ContribList = [],
        log_trace(info, "[Abstract] add_to_abstract_contrib No existing list, creating an empty one", [])
    ),

    % add the event ID to the list
    EventID = EventDict.id,
    log_trace(info, "[Abstract] add_to_abstract_contrib Event ID to add: ~w", [EventID]),
    NewContrib = [EventID | ContribList],
    log_trace(info, "[Abstract] add_to_abstract_contrib Updated contribution list: ~w", [NewContrib]),

    % update the dict
    AbsDictUpdated = AbsDict.put(_{abstract_contrib: NewContrib}),
    log_trace(info, "[Abstract] add_to_abstract_contrib  AbsDict updated: ~w", [AbsDictUpdated]).

% update the linked_abstract_field with the AbsDict.id

update_linked_abstract(EventType, EventDict, AbstractID, EventDictUpdated) :-
    must_be(dict, EventDict),
    ( get_dict(linked_abstract, EventDict, LinkedList) -> true ; LinkedList = [] ),
    NewLinkedList = [AbstractID | LinkedList],
    EventDictUpdated = EventDict.put(_{linked_abstract: NewLinkedList}),
    log_trace(info, '[Abstract] update_linked_abstract Updated linked_abstract for Event: ~w', [event(EventType,EventDictUpdated)]),
    replace_event(EventType,EventDict, EventDictUpdated).

