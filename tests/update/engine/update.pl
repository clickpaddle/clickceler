:- module(update, [thread_goal_update/1, start_update_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic update_rule/6.
:- dynamic update_queue/1.

init_queue :-
    ( catch(message_queue_property(update_queue,_),_,fail) -> true
    ; message_queue_create(update_queue),
      log_trace(info,'[Update] Created message queue update',[])
    ).

thread_goal_update(ClientID) :-
    log_trace(info,'[Update ~w] Thread started', [ClientID]).

load_update_rules :-
    RuleFile = '../rules/update.pl',
    exists_file(RuleFile),
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Update] Rules loaded from ~w', [RuleFile]).
load_update_rules :-
    log_trace(info,'[Update] Rules file not found.', []).

start_update_loop :-
    load_update_rules,
    init_queue,
    update_loop.

update_loop :-
    thread_get_message(update_queue, EventTerm),
    ( catch(handle_event(EventTerm), E,
            (log_trace(error,'[Update] update_loop Error: ~w', [E]), fail))
    -> true
    ; log_trace(warning,'[Update] update_loop EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Update] update_loop Received: ~q', [EventTerm]),
    update_loop.

% Handle an incoming event
handle_event(event(EventType, DictIn)) :-
    log_trace(info, '[Update] handle_event Received event: ~q', [event(EventType, DictIn)]),

    % Construct the pattern using only the EventType
    Pattern =.. [EventType, DictIn],
    log_trace(info,"[Update] handle_event Pattern ~w" ,[Pattern]),
    % Find all rules that match this event
    findall(update_rule(RuleID, Priority, [Pattern], SConditions, [Updates], Settings, Keys, TConditions, Transformations),
        ( catch(
              update_rule_match(RuleID, Priority, [Pattern], SConditions, [Updates], Settings, Keys, TConditions, Transformations, event(EventType, DictIn)),
              E,
              log_trace(warning, '[Update] handle_event Rule ~w raised error: ~w', [RuleID, E])
          )
        ),
        RuleList
    ),

    log_trace(info, '[Update] handle_event RuleList: ~q', [RuleList]),

    % If any rules matched, sort by priority descending and apply them
    ( RuleList \= [] ->
        sort(2, @>=, RuleList, SortedRules),
        log_trace(info, '[Update] handle_event Sorted rules by priority: ~q', [SortedRules]),
        apply_matching_rules(SortedRules, event(EventType, DictIn))
    ;   log_trace(warning, '[Update] handle_event No rules matched for Event: ~q', [event(EventType, DictIn)])
    ).

%  Main orchestrator: update_rule_match/10
update_rule_match(RuleID, Priority, [Pattern], SConditions, [Update], Settings, Keys, TConditions, Transformations, event(EventType, DictIn)) :-
    update_rule(RuleID, Priority, [Pattern], SConditions, [Update], Settings, Keys, TConditions, Transformations), 
    log_trace(info, '[Update] update_rule_match ~w ~w ~w ~w', [RuleID, Priority, Pattern, SConditions]),
    Pattern =.. [EventType, F], 
    F = DictIn,  
    log_trace(info, '[Update] update_rule_match EventType ~w matches Pattern type ~w', [EventType, EventType]),
    
    ( match_all_conditions(SConditions, F)
    -> log_trace(info, '[Update] Source conditions matched for RuleID ~w', [RuleID]),
       find_all_events_to_update(Update, TConditions, Keys, DictIn, Settings, Candidates),
       
       ( Candidates \= [] 
       -> log_trace(info, '[Update] Found ~w candidates for RuleID ~w', [Candidates, RuleID]),
          apply_updates(event(EventType, DictIn), Candidates, Transformations)
       ;  log_trace(warn, '[Update] No candidate found for RuleID ~w', [RuleID])
       )
    ;  log_trace(info, '[Update] Source conditions FAILED for RuleID ~w', [RuleID]),
       fail
    ).

% Apply a list of matching rules sequentially on an event
apply_matching_rules([], _).

apply_matching_rules([update_rule(RuleID, Priority, _Patterns, SConditions, Updates, Settings, Keys, Transformations)|Rest], event(EventType, DictIn)) :-
    log_trace(info, '[Update] apply_matching_rules Applying update_rule ~w (Priority: ~w) Updates: ~w', [RuleID, Priority, Updates]),

    % Extract first pattern and update
    Updates = [Update | _],
    Update =.. [UpdType, _],

    % Find candidates and apply transformations
    ( get_candidate_to_update(event(EventType, DictIn), event(UpdType, UpdatedDict), Settings, Keys, SConditions, Transformations)
      -> log_trace(info, '[Update] apply_matching_rules Rule applied successfully: ~w', [RuleID]),
         NewDict = UpdatedDict
      ;  log_trace(warn, '[Update] apply_matching_rules Rule FAILED: ~w', [RuleID]),
         NewDict = DictIn
    ),

    % Continue applying remaining rules with updated event
    apply_matching_rules(Rest, event(EventType, NewDict)).

% Check that all keys match between source and target
match_keys(Keys, SourceDict, TargetDict) :-
    forall(member(K, Keys),
        ( get_dict(K, SourceDict, V), get_dict(K, TargetDict, V) )).

% Find all candidate events to update according to TConditions, Keys and time window
find_all_events_to_update(Update, TConditions, Keys, SourceDict, Settings, Candidates) :-
    Update =.. [UpdType, _],
    member(window(TWString), Settings),
    duration_to_seconds(TWString, TimeWindow), % convert window string to seconds
    get_dict(timestamp, SourceDict, SourceTime),
    TimeWindowStruct = window(SourceTime, TimeWindow),
    findall(event(UpdType, TDict),
        (   find_event(UpdType, TimeWindowStruct, Keys, TDict),
            match_all_conditions(TConditions, TDict),
            match_keys(Keys, SourceDict, TDict)
        ),
        Candidates
    ).

% Apply transformations on source event and all candidate events
apply_updates(event(EventType, SourceDict), Candidates, Transformations) :-
    forall(member(event(TType, TDict), Candidates),
        (   update_events(event(EventType, SourceDict), event(TType, TDict), Transformations, EventDictUpdated, UpdDictUpdated),
            ( nonvar(EventDictUpdated) -> replace_event(EventType, SourceDict, EventDictUpdated) ; true ),
            ( nonvar(UpdDictUpdated) -> replace_event(TType, TDict, UpdDictUpdated) ; true )
        )
    ).

