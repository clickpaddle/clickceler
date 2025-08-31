:- module(refine, [ thread_goal_refine/1, start_refine_loop/0 % Predicate to start the refine thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module('../types/types.pl',[subtype/2, valid_status/1, valid_severity/1]).
:- use_module(kb_shared,[eventlog_mutex/1, log_event/1, is_subtype/2, print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic refine_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(refine_queue, _), _, fail) ->
        true
    ; message_queue_create(refine_queue),
      log_trace(info,'[Refine] Created message queue refine',[])
    ).

thread_goal_refine(ClientID) :-
    log_trace(info,'[Refine ~w] Thread started', [ClientID]),
    init_queue, 
    load_refine_rules.

% Load Dynamic rules


load_refine_rules :-
    % relative Path to dynamic refine rules 
    RuleFile = '../rules/refine.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Refine] Rules loaded from ~w', [RuleFile]).

load_refine_rules :-
    log_trace(warning,'[Refine] Rules file not found.', []).

% Main loop of the refine thread.
% It continuously fetches messages from its message queue and processes them.

start_refine_loop :-
    refine_loop.

    % Wait for a message.
refine_loop :-
    thread_get_message(refine_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (log_trace(error,'[Refine] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(warning,'[Refine]EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Refine] Received: ~q', [EventTerm]),
    refine_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Refine Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Refine] Normalized DictIn: ~q', [DictIn]),
    findall( refine_rule(RuleID, Priority, [Pattern],Conditions, Transformations),
    refine_rule_match(EventType, RuleID, Priority, [Pattern], Conditions, Transformations, DictIn), RuleList),
    log_trace(info,'[Refine] Matched rules: ~q', [RuleList]),
    % Sort by decreasing PRIORITY 100 > PRIORITY 10 
    sort(2, @>=, RuleList, SortedRules),
    log_trace(info,'[Refine] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Refine] After apply_matching_rules: ~q', [DictOut]),
    EventOut = event(EventType, DictOut),
    log_trace(info,'[Refine] Final event to assert: ~q', [EventOut]),
    safe_thread_send_message(filter_queue, EventOut).


% ===== REFINE RULES =====

refine_rule_match(EventType, RuleID,  Priority, [Pattern],CondsDict, TransDict, DictIn) :-
    refine_rule(RuleID, Priority,[Pattern], CondsDict, TransDict),
    Pattern =.. [RuleEventType, E],
    E = DictIn,
    is_subtype(EventType, RuleEventType),
    log_trace(info,'[Refine] = ~w is subtype of ~w', [EventType, RuleEventType]),  
    match_all_conditions(CondsDict, E).

%% === APPLY RULE ENGINE ===

apply_matching_rules([], Dict, Dict).
apply_matching_rules([refine_rule( RuleID, _Priority, [_Pattern], _Conds, Transforms) | Rest], DictIn, DictOut) :-
    log_trace(info,'[Refine] Applying rule ~w', [RuleID]),
    log_trace(info,'[Refine Transforms: ~w ',[Transforms]),
    apply_transformations(Transforms, DictIn, DictNext),
    apply_matching_rules(Rest, DictNext, DictOut).

queue_exists(QueueName) :-
    catch(message_queue_property(QueueName, _), _, fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.', [QueueName])
    ).

