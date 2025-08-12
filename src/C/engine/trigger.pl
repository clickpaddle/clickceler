:- module(trigger, [ thread_goal_trigger/1, start_trigger_loop/0 % Predicate to start the trigger thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic trigger_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(trigger_queue, _), _, fail) ->
        true
    ; message_queue_create(trigger_queue),
      log_trace(info,'[Trigger] Created message queue trigger',[])
    ).

thread_goal_trigger(ClientID) :-
    log_trace(info,'[Trigger ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_trigger_rules :-
    % relative Path to dynamic trigger rules 
    RuleFile = '../rules/trigger.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Trigger] Rules loaded from ~w', [RuleFile]).

load_trigger_rules :-
    log_trace(info,'[Trigger] Warning: Rules file not found.', []).

% Main loop of the trigger thread.
% It continuously fetches messages from its message queue and processes them.

start_trigger_loop :-
    load_trigger_rules,
    init_queue, 
    trigger_loop.

    % Wait for a message.
trigger_loop :-
    thread_get_message(trigger_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (log_trace(info,'[Trigger] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(info,'[Trigger] Warning: EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Trigger] Received: ~q', [EventTerm]),
    trigger_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Trigger Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Trigger] Normalized DictIn: ~q', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    trigger_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    log_trace(info,'[Trigger] Matched rules: ~q', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Trigger] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Trigger] After apply_matching_rules: ~q', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_trace(info,'[Trigger] Final event to assert: ~q', [EventOut]),
    log_event(EventOut).


% trigger_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
trigger_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    trigger_rule(EventType, Priority, RuleID, Conditions, Transformations),
    match_all_conditions(Conditions, DictIn).


assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      (
        retractall(kb_shared:event(Type, Dict)),
        assertz(kb_shared:event(Type, Dict))
      )
    ).


