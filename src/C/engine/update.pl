:- module(update, [ thread_goal_update/1, start_update_loop/0 % Predicate to start the update thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic update_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(update_queue, _), _, fail) ->
        true
    ; message_queue_create(update_queue),
      log_trace(info,'[Update] Created message queue update',[])
    ).

thread_goal_update(ClientID) :-
    log_trace(info,'[Update ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_update_rules :-
    % relative Path to dynamic update rules 
    RuleFile = '../rules/update.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Update] Rules loaded from ~w', [RuleFile]).

load_update_rules :-
    log_trace(info,'[Update] Warning: Rules file not found.', []).

% Main loop of the update thread.
% It continuously fetches messages from its message queue and processes them.

start_update_loop :-
    load_update_rules,
    init_queue, 
    update_loop.

    % Wait for a message.
update_loop :-
    thread_get_message(update_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (log_trace(info,'[Update] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(info,'[Update] Warning: EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Update] Received: ~q', [EventTerm]),
    update_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Update Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Update] Normalized DictIn: ~q', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    update_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    log_trace(info,'[Update] Matched rules: ~q', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Update] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Update] After apply_matching_rules: ~q', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_trace(info,'[Update] Final event to assert: ~q', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(correlate_queue, EventOut).

% update_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
update_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    update_rule(EventType, Priority, RuleID, Conditions, Transformations),
    match_all_conditions(Conditions, DictIn).


assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      (
        retractall(kb_shared:event(Type, Dict)),
        assertz(kb_shared:event(Type, Dict))
      )
    ).

queue_exists(QueueName) :-
    catch(message_queue_property(QueueName, _), _, fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.', [QueueName])
    ).

