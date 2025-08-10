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
      format('[Update] Created message queue update~n')
    ).

thread_goal_update(ClientID) :-
    format('[Update ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_update_rules :-
    % relative Path to dynamic update rules 
    RuleFile = '../rules/update.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Update] Rules loaded from ~w~n', [RuleFile]).

load_update_rules :-
    format('[Update] Warning: Rules file not found.~n', []).

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
              (format('[Update] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Update] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Update] Received: ~q~n', [EventTerm]),
    update_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Update Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Update] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    update_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Update] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Update] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Update] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Update] Final event to assert: ~q~n', [EventOut]),
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
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

