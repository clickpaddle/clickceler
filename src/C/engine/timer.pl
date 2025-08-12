:- module(timer, [ thread_goal_timer/1, start_timer_loop/0 % Predicate to start the timer thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic timer_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(timer_queue, _), _, fail) ->
        true
    ; message_queue_create(timer_queue),
      log_trace(info,'[Timer] Created message queue timer~n',[])
    ).

thread_goal_timer(ClientID) :-
    log_trace(info,'[Timer ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_timer_rules :-
    % relative Path to dynamic timer rules 
    RuleFile = '../rules/timer.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Timer] Rules loaded from ~w~n', [RuleFile]).

load_timer_rules :-
    log_trace(info,'[Timer] Warning: Rules file not found.~n', []).

% Main loop of the timer thread.
% It continuously fetches messages from its message queue and processes them.

start_timer_loop :-
    load_timer_rules,
    init_queue, 
    timer_loop.

    % Wait for a message.
timer_loop :-
    thread_get_message(timer_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (log_trace(error,'[Timer] Error: ~w~n', [E]), fail))
    ->  true
    ;   log_trace(warning,'[Timer]EventTerm not handled: ~w~n', [EventTerm])
    ),
    log_trace(info,'[Timer] Received: ~q~n', [EventTerm]),
    timer_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Timer Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Timer] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    timer_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    log_trace(info,'[Timer] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Timer] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Timer] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_trace(info,'[Timer] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(propagate_queue,EventOut).

% timer_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
timer_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    timer_rule(EventType, Priority, RuleID, Conditions, Transformations),
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

