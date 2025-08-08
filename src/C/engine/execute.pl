:- module(execute, [ thread_goal_execute/1, start_execute_loop/0 % Predicate to start the execute thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic execute_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(execute_queue, _), _, fail) ->
        true
    ; message_queue_create(execute_queue),
      format('[Execute] Created message queue execute~n')
    ).

thread_goal_execute(ClientID) :-
    format('[Execute ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_execute_rules :-
    % relative Path to dynamic execute rules 
    RuleFile = '../rules/execute.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Execute] Rules loaded from ~w~n', [RuleFile]).

load_execute_rules :-
    format('[Execute] Warning: Rules file not found.~n', []).

% Main loop of the execute thread.
% It continuously fetches messages from its message queue and processes them.

start_execute_loop :-
    load_execute_rules,
    init_queue, 
    execute_loop.

    % Wait for a message.
execute_loop :-
    thread_get_message(execute_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (format('[Execute] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Execute] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Execute] Received: ~q~n', [EventTerm]),
    execute_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Execute Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Execute] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    execute_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Execute] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Execute] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Execute] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Execute] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(timer_queue, EventOut).


% execute_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
execute_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    execute_rule(EventType, Priority, RuleID, Conditions, Transformations),
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

