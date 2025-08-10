:- module(correlate, [ thread_goal_correlate/1, start_correlate_loop/0 % Predicate to start the correlate thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic correlate_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(correlate_queue, _), _, fail) ->
        true
    ; message_queue_create(correlate_queue),
      format('[Correlate] Created message queue correlate~n')
    ).

thread_goal_correlate(ClientID) :-
    format('[Correlate ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_correlate_rules :-
    % relative Path to dynamic correlate rules 
    RuleFile = '../rules/correlate.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Correlate] Rules loaded from ~w~n', [RuleFile]).

load_correlate_rules :-
    format('[Correlate] Warning: Rules file not found.~n', []).

% Main loop of the correlate thread.
% It continuously fetches messages from its message queue and processes them.

start_correlate_loop :-
    load_correlate_rules,
    init_queue, 
    correlate_loop.

    % Wait for a message.
correlate_loop :-
    thread_get_message(correlate_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (format('[Correlate] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Correlate] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Correlate] Received: ~q~n', [EventTerm]),
    correlate_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Correlate Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Correlate] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    correlate_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Correlate] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Correlate] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Correlate] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Correlate] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(execute_queue, EventOut).


% correlate_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
correlate_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    correlate_rule(EventType, Priority, RuleID, Conditions, Transformations),
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

