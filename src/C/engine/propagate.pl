:- module(propagate, [ thread_goal_propagate/1, start_propagate_loop/0 % Predicate to start the propagate thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic propagate_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(propagate_queue, _), _, fail) ->
        true
    ; message_queue_create(propagate_queue),
      format('[Propagate] Created message queue propagate~n')
    ).

thread_goal_propagate(ClientID) :-
    format('[Propagate ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_propagate_rules :-
    % relative Path to dynamic propagate rules 
    RuleFile = '../rules/propagate.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Propagate] Rules loaded from ~w~n', [RuleFile]).

load_propagate_rules :-
    format('[Propagate] Warning: Rules file not found.~n', []).

% Main loop of the propagate thread.
% It continuously fetches messages from its message queue and processes them.

start_propagate_loop :-
    load_propagate_rules,
    init_queue, 
    propagate_loop.

    % Wait for a message.
propagate_loop :-
    thread_get_message(propagate_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (format('[Propagate] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Propagate] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Propagate] Received: ~q~n', [EventTerm]),
    propagate_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Propagate Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Propagate] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    propagate_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Propagate] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Propagate] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Propagate] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Propagate] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut).


% propagate_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
propagate_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    propagate_rule(EventType, Priority, RuleID, Conditions, Transformations),
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

