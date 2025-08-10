:- module(throttle, [ thread_goal_throttle/1, start_throttle_loop/0 % Predicate to start the throttle thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic throttle_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(throttle_queue, _), _, fail) ->
        true
    ; message_queue_create(throttle_queue),
      format('[Throttle] Created message queue throttle~n')
    ).

thread_goal_throttle(ClientID) :-
    format('[Throttle ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_throttle_rules :-
    % relative Path to dynamic throttle rules 
    RuleFile = '../rules/throttle.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Throttle] Rules loaded from ~w~n', [RuleFile]).

load_throttle_rules :-
    format('[Throttle] Warning: Rules file not found.~n', []).

% Main loop of the throttle thread.
% It continuously fetches messages from its message queue and processes them.

start_throttle_loop :-
    load_throttle_rules,
    init_queue, 
    throttle_loop.

    % Wait for a message.
throttle_loop :-
    thread_get_message(throttle_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (format('[Throttle] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Throttle] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Throttle] Received: ~q~n', [EventTerm]),
    throttle_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Throttle Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Throttle] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    throttle_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Throttle] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Throttle] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Throttle] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Throttle] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(abstract_queue, EventOut).

% throttle_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
throttle_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    throttle_rule(EventType, Priority, RuleID, Conditions, Transformations),
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

