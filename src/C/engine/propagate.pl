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
      log_trace(info,'[Propagate] Created message queue propagate',[])
    ).

thread_goal_propagate(ClientID) :-
    log_trace(info,'[Propagate ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_propagate_rules :-
    % relative Path to dynamic propagate rules 
    RuleFile = '../rules/propagate.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Propagate] Rules loaded from ~w', [RuleFile]).

load_propagate_rules :-
    log_trace(warning,'[Propagate] Rules file not found.', []).

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
              (log_trace(info,'[Propagate] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(info,'[Propagate] Warning: EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Propagate] Received: ~q', [EventTerm]),
    propagate_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Propagate Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Propagate] Normalized DictIn: ~q', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    propagate_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    log_trace(info,'[Propagate] Matched rules: ~q', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Propagate] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Propagate] After apply_matching_rules: ~q', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_trace(info,'[Propagate] Final event to assert: ~q', [EventOut]),
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
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.', [QueueName])
    ).

