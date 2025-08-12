:- module(filter, [ thread_goal_filter/1, start_filter_loop/0 ]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module('../types/types.pl',[subtype/2, valid_severity/1, valid_status/1]).
:- use_module(kb_shared,[eventlog_mutex/1, log_event/1, is_subtype/2, print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic filter_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(filter_queue, _), _, fail) ->
        true
    ; message_queue_create(filter_queue),
      log_trace(info,'[Filter] Created message queue filter',[])
    ).

thread_goal_filter(ClientID) :-
    log_trace(info,'[Filter ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_filter_rules :-
    % relative Path to dynamic filter rules 
    RuleFile = '../rules/filter.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Filter] Rules loaded from ~w', [RuleFile]).

load_filter_rules :-
    log_trace(warning,'[Filter] Warning: Rules file not found.', []).

% Main loop of the filter thread.
% It continuously fetches messages from its message queue and processes them.

start_filter_loop :-
    load_filter_rules,
    init_queue, 
    filter_loop.

    % Wait for a message.
filter_loop :-
    thread_get_message(filter_queue, EventTerm),
    (   catch(handle_event_filter(EventTerm), E,
              (log_trace(error,'[Filter] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(warning,'[Filter] EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Filter] Received: ~q', [EventTerm]),
    filter_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Filter Normalized event of the form event(Type, Dict)
handle_event_filter(event(EventType, DictIn)) :-
    log_trace(info,'[Filter] Normalized DictIn: ~q', [DictIn]),

    % Find filter rule(s) matching event 
    findall( filter_rule(RuleID, Priority, [Pattern], Conditions, Transformations),
        filter_rule_match(EventType, RuleID, Priority, [Pattern], Conditions, Transformations, DictIn), RuleList),
        log_trace(info,'[Filter] Matched rules: ~q', [RuleList]),

    % Sort filter rules by decreasing priority (100 > 10)
    sort(2, @>=, RuleList, SortedRules),
    log_trace(info,'[Filter] Sorted rules by priority: ~q', [SortedRules]),

    % Apply filter_rule 
    (   apply_filter_rules(SortedRules, event(EventType, DictIn))
    ->  ( EventOut = event(EventType, DictIn),
          log_event(EventOut),
          safe_thread_send_message(throttle_queue, EventOut))
    ;   log_trace(info,'[Filter] Event was rejected.', [])
    ).

% filter_rule_match(+EventType, -RuleID, , -Priority, -[Pattern], -Conditions, -Transformations, +DictIn)
filter_rule_match(EventType, RuleID, Priority, [Pattern], CondsDict, TransDict, DictIn) :-
    filter_rule(RuleID, Priority, [Pattern], CondsDict, TransDict),
    Pattern =.. [RuleEventType, E],
    E = DictIn,
    is_subtype(EventType, RuleEventType),
    log_trace(info,'[Filter] = ~w is subtype of ~w', [EventType, RuleEventType]),
    (
        ( match_all_conditions(CondsDict, E),  % Conditions met: normal rule match
          log_trace(info,'[Filter] Matched rule: ~q', [RuleID])  
        )
    ;
        (   % Otherwise, if conditions fail but action is pass => delete event
            \+ match_all_conditions(CondsDict, E),
            member(pass, TransDict),
            log_trace(info,'[Filter] Conditions failed but action is pass, deleting event', []),
            delete_event(event(EventType, DictIn)),
            fail
        )
    ).

apply_filter_rules([], Event) :-
    % No matching rules: continue by default
    assert_event(Event),
    !.

apply_filter_rules([filter_rule(_, _, [_], _, Actions) | _], event(Type, Dict)) :-
    member(Action, Actions),
    (   Action = nopass
    ->  (delete_event(event(Type, Dict)),
         log_trace(info,'[Filter] Action: ~q ~q ', [Action, event(Type,Dict)]),
         !, fail)  % Stop after deletion of event 
    ;   Action = pass
    ->  (assert_event(event(Type, Dict)),
         log_trace(info,'[Filter] Action: ~q ~q ', [Action, event(Type, Dict)]),
         !)
    ;   true
    ).

apply_filter_rules([_ | Rest], Event) :-
    apply_filter_rules(Rest, Event).

delete_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      (
        log_trace(info,'[Filter] ~w Rejected. Stop processing ', [event(Type, Dict)]),
        retractall(kb_shared:event(Type, Dict))
      )
    ).

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

