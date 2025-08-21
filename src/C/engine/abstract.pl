:- module(abstract, [ thread_goal_abstract/1, start_abstract_loop/0 % Predicate to start the abstract thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/5.
%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(abstract_queue, _), _, fail) ->
        true
    ; message_queue_create(abstract_queue),
      log_trace(info,'[Abstract] Created message queue abstract',[])
    ).

thread_goal_abstract(ClientID) :-
    log_trace(info,'[Abstract ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_abstract_rules :-
    % relative Path to dynamic abstract rules 
    RuleFile = '../rules/abstract.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Abstract] Rules loaded from ~w', [RuleFile]).

load_abstract_rules :-
    log_trace(info,'[Abstract] Warning: Rules file not found.', []).

% Main loop of the abstract thread.
% It continuously fetches messages from its message queue and processes them.

start_abstract_loop :-
    load_abstract_rules,
    init_queue, 
    abstract_loop.

    % Wait for a message.
abstract_loop :-
    thread_get_message(abstract_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (log_trace(error,'[Abstract] Error: ~w', [E]), fail))
    ->  true
    ;   log_trace(warning,'[Abstract] Warning: EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Abstract] Received: ~q', [EventTerm]),
    abstract_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Abstract Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] Normalized DictIn: ~q', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    abstract_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    log_trace(info,'[Abstract] Matched rules: ~q', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    log_trace(info,'[Abstract] Sorted rules by priority: ~q', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    log_trace(info,'[Abstract] After apply_matching_rules: ~q', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    log_trace(info,'[Abstract] Final event to assert: ~q', [EventOut]),
    log_event(EventOut),
    safe_thread_send_message(update_queue, EventOut).


% abstract_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
abstract_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    abstract_rule(EventType, Priority, RuleID, Conditions, Transformations),
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




----------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Closure handling (event closed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_event(EventID) :-
    event(EventID, _, closed, _, _, LinkedAbstracts),
    forall(member(AbstractID, LinkedAbstracts),
           remove_from_abstract_contrib(AbstractID, EventID)),
    % Check if abstract has no more contributors
    forall(existing_abstract(_, AbstractID),
           (abstract(AbstractID, _, _, _, _, Contrib),
            Contrib = [],
            set_abstract_field(AbstractID, status, closed),
            trace(info, "Abstract updated", []))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates (assumed implemented)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generate_abstract(EventID, AbstractID)
% add_to_abstract_contrib(AbstractID, EventID)
% add_abstract_to_event(EventID, AbstractID)
% set_abstract_field(AbstractID, Field, Value)
% existing_abstract(EventID, AbstractID)
% remove_from_abstract_contrib(AbstractID, EventID)

