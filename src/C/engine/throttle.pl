:- module(throttle, [ thread_goal_throttle/1, start_throttle_loop/0 ]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module('../types/types.pl',[subtype/2, valid_severity/1, valid_status/1]).
:- use_module(kb_shared,[eventlog_mutex/1, log_event/1, is_subtype/2, print_all_events/1]).
:- use_module(library(apply)).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic buffered_event/2
:- dynamic delay_timer_active/1
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
    (   catch(handle_event_throttle(EventTerm), E,
              (format('[Throttle] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Throttle] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Throttle] Received: ~q~n', [EventTerm]),
    throttle_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Throttle Normalized event of the form event(Type, Dict)
% handle_event_throttle(+Event)
handle_event_throttle(event(EventType, DictIn)) :-
    format('[Throttle] Normalized DictIn: ~q~n', [DictIn]),

    % Find filter rule(s) matching event 
    findall(
        throttle_rule(RuleID, Priority, [Pattern], Conditions, Params, Transformations),
        throttle_rule_match(EventType, RuleID, Priority, [Pattern], Conditions, Params, Transformations, DictIn), RuleList),
        format('[RuleList] Matched rules: ~q~n', [RuleList]),

    % Sort filter rules by decreasing priority (100 > 10)
    sort(2, @>=, RuleList, SortedRules),
    format('[Throttle] Sorted rules by priority: ~q~n', [SortedRules]),

    % Apply throttle rules
    apply_throttle_rules(SortedRules, event(EventType, DictIn)).

apply_throttle_rules([], event(EventType, DictIn)) :-
    % No rules left to apply, send event directly
    format('[ApplyThrottle] No more throttle rules, sending event directly~n').

apply_throttle_rules([throttle_rule(RuleID, Priority, [Pattern], Conditions, Params, Transformations)], event(EventType, DictIn)) :-
    % Extract params
    get_dict(limit, Params, Limit),
    get_dict(window, Params, Window),
    ( get_dict(delay, Params, Delay) -> true ; Delay = 0 ),

    % Extract transformation to apply (send_first or send_last)
    ( member(send_first, Transformations) -> SendMethod = send_first
    ; member(send_last, Transformations) -> SendMethod = send_last
    ; SendMethod = send_first % Default to send_first if unspecifie
    ),

    format('[Throttle] Apply Rule ~w with Limit=~w, Window=~w, Delay=~w, SendMethod=~w~n',
           [RuleID, Limit, Window, Delay, SendMethod]),

    % Buffer the event
    assertz(buffered_event(event(EventType, DictIn))),

   % Get buffer size and oldest event time
    buffer_size(BufferSize),
    ( buffer_oldest_event_time(OldestTime) -> true ; OldestTime = 0 ),
    current_time(CurrentTime),
    Elapsed is CurrentTime - OldestTime, 
    
( BufferSize =:= 1,
      SendMethod == send_first
    ->
        % First event + send_first: send after Delay, do NOT clear buffer
        send_first(Delay, event(EventType, DictIn))
    ; (BufferSize >= Limit ; Elapsed >= Window)
    ->
        % Buffer full or window expired
        ( SendMethod == send_last
        ->
            get_last_buffered_event(event(LastEventType, LastDict)),
            send_last(Delay, event(LastEventType, LastDict)),
            clear_buffer
        ; SendMethod == send_first
        ->
            get_first_buffered_event(event(FirstEventType, FirstDict)),
            clear_buffer
        )
    ;   
        % Otherwise, keep buffering without sending
        true
    ),
    % Apply next rules (if any)
    apply_throttle_rules(Rest, Event).
 
% Buffer helpers
get_last_buffered_event(event(EventOut, DictOut)) :-
    findall(event(EvtType, Dict), buffered_event(event(EvtType, Dict)), Events),
    last(Events, event(EventOut, DictOut)). 

buffer_size(Size) :-
    findall(E, buffered_event(E), Events),
    length(Events, Size).

buffer_oldest_event_time(OldestTime) :-
    findall(T, (buffered_event(event(_, Dict)), get_dict(timestamp, Dict, T)), Times),
    Times \= [],
    min_list(Times, OldestTime).

clear_buffer :-
    retractall(buffered_event(_)),
    format('[Throttle] Buffer cleared~n').

throttle_rule_match(EventType, RuleID, Priority, [Pattern], CondsDict, Params, TransDict, DictIn) :-
    throttle_rule(RuleID, Priority, [Pattern], CondsDict, Params, TransDict),
    % Extract Event Type from throttle_rule 
    Pattern =.. [RuleEventType, E],
    E = DictIn,
    % Verify que EventType s a sub-type of  RuleEventType
    is_subtype(EventType, RuleEventType),
    format('[Throttle] ~w is subtype of ~w~n', [EventType, RuleEventType]),
    ( match_all_conditions(CondsDict, E) -> 
            % Conditions respected : rule is ok 
            format('[throttle] Matched rule: ~q~n', [RuleID])
    ; 
        assert_event(event(EventType, DictIn)), 
         log_event(event(EventType, DictIn)),
         safe_thread_send_message(abstract_queue,event(EventType, DictIn)),
        !, fail
    ).


send_first(Delay, Event) :-
    thread_create(
        delayed_safe_send(Delay, Event),
        _ThreadId,
        [detached(true)]
    ).

send_last(Delay, Event) :-
    thread_create(
        delayed_safe_send(Delay, Event),
        _ThreadId,
        [detached(true)]
    ).

delayed_safe_send(Delay, Event) :-
    sleep(Delay),
    assert_event(Event),
    log_event(Event),
    safe_thread_send_message(abstract_queue,Event).

% Queue existence and safe send predicate
queue_exists(QueueName) :-
    catch(message_queue_property(QueueName, _), _, fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

% Assert event safely under mutex to kb_shared:event/2
assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      (
        retractall(kb_shared:event(Type, Dict)),
        assertz(kb_shared:event(Type, Dict))
      )
    ).

current_time(Time) :-
    get_time(Time).


