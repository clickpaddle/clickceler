:- module(throttle, [ thread_goal_throttle/1, start_throttle_loop/0 ]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module('../types/types.pl',[subtype/2, valid_severity/1, valid_status/1]).
:- use_module(kb_shared).
:- use_module(library(apply)).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic buffered_event/2.
:- dynamic delay_timer_active/1.
:- dynamic throttle_rule/6.

%:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(throttle_queue, _), _, fail) ->
        true
    ; message_queue_create(throttle_queue),
      log_trace(info,'[Throttle] Created message queue throttle',[])
    ).

thread_goal_throttle(ClientID) :-
    log_trace(info,'[Throttle ~w] Thread started', [ClientID]).

% Load Dynamic rules


load_throttle_rules :-
    % relative Path to dynamic throttle rules 
    RuleFile = '../rules/throttle.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Throttle] Rules loaded from ~w', [RuleFile]).

load_throttle_rules :-
    log_trace(warning,'[Throttle]Rules file not found.', []).

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
              (log_trace(error,'[Throttle] Error: ~w', [E]), fail))
    ->  true; true
    ),
    log_trace(info,'[Throttle] Received: ~q', [EventTerm]),
    throttle_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Throttle Normalized event of the form event(Type, Dict)
% handle_event_throttle(+Event)
handle_event_throttle(event(EventType, DictIn)) :-
    log_trace(info,'[Throttle] Normalized DictIn: ~q', [DictIn]),

    % Find filter rule(s) matching event 
    findall(
        throttle_rule(RuleID, Priority, [Pattern], Conditions, ParamList, Transformations),
        throttle_rule_match(EventType, RuleID, Priority, [Pattern], Conditions, ParamList, Transformations, DictIn), 
        RuleList
    ),
    log_trace(info,'[Throttle] Matched rules: ~q', [RuleList]),

    % Sort filter rules by decreasing priority (100 > 10)
    sort(2, @>=, RuleList, SortedRules),
    log_trace(info,'[Throttle] Sorted rules by priority: ~q', [SortedRules]),

    % Apply throttle rules
    apply_throttle_rules(SortedRules, event(EventType, DictIn)).


apply_throttle_rules([], Event) :-
    % No rules left to apply, send event directly
    safe_thread_send_message(abstract_queue, Event),
    log_trace(info,'[Throttle] No more throttle rules, Prepare to send event with  a delay',[]).

apply_throttle_rules([throttle_rule(RuleID, _Priority, [_Pattern], _Conditions, ParamList, Transformations)| Rest], event(EventType, DictIn)) :-
    % Extract params
    log_trace(info,'[Throttle] Parsing settings: ~q ',[ParamList]), 
    ParamList = [Params],
    log_trace(info,'[Throttle] Parsing settings: ~q ',[Params]), 
    get_dict(limit, Params, Limit),
    get_dict(window, Params, Window),
    ( get_dict(delay, Params, Delay) -> true ; Delay = 0 ),

    % Extract transformation to apply (send_first or send_last)
    ( member(send_first, Transformations) -> SendMethod = send_first
    ; member(send_last, Transformations) -> SendMethod = send_last
    ; SendMethod = send_first % Default to send_first if unspecified
    ),

    log_trace(info,'[Throttle] Apply Rule ~w with Limit=~w, Window=~w, Delay=~w, SendMethod=~w', [RuleID, Limit, Window, Delay, SendMethod]),

    % Buffer the event
    assertz(buffered_event(event(EventType, DictIn))),

   %t Get buffer size and oldest event time
    buffer_size(BufferSize),
    ( buffer_oldest_event_time(OldestTime) -> true ; OldestTime = 0 ),
    current_time(CurrentTime),
    Elapsed is CurrentTime - OldestTime, 
    
( BufferSize =:= 1,
      SendMethod == send_first
    ->
        % First event + send_first: send after Delay, do NOT clear buffer
        send_first(RuleID,Delay, event(EventType, DictIn), Limit)
    ;  duration_to_seconds(Window, WSec),
       (BufferSize >= Limit ; Elapsed >= WSec)
    ->
        % Buffer full or window expired
        ( SendMethod == send_last
        ->
            get_last_buffered_event(event(LastEventType, LastDict)),
            send_last(RuleID,Delay, event(LastEventType, LastDict), Limit),
            clear_buffer(RuleID)
        ; SendMethod == send_first
        ->
            clear_buffer(RuleID)
        )
    ;   
        % Otherwise, keep buffering without sending
        true
    ),
    % Apply next rules (if any)
    apply_throttle_rules(Rest, event(EventType, DictIn)).


% Buffer helpers
get_last_buffered_event(event(EventOut, DictOut)) :-
    findall(event(EvtType, Dict), buffered_event(event(EvtType, Dict)), Events),
    last(Events, event(EventOut, DictOut)). 

buffer_size(Size) :-
    findall(E, buffered_event(E), Events),
    length(Events, Size).

buffer_oldest_event_time(OldestTime) :-
    findall(T, (buffered_event(event(_, Dict)), get_dict(timestamp_collected, Dict, T)), Times),
    Times \= [],
    min_list(Times, OldestTime).

clear_buffer(RuleID) :-
    retractall(buffered_event(_)),
    log_trace(info,'[Throttle] Buffer cleared: ~w ',[RuleID]).

throttle_rule_match(EventType, RuleID, Priority, [Pattern], CondsDict, ParamList, TransDict, DictIn) :-
    throttle_rule(RuleID, Priority, [Pattern], CondsDict, ParamList, TransDict),
    % Extract Event Type from throttle_rule 
    Pattern =.. [RuleEventType, E],
    E = DictIn,
    % Verify que EventType s a sub-type of  RuleEventType
    is_subtype(EventType, RuleEventType),
    log_trace(info,'[Throttle] ~w is subtype of ~w', [EventType, RuleEventType]),
    ( match_all_conditions(CondsDict, E) -> 
            % Conditions respected : rule is ok 
            log_trace(info,'[throttle] Matched rule: ~q', [RuleID])
    ; 
        assert_event(event(EventType, DictIn)), 
         safe_thread_send_message(abstract_queue,event(EventType, DictIn)),
        !, fail
    ).


send_first(RuleID,Delay, event(Type, Dict), Limit) :-
    put_dict(counter,Dict,Limit,DictOut),
    Event = event(Type, DictOut),
    log_trace(info,'[Throttle] Delaying ~w s RuleID: ~w send_first; ~w ',[Delay,RuleID,Event]),
    thread_create(
        delayed_safe_send(RuleID, Delay, Event),
        _ThreadId,
        [detached(true)]
    ).

send_last(RuleID,Delay, event(Type, Dict), Limit) :-
    put_dict(counter,Dict,Limit,DictOut),
    Event = event(Type, DictOut),
    log_trace(info,'[Throttle] Delaying ~w s ruleID: ~w send_last; ~w ',[Delay,RuleID,Event]),
    thread_create(
        delayed_safe_send(RuleID, Delay, Event),
        _ThreadId,
        [detached(true)]
    ).

delayed_safe_send(RuleID,Delay, Event) :-
    duration_to_seconds(Delay,Seconds), 
    sleep(Seconds),
    assert_event(Event),
    log_trace(info,'[Throttle] Delayed ~w  RuleID: ~w throttling event sent to next phase: ~w',[RuleID,Seconds,Event]),
    safe_thread_send_message(abstract_queue,Event).

% Queue existence and safe send predicate
queue_exists(QueueName) :-
    catch(message_queue_property(QueueName, _), _, fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        assert_event(Message),
        log_event(Message),  
        thread_send_message(QueueName, Message)
     
    ; format(user_error, '[ERROR] Message queue ~w does not exist. Message not sent.', [QueueName])
    ).


current_time(Time) :-
    get_time(Time).


