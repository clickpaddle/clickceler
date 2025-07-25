:- module(refine, [ thread_goal_refine/1, start_refine_loop/0 % Predicate to start the refine thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared).          % Assuming kb_shared module exists and is correctly defined

thread_goal_refine(ClientID) :-
    format('[Refine ~w] Thread started~n', [ClientID]).

% File to store refined events
log_file('../logs/refined_events.log').

% This predicate is not used by start_refine_loop as currently defined.
% It can be removed or its purpose re-evaluated if needed for thread creation arguments.
% thread_goal_refine(ClientID) :-
%     format('[refine ~w] Thread started~n', [ClientID]).

% Start the refine loop in the thread.
% It creates a new thread with the alias 'refine' and puts it into a detached loop.
start_refine_loop :-
    thread_create(refine_thread_loop, _, [alias(refine), detached(true)]),
    format('[refine] Thread started and waiting for messages...~n', []).

% Main loop of the refine thread.
% It continuously fetches messages from its message queue and processes them.
refine_thread_loop :-
    refine_loop.

refine_loop :-
    % Wait for a message.
    thread_get_message(Msg),
    (   Msg = thread_exit
    ->  % Termination signal received, the thread exits gracefully
        format('[refine] Thread exiting gracefully.~n', [])
    ;   % Handle other messages
        handle_message(Msg)
    ),
    % Continue the loop unless the 'thread_exit' signal was received
    (   Msg = thread_exit -> true ; refine_loop ).

% Handle incoming messages
% EventData is the already-parsed Prolog term (e.g., a dict), not a JSON string.
handle_message(refine_event(Id, EventData)) :-
    must_be(nonvar, Id),
    must_be(nonvar, EventData),
    format("[refine] Received refine_event for ~w~n", [Id]),
    catch(refine_logic(Id, EventData), % Pass EventData, not Json
          E,
          format(user_error, "[refine ERROR] Exception processing event ~w: ~w~n", [Id, E])).

handle_message(stop) :- % This is a custom stop message you can send
    format("[refine] Stopping refine thread~n"),
    !. % stop loop by not calling loop again (handled by refine_loop now)

handle_message(UnexpectedMsg) :-
    % Log unexpected messages for debugging
    format(user_error, "[refine WARNING] Received unexpected message: ~q~n", [UnexpectedMsg]).

% Refine logic: optionally transform EventData, then assert to kb_shared and log event
refine_logic(Id, EventData) :-
    % Example: You might transform EventData here if needed
    % TransformedEventData = EventData, % Or apply some transformation

    format("[refine INFO] Processing event ID=~w~n", [Id]),

    % Attempt to assert the event to kb_shared
    (   catch(kb_shared:assert_json_event(Id, EventData), Err, % Pass EventData
              (format(user_error, "[refine ERROR] Failed to assert event ~w to kb_shared: ~w~n", [Id, Err]), fail))
    ->  format("[refine INFO] Successfully asserted event ~w to kb_shared~n", [Id])
    ;   format(user_error, "[refine ERROR] Assertion failed for event ~w (catch block did not fail)~n", [Id])
    ),

    % Log the refined event
    log_refined_event(Id, EventData).

% Appends the refined event to the log file.
% Uses setup_call_cleanup/3 to ensure the file stream is always closed, even on errors.
log_refined_event(Id, EventData) :-
    log_file(File),
    setup_call_cleanup(
        open(File, append, Stream, [encoding(utf8)]), % Open the file in append mode, UTF-8 encoding
        (   get_time(TS), % Get the current timestamp
            format_time(atom(DateTime), '%Y-%m-%d %H:%M:%S', TS), % Format the timestamp
            % Use ~q for a "quoted" Prolog representation of complex terms (like dictionaries)
            format(Stream, "[~w] Refined Event ~w: ~q~n", [DateTime, Id, EventData])
        ),
        close(Stream) % Close the file stream
    ).

