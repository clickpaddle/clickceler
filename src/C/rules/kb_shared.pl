:- module(kb_shared, [thread_goal_kb_shared/1,start_kb_shared_loop/0, assert_json_event/2, event/2, print_all_events/0]).

:- dynamic event/2.

thread_goal_kb_shared(ClientID) :-
    format('[kb_shared ~w] Thread started~n', [ClientID]).

% Principal Loop
start_kb_shared_loop :-
    thread_self(Main),
    format("[Main ~w] Starting kb_shared loop~n", [Main]),
    loop.

% Get and Handle messages
loop :-
    thread_get_message(Message),
    handle_message(Message),
    loop.

% Store Events safely
handle_message(json_event(Id, Json)) :-
    catch(
        assertz(event(Id, Json)),
        Err,
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w~n", [Id, Err]), fail)
    ),
    format("[Main] Event asserted: ~w => ~w~n", [Id, Json]).

handle_message(stop) :-
    format("[Main] Stopping kb_shared~n"),
    !.

% Public interface to assert event safely
assert_json_event(Id, Json) :-
    catch(
        assertz(event(Id, Json)),
        Err,
        (format("[kb_shared ERROR] Failed to assert event ~w: ~w~n", [Id, Err]), fail)
    ).

print_all_events :-
    forall(event(Id, Json), format("Event ~w: ~w~n", [Id, Json])).

