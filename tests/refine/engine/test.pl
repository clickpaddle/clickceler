:- module(main, [
    start_refine/0,
    send_event/1,
    stop_refine/0
]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared).
:- use_module(utils).
:- use_module(library(debug)).
:- use_module(refine).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic refine_rule/6.
:- dynamic refine_queue/1.

% --- Thread management ---

start_refine :-
    refine:init_queue, 
    thread_create(start_refine_loop, _, [alias(refine_thread)]),
    debug(main, "Refine thread started", []).

send_event(Event) :-
    thread_send_message(refine_queue, Event).

stop_refine :-
    thread_send_message(refine_queue, stop),
    message_queue_destroy(refine_queue),
    retractall(refine_q(_)).

% --- Test code: run at load time ---
:- initialization(run_test).

run_test :-
    start_refine,

    % Event that matches the "server" rule
    Event1 = event(server, _{
        counter:0,
        env:dev,
        hostname:"server01",
        severity:critical,
        status:open,
        origin: 111,
        source: "AAA",
        timestamp_collected:1756560199.9969842
    }),
    send_event(Event1),
     sleep(5), 
     print_all_events.

