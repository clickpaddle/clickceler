:- module(main, [
    start_filter/0,
    send_event/1,
    stop_filter/0
]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared).
:- use_module(utils).
:- use_module(library(debug)).
:- use_module(filter).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic filter_rule/6.
:- dynamic filter_queue/1.

% --- Thread management ---

start_filter :-
    filter:init_queue, 
    thread_create(start_filter_loop, _, [alias(filter_thread)]),
    debug(main, "Filter thread started", []).

send_event(Event) :-
    thread_send_message(filter_queue, Event).

stop_filter :-
    thread_send_message(filter_queue, stop),
    message_queue_destroy(filter_queue),
    retractall(filter_q(_)).

% --- Test code: run at load time ---
:- initialization(run_test).

run_test :-
    start_filter,

    % Event that matches the "server" rule
    Event1 = event(disk, _{
        counter:0,
        env:dev,
        hostname:"server01",
        severity:ok,
        status:open,
        origin: 111,
        source: "AAA"
    }),
    send_event(Event1),
       Event2 = event(cpu, _{
        counter:0,
        env:prod,
        hostname:"server01",
        severity:critical,
        status:open,
        origin: 111,
        source: "AAA"
    }),
    send_event(Event2),
 
     sleep(5), 
     print_all_events.

