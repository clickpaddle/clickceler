:- module(main, [
    start_update/0,
    send_event/1,
    stop_update/0
]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared, [ log_event/1]).
:- use_module(utils).
:- use_module(library(debug)).
:- use_module(update).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic update_rule/6.
:- dynamic update_queue/1.

% --- Thread management ---

start_update :-
    update:init_queue, 
    thread_create(start_update_loop, _, [alias(update_thread)]),
    debug(main, "Abstract toreador started", []).

send_event(Event) :-
    thread_send_message(update_queue, Event).

stop_update :-
    thread_send_message(update_queue, stop),
    message_queue_destroy(update_queue),
    retractall(update_q(_)).

% --- Test code: run at load time ---
:- initialization(run_test).

run_test :-
    start_update,

    % Event that matches the "server" rule
    Event1 = event(cpu, _{
        counter:0,
        env:dev,
        hostname:"server01",
        id:17565601999960001,
        severity:ok,
        status:open,
        origin: 111,
        source: "AAA",
        subnet: "10.0.0.0.24" ,
        load: 50,
        timestamp_collected:1756560199.9969842
    }),
    send_event(Event1),

    % Event that matches the "disk" rule
    Event2 = event(cpu, _{
       counter:0,
        env:dev,
        hostname:"server01",
        id:17565601999960002,
        severity:ok,
        status:open,
        origin: 111,
        source: "AAA",
        subnet: "10.0.0.0.24" ,
        load: 80,
        timestamp_collected:1756560200.9969842

    }),
    send_event(Event2),
    sleep(35),
     print_all_events,
     halt.

