:- module(main, [
    start_abstract/0,
    send_event/1,
    stop_abstract/0
]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared, [ log_event/1]).
:- use_module(utils).
:- use_module(library(debug)).
:- use_module(abs).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/6.
:- dynamic abstract_queue/1.

% --- Thread management ---

start_abstract :-
    abs:init_queue, 
    thread_create(start_abstract_loop, _, [alias(abs_thread)]),
    debug(main, "Abstract toreador started", []).

send_event(Event) :-
    thread_send_message(abstract_queue, Event).

stop_abstract :-
    thread_send_message(abstract_queue, stop),
    message_queue_destroy(abstract_queue),
    retractall(abstract_q(_)).

% --- Test code: run at load time ---
:- initialization(run_test).

run_test :-
    start_abstract,
    Event = event(server, _{
        counter:0,
        env:dev,
        hostname:"server01",
        id:17565601999960001,
        severity:critical,
        status:open,
        timestamp_collected:1756560199.9969842
    }),
    send_event(Event),
 % Second event: disk
    Event2 = event(disk, _{
        status: open,
        id: 222222222222222, 
        severity: critical,
        env :dev,
        used_space :100,
        hostname: "server01"
    }),
    send_event(Event2),
    print_all_events.


