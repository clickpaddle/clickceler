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

    % Event that matches the "server" rule
    Event1 = event(server, _{
        counter:0,
        env:dev,
        hostname:"server01",
        id:17565601999960001,
        severity:critical,
        status:open,
        timestamp_collected:1756560199.9969842
    }),
    send_event(Event1),

    % Event that matches the "disk" rule
    Event2 = event(disk, _{
        counter:1,
        env:dev,
        hostname:"server01",
        id:17565602000000002,
        severity:critical,
        status:open,
        used_space:150,  % >= 100 to match gte(F, used_space, 100)
        timestamp_collected:1756560200.123456
    }),
    send_event(Event2),

    % Event that matches the "memory" rule
    Event3 = event(memory, _{
        counter:2,
        env:dev,
        hostname:"server01",
        id:17565602000000003,
        severity:critical,
        status:open,
        free_memory:0,   % <= 0 to match le(F, free_memory, 0)
        timestamp_collected:1756560200.654321
    }),
    send_event(Event3),
    
     sleep(5), halt.

