:- module(main, [
    start_throttle/0,
    send_event/1,
    stop_throttle/0
]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared).
:- use_module(utils).
:- use_module(library(debug)).
:- use_module(throttle).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic throttle_rule/6.
:- dynamic throttle_queue/1.

% --- Thread management ---

start_throttle :-
    throttle:init_queue, 
    thread_create(start_throttle_loop, _, [alias(throttle_thread)]),
    debug(main, "Filter thread started", []).

send_event(Event) :-
    thread_send_message(throttle_queue, Event).

stop_throttle :-
    thread_send_message(throttle_queue, stop),
    message_queue_destroy(throttle_queue),
    retractall(throttle_q(_)).

% --- Test code: run at load time ---
:- initialization(run_test).

run_test :-
    start_throttle,

    % Event that matches the "server" rule
    Event1 = event(snmptrap, _{
        counter:0,
        env:dev,
        hostname:"server01",
        severity:info,
        status:open,
        origin: 111,
        source: "AAA",
        id: 1111 ,
        msg: "Hello World!"
        
    }),
    send_event(Event1),
       Event2 = event(snmptrap, _{
        counter:0,
        env:dev,
        hostname:"server01",
        severity:info,
        status:open,
        origin: 111,
        source: "AAA",
        id: 222222,  
        msg: "Hello World!"

    }),
    send_event(Event2),
     sleep(16), 
    send_event(Event1),
       Event3 = event(snmptrap, _{
        counter:0,
        env:dev,
        hostname:"server01",
        severity:info,
        status:open,
        origin: 111,
        source: "AAA",
        id: 3333,
        msg: "Hello World!"
    
    }),
    send_event(Event3),
    sleep(50) ,
    print_all_events,
    halt.
    

