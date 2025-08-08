:- use_module(library(thread)).
:- use_module(library(message_queue)).

init_queue :-
    thread_create_message_queue(refine),
    writeln('Queue created.').
