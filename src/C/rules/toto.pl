:- use_module(library(http/json)).

read_event_from_file(File) :-
    open(File, read, Stream),
    json_read(Stream, JSON),
    close(Stream),
    % Si tu veux stocker comme fait Prolog :
    JSON = json([type=Type, source=Source, value=Value]),
    assertz(event(Type, Source, Value)).

