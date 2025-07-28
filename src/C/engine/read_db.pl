:- module(read_db, [ reload_eventdb/1, show_all_events/1 ]).

% Reloads all events from the event database file as Prolog terms
reload_eventdb(Events) :-
    open('../logs/eventdb.log', read, Stream),
    read_terms(Stream, Events),
    close(Stream).

% Reads all terms from the stream until end_of_file, returns list of events
read_terms(Stream, []) :-
    at_end_of_stream(Stream), !.

read_terms(Stream, [Event | Rest]) :-
    read(Stream, Term),
    ( Term == end_of_file ->
        Rest = [],
        Event = []
    ;   
        Event = Term,
        read_terms(Stream, Rest)
    ).

% Prints all events in a readable format
show_all_events([]).
show_all_events([Event|Rest]) :-
    format("~q.~n", [Event]),
    show_all_events(Rest).

