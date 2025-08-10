:- module(read_db, [ reload_eventdb/1, show_all_events/1 ]).

% Reload all events from ../logs/eventdb.log
reload_eventdb(Events) :-
    open('../logs/eventdb.log', read, Stream),
    read_event_terms(Stream, Events),
    close(Stream).

%  Read all Events
read_event_terms(Stream, []) :-
    at_end_of_stream(Stream), !.

read_event_terms(Stream, [Event | Rest]) :-
    read_term(Stream, Event, []),
    ( Event == end_of_file ->
        Rest = []
    ;
        read_event_terms(Stream, Rest)
    ).


% show_all_events(+Events)
show_all_events(Events) :-
    maplist(portray_clause, Events).


