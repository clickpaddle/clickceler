:- module(_, [thread_goal/1, assert_json_event/2]).
:- use_module(library(http/json)).
:- dynamic event/2.

thread_goal(Id) :-
    format("Thread ~w prêt à recevoir des JSON…~n", [Id]).

assert_json_event(Id, JsonAtom) :-
    catch(
        atom_json_dict(JsonAtom, Dict, []),
        Error,
        ( print_message(error, Error),
          fail )
    ),
    assertz(event(Id, Dict)),
    format("Thread ~w a inséré : ~w~n", [Id, Dict]).

