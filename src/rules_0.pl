:- module(_, [thread_goal/1, assert_json_event/2]).
:- use_module(library(http/json)).         % Pour atom_json_dict/3
:- dynamic event/2.

% Appelé au démarrage du thread
thread_goal(Id) :-
    format("Thread ~w prêt à recevoir des JSON…~n", [Id]).

% Reçoit un atome JSON, le convertit en dictionnaire et l’enregistre
assert_json_event(Id, JsonAtom) :-
    catch(
        atom_json_dict(JsonAtom, Dict, []),
        Error,
        ( print_message(error, Error),
          fail )
    ),
    assertz(event(Id, Dict)),
    format("Thread ~w a inséré : ~w~n", [Id, Dict]).

