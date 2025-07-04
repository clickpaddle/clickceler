:- module(_, [thread_goal/1, assert_json_event/2]).
:- use_module(library(http/json)).     % atom_json_dict/3
:- dynamic event/2.

thread_goal(Id) :-
    format("Thread ~w (rules_0) prêt !~n", [Id]).

assert_json_event(Id, JsonAtom) :-
    catch(atom_json_dict(JsonAtom, D, []), E,
          (print_message(error,E), fail)),
    assertz(event(Id, D)),
    format("T~w a ajouté : ~w~n", [Id, D]).

