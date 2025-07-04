:- module(_, [thread_goal/1, assert_json_event/2]).
:- use_module(library(http/json)).
:- dynamic event/2.

thread_goal(Id) :-
    format("Thread ~w (rules_1) opérationnel.~n", [Id]).

assert_json_event(Id, J) :-
    catch(atom_json_dict(J, D, []), E,
          (print_message(error,E), fail)),
    % Exemple : si pas de clé severity ⇒ on l’ajoute à warning
    ( get_dict(severity, D, _) -> D2 = D
    ; put_dict(severity, D, warning, D2)
    ),
    assertz(event(Id, D2)),
    format("T~w stocke : ~w~n", [Id, D2]).

