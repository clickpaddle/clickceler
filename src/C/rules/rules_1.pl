:- module(rules_1, [thread_goal_1/1, get_my_event/2, get_all_my_events/1]).
:- use_module(library(http/json)).


:- use_module(rules_0, [event/2]).

:- initialization(init_rules_1).

init_rules_1 :-
    format("[rules_1] Module chargé avec succès.~n").

thread_goal_1(Id) :-
    format("[rules_1] Thread ~w is operational.~n", [Id]).

get_my_event(Id, EventDict) :-
    with_mutex(event_update,
               event(Id, EventDict)).

get_all_my_events(ListOfEvents) :-
    with_mutex(event_update,
               findall(Dict, event(_, Dict), ListOfEvents)).

