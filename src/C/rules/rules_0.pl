:- module(rules_0, [thread_goal_0/1, assert_json_event/2, event/2, print_all_events/0, get_my_event/2, get_all_my_events/1]).
:- use_module(library(http/json)).
:- dynamic event/2.

thread_goal_0(Id) :-
    format("Thread ~w (rules_0) ready!~n", [Id]).

print_all_events :-
    with_mutex(event_update,
        forall(event(Id, Event),
        format("[rules_0] Event ~w: ~w~n", [Id, Event]))).

assert_json_event(Id, JsonAtom) :-
    catch(atom_json_dict(JsonAtom, Events, []), E,
          (print_message(error, E), fail)),
    is_list(Events),
    with_mutex(event_update,
        forall(member(Event, Events),
            ( assertz(event(Id, Event)),
              format("[rules_0] Added event for ~w: ~w~n", [Id, Event])
            ))
    ),
    print_all_events.



get_my_event(Id, EventDict) :-
    with_mutex(event_update,
               event(Id, EventDict)).

get_all_my_events(ListOfEvents) :-
    with_mutex(event_update,
               findall(Dict, event(_, Dict), ListOfEvents)).

