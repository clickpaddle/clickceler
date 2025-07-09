:- module(rules_1, [thread_goal_1/1, assert_json_event/2, event/2, print_all_events/0,  get_all_my_events/2]).
:- use_module(library(http/json)).
:- dynamic event/2.

thread_goal_1(Id) :-
    format("Thread ~w (rules_1) ready!~n", [Id]).

print_all_events :-
        forall(event(Id, Event),
        format("[rules_1] Event ~w: ~w~n", [Id, Event])).

assert_json_event(Id, JsonAtom) :-
    catch(atom_json_dict(JsonAtom, Events, []), E,
          (print_message(error, E), fail)),
    is_list(Events),
        forall(member(Event, Events),
              (with_mutex(event_update,(assertz(event(Id, Event)))),
              format("[rules_1] Added event for ~w: ~w~n", [Id, Event]))),
    print_all_events.


get_all_my_events(ClientID, Events) :-
    findall(Event, event(ClientID, Event), Events).
