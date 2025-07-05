:- module(rules_2,
          [ thread_goal_2/1,
            update_event_severity/0
          ]).

:- use_module(rules_0,[event/2]).  % for access to rules_0:event/2

thread_goal_2(Id) :-
    format("[rules_2] Thread ~w is operational.~n", [Id]).

update_event_severity :-
    catch(
        with_mutex(event_update,
            forall(rules_0:event(Id, Dict0),
                   (
                       % only update if Dict0 is a dict
                       ( is_dict(Dict0) ->
                           put_dict(severity, Dict0, warning, Dict1),
                           rules_0:retract(event(Id, Dict0)),
                           rules_0:assertz(event(Id, Dict1))
                       ;   format("[rules_2] Skipped non-dict value: ~w~n", [Dict0])
                       )
                   ))
        ),
        Error,
        (
            print_message(error, Error),
            format("[rules_2] Error caught in update_event_severity.~n")
        )
    ).

