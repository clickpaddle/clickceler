:- use_module(library(http/json)).   % ← charge la bibliothèque JSON
:- module(rules, [
    start_rules_thread/0,
    add_event/1,
    add_events/1,
    set_severity/2,
    critical_events/1
]).

/** <module> Gestion des events JSON et règles associées

Ce module permet de stocker des events sous forme de dicts,
de modifier leur sévérité, et de lister les events critiques.
*/

:- dynamic event/1.

%% start_rules_thread
%
%  Point d’entrée pour lancer un thread ou initialiser le module.
start_rules_thread :-
    format("Rules module démarré~n").

%% add_event(+EventDict)
%
%  Ajoute un event sous forme de dict JSON en mémoire.
add_event(EventDict) :-
    % On s’assure que c’est un dict
    (is_dict(EventDict) ->
        assertz(event(EventDict))
    ;
        format(user_error, "Erreur : add_event attend un dict, reçu : ~w~n", [EventDict]),
        fail).

%% add_events(+ListOfEventDicts)
%
%  Ajoute une liste d’events (list of dicts).
add_events([]).
add_events([E|Es]) :-
    add_event(E),
    add_events(Es).

%% set_severity(+EventId, +NewSeverity)
%
%  Modifie la sévérité d’un event identifié par son id.
set_severity(EventId, NewSeverity) :-
    % Retire l'ancien event avec ce id
    retract(event(Event)),
    get_dict(id, Event, EventId),
    % Met à jour la sévérité dans un nouveau dict
    put_dict(severity, Event, NewSeverity, UpdatedEvent),
    assertz(event(UpdatedEvent)).

%% critical_events(-List)
%
%  Renvoie la liste des events dont la sévérité est 'critical'.
critical_events(CriticalList) :-
    findall(
        Event,
        (event(Event), get_dict(severity, Event, critical)),
        CriticalList
    ).

