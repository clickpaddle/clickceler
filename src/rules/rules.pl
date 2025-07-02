% Chargement des faits des événements (à inclure ou à charger dans ton environnement Prolog)
:- dynamic event/2.

% Exemple de règle simple qui affiche les événements critiques
critical_event(Id, Message) :-
    event(Id, Event),
    get_dict(severity, Event, critical),
    get_dict(message, Event, Message).

% Règle pour afficher les événements avec une sévérité donnée (insensible à la casse)
event_with_severity(Id, Type, Severity, Message) :-
    event(Id, Event),
    get_dict(severity, Event, Sev),
    downcase_atom(Sev, SevLower),
    downcase_atom(Severity, SevLower),
    get_dict(message, Event, Message),
    ( Type = error_event ; Type = info_event ),
    get_dict(type, Event, Type).

% Règle pour lister tous les événements
list_all_events :-
    event(Id, Event),
    format('Event ~w: ~w~n', [Id, Event]),
    fail.
list_all_events.

