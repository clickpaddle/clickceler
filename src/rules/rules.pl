%Load facts to be included into Prolog env.
:- dynamic event/2.

% Rule showing critical events
critical_event(Id, Message) :-
    event(Id, Event),
    get_dict(severity, Event, critical),
    get_dict(message, Event, Message).

% Rule to display event with a specific severity
event_with_severity(Id, Type, Severity, Message) :-
    event(Id, Event),
    get_dict(severity, Event, Sev),
    downcase_atom(Sev, SevLower),
    downcase_atom(Severity, SevLower),
    get_dict(message, Event, Message),
    ( Type = error_event ; Type = info_event ),
    get_dict(type, Event, Type).


load_facts(File) :-
    consult(File).


%  Reset critical severity events to major.
reset_severity :-
    % search critical events
    event(Id, EventDict),
    get_dict(severity, EventDict, critical),
    % Remove event
    retract(event(Id, EventDict)),
    % Create anew Dict with the new severity set
    UpdatedDict = EventDict.put(severity, major),
    % ReCreate event with new severity
    assert(event(Id, UpdatedDict)),
    % continue la récursion
    reset_severity.
    
reset_severity.


