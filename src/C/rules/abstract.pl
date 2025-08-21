%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Facts examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% event(EventID, Type, Status, Severity, Hostname, LinkedAbstracts)
% abstract(AbstractID, Type, Status, Severity, Hostname, AbstractContrib)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rule: create abstract from sensor or probe events
abstract_rule(create_abstract_from_sensor_and_probe_events, 100, EventID, AbstractID) :-
    (   % OR block
        % First branch: sensor critical open
        event(EventID, sensor, Status, Severity, Hostname, _Linked),
        Status = open,
        Severity = critical
    ;
        % Second branch: probe critical open with same hostname as a sensor
        event(EventID, probe, Status, Severity, Hostname, _Linked),
        Status = open,
        Severity = critical,
        event(SensorID, sensor, _, _, Hostname, _)
    ),
    \+ existing_abstract(EventID, _AbstractID),
    !,
    generate_abstract(EventID, AbstractID),
    add_to_abstract_contrib(AbstractID, EventID),
    set_abstract_field(AbstractID, type, incident),
    set_abstract_field(AbstractID, status, open),
    ( event(EventID, sensor, _, Severity, Hostname, _) -> set_abstract_field(AbstractID, severity, Severity) ; true ),
    ( event(EventID, probe, _, Severity, Hostname, _) -> set_abstract_field(AbstractID, severity, Severity) ; true ),
    set_abstract_field(AbstractID, hostname, Hostname),
    trace(info, "Abstract created from sensor or probe event", []).

% Rule: associate new event with existing abstract
abstract_rule(associate_event_with_existing_abstract, 90, EventID, AbstractID) :-
    event(EventID, Type, Status, Severity, Hostname, Linked),
    existing_abstract(EventID, AbstractID),
    \+ member(AbstractID, Linked),
    add_to_abstract_contrib(AbstractID, EventID),
    add_abstract_to_event(EventID, AbstractID),
    trace(info, "Event updated with new abstract association", []).


