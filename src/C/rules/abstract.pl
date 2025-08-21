% Rule: abstract_incident
abstract_rule(abstract_incident, 100, event(Type, Dict), AbstractID) :-
    % Match any of server, disk, memory events
    ((Type = server, Dict.status = open, Dict.severity = critical, Dict.env = dev, Dict.msg = "host not responding");
     (Type = disk,   Dict.status = open, Dict.message = "Disk full", Dict.severity = critical, Dict.env = dev, Dict.used_space >= 100);
     (Type = memory, Dict.status = open, Dict.message = "Memory full", Dict.severity = critical, Dict.env = dev, Dict.free_memory <= 0)),

    ( existing_abstract(_, AbstractID) ->
        % Existing abstract: associate event
        add_to_abstract_contrib(AbstractID, event(Type, Dict)),
        add_abstract_to_event(event(Type, Dict), AbstractID),
        % Update linked abstracts in the event dict
        update_event_linked_abstracts(event(Type, Dict), AbstractID),
        trace(info, "Event associated with existing abstract", [])
    ;
        % No existing abstract: create new
        generate_abstract(incident, Dict, AbstractID),
        add_to_abstract_contrib(AbstractID, event(Type, Dict)),
        add_abstract_to_event(event(Type, Dict), AbstractID),
        % Update linked abstracts in the event dict
        update_event_linked_abstracts(event(Type, Dict), AbstractID),
        % Define transformations for the abstract
        Transforms = [
            set_field(_E, status, open),
            set_field(_E, severity, major),
            set_field(_E, message, "Server unresponsive. Crash imminent"),
            set_field(_E, category, infrastructure),
            set_field(_E, sub_category, server),
            set_field(_E, priority, p2),
            set_field(_E, impact, dev),
            set_field(_E, reported_by, clickceller),
            set_field(_E, hostname, Dict.hostname)
        ],
        % Apply transformations to the abstract dict
        apply_transformations(Transforms, AbstractEvent),
    ).


% Rule: abstract_network
abstract_rule(abstract_network, 90, event(Type, Dict), AbstractEvent) :-
    % Match any of firewall, snmptrap, ping events
    ((Type = firewall, Dict.status = open, Dict.severity = critical, Dict.env = prod);
     (Type = snmptrap, Dict.status = open, Dict.severity = critical, Dict.env = prod);
     (Type = ping,     Dict.status = failed, Dict.severity = critical, Dict.env = prod)),

    ( existing_abstract(_, AbstractEvent) ->
        % Existing abstract: associate event
        add_to_abstract_contrib(AbstractEvent, event(Type, Dict)),
        add_abstract_to_event(event(Type, Dict), AbstractEvent),
        update_event_linked_abstracts(event(Type, Dict), AbstractEvent),
        trace(info, "Event associated with existing network abstract", [])
    ;
        % No existing abstract: create new
        generate_abstract(network_incident, Dict, AbstractEvent),
        add_to_abstract_contrib(AbstractEvent, event(Type, Dict)),
        add_abstract_to_event(event(Type, Dict), AbstractEvent),
        update_event_linked_abstracts(event(Type, Dict), AbstractEvent),
        % Define transformations for the abstract
        Transforms = [
            set_field(_E, severity, critical),
            set_field(_E, message, "Network connectivity issue detected"),
            set_field(_E, category, infrastructure),
            set_field(_E, sub_category, network),
            set_field(_E, priority, p1),
            set_field(_E, impact, prod),
            set_field(_E, reported_by, clickceller),
            set_field(_E, source, Dict.type),
            set_field(_E, hostname, Dict.hostname)
        ],
        % Apply transformations to the abstract
        apply_abstract_transformations(Transforms, AbstractEvent),
    ).
