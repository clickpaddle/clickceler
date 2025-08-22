
abstract_rule(infrastructure_issue, 100,
    [ event(server(F)) ],
    [ eq(F, status, open), eq(F, severity, critical), eq(F, env, dev) ],
    [ incident(E) ],
    [ set_field(E, status, open), set_field(E, severity, major), set_field(E, message, "infrastructure issue"), set_field(E, category, infrastructure), set_field(E, sub_category, "infrastructure issue"), set_field(E, priority, p2), set_field(E, impact, dev), set_field(E, reported_by, clickceller), set_field(E, hostname, F.hostname) ]
).
    
abstract_rule(infrastructure_issue, 100,
    [ event(disk(F)) ],
    [ eq(F, status, open), eq(F, severity, critical), eq(F, env, dev), ge(F, used_space, 100) ],
    [ incident(E) ],
    [ set_field(E, status, open), set_field(E, severity, major), set_field(E, message, "infrastructure issue"), set_field(E, category, infrastructure), set_field(E, sub_category, "infrastructure issue"), set_field(E, priority, p2), set_field(E, impact, dev), set_field(E, reported_by, clickceller), set_field(E, hostname, F.hostname) ]
).
    
abstract_rule(infrastructure_issue, 100,
    [ event(memory(F)) ],
    [ eq(F, status, open), eq(F, severity, critical), eq(F, env, dev), le(F, free_memory, 0) ],
    [ incident(E) ],
    [ set_field(E, status, open), set_field(E, severity, major), set_field(E, message, "infrastructure issue"), set_field(E, category, infrastructure), set_field(E, sub_category, "infrastructure issue"), set_field(E, priority, p2), set_field(E, impact, dev), set_field(E, reported_by, clickceller), set_field(E, hostname, F.hostname) ]
).
    

abstract_rule(abstract_network, 90,
    [ event(firewall(F)) ],
    [ eq(F, status, open), eq(F, severity, critical), eq(F, env, prod) ],
    [ network(E) ],
    [ set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
    
abstract_rule(abstract_network, 90,
    [ event(snmptrap(F)) ],
    [ eq(F, status, open), eq(F, severity, critical), eq(F, env, prod) ],
    [ network(E) ],
    [ set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
    
abstract_rule(abstract_network, 90,
    [ event(ping(F)) ],
    [ eq(F, status, failed), eq(F, severity, critical), eq(F, env, prod) ],
    [ network(E) ],
    [ set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
    

