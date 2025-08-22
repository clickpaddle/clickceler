
abstract_rule(abstract_network, 90,
    [eq(E, status, open), eq(E, severity, critical), eq(E, env, prod) ],
    [set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
abstract_rule(abstract_network, 90,
    [eq(E, status, open), eq(E, severity, critical), eq(E, env, prod) ],
    [set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
abstract_rule(abstract_network, 90,
    [eq(E, status, failed), eq(E, severity, critical), eq(E, env, prod) ],
    [set_field(E, severity, critical), set_field(E, message, "Network connectivity issue detected"), set_field(E, category, infrastructure), set_field(E, sub_category, network), set_field(E, priority, p1), set_field(E, impact, prod) ]
).
