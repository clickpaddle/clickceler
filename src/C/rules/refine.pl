
refine_rule(decrease_event_severity_event_if_de_and_close, 100,
    [ server(E) ],
    [eq(E, status, open), eq(E, severity, critical), eq(E, env, dev) ],
    [set_field(E, status, closed), set_field(E, severity, warning) ]
).
