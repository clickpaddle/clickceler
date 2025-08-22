
throttle_rule(throttle_network_event, 100,
    [ snmptrap(E) ],
    [within(E, severity, [info]), eq(E, env, dev), contains(E, msg, "Hello World!") ],
    [settings{"limit": 2}, settings{"window": "30s"}, settings{"delay": "15s"} ],
    [send_first ]
).
