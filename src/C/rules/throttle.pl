% Auto-generated throttle rules

throttle_rule(
    throttle_network_event,
    100,
    [snmptrap(E)],
    [within(E, severity, [info]), eq(E, env, dev), contains(E, message, "Hello World!")],
    [settings{'limit': 2, 'window': '30s', 'queue_size': 100, 'on_exceed': 'reject', 'delay': '30s'}],
    ['send_first']
).

