% Auto-generated throttle rules

throttle_rule(
    throttle_event_log_where_severity_equals_info_and_env_eq_dev,
    100,
    [log(E)],
    [within(E, severity, [info]), eq(E, env, dev), contains(E, message, "Hello World!")],
    [{'limit': 2}, {'window': '30s'}, {'queue_size': 100}, {'on_exceed': 'reject'}, {'delay': '0s'}],
    ['send_first']
).

