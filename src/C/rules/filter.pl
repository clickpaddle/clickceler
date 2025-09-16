filter_rule(filter_no_pass_log_critical_dev_drop_event, 100,
    [ cpu(E) ],
    [within(E, severity, [info, ok]), eq(E, env, dev) ],
    [nopass ]
).
filter_rule(filter_pass_log_major_prod_event, 100,
    [ server(E) ],
    [within(E, severity, [warning, major, critical, fatal]), eq(E, env, prod) ],
    [pass ]
).
