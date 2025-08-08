filter_rule(
    filter_no_pass_log_critical_dev_drop_event,
    100,
    [log(E)],
    [within(E, severity, [critical, info, ok]), eq(E, env, dev)],
    [nopass]
).

filter_rule(
    filter_pass_log_major_prod_event,
    100,
    [log(E)],
    [
     within(E, severity, [major,fatal]), 
     eq(E, env, prod)
    ],
    [pass]
).

