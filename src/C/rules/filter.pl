filter_rule(
    filter_server_info_drop_event,
    100,
    [log(E)],
    [within(E, severity, [info, ok]), eq(E, env, dev)],
    [nopass]
).

filter_rule(
    filter_server_info_accept_event,
    100,
    [server(E)],
    [
     within(E, severity, [major,fatal]), 
     eq(E, env, prod)
    ],
    [pass]
).

