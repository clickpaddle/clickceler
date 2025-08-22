% Rule: abstract server not responding
abstract_rule(
    abstract_server_down,
    100,
    [event(server, E)],
    [
      eq(E, status, open),
      eq(E, severity, critical),
      eq(E, env, dev)
    ],
    [
      set_field(E, status, open),
      set_field(E, severity, major),
      set_field(E, message, "Server unresponsive. Crash imminent"),
      set_field(E, category, infrastructure),
      set_field(E, sub_category, server),
      set_field(E, priority, p2),
      set_field(E, impact, dev),
      set_field(E, reported_by, clickceller),
      set_field(E, hostname, E.hostname)
    ]
).


% Rule: abstract disk full
abstract_rule(
    abstract_disk_full,
    100,
    [event(disk, E)],
    [ eq(E, status, open),
      eq(E, severity, critical),
      eq(E, env, dev),
      ge(E, used_space, 100)
    ],
    [
      set_field(E, status, open),
      set_field(E, severity, major),
      set_field(E, message, "Disk full on host"),
      set_field(E, category, infrastructure),
      set_field(E, sub_category, disk),
      set_field(E, priority, p2),
      set_field(E, impact, dev),
      set_field(E, reported_by, clickceller),
      set_field(E, hostname, E.hostname)
    ]
).


% Rule: abstract memory full
abstract_rule(
    abstract_memory_full,
    100,
    [event(memory, E)],
    [
      eq(E, status, open),
      eq(E, severity, critical),
      eq(E, env, dev),
      le(E, free_memory, 0)
    ],
    [
      set_field(E, status, open),
      set_field(E, severity, major),
      set_field(E, message, "Memory full on host"),
      set_field(E, category, infrastructure),
      set_field(E, sub_category, memory),
      set_field(E, priority, p2),
      set_field(E, impact, dev),
      set_field(E, reported_by, clickceller),
      set_field(E, hostname, E.hostname)
    ]
).
