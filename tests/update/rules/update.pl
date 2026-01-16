update_rule(update_cpu_status_and_metric, 100,
    [ cpu(E)] ,
    [ eq(E,status,open) ],
    [ cpu(F)],
    [ settings{ window:"5m"} ],
    [ hostname, origin, subnet ],
    [ eq(F,status,open) ],
    [ set_field(F,load,E.load),
      increment(F,counter) ]).

