refine_rule(
    refine_example,
    100,
    'close_low_severity',
    [
     eq(status, open), 
     eq(severity, critical)
    ],
    [
     set_field(status, closed)]
).
