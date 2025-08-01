refine_rule( server, 100, 'close_low_severity', [ eq(status, open), eq(severity, critical) ], [ set_field(status, closed),set_field(severity, warning) ]).
refine_rule( log, 100, 'close_low_severity', [ eq(status, open), eq(severity, critical) ], [ set_field(status, closed),set_field(severity, warning) ]).
refine_rule( disk, 100, 'close_low_severity', [ eq(status, open), eq(severity, critical) ], [ set_field(status, closed),set_field(severity, warning) ]).
refine_rule( memory, 100, 'close_low_severity', [ eq(status, open), eq(severity, critical) ], [ set_field(status, closed),set_field(severity, warning) ]).
refine_rule( cpu, 100, 'close_low_severity', [ eq(status, open), eq(severity, critical) ], [ set_field(status, closed),set_field(severity, warning) ]).

