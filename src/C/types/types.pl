:- module(types,[ subtype/2, valid_severity/1, valid_status/1, severity_gte/2]).
:- dynamic subtype/2, valid_severity/1, valid_status/1, severity_gte/2.

subtype(log, application).
subtype(cpu, server).
subtype(disk, server).
subtype(memory, server).
subtype(webserver, server).
subtype(router, network).
subtype(switch, network).
subtype(firewall, network).
subtype(snmptrap,network).

valid_severity(info).
valid_severity(ok).
valid_severity(warning).
valid_severity(major).
valid_severity(critical).
valid_severity(fatal).

valid_status(open).
valid_status(ack).
valid_status(pending).
valid_status(waiting).
valid_status(closed).

severity_level(info,    1).
severity_level(ok,      2).
severity_level(warning, 3).
severity_level(major,   4).
severity_level(critical,5).
severity_level(fatal,   6).

severity_gte(S1, S2) :-
    severity_level(S1, L1),
    severity_level(S2, L2),
    L1 >= L2.

:- dynamic server/1, cpu/1, disk/1, memory/1, webserver/1, router/1, switch/1, network/1, firewall/1, log/1, snmptrap/1.

