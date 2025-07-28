:- use_module(kb_shared, [print_all_events/1]).
:- dynamic kb_shared:event/2.
:- multifile kb_shared/2.

% Faits d'événements
kb_shared:event("Log",[category-"DEBUG",hostname-"server02",id-17536330259070001,message-"Temperature sensor malfunction",origin-"datacenter3",source-"Maintenance Scheduler",sub_source-"CPUUnit",timestamp-"2025-07-01T19:13:00Z"]).
kb_shared:event("CPU",[category-"DEBUG",hostname-"backup01",id-17536330259070002,origin-"datacenter2",source-"NetworkMonitor",state-"throttled",sub_source-"RAMModule",timestamp-"2025-07-01T10:53:00Z",usage_percent-27]).
kb_shared:event("Disk",[category-"DEBUG",free_space_gb-205.49,hostname-"server01",id-17536330259070003,origin-"datacenter2",source-"Auth Service",state-"healthy",sub_source-"BackupService",timestamp-"2025-07-01T00:32:00Z"]).
kb_shared:event("Server",[category-"INFO",hostname-"sensor01",id-17536330259070004,origin-"datacenter1",source-"Auth Service",status-"down",sub_source-"DiskMonitor",timestamp-"2025-07-01T15:14:00Z"]).

% Prédicat qui retourne tous les events sous forme de termes Prolog bien formatés
% Affiche la liste des attributs sous la forme 'key'-'value', séparés par des virgules, sans parenthèses.
