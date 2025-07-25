:- module(collector, [collect_event/2]).

:- use_module(library(http/json)).  % pour is_dict/1 si nécessaire
:- use_module(library(lists)).      % pour is_list/1

% Log file for collector events
log_file('../logs/collector_events.log').

thread_goal_collector(ClientID) :-
    format('[collector ~w] Thread started~n', [ClientID]).

% Entrée principale
collect_event(Id, Json) :-
    format("[collector] Received event ~w~n", [Id]),
    log_collector_event(Id, Json),
    (
        is_list(Json) ->
            format("[collector] Json is a list, forwarding each element~n"),
            send_each_event(Id, Json)
        ;
            format("[collector] Json is a single event, forwarding~n"),
            thread_send_message(refine, refine_event(Id, Json))
    ).

% Traitement d’une liste : envoie chaque événement avec un identifiant unique
send_each_event(BaseId, Events) :-
    send_each_event(BaseId, Events, 1).

send_each_event(_, [], _).
send_each_event(BaseId, [Json | Rest], N) :-
    format(atom(NewId), '~w_~w', [BaseId, N]),
    format("[collector] Forwarding event ~w to refine thread~n", [NewId]),
    thread_send_message(refine, refine_event(NewId, Json)),
    N1 is N + 1,
    send_each_event(BaseId, Rest, N1).

% Append the collector event to the log file
log_collector_event(Id, Json) :-
    log_file(File),
    open(File, append, Stream),
    get_time(TS),
    format_time(atom(DateTime), '%Y-%m-%d %H:%M:%S', TS),
    % Utiliser un affichage sûr même pour dicts
    catch(
        term_string(Json, JsonStr),
        _,
        JsonStr = '<<unserializable_json>>'
    ),
    format(Stream, "[~w] Collector Event ~w: ~w~n", [DateTime, Id, JsonStr]),
    close(Stream).

