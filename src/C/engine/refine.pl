:- module(refine, [ thread_goal_refine/1, start_refine_loop/0 % Predicate to start the refine thread loop
]).

% Import necessary libraries
:- use_module(library(thread)).     % For thread management and message passing
:- use_module(library(time)).      % For time management and log formatting
:- use_module(library(error)).     % For error handling (e.g., must_be/2)
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic refine_rule/5.
:- initialization(init_queue).

init_queue :-
    ( catch(message_queue_property(refine_queue, _), _, fail) ->
        true
    ; message_queue_create(refine_queue),
      format('[Refine] Created message queue refine~n')
    ).

thread_goal_refine(ClientID) :-
    format('[Refine ~w] Thread started~n', [ClientID]).

% Load Dynamic rules


load_refine_rules :-
    % relative Path to dynamic refine rules 
    RuleFile = '../rules/refine.pl',
    exists_file(RuleFile),           
    !,
    load_files(RuleFile, [if(changed)]),
    format('[Refine] Rules loaded from ~w~n', [RuleFile]).

load_refine_rules :-
    format('[Refine] Warning: Rules file not found.~n', []).

% Main loop of the refine thread.
% It continuously fetches messages from its message queue and processes them.

start_refine_loop :-
    load_refine_rules,
    init_queue, 
    refine_loop.

    % Wait for a message.
refine_loop :-
    thread_get_message(refine_queue, EventTerm),
    (   catch(handle_event(EventTerm), E,
              (format('[Refine] Error: ~w~n', [E]), fail))
    ->  true
    ;   format('[Refine] Warning: EventTerm not handled: ~w~n', [EventTerm])
    ),
    format('[Refine] Received: ~q~n', [EventTerm]),
    refine_loop.


% Handle incoming messages
% handle_event(+EventTerm) event is normalized
% Refine Normalized event of the form event(Type, Dict)
handle_event(event(EventType, DictIn)) :-
    format('[Refine] Normalized DictIn: ~q~n', [DictIn]),
    findall( rule(Priority, RuleID, Conditions, Transformations),
    refine_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn),
    RuleList
    ),
    format('[Refine] Matched rules: ~q~n', [RuleList]),
    % Sort by decreasing priority PRIORITY 100 > PRIORITY 10 
    sort(1, @>=, RuleList, SortedRules),
    format('[Refine] Sorted rules by priority: ~q~n', [SortedRules]),
    apply_matching_rules(SortedRules, DictIn, DictOut),
    format('[Refine] After apply_matching_rules: ~q~n', [DictOut]),
    EventOut = event(EventType, DictOut),
    assert_event(EventOut),
    format('[Refine] Final event to assert: ~q~n', [EventOut]),
    log_event(EventOut).


% refine_rule_match(+EventType, -Priority, -RuleID, -Conditions, -Transformations, +DictIn)
refine_rule_match(EventType, Priority, RuleID, Conditions, Transformations, DictIn) :-
    refine_rule(EventType, Priority, RuleID, Conditions, Transformations),
    match_all_conditions(Conditions, DictIn).


% match_all_conditions(+Conditions, +Dict)
match_all_conditions([], _).
match_all_conditions([Cond | Rest], Dict) :-
    match_condition(Cond, Dict),
    match_all_conditions(Rest, Dict).
% apply_matching_rules(+Rules, +DictIn, -DictOut)
apply_matching_rules([], Dict, Dict).

apply_matching_rules([rule(_P, RuleID, _Conds, Transforms) | Rest], DictIn, DictOut) :-
    format('[Refine] Applying rule ~w~n', [RuleID]),
    apply_transformations(Transforms, DictIn, DictNext),
    apply_matching_rules(Rest, DictNext, DictOut).

% apply_transformations(+TransformList, +DictIn, -DictOut)

apply_transformations([], Dict, Dict).

% Transformation : set_field(Key, Value)
apply_transformations([set_field(Key, Value)|Rest], DictIn, DictOut) :-
    put_dict(Key, DictIn, Value, DictNext),
    apply_transformations(Rest, DictNext, DictOut).

% Transformation : add_tag(Tag)
% Add a Tag in the transformation for future processing with CI association in another thread that will manage Event and CI association. 
apply_transformations([add_tag(Tag)|Rest], DictIn, DictOut) :-
    ( get_dict(tags, DictIn, Tags0) -> true ; Tags0 = [] ),
    % Évite les doublons
    ( memberchk(Tag, Tags0) -> Tags1 = Tags0 ; Tags1 = [Tag | Tags0] ),
    put_dict(tags, DictIn, Tags1, DictNext),
    apply_transformations(Rest, DictNext, DictOut).

% Transformation : remove_tag(Tag)
% Remove a tag from the list of tags if present
apply_transformations([remove_tag(Tag)|Rest], DictIn, DictOut) :-
    ( get_dict(tags, DictIn, Tags0) -> true ; Tags0 = [] ),
    delete(Tags0, Tag, Tags1),
    put_dict(tags, DictIn, Tags1, DictNext),
    apply_transformations(Rest, DictNext, DictOut).


% Transformation : increment_field(Field)
% Incrémente un champ numérique
apply_transformations([increment_field(Field)|Rest], DictIn, DictOut) :-
    ( get_dict(Field, DictIn, Val), number(Val) ->
        NewVal is Val + 1,
        put_dict(Field, DictIn, NewVal, DictNext)
    ;   % Valeur absente ou non numérique
        put_dict(Field, DictIn, 1, DictNext)
    ),
    apply_transformations(Rest, DictNext, DictOut).

% Si transformation inconnue : afficher un warning mais continuer
apply_transformations([Unknown|Rest], DictIn, DictOut) :-
    format('[Refine] Warning: Unknown transformation ~w~n', [Unknown]),
    apply_transformations(Rest, DictIn, DictOut).

assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        assertz(kb_shared:event(Type, Dict))
    ).

