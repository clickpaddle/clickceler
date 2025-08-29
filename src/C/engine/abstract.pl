:- module(abstract, [thread_goal_abstract/1, start_abstract_loop/0]).

:- use_module(library(thread)).
:- use_module(library(time)).
:- use_module(library(error)).
:- use_module(kb_shared,[eventlog_mutex/1,log_event/1,print_all_events/1]).
:- use_module(utils).

:- dynamic kb_shared:event/2.
:- multifile kb_shared:event/2.
:- dynamic abstract_rule/5.

% Initialize abstract queue
init_queue :-
    ( catch(message_queue_property(abstract_queue,_),_,fail) -> true
    ; message_queue_create(abstract_queue),
      log_trace(info,'[Abstract] Created message queue abstract',[])
    ).

% Thread entry
thread_goal_abstract(ClientID) :-
    log_trace(info,'[Abstract ~w] Thread started', [ClientID]).

% Load rules
load_abstract_rules :-
    RuleFile = '../rules/abstract.pl',
    exists_file(RuleFile),
    !,
    load_files(RuleFile, [if(changed)]),
    log_trace(info,'[Abstract] Rules loaded from ~w', [RuleFile]).
load_abstract_rules :-
    log_trace(info,'[Abstract] Rules file not found.', []).

% Main loop
start_abstract_loop :-
    load_abstract_rules,
    init_queue,
    abstract_loop.

abstract_loop :-
    thread_get_message(abstract_queue, EventTerm),
    ( catch(handle_event(EventTerm), E,
            (log_trace(error,'[Abstract] Error: ~w', [E]), fail))
    -> true
    ; log_trace(warning,'[Abstract] EventTerm not handled: ~w', [EventTerm])
    ),
    log_trace(info,'[Abstract] Received: ~q', [EventTerm]),
    abstract_loop.

% Handle event
handle_event(event(EventType, DictIn)) :-
    log_trace(info,'[Abstract] Normalized DictIn: ~q', [DictIn]),

    % On essaye de trouver toutes les règles qui matchent
    findall(
        rule(Priority, RuleID, Conditions, Transformations),
        (
            catch(
                (
                    abstract_rule_match(RuleID, Priority, EventType, Conditions, Abstract, Transformations, DictIn),
                    log_trace(info, '[Abstract] Rule matched: ~w (Priority: ~w)', [RuleID, Priority])
                ),
                E,
                log_trace(warning,'[Abstract] Rule ~w raised error: ~w', [RuleID, E])
            )
        ),
        RuleList
    ),

    ( RuleList \= [] ->
        log_trace(info,'[Abstract] Matched rules: ~q', [RuleList]),
        sort(1, @>=, RuleList, SortedRules),
        log_trace(info,'[Abstract] Sorted rules by priority: ~q', [SortedRules]),
        apply_matching_rules(SortedRules, event(EventType, DictIn), DictOut),
        EventOut = event(EventType, DictOut),
        assert_event(EventOut),
        log_event(EventOut),
        safe_thread_send_message(update_queue, EventOut)
    ; log_trace(warning,'[Abstract] No rules matched for EventTerm: ~w', [event(EventType, DictIn)])
    ).

% abstract_rule_match(RuleID, Priority, EventType, Conditions, Abstract,Transformations, DictIn)
abstract_rule_match(RuleID, Priority, EventType, Conditions, Transformations, DictIn) :-
    log_trace(info, '[Abstract] Trying to match EventType: ~w with DictIn: ~q', [EventType, DictIn]),

    % Récupération de la règle
    abstract_rule(RuleID, Priority, [Pattern], Conditions, [Abstract], Transformations),
    log_trace(info, '[Abstract] Testing abstract_rule: RuleID=~w, Priority=~w, Pattern=~q, Abstract=~q',
              [RuleID, Priority, Pattern, Abstract]),
    log_trace(info, '[Abstract] Found abstract_rule: RuleID=~w, Priority=~w, Pattern=~q, Abstract=~q',
              [RuleID, Priority, Pattern, Abstract]),

    % Vérification que la priorité est bien une constante
    ( ground(Priority) ->
        true
    ;
        log_error('[Abstract] ERROR: Priority for RuleID=~w is not ground: ~q', [RuleID, Priority]),
        fail
    ),

    % Vérification du type d’événement
    Pattern =.. [RuleEventType, F],
    log_trace(info, '[Abstract] RuleEventType=~w, PatternVar=~q', [RuleEventType, F]),

    ( is_subtype(EventType, RuleEventType) ->
        log_trace(info, '[Abstract] EventType ~w is subtype of RuleEventType ~w',
                  [EventType, RuleEventType])
    ;
        log_trace(info, '[Abstract] EventType ~w does NOT match RuleEventType ~w',
                  [EventType, RuleEventType]),
        fail
    ),

    % Bind du dict si nécessaire
    ( var(F) -> F = DictIn ; true ),
    log_trace(info, '[Abstract] After binding, PatternVar F=~q', [F]),

    % Vérification des conditions
    ( match_all_conditions(Conditions, F) ->
        log_trace(info, '[Abstract] Conditions matched for RuleID ~w', [RuleID])
    ;
        log_trace(info, '[Abstract] Conditions FAILED for RuleID ~w', [RuleID]),
        fail
    ),

    % Construction de l’abstraction
    Abstract =.. [AbstractType, G],
    ( var(G) -> G = _{} ; true ),
    log_trace(info, '[Abstract] Bound abstract variable: ~w=~q', [AbstractType, G]).

    % Build the abstract event, initializing from DictIn
    Abstract =.. [AbstractType, G],
    ( var(G) ->
        G = DictIn.put(_{id:_, type:AbstractType, is_abstract:true, abstract_contrib:[]})
    ; true ),
    log_trace(info, '[Abstract] Bound abstract variable: ~w=~q', [AbstractType, G]).


% Apply matching rules
apply_matching_rules([], _Event, DictOut) :- DictOut = _{}.
apply_matching_rules([rule(_P,_ID,_C,Transforms)|Rest], event(EventType, DictIn), DictOut) :-
    log_trace(info,'[Abstract] Applying rule ~w', [RuleID]),
    log_trace(info,'[Abstract Transforms: ~w ',[Transforms]),
    prepare_transforms(Transforms, DictIn, BoundTransforms),
    apply_transformations(BoundTransforms, DictIn, UpdatedDict),
    apply_matching_rules(Rest, event(EventType, UpdatedDict), DictOut).

% Apply a list of transformations to an event dict, handling abstracts
apply_transformations([], Dict, Dict).

apply_transformations([Transform|Rest], DictIn, DictOut) :-
    apply_single_transform(Transform, DictIn, TempDict),
    apply_transformations(Rest, TempDict, DictOut).

% Apply a single transformation
apply_single_transform(set_field(Key, Value), DictIn, DictOut) :-
    % If Value is a variable representing an abstract, ensure the abstract exists
    ( var(Value) ->
        ( existing_abstract(Key, AbsID) ->
            UpdatedValue = AbsID,
            log_trace(info, '[Abstract] Reusing existing abstract ~w for key ~w', [AbsID, Key])
        ; generate_abstract(Key, DictIn, AbsID),
          UpdatedValue = AbsID,
          log_trace(info, '[Abstract] Generated new abstract ~w for key ~w', [AbsID, Key])
        )
    ; UpdatedValue = Value,
      log_trace(info, '[Abstract] Using existing value for key ~w: ~w', [Key, Value])
    ),
    % Update the dictionary
    DictOut = DictIn.put(Key, UpdatedValue),
    log_trace(info, '[Abstract] Updated Dict field ~w -> ~w', [Key, UpdatedValue]),
    % If the field points to an abstract, update its contrib list
    ( get_dict(is_abstract, DictOut, true) ->
        true  % Already an abstract, do nothing
    ; atom(UpdatedValue),
      sub_atom(UpdatedValue, 0, 4, _, 'abs_') ->
        add_to_abstract_contrib(UpdatedValue, DictOut),
        log_trace(info, '[Abstract] Added Dict to abstract contrib list for ~w', [UpdatedValue])
    ; true ).

% Optionally, handle other kinds of transformations in future
% e.g., apply_single_transform(custom_transform(...), DictIn, DictOut) :- ...

prepare_transforms([], _Dict, []).
prepare_transforms([Transform|Rest], DictIn, [NewTransform|RestOut]) :-
    Transform =.. [Functor, Var],
    ( var(Var) ->
        ( existing_abstract(Functor, AbsID) ->
            NewVar = AbsID,
            log_trace(info, '[Abstract] Reusing existing abstract ~w for Functor ~w', [AbsID, Functor])
        ; generate_abstract(Functor, DictIn, AbsID),
          NewVar = AbsID,
          log_trace(info, '[Abstract] Generated new abstract ~w for Functor ~w', [AbsID, Functor])
        )
    ; NewVar = Var,
      log_trace(info, '[Abstract] Using existing value for Functor ~w: ~w', [Functor, Var])
    ),
    NewTransform =.. [Functor, NewVar],
    prepare_transforms(Rest, DictIn, RestOut).

% Assert event safely
assert_event(event(Type, Dict)) :-
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
      ( retractall(kb_shared:event(Type,Dict)),
        assertz(kb_shared:event(Type,Dict))
      )
    ).

% Mutex-protected add event to abstract contrib
add_to_abstract_contrib(AbstractID, EventDict) :-
    kb_shared:event(Type, AbsDict),
    get_dict(is_abstract, AbsDict, true),
    get_dict(id, AbsDict, AbstractID),
    get_dict(abstract_contrib, AbsDict, Contrib),
    \+ memberchk(EventDict, Contrib),
    NewContrib = [EventDict|Contrib],
    NewDict = AbsDict.put(abstract_contrib, NewContrib),
    eventlog_mutex(Mutex),
    with_mutex(Mutex,
        ( retract(kb_shared:event(Type, AbsDict)),
          assertz(kb_shared:event(Type, NewDict))
        )
    ),
    log_trace(info, '[Abstract] Added event to abstract ~w. Total contributors: ~w', [AbstractID, length(NewContrib)]).

% Check if abstract exists or not
existing_abstract(EventType, AbstractID) :-
    kb_shared:event(_Type, AbsDict),
    get_dict(is_abstract, AbsDict, true),
    get_dict(type, AbsDict, EventType),
    get_dict(abstract_contrib, AbsDict, Contrib),
    Contrib \= [],
    get_dict(id, AbsDict, AbstractID),
    log_trace(info, '[Abstract] Found existing abstract ~w for EventType ~w', [AbstractID, EventType]).

% Optionnel : log if not Abstract of EventType found. 
existing_abstract(EventType, _) :-
    \+ ( kb_shared:event(_Type, AbsDict),
         get_dict(is_abstract, AbsDict, true),
         get_dict(type, AbsDict, EventType),
         get_dict(abstract_contrib, AbsDict, Contrib),
         Contrib \= []
       ),
    log_trace(info, '[Abstract] No existing abstract found for EventType ~w', [EventType]),
    fail.

% Generate new abstract
generate_abstract(EventType, EventDict, AbstractID) :-
    gensym(abs_, AbstractID),
    AbsDict = EventDict.put(_{
        id: AbstractID,
        type: EventType,
        is_abstract: true,
        abstract_contrib: []
    }),
    assert_event(event(AbstractID, AbsDict)),
    log_trace(info,'[Abstract] Generated new abstract: ~w', [event(AbastractID,AbsDict)]).

% Thread-safe message send
queue_exists(QueueName) :-
    catch(message_queue_property(QueueName,_),_,fail).

safe_thread_send_message(QueueName, Message) :-
    ( queue_exists(QueueName) ->
        thread_send_message(QueueName, Message)
    ; log_tace(error,' [Abstract] Message queue ~w does not exist. Message not sent.~n', [QueueName])
    ).

% List events / abstracts
list_all_events :-
    kb_shared:event(Type, Dict),
    log_tace(info,'[Abstract] Event Type: ~w~n', [Type]),
    log_tace(info,'[Abstract] Event Dict: ~w~n', [Dict]),
    fail.
list_all_events.

list_all_abstracts :-
    kb_shared:event(Type, Dict),
    get_dict(is_abstract, Dict, true),
    log_tace(info,'[Abstract] Type: ~w, ID: ~w~n', [Type, Dict.id]),
    ( get_dict(abstract_contrib, Dict, Contrib) ->
        log_tace(info,'[Abstract] Contributors:~w ~n',[Contrib])
    ;   log_tace(info,'[Abstract] No contributors.~n',[])
    ),
    fail.
list_all_abstracts.

