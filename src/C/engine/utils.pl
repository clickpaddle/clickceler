:- module(utils, [
    match_all_conditions/2,
    match_condition/2,
    apply_transformations/3,
    equal_values/2,
    not_equal_values/2,
    greater_than/2,
    greater_equal/2,
    less_than/2,
    less_equal/2,
    memberchk_conv/2,
    duration_to_seconds/2,
    unit_to_seconds/2,
    log_trace/3,
    set_log_level/1,
    get_log_level/1
    ]).
    

:- use_module(library(date)).
:- use_module(library(time)).

:- dynamic current_log_level/1.

current_log_level(info).

% Log level numeric values for comparison
log_level_value(debug, 10).
log_level_value(info, 20).
log_level_value(warning, 30).
log_level_value(error, 40).

%% set_log_level(+Level)
%  Sets the minimal log level to output messages
set_log_level(Level) :-
    retractall(current_log_level(_)),
    assert(current_log_level(Level)).

%% get_log_level(-Level)
%  Gets the current minimal log level
get_log_level(Level) :-
    current_log_level(Level).

%% log_trace(+Level, +Format, +Args)
%  Logs a message if Level is >= current_log_level
log_trace(Level, Format, Args) :-
    current_log_level(CurrentLevel),
    log_level_value(Level, LevelVal),
    log_level_value(CurrentLevel, CurrentLevelVal),
    LevelVal >= CurrentLevelVal,
    get_time(TS),
    format_time(string(TimeStamp), '%Y-%m-%dT%H:%M:%S', TS),
    format('[~w] [~w] ', [TimeStamp, Level]),
    format(Format, Args),
    nl.

% Fail silently if below threshold
log_trace(Level, _Format, _Args) :-
    current_log_level(CurrentLevel),
    log_level_value(Level, LevelVal),
    log_level_value(CurrentLevel, CurrentLevelVal),
    LevelVal < CurrentLevelVal,
    !, fail.


%log_trace(Level, FormatString, Args) :-
%    get_time(TimeFloat),
%    Seconds is floor(TimeFloat),
%    Microsecs is round((TimeFloat - Seconds) * 1_000_000),
%    format_time(atom(DatePart), '%Y-%m-%dT%H:%M:%S', Seconds),
%    format(atom(Timestamp), '~w.~06d', [DatePart, Microsecs]),
%    format(atom(Message), FormatString, Args),
%    format('[~w] [~w] ~w~n', [Timestamp, Level, Message]).

%% ========================
%% === MATCH CONDITIONS ===
%% ========================

% match_all_conditions(+Conditions, +Dict)
match_all_conditions([], _).
match_all_conditions([Cond | Rest], Dict) :-
    match_condition(Cond, Dict),
    match_all_conditions(Rest, Dict).

% match_condition(+Condition, +Dict)
match_condition(eq(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    equal_values(V, Value).

match_condition(neq(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    not_equal_values(V, Value).

match_condition(gt(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    greater_than(V, Value).

match_condition(gte(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    greater_equal(V, Value).

match_condition(lt(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    less_than(V, Value).

match_condition(lte(_E,Field, Value), Dict) :-
    get_dict(Field, Dict, V),
    less_equal(V, Value).

match_condition(within(_E,Field, List), Dict) :-
    get_dict(Field, Dict, V),
    memberchk_conv(V, List).

match_condition(notin(_E,Field, List), Dict) :-
    get_dict(Field, Dict, V),
    \+ memberchk_conv(V, List).

match_condition(contains(_E, Field, SubStr), Dict) :-
    get_dict(Field, Dict, V),
    string(V),
    sub_string(V, _, _, _, SubStr).


%% ============================
%% === TRANSFORMATIONS CORE ===
%% ============================

% apply_transformations(+TransformList, +DictIn, -DictOut)
apply_transformations([], Dict, Dict).

% Transformation: set_field(Key, Value)
apply_transformations([set_field(_E,Key, Value) | Rest], DictIn, DictOut) :-
    put_dict(Key, DictIn, Value, DictNext),
    apply_transformations(Rest, DictNext, DictOut).



% Transformation: add_tag(Tag)
apply_transformations([add_tag(_E,Tag) | Rest], DictIn, DictOut) :-
    ( get_dict(tags, DictIn, Tags0) -> true ; Tags0 = [] ),
    ( memberchk(Tag, Tags0) -> Tags1 = Tags0 ; Tags1 = [Tag | Tags0] ),
    put_dict(tags, DictIn, Tags1, DictNext),
    apply_transformations(Rest, DictNext, DictOut).

% Transformation: remove_tag(Tag)
apply_transformations([remove_tag(_E,Tag) | Rest], DictIn, DictOut) :-
    ( get_dict(tags, DictIn, Tags0) -> true ; Tags0 = [] ),
    delete(Tags0, Tag, Tags1),
    put_dict(tags, DictIn, Tags1, DictNext),
    apply_transformations(Rest, DictNext, DictOut).

% Transformation: increment_field(Field)
apply_transformations([increment_field(_E,Field) | Rest], DictIn, DictOut) :-
    ( get_dict(Field, DictIn, Val), number(Val) ->
        NewVal is Val + 1,
        put_dict(Field, DictIn, NewVal, DictNext)
    ;   put_dict(Field, DictIn, 1, DictNext)
    ),
    apply_transformations(Rest, DictNext, DictOut).

% Unknown transformation
apply_transformations([Unknown | Rest], DictIn, DictOut) :-
    format('[Refine] Warning: Unknown transformation ~w~n', [Unknown]),
    apply_transformations(Rest, DictIn, DictOut).


%% ========================
%% === VALUE COMPARISON ===
%% ========================


% equal_values(+V1, +V2)
equal_values(V1, V2) :-
    to_string(V1, S1),
    to_string(V2, S2),
    S1 == S2.

% not_equal_values(+V1, +V2)
not_equal_values(V1, V2) :-
    \+ equal_values(V1, V2).

% greater_than(+V1, +V2)
greater_than(V1, V2) :-
    to_string(V1, S1),
    to_string(V2, S2),
    S1 @> S2.

% greater_equal(+V1, +V2)
greater_equal(V1, V2) :-
    to_string(V1, S1),
    to_string(V2, S2),
    S1 @>= S2.

% less_than(+V1, +V2)
less_than(V1, V2) :-
    to_string(V1, S1),
    to_string(V2, S2),
    S1 @< S2.

% less_equal(+V1, +V2)
less_equal(V1, V2) :-
    to_string(V1, S1),
    to_string(V2, S2),
    S1 @=< S2.


%% ================================
%% === MEMBERSHIP CONVERSION ====
%% ================================

% within(+Dict, +Field, +List)
atom_to_string_if_needed(AtomOrString, String) :-
    (   atom(AtomOrString) -> atom_string(AtomOrString, String)
    ;   String = AtomOrString
    ).

% Succeeds if the value of Field in the given dictionary Dict (converted to string) is in the given list List.
within(Dict, Field, List) :-
    get_dict(Field, Dict, ValueStr),  % déjà une string
    maplist(atom_to_string_if_needed, List, StringList),
    memberchk_conv(ValueStr, StringList).

% notin(+Dict, +Field, +List)
% Succeeds if the value of Field in the given dictionary Dict (converted to string) is NOT in the given list List.
notin(Dict, Field, List) :-
    get_dict(Field, Dict, Value),
    to_string(Value, S),
    \+ memberchk_string(S, List).

% to_string(+Value, -String)
% Converts various types (atom, string, number, or any term) into a string.
to_string(Value, S) :-
    (   string(Value) -> S = Value
    ;   atom(Value)   -> atom_string(Value, S)
    ;   number(Value) -> number_string(Value, S)
    ;   term_string(Value, S)  % fallback for complex terms
    ).

% memberchk_string(+String, +List)
% True if String matches any element in List after converting each element to a string.
memberchk_string(_, []) :- fail.
memberchk_string(S, [H | T]) :-
    to_string(H, SH),
    ( S == SH -> true ; memberchk_string(S, T) ).

% memberchk_conv(+Value, +List)
% Shortcut: converts Value to string and checks if it appears in List using memberchk_string/2.
memberchk_conv(Value, List) :-
    to_string(Value, S),
    memberchk_string(S, List).

duration_to_seconds(DurationAtom, Seconds) :-
    atom_chars(DurationAtom, Chars),
    append(NumberChars, [UnitChar], Chars),
    number_chars(Number, NumberChars),
    unit_to_seconds(UnitChar, UnitSeconds),
    Seconds is Number * UnitSeconds.

unit_to_seconds('s', 1).
unit_to_seconds('m', 60).
unit_to_seconds('h', 3600).
unit_to_seconds('d', 86400).
unit_to_seconds('w', 604800).
unit_to_seconds('M', 2592000).
unit_to_seconds('Y', 31536000).
