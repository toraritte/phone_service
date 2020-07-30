-module(futil).

-export(
    [
    % Functional helpers
      pipe/1
    , composeFlipped/1
    , cflip/1
    , curry/1
    , flip/3

    % General helpers
    , stitch/1
    , stringify/1
    , sanitize_string/1
    ]).

% FUNCTIONAL HELPERS ================================================== {{-
% Recursive left-to-right composition instead of a traditional one instead of (b -> c) -> (a -> b) -> (a -> c), it is (a -> b) -> (b -> c) -> ... -> (x -> y) -> (y -> z)
% See PureScript's Control.Semigroupoid.composeFlipped (>>>) or Haskell's Control.Arrow.>>>
composeFlipped([G|[]]) -> % {{-
    G;
composeFlipped([F,G|Rest]) ->
    Composition =
        fun(X) ->
            G(F(X))
        end,
    composeFlipped([Composition|Rest]).
% }}-

pipe([Arg|Functions]) ->
    (composeFlipped(Functions))(Arg).

flip(F, A, B) ->
    F(B,A).

% [ ((curry(fun flip/3))(fun string:join/2))("")
% (a -> b -> c) -> b -> a -> c
cflip(Arg) ->
    (curry(fun flip/3))(Arg).

curry(AnonymousFun) -> % {{-
    {arity, Arity} =
        erlang:fun_info(AnonymousFun, arity),

    do_curry(AnonymousFun, Arity, [[], [], []]).

%% `curry/1` internals {{-
do_curry(Fun, 0, [_Fronts, _Middle, _Ends] = X) ->
    [F, M, E] =
        lists:map(fun(L) -> string:join(L, "") end, X),
    Fstring =
        F ++ "Run(" ++ string:trim(M, trailing, ",") ++ ")" ++ E,

    {ok, Tokens, _} =
        erl_scan:string(Fstring ++ "."),
    {ok, Parsed} =
        erl_parse:parse_exprs(Tokens),

    FunBinding =
        erl_eval:add_binding(
          'Run',
          Fun,
          erl_eval:new_bindings()
        ),
    {value ,CurriedFun, _} =
        erl_eval:exprs(Parsed, FunBinding),

    CurriedFun;

do_curry(Fun, Arity, [Fronts, Middle, Ends]) ->
    VarName = [64 + Arity],
    NewFronts = ["fun(" ++ VarName ++ ") -> " | Fronts] ,
    NewMiddle = [VarName ++ ","|Middle],
    NewEnds = [" end"|Ends],
    do_curry(Fun, Arity-1, [NewFronts, NewMiddle, NewEnds]).
%    }}-
%   }}-

% }}-
% GENERERAL HELPERS =================================================== {{-

% Same as `lists:flatten(lists:join(" ", UtteranceList))`.
stitch([Utterance]) ->
    Utterance;
stitch([Utterance|Rest]) ->
    Utterance ++ " " ++ stitch(Rest).

stringify(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).

sanitize_string(String) ->
    lists:filter
        ( fun
            (Char) when Char >= 65, Char =< 90
                      ; Char >= 97, Char =< 122
                      ; Char >= 48, Char =< 57
                -> true;
            (_) -> false
          end
        , String
        ).

% }}-

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:
