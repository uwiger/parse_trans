-module(ct_expand_test).

-export([f/0]).

-compile({parse_transform, ct_expand}).
-pt_pp_src(true).

f() ->
    ct_expand:term(
      [{a, 1},
       {b, ct_expand:term(
             [{ba, 1},
              {bb, ct_expand:term(2)}])}]).

%% expand a term which calls a local function - even one which uses a fun reference.
g() ->
    ct_expand:term(zip([1,2], [a,b])).

%% this doesn't work: a function in the expanded expr returns a fun, which is then passed
%% to another function. This is because erl_eval returns results as concrete terms, which
%% must then be abstracted in order to be passed as input arguments to am interpreted
%% function. This works most of the time, but erl_parse:abstract/1 crashes on funs.
%%
%% h() ->
%%     ct_expand:term(wrap(my_fun())).

zip([H1|T1], [H2|T2]) ->
    F = fun wrap/1,
    [{F(H1),F(H2)} | zip(T1, T2)];
zip([], []) ->
    [].

wrap(X) ->
    {X}.

my_fun() ->
    fun() -> foo end.

