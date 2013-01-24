-module(test_exprecs).

-pt_renumber(true).
-pt_log_forms(true).

-export([f/0]).

-compile({parse_transform, exprecs}).

-record(r, {a = 0 :: integer(), b = 0 :: integer(), c = 0 :: integer()}).
-record(s, {a}).

-export_records([r, s]).


f() ->
    foo.
