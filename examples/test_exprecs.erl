-module(test_exprecs).

-export([f/0]).

-compile({parse_transform, exprecs}).

-record(r, {a, b, c}).

-export_records([r]).


f() ->
    {new, '#new-r'([])}.
