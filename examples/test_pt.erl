-module(test_pt).

-export([parse_transform/2]).
-compile(export_all).


parse_transform(Forms, Options) ->
    io:fwrite("Forms = ~p~n", [Forms]),
    Forms.

