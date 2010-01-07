-module(test).
-compile({parse_transform, test_pt}).

-export([f/1]).
-export_records([r]).
-record(r, {a = [1,2,3],
            b}).

f(X) ->
    X.
