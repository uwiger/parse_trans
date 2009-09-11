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
                     
