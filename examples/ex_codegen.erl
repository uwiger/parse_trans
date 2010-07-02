-module(ex_codegen).

-compile({parse_transform, parse_trans_codegen}).

-export([f/1, g/2, gen/2]).


f(Name) ->
    codegen:gen_function(
      Name,
      fun(1,2,3) ->
	      foo;
	  (A,B,C) ->
	      {A,B,C}
      end).



g(Name, V) ->
    codegen:gen_function(
      Name,
      fun(L) ->
	      member({'$var',V}, L)
      end).


gen(Name, X) ->
    codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).
