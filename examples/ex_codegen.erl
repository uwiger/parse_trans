-module(ex_codegen).

-compile({parse_transform, parse_trans_codegen}).

-export([f/1, g/2]).


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

