-module(ex_codegen).

-compile({parse_transform, parse_trans_codegen}).

-export([f/1]).


f(Name) ->
    codegen:gen_function(
      Name,
      fun(1,2,3) ->
	      foo;
	  (A,B,C) ->
	      {A,B,C}
      end).



