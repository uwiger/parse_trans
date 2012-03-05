%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/EPLICENSE
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is exprecs-0.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB.
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : parse_trans_codegen.erl
%%% @author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%% @end
%%% Description : 
%%%-------------------------------------------------------------------

%%% @doc Parse transform for code generation pseduo functions
%%%
%%% <p>...</p>
%%%
%%% @end

-module(parse_trans_codegen).

-export([parse_transform/2]).


%% @spec (Forms, Options) -> NewForms
%%
%% @doc
%% Searches for calls to pseudo functions in the module `codegen',
%% and converts the corresponding erlang code to a data structure
%% representing the abstract form of that code.
%%
%% The purpose of these functions is to let the programmer write
%% the actual code that is to be generated, rather than manually
%% writing abstract forms, which is more error prone and cannot be
%% checked by the compiler until the generated module is compiled.
%%
%% Supported functions:
%%
%% <h2>gen_function/2</h2>
%%
%% Usage: `codegen:gen_function(Name, Fun)'
%%
%% Substitutes the abstract code for a function with name `Name'
%% and the same behaviour as `Fntun'.
%%
%% `Fun' can either be a anonymous `fun', which is then converted to
%% a named function. It can also be an `implicit fun', e.g.
%% `fun is_member/2'. In this case, the referenced function is fetched
%% and converted to an abstract form representation. It is also renamed
%% so that the generated function has the name `Name'.
%%
%% <h2>gen_functions/1</h2>
%%
%% Takes a list of `{Name, Fun}' tuples and produces a list of abstract
%% data objects, just as if one had written
%% `[codegen:gen_function(N1,F1),codegen:gen_function(N2,F2),...]'.
%%
%% <h2>exprs/1</h2>
%%
%% Usage: `codegen:exprs(Fun)'
%%
%% `Fun' is either an anonymous function, or an implicit fun with only one
%% function clause. This "function" takes the body of the fun and produces
%% a data type representing the abstract form of the list of expressions in
%% the body. The arguments of the function clause are ignored, but can be
%% used to ensure that all necessary variables are known to the compiler.
%%
%% <h2>Variable substitution</h2>
%%
%% It is possible to do some limited expansion (importing a value
%% bound at compile-time), using the construct <code>{'$var', V}</code>, where
%% `V' is a bound variable in the scope of the call to `gen_function/2'.
%%
%% Example:
%% <pre>
%% gen(Name, X) ->
%%    codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).
%% </pre>
%%
%% After transformation, calling `gen(contains_17, 17)' will yield the
%% abstract form corresponding to:
%% <pre>
%% contains_17(L) ->
%%    lists:member(17, L).
%% </pre>
%%
%% <h2>Form substitution</h2>
%%
%% It is possible to inject abstract forms, using the construct
%% <code>{'$form', F}</code>, where `F' is bound to a parsed form in
%% the scope of the call to `gen_function/2'.
%%
%% Example:
%% <pre>
%% gen(Name, F) ->
%%    codegen:gen_function(Name, fun(X) -> X =:= {'$form',F} end).
%% </pre>
%%
%% After transformation, calling `gen(is_foo, {atom,0,foo})' will yield the
%% abstract form corresponding to:
%% <pre>
%% is_foo(X) ->
%%    X =:= foo.
%% </pre>
%% @end
%%
parse_transform(Forms, Options) ->
    {NewForms, _} =
	parse_trans:depth_first(fun xform_fun/4, _Acc = Forms, Forms, Options),
    parse_trans:revert(NewForms).

xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
	{codegen, {gen_function, 2}} ->
	    [NameF, FunF] =
	    	erl_syntax:application_arguments(Form),
	    NewForm = gen_function(NameF, FunF, erl_syntax:get_pos(Form), Acc),
	    {NewForm, Acc};
	{codegen, {gen_function, 3}} ->
	    [NameF, FunF, LineF] =
		erl_syntax:application_arguments(Form),
	    NewForm = gen_function(
			NameF, FunF, erl_syntax:integer_value(LineF), Acc),
	    {NewForm, Acc};
	{codegen, {gen_functions, 1}} ->
	    [List] = erl_syntax:application_arguments(Form),
	    Elems = erl_syntax:list_elements(List),
	    Pos = erl_syntax:get_pos(hd(Elems)),
	    NewForms = lists:map(
			 fun(E) ->
				 [NameF, FunF] = erl_syntax:tuple_elements(E),
				 gen_function(NameF, FunF, Pos, Acc)
			 end, Elems),
	    {erl_syntax:list(NewForms), Acc};
	{codegen, {exprs, 1}} ->
	    [FunF] = erl_syntax:application_arguments(Form),
	    [Clause] = erl_syntax:fun_expr_clauses(FunF),
	    [{clause,_,_,_,Body}] = parse_trans:revert([Clause]),
	    NewForm = substitute(erl_parse:abstract(Body)),
	    {NewForm, Acc};
	_ ->
	    {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.

gen_function(NameF, FunF, L, Acc) ->
    case erl_syntax:type(FunF) of
	implicit_fun ->
	    AQ = erl_syntax:implicit_fun_name(FunF),
	    Name = erl_syntax:atom_value(erl_syntax:arity_qualifier_body(AQ)),
	    Arity = erl_syntax:integer_value(
		      erl_syntax:arity_qualifier_argument(AQ)),
	    NewForm = find_function(Name, Arity, Acc),
	    ClauseForms = erl_syntax:function_clauses(NewForm),
	    {tuple, 1, [{atom, 1, function},
			{integer, 1, L},
			NameF,
			{integer, 1, Arity},
			abstract_clauses(ClauseForms)]};
	_ ->
	    ClauseForms = erl_syntax:fun_expr_clauses(FunF),
	    Arity = get_arity(ClauseForms),
	    {tuple,L,[{atom,1,function},
		      {integer, 1, L},
		      NameF,
		      {integer,1, Arity},
		      abstract_clauses(ClauseForms)]}
    end.

find_function(Name, Arity, Forms) ->
    [Form] = [F || {function,_,N,A,_} = F <- Forms,
		N == Name,
		A == Arity],
    Form.

abstract_clauses(ClauseForms) ->
    Abstract = erl_parse:abstract(parse_trans:revert(ClauseForms)),
    substitute(Abstract).

substitute({tuple,L0,
	    [{atom,_,tuple},
	     {integer,_,L},
	     {cons,_,
	      {tuple,_,[{atom,_,atom},{integer,_,_},{atom,_,'$var'}]},
	      {cons,_,
	       {tuple,_,[{atom,_,var},{integer,_,_},{atom,_,V}]},
	       {nil,_}}}]}) ->
    {call, L0, {remote,L0,{atom,L0,erl_parse},
		{atom,L0,abstract}},
     [{var, L0, V}, {integer, L0, L}]};
substitute({tuple,L0,
	    [{atom,_,tuple},
	     {integer,_,_},
	     {cons,_,
	      {tuple,_,[{atom,_,atom},{integer,_,_},{atom,_,'$form'}]},
	      {cons,_,
	       {tuple,_,[{atom,_,var},{integer,_,_},{atom,_,F}]},
	       {nil,_}}}]}) ->
    {var, L0, F};
substitute([]) ->
    [];
substitute([H|T]) ->
    [substitute(H) | substitute(T)];
substitute(T) when is_tuple(T) ->
    list_to_tuple(substitute(tuple_to_list(T)));
substitute(X) ->
    X.

get_arity(Clauses) ->
    Ays = [length(erl_syntax:clause_patterns(C)) || C <- Clauses],
    case lists:usort(Ays) of
	[Ay] ->
	    Ay;
	Other ->
	    erlang:error(ambiguous, Other)
    end.
