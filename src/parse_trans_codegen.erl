%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
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


parse_transform(Forms, Options) ->
    {NewForms, _} =
	parse_trans:depth_first(fun xform_fun/4, [], Forms, Options),
    parse_trans:revert(NewForms).

xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
	{codegen, {gen_function, 2}} ->
	    [NameF, FunF] =
		erl_syntax:application_arguments(Form),
	    Clauses = erl_syntax:fun_expr_clauses(FunF),
	    ClauseForms = parse_trans:revert(Clauses),
	    io:fwrite("ClauseForms = ~p~n", [ClauseForms]),
	    Arity = get_arity(ClauseForms),
	    NewClauses = erl_parse:abstract(ClauseForms),
	    NewForm = {tuple,1,[{atom,1,function},
				NameF,
				{integer,1, Arity},
				NewClauses]},
	    io:fwrite("NewForm = ~p~n", [NewForm]),
	    {NewForm, Acc};
	_ ->
	    {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.


get_arity(Clauses) ->
    Ays = [length(P) || {clause, _, P, _, _} <- Clauses],
    io:fwrite("Ays = ~p~n", [Ays]),
    case lists:usort(Ays) of
	[Ay] ->
	    Ay;
	Other ->
	    erlang:error(ambiguous, Other)
    end.
