%% The contents of this file are subject to the Erlang Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.erlang.org/license/EPL1_0.txt
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is exprecs-0.2.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%% All Rights Reserved.
%%
%% Contributor(s): ______________________________________.

%%-------------------------------------------------------------------
%% File    : ct_expand.erl
%% @author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @end
%% Description :
%%
%% Created : 7 Apr 2010 by Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%-------------------------------------------------------------------

%% @doc Compile-time expansion utility
%%
%% This module serves as an example of parse_trans-based transforms,
%% but might also be a useful utility in its own right.
%% The transform searches for calls to the pseudo-function
%% `ct_expand:term(Expr)', and then replaces the call site with the
%% result of evaluating `Expr' at compile-time.
%%
%% For example, the line
%%
%% `ct_expand:term(lists:sort([3,5,2,1,4]))'
%%
%% would be expanded at compile-time to `[1,2,3,4,5]'.
%%
%% ct_expand has now been extended to also evaluate calls to local functions.
%% See examples/ct_expand_test.erl for some examples, and also limitations.
%% Specifically, using functions that return funs, that are then passed to other
%% functions, doesn't work.
%%
%% A debugging facility exists: passing the option {ct_expand_trace, Flags} as an option,
%% or adding a compiler attribute -ct_expand_trace(Flags) will enable a form of call trace.
%%
%% `Flags' can be `[]' (no trace) or `[F]', where `F' is `c' (call trace),
%% `r' (return trace), or `x' (exception trace)'.
%%
%% @end
-module(ct_expand).
-export([parse_transform/2]).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    Trace = ct_trace_opt(Options, Forms),
    case parse_trans:depth_first(fun(T,F,C,A) ->
					 xform_fun(T,F,C,A,Forms, Trace)
				 end, [], Forms, Options) of
	{error, Es} ->
	    Es ++ Forms;
	{NewForms, _} ->
	    parse_trans:revert(NewForms)
    end.

ct_trace_opt(Options, Forms) ->
    case proplists:get_value(ct_expand_trace, Options) of
	undefined ->
	    case [Opt || {attribute,_,ct_expand_trace,Opt} <- Forms] of
		[] ->
		    [];
		[_|_] = L ->
		    lists:last(L)
	    end
    end.

xform_fun(application, Form, _Ctxt, Acc, Forms, Trace) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?MODULE, {term, 1}} ->
	    LFH = fun(Name, Args, Bs) ->
			  eval_lfun(
			    extract_fun(Name, length(Args), Forms),
			    Args, Bs, Forms, Trace)
		  end,
	    Args = erl_syntax:application_arguments(Form),
            RevArgs = parse_trans:revert(Args),
            case erl_eval:exprs(RevArgs, [], {eval, LFH}) of
                {value, Value,[]} ->
                    {erl_syntax:abstract(Value), Acc};
                Other ->
                    parse_trans:error(cannot_evaluate,?LINE,
                                      [{expr, RevArgs},
                                       {error, Other}])
            end;
        _ ->
            {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc, _, _) ->
    {Form, Acc}.

extract_fun(Name, Arity, Forms) ->
    case [F_ || {function,_,N_,A_,_Cs} = F_ <- Forms,
		N_ == Name, A_ == Arity] of
	[] ->
	    erlang:error({undef, [{Name, Arity}]});
	[FForm] ->
	    FForm
    end.

eval_lfun({function,L,F,_,Clauses}, Args, Bs, Forms, Trace) ->
    try
	begin
	    {ArgsV, Bs1} = lists:mapfoldl(
			     fun(A, Bs_) ->
				     {value,AV,Bs1_} =
					 erl_eval:expr(A, Bs_, lfh(Forms, Trace)),
				     {erl_parse:abstract(AV), Bs1_}
			     end, Bs, Args),
	    Expr = {call, L, {'fun', L, {clauses, lfun_rewrite(Clauses, Forms)}}, ArgsV},
	    call_trace(Trace =/= [], L, F, ArgsV),
	    {value, Ret, _} =
		erl_eval:expr(Expr, erl_eval:new_bindings(), lfh(Forms, Trace)),
	    ret_trace(lists:member(r, Trace) orelse lists:member(x, Trace),
		      L, F, Args, Ret),
	    %% restore bindings
	    {value, Ret, Bs1}
	end
    catch
	error:Err ->
	    exception_trace(lists:member(x, Trace), L, F, Args, Err),
	    error(Err)
    end.

lfh(Forms, Trace) ->
    {eval, fun(Name, As, Bs1) ->
		   eval_lfun(
		     extract_fun(Name, length(As), Forms),
		     As, Bs1, Forms, Trace)
	   end}.

call_trace(false, _, _, _) -> ok;
call_trace(true, L, F, As) ->
    io:fwrite("ct_expand (~w): call ~w(~p)~n", [L, F, As]).

ret_trace(_, _, _, _, _) -> ok.

exception_trace(false, _, _, _, _) -> ok;
exception_trace(true, L, F, Args, Err) ->
    io:fwrite("ct_expand (~w): exception from ~w/~w: ~p~n", [L, F, length(Args), Err]).


lfun_rewrite(Exprs, Forms) ->
    parse_trans:plain_transform(
      fun({'fun',L,{function,F,A}}) ->
	      {function,_,_,_,Cs} = extract_fun(F, A, Forms),
	      {'fun',L,{clauses, Cs}};
	 (_) ->
	      continue
      end, Exprs).
