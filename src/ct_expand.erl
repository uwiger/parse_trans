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
%% @end
-module(ct_expand).
-export([parse_transform/2]).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    {NewForms,_} = 
        parse_trans:depth_first(fun xform_fun/4, [], Forms, Options),
    parse_trans:revert(NewForms).


xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?MODULE, {term, 1}} ->
            Args = erl_syntax:application_arguments(Form),
            RevArgs = parse_trans:revert(Args),
            case erl_eval:exprs(RevArgs, []) of
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
xform_fun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.
  
