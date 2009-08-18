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
%%% File    : parse_trans.erl
%%% @author  : Ulf Wiger <ulf.wiger@erlang-consulting.com>
%%% @end
%%% Description : 
%%%
%%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@erlang-consulting.com>
%%%-------------------------------------------------------------------

%%% @doc Generic parse transform library for Erlang.
%%% 
%%% <p>...</p>
%%%
%%% @end

-module(parse_trans).

-export([
         inspect/4,
	 transform/4,
         revert/1
        ]).

-export([
         error/3,
         format_error/1
        ]).

-export([
         initial_context/2,
         do_inspect/4,
         do_transform/4
        ]).

-export([do_insert_forms/4]).

-export([
         context/2,
         get_pos/1,
         get_file/1,
         get_module/1,
         get_attribute/2,
         get_orig_syntax_tree/1,
         function_exists/3,
         optionally_pretty_print/3,
         pp_src/2
        ]).

-import(erl_syntax, [atom_value/1,
                     attribute_name/1,
                     attribute_arguments/1,
                     string_value/1,
                     type/1
                    ]).

-record(context, {module,
		  function,
		  arity,
                  file,
                  options}).

%% Useful macros for debugging and error reporting
-define(HERE, {?MODULE, ?LINE}).

-define(DUMMY_LINE, 9999).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            
            throw({error,get_pos(I),{unknown,R}})
        end).



%%% @spec (Reason, Form, Info) -> throw()
%%% Info = [{Key,Value}]
%%%
%%% @doc
%%% <p>Used to report errors detected during the parse transform.</p>
%%% @end
%%%
error(R, F, I) ->
    rpt_error(R, F, I),
    throw({error,get_pos(I),{unknown,R}}).


get_pos(I) when is_list(I) ->
    case proplists:get_value(form, I) of
	undefined ->
	    ?DUMMY_LINE;
	Form ->
	    erl_syntax:get_pos(Form)
    end.


%%% @spec (Forms) -> string()
%%% @doc
%%% Returns the name of the file being compiled.
%%% @end
%%%
get_file(Forms) ->
    string_value(hd(get_attribute(file, Forms))).



%%% @spec (Forms) -> atom()
%%% @doc
%%% Returns the name of the module being compiled.
%%% @end
%%%
get_module(Forms) ->
    atom_value(hd(get_attribute(module, Forms))).



%%% @spec (A, Forms) -> any()
%%% A = atom()
%%%
%%% @doc
%%% Returns the value of the first occurence of attribute A.
%%% @end
%%%
get_attribute(A, [F|Forms]) ->
    case type(F) == attribute
        andalso atom_value(attribute_name(F)) == A of
        true ->
            attribute_arguments(F);
        false ->
            get_attribute(A, Forms)
    end;
get_attribute(A, []) ->
    throw({error, ?DUMMY_LINE, {missing_attribute, A}}).


function_exists(Fname, Arity, Forms) ->
    Fns = proplists:get_value(
            functions, erl_syntax_lib:analyze_forms(Forms), []),
    lists:member({Fname,Arity}, Fns).


%%% @spec (Forms, Options) -> #context{}
%%%
%%% @doc
%%% Initializes a context record. When traversing through the form
%%% list, the context is updated to reflect the current function and 
%%% arity. Static elements in the context are the file name, the module
%%% name and the options passed to the transform function.
%%% @end
%%%
initial_context(Forms, Options) ->
    File = get_file(Forms),
    io:fwrite("File = ~p~n", [File]),
    Module = get_module(Forms),
    io:fwrite("Module = ~p~n", [Module]),
    #context{file = File,
             module = Module,
             options = Options}.

%%% @spec (Fun, Acc, Forms, Options) -> {TransformedForms, NewAcc}
%%% Fun = function()
%%% Options = [{Key,Value}]
%%%
%%% @doc
%%% Makes one pass
%%% @end
transform(Fun, Acc, Forms, Options) when is_function(Fun, 5) ->
    Context = initial_context(Forms, Options),
    File = Context#context.file,
    try do_transform(Fun, Acc, Forms, Context) of
	{_, NewForms} = Result ->
            optionally_pretty_print(NewForms, Options, Context),
            Result
    catch
        error:Reason ->
            {error,
             [{File, [{?DUMMY_LINE, ?MODULE,
                       {Reason, erlang:get_stacktrace()}}]}]};
	throw:{error, Ln, What} ->
	    {error, [{File, [{Ln, ?MODULE, What}]}], []}
    end.


do_insert_forms(above, Insert, Forms, Context) when is_list(Insert) ->
    {NewForms, _} =
        do_transform(
          fun(function, F, _Ctxt, false) ->
                  {Insert, F, [], _Recurse = false, true};
             (_, F, _Ctxt, Acc) ->
                  {F, _Recurse = false, Acc}
          end, false, Forms, Context),
    NewForms;
do_insert_forms(below, Insert, Forms, Context) when is_list(Insert) ->
    insert_below(Forms, Insert).


insert_below([F|Rest] = Forms, Insert) ->
    case type(F) of
        eof_marker ->
            Insert ++ [F];
        _ ->
            [F|insert_below(Rest, Insert)]
    end.

optionally_pretty_print(Result, Options, Context) ->
    case lists:member(pt_pp_src, Options) of
        true ->
            File = Context#context.file,
            Out = outfile(File),
            pp_src(Result, Out),
            io:fwrite("Pretty-printed in ~p~n", [Out]);
        _ ->
            io:fwrite("Will not pretty-print~n", []),
            ok
    end.


%%% @spec (Fun, Forms, Acc, Options) -> NewAcc
%%% Fun = function()
%%% @doc
%%% Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).
%%% @end
%%%
inspect(F, Acc, Forms, Options) ->
    Context = initial_context(Forms, Options),
    do_inspect(F, Acc, Forms, Context).



outfile(File) ->
    "lre." ++ RevF = lists:reverse(File),
    lists:reverse("mfx." ++ RevF).

pp_src(Res, F) ->
    Str = [io_lib:fwrite("~s~n",
                         [lists:flatten([erl_pp:form(Fm) ||
                                            Fm <- revert(Res)])])],
    file:write_file(F, list_to_binary(Str)).

%% pp_debug_info(Mod) when is_atom(Mod) ->
%%     case code:which(Mod) of
%%         F when is_list(F) ->
%%             dialyzer_utils:


%%% @spec (File) -> Forms
%%%
%%% @doc
%%% <p>Fetches a Syntax Tree representing the code before pre-processing,
%%% that is, including record and macro definitions. Note that macro
%%% definitions must be syntactically complete forms (this function
%%% uses epp_dodger).</p>
%%% @end
%%%
get_orig_syntax_tree(undefined) ->
    ?ERROR(unknown_source_file, ?HERE, []);
get_orig_syntax_tree(File) ->
    case epp_dodger:parse_file(File) of
        {ok, Forms} ->
            Forms;
        Err ->
            error(error_reading_file, ?HERE, [{File,Err}])
    end.

%%% @spec (Tree) -> Forms
%%%
%%% @doc Reverts back from Syntax Tools format to Erlang forms.
%%% <p>Note that the Erlang forms are a subset of the Syntax Tools
%%% syntax tree, so this function is safe to call even on a list of 
%%% regular Erlang forms.</p>
%%% @end
%%%
revert(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].


%%% @spec (Attr, Context) -> any()
%%% Attr = module | function | arity | options
%%% 
%%% @doc
%%% Accessor function for the Context record.
%%% @end
context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A;
context(file,     #context{file = F}    ) -> F;
context(options,  #context{options = O} ) -> O.



do_inspect(F, Acc, Forms, Context) ->
    F1 = 
        fun(Form, Acc0) ->
                Type = type(Form),
                {Recurse, Acc1} = apply_F(F, Type, Form, Context, Acc0),
                if_recurse(
                  Recurse, Form, Acc1,
                  fun(ListOfLists) ->
                          lists:foldl(
                            fun(L, AccX) ->
                                    do_inspect(
                                      F, AccX, L,
                                      update_context(Form, Context))
                            end, Acc1, ListOfLists)
                  end)
        end,
    lists:foldl(F1, Forms, Acc).


do_transform(F, Acc, Forms, Context) ->
    F1 =
	fun(Form, Acc0) ->
		Type = type(Form),
		{Before1, Form1, After1, Recurse, Acc1} =
                    case apply_F(F, Type, Form, Context, Acc0) of
			{Form1x, Rec1x, A1x} ->
			    {[], Form1x, [], Rec1x, A1x};
			{_Be1, _F1, _Af1, _Rec1, _Ac1} = Res1 ->
			    Res1
		    end,
                if_recurse(
                  Recurse, Form,
                  {Before1, Form1, After1, Acc1},
                  fun(ListOfLists) ->
                          {NewListOfLists, NewAcc} =
                              mapfoldl(
                                fun(L, AccX) ->
                                        do_transform(
                                          F, AccX, L,
                                          update_context(
                                            Form1, Context))
                                end, Acc1, ListOfLists),
                          NewForm =
                              erl_syntax:update_tree(
                                Form, NewListOfLists),
                          {Before1, NewForm, After1, NewAcc}
                  end)
	end,
    mapfoldl(F1, Acc, Forms).

apply_F(F, Type, Form, Context, Acc) ->
    try F(Type, Form, Context, Acc)
    catch
        error:Reason ->
            ?ERROR(Reason,
                   ?HERE,
                   [{type, Type},
                    {context, Context},
                    {acc, Acc},
                    {form, Form}])
    end.

if_recurse(true, Form, Else, F) ->
    case erl_syntax:subtrees(Form) of
        [] ->
            Else;
        [_|_] = ListOfLists ->
            F(ListOfLists)
    end;
if_recurse(false, _, Else, _) ->
    Else.



update_context(Form, Context0) ->
    case type(Form) of
	function ->
	    {Fun, Arity} =
		erl_syntax_lib:analyze_function(Form),
	    Context0#context{function = Fun,
			     arity = Arity};
	_ ->
	    Context0
    end.




%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {Before, Res, After, Accu1} =
	case F(Hd, Accu0) of
	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
		Result;
	    {R1, A1} ->
		{[], R1, [], A1}
	end,
    {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
    {Before ++ [Res| After ++ Rs], Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.


rpt_error(Reason, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n",
             "*** Location: ~p~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, Fun | 
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).


format_error({_Cat, Error}) ->
    Error.
