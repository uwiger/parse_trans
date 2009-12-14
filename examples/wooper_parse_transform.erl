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
%%% File    : exprecs.erl
%%% @author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% @end
%%% Description : 
%%%
%%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------

%%% @doc Parse transform for generating record access functions.
%%% <p>This parse transform can be used to reduce compile-time 
%%% dependencies in large systems.</p>
%%% <p>In the old days, before records, Erlang programmers often wrote
%%% access functions for tuple data. This was tedious and error-prone.
%%% The record syntax made this easier, but since records were implemented
%%% fully in the pre-processor, a nasty compile-time dependency was 
%%% introduced.</p>
%%% <p>This module automates the generation of access functions for 
%%% records. While this method cannot fully replace the utility of 
%%% pattern matching, it does allow a fair bit of functionality on 
%%% records without the need for compile-time dependencies.</p>
%%% <p>Whenever record definitions need to be exported from a module,
%%% inserting a compiler attribute,
%%% <code>export_records([RecName|...])</code> causes this transform
%%% to lay out access functions for the exported records:</p>
%%%
%%% <pre>
%%% -module(foo)
%%% -compile({parse_transform, dia_exprecs}).
%%%
%%% -record(a, {a, b, c}).
%%% -export_records([a]).
%%% -export(['#info-'/1, '#info-'/2,
%%%          '#get-'/2, '#set-'/2,
%%%          '#new-a'/0, '#new-a'/1,
%%%          '#get-a'/2, '#set-a'/2,
%%%          '#info-a'/1]).
%%%
%%% '#info-'(Rec) -&gt;
%%%     '#info-'(Rec, fields).
%%%
%%% '#info-'(a, Info) -&gt;
%%%     '#info-a'(Info).
%%%
%%% '#new-a'() -&gt; #a{}.
%%% '#new-a'(Vals) -&gt; '#set-a'(Vals, #a{}).
%%%
%%% '#get-'(Attrs, Rec) when is_record(Rec, a) -&gt;
%%%     '#get-a'(Attrs, Rec).
%%%
%%% '#get-a'(Attrs, R) when is_list(Attrs) -&gt;
%%%     ['#get-a'(A, R) || A &lt;- Attrs];
%%% '#get-a'(a, R) -&gt; R#a.a;
%%% '#get-a'(b, R) -&gt; R#a.b;
%%% '#get-a'(c, R) -&gt; R#a.c.
%%%
%%% '#set-'(Vals, Rec) when is_record(Rec, a) -&gt;
%%%     '#set-a'(Vals, Rec).
%%%
%%% '#set-a'(Vals, Rec) -&gt;
%%%     F = fun ([], R, _F1) -> R;
%%%             ([{a, V} | T], R, F1) -&gt; F1(T, R#a{a = V}, F1);
%%%             ([{b, V} | T], R, F1) -&gt; F1(T, R#a{b = V}, F1);
%%%             ([{c, V} | T], R, F1) -&gt; F1(T, R#a{c = V}, F1)
%%%         end,
%%%     F(Vals, Rec, F).
%%%
%%% '#info-a'(fields) -&gt; record_info(fields, a);
%%% '#info-a'(size) -&gt; record_info(size, a).
%%% </pre>
%%% @end

-module(wooper_parse_transform).

-export([parse_transform/2]).
-export([format_error/1]).

-import(erl_syntax, [atom/1, application/3]).

parse_transform(Forms0, Options) ->
    %%
    %% get macros using epp_dodger
    %%
    Forms = restructure(Forms0),
    Ctxt = parse_trans:initial_context(Forms, Options),
    File = parse_trans:context(file, Ctxt),
    try
	Attrs = combine_attrs(wooper_get_attrs(Forms)),
	io:fwrite("Attrs = ~p~n", [Attrs]),
	WInfo = wooper_info(Attrs, Ctxt),
	WRec = {wooper_info, [{record_field, 1, {atom,1,K},
			       erl_parse:abstract(V)} ||
				 {K,V} <- WInfo]},
	SRec = wooper_state_rec(WInfo),
	Above = 
        [{attribute, 1, export_records, [wooper_info, wooper_state]},
         {attribute, 1, record, WRec},
         {attribute, 1, record, SRec}],
	io:fwrite("Above = ~p~n", [Above]),
	Functions = functions(Forms),
	MethodsToAdd = methods_to_add(WInfo, Functions),
	io:fwrite("MethodsToAdd = ~p~n", [MethodsToAdd]),
	{NewExports, NewForms} = generate_new(WInfo, Functions),
	Methods = generate_methods(MethodsToAdd),
	ConstructExport = [{construct,
			    length(
			      proplists:get_value(attributes, WInfo))+1}],
	AllExports = (NewExports ++
		      ConstructExport ++
		      info(member_methods,WInfo) ++
		      info(static_methods,WInfo) ++
		      [M || {M,_} <- MethodsToAdd]),
	MethodExport = exports(AllExports, Forms),
	Add = fun(Where, What, Fs) ->
		      parse_trans:do_insert_forms(Where, What, Fs, Ctxt)
	      end,
	Result = exprecs:parse_transform(Add(above, [MethodExport|Above],
					     Add(below, NewForms,
						 Add(below, Methods,
						     rewrite_funs(
						       Forms,Ctxt)))),
					 [{exprecs_strict, false}|Options]),
	% right now, the option to pretty-print is hard-coded...
	parse_trans:optionally_pretty_print(
	  Result, [{pt_pp_src,true},{pt_log_forms,true}|Options], Ctxt),
	Result
    catch
	throw:{?MODULE, {Ln,R,I}} ->
	    {error, [{File, [{Ln, ?MODULE, {R, I}}]}], []}
    end.

%% a quick-and-dirty way to ensure that attributes and exports appear before
%% all function definitions
restructure(Forms) ->
    Attrs = [A || {attribute,_,_,_} = A <- Forms],
    NonAttrs = [F || F <- Forms,
		     element(1, F) =/= attribute],
    Attrs ++ NonAttrs.
		  


exports(Es, Forms) ->
    FormInfo = erl_syntax_lib:analyze_forms(Forms),
    Existing = proplists:get_value(exports, FormInfo, []),
    {attribute,1,export,
     [E || E <- Es,
	   not lists:member(E, Existing)]}.

info(K, List) ->
    proplists:get_value(K, List, []).

rewrite_funs(Forms, Ctxt) ->
    {NewForms, _} =
	parse_trans:do_transform(
	  fun(application, Form, _C, Acc) ->
		  Op = erl_syntax:application_operator(Form),
		  Args = erl_syntax:application_arguments(Form),
		  Arity = length(Args),
		  case erl_syntax:type(Op) of
		      atom ->
			  NewForm =
			      rewrite_f(erl_syntax:atom_value(Op),
					Arity, Args, Form),
			  {NewForm,true,Acc};
		      _ ->
			  {Form, true, Acc}
		  end;
	     (_, Form, _C, Acc) ->
		  {Form, true, Acc}
	  end, [], Forms, Ctxt),
    NewForms.

rewrite_f(F, Arity, Args, Form) ->
    case lists:member({F,Arity}, wooper_bifs()) of
	true ->
	    application(atom(wooper), atom(F), Args);
	false ->
	    Form
    end.
    
wooper_bifs() ->
    [{setAttribute, 3},
     {setAttributes, 2},
     {getAttribute, 2},
     {executeOneway, 3},
     {executeRequest, 3}].

% old_stuff(Forms, Options) ->
%     Ctxt = parse_trans:initial_context(Forms, Options),
%     File = parse_trans:context(file, Ctxt),
%     try
% 	Dodge = parse_trans:get_orig_syntax_tree(File),
% 	Macros = wooper_get_macros(Dodge),
% 	io:fwrite("Macros = ~p~n", [Macros]),
% 	WInfo = wooper_info_old(Macros, Ctxt),
% 	WRec = {wooper_info, [{record_field, 1, {atom,1,K},
% 			       erl_parse:abstract(V)} ||
% 				 {K,V} <- WInfo]},
% 	SRec = wooper_state_rec_old(Macros),
% 	Above = 
%         [{attribute, 1, export_records, [wooper_info, wooper_state]},
%          {attribute, 1, record, WRec},
%          {attribute, 1, record, SRec}],
% 	io:fwrite("Above = ~p~n", [Above]),
% 	MethodsToAdd = methods_to_add(WInfo, Forms),
% 	io:fwrite("MethodsToAdd = ~p~n", [MethodsToAdd]),
% 	Methods = generate_methods(MethodsToAdd),
% 	MethodExport = {attribute,1,export,[M || {M,_} <- MethodsToAdd]},
% 	Add = fun(Where, What, Fs) ->
% 		      parse_trans:do_insert_forms(Where, What, Fs, Ctxt)
% 	      end,
% 	Result = exprecs:parse_transform(Add(above, [MethodExport|Above],
% 					     Add(below, Methods, Forms)),
% 					 Options),
% 	parse_trans:optionally_pretty_print(
% 	  Result, Options, Ctxt),
% 	Result
%     catch
% 	throw:{?MODULE, {Ln,R,I}} ->
% 	    {error, [{File, [{Ln, ?MODULE, {R, I}}]}], []}
%     end.

format_error({unknown_superclass, I}) ->
    io_lib:fwrite("WOOPS! Unknown superclass: ~p~n", [I]).


wooper_get_attrs(Forms) ->
    lists:foldl(
      fun(F, Acc) ->
	      case erl_syntax:type(F) of
		  attribute ->
		      Name = erl_syntax:atom_value(
			       erl_syntax:attribute_name(F)),
		      io:fwrite("Name = ~p~n", [Name]),
		      case atom_to_list(Name) of
			  "wooper_" ++ _ ->
			      [Arg] = erl_syntax:attribute_arguments(F),
			      io:fwrite("Arg = ~p~n", [Arg]),
			      [{Name, erl_syntax:concrete(Arg)}|Acc];
			  _ -> Acc
		      end;
		  _ ->
		      Acc
	      end
      end, [], Forms).

combine_attrs(Attrs) ->
    lists:foldl(fun({A,Vs}, D) ->
			orddict:append_list(A, Vs, D)
		end, orddict:new(), Attrs).

% wooper_get_macros(Forms) ->
%     Attrs = [a_info(A) ||
%                 A <- Forms,
%                 erl_syntax:type(A) == attribute,
%                 erl_syntax:atom_value(
%                   erl_syntax:attribute_name(A)) == define,
%                 is_wooper_define(A)],
%     Attrs.

wooper_info(Attrs, Ctxt) ->
    SuperClasses = proplists:get_value(wooper_superclasses,Attrs,[]),
    LocalClassAttrs =
        proplists:get_value(wooper_attributes,Attrs,[]),
    MethodExports = proplists:get_value(wooper_member_methods,Attrs,[]),
    StaticExports = proplists:get_value(wooper_static_methods,Attrs,[]),
    Class = parse_trans:context(module, Ctxt),
    InheritedAttrs = inherited_attributes(SuperClasses),
    ClassAttributes = attr_names(LocalClassAttrs) ++ attr_names(InheritedAttrs),
    io:fwrite("InheritedAttrs = ~p~n", [InheritedAttrs]),
    [{class, Class},
     {superclasses,SuperClasses},
     {specific_attributes, LocalClassAttrs},
     {parent_attributes, InheritedAttrs},
     {attributes, ClassAttributes},
     {member_methods, MethodExports},
     {static_methods, StaticExports},
     {inherited_methods, inherited_methods(MethodExports, SuperClasses)}].

attr_names(As) ->
    lists:map(
      fun(A) when is_atom(A) -> A;
	 ({A,T} = Attr) when is_atom(A) ->
	      case lists:member(T, [private,protected,public]) of
		  true ->
		      A;
		  false ->
		      throw({?MODULE, {999, invalid_attribute, [Attr]}})
	      end;
	 (Other) ->
	      throw({?MODULE, {999, invalid_attribute, [Other]}})
      end, As).

% wooper_info_old(Macros, Ctxt) ->
%     SuperClasses = proplists:get_value(wooper_superclasses,Macros,[]),
%     ConstructParams =
%         proplists:get_value(wooper_construct_parameters,Macros,[]),
%     MemberExports = proplists:get_value(wooper_member_export,Macros,[]),
%     MethodExports = proplists:get_value(wooper_method_export,Macros,[]),
%     Class = parse_trans:context(module, Ctxt),
%     {LocalAttrs, ParentAttrs} =
%         wooper_attributes(ConstructParams, SuperClasses),
%     AllAttrs = 
%     [{class, Class},
%      {superclasses,SuperClasses},
%      {construct_parameters, ConstructParams},
%      {specific_attributes, LocalAttrs},
%      {parent_attributes, ParentAttrs},
%      {member_exports, MemberExports},
%      {method_exports, MethodExports},
%      {inherited_methods, inherited_methods(MethodExports, SuperClasses)}].

wooper_state_rec(Info) ->
    {_, Attrs} = lists:keyfind(attributes, 1, Info),
    {wooper_state,
     [{record_field, 1, {atom,1,N}} || N <- Attrs]}.

% wooper_state_rec_old(Macros) ->
%     ConstructParams =
%         proplists:get_value(wooper_construct_parameters,Macros,[]),
%     {wooper_state,
%      [{record_field, 1, {atom,1,N}} || N <- ConstructParams]}.



%% HACK! The attribute names used to get and set state attributes 
%% are lexically derived here from the construct parameters. Maybe this is 
%% acceptable? At any rate, it would be confusing to depart from the naming
%% convention.
%%

inherited_attributes(SuperClasses) ->
    lists:concat([[{A,C} || A <- get_attributes(C)] ||
		     C <- SuperClasses]).
    

% wooper_attributes(ConstructParams, SuperClasses) ->
%     Attrs = [from_camel(P) || P <- ConstructParams],
%     SuperAttrs = lists:concat([[{A,C} || A <- get_attributes(C)] ||
%                                  C <- SuperClasses]),
%     Specific = Attrs -- [A || {A,_} <- SuperAttrs],
%     {Specific, SuperAttrs}.

inherited_methods(Local, SuperClasses) ->
    lists:foldr(
      fun(C, Acc) ->
              Methods = get_value(member_methods, C),
              case [{M,C} || M <- Methods,
                             not lists:member(M, Local)] of
                  [] -> Acc;
                  Inherited ->
                      Acc ++ Inherited
              end
      end, [], SuperClasses).

methods_to_add(WInfo, Functions) ->
    Methods = proplists:get_value(inherited_methods, WInfo),
    [{M,C} || {M,C} <- Methods,
              not lists:member(M, Functions)] ++
	wooper_builtins(Functions).

functions(Forms) ->
    FormInfo = erl_syntax_lib:analyze_forms(Forms),
    proplists:get_value(functions, FormInfo).


generate_new(WInfo, Functions) ->
    Funs = [new, new_link,
	    synchronous_new,
	    synchronous_new_link,
	    synchronous_timed_new,
	    synchronous_timed_new_link],
    L = [generate_new(Fname, WInfo, Functions) || Fname <- Funs],
    {Exports, Forms} = lists:unzip(L),
    {Exports, lists:concat(Forms)}.
	    
generate_new(Fname, WInfo, Functions) ->
    case lists:keyfind(Fname, 1, Functions) of
	false ->
	    Arity = length(proplists:get_value(attributes, WInfo, [])),
	    Class = proplists:get_value(class, WInfo),
	    L = 999,
	    Vars = vars(Arity, L),
	    Form = [{function, L, Fname, Arity,
		     [{clause, L, [V || V <- Vars], [],
		       [{call, L, {remote, L,
				   {atom,L,wooper},{atom,L,Fname}},
			 [{atom,L,Class}, mk_cons(Vars, L)]}]}]}],
	    {{Fname,Arity}, Form};
	Other ->
	    {Other, []}
    end.

mk_cons([], L) ->
    {nil,L};
mk_cons([H|T], L) ->
    {cons, L, H, mk_cons(T, L)}.

wooper_builtins(_Functions) ->
    [].

generate_methods(Methods) ->
    [abstract_method(F, A, C) || {{F,A}, C} <- Methods].


abstract_method(Fname, Arity, Class) ->
    L = 999,
    Vars = vars(Arity, L),
    {function, 999, Fname, Arity,
     [{clause, L, [V || V <- Vars], [],
       [{call, L, {remote, L, {atom, L, Class}, {atom, L, Fname}}, Vars}]}]}.
    
vars(Arity, L) ->
    [{var, L, list_to_atom("V" ++ integer_to_list(N))} ||
	N <- lists:seq(1,Arity)].

% from_camel(P) ->
%     [H|T] = atom_to_list(P),
%     list_to_atom(
%       [to_lower_c(H) | to_lower(re:replace(T,"[A-Z]","_&",[{return,list}]))]).

% to_lower_c(C) when $A =< C, C =< $Z ->
%     C + $a - $A;
% to_lower_c(C) ->
%     C.

get_attributes(C) -> get_value(specific_attributes, C).

get_value(Attr, C) ->
    try
	I = C:'#new-wooper_info'(),
	C:'#get-wooper_info'(Attr, I)
    catch
	error:undef ->
	    throw({?MODULE, {999, unknown_superclass, [{class,C}]}})
    end.



% to_lower(S) ->
%     [to_lower_c(C) || C <- S].
    
    


% a_info(A) ->
%     Name = macro_name(A),
%     Args = macro_args(A),
%     {Name, macro_def_args(Name, Args)}.

% a_args(A) -> erl_syntax:attribute_arguments(A).

% macro_name(A) ->
%     [N|_] = a_args(A),
%     erl_syntax:atom_value(N).

% macro_args(A) ->
%     [_|Args] = a_args(A),
%     Args.

% is_wooper_define(A) ->
%     lists:member(macro_name(A), defines()).

% defines() ->
%     [wooper_superclasses,
%      wooper_construct_parameters,
%      wooper_construct_export,
%      wooper_method_export,
%      wooper_member_export].

% macro_def_args(wooper_superclasses, [List]) ->
%     Elems = erl_syntax:list_elements(List),
%     [erl_syntax:atom_value(A) || A <- Elems];
% macro_def_args(wooper_construct_parameters, Args) ->
%     [erl_syntax:variable_name(V) || V <- Args];
% macro_def_args(D, Args) when D==wooper_construct_export;
%                                  D==wooper_method_export ->
%     [{erl_syntax:atom_value(erl_syntax:infix_expr_left(A)),
%       erl_syntax:integer_value(erl_syntax:infix_expr_right(A))} ||
%         A <- Args];
% macro_def_args(_, Args) ->
%     Args.
