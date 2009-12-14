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

-module(exprecs).

-export([parse_transform/2,
	 format_error/1,
%	 transform/3,
	 context/2]).

-record(context, {module,
		  function,
		  arity}).

-record(pass1, {exports = [],
		generated = false,
		records = [],
                versions = orddict:new(),
                inserted = false}).

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            
            throw({error,get_pos(I),{unknown,R}})
        end).

get_pos(I) ->
    case proplists:get_value(form, I) of
	undefined ->
	    0;
	Form ->
	    erl_syntax:get_pos(Form)
    end.

parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
    io:fwrite("in do_transform/2~n", []),
    Acc1 = versioned_records(
	     parse_trans:do_inspect(fun inspect_f/4, #pass1{}, Forms, Context)),
    io:fwrite("Acc1 = ~p~n", [Acc1]),
    {Forms2, Acc2} =
	parse_trans:do_transform(fun generate_f/4, Acc1, Forms, Context),
    parse_trans:revert(verify_generated(Forms2, Acc2, Context)).


inspect_f(attribute, {attribute,_L,record,RecDef}, _Ctxt, Acc) ->
    Recs0 = Acc#pass1.records,
    {false, Acc#pass1{records = [RecDef|Recs0]}};
inspect_f(attribute, {attribute,_L,export_records, E}, _Ctxt, Acc) ->
    Exports0 = Acc#pass1.exports,
    NewExports = Exports0 ++ E,
    {false, Acc#pass1{exports = NewExports}};
inspect_f(_Type, _Form, _Context, Acc) ->
    {false, Acc}.

generate_f(attribute, {attribute,L,export_records,_} = Form, _Ctxt,
	    #pass1{exports = [_|_] = Es, versions = Vsns,
                   inserted = false} = Acc) ->
    case check_record_names(Es, L, Acc) of
	ok -> continue;
	{error, Bad} ->
	    ?ERROR(invalid_record_exports, ?HERE, Bad)
    end,
    Exports = [{fname(new), 1},
	       {fname(info), 1},
	       {fname(info), 2},
	       {fname(get), 2},
	       {fname(set), 2},
	       {fname(fromlist), 2} |
	       lists:flatmap(
		 fun(Rec) ->
			 RecS = atom_to_list(Rec),
			 FNew = fname(new, RecS),
			 [{FNew, 0}, {FNew,1},
			  {fname(get, RecS), 2},
			  {fname(set, RecS), 2},
			  {fname(fromlist, RecS), 2},
			  {fname(info, RecS), 1}]
		 end, Es)] ++ version_exports(Vsns),
    {[], Form,
     [{attribute,L,export,Exports}],
     false, Acc#pass1{inserted = true}};
generate_f(function, Form, _Context, #pass1{exports = [_|_],
					    generated = false} = Acc) ->
    % Layout record funs before first function
    L = erl_syntax:get_pos(Form),
    Funs = generate_accessors(L, Acc),
    {Funs, Form, [], false, Acc#pass1{generated = true}};
generate_f(_Type, Form, _Ctxt, Acc) ->
    {Form, false, Acc}.

verify_generated(Forms, #pass1{} = Acc, _Context) ->
    case (Acc#pass1.generated == true) orelse (Acc#pass1.exports == []) of
	true ->
	    Forms;
	false ->
	    % should be re-written to use the parse_trans helper...?
	    [{eof,Last}|RevForms] = lists:reverse(Forms),
	    [{function, NewLast, _, _, _}|_] = RevAs = 
		lists:reverse(generate_accessors(Last, Acc)),
	    lists:reverse([{eof, NewLast+1} | RevAs] ++ RevForms)
    end.


check_record_names(Es, L, #pass1{records = Rs}) ->
    case [E || E <- Es,
               not(lists:keymember(E, 1, Rs))] of
        [] ->
            ok;
        Bad ->
            {error, [{L,E} || E <- Bad]}
    end.

versioned_records(#pass1{exports = Es, records = Rs} = Pass1) ->
    io:fwrite("versioned_records/1~n", []),
    case split_recnames(Rs) of
        [] ->
            Pass1#pass1{versions = []};
        [_|_] = Versions ->
            Exp_vsns =
                lists:foldl(
                  fun(Re, Acc) ->
                          case orddict:find(atom_to_list(Re), Versions) of
                              {ok, Vs} ->
                                  orddict:store(Re, Vs, Acc);
                              error ->
                                  Acc
                          end
                  end, orddict:new(), Es),
            Pass1#pass1{versions = Exp_vsns}
    end.

version_exports([]) ->
    [];
version_exports([_|_] = _Vsns) ->
    [{list_to_atom(fname_prefix(info)), 3},
     {list_to_atom(fname_prefix(convert)), 2}].


version_accessors(_L, #pass1{versions = []}) ->
    [];
version_accessors(L, #pass1{versions = Vsns}) ->
    Flat_vsns = flat_versions(Vsns),
    [f_convert(Vsns, L),
     f_info_3(Vsns, L)]
        ++ [f_info_1(Rname, L, V) || {Rname,V} <- Flat_vsns].

flat_versions(Vsns) ->
    lists:flatmap(fun({R,Vs}) ->
                          [{R,V} || V <- Vs]
                  end, Vsns).

split_recnames(Rs) ->
    lists:foldl(
      fun({R,_As}, Acc) ->
              case regexp:split(atom_to_list(R), "__") of
                  {ok, [Base, V]} ->
                      orddict:append(Base,V,Acc);
                  {ok,[_]} ->
                      Acc
              end
      end, orddict:new(), Rs).

% pass(Forms, Fun, Acc) ->
%     {NewTree, NewAcc} = transform(Forms, Fun, Acc),
%     NewForms = [erl_syntax:revert(T) || T <- lists:flatten(NewTree)],
%     {NewForms, NewAcc}.


generate_accessors(L, Acc) ->
    [f_new_(Acc, L),
     f_info(Acc, L),
     f_info_2(Acc, L),
     f_get(Acc, L),
     f_set(Acc, L),
     f_fromlist(Acc, L) |
     lists:concat(
       lists:map(
	 fun(Rname) ->
		 Fields = get_flds(Rname, Acc),
		 [f_new_0(Rname, L),
		  f_new_1(Rname, L),
		  f_get_2(Rname, Fields, L),
		  f_set_2(Rname, Fields, L),
		  f_fromlist_2(Rname, Fields, L),
		  f_info_1(Rname, L)]
	 end, Acc#pass1.exports))] ++ version_accessors(L, Acc).

get_flds(Rname, #pass1{records = Rs}) ->
    {value, {_, Flds}} = lists:keysearch(Rname, 1, Rs),
    lists:map(
      fun({record_field,_, {atom,_,N}}) -> N;
	 ({record_field,_, {atom,_,N}, _}) -> N
      end, Flds).



fname_prefix(Op) ->
    case Op of
	new -> "#new-";
	get -> "#get-";
	set -> "#set-";
	fromlist -> "#fromlist-";
	info -> "#info-";
        convert -> "#convert-"
    end.

fname_prefix(Op, Rname) ->
    fname_prefix(Op) ++ str(Rname).

str(A) when is_atom(A) ->
    atom_to_list(A);
str(S) when is_list(S) ->
    S.

fname(Op) ->
    list_to_atom(fname_prefix(Op)).

fname(Op, Rname) ->
    list_to_atom(fname_prefix(Op, Rname)).


fname(Op, Rname, V) ->
    list_to_atom(fname_prefix(Op, Rname) ++ "__" ++ V).

%%% Accessor functions
%%%
f_new_(#pass1{exports = Es}, L) ->
    {function, L, fname(new), 1,
     [{clause, L, [{atom, L, Re}], [],
       [{call, L, {atom, L, fname(new, Re)}, []}]}
      || Re <- Es]}.

f_new_0(Rname, L) ->
    {function, L, fname(new, Rname), 0,
     [{clause, L, [], [],
       [{record, L, Rname, []}]}]}.


f_new_1(Rname, L) ->
    {function, L, fname(new, Rname), 1,
     [{clause, L, [{var, L, 'Vals'}], [],
       [{call, L, {atom, L, fname(set, Rname)},
	 [{var, L, 'Vals'},
	  {record, L, Rname, []}
	 ]}]
       }]}.

f_set_2(Rname, Flds, L) ->
    {function, L, fname(set, Rname), 2,
     [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
       [{match, L, {var, L, 'F'},
	 {'fun', L, 
	  {clauses, 
	   [{clause, L, [{nil,L},
			 {var,L,'R'},
			 {var,L,'_F1'}],
	     [],
	     [{var, L, 'R'}]} |
	    [{clause, L, 
	      [{cons, L, {tuple, L, [{atom, L, Attr},
				     {var,  L, 'V'}]},
		{var, L, 'T'}},
	       {var, L, 'R'},
	       {var, L, 'F1'}],
	      [],
	      [{call, L, {var, L, 'F1'},
		[{var,L,'T'},
		 {record, L, {var,L,'R'}, Rname,
		  [{record_field, L,
		    {atom, L, Attr},
		    {var, L, 'V'}}]},
		 {var, L, 'F1'}]}]} || Attr <- Flds]]}}},
	{call, L, {var, L, 'F'}, [{var, L, 'Vals'},
				  {var, L, 'Rec'},
				  {var, L, 'F'}]}]}]}.

f_fromlist_2(Rname, Flds, L) ->
    Fname = fname(fromlist, Rname),
    FldList = erl_parse:abstract(
		lists:zip(Flds, lists:seq(2, length(Flds)+1))),
    {function, L, Fname, 2,
     [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
       [{match, L, {var, L, 'AttrNames'}, FldList},
	{match, L, {var, L, 'F'},
	 {'fun', L,
	  {clauses,
	   [{clause, L, [{nil, L},
			 {var, L,'R'},
			 {var, L,'_F1'}],
	     [],
	     [{var, L, 'R'}]},
	    {clause, L, [{cons, L,
			  {tuple, L, [{var, L, 'H'},
				      {var, L, 'Pos'}]},
			  {var, L, 'T'}},
			 {var, L, 'R'}, {var, L, 'F1'}],
	     [],
	     [{'case', L, {call, L, {remote, L,
				     {atom,L,lists},{atom,L,keyfind}},
			   [{var,L,'H'},{integer,L,1},{var,L,'Vals'}]},
	       [{clause, L, [{atom,L,false}], [],
		 [{call, L, {var, L, 'F1'}, [{var, L, 'T'},
					     {var, L, 'R'},
					     {var, L, 'F1'}]}]},
		{clause, L, [{tuple, L, [{var,L,'_'},{var,L,'Val'}]}],
		 [],
		 [{call, L, {var, L, 'F1'},
		   [{var, L, 'T'},
		    {call, L, {atom, L, 'setelement'},
		     [{var, L, 'Pos'}, {var, L, 'R'}, {var, L, 'Val'}]},
		    {var, L, 'F1'}]}]}
	       ]}
	     ]}
	   ]}}},
	{call, L, {var, L, 'F'}, [{var, L, 'AttrNames'},
				  {var, L, 'Rec'},
				  {var, L, 'F'}]}
       ]}
     ]}.


f_get_2(Rname, Flds, L) ->
    FName = fname(get, Rname),
    {function, L, FName, 2,
     [{clause, L, [{var, L, 'Attrs'}, {var, L, 'R'}],
       [[{call, L, {atom, L, is_list}, [{var, L, 'Attrs'}]}]],
       [{lc, L, {call, L, {atom, L, FName}, [{var, L, 'A'}, {var, L, 'R'}]},
	 [{generate, L, {var, L, 'A'}, {var, L, 'Attrs'}}]}]
       } |
      [{clause, L, [{atom, L, Attr}, {var, L, 'R'}], [],
	[{record_field, L, {var, L, 'R'}, Rname, {atom, L, Attr}}]} ||
	  Attr <- Flds]]
    }.


f_info(_Acc, L) ->
    Fname = list_to_atom(fname_prefix(info)),
    {function, L, Fname, 1,
     [{clause, L,
       [{var, L, 'RecName'}], [],
       [{call, L, {atom, L, Fname}, [{var, L, 'RecName'}, {atom, L, fields}]}]
      }]}.
%%%    {function, L, Fname, 1,
%%%     [{clause, L,
%%%       [{var, L, 'Rec'}],
%%%       [[{call, L,
%%%	  {atom, L, is_record},
%%%	  [{var, L, 'Rec'}, {atom, L, R}]}]],
%%%       [{call, L, {atom, L, fname(info, R)}, [{atom, L, fields}]}]} ||
%%%	 R <- Acc#pass1.exports]}.

f_info_2(Acc, L) ->
    Fname = list_to_atom(fname_prefix(info)),
    {function, L, Fname, 2,
     [{clause, L,
       [{atom, L, R},
	{var, L, 'Info'}],
       [],
       [{call, L, {atom, L, fname(info, R)}, [{var, L, 'Info'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_info_3(Versions, L) ->
    Fname = list_to_atom(fname_prefix(info)),
    F = {function, L, Fname, 3,
     [{clause, L,
       [{atom, L, R},
        {var, L, 'Info'},
        {string, L, V}],
       [],
       [{call, L, {atom, L, fname(info,R,V)}, [{var, L, 'Info'}]}]} ||
         {R,V} <- flat_versions(Versions)]},
    io:fwrite("Versions = ~p~n", [Versions]),
    io:fwrite("F = ~p~n", [F]),
    F.


f_get(Acc, L) ->
    Fname = list_to_atom(fname_prefix(get)),
    {function, L, Fname, 2,
     [{clause, L,
       [{var, L, 'Attrs'},
	{var, L, 'Rec'}],
       [[{call, L,
	  {atom, L, is_record},
	  [{var, L, 'Rec'}, {atom, L, R}]}]],
       [{call, L, {atom, L, fname(get, R)}, [{var, L, 'Attrs'},
					     {var, L, 'Rec'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_set(Acc, L) ->
    Fname = list_to_atom(fname_prefix(set)),
    {function, L, Fname, 2,
     [{clause, L,
       [{var, L, 'Vals'},
	{var, L, 'Rec'}],
       [[{call, L,
	  {atom, L, is_record},
	  [{var, L, 'Rec'}, {atom, L, R}]}]],
       [{call, L, {atom, L, fname(set, R)}, [{var, L, 'Vals'},
					     {var, L, 'Rec'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_fromlist(Acc, L) ->
    Fname = list_to_atom(fname_prefix(fromlist)),
    {function, L, Fname, 2,
     [{clause, L,
       [{var, L, 'Vals'},
	{var, L, 'Rec'}],
       [[{call, L,
	  {atom, L, is_record},
	  [{var, L, 'Rec'}, {atom, L, R}]}]],
       [{call, L, {atom, L, fname(fromlist, R)}, [{var, L, 'Vals'},
						  {var, L, 'Rec'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_info_1(Rname, L) ->
    {function, L, fname(info, Rname), 1,
     [{clause, L, [{atom, L, fields}], [],
       [{call, L, {atom, L, record_info},
	 [{atom, L, fields}, {atom, L, Rname}]}]
      },
      {clause, L, [{atom, L, size}], [],
       [{call, L, {atom, L, record_info},
	 [{atom, L, size}, {atom, L, Rname}]}]
      }]}.

f_info_1(Rname, L, V) ->
    f_info_1(recname(Rname, V), L).

recname(Rname, V) ->
    list_to_atom(lists:concat([Rname,"__",V])).

f_convert(_Vsns, L) ->
    {function, L, fname(convert), 2,
     [{clause, L,
       [{var, L, 'FromVsn'},
        {var, L, 'Rec'}],
       [[{call,L,{atom, L, is_tuple},
         [{var, L, 'Rec'}]}]],
       [{match, L, {var, L, 'Rname'},
         {call, L, {atom, L, element},
          [{integer, L, 1}, {var, 1, 'Rec'}]}},
        {match,L,{var,L,'Size'},
         {call, L, {atom, L, fname(info)},
          [{var,L,'Rname'}, {atom, L, size}, {var,L,'FromVsn'}]}},
        {match, L, {var, L, 'Size'},
         {call, L, {atom, L, size},
          [{var, L, 'Rec'}]}},
        %%
        {match, L, {var, L, 'Old_fields'},
         {call, L, {atom,L,fname(info)},
            [{var,L,'Rname'},{atom,L,fields},{var,L,'FromVsn'}]}},
        {match, L, {var, L, 'New_fields'},
         {call, L, {atom,L,fname(info)},
            [{var,L,'Rname'},{atom,L,fields}]}},
        {match, L, {var, L, 'Common_fields'},
         {op, L, '--',
          {var, L, 'Old_fields'},
          {op, L, '--',
           {var, L, 'Old_fields'},
           {var, L, 'New_fields'}}}},
        %%
        {match, L, {var, L, 'Values'},
         {call, L, {remote, L, {atom, L, lists}, {atom, L, zip}},
          [{call, L, {atom,L,fname(info)},
            [{var,L,'Rname'},{atom,L,fields},{var,L,'FromVsn'}]},
           {call, L, {atom, L, 'tl'},
            [{call, L, {atom, L, tuple_to_list},
              [{var, L, 'Rec'}]}]}]}},
        {match, L, {tuple, L, [{var, L, 'Matching'},
                               {var, L, 'Discarded'}]},
         {call, L, {remote, L, {atom, L, lists}, {atom, L, partition}},
          [{'fun',L,
            {clauses,
             [{clause,L,
               [{tuple,L,[{var,L,'F'},{var,L,'_'}]}],
               [],
               [{call,L,
                 {remote,L,{atom,L,lists},{atom,L,member}},
                 [{var, L, 'F'}, {var,L,'New_fields'}]}]}]}},
           {var, L, 'Values'}]}},
        {tuple, L, [{call, L, {atom, L, fname(set)},
                     [{var, L, 'Matching'},
                      {call, L, {atom, L, fname(new)},
                       [{var, L, 'Rname'}]}]},
                    {var, L, 'Discarded'}]}]
      }]}.
           

%%% ========== generic parse_transform stuff ==============

context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A.


% transform(Forms, F, Acc) ->
%     case  [{L,M} || {attribute, L, module, M} <- Forms] of
% 	[{_,Module}] ->
% 	    transform(Forms, F, #context{module = Module}, Acc);
% 	[] ->
% 	    ?ERROR(missing_module_attribute, ?HERE, []);
% 	[_|_] = Multiple ->
% 	    ?ERROR(multiple_module_attributes, ?HERE,
% 		   [{L,{module,M}} || {L,M} <- Multiple])
%     end.

% transform(Forms, F, Context, Acc) ->
%     F1 =
% 	fun(Form, Acc0) ->
% 		Type = erl_syntax:type(Form),
% 		{Before1, Form1, After1, Recurse, Acc1} =
% 		    try F(Type, Form, Context, Acc0) of
% 			{F1, Rec1, A1} ->
% 			    {[], F1, [], Rec1, A1};
% 			{_Be1, _F1, _Af1, _Rec1, _Ac1} = Res1 ->
% 			    Res1
% 		    catch
% 			error:Reason ->
% 			    ?ERROR(Reason,
% 				   ?HERE,
% 				   [{type, Type},
% 				    {context, Context},
% 				    {acc, Acc},
% 				    {form, Form}])
% 		    end,
% 		if Recurse == true ->
% 			case erl_syntax:subtrees(Form1) of
% 			    [] ->
% 				{Before1, Form1, After1, Acc1};
% 			    ListOfLists ->
% 				{NewListOfLists, NewAcc} =
% 				    mapfoldl(
% 				      fun(L, AccX) ->
% 					      transform(
% 						L, F, 
% 						new_context(
% 						  Form1, Context), AccX)
% 				      end, Acc1, ListOfLists),
% 				NewForm =
% 				    erl_syntax:update_tree(
% 				      Form, NewListOfLists),
% 				{Before1, NewForm, After1, NewAcc}
% 			end;
% 		   true ->
% 			{Before1, Form1, After1, Acc1}
% 		end
% 	end,
%     mapfoldl(F1, Acc, Forms).


% new_context(Form, Context0) ->
%     case erl_syntax:type(Form) of
% 	function ->
% 	    {Fun, Arity} =
% 		erl_syntax_lib:analyze_function(Form),
% 	    Context0#context{function = Fun,
% 			     arity = Arity};
% 	_ ->
% 	    Context0
%     end.




%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
% mapfoldl(F, Accu0, [Hd|Tail]) ->
%     {Before, Res, After, Accu1} =
% 	case F(Hd, Accu0) of
% 	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
% 		Result;
% 	    {R1, A1} ->
% 		{[], R1, [], A1}
% 	end,
%     {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
%     {Before ++ [Res| After ++ Rs], Accu2};
% mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.



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
