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
%% Copyright (c) 2010 Erlang Solutions Ltd.
%% The Initial Developer of the Original Code is Ericsson AB.
%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%% All Rights Reserved.
%%
%% Contributor(s): ______________________________________.

%%-------------------------------------------------------------------
%% File    : exprecs.erl
%% @author  : Ulf Wiger <ulf.wiger@ericsson.com>
%% @end
%% Description :
%%
%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@ericsson.com>
%% Rewritten: Jan-Feb 2010 by Ulf Wiger <ulf.wiger@elang-solutions.com>
%%-------------------------------------------------------------------

%% @doc Parse transform for generating record access functions.
%% <p>This parse transform can be used to reduce compile-time
%% dependencies in large systems.</p>
%% <p>In the old days, before records, Erlang programmers often wrote
%% access functions for tuple data. This was tedious and error-prone.
%% The record syntax made this easier, but since records were implemented
%% fully in the pre-processor, a nasty compile-time dependency was
%% introduced.</p>
%% <p>This module automates the generation of access functions for
%% records. While this method cannot fully replace the utility of
%% pattern matching, it does allow a fair bit of functionality on
%% records without the need for compile-time dependencies.</p>
%% <p>Whenever record definitions need to be exported from a module,
%% inserting a compiler attribute,
%% <code>export_records([RecName|...])</code> causes this transform
%% to lay out access functions for the exported records:</p>
%%
%% <pre>
%% -module(test_exprecs).
%%
%% -record(r, {a, b, c}).
%% -export_records([r]).
%%
%% -export(['#new-'/1, '#info-'/1, '#info-'/2, '#pos-'/2,
%%          '#is_record-'/2, '#get-'/2, '#set-'/2, '#fromlist-'/2,
%%          '#new-r'/0, '#new-r'/1, '#get-r'/2, '#set-r'/2,
%%          '#pos-r'/1, '#fromlist-r'/2, '#info-r'/1]).
%%
%% '#new-'(r) -&gt; '#new-r'().
%%
%% '#info-'(RecName) -&gt; '#info-'(RecName, fields).
%%
%% '#info-'(r, Info) -&gt; '#info-r'(Info).
%%
%% '#pos-'(r, Attr) -&gt; '#pos-r'(Attr).
%%
%% '#is_record-'(r, Rec)
%%     when tuple_size(Rec) == 3, element(1, Rec) == r -&gt;
%%     true;
%% '#is_record-'(_, _) -&gt; false.
%%
%% '#get-'(Attrs, Rec) when is_record(Rec, r) -&gt;
%%     '#get-r'(Attrs, Rec).
%%
%% '#set-'(Vals, Rec) when is_record(Rec, r) -&gt;
%%     '#set-r'(Vals, Rec).
%%
%% '#fromlist-'(Vals, Rec) when is_record(Rec, r) -&gt;
%%     '#fromlist-r'(Vals, Rec).
%%
%% '#new-r'() -&gt; #r{}.
%%
%% '#new-r'(Vals) -&gt; '#set-r'(Vals, #r{}).
%%
%% '#get-r'(Attrs, R) when is_list(Attrs) -&gt;
%%     ['#get-r'(A, R) || A &lt;- Attrs];
%% '#get-r'(a, R) -&gt; R#r.a;
%% '#get-r'(b, R) -&gt; R#r.b;
%% '#get-r'(c, R) -&gt; R#r.c;
%% '#get-r'(Attr, R) -&gt;
%%     erlang:error(bad_record_op, ['#get-r', Attr, R]).
%%
%% '#set-r'(Vals, Rec) -&gt;
%%     F = fun ([], R, _F1) -&gt; R;
%%             ([{a, V} | T], R, F1) -&gt; F1(T, R#r{a = V}, F1);
%%             ([{b, V} | T], R, F1) -&gt; F1(T, R#r{b = V}, F1);
%%             ([{c, V} | T], R, F1) -&gt; F1(T, R#r{c = V}, F1);
%%             (Vs, R, _) -&gt;
%%                 erlang:error(bad_record_op, ['#set-r', Vs, R])
%%         end,
%%     F(Vals, Rec, F).
%%
%% '#fromlist-r'(Vals, Rec) -&gt;
%%     AttrNames = [{a, 2}, {b, 3}, {c, 4}],
%%     F = fun ([], R, _F1) -&gt; R;
%%             ([{H, Pos} | T], R, F1) -&gt;
%%                 case lists:keyfind(H, 1, Vals) of
%%                   false -&gt; F1(T, R, F1);
%%                   {_, Val} -&gt; F1(T, setelement(Pos, R, Val), F1)
%%                 end
%%         end,
%%     F(AttrNames, Rec, F).
%%
%% '#pos-r'(a) -&gt; 2;
%% '#pos-r'(b) -&gt; 3;
%% '#pos-r'(c) -&gt; 4;
%% '#pos-r'(_) -&gt; 0.
%%
%% '#info-r'(fields) -&gt; record_info(fields, r);
%% '#info-r'(size) -&gt; record_info(size, r).
%% </pre>
%% @end

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

-include("../include/codegen.hrl").

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            throw({error,get_pos(I),{unknown,R}})
        end).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


get_pos(I) ->
    case proplists:get_value(form, I) of
	undefined ->
	    0;
	Form ->
	    erl_syntax:get_pos(Form)
    end.

-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
%%    io:fwrite("in do_transform/2~n", []),
    Acc1 = versioned_records(
	     parse_trans:do_inspect(fun inspect_f/4, #pass1{}, Forms, Context)),
%%    io:fwrite("Acc1 = ~p~n", [Acc1]),
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
               {fname(pos), 2},
	       {fname(isrec), 1},
	       {fname(isrec), 2},
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
                          {fname(pos, RecS), 1},
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
%%    io:fwrite("versioned_records/1~n", []),
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
	      case re:split(atom_to_list(R), "__", [{return, list}]) of
                  [Base, V] ->
                      orddict:append(Base,V,Acc);
                  [_] ->
                      Acc
              end
      end, orddict:new(), Rs).


generate_accessors(L, Acc) ->
    [f_new_(Acc, L),
     f_info(Acc, L),
     f_info_2(Acc, L),
     f_pos_2(Acc, L),
     f_isrec_1(Acc, L),
     f_isrec_2(Acc, L),
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
                  f_pos_1(Rname, Fields, L),
		  f_info_1(Rname, L)]
	 end, Acc#pass1.exports))] ++ version_accessors(L, Acc).

get_flds(Rname, #pass1{records = Rs}) ->
    {_, Flds} = lists:keyfind(Rname, 1, Rs),
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
	info     -> "#info-";
        pos      -> "#pos-";
	isrec    -> "#is_record-";
        convert  -> "#convert-"
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
    Fname = fname(set, Rname),
    {function, L, Fname, 2,
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
		 {var, L, 'F1'}]}]} || Attr <- Flds]
	    ++ [{clause, L, [{var, L, 'Vs'}, {var,L,'R'},{var,L,'_'}],
		 [],
		 [bad_record_op(L, Fname, 'Vs', 'R')]}]
	    ]}}},
	{call, L, {var, L, 'F'}, [{var, L, 'Vals'},
				  {var, L, 'Rec'},
				  {var, L, 'F'}]}]}]}.

bad_record_op(L, Fname, Val, R) ->
    {call, L, {remote, L, {atom,L,erlang}, {atom,L,error}},
     [{atom,L,bad_record_op}, {cons, L, {atom, L, Fname},
			       {cons, L, {var, L, Val},
				{cons, L, {var, L, R},
				 {nil, L}}}}]}.
    


f_pos_1(Rname, Flds, L) ->
    Fname = fname(pos, Rname),
    FieldList = lists:zip(Flds, lists:seq(2, length(Flds)+1)),
    {function, L, Fname, 1,
     [{clause, L,
       [{atom, L, FldName}],
       [],
       [{integer, L, Pos}]} || {FldName, Pos} <- FieldList] ++
     [{clause, L,
       [{var, L, '_'}], [], [{integer, L, 0}]}]
    }.


f_fromlist_2(Rname, Flds, L) ->
    Fname = fname(fromlist, Rname),
    FldList = field_list(Flds),
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

field_list(Flds) ->
    erl_parse:abstract(
      lists:zip(Flds, lists:seq(2, length(Flds)+1))).



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
	  Attr <- Flds]] ++
     [{clause, L, [{var, L, 'Attr'}, {var, L, 'R'}], [],
       [bad_record_op(L, FName, 'Attr', 'R')]}]
    }.


f_info(_Acc, L) ->
    Fname = list_to_atom(fname_prefix(info)),
    {function, L, Fname, 1,
     [{clause, L,
       [{var, L, 'RecName'}], [],
       [{call, L, {atom, L, Fname}, [{var, L, 'RecName'}, {atom, L, fields}]}]
      }]}.

f_isrec_2(Acc, L) ->
    Fname = list_to_atom(fname_prefix(isrec)),
    Info = [{R,length(As) + 1} || {R,As} <- Acc#pass1.records],
    {function, L, Fname, 2,
     lists:map(
       fun({R, Ln}) ->
	       {clause, L,
		[{atom, L, R}, {var, L, 'Rec'}],
		[[{op,L,'==',
		   {call, L, {atom,L,tuple_size},[{var,L,'Rec'}]},
		   {integer, L, Ln}},
		  {op,L,'==',
		   {call,L,{atom,L,element},[{integer,L,1},
					     {var,L,'Rec'}]},
		   {atom, L, R}}]],
		[{atom, L, true}]}
       end, Info) ++
     [{clause, L, [{var,L,'_'}, {var,L,'_'}], [],
       [{atom, L, false}]}]}.


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
%%    io:fwrite("Versions = ~p~n", [Versions]),
%%    io:fwrite("F = ~p~n", [F]),
    F.

f_pos_2(Acc, L) ->
    Fname = list_to_atom(fname_prefix(pos)),
    {function, L, Fname, 2,
     [{clause, L,
       [{atom, L, R},
	{var, L, 'Attr'}],
       [],
       [{call, L, {atom, L, fname(pos, R)}, [{var, L, 'Attr'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_isrec_1(Acc, L) ->
    Fname = list_to_atom(fname_prefix(isrec)),
    {function, L, Fname, 1,
     [{clause, L,
       [{var, L, 'X'}],
       [],
       [{'if',L,
	 [{clause, L, [], [[{call, L, {atom,L,is_record},
			    [{var,L,'X'},{atom,L,R}]}]],
	   [{atom,L,true}]} || R <- Acc#pass1.exports] ++
	     [{clause,L, [], [[{atom,L,true}]],
	       [{atom, L, false}]}]}]}
     ]}.



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

-spec context(atom(), #context{}) ->
    term().
%% @hidden
context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A.



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

-spec format_error({atom(), term()}) ->
    iolist().
%% @hidden
format_error({_Cat, Error}) ->
    Error.
