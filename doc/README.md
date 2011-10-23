

#The parse_trans application#


__Authors:__ Ulf Wiger ([`ulf.wiger@erlang-consulting.com`](mailto:ulf.wiger@erlang-consulting.com)).

A generic parse transform library
This library is intended to simplify the task of writing parse transform
modules for Erlang.



#Introduction to parse transforms#




##The simplest transform##




The very simplest transform we can make is one that doesn't
change a thing. For convenience, we will at least print the forms.
This will enlighten us as to what the forms actually look like.

<pre>
-module(test_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:fwrite("Forms = ~p~n", [Forms]),
    Forms.
</pre>



Trying this with a very simple module:

<pre>
-module(ex1).
-export([add/2]).

add(X,Y) ->
    X + Y.
</pre>

<pre>
1> c(ex1, [{parse_transform,test_pt}]).
Forms = [{attribute,1,file,{"./ex1.erl",1}},
         {attribute,1,module,ex1},
         {attribute,2,export,[{add,2}]},
         {function,4,add,2,
                   [{clause,4,
                            [{var,4,'X'},{var,4,'Y'}],
                            [],
                            [{op,5,'+',{var,5,'X'},{var,5,'Y'}}]}]},
         {eof,6}]
{ok,ex1}
</pre>



##`transform/4`##



...




#Current limitations#



...


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ct_expand.md" class="module">ct_expand</a></td></tr>
<tr><td><a href="exprecs.md" class="module">exprecs</a></td></tr>
<tr><td><a href="parse_trans.md" class="module">parse_trans</a></td></tr>
<tr><td><a href="parse_trans_codegen.md" class="module">parse_trans_codegen</a></td></tr>
<tr><td><a href="parse_trans_mod.md" class="module">parse_trans_mod</a></td></tr>
<tr><td><a href="parse_trans_pp.md" class="module">parse_trans_pp</a></td></tr></table>

