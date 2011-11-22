

#Module ct_expand#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Compile-time expansion utility.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).<a name="description"></a>

##Description##




This module serves as an example of parse_trans-based transforms,
but might also be a useful utility in its own right.
The transform searches for calls to the pseudo-function
`ct_expand:term(Expr)`, and then replaces the call site with the
result of evaluating `Expr` at compile-time.



For example, the line



`ct_expand:term(lists:sort([3,5,2,1,4]))`

would be expanded at compile-time to `[1,2,3,4,5]`.

<a name="types"></a>

##Data Types##




###<a name="type-form">form()</a>##



<pre>form() = any()</pre>



###<a name="type-forms">forms()</a>##



<pre>forms() = [[form()](#type-form)]</pre>



###<a name="type-options">options()</a>##



<pre>options() = [{atom(), any()}]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse_transform-2"></a>

###parse_transform/2##




<pre>parse_transform(Forms::[forms()](#type-forms), Options::[options()](#type-options)) -&gt; [forms()](#type-forms)</pre>
<br></br>


