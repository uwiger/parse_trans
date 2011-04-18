Module ct_expand
================


<h1>Module ct_expand</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Compile-time expansion utility.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<h2><a name="description">Description</a></h2>





This module serves as an example of parse_trans-based transforms,
but might also be a useful utility in its own right.
The transform searches for calls to the pseudo-function
`ct_expand:term(Expr)`, and then replaces the call site with the
result of evaluating `Expr` at compile-time.



For example, the line



`ct_expand:term(lists:sort([3,5,2,1,4]))`

would be expanded at compile-time to `[1,2,3,4,5]`.



<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-form">form()</a></h3>




<pre>form() = any()</pre>



<h3 class="typedecl"><a name="type-forms">forms()</a></h3>




<pre>forms() = [<a href="#type-form">form()</a>]</pre>



<h3 class="typedecl"><a name="type-options">options()</a></h3>




<pre>options() = [{atom(), any()}]</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="parse_transform-2"></a>

<h3>parse_transform/2</h3>





<pre>parse_transform(Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -> <a href="#type-forms">forms()</a></pre>
<br></br>


