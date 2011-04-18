Module parse_trans
==================


<h1>Module parse_trans</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Generic parse transform library for Erlang.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-consulting.com`](mailto:ulf.wiger@erlang-consulting.com)).

<h2><a name="description">Description</a></h2>





...



<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-form">form()</a></h3>




<pre>form() = any()</pre>



<h3 class="typedecl"><a name="type-forms">forms()</a></h3>




<pre>forms() = [<a href="#type-form">form()</a>]</pre>



<h3 class="typedecl"><a name="type-insp_f">insp_f()</a></h3>




<pre>insp_f() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, A) -> {boolean(), A})</pre>



<h3 class="typedecl"><a name="type-options">options()</a></h3>




<pre>options() = [{atom(), any()}]</pre>



<h3 class="typedecl"><a name="type-type">type()</a></h3>




<pre>type() = atom()</pre>



<h3 class="typedecl"><a name="type-xform_f_df">xform_f_df()</a></h3>




<pre>xform_f_df() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -> {<a href="#type-form">form()</a>, Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, Acc})</pre>



<h3 class="typedecl"><a name="type-xform_f_rec">xform_f_rec()</a></h3>




<pre>xform_f_rec() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -> {<a href="#type-form">form()</a>, boolean(), Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, boolean(), Acc})</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#context-2">context/2</a></td><td>
Accessor function for the Context record.</td></tr><tr><td valign="top"><a href="#depth_first-4">depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_depth_first-4">do_depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_insert_forms-4">do_insert_forms/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_inspect-4">do_inspect/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_transform-4">do_transform/4</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>.</td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#function_exists-3">function_exists/3</a></td><td>
Checks whether the given function is defined in Forms.</td></tr><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td>
Returns the value of the first occurence of attribute A.</td></tr><tr><td valign="top"><a href="#get_file-1">get_file/1</a></td><td>
Returns the name of the file being compiled.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>
Returns the name of the module being compiled.</td></tr><tr><td valign="top"><a href="#get_orig_syntax_tree-1">get_orig_syntax_tree/1</a></td><td>.</td></tr><tr><td valign="top"><a href="#get_pos-1">get_pos/1</a></td><td>
Tries to retrieve the line number from an erl_syntax form.</td></tr><tr><td valign="top"><a href="#initial_context-2">initial_context/2</a></td><td>
Initializes a context record.</td></tr><tr><td valign="top"><a href="#inspect-4">inspect/4</a></td><td>
Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).</td></tr><tr><td valign="top"><a href="#optionally_pretty_print-3">optionally_pretty_print/3</a></td><td></td></tr><tr><td valign="top"><a href="#pp_beam-1">pp_beam/1</a></td><td>
Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.</td></tr><tr><td valign="top"><a href="#pp_beam-2">pp_beam/2</a></td><td>
Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.</td></tr><tr><td valign="top"><a href="#pp_src-2">pp_src/2</a></td><td>Pretty-prints the erlang source code corresponding to Forms into Out.</td></tr><tr><td valign="top"><a href="#revert-1">revert/1</a></td><td>Reverts back from Syntax Tools format to Erlang forms.</td></tr><tr><td valign="top"><a href="#top-3">top/3</a></td><td></td></tr><tr><td valign="top"><a href="#transform-4">transform/4</a></td><td>
Makes one pass.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="context-2"></a>

<h3>context/2</h3>





<pre>context(X1::Attr, Context) -> any()</pre>
<ul class="definitions"><li><pre>Attr = module | function | arity | options</pre></li></ul>




Accessor function for the Context record.<a name="depth_first-4"></a>

<h3>depth_first/4</h3>





<pre>depth_first(Fun::<a href="#type-xform_f_df">xform_f_df()</a>, Acc, Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -> {<a href="#type-forms">forms()</a>, Acc} | {error, list()}</pre>
<br></br>


<a name="do_depth_first-4"></a>

<h3>do_depth_first/4</h3>





<pre>do_depth_first(F::<a href="#type-xform_f_df">xform_f_df()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> {<a href="#type-forms">forms()</a>, term()}</pre>
<br></br>


<a name="do_insert_forms-4"></a>

<h3>do_insert_forms/4</h3>





<pre>do_insert_forms(X1::above | below, Insert::<a href="#type-forms">forms()</a>, Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> <a href="#type-forms">forms()</a></pre>
<br></br>


<a name="do_inspect-4"></a>

<h3>do_inspect/4</h3>





<pre>do_inspect(F::<a href="#type-insp_f">insp_f()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> term()</pre>
<br></br>


<a name="do_transform-4"></a>

<h3>do_transform/4</h3>





<pre>do_transform(F::<a href="#type-xform_f_rec">xform_f_rec()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> {<a href="#type-forms">forms()</a>, term()}</pre>
<br></br>


<a name="error-3"></a>

<h3>error/3</h3>





<pre>error(R::Reason, F::Form, I::Info) -> <a href="#type-throw">throw()</a></pre>
<ul class="definitions"><li><pre>Info = [{Key, Value}]</pre></li></ul>






Used to report errors detected during the parse transform.<a name="format_error-1"></a>

<h3>format_error/1</h3>





<pre>format_error(X1::{atom(), term()}) -> iolist()</pre>
<br></br>


<a name="function_exists-3"></a>

<h3>function_exists/3</h3>





<pre>function_exists(Fname::atom(), Arity::integer(), Forms) -> boolean()</pre>
<br></br>





Checks whether the given function is defined in Forms.<a name="get_attribute-2"></a>

<h3>get_attribute/2</h3>





<pre>get_attribute(A, Forms) -> any()</pre>
<ul class="definitions"><li><pre>A = atom()</pre></li></ul>




Returns the value of the first occurence of attribute A.<a name="get_file-1"></a>

<h3>get_file/1</h3>





<pre>get_file(Forms) -> string()</pre>
<br></br>





Returns the name of the file being compiled.<a name="get_module-1"></a>

<h3>get_module/1</h3>





<pre>get_module(Forms) -> atom()</pre>
<br></br>





Returns the name of the module being compiled.<a name="get_orig_syntax_tree-1"></a>

<h3>get_orig_syntax_tree/1</h3>





<pre>get_orig_syntax_tree(File) -> Forms</pre>
<br></br>







Fetches a Syntax Tree representing the code before pre-processing,
that is, including record and macro definitions. Note that macro
definitions must be syntactically complete forms (this function
uses epp_dodger).<a name="get_pos-1"></a>

<h3>get_pos/1</h3>





<pre>get_pos(I::list()) -> integer()</pre>
<br></br>





Tries to retrieve the line number from an erl_syntax form. Returns a
(very high) dummy number if not successful.<a name="initial_context-2"></a>

<h3>initial_context/2</h3>





<pre>initial_context(Forms, Options) -> #context{}</pre>
<br></br>





Initializes a context record. When traversing through the form
list, the context is updated to reflect the current function and
arity. Static elements in the context are the file name, the module
name and the options passed to the transform function.<a name="inspect-4"></a>

<h3>inspect/4</h3>





<pre>inspect(F::Fun, Acc::Forms, Forms::Acc, Options) -> NewAcc</pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li></ul>




Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).<a name="optionally_pretty_print-3"></a>

<h3>optionally_pretty_print/3</h3>





<pre>optionally_pretty_print(Result::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>, Context::#context{}) -> ok</pre>
<br></br>


<a name="pp_beam-1"></a>

<h3>pp_beam/1</h3>





<pre>pp_beam(Beam::<a href="file.md#type-filename">file:filename()</a>) -> string() | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.<a name="pp_beam-2"></a>

<h3>pp_beam/2</h3>





<pre>pp_beam(Beam::<a href="#type-filename">filename()</a>, Out::<a href="#type-filename">filename()</a>) -> ok | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.<a name="pp_src-2"></a>

<h3>pp_src/2</h3>





<pre>pp_src(Res::Forms, Out::<a href="#type-filename">filename()</a>) -> ok</pre>
<br></br>




Pretty-prints the erlang source code corresponding to Forms into Out
<a name="revert-1"></a>

<h3>revert/1</h3>





<pre>revert(Tree) -> Forms</pre>
<br></br>




Reverts back from Syntax Tools format to Erlang forms.


Note that the Erlang forms are a subset of the Syntax Tools
syntax tree, so this function is safe to call even on a list of
regular Erlang forms.<a name="top-3"></a>

<h3>top/3</h3>





<pre>top(F::function(), Forms::<a href="#type-forms">forms()</a>, Options::list()) -> <a href="#type-forms">forms()</a> | {error, term()}</pre>
<br></br>


<a name="transform-4"></a>

<h3>transform/4</h3>





<pre>transform(Fun, Acc, Forms, Options) -> {TransformedForms, NewAcc}</pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li><li><pre>Options = [{Key, Value}]</pre></li></ul>




Makes one pass