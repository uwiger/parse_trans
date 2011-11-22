

#Module parse_trans#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Generic parse transform library for Erlang.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-consulting.com`](mailto:ulf.wiger@erlang-consulting.com)).<a name="description"></a>

##Description##




...

<a name="types"></a>

##Data Types##




###<a name="type-form">form()</a>##



<pre>form() = any()</pre>



###<a name="type-forms">forms()</a>##



<pre>forms() = [[form()](#type-form)]</pre>



###<a name="type-insp_f">insp_f()</a>##



<pre>insp_f() = fun(([type()](#type-type), [form()](#type-form), #context{}, A) -&gt; {boolean(), A})</pre>



###<a name="type-options">options()</a>##



<pre>options() = [{atom(), any()}]</pre>



###<a name="type-type">type()</a>##



<pre>type() = atom()</pre>



###<a name="type-xform_f_df">xform_f_df()</a>##



<pre>xform_f_df() = fun(([type()](#type-type), [form()](#type-form), #context{}, Acc) -&gt; {[form()](#type-form), Acc} | {[forms()](#type-forms), [form()](#type-form), [forms()](#type-forms), Acc})</pre>



###<a name="type-xform_f_rec">xform_f_rec()</a>##



<pre>xform_f_rec() = fun(([type()](#type-type), [form()](#type-form), #context{}, Acc) -&gt; {[form()](#type-form), boolean(), Acc} | {[forms()](#type-forms), [form()](#type-form), [forms()](#type-forms), boolean(), Acc})</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#context-2">context/2</a></td><td>
Accessor function for the Context record.</td></tr><tr><td valign="top"><a href="#depth_first-4">depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_depth_first-4">do_depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_insert_forms-4">do_insert_forms/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_inspect-4">do_inspect/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_transform-4">do_transform/4</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>.</td></tr><tr><td valign="top"><a href="#export_function-3">export_function/3</a></td><td></td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#function_exists-3">function_exists/3</a></td><td>
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
Erlang source code, storing it in the file Out.</td></tr><tr><td valign="top"><a href="#pp_src-2">pp_src/2</a></td><td>Pretty-prints the erlang source code corresponding to Forms into Out.</td></tr><tr><td valign="top"><a href="#replace_function-4">replace_function/4</a></td><td></td></tr><tr><td valign="top"><a href="#revert-1">revert/1</a></td><td>Reverts back from Syntax Tools format to Erlang forms.</td></tr><tr><td valign="top"><a href="#top-3">top/3</a></td><td></td></tr><tr><td valign="top"><a href="#transform-4">transform/4</a></td><td>
Makes one pass.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="context-2"></a>

###context/2##




<pre>context(X1::Attr, Context) -&gt; any()</pre>
<ul class="definitions"><li><pre>Attr = module | function | arity | options</pre></li></ul>




Accessor function for the Context record.<a name="depth_first-4"></a>

###depth_first/4##




<pre>depth_first(Fun::[xform_f_df()](#type-xform_f_df), Acc, Forms::[forms()](#type-forms), Options::[options()](#type-options)) -&gt; {[forms()](#type-forms), Acc} | {error, list()}</pre>
<br></br>


<a name="do_depth_first-4"></a>

###do_depth_first/4##




<pre>do_depth_first(F::[xform_f_df()](#type-xform_f_df), Acc::term(), Forms::[forms()](#type-forms), Context::#context{}) -&gt; {[forms()](#type-forms), term()}</pre>
<br></br>


<a name="do_insert_forms-4"></a>

###do_insert_forms/4##




<pre>do_insert_forms(X1::above | below, Insert::[forms()](#type-forms), Forms::[forms()](#type-forms), Context::#context{}) -&gt; [forms()](#type-forms)</pre>
<br></br>


<a name="do_inspect-4"></a>

###do_inspect/4##




<pre>do_inspect(F::[insp_f()](#type-insp_f), Acc::term(), Forms::[forms()](#type-forms), Context::#context{}) -&gt; term()</pre>
<br></br>


<a name="do_transform-4"></a>

###do_transform/4##




<pre>do_transform(F::[xform_f_rec()](#type-xform_f_rec), Acc::term(), Forms::[forms()](#type-forms), Context::#context{}) -&gt; {[forms()](#type-forms), term()}</pre>
<br></br>


<a name="error-3"></a>

###error/3##




<pre>error(R::Reason, F::Form, I::Info) -&gt; [throw()](#type-throw)</pre>
<ul class="definitions"><li><pre>Info = [{Key, Value}]</pre></li></ul>






Used to report errors detected during the parse transform.<a name="export_function-3"></a>

###export_function/3##




`export_function(F, Arity, Forms) -> any()`

<a name="format_error-1"></a>

###format_error/1##




<pre>format_error(X1::{atom(), term()}) -&gt; iolist()</pre>
<br></br>


<a name="function_exists-3"></a>

###function_exists/3##




<pre>function_exists(Fname::atom(), Arity::integer(), Forms) -&gt; boolean()</pre>
<br></br>





Checks whether the given function is defined in Forms.<a name="get_attribute-2"></a>

###get_attribute/2##




<pre>get_attribute(A, Forms) -&gt; any()</pre>
<ul class="definitions"><li><pre>A = atom()</pre></li></ul>




Returns the value of the first occurence of attribute A.<a name="get_file-1"></a>

###get_file/1##




<pre>get_file(Forms) -&gt; string()</pre>
<br></br>





Returns the name of the file being compiled.<a name="get_module-1"></a>

###get_module/1##




<pre>get_module(Forms) -&gt; atom()</pre>
<br></br>





Returns the name of the module being compiled.<a name="get_orig_syntax_tree-1"></a>

###get_orig_syntax_tree/1##




<pre>get_orig_syntax_tree(File) -&gt; Forms</pre>
<br></br>







Fetches a Syntax Tree representing the code before pre-processing,
that is, including record and macro definitions. Note that macro
definitions must be syntactically complete forms (this function
uses epp_dodger).<a name="get_pos-1"></a>

###get_pos/1##




<pre>get_pos(I::list()) -&gt; integer()</pre>
<br></br>





Tries to retrieve the line number from an erl_syntax form. Returns a
(very high) dummy number if not successful.<a name="initial_context-2"></a>

###initial_context/2##




<pre>initial_context(Forms, Options) -&gt; #context{}</pre>
<br></br>





Initializes a context record. When traversing through the form
list, the context is updated to reflect the current function and
arity. Static elements in the context are the file name, the module
name and the options passed to the transform function.<a name="inspect-4"></a>

###inspect/4##




<pre>inspect(F::Fun, Acc::Forms, Forms::Acc, Options) -&gt; NewAcc</pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li></ul>




Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).<a name="optionally_pretty_print-3"></a>

###optionally_pretty_print/3##




<pre>optionally_pretty_print(Result::[forms()](#type-forms), Options::[options()](#type-options), Context::#context{}) -&gt; ok</pre>
<br></br>


<a name="pp_beam-1"></a>

###pp_beam/1##




<pre>pp_beam(Beam::[file:filename()](file.md#type-filename)) -&gt; string() | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.<a name="pp_beam-2"></a>

###pp_beam/2##




<pre>pp_beam(Beam::[filename()](#type-filename), Out::[filename()](#type-filename)) -&gt; ok | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.<a name="pp_src-2"></a>

###pp_src/2##




<pre>pp_src(Res::Forms, Out::[filename()](#type-filename)) -&gt; ok</pre>
<br></br>




Pretty-prints the erlang source code corresponding to Forms into Out
<a name="replace_function-4"></a>

###replace_function/4##




`replace_function(F, Arity, NewForm, Forms) -> any()`

<a name="revert-1"></a>

###revert/1##




<pre>revert(Tree) -&gt; Forms</pre>
<br></br>




Reverts back from Syntax Tools format to Erlang forms.


Note that the Erlang forms are a subset of the Syntax Tools
syntax tree, so this function is safe to call even on a list of
regular Erlang forms.<a name="top-3"></a>

###top/3##




<pre>top(F::function(), Forms::[forms()](#type-forms), Options::list()) -&gt; [forms()](#type-forms) | {error, term()}</pre>
<br></br>


<a name="transform-4"></a>

###transform/4##




<pre>transform(Fun, Acc, Forms, Options) -&gt; {TransformedForms, NewAcc}</pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li><li><pre>Options = [{Key, Value}]</pre></li></ul>




Makes one pass