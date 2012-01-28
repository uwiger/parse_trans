

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



<pre>forms() = [<a href="#type-form">form()</a>]</pre>



###<a name="type-insp_f">insp_f()</a>##



<pre>insp_f() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, A) -> {boolean(), A})</pre>



###<a name="type-options">options()</a>##



<pre>options() = [{atom(), any()}]</pre>



###<a name="type-type">type()</a>##



<pre>type() = atom()</pre>



###<a name="type-xform_f_df">xform_f_df()</a>##



<pre>xform_f_df() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -> {<a href="#type-form">form()</a>, Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, Acc})</pre>



###<a name="type-xform_f_rec">xform_f_rec()</a>##



<pre>xform_f_rec() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -> {<a href="#type-form">form()</a>, boolean(), Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, boolean(), Acc})</pre>
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
Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).</td></tr><tr><td valign="top"><a href="#optionally_pretty_print-3">optionally_pretty_print/3</a></td><td></td></tr><tr><td valign="top"><a href="#plain_transform-2">plain_transform/2</a></td><td>
Performs a transform of <code>Forms</code> using the fun <code>Fun(Form)</code>.</td></tr><tr><td valign="top"><a href="#pp_beam-1">pp_beam/1</a></td><td>
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




<pre>depth_first(Fun::<a href="#type-xform_f_df">xform_f_df()</a>, Acc, Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -> {<a href="#type-forms">forms()</a>, Acc} | {error, list()}</pre>
<br></br>


<a name="do_depth_first-4"></a>

###do_depth_first/4##




<pre>do_depth_first(F::<a href="#type-xform_f_df">xform_f_df()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> {<a href="#type-forms">forms()</a>, term()}</pre>
<br></br>


<a name="do_insert_forms-4"></a>

###do_insert_forms/4##




<pre>do_insert_forms(X1::above | below, Insert::<a href="#type-forms">forms()</a>, Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> <a href="#type-forms">forms()</a></pre>
<br></br>


<a name="do_inspect-4"></a>

###do_inspect/4##




<pre>do_inspect(F::<a href="#type-insp_f">insp_f()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> term()</pre>
<br></br>


<a name="do_transform-4"></a>

###do_transform/4##




<pre>do_transform(F::<a href="#type-xform_f_rec">xform_f_rec()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -> {<a href="#type-forms">forms()</a>, term()}</pre>
<br></br>


<a name="error-3"></a>

###error/3##




<pre>error(R::Reason, F::Form, I::Info) -> <a href="#type-throw">throw()</a></pre>
<ul class="definitions"><li><pre>Info = [{Key, Value}]</pre></li></ul>






Used to report errors detected during the parse transform.<a name="export_function-3"></a>

###export_function/3##




`export_function(F, Arity, Forms) -> any()`

<a name="format_error-1"></a>

###format_error/1##




<pre>format_error(Error::{atom(), term()}) -&gt; iolist()</pre>
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




<pre>optionally_pretty_print(Result::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>, Context::#context{}) -> ok</pre>
<br></br>


<a name="plain_transform-2"></a>

###plain_transform/2##




<pre>plain_transform(Fun, Forms) -> <a href="#type-forms">forms()</a></pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li><li><pre>Forms = <a href="#type-forms">forms()</a></pre></li></ul>






Performs a transform of `Forms` using the fun `Fun(Form)`. `Form` is always
an Erlang abstract form, i.e. it is not converted to syntax_tools
representation. The intention of this transform is for the fun to have a
catch-all clause returning `continue`. This will ensure that it stays robust  
against additions to the language.



`Fun(Form)` must return either of the following:



* `NewForm` - any valid form
* `continue` - dig into the sub-expressions of the form
* `{done, NewForm}` - Replace `Form` with `NewForm`; return all following
forms unchanged
* `{error, Reason}` - Abort transformation with an error message.

Example - This transform fun would convert all instances of `P ! Msg` to
`gproc:send(P, Msg)`:
<pre>
  parse_transform(Forms, _Options) ->
      parse_trans:plain_transform(fun do_transform/1, Forms).
 
  do_transform({'op', L, '!', Lhs, Rhs}) ->
       [NewLhs] = parse_trans:plain_transform(fun do_transform/1, [Lhs]),
       [NewRhs] = parse_trans:plain_transform(fun do_transform/1, [Rhs]),
      {call, L, {remote, L, {atom, L, gproc}, {atom, L, send}},
       [NewLhs, NewRhs]};
  do_transform(_) ->
      continue.
  </pre><a name="pp_beam-1"></a>

###pp_beam/1##




<pre>pp_beam(Beam::<a href="file.md#type-filename">file:filename()</a>) -> string() | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.<a name="pp_beam-2"></a>

###pp_beam/2##




<pre>pp_beam(Beam::<a href="#type-filename">filename()</a>, Out::<a href="#type-filename">filename()</a>) -> ok | {error, Reason}</pre>
<br></br>





Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.<a name="pp_src-2"></a>

###pp_src/2##




<pre>pp_src(Res::Forms, Out::<a href="#type-filename">filename()</a>) -> ok</pre>
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




<pre>top(F::function(), Forms::<a href="#type-forms">forms()</a>, Options::list()) -> <a href="#type-forms">forms()</a> | {error, term()}</pre>
<br></br>


<a name="transform-4"></a>

###transform/4##




<pre>transform(Fun, Acc, Forms, Options) -&gt; {TransformedForms, NewAcc}</pre>
<ul class="definitions"><li><pre>Fun = function()</pre></li><li><pre>Options = [{Key, Value}]</pre></li></ul>




Makes one pass