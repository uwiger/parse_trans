Module parse_trans_pp
=====================


<h1>Module parse_trans_pp</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Generic parse transform library for Erlang.



__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<h2><a name="description">Description</a></h2>





This module contains some useful utility functions for inspecting
the results of parse transforms or code generation.
The function `main/1` is called from escript, and can be used to   
pretty-print debug info in a .beam file from a Linux shell.

Using e.g. the following bash alias:
<pre>
   alias pp='escript $PARSE_TRANS_ROOT/ebin/parse_trans_pp.beam'
   </pre>

   
a file could be pretty-printed using the following command:

`$ pp ex_codegen.beam | less`

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#main-1">main/1</a></td><td></td></tr><tr><td valign="top"><a href="#pp_beam-1">pp_beam/1</a></td><td>
Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.</td></tr><tr><td valign="top"><a href="#pp_beam-2">pp_beam/2</a></td><td>
Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.</td></tr><tr><td valign="top"><a href="#pp_src-2">pp_src/2</a></td><td>Pretty-prints the erlang source code corresponding to Forms into Out.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="main-1"></a>

<h3>main/1</h3>





<pre>main(X1::[string()]) -> any()</pre>
<br></br>


<a name="pp_beam-1"></a>

<h3>pp_beam/1</h3>





<pre>pp_beam(Beam::<a href="#type-filename">filename()</a>) -> string() | {error, Reason}</pre>
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
