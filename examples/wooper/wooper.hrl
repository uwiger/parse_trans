% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% WOOPER: Wrapper for OOP in ERlang.

% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date: Friday, July 6, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL, see:
% http://ceylan.sourceforge.net/main/documentation/wooper/index.html#license


% Provides most classical constructs: new/delete operators, remote method 
% invocation (RMI), polymorphism and multiple inheritance, all with state
% management and in a quite efficient way (i.e. no significantly faster 
% approach in Erlang could be imagined by the author).

% Instances are created thanks to the new operator, which calls automatically
% the relevant constructor ('construct' function).

% A class C is mapped to an Erlang module, preferably named 'class_C'.
% An active object is mapped to an Erlang process.
% Methods support Remote Invocation Calls, mapped to Erlang messages.
% Inheritance is implemented thanks to a per-class method virtual table,
% including the locally-defined ones and all the inherited ones.
% This table is shared among all the instances of a given class, thanks to
% a singleton-like class manager process that keeps references to the virtual
% table of each class. 
% Instance state is maintained thanks to a per-instance attribute table,
% storing all its attributes, including all the inherited ones.
%
% The hashtable type, defined in hashtable.erl, is used at all levels: 
% per-instance (for the attribute table), per-class (for the so-called virtual
% table), per-node (for the class manager).
% The proplist module could be used instead.

% When an exported function is called as a method (i.e. it is listed in 
% the wooper_method_export variable, see below) the list of parameters
% being received is prefixed with the instance state (a bit like 'self' in
% Python): A ! { aMethod, [1,2] } results in the calling of the 'aMethod'
% function defined in the class module of A (exported thanks to
% wooper_method_export) with parameters automatically given to that function
% being: 'CurrentStateOfA, 1, 2' instead of '1, 2', with 
% CurrentStateOfA being the A state variable automatically kept in the instance
% WOOPER main loop.
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2
% (it is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is
% called with only two parameters specified (state not being included).


% The usual content of the '-export([XXX]).' clause in a class module should be
% dispatched in:
%
%    '-define( wooper_method_export, YYY ).', to declare methods, ex: 
% '-define( wooper_method_export, getAge/1, setAge/2, declareBirthday/1 ).'
% Zero arity is not possible since there is at least the 'State' first 
% parameter. So one just increments the number of intended real 
% function-specific parameters in this export. 
% Ex: a function 'setAge' taking in input only one logical parameter, NewAge,
% should actually be defined as 'setAge(State,NewAge) -> [..]' and therefore
% declared as: '-define( wooper_method_export, a/1, setAge/2, b/2 ).'
% Note: one should not forget, when overloading a method F/A, to specify it in
% wooper_method_export, otherwise its closest ancestor method will be called
% instead. In this case a warning is issued at compilation of the child class:
% 'Warning: function F/A is unused.'; static methods can be declared also here. 
%
%	'-define( wooper_construct_export, new/p, new_link/p, construct/p+1, ...).'
% Ex:
% '-define( wooper_construct_export, new/2, new_link/2, construct/3, ...).' 
% to declare the appropriate construction-related functions (the 'new' 
% variations and the 'construct' operator), p being the number of
% parameters defined in the wooper_construct_parameters variable.
% Only the relevant 'construct' function has to be actually defined by the
% developer: all new variations are automatically defined appropriately
% (see in this file). 
% Declaring and implementing a toString/1 method is optional, but may be
% convenient for the debugging of method implementations.
%
%	'-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
% functions, that are not methods.
%
% Note that the dispatching of functions into wooper_method_export,
% wooper_construct_export and classical exports is done mainly for 
% self-documenting purpose (they are all just translated into the usual 
% export declarations).  
%    



% Shared code. 
%
% All WOOPER classes should mention their superclasses and their WOOPER 
% exports before the WOOPER header is included.

% Example:
% -module(class_Cat).
% -define( wooper_superclasses, [class_Mammal,class_ViviparousBeing] ).
% -define( wooper_method_export, hasWhiskers/1, canEat/2 ).
% -define( wooper_construct_parameters, Age, Gender, FurColor ).
% -define( wooper_construct_export, new/3, new_link/3, construct/4, ... ).
% -include("wooper.hrl").
% [...]
% See also: class_Template.erl
%



% Allows to define WOOPER base variables and methods for that class.



% Implementation notes.


% Records the state of an instance.
% Module is the Erlang module the class is mapped to.
% This is the class-specific object state, each instance of this class
% will have its own state_holder, quite similar to the 'C++' this pointer.
% Constant data (ex: the virtual table) are referenced by each class instance,
% they are not duplicated (pointer to a virtual table shared by all class
% instances rather than deep copy).
%
% The virtual table holds the method name to module mapping for a given class.
% The attribute table (a hashtable) records all the data members of a given
% instance, including all the inherited ones. 
% The request sender member is used internally by WOOPER so that a request
% method have a way of retrieving the corresponding caller PID. This avoids
% the caller to specify its PID twice, one for WOOPER, one for the method, as
% a method parameter, in the case the method itself needs the caller PID, for
% example to register it in a list in its own state. Thus a caller does not have
% to specify: 'MyInstance ! {my_request,[self()],self()}', specifying
% 'MyInstance ! {my_request,[],self()}' is enough: the method will be able to
% retrieve the caller PID thanks to the request_sender member, automatically
% set by WOOPER. For non-request methods (oneways), WOOPER will set
% request_sender to the atom 'undefined', to ensure the oneway crashes whenever
% trying to use this request-specific information to send a message.
% 
% Therefore when you see the first parameter of a method, 'State', it is 
% actually just an instance of the following record:
-record( state_holder, {
		virtual_table,
		attribute_table,
		request_sender
	}).



% A list could be managed that would allow to discriminate the methods from
% the other exported functions. As macros cannot be substitued in strings
% it would probably force the developer to list them twice.

% The class name, as mapped to a module.
-define(className,?MODULE).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of
% actual attributes for a given class)
-define(WooperAttributeCountUpperBound,16).


% For the name of the registered process that keeps the per-class method 
% hashtables:
-include("wooper_class_manager.hrl").


% WOOPER internal functions.

% Comment/uncomment to respectively disable and enable debug mode:
-define(wooper_debug,).


% On debug mode, methods will have to return an atom to ensure they
% respect the right format:
-ifdef(wooper_debug).

	% These methods/functions are defined for all classes:
	-define( WooperBaseMethods, get_class_name/0, get_class_name/1,
		get_superclasses/0, get_superclasses/1,
		executeRequest/2, executeRequest/3, executeOneway/2, executeOneway/3,
		delete_any_instance_referenced_in/2, wooper_destruct/1, 
		wooper_pop_from_attribute/2, wooper_check_undefined/2,
		wooper_construct_and_run/1, wooper_construct_and_run_synchronous/2,
		wooper_get_all_attributes/1, 
		is_wooper_debug/0, wooper_debug_listen/3,
		wooper_display_state/1, wooper_display_virtual_table/1,
		wooper_display_instance/1,
		wooper_get_state_description/1, wooper_get_virtual_table_description/1,
		wooper_get_instance_description/1, wooper_display_loop_state/1 ).
	
	
	-export([?WooperBaseMethods]).
	
	-ifdef(wooper_method_export).
		-export([?wooper_method_export]).
	-endif.
	
	% Must be defined, but an error message at their call should be clearer:
	-ifdef(wooper_construct_export).
		-export([?wooper_construct_export]).
	-endif.
	

	is_wooper_debug() ->
		true.


	wooper_display_loop_state(State) ->
		wooper_display_state(State).

	% Uncomment to have all WOOPER recompiled classes output verbosely their
	% information:
	%-define(wooper_log_wanted,).
	
	-ifdef(wooper_log_wanted).
		-define(wooper_log(Msg),io:format(Msg)).
		-define(wooper_log_format(Msg,Format),io:format(Msg,Format)).
	-else.	
		-define(wooper_log(Msg),no_wooper_log).
		-define(wooper_log_format(Msg,Format),no_wooper_log).
	-endif.	
	
	
-else.

	% Not in debug mode here:

	
	% These methods/functions are defined for all classes:
	-define( WooperBaseMethods, get_class_name/0, get_class_name/1,
		get_superclasses/0, get_superclasses/1,
		executeRequest/2, executeRequest/3, executeOneway/2, executeOneway/3,
		delete_any_instance_referenced_in/2, wooper_destruct/1, 
		wooper_pop_from_attribute/2, wooper_check_undefined/2,
		wooper_construct_and_run/1, wooper_construct_and_run_synchronous/2,
		wooper_get_all_attributes/1,
		is_wooper_debug/0, wooper_debug_listen/3,
		wooper_display_state/1, wooper_display_virtual_table/1,
		wooper_display_instance/1, wooper_display_loop_state/1 ).

	-export([?WooperBaseMethods]).

	-ifdef(wooper_method_export).
		-export([?wooper_method_export]).
	-endif.
	
	% Must be defined, but an error message at their call should be clearer:
	-ifdef(wooper_construct_export).
		-export([?wooper_construct_export]).
	-endif.
	

	is_wooper_debug() ->
		false.

	
	wooper_display_loop_state(_) ->
		debug_no_activated.

	-define(wooper_log(Msg),no_wooper_log).
	-define(wooper_log_format(Msg,Format),no_wooper_log).
		
-endif.


% Now that typ-checking on the state record is performed in debug mode,
% in both modes method results are sent directly:
% (no wooper_result atom added any more in debug mode)
-define(wooper_return_state_result(State,Result),{State,Result}).
-define(wooper_return_state_only(State), State).



% Returns all the attributes of this instance, as a list of {attribute_name,
% attribute_value} pairs.
wooper_get_all_attributes(State) ->
	hashtable:enumerate( State#state_holder.attribute_table ).



% Helper function to test requests.
% Allows to test from the shell an instance by sending it requests
% (hence needing a receive whereas in the shell).
% Available even when debug mode is off. 
wooper_debug_listen( Pid, Action, Arguments ) ->
	Pid ! {Action,Arguments,self()},
	receive

		% A list is assumed to be a string here:
		{wooper_result,Result} when is_list(Result) ->
			io:format("String result of call to '~w' "
				"with arguments '~w': ~s~n", [Action,Arguments,Result]);
		
		{wooper_result,Result} ->
			io:format("Result of call to '~w' with arguments '~w': ~s~n",
				[Action,Arguments,basic_utils:term_toString(Result)]);
	
		Anything ->
			io:format("Answer to call to '~w' with arguments '~w': ~s~n",
				[Action,Arguments,basic_utils:term_toString(Anything)])
	
	end.


% "Static method" (only a function) which returns the name of the class.
get_class_name() ->
	?className.


% "Static method" (only a function) which returns the list of the 
% superclasses for that class.
get_superclasses() ->
	?wooper_superclasses.


% Method that returns the classname of the instance.
get_class_name(State) ->
	?wooper_return_state_result(State,?className).

% Method that returns the superclasses of the instance.
get_superclasses(State) ->
	?wooper_return_state_result(State,?wooper_superclasses).




% The creation of an instance of a WOOPER class can be:
%
%  - either non-blocking or synchronous (and, if synchronous, with or 
% without a time-out)
%
%  - either not linked or linked to the current process
%
%  - either local (on the current node) or remote (on another node)
%


% Following construction variations are always declared, for a constructor
% expected to take N base parameters:
% new/N, new_link/N, synchronous_new/N, synchronous_new_link/N

% If use_synchronous_timed_new is defined, WOOPER adds:
% synchronous_timed_new/N, synchronous_timed_new_link/N

% If use_remote_new is defined, WOOPER adds:
% remote_new/N+1, remote_new_link/N+1, remote_synchronous_new/N+1,
% remote_synchronous_new_link/N+1.

% Finally, if use_synchronous_timed_new *and* use_remote_new are defined,
% WOOPER adds:
% remote_synchronous_timed_new/N+1, remote_synchronous_timed_new_link/N+1.

% Therefore for a template for a full-blown declaration would be:
% (in  the next block, just search and replace with your text editor A with N,
% and B with N+1)

% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
%-define( wooper_construct_export, new/A, new_link/A, 
%	synchronous_new/A, synchronous_new_link/A,
%	synchronous_timed_new/A, synchronous_timed_new_link/A,
%	remote_new/B, remote_new_link/B, remote_synchronous_new/B,
%	remote_synchronous_new_link/B, remote_synchronous_timed_new/B,
%	remote_synchronous_timed_new_link/B, construct/B ).

% Note: delete/1 can be removed if no special destructor is to be defined.


% There are construction operators that just take the construction parameters,
% ?wooper_construct_parameters (like new/N), and other operators that take
% an additional parameter, the target node (like remote_new/N+1).
% 
% As wooper_construct_parameters can be void
% (i.e. declared as '-define(wooper_construct_parameters,).'), 
% the declaration 'new( ?wooper_construct_parameters )' would be ok but
% 'remote_new( Node,?wooper_construct_parameters )' would result in the
% incorrect syntax 'remote_new( Node,)'.
% A solution could be, depending on wooper_construct_parameters being 
% void or not, to define either 'remote_new( Node )' (if void) or
% 'remote_new( Node,?wooper_construct_parameters )' (if not void).
% However the Erlang macros do not seem to support tests based on their value 
% (we can only test whether they are defined or not), thus the
% following convention has been used:

% wooper_construct_parameters should be defined if and only if there is
% at least one parameter to be declared.
%
% Therefore, in the case of a constructor taking two parameters, X and Y, 
% we will have:
%
% -define(wooper_construct_parameters,X,Y).
% ...
% construct(State,?wooper_construct_parameters) ->
% ...
%
% whereas in the case of a constructor taking no parameter, we will have:
%
% ...
% construct(State) ->
% ...
%

% i.e. there will be in this case no: '-define(wooper_construct_parameters,)'.


% First case: wooper_construct_parameters is defined:

-ifdef(wooper_construct_parameters).

% Spawns a new instance for this class, using specified parameters to
% construct it.
% Returns the PID of the newly created instance.
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
new( ?wooper_construct_parameters ) ->
	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]), 
	% Double-list: list with a list in it.
	spawn( ?MODULE, wooper_construct_and_run,
		[[?wooper_construct_parameters]] ).
	


% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created and linked instance.
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
new_link( ?wooper_construct_parameters ) ->
	% Double-list: list with a list in it.
	spawn_link( ?MODULE, wooper_construct_and_run,
		[[?wooper_construct_parameters]] ).



% Spawns a new instance for this class, using specified parameters to
% construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
synchronous_new( ?wooper_construct_parameters ) ->
	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]), 
	% Double-list: list with a list in it, and ++ used to allow for empty
	% parameter list.
	SpawnedPid = spawn( ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.
	

% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
synchronous_new_link( ?wooper_construct_parameters ) ->
	% Double-list: list with a list in it.
	SpawnedPid= spawn_link( ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
	
	% Blocks until the spawned process answers:	
	% (no risk of synchronous spawns mismatch, as each synchronous calls is
	% waited for)
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.



% Uncomment to activate synchronous new with time-out:
-define(use_synchronous_timed_new,).


-ifdef(use_synchronous_timed_new).


% The duration in milliseconds before a time-out is triggered after a created
% instance does not seem to answer properly and after a reasonable duration:
-define(synchronous_time_out,5000).
			
			
% Spawns a new instance for this class, using specified parameters to
% construct it.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new( ?wooper_construct_parameters ) ->
	SpawnedPid = spawn( ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



% Spawns a new instance for this class, and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new_link( ?wooper_construct_parameters ) ->
	SpawnedPid = spawn_link( ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



-endif. % use_synchronous_timed_new



% If use_remote_new is defined, following construction variations will be
% automatically defined (class implementor will have to declare them):
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then having remote_new/N+1.
% 



% Uncomment to activate remote new constructions:
-define(use_remote_new,).


-ifdef(use_remote_new).


% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
% If Node does not exist, a useless pid is returned. 
% Returns the PID of the newly created instance.
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new( Node, ?wooper_construct_parameters ) ->
	spawn( Node, ?MODULE, wooper_construct_and_run,
		[ [?wooper_construct_parameters] ] ).


% Spawns a new instance for this class on specified interconnected node, 
% and links it to the current process, using specified parameters to 
% construct it.
% If Node does not exist, a useless pid is returned. 
% Returns the PID of the newly created instance.
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new_link( Node, ?wooper_construct_parameters ) ->
	spawn_link( Node, ?MODULE, wooper_construct_and_run,
		[ [?wooper_construct_parameters] ] ).



% Spawns a new instance for this class on specified interconnected node, 
% using specified parameters to construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: remote_synchronous_new will return only 
% when the created process reports it is up and running.
remote_synchronous_new( Node, ?wooper_construct_parameters ) ->
	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]), 
	% Double-list: list with a list in it.
	SpawnedPid = spawn( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.
	

% Spawns a new instance for this class on specified interconnected node 
% and links it to the current process, using specified parameters 
% to construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: remote_synchronous_new_link will return only 
% when the created process reports it is up and running.
remote_synchronous_new_link( Node, ?wooper_construct_parameters ) ->
	% Double-list: list with a list in it.
	SpawnedPid= spawn_link( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
	
	% Blocks until the spawned process answers:	
	% (no risk of synchronous spawns mismatch, as each synchronous calls is
	% waited for)
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node, 
% using specified parameters to construct it.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: remote_synchronous_timed_new will return 
% only when the created process reports it is up and running, or when
% a time-out occurs.
remote_synchronous_timed_new( Node, ?wooper_construct_parameters ) ->
	SpawnedPid = spawn( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



% Spawns a new instance for this class on specified interconnected node, 
% and links it to the current process, using specified parameters
% to construct it.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: remote_synchronous_timed_new_link will return
% only when the created process reports it is up and running, or when 
% a time-out occurs.
remote_synchronous_timed_new_link( Node, ?wooper_construct_parameters ) ->
	SpawnedPid = spawn_link( Node, ?MODULE, 
		wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new







-else. % -ifdef(wooper_construct_parameters).


% Second case: wooper_construct_parameters is *not* defined:


% Spawns a new instance for this class.
% Returns the PID of the newly created instance.
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
new() ->
	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]), 
	% Double-list: list with a list in it.
	spawn( ?MODULE, wooper_construct_and_run, [[]] ).
	


% Spawns a new instance for this class and links it to the current process.
% Returns the PID of the newly created and linked instance.
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
new_link() ->
	% Double-list: list with a list in it.
	spawn_link( ?MODULE, wooper_construct_and_run, [[]] ).



% Spawns a new instance for this class.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
synchronous_new() ->
	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]), 
	% Double-list: list with a list in it.
	SpawnedPid = spawn( ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
		
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.
	

% Spawns a new instance for this class and links it to the current process.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
synchronous_new_link() ->
	% Double-list: list with a list in it.
	SpawnedPid= spawn_link( ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
	
	% Blocks until the spawned process answers:	
	% (no risk of synchronous spawns mismatch, as each synchronous calls is
	% waited for)
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.



% Uncomment to activate synchronous new with time-out:
-define(use_synchronous_timed_new,).


-ifdef(use_synchronous_timed_new).


% The duration in milliseconds before a time-out is triggered after a created
% instance does not seem to answer properly and after a reasonable duration:
-define(synchronous_time_out,5000).
			
			
% Spawns a new instance for this class.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new() ->
	SpawnedPid = spawn( ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



% Spawns a new instance for this class, and links it to the current process.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new_link() ->
	SpawnedPid = spawn_link( ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



-endif. % use_synchronous_timed_new



% If use_remote_new is defined, following construction variations will be
% automatically defined (class implementor will have to declare them):
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then having remote_new/N+1.




% Uncomment to activate remote new constructions:
-define(use_remote_new,).


-ifdef(use_remote_new).


% Spawns a new instance for this class on specified interconnected node.
% If Node does not exist, a useless pid is returned. 
% Returns the PID of the newly created instance.
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new( Node ) ->
	spawn( Node, ?MODULE, wooper_construct_and_run, [ [] ] ).


% Spawns a new instance for this class on specified interconnected node, 
% and links it to the current process.
% If Node does not exist, a useless pid is returned. 
% Returns the PID of the newly created instance.
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new_link( Node ) ->
	spawn_link( Node, ?MODULE, wooper_construct_and_run,
		[ [] ] ).



% Spawns a new instance for this class on specified interconnected node.
% Returns the PID of the newly created instance.
% Creation is synchronous: remote_synchronous_new will return only 
% when the created process reports it is up and running.
remote_synchronous_new( Node ) ->
	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]), 
	% Double-list: list with a list in it.
	SpawnedPid = spawn( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
		
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.
	

% Spawns a new instance for this class on specified interconnected node 
% and links it to the current process.
% Returns the PID of the newly created instance.
% Creation is synchronous: remote_synchronous_new_link will return only 
% when the created process reports it is up and running.
remote_synchronous_new_link( Node ) ->
	% Double-list: list with a list in it.
	SpawnedPid= spawn_link( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
	
	% Blocks until the spawned process answers:	
	% (no risk of synchronous spawns mismatch, as each synchronous calls is
	% waited for)
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: remote_synchronous_timed_new will return 
% only when the created process reports it is up and running, or when
% a time-out occurs.
remote_synchronous_timed_new( Node ) ->
	SpawnedPid = spawn( Node, ?MODULE, wooper_construct_and_run_synchronous,
		[ [], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



% Spawns a new instance for this class on specified interconnected node, 
% and links it to the current process.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: remote_synchronous_timed_new_link will return
% only when the created process reports it is up and running, or when 
% a time-out occurs.
remote_synchronous_timed_new_link( Node ) ->
	SpawnedPid = spawn_link( Node, ?MODULE,
		 wooper_construct_and_run_synchronous, [ [], self() ] ),
		
	% Blocks until the spawned process answers or a time-out occurs:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	after ?synchronous_time_out -> 
	
		time_out
		
	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new


-endif. % -ifdef(wooper_construct_parameters).




-ifdef(wooper_debug).


% Extensive testings in this mode.

% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run(ParameterList) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	case apply(?MODULE,construct,[BlankTable|ParameterList]) of 
	
		ConstructState when is_record(ConstructState,state_holder) ->
			wooper_main_loop( ConstructState );
			
		Other ->
			error_logger:error_msg(	"WOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned ~w instead. "
				"Construction parameters were ~w.~n",
				[ self(), ?MODULE, Other, ParameterList] ),
			% Wait a bit as error_msg seems asynchronous:
			timer:sleep(1000),
			throw({invalid_constructor,?MODULE}) 	

	end.


% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run_synchronous(ParameterList,SpawnerPid) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	case apply(?MODULE,construct,[BlankTable|ParameterList]) of 
	
		ConstructState when is_record(ConstructState,state_holder) ->
			SpawnerPid ! { spawn_successful, self() },
			wooper_main_loop( ConstructState );
			
		Other ->
			error_logger:error_msg(	"WOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned ~w instead. "
				"Construction parameters were ~w.~n",
				[ self(), ?MODULE, Other, ParameterList] ),
			% Wait a bit as error_msg seems asynchronous:
			timer:sleep(1000),
			throw({invalid_constructor,?MODULE}) 	

	end.



-else.


% Less testings in this mode.

% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run(ParameterList) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	wooper_main_loop( apply(?MODULE,construct,[BlankTable|ParameterList]) ).


% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run_synchronous(ParameterList,SpawnerPid) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	ConstructState = apply(?MODULE,construct,[BlankTable|ParameterList]),
	SpawnerPid ! { spawn_successful, self() },
	wooper_main_loop( ConstructState ).


-endif.




% State management section.


% Note that these macros should be one-liners, otherwise their value would be
% the first evaluated in the macro - not the last...


% Sets specified attribute of the instance to the specified value, based from
% specified state.
% Returns an updated state.
% Always succeeds.
% See also: the setAttributes macro to set more than one attribute at a time.
-define(setAttribute(State,AttributeName,AttributeValue),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntry(
			AttributeName,
			AttributeValue,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Sets a list of attribute/value pairs in specified state.
% The expected parameter is a list of pairs (2-element tuples), each pair 
% containing in first position the attribute name and in second one the 
% attribute value.
% Returns an updated state.
% Always succeeds.
% See also: the setAttribute macro.
-define(setAttributes(State,ListOfAttributePairs),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntries(
			ListOfAttributePairs,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).
	

% Tells whether specified attribute exists, returns true or false.
% Note: usually the best practise is to set all possible attributes from the
% constructor, either to an appropriate value or to 'undefined', instead of
% having instances with or without a given attribute.
-define(hasAttribute(State,AttributeName),
	hashtable:hasEntry( AttributeName, State#state_holder.attribute_table ) ).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise triggers a case clause crash.
% Note: almost never used, as either the attribute can be obtained with 
% getAttr/1 (as externally defined) or it is already bound to a variable.
% See also: getAttr/1.
-define(getAttribute(State,AttributeName),
	hashtable:getEntry( AttributeName, State#state_holder.attribute_table ) ).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise triggers a case clause crash.
% Beware to the implicit use of the 'State' variable: in some cases other
% states should be used. 
% See the getAttribute/2 macro.
-define(getAttr(AttributeName),?getAttribute(State,AttributeName)).


% Returns an updated state not having anymore specified attribute.
% No error is triggered if the specified attribute was not existing.
-define(removeAttribute(State,AttributeName),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:removeEntry( AttributeName,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Adds specified value to specified attribute, supposed to be a number.
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
-define(addToAttribute(State,AttributeName,Value),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addToEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Substracts specified value from specified attribute, supposed to be a number.
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no substraction can be performed on the attribute value.
-define(substractFromAttribute(State,AttributeName,Value),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:substractFromEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).

 
% Returns an updated state in which specified boolean attribute is toggled:
% if true will be false, if false will be true.
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
-define(toggleAttribute(State,BooleanAttributeName),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:toggleEntry(
			BooleanAttributeName, 
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).	


% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
-define(appendToAttribute(State,AttributeName,Element),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:appendToEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
% A case clause is triggered if the attribute did not exist.
% If the element is not in the specified list, the list will not be modified.
-define(deleteFromAttribute(State,AttributeName,Element),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:deleteFromEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Assumes the specified attribute is a hashtable and adds the specified
% key/value pair to it.
% Several lines compacted into a rather impressive one-liner.
-define(addKeyValueToAttribute(State,AttributeName,Key,Value),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntry( AttributeName, 
			hashtable:addEntry(Key,Value,
				hashtable:getEntry( AttributeName,
					State#state_holder.attribute_table )),
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).



% Removes the head from specified attribute, supposed to be a list, and 
% returns a tuple {NewState,PoppedHead}.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{PoppedState,Head} = ?popFromAttribute(State,my_list)'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
% Note: This cannot be a one-line macro, it has to be a function.
%
-define(popFromAttribute(State,AttributeName),
	wooper_pop_from_attribute(State,AttributeName)
).


% Helper function for the popFromAttribute macro.
wooper_pop_from_attribute(State,AttributeName) ->
	{Head,PoppedAttributeTable} = hashtable:popFromEntry(AttributeName,
		State#state_holder.attribute_table),
	
	{ #state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = PoppedAttributeTable,
		request_sender  = State#state_holder.request_sender	},
		Head
	}.	



% Checks that the value of specified attribute is 'undefined'.
% Triggers an exception otherwise.
% Note: operates on a state called 'State'.
% The check could be disabled in debug mode.
% This results in function call, as a pure macro, if used more than
% once in a function, would trigger warnings about unsage variables.
-define(checkUndefined(Attribute),
	wooper_check_undefined(State,Attribute)
). 


% Helper function for the checkUndefined macro.
wooper_check_undefined(State,Attribute) -> 
	try 
		undefined = ?getAttr(Attribute)
	catch	

		exit:{{badmatch,UnexpectedValue},Stack} ->
			% Attribute value was not equal to 'undefined':
			throw( {attribute_was_not_undefined,{Attribute,UnexpectedValue},
				Stack} );
				
		exit:Error ->
			% Other error (ex: unknown attribute):
			throw( {attribute_error,Attribute,Error} );
		
		OtherError ->
			throw( {unexptected_attribute_error,Attribute,OtherError} )
		
	end.		



% Returns the sender of the request.
-define(getSender(),State#state_holder.request_sender).




% Returns the Wooper Class Manager.
% If if it is already running, find it and returns its atom, otherwise launch
% it and returns that same atom as well.
wooper_get_class_manager() ->
	case lists:member( ?WooperClassManagerName, registered() ) of
		true ->
			?WooperClassManagerName;
		
		_ ->
			spawn(?WooperClassManagerName,start,[self()]),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			receive
			
				class_manager_registered ->
					?WooperClassManagerName
			
			% 10-second time-out:
			after 10000	->
				error_logger:error_msg( "wooper_get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand.~n"
				),
				undefined
					
			end
			
	end.		


% Most functions below could be encapsulated in a WOOPER-dedicated module
% (in a .erl instead of a .hrl), but in this case calls may be less efficient.



% Methods for getting informations about an instance.


% Returns a textual representation of the attributes of the specified state.
wooper_state_toString(State) ->
	Attributes = hashtable:enumerate(State#state_holder.attribute_table),
	lists:foldl(
		fun({AttName,AttrValue},Acc) ->
			Acc ++ io_lib:format( 
				"     * ~s = ~s~n", 
				[
					basic_utils:term_toString(AttName),
					basic_utils:term_toString(AttrValue)
				])
			
		end, 
		io_lib:format( "State of ~w:~nInstance of ~s with ~B attribute(s):~n",
			[self(),get_class_name(),length(Attributes)]),
		Attributes).	


% Returns a textual representation of the virtual table corresponding to
% specified state.
wooper_virtual_table_toString(State) ->
	lists:foldl(
		fun( {{Name,Arity},Module}, String ) ->
			String ++ io_lib:format( "     * ~s/~B -> ~s~n",
				[Name,Arity,Module] )
		end,
		io_lib:format( "Virtual table of ~w:~n"
			"(method name/arity -> module defining that method)~n", [self()] ),
		hashtable:enumerate( State#state_holder.virtual_table )). 


% Returns a textual representation of this instance, including its state and
% virtual table.
wooper_instance_toString(State) ->
	io_lib:format( "Inpection of instance ~w:~n~n  + ~s~n  + ~s",
		[ self(), wooper_state_toString(State),
			wooper_virtual_table_toString(State) ]).



% Displays the inner state of this instance.
% This is not a method.
wooper_display_state(State) ->
	error_logger:info_msg( "~s~n", [wooper_state_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_virtual_table(State) ->
	error_logger:info_msg( "~s~n", [wooper_virtual_table_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_instance(State) ->
	error_logger:info_msg( "~s~n", [wooper_instance_toString(State)] ).


-ifdef(wooper_debug).


% Returns a textual description of the inner state of this instance.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_state_description(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).


% Returns a textual description of the virtual table used by this instance.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_virtual_table_description(State) ->
	?wooper_return_state_result( State,	wooper_virtual_table_toString(State) ).
		
		
% Returns a full textual description of this instance, including its state and
% virtual table.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_instance_description(State) ->
	?wooper_return_state_result( State, wooper_instance_toString(State) ).
	
-endif.
	
	
	
% WOOPER default EXIT handler.
% Can be overriden by defining or inheriting the onWooperExitReceived/3 method.	
wooper_default_exit_handler(State,Pid,ExitType) ->
	io:format( "WOOPER default EXIT handler ignored signal '~w' from ~w.~n",
		[ExitType,Pid] ),
	?wooper_return_state_only(State).	



% Waits for incoming requests and serves them.
wooper_main_loop(State) ->
	
	?wooper_log( "wooper_main_loop start.~n" ),

	% Comment-out to avoid the state display prior to each method call:
	%wooper_display_loop_state(State),
		
	receive
			
		% Requests with response:
		
		% Instance PID could be sent back as well to discriminate 
		% received answers on the caller side.
		{ MethodAtom, ArgumentList, CallerPid } 
				when is_pid(CallerPid) and is_list(ArgumentList) ->
			?wooper_log_format( "Main loop (case A) for ~w: "
				"request ~s with argument list ~w for ~w.~n", 
				[self(), MethodAtom, ArgumentList, CallerPid] ),
			SenderAwareState = State#state_holder{request_sender=CallerPid},
			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, ArgumentList ), 
			%CallerPid ! { self(), Result }
			CallerPid ! Result,
			
			% Force a crash if instance-side error detected: 
			case element(1,Result) of
			
				wooper_method_failed ->
					throw(Result) ;
				
				wooper_method_faulty_return ->
					throw(Result) ;
				
				_ ->
					ok	
			
			end, 
			SenderAgnosticState =
				NewState#state_holder{request_sender=undefined},
			?wooper_log( "Main loop (case A) ended.~n" ),	
			wooper_main_loop(SenderAgnosticState);

		
		% Auto-wrapping single arguments implies putting lists between
		% double-brackets		
		{ MethodAtom, Argument, CallerPid } when is_pid(CallerPid) ->
			?wooper_log_format( "Main loop (case B) for ~w: "
				"request ~s with argument ~w for ~w.~n", 
				[self(), MethodAtom, Argument, CallerPid] ),
			SenderAwareState = State#state_holder{request_sender=CallerPid},
			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, [ Argument ] ), 
			%CallerPid ! { self(), Result }
			CallerPid ! Result,
			
			% Force a crash if instance-side error detected: 
			case element(1,Result) of
			
				wooper_method_failed ->
					throw(Result) ;
				
				wooper_method_faulty_return ->
					throw(Result) ;
				
				_ ->
					ok	
			
			end, 
			SenderAgnosticState =
				NewState#state_holder{request_sender=undefined},
			?wooper_log( "Main loop (case B) ended.~n" ),	
			wooper_main_loop(SenderAgnosticState);


		% Oneway calls (no caller PID sent, no answer sent back):
		
		% (either this method does not return anything, or the sender is not
		% interested in the result)
		{ MethodAtom, ArgumentList } when is_list(ArgumentList) ->
			?wooper_log_format( "Main loop (case C) for ~w: "
				"oneway ~s with argument list ~w.~n",
				[self(), MethodAtom, ArgumentList] ),
			% Any result would be ignored, only the updated state is kept:
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, ArgumentList ),
			?wooper_log( "Main loop (case C) ended.~n" ),	
			wooper_main_loop(NewState);
		
		{ synchronous_delete, CallerPid } ->
			?wooper_log("Main loop: oneway synchronous delete.~n"),
			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			wooper_destruct( State ),
			CallerPid ! {deleted,self()},
			deleted;
			% (do nothing, loop ends here).		
			
		
		% ping is always available and cannot be overriden:
		{ ping, CallerPid } ->
			?wooper_log_format( "Main loop (case D) for ~w: oneway ping.~n",
				[self()]),
			CallerPid ! {pong,self()},
			?wooper_log( "Main loop (case D) ended.~n" ),	
			wooper_main_loop(State);
		
		
		% Oneway with parameters:		
		{ MethodAtom, Argument } ->
			?wooper_log_format( "Main loop (case E) for ~w: "
				"oneway ~s with argument ~w.~n",[self(),MethodAtom, Argument] ),
			% Any result would be ignored, only the updated state is kept:
			{ NewState, _ } = wooper_execute_method( MethodAtom, State, 
				[ Argument ] ), 
			?wooper_log( "Main loop (case E) ended.~n" ),	
			wooper_main_loop(NewState);
			
			
		delete ->
			?wooper_log("Main loop: oneway delete.~n"),
			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			wooper_destruct( State ),
			deleted;
			% (do nothing, loop ends here).		
			
			
		MethodAtom when is_atom(MethodAtom) ->
			?wooper_log_format( 
				"Main loop (case F) for ~w: oneway from atom ~s.~n",
				[self(),MethodAtom]),
			{ NewState, _ } = wooper_execute_method( MethodAtom, State, [] ),
			?wooper_log( "Main loop (case F) ended.~n" ),	
			wooper_main_loop(NewState);
		
		
		{'EXIT',Pid,ExitType} when is_pid(Pid) ->
			?wooper_log_format( "Main loop (case G) for ~w: exit with ~w.~n",
				[self(),{Pid,ExitType}] ),			
			case hashtable:lookupEntry( {onWooperExitReceived,3},
					State#state_holder.virtual_table) of 
				
				undefined ->
					% EXIT handler not overriden, using default one:
					?wooper_log( "Main loop (case G) ended.~n" ),	
					wooper_main_loop( wooper_default_exit_handler( State,
						Pid,ExitType ) );
				
				{ value, _ } -> 
					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule,onWooperExitReceived,...)':
					{ NewState, _ } = wooper_execute_method(
						onWooperExitReceived, State, [Pid,ExitType] ),
					?wooper_log( "Main loop (case G) ended.~n" ),	
					wooper_main_loop(NewState)
								
			end;
			
		
		Other ->
			?wooper_log_format( "Main loop (case H) for ~w: unmatched ~s.~n",
				[self(),Other] ),			
			error_logger:warning_msg( "WOOPER ignored following message: ~w.~n",
				[Other]),
			?wooper_log( "Main loop (case H) ended.~n" ),	
			wooper_main_loop(State)
			

	end.
	% Commented out to preserve (presumably) tail-recursion:
	% io:format( "wooper_main_loop exited.~n" ).

	
	
% Returns the virtual table corresponding to this class.
wooper_retrieve_virtual_table() ->
	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).
	wooper_get_class_manager() ! { get_table, ?MODULE, self() },
	receive
	
		{ virtual_table, ?MODULE, Table } ->
			%hashtable:display(Table),
			Table
	
	end.

	
-ifdef(wooper_debug).


% Calls recursively the destructors through the inheritance tree.
% Each wooper_destruct function is purely local to the current module. 
% Initial specified is always valid (comes from the main loop, but states
% returned by user-defined destructors must be checked in debug mode.
wooper_destruct( State ) ->			
	% If a class-specific delete is defined, execute it, otherwise
	% do nothing. Then recurse with higher-level destructors
	% (maybe just storing delete/1 in the method table would be
	% more efficient, see wooper_class_manager:get_virtual_table_for):
	DeletedState = case lists:member( {delete,1}, module_info(exports) ) of
	
		true ->
			% All destructors, included user-defined ones, must return 
			% a (possibly updated) state: 
			case ?MODULE:delete(State) of 
			
				ReturnedState when is_record(ReturnedState,state_holder) ->
					ReturnedState;
				
				Other ->
					error_logger:error_msg(	
						"WOOPER error for PID ~w of class ~s: "
						"user-defined destructor did not return a state, "
						"but returned ~w instead.~n",
						[ self(), ?MODULE, Other] ),
					% Wait a bit as error_msg seems asynchronous:
					timer:sleep(1000),
					throw({invalid_destructor,?MODULE}) 	
						
			end;	 
		
		false ->
			% Destructor not overriden, using default one:
			%io:format( "Deleting ~w (default do-nothing destructor "
			%	"for class ~w).~n", [ self(), ?MODULE ] )
			% State unchanged:
			State
				
	end,
	% Then automatically call the direct mother destructors.			
	% Using foldr, not foldl: the destructors of mother classes
	% are called in the reverse order compared to the order
	% that was used for construction, for the sake of symmetry.
	% Final state is dropped.
	lists:foldr( 
		fun(Class,NewState) -> apply(Class,wooper_destruct,[NewState]) end,
		DeletedState, get_superclasses() ).
	
-else.
	

% Calls recursively the destructors through the inheritance tree.
% Each wooper_destruct function is purely local to the current module. 
wooper_destruct( State ) ->			
	% If a class-specific delete is defined, execute it, otherwise
	% do nothing. Then recurse with higher-level destructors
	% (maybe just storing delete/1 in the method table would be
	% more efficient, see wooper_class_manager:get_virtual_table_for):
	DeletedState = case lists:member( {delete,1}, module_info(exports) ) of
	
		true ->
			% All destructors, included user-defined ones, must return 
			% a (possibly updated) state: 
			?MODULE:delete(State);	 
		
		false ->
			% Destructor not overriden, using default one:
			%io:format( "Deleting ~w (default do-nothing destructor "
			%	"for class ~w).~n", [ self(), ?MODULE ] )
			% State unchanged:
			State
				
	end,
	% Then automatically call the direct mother destructors.			
	% Using foldr, not foldl: the destructors of mother classes
	% are called in the reverse order compared to the order
	% that was used for construction, for the sake of symmetry.
	% Final state is dropped.
	lists:foldr( 
		fun(Class,NewState) -> apply(Class,wooper_destruct,[NewState]) end,
		DeletedState, get_superclasses() ).
	
-endif.

	
	
% Looks-up specified method (Method/Arity, ex: toString/0) to be found 
% in heritance tree and returns either { methodFound, Module } with 
% Module corresponding to the class that implements that method, or an error, 
% like: {hashtable_key_not_found,{aMethodName,AnUnexpectedArity}}.
% Note: uses the pre-built virtual table for this class.
wooper_lookupMethod(State,MethodAtom,Arity) ->
	hashtable:lookupEntry( {MethodAtom,Arity},
		State#state_holder.virtual_table).



% Following code is duplicated because no '-ifdef' clause can be defined in
% case clauses:


-ifdef(wooper_debug).	
 
 
% Executes the specified method, designated by its atom, with specified 
% instance state and list of parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom
% 'wooper_method_not_found' is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'wooper_method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {wooper_result,Result} is returned, with 
% Result being the actual result of the method call, with an updated state.
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returned, which allows a caller that sent his
% PID to be warned it is useless, as no answer should be expected.
% The error logs have been added, as debugging faulty oneways is more difficult:
% they cannot return any error to the caller, they can just crash and notify
% any linked or monitoring process.
% Not: atom and state checking in guards should be superfluous.
wooper_execute_method(MethodAtom,State,Parameters) when is_atom(MethodAtom) 
		andalso is_record(State,state_holder) andalso is_list(Parameters) ->	

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]), 	
	
	% +1: take into account the State additional parameter:
	case wooper_lookupMethod( State, MethodAtom, length(Parameters)+1 ) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:

			%io:format("wooper_execute_method: executing ~s:~s(~w).~n",
			%	[ ?MODULE, MethodAtom, Parameters ]), 	
			
			try apply(LocatedModule,MethodAtom,[State|Parameters]) of
			
				% Void method (no result returned, only a state):
				% ?wooper_return_state_only:
				NewState when is_record(NewState,state_holder) ->  
					{NewState,wooper_method_returns_void};
				
				% Method returning a result (and a state of course):
				% ?wooper_return_state_result:
				{NewState,Result} when is_record(NewState,state_holder) ->		
					{NewState,{wooper_result,Result}};
			
				% Not a wooper result neither an exception: faulty return.
				Other ->
					
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B made a faulty return "
								"~w, parameters were ~w.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Other, Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
								
							% Terminates the process:	
							throw( {wooper_method_faulty_return, self(),
								LocatedModule, MethodAtom, 
								length(Parameters)+1, Parameters, Other} );    
						   
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B made a faulty return "
								"~w, parameters were ~w.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Other, Parameters ] ),
								
							{State, 
							   	{wooper_method_faulty_return, self(),
									LocatedModule, MethodAtom,
									length(Parameters)+1, Parameters, Other } }
									 
					end
					
			catch
			
				% All next cases are error cases.
				% One option is to return an appropriate error term, but it 
				% is useful only for requests, as oneways send back no result. 
				% Another option is to let the faulty process crash: oneways
				% would not send more answers, but linked and monitoring 
				% processes could nevertheless be notified.
				
				% Finally, failed requests result in a log, an error answer
				% being returned, then a crash if the error is due to internal
				% reasons, whereas failed oneways result in a log then a crash,
				% similarly if the error is due to internal reasons.

				% User-defined exceptions are not caught, they will be rethrown
				% and make the process fail.
				
				% This the counterpart of {'EXIT',ErrorTerm}.
				% What would be the counterpart of {'EXIT',Pid,ErrorTerm}?
				% Reason can be: throw, error or exit
				Reason:ErrorTerm ->

					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed (cause: ~w) "
								"with error term '~w' for parameters ~w, "
								"stack trace was (latest calls first):~n~p.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters,erlang:get_stacktrace()] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
																
							% Terminates the process:	
							throw( {wooper_oneway_failed, self(), LocatedModule,
								MethodAtom, length(Parameters)+1, Parameters,
								ErrorTerm} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed (cause: ~w) "
								"with error term '~w' for parameters ~w, "
								"stack trace was (latest calls first):~n~p.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters,erlang:get_stacktrace()] ),
								
							{State,
								{wooper_method_failed, self(), LocatedModule,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, ErrorTerm} }
														
					end
										
						
			end;

				
		% Method not found:	
		{hashtable_key_not_found,{_MethodName,_Arity}} ->
		
			case State#state_holder.request_sender of
					
				undefined ->
		
					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple 
					% elements, as if in a single string ("M/A"), the result
					% is displayed as a list:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"oneway method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep(1000),
					
					% Terminates the process:	
					throw( {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters} );   
					 
				_ ->
						
					% This is a request, send error term and rely on
					% the calling function (wooper_main_loop) to crash:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"request method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					{State, {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters } }
				
					
			% No other term can be returned.
		
			end;
			
			
		Other ->
			error_logger:warning_msg( "WOOPER ignored following message: ~w.~n",
				[Other]),
			wooper_main_loop(State)
			
	end.



% Parameter-less request.
executeRequest(State,RequestAtom) when is_record(State,state_holder) 
		andalso is_atom(RequestAtom) -> 

	%io:format("executeRequest/2: executing ~s() from ~s.~n", 
	%	[ RequestAtom , ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ), 

	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result};
	
	
executeRequest(StateError,RequestAtom) when is_atom(RequestAtom) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing request ~w: "
		"first parameter should be a state, not ~w.~n",
		[ self(), ?MODULE, RequestAtom, StateError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_request_call,RequestAtom});
	
	
executeRequest(_State,RequestAtomError) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing request: ~w is not an atom.~n",
		[ self(), ?MODULE, RequestAtomError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_request_call,RequestAtomError}).
	
	
	


% Allows to call synchronously from the code of a given class its actual 
% overriden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeRequest(State,RequestAtom,ArgumentList) when
		is_record(State,state_holder) andalso is_atom(RequestAtom) 
		andalso is_list(ArgumentList) -> 

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, ?MODULE ]), 	

			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ), 
		
	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result};
	
	
% Here third parameter is not a list:
executeRequest(State,RequestAtom,StandaloneArgument) when
		is_record(State,state_holder) andalso is_atom(RequestAtom)-> 

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ RequestAtom, StandaloneArgument, ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [StandaloneArgument] ), 

	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result};


% Catches all errors:
executeRequest(StateError,RequestAtom,_LastArg) when is_atom(RequestAtom) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing request ~w: "
		"first parameter should be a state, not ~w.~n",
		[ self(), ?MODULE, RequestAtom, StateError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_request_call,RequestAtom});
	
	
executeRequest(_State,RequestAtomError,_LastArg) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing request: ~w is not an atom.~n",
		[ self(), ?MODULE, RequestAtomError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_request_call,RequestAtomError}).




% Parameter-less oneway.
executeOneway(State,OnewayAtom) when is_record(State,state_holder)
		andalso is_atom(OnewayAtom) -> 

	%io:format("executeOneway/2: executing ~s() from ~s.~n", 
	%	[ OnewayAtom, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [] ), 

	% Returns:			
	NewState;

executeOneway(StateError,OnewayAtom) when is_atom(OnewayAtom) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway ~w: "
		"first parameter should be a state, not ~w.~n",
		[ self(), ?MODULE, OnewayAtom, StateError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_oneway_call,OnewayAtom});
	
	
executeOneway(_State,OnewayError) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway: ~w is not an atom.~n",
		[ self(), ?MODULE, OnewayError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_oneway_call,OnewayError}).


		
% Allows to call synchronously from the code of a given class its actual 
% overriden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeOneway should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeOneway(State,OnewayAtom,ArgumentList) when
		is_record(State,state_holder) andalso is_atom(OnewayAtom) 
		andalso is_list(ArgumentList) -> 

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, ?MODULE ]), 	
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, ArgumentList ), 
	
	% Returns:	
	NewState;

	
% Here third parameter is not a list:
executeOneway(State,OnewayAtom,StandaloneArgument) when
		is_record(State,state_holder) andalso is_atom(OnewayAtom) -> 

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ OnewayAtom, StandaloneArgument, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [StandaloneArgument] ), 

	% Returns:			
	NewState;


% Catches all errors:
executeOneway(StateError,OnewayAtom,_LastArg) when is_atom(OnewayAtom) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway ~w: "
		"first parameter should be a state, not ~w.~n",
		[ self(), ?MODULE, OnewayAtom, StateError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_oneway_call,OnewayAtom});
	
	
executeOneway(_State,OnewayAtomError,_LastArg) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway: ~w is not an atom.~n",
		[ self(), ?MODULE, OnewayAtomError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_oneway_call,OnewayAtomError}).



-else.





% Not in debug mode here:


% Executes the specified method, designated by its atom, with specified 
% instance state and parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom
% 'wooper_method_not_found' is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'wooper_method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {wooper_result,Result} is returned (with 
% Result being the actual result of the method call) with an updated state.
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returns, which allows a caller that sent his
% PID to be warned it is useless, as no answer should be expected.
wooper_execute_method(MethodAtom,State,Parameters) ->

	% +1: take into account the State additional parameter:
	case wooper_lookupMethod( State, MethodAtom, length(Parameters)+1 ) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:
			try apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Method returning a result (and a state of course):
				% ?wooper_return_state_result:
				{NewState,Result} ->  
					{NewState,{wooper_result,Result}};
														
				% Void method (no result returned, only a state):
				% (catch-all, no faulty return can be detected here, when not
				% in debug mode)
				% ?wooper_return_state_only:
				NewState ->  
					{NewState,wooper_method_returns_void}
				
				% No catch-clause available here (no faulty return possible).
						
			catch

				% Matched expressions have to be reordered depending on the
				% debug mode: 
				Reason:ErrorTerm ->
				
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed (cause: ~w) "
								"with error term '~w' for parameters ~w, "
								"stack trace was (latest calls first):~n~p.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters,erlang:get_stacktrace()] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
								
							% Terminates the process:	
							throw( {wooper_oneway_failed, self(), LocatedModule,
								MethodAtom, length(Parameters)+1, Parameters,
								ErrorTerm} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed (cause: ~w) "
								"with error term '~w' for parameters ~w, "
								"stack trace was (latest calls first):~n~p.~n",
								[ self(), LocatedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters,erlang:get_stacktrace()] ),
								
							{State,
								{wooper_method_failed, self(), LocatedModule,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, ErrorTerm} } 					
					end

			end;
			
			
			
		undefined ->
		
			case State#state_holder.request_sender of
					
				undefined ->
		
					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple elements,
					% as if in a single string ("M/A"), the result is displayed
					% as a list:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"oneway method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), LocatedModule, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					% Wait a bit as error_msg seems asynchronous:
					timer:sleep(1000),
					
					% Terminates the process:	
					throw( {wooper_method_not_found, self(), LocatedModule,
						MethodAtom, length(Parameters)+1, Parameters} );   
					 
				_ ->
						
					% This is a request, send error term and rely on
					% the calling function (wooper_main_loop) to crash:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"request method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), LocatedModule, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					{State, {wooper_method_not_found, self(), LocatedModule,
						MethodAtom, length(Parameters)+1, Parameters } }
				
			end	
					
		% No other term can be returned.
		
	end.	



% Parameter-less request.
executeRequest(State,RequestAtom) -> 

	%io:format("executeRequest/2: executing ~s() from ~s.~n", 
	%	[ RequestAtom , ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ), 

	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result}.



% Allows to call synchronously from the code of a given class its actual 
% overriden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeRequest should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeRequest(State,RequestAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, ?MODULE ]), 	
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ), 

	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result};
	
	
	
% Here third parameter is not a list:
executeRequest(State,RequestAtom,StandaloneArgument) -> 

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ RequestAtom, StandaloneArgument, ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [StandaloneArgument] ), 

	% Returns:			
	{NewState#state_holder{request_sender=undefined}, Result}.




% Parameter-less oneway.
executeOneway(State,OnewayAtom) -> 

	%io:format("executeOneway/2: executing ~s() from ~s.~n", 
	%	[ OnewayAtom, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, [] ), 

	% Returns:			
	NewState.

	
% Allows to call synchronously from the code of a given class its actual 
% overriden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeOneway should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeOneway(State,OnewayAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, ?MODULE ]), 	
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, ArgumentList ), 
	
	% Returns:	
	NewState;

	
% Here third parameter is not a list:
executeOneway(State,OnewayAtom,StandaloneArgument) -> 

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ OnewayAtom, StandaloneArgument, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, [StandaloneArgument] ), 

	% Returns:			
	NewState.

				
-endif.



% Section for functions whose definitions do not change depending on the debug
% mode.


% Deletes the process(es) potentially stored in specified WOOPER attribute list.
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
% Ex: in a destructor:
% delete_any_instance_referenced_in(State, [first_pid,second_pid] ) or
% delete_any_instance_referenced_in(State, my_pid ).
delete_any_instance_referenced_in( State, [] ) ->
	State;
	
delete_any_instance_referenced_in( State, [PidAttribute|T] ) ->
	NewState = case ?getAttr(PidAttribute) of
	
		undefined ->
			State;
		
		Pid when is_pid(Pid) ->
			Pid ! delete,
			?setAttribute( State, PidAttribute, undefined )
				
	end,
	delete_any_instance_referenced_in( NewState, T );
	
delete_any_instance_referenced_in( State, PidAttribute ) ->
	case ?getAttr(PidAttribute) of
	
		undefined ->
			State;
		
		Pid when is_pid(Pid) ->
			Pid ! delete,
			?setAttribute( State, PidAttribute, undefined )
				
	end.

