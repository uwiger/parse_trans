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
	   module,
	   state,
	   request_sender
	}).



% A list could be managed that would allow to discriminate the methods from
% the other exported functions. As macros cannot be substitued in strings
% it would probably force the developer to list them twice.

-ifdef(wooper_log_wanted).
 -define(wooper_log(Msg),io:format(Msg)).
 -define(wooper_log_format(Msg,Format),io:format(Msg,Format)).
-else.  
 -define(wooper_log(Msg),no_wooper_log).
 -define(wooper_log_format(Msg,Format),no_wooper_log).
-endif. 

-ifndef(wooper_return_state_result).
-define(wooper_return_state_result(State,Result),{State,Result}).
-endif.
-ifndef(wooper_return_state_only).
-define(wooper_return_state_only(State), State).
-endif.
