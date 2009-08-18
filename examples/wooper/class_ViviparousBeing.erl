% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_ViviparousBeing).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/0, new_link/0, 
	synchronous_new/0, synchronous_new_link/0,
	synchronous_timed_new/0, synchronous_timed_new_link/0,
	remote_new/1, remote_new_link/1, remote_synchronous_new/1,
	remote_synchronous_new_link/1, remote_synchronous_timed_new/1,
	remote_synchronous_timed_new_link/1, construct/1 ).


% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export, getMeanChildrenCount/1, getBirthGivenCount/1,
	giveBirth/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Viviparous being (parameter-less constructor).
construct(State) ->
	?setAttribute(State,birth_given_count,0).
	
	
	
	
% Method implementations.


% Let's say an average means something here:
% (this is a static method, as it does not depend on a state)
getMeanChildrenCount(State) ->
	?wooper_return_state_result( State, 4 ).
	
	
% Returns the number of times this viviparous being gave birth:	
getBirthGivenCount(State) ->
	?wooper_return_state_result( State, 
		?getAttribute(State,birth_given_count) ).
		
		
% Increase the number of times this viviparous being gave birth:	
giveBirth(State,NumberOfNewChildren) ->
	?wooper_return_state_only( ?setAttribute(State,birth_given_count, 
		?getAttribute(State,birth_given_count) + NumberOfNewChildren ) ).
		
	
