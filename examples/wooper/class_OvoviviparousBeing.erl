% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_OvoviviparousBeing).


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
-define(wooper_method_export, getMeanEggsCount/1, getEggsLaidCount/1,
	layEggs/2 ).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Ovoviviparous being (parameter-less constructor).
construct(State) ->
	?setAttribute(State,eggs_count,0).


	
% Method implementations.


% Let's say an average means something here:
% (this is a static method, as it does not depend on a state)
getMeanEggsCount(State) ->
	?wooper_return_state_result( State, 1000 ).
	

% Returns the number of eggs this ovoviviparous laid:	
getEggsLaidCount(State) ->
	?wooper_return_state_result( State, 
		?getAttribute(State,eggs_count) ).
		
		
% Increase the number of eggs this ovoviviparous laid:	
layEggs(State,NumberOfNewEggs) ->
	?wooper_return_state_only( ?setAttribute(State,eggs_count, 
		?getAttribute(State,eggs_count) + NumberOfNewEggs ) ).
		
