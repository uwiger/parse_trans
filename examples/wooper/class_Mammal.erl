% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Mammal).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Creature]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the mother class (creature) plus fur color:
-define(wooper_construct_parameters, Age, Gender, FurColor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3, 
	synchronous_new/3, synchronous_new_link/3,
	synchronous_timed_new/3, synchronous_timed_new_link/3,
	remote_new/4, remote_new_link/4, remote_synchronous_new/4,
	remote_synchronous_new_link/4, remote_synchronous_timed_new/4,
	remote_synchronous_timed_new_link/4, construct/4, delete/1 ).


% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export, setAge/2, isHotBlooded/1, getFurColor/1,
	getArbitraryNumber/1 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Mammal.
construct(State,?wooper_construct_parameters) ->
	CreatureState = class_Creature:construct(State,Age,Gender),
	?setAttribute(CreatureState,fur_color,FurColor).
	
	
% Overriding default destructor:	
% State should be returned, and destructors should be called in leaf-to-root
% order in inheritance tree.
delete(State) ->
	io:format( "Deleting mammal ~w! (overridden destructor)~n", [self()] ),
	State.
	
	
% Method implementations.


% Sets correctly the age of this Mammal (not like faulty implementation of the
% Creature mother class).
% Overridden from Creature, useful to show the use of executeOneway.
% (oneway)
setAge(State,NewAge) ->
	?wooper_return_state_only(?setAttribute(State,age,NewAge)).


% All mammals are hot-blooded:
% (request)
isHotBlooded(State) ->
	?wooper_return_state_result(State,true).


% Attribute names could be defined in '-define().' header (.hrl) clauses,
% to ensure consistency.
getFurColor(State) ->
	?wooper_return_state_result( State, ?getAttribute(State,fur_color) ).
	
	
% Returns a class-specific arbitrary number.
% Overridden from Creature, useful to show the use of executeRequest.
% (request)
getArbitraryNumber(State) ->
	% Interesting test for the stack trace, when called from the Mammal test:	
	%throw( exception_throw_test_from_request ),
	?wooper_return_state_result(State,15).

