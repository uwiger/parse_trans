% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Cat).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_Mammal,class_ViviparousBeing] ).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the Mammal mother class (the viviparous being 
% constructor does not need any parameter) plus whisker color.
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, Age, Gender, FurColor, WhiskerColor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4, 
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronous_timed_new/5,
	remote_synchronous_timed_new_link/5, construct/5, delete/1 ).


% Method declarations.
-define( wooper_method_export, getTeatCount/1, canEat/2, getWhiskerColor/1 ).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Cat.
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),
	
	% Then the class-specific attributes:
	?setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).
	
	
delete(State) ->
	io:format( "Deleting cat ~w! (overridden destructor)~n", [self()] ),
	State.
	
	
% No guarantee on biological fidelity:	
getTeatCount(State) ->
	?wooper_return_state_result( State, 6 ).

	
% Cats are supposed carnivorous though:
canEat(State,soup) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,chocolate) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,croquette) ->	
	?wooper_return_state_result( State, true );

canEat(State,meat) ->	
	?wooper_return_state_result( State, true );

canEat(State,_) ->
	?wooper_return_state_result( State, false ).


getWhiskerColor(State)->
	?wooper_return_state_result( State, ?getAttr(whisker_color) ).

