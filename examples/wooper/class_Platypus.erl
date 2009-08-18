% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Platypus).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Mammal,class_OvoviviparousBeing]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the Mammal mother class (the ovoviviparous being 
% constructor does not need any parameter) plus nozzle color.
% These are class-specific data needing to be set in the constructor:
-define(wooper_construct_parameters, Age, Gender, FurColor, NozzleColor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4, 
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronous_timed_new/5,
	remote_synchronous_timed_new_link/5, construct/5 ).


% Method declarations.
-define(wooper_method_export, getMeanEggsCount/1, getTeatCount/1, canEat/2,
	getNozzleColor/1, getAlternateNames/1, popFirstAlternateName/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Constructs a new Platypus.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	OvoviviparousMammalState = class_OvoviviparousBeing:construct( 
		MammalState ),
	
	% Then the class-specific attributes:
	?setAttributes( OvoviviparousMammalState, 
		[ {nozzle_color,NozzleColor},
		{alternate_names,[hector,edgar,roger,sean]} ] ).
	

	
getMeanEggsCount(State) ->
	?wooper_return_state_result( State, 2 ).
	
	
% It is a mammal, though !	
getTeatCount(State) ->
	?wooper_return_state_result( State, 0 ).

	
% Platypuses are supposed carnivorous though:
canEat(State,leaf) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,chocolate) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,weed) ->	
	?wooper_return_state_result( State, true );

canEat(State,fish) ->	
	?wooper_return_state_result( State, true );

canEat(State,_) ->
	?wooper_return_state_result( State, false ).


getNozzleColor(State)->
	?wooper_return_state_result( State, ?getAttribute(State,nozzle_color) ).


% Returns the list of alternate names for this platypus.
getAlternateNames(State) ->
	?wooper_return_state_result( State, ?getAttribute(State,alternate_names) ).
	

% Returns the first alternate name for this platypus and forget it.
popFirstAlternateName(State) ->
	{NewState,Name} = ?popFromAttribute(State,alternate_names),
	?wooper_return_state_result( NewState, Name ).

