% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the ViviparousBeing class implementation.
% See the class_ViviparousBeing.erl tested module.

-module(class_ViviparousBeing_test).

-export([run/0]).

-define(Tested_module,class_ViviparousBeing).

-define(Prefix,"--> ").


% Comment out to be able to use the interpreter after the test :
-define(ExitAfterTest,).

-ifdef(ExitAfterTest).

testFinished() ->
	erlang:halt().
	
-else.

testFinished() ->
	io:format( "(interpreter still running)~n" ),
	test_success.
	
-endif.


testFailed(Reason) ->
	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable list.
	io:format( "~n!!!! Test failed for module ~s, reason : ~s~n~n",
		[ ?Tested_module, Reason ] ),
	erlang:error( "Test failed" ).	


run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Debug mode : ~s.~n", 
		[ class_ViviparousBeing:is_wooper_debug() ] ),	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_ViviparousBeing:get_class_name(), 
			class_ViviparousBeing:get_superclasses() ] ),
	MyV = class_ViviparousBeing:new(),
	MyV ! {get_class_name,[],self()},
	receive
	
		{wooper_result,class_ViviparousBeing} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned "
				"'class_ViviparousBeing' as expected.~n");
				
		{wooper_result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class : ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyV ! {get_superclasses,[],self()},
	receive
	
		{wooper_result, []} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned [] "
				"as expected.~n");

		{wooper_result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses : ~p", 
				[ UnexpectedSuperclasses ] ) )
		
	end,
	MyV ! {getMeanChildrenCount,[],self()},
	receive
	
		{wooper_result,4} ->
			io:format( ?Prefix 
				"After constructor, getMeanChildrenCount returned 4 "
				"as expected.~n");

		{wooper_result,UnexpectedMeanCount} -> 
			testFailed( io_lib:format( "wrong mean children count : ~p", 
				[ UnexpectedMeanCount ] ) )
	
		
	end,
	MyV ! {getBirthGivenCount,[],self()},
	receive
	
		{wooper_result,0} ->
			io:format( ?Prefix 
				"After constructor, getBirthGivenCount returned 0 "
				"as expected.~n");
	
		{wooper_result,UnexpectedFirstCount} -> 
			testFailed( io_lib:format( "wrong first children count : ~p", 
				[ UnexpectedFirstCount ] ) )
			
	end,
	MyV ! {giveBirth,7},
	MyV ! {getBirthGivenCount,[],self()},
	receive
	
		 {wooper_result,7}->
			io:format(?Prefix 
				"After giveBirth, getBirthGivenCount returned 7 "
				"as expected.~n");
	
		{wooper_result,UnexpectedSecondCount} -> 
			testFailed( io_lib:format( "wrong second children count : ~p", 
				[ UnexpectedSecondCount ] ) )
				
	end,		
	case class_ViviparousBeing:is_wooper_debug() of 
		true ->
			MyV ! { wooper_get_instance_description,[], self() },
			receive
			
				{wooper_result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,
	MyV ! delete,			
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

