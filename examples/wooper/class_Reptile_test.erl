% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Reptile class implementation.
% See the class_Reptile.erl tested module.

-module(class_Reptile_test).

-export([run/0]).

-define(Tested_module,class_Reptile).

-define(Prefix,"--> ").


% Comment out to be able to use the interpreter after the test:
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
	io:format( "~n!!!! Test failed for module ~s, reason: ~s~n~n",
		[ ?Tested_module, Reason ] ),
	erlang:error( "Test failed" ).	


run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Debug mode: ~s.~n", 
		[ class_Reptile:is_wooper_debug() ] ),	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_Reptile:get_class_name(), 
			class_Reptile:get_superclasses() ] ),
	MyR = class_Reptile:new(1,male),
	MyR ! {get_class_name,[],self()},
	receive
	
		{wooper_result,class_Reptile} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned 'class_Reptile' "
				"as expected.~n");
				
		{wooper_result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class: ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyR ! {get_superclasses,[],self()},
	receive
	
		{wooper_result, [class_Creature]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned [class_Creature] "
				"as expected.~n");

		{wooper_result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses: ~p", 
				[ UnexpectedSuperclasses ] ) )
	
	end,
	MyR ! {getAge,[],self()},
	receive
	
		{wooper_result,1} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 1 as expected.~n");

		{wooper_result,UnexpectedAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedAge ] ) )
		
	end,
	MyR ! {getGender,[],self()},
	receive
	
		{wooper_result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");
	
		{wooper_result,UnexpectedGender} -> 
			testFailed( io_lib:format( "wrong gender: ~p", 
				[ UnexpectedGender ] ) )
			
	end,
	MyR ! {setAge,2},
	MyR ! {getAge,[],self()},
	receive
	
		{wooper_result,2}->
			io:format(?Prefix 
				"After setAge, getAge returned 2 as expected.~n");
	
		{wooper_result,UnexpectedNewAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedNewAge ] ) )
			
	end,	
	MyR ! declareBirthday,
	MyR ! {getAge,[],self()},
	receive
	
		 {wooper_result,3}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 3 as expected.~n");

		{wooper_result,UnexpectedLastAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedLastAge ] ) )
	
	end,	
	MyR ! declareBirthday,
	MyR ! {isHotBlooded,[],self()},
	receive
	
		{wooper_result,false}->
			io:format(?Prefix 
				"isHotBlooded returned false as expected.~n");

		{wooper_result,UnexpectedBlood} -> 
			testFailed( io_lib:format( "wrong blood type: ~p", 
				[ UnexpectedBlood ] ) )
	
	end,	
	MyR ! {canMoult,[],self()},
	receive
	
		{wooper_result,true}->
			io:format(?Prefix 
				"canMoult returned true as expected.~n");

		{wooper_result,UnexpectedMoultType} -> 
			testFailed( io_lib:format( "wrong moult type: ~p", 
				[ UnexpectedMoultType ] ) )
	
	end,
	case class_Reptile:is_wooper_debug() of 
		true ->
			MyR ! { wooper_get_instance_description,[], self() },
			receive
			
				{wooper_result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,	
	
	% To check the result when using a faulty destructor:
	io:format(?Prefix "synchronous deletion of the instance.~n" ),
	MyR ! {synchronous_delete,self()},	
	receive
	
		{deleted,MyR} ->
			ok
			
	end,	
		
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

