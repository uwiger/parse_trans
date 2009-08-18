% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Creature class implementation.
% See the class_Creature.erl tested module.

-module(class_Creature_test).

-export([run/0]).

-define(Tested_module,class_Creature).

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
		[ class_Creature:is_wooper_debug() ] ),	
	io:format( ?Prefix "Class name is ~s, superclasses are ~w.~n", [
		class_Creature:get_class_name(), class_Creature:get_superclasses() ] ),
	MyC = class_Creature:new(30,male),
	MyC ! {getAge,[],self()},
	receive
	
		{wooper_result,30} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 30 as expected.~n");

		{wooper_result,UnexpectedAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedAge ] ) )
	
	end,
	MyC ! {getGender,[],self()},
	receive
	
		{wooper_result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");

		{wooper_result,UnexpectedGender} -> 
			testFailed( io_lib:format( "wrong gender: ~p", 
				[ UnexpectedGender ] ) )
	
	end,
	MyC ! {setAge,5},
	% class_Creature:setAge returns always 36 for executeRequest test purposes:
	MyC ! {getAge,[],self()},
	receive
	
		{wooper_result,36}->
			io:format(?Prefix 
				"After setAge, getAge returned 36 as expected.~n");

		{wooper_result,UnexpectedNewAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedNewAge ] ) )
				
	end,	
	MyC ! declareBirthday,
	MyC ! {getAge,[],self()},
	receive
	
		 {wooper_result,37}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 37 as expected.~n");

		{wooper_result,UnexpectedLastAge} -> 
			testFailed( io_lib:format( "wrong age: ~p", 
				[ UnexpectedLastAge ] ) )	
	
	end,	
	
	MyC ! declareBirthday,
	
	MyC ! delete,	
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

