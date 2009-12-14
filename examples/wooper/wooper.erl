-module(wooper).
-export([new/2,
	 new_link/2,
	 synchronous_new/2,
	 synchronous_timed_new/2,
	 getAttribute/2,
	 setAttribute/3,
	 setAttributes/2,
	 get_all_attributes/1,
	 executeOneway/2,
	 executeOneway/3,
	 executeOnewayWith/3,
	 executeOnewayWith/4,
	 executeRequest/2,
	 executeRequest/3,
	 wooper_execute_method/3,
	 wooper_execute_method_with/4]).

-include("wooper.hrl").


new(M, Args) ->
    proc_lib:spawn(fun() -> S = init_p(M, Args),
			    wooper_main_loop(S)
		   end).

new_link(M, Args) ->
    proc_lib:spawn_link(fun() -> S = init_p(M, Args),
				 wooper_main_loop(S)
			end).

synchronous_new(M, Args) ->
    synchronous_timed_new(M, Args, infinity).

synchronous_timed_new(M, Args) ->
    synchronous_timed_new(M, Args, 5000).

synchronous_timed_new(M, Args, Timeout) ->
    Parent = self(),
    start_link(fun() ->
		       S = init_p(M, Args),
		       proc_lib:init_ack(Parent, self()),
		       wooper_main_loop(S)
	       end, Timeout).


start_link(F, Timeout) ->
    Pid = proc_lib:spawn_link(F),
    sync_wait(Pid, Timeout).
    
%% copied from proc_lib.erl
sync_wait(Pid, Timeout) ->
    receive
        {ack, Pid, Return} ->
            Return;
        {'EXIT', Pid, Reason} ->
            {error, Reason}
    after Timeout ->
            unlink(Pid),
            exit(Pid, kill),
            flush(Pid),
            {error, timeout}
    end.

-spec flush(pid()) -> 'true'.

flush(Pid) ->
    receive
        {'EXIT', Pid, _} ->
            true
    after 0 ->
            true
    end.
%% -- end copy from proc_lib.

init_p(M, Args) ->
    Rec = M:'#new-'(wooper_state),
    State0 = #state_holder{module = M,
			  state = Rec},
    case apply(M, construct, [State0|Args]) of
	State when is_record(State, state_holder) ->
	    State;
	Other ->
	    error_msg(
	      "WOOPER error for PID ~w of class ~s: "
	      "constructor did not return a state, but returned ~w instead. "
	      "Construction parameters were ~w.~n",
	      [ self(), ?MODULE, Other, Args] ),
	    % never throw without a catch
	    erlang:error({invalid_constructor,?MODULE})
    end.


getAttribute(#state_holder{module = M, state = S}, AttrName) ->
    M:'#get-wooper_state'(AttrName, S).


setAttribute(State, Attr, Value) ->
    setAttributes(State, [{Attr,Value}]).
    
setAttributes(#state_holder{module = M, state = S} = State, Attrs) ->
    S1 = M:'#set-wooper_state'(Attrs, S),
    State#state_holder{state = S1}.


get_all_attributes(#state_holder{module = M,
				 state = S}) ->
    Names = M:'#info-wooper_state'(fields),
    lists:zip(Names, tl(tuple_to_list(S))).


% Waits for incoming requests and serves them.
wooper_main_loop(State) ->
    receive
	Msg ->
	    NewState = handle_msg(Msg, State),
	    wooper_main_loop(NewState)
    end.
	

handle_msg({ MethodAtom, ArgumentList, CallerPid }, State)
  when is_pid(CallerPid) and is_list(ArgumentList) ->
	% Instance PID could be sent back as well to discriminate 
	% received answers on the caller side.
    ?wooper_log_format( "Main loop (case A) for ~w: "
			"request '~s' with argument list ~w for ~w.~n", 
			[self(), MethodAtom, ArgumentList, CallerPid] ),
    SenderAwareState = State#state_holder{request_sender=CallerPid},
    { NewState, Result } =
	wooper_execute_method( MethodAtom,
			       SenderAwareState, ArgumentList ), 
    %CallerPid ! { self(), Result }
    CallerPid ! Result,
                        
    % Force a crash if instance-side error detected:
    maybe_exit(Result),
    NewState#state_holder{request_sender=undefined};
handle_msg({MethodAtom, Argument, CallerPid}, State) when is_pid(CallerPid) ->
    ?wooper_log_format( "Main loop (case B) for ~w: "
			"request '~s' with argument ~w for ~w.~n", 
			[self(), MethodAtom, Argument, CallerPid] ),
    SenderAwareState = State#state_holder{request_sender=CallerPid},
    { NewState, Result } =
	wooper_execute_method( MethodAtom,
			       SenderAwareState, [ Argument ] ), 
    CallerPid ! Result,
    maybe_exit(Result),
    NewState#state_holder{request_sender=undefined};

% (either this method does not return anything, or the sender is not
% interested in the result)
handle_msg({MethodAtom, ArgumentList}, State) when is_list(ArgumentList) ->
    ?wooper_log_format( "Main loop (case C) for ~w: "
			"oneway '~s' with argument list ~w.~n",
			[self(), MethodAtom, ArgumentList] ),
    % Any result would be ignored, only the updated state is kept:
    { NewState, _ } = wooper_execute_method( 
			MethodAtom, State, ArgumentList ),
    NewState;
handle_msg({synchronous_delete, CallerPid}, State) ->
    ?wooper_log("Main loop: oneway synchronous delete.~n"),
    wooper_destruct( State ),
    CallerPid ! {deleted,self()},
    exit(normal);
% ping is always available and cannot be overridden:
handle_msg({ ping, CallerPid }, State) ->
    ?wooper_log_format( "Main loop (case D) for ~w: oneway ping.~n",
			[self()]),
    CallerPid ! {pong,self()},
    State;
% Oneway with parameters:               
handle_msg({ MethodAtom, Argument }, State) ->
    ?wooper_log_format( "Main loop (case E) for ~w: "
			"oneway '~s' with argument ~w.~n",
			[ self(), MethodAtom, Argument ] ),
    % Any result would be ignored, only the updated state is kept:
    { NewState, _ } = wooper_execute_method( MethodAtom, State, 
					     [ Argument ] ),
    NewState;
handle_msg(delete, State) ->
    ?wooper_log("Main loop: oneway delete.~n"),
    % Triggers the recursive call of destructors in the inheritance
    % graph (bottom-up):
    wooper_destruct( State ),
    exit(normal);
handle_msg(MethodAtom, State) when is_atom(MethodAtom) ->
    ?wooper_log_format( 
       "Main loop (case F) for ~w: oneway from atom ~s.~n",
       [self(),MethodAtom]),
    { NewState, _ } = wooper_execute_method( MethodAtom, State, [] ),
    NewState;
handle_msg({'EXIT',Pid,ExitType}, State) when is_pid(Pid) ->
    ?wooper_log_format( "Main loop (case G) for ~w: exit with ~w.~n",
			[self(),{Pid,ExitType}] ),
    #state_holder{module = M, state = Rec} = State,
    try M:'#get-wooper_state'({onWooperExitReceived, 3}, Rec),
	{S1, _} = wooper_execute_method(
		    onWooperExitReceived, State, [Pid, ExitType]),
	S1
    catch
	error:_ ->
	    wooper_default_exit_handler(State, Pid, ExitType)
    end;
handle_msg(Other, State) ->
    ?wooper_log_format( "Main loop (case H) for ~w: unmatched ~s.~n",
			[self(),Other] ),                       
    error_logger:warning_msg( "WOOPER ignored following message: ~w.~n",
			      [Other]),
    State.


maybe_exit(Result) ->
    case element(1,Result) of
	X when X==wooper_method_failed; X==wooper_method_faulty_return ->
	    erlang:error(Result) ;
	_ ->
	    ok      
    end.

%% executeOneway/2
%% Parameter-less oneway.
%%
executeOneway(State, OnewayAtom) when is_record(State,state_holder)
				       andalso is_atom(OnewayAtom) -> 
			
    % No request_sender to change with oneways.
    
    % Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method( OnewayAtom, State, [] ), 
    
    % Returns:			
    NewState;
executeOneway( StateError, OnewayAtom ) when is_atom(OnewayAtom) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway ~w: "
	      "first parameter should be a state, not '~w'.~n",
	      [ self(), ?MODULE, OnewayAtom, StateError] ),
    erlang:error({invalid_oneway_call,OnewayAtom});
executeOneway( _State, OnewayError ) -> 
	error_logger:error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway: '~w' is not an atom.~n",
		[ self(), ?MODULE, OnewayError] ),
	% Wait a bit as error_msg seems asynchronous:
	timer:sleep(1000),	
	throw({invalid_oneway_call,OnewayError}).


%% executeOneway/3
%%
executeOneway( State, OnewayAtom, ArgumentList ) when
		is_record(State,state_holder) andalso is_atom(OnewayAtom) 
		andalso is_list(ArgumentList) -> 
    % No request_sender to change with oneways.
    % Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method( OnewayAtom, State, ArgumentList ), 
    % Returns:	
    NewState;
% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
		is_record(State,state_holder) andalso is_atom(OnewayAtom) -> 
    % No request_sender to change with oneways.
    % Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method( OnewayAtom, State, [StandaloneArgument] ), 
    
    % Returns:			
    NewState;
% Catches all errors:
executeOneway( StateError, OnewayAtom, _LastArg ) when is_atom(OnewayAtom) -> 
    error_msg(	"WOOPER error for PID ~w of class ~s "
		"when executing oneway ~w: "
		"first parameter should be a state, not '~w'.~n",
		[ self(), ?MODULE, OnewayAtom, StateError] ),
    erlang:error({invalid_oneway_call,OnewayAtom});
executeOneway( _State, OnewayAtomError, _LastArg ) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway: '~w' is not an atom.~n",
	      [ self(), ?MODULE, OnewayAtomError] ),
    erlang:error({invalid_oneway_call,OnewayAtomError}).



% Parameter-less oneway, relying on the specified actual class to be used,
% instead of determining it from the instance virtual table.
executeOnewayWith( State, ClassName, OnewayAtom ) 
  when is_record(State,state_holder) andalso is_atom(ClassName) 
       andalso is_atom(OnewayAtom) ->
	% No request_sender to change with oneways.
	% Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method_with(	ClassName, OnewayAtom, State, [] ), 
    % Returns:			
    NewState;
executeOnewayWith( StateError, ClassName, OnewayAtom )
  when is_atom(ClassName) andalso is_atom(OnewayAtom) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway ~w with ~s: "
	      "first parameter should be a state, not '~w'.~n",
	      [ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),
    erlang:error({invalid_oneway_call,OnewayAtom});
executeOnewayWith( _State, ClassNameError, OnewayError ) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway: both '~w' and '~w' should be atoms.~n",
	      [ self(), ?MODULE, ClassNameError, OnewayError] ),
    erlang:error({invalid_oneway_call,ClassNameError,OnewayError}).



%% executeOnewayWith/4
%%
% Allows to call synchronously from the code of a given class the oneway 
% defined in specified class, instead of determining it from the instance
% virtual table.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeOnewayWith( State, ClassName, OnewayAtom, ArgumentList )
  when is_record(State,state_holder) andalso is_atom(ClassName)  
       andalso is_atom(OnewayAtom) andalso is_list(ArgumentList) -> 
    % No request_sender to change with oneways.
    % Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method_with(
	  ClassName, OnewayAtom, State, ArgumentList ), 
    % Returns:	
    NewState;
% Here third parameter is not a list:
executeOnewayWith( State, ClassName, OnewayAtom, StandaloneArgument )
  when is_record(State,state_holder) andalso is_atom(ClassName) 
       andalso is_atom(OnewayAtom) -> 
    % No request_sender to change with oneways.
    % Correction checking by pattern-matching:
    { NewState, wooper_method_returns_void } =
	wooper_execute_method_with(
	  ClassName, OnewayAtom, State, [StandaloneArgument] ), 
    % Returns:			
    NewState;
executeOnewayWith( StateError, ClassName, OnewayAtom, _LastArg ) 
  when is_atom(ClassName) andalso is_atom(OnewayAtom) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway ~w with ~s: "
	      "first parameter should be a state, not '~w'.~n",
	      [ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),
    erlang:error({invalid_oneway_call,OnewayAtom});
% Catches all remaining errors:
executeOnewayWith( _State, ClassName, OnewayAtomError, _LastArg ) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing oneway with ~s: "
	      "both '~w' and '~w' should be atoms.~n",
	      [ self(), ?MODULE, ClassName, OnewayAtomError ] ),
    erlang:error({invalid_oneway_call,OnewayAtomError}).


% Parameter-less request, calling implicitly any overridden version of the
% method.
executeRequest( State, RequestAtom )
  when is_record(State,state_holder) andalso is_atom(RequestAtom) -> 
	% Auto-calling method:		
    SenderAwareState = State#state_holder{request_sender=self()},
    % Correction checking by pattern-matching:
    { NewState, {wooper_result,Result} } =
	wooper_execute_method( RequestAtom,
			       SenderAwareState, [] ), 
    % Returns:			
    {NewState#state_holder{request_sender=undefined}, Result};
executeRequest( StateError, RequestAtom ) when is_atom(RequestAtom) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing request ~w: "
	      "first parameter should be a state, not '~w'.~n",
	      [ self(), ?MODULE, RequestAtom, StateError] ),
    erlang:error({invalid_request_call,RequestAtom});
executeRequest( _State, RequestAtomError ) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing request: '~w' is not an atom.~n",
	      [ self(), ?MODULE, RequestAtomError] ),
    erlang:error({invalid_request_call,RequestAtomError}).

% Allows to call synchronously from the code of a given class its actual 
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the 
% (possibly overridden by, say, a class Car) startEngine method, then 
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%  
executeRequest( State, RequestAtom, ArgumentList )
  when is_record(State,state_holder) andalso is_atom(RequestAtom) 
       andalso is_list(ArgumentList) -> 
    % Auto-calling method:		
    SenderAwareState = State#state_holder{request_sender=self()},
    % Correction checking by pattern-matching:
    { NewState, {wooper_result,Result} } =
	wooper_execute_method( RequestAtom,
			       SenderAwareState, ArgumentList ), 
    % Returns:			
    {NewState#state_holder{request_sender=undefined}, Result};
% Here the third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument )
  when is_record(State,state_holder) andalso is_atom(RequestAtom)-> 
    % Auto-calling method:		
    SenderAwareState = State#state_holder{request_sender=self()},
    % Correction checking by pattern-matching:
    { NewState, {wooper_result,Result} } =
	wooper_execute_method( RequestAtom,
			       SenderAwareState, [StandaloneArgument] ), 
    % Returns:			
    {NewState#state_holder{request_sender=undefined}, Result};
% Catches all errors:
executeRequest( StateError, RequestAtom, _LastArg ) 
  when is_atom(RequestAtom) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing request ~w: "
	      "first parameter should be a state, not '~w'.~n",
	      [ self(), ?MODULE, RequestAtom, StateError] ),
    erlang:error({invalid_request_call,RequestAtom});
executeRequest( _State, RequestAtomError, _LastArg ) -> 
    error_msg("WOOPER error for PID ~w of class ~s "
	      "when executing request: '~w' is not an atom.~n",
	      [ self(), ?MODULE, RequestAtomError] ),
    erlang:error({invalid_request_call,RequestAtomError}).


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
wooper_execute_method(MethodAtom,
		      #state_holder{module = M} = State,Parameters) ->
    wooper_execute_method_with(M, MethodAtom, State, Parameters).

wooper_execute_method_with(Class, MethodAtom, State, Parameters) ->
    % The code transform stage makes sure that all methods are mapped
    % from the current module.
    try apply(Class, MethodAtom, [State|Parameters]) of
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
		    % (error term would often be unreadable with ~p)
		    error_msg(
		      "WOOPER error for PID ~w: "
		      "oneway method ~s:~s/~B failed (cause: ~w) "
		      "with error term '~w' for parameters ~w, "
		      "state held in module ~w, "
		      "stack trace was (latest calls first):~n~p.~n",
		      [ self(), Class, MethodAtom,
			length(Parameters)+1, Reason, ErrorTerm,
			Parameters, State#state_holder.module,
			erlang:get_stacktrace()] ),
		    % Terminates the process:       
		    exit( {wooper_oneway_failed, self(),
			   Class, MethodAtom, length(Parameters)+1,
			   Parameters, ErrorTerm} );
		_ ->
		    % This is a request, send error term and rely on
		    % the calling function (wooper_main_loop) to crash:
		    % (error term would often be unreadable with ~p)
		    error_msg(
		      "WOOPER error for PID ~w: "
		      "request method ~s:~s/~B failed (cause: ~w) "
		      "with error term '~w' for parameters ~w, "
		      "stack trace was (latest calls first):~n~p.~n",
		      [ self(), Class, MethodAtom,
			length(Parameters)+1, Reason, ErrorTerm,
			Parameters,erlang:get_stacktrace()] ),
		    {State,
		     {wooper_method_failed, self(), Class,
		      MethodAtom, length(Parameters)+1, 
		      Parameters, ErrorTerm} }                        
	    end
    end. 

wooper_destruct(#state_holder{module = M} = State) ->
    case erlang:function_exported(M, delete, 1) of
	true ->
	    case M:delete(State) of
		NewState when is_record(NewState, state_holder) ->
		    NewState;
		Other ->
		    error_msg(
		      "WOOPER error for PID ~w of class ~s: "
		      "user-defined destructor did not return a state, "
		      "but returned '~w' instead.~n", 
		      [ self(), ?MODULE, Other]),
		    exit({invalid_destructor, M})
	    end;
	false ->
	    State
    end.

error_msg(Fmt, Args) ->
    % not sure why we would want to wait 1 second, but that can be added here.
    error_logger:error_msg(Fmt, Args).


% WOOPER default EXIT handler.
% Can be overridden by defining or inheriting the onWooperExitReceived/3 
% method.	
wooper_default_exit_handler(State,Pid,ExitType) ->
	io:format( "  Warning: the WOOPER default EXIT handler ignored "
		"following EXIT message from ~w:~n~p.~n~n", [Pid,ExitType] ),
	?wooper_return_state_only(State).
