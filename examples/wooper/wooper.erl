-module(wooper).
-export([new/2, synchronous_new/2]).

-include("wooper.hrl").


new(M, Args) ->
    proc_lib:spawn(fun() -> S = init_p(M, Args),
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
	    error_logger:error_msg(
	      "WOOPER error for PID ~w of class ~s: "
	      "constructor did not return a state, but returned ~w instead. "
	      "Construction parameters were ~w.~n",
	      [ self(), ?MODULE, Other, Args] ),
	    % Wait a bit as error_msg seems asynchronous:
	    timer:sleep(1000),
	    exit({invalid_constructor,?MODULE})  % never throw without a catch
    end.



getAttribute(#state_holder{module = M, state = S}, AttrName) ->
    M:'#get-wooper_state'(AttrName).

setAttributes(#state_holder{module = M, state = S}, Attrs) ->
    M:'#set-wooper_state'(Attrs, S).


get_all_attributes(#state_holder{module = M,
				 state = S}) ->
    Names = M:'#info-wooper_state'(fields),
    lists:zip(Names, tl(tuple_to_list(S))).


