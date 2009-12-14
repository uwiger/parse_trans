-compile({parse_transform, wooper_parse_transform}).




% The class name, as mapped to a module.
-define(className,?MODULE).

-ifndef(wooper_return_state_result).
-define(wooper_return_state_result(State,Result),{State,Result}).
-define(wooper_return_state_only(State), State).
-endif.


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of
% actual attributes for a given class)
-define(WooperAttributeCountUpperBound,16).

%% predefined methods, hard-coded here just to get on with testing

-ifdef(wooper_construct_export).
-export([?wooper_construct_export]).
-endif.


-export([is_wooper_debug/0,
	 get_class_name/0,
	 get_class_name/1,
	 get_superclasses/0,
	 get_superclasses/1]).
is_wooper_debug() ->
    false.

get_class_name() ->
    '#get-wooper_info'(class, '#new-wooper_info'()).
get_class_name(State) ->
	?wooper_return_state_result(State,get_class_name()).

get_superclasses() ->
    '#get-wooper_info'(superclasses, '#new-wooper_info'()).

get_superclasses(State) ->
	?wooper_return_state_result(State,get_superclasses()).

-ifdef(wooper_construct_parameters).
new(?wooper_construct_parameters) ->
    wooper:new(?MODULE, [?wooper_construct_parameters]).
new_link(?wooper_construct_parameters) ->
    wooper:new_link(?MODULE, [?wooper_construct_parameters]).
synchronous_new(?wooper_construct_parameters) ->
    wooper:synchronous_new(?MODULE, [?wooper_construct_parameters]).
synchronous_new_link(?wooper_construct_parameters) ->
    wooper:synchronous_new_link(?MODULE, [?wooper_construct_parameters]).
synchronous_timed_new(?wooper_construct_parameters) ->
    wooper:synchronous_timed_new(?MODULE, [?wooper_construct_parameters]).
synchronous_timed_new_link(?wooper_construct_parameters) ->
    wooper:synchronous_timed_new_link(?MODULE, [?wooper_construct_parameters]).
remote_new(Node, ?wooper_construct_parameters) ->
    wooper:remote_new(Node, ?MODULE, [?wooper_construct_parameters]).
remote_new_link(Node, ?wooper_construct_parameters) ->
    wooper:remote_new_link(Node, ?MODULE, [?wooper_construct_parameters]).
remote_synchronous_new(Node, ?wooper_construct_parameters) ->
    wooper:remote_synchronous_new(Node, ?MODULE,
				  [?wooper_construct_parameters]).
remote_synchronous_new_link(Node, ?wooper_construct_parameters) ->
    wooper:remote_synchronous_new_link(Node, ?MODULE,
				       [?wooper_construct_parameters]).
remote_synchronous_timed_new(Node, ?wooper_construct_parameters) ->
    wooper:remote_synchronous_timed_new(Node, ?MODULE,
					[?wooper_construct_parameters]).
remote_synchronous_timed_new_link(Node, ?wooper_construct_parameters) ->
    wooper:remote_synchronous_timed_new_link(Node, ?MODULE,
					     [?wooper_construct_parameters]).

-endif.


-define(getAttr(AttributeName), wooper:getAttribute(State, (AttributeName))).

