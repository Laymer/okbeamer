% @doc okbeamer public API.
% @end
-module(okbeamer).

% Callbacks
-export([hello_world/0]).

%--- API ----------------------------------------------------------------------

hello_world() ->
	logger:info("hello world ! ~n").
