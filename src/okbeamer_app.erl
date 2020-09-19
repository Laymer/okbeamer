% @doc okbeamer public API.
% @end
-module(okbeamer_app).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> okbeamer_sup:start_link().

stop(_State) -> ok.
