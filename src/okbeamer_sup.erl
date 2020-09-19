% @doc okbeamer top level supervisor.
% @end
-module(okbeamer_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    SupFlags = #{strategy => one_for_one, 
    			intensity => 3, 
    			period => 15},

    ChildSpecs = [#{id => okbeamer_server,
                    start => {okbeamer_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [okbeamer_server]}],
    
    {ok, {SupFlags, ChildSpecs}}.
