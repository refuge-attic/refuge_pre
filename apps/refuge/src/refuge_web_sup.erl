
-module(refuge_web_sup).

-behaviour(supervisor).

-include("supervisor.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    HTTP = refuge_web:init(),
    {ok, { {one_for_one, 10, 10}, [HTTP]} }.
