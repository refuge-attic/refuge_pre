
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

    %% initialize the UI app if needed
    Path = filename:join([refuge_util:priv_dir(), "ui"]),
    ok = refuge_couchapp:save_couchapp(<<"rc_refuge">>, Path, [create_db]),

    {ok, { {one_for_one, 10, 10}, [HTTP]} }.
