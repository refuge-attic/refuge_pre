-module(refuge_sup).
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
    Tables       = ?CHILD(refuge_table),
    EventManager = ?CHILD(refuge_event),

    HTTP = ?SUP(refuge_web_sup),

    %% UPnP subsystemm is optional.
    UPNPSup = case couch_config:get("refuge", "use_upnp", "false") of
        "true" ->
            [{upnp_sup,
              {refuge_upnp_sup, start_link, []},
              permanent, infinity, supervisor, [refuge_upnp_sup]}];
        _ ->
            []
    end,

    DNSSD = case couch_config:get("refuge", "use_dnssd", "true") of
        "true" ->
            [?CHILD(refuge_dnssd)];
        _ ->
            []
    end,

    Children = [HTTP, Tables, EventManager] ++  UPNPSup ++  DNSSD,
    {ok, { {one_for_one, 10, 10}, Children} }.
