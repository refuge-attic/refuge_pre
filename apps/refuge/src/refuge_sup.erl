
-module(refuge_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DNSDService = case refuge_dnssd:use_dnssd() of
        true ->
            [?CHILD(refuge_dnssd, worker)];
        false ->
            []
    end,

    {ok, { {one_for_one, 5, 10}, DNSDService} }.

