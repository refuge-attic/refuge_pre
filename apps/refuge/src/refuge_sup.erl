
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
    setup(),

    DNSDService = case refuge_dnssd:use_dnssd() of
        true ->
            DNSSDDiscover = [?CHILD(refuge_dnssd_discover, worker)],
            case refuge_dnssd:discoverable() of
                true ->
                    [?CHILD(refuge_dnssd, worker)|DNSSDDiscover];
                false ->
                    DNSSDDiscover
            end;
        false ->
            []
    end,

    {ok, { {one_for_one, 5, 10}, DNSDService} }.

%% @doc setup refuge
setup() ->
    ConfDir = couch:get_app_env(config_dir, code:root_dir()),

    io:format("conf dir ~p~n", [ConfDir]),
    %% create initial certificate
    case couch_config:get("ssl", "key_file") of
        undefined ->
            io:format("Create refuge certificate", []),
            Cert = refuge_cert:make_cert(),
            refuge_cert:write_pem(ConfDir, "refuge", Cert),
            couch_config:set("ssl", "cert_file", filename:join(ConfDir,
                    "refuge.pem")),
            couch_config:set("ssl", "key_file", filename:join(ConfDir,
                    "refuge_key.pem")),
            couch_config:set("daemons", "httpsd",
                "{couch_httpd, start_link, [https]}");


        _ ->
            ok
    end.

