%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of refuge released under the Apache license 2.
%% See the NOTICE for more information.
%%
%% @author Benoît Chesneau <benoitc@refuge.io>
%% @doc module in charge of dnssd registration of refuge.

-module(refuge_dnssd).

-behaviour(gen_server).

-export([start_link/0, use_dnssd/0, discoverable/0, get_port/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("couch/include/couch_db.hrl").

-define(SERVER, ?MODULE).


-record(state, {
        reg_ref = nil,
        sreg_ref = nil,
        node_id}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

use_dnssd() ->
    case couch_config:get("refuge", "enable_dnssd", "true") of
        "true" -> true;
        _ -> false
    end.

discoverable() ->
    case  couch_config:get("refuge", "register_dnssd", "true") of
        "true" -> true;
        _ -> false
    end.

get_port(httpd) ->
    mochiweb_socket_server:get(couch_httpd, port);
get_port(httpsd) ->
    mochiweb_socket_server:get(https, port).

init(_) ->
    NodeId = refuge_util:node_id(),
    RegRef = register_service(httpd, NodeId),
    SRegRef = register_service(httpsd, NodeId),
    {ok, #state{reg_ref=RegRef, sreg_ref=SRegRef, node_id=NodeId}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{reg_ref=RegRef, sreg_ref=SRegRef}) ->
    _ = [ ok = dnssd:stop(Ref) || Ref <- [RegRef, SRegRef], is_reference(Ref) ],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internal functions

register_service(Name, NodeId) ->
    case refuge_util:is_daemon(Name) of
        true ->
            Port = get_port(Name),
            ServiceName = service_name(),
            Type = case Name of
                       httpd -> "_http._tcp,_refuge";
                       httpsd -> "_https._tcp,_refuge"
                   end,

            Txt = [{path, "/"}, {node_id, NodeId}],
            {ok, Ref} = dnssd:register(ServiceName, Type, Port, Txt),
            Ref;
        false ->
            nil
    end.

service_name() ->
    case couch_config:get(<<"refuge">>, <<"name">>) of
        ServiceName when is_list(ServiceName) andalso
        length(ServiceName) < 64 ->
            ServiceName;
        _ ->
            Name = build_service_name(),
            couch_config:set("refuge", "name", Name),
            Name
    end.

build_service_name() ->
    Prefix = case username() of
        {ok, Username} ->
            case lists:reverse(Username) of
                "s" ++ _ -> Username ++ "' refuge on ";
                _ -> Username ++ "'s refuge on "
            end;
        _ -> "refuge on "
    end,
    PrefixLen = length(Prefix),
    case inet:gethostname() of
        {ok, Hostname} ->
            HostnameLen = length(Hostname),
            if HostnameLen + PrefixLen < 64 ->
                    Prefix ++ Hostname;
                true -> ""
            end;
        _ -> ""
    end.

username() ->
    case os:getenv("USER") of
        Username when is_list(Username) ->
            {ok, Username};
        _ ->
            case os:getenv("USERNAME") of
                Username when is_list(Username) ->
                    {ok, Username};
                _ ->
                    undefined
            end
    end.

