%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of refuge released under the Apache license 2.
%% See the NOTICE for more information.
%%
%% @author Benoît Chesneau <benoitc@refuge.io>
%% @doc module in charge of dnssd discovery of refuge.

-module(refuge_dnssd_discover).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("couch/include/couch_db.hrl").
-include("refuge.hrl").

-define(SERVER, ?MODULE).

-record(state, {
        browse_ref,
        active = []
    }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    {ok, Ref} = dnssd:browse("_refuge._tcp"),
    {ok, #state{browse_ref=Ref}}.


handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({dnssd, Ref, {browse, add, Result}}, #state{browse_ref = Ref}=State) ->
    Pid = add_node(Result),
    {noreply, State#state{active=[Pid | State#state.active]}};

handle_info({dnssd, Ref, {browse, remove, Result}}, #state{browse_ref = Ref}=State) ->
    ?LOG_INFO("remove node ~p~n", [Result]),
    Pid = remove_node(Result),
    {noreply, State#state{active=[Pid | State#state.active]}};

handle_info({'EXIT', From, normal}, #state{active=Pids}=State) ->
    {noreply, State#state{active = Pids -- [From]}};

handle_info({'EXIT', From, Reason}, State) ->
    {stop, {dnssd_error, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{browse_ref=Ref, active=Pids}) ->
    ok = dnssd:stop(Ref),
    %% close running jobs
    lists:foreach(fun(Pid) ->
                catch unlink(Pid),
                catch exit(Pid, stop)
        end, Pids),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% private

add_node({Name, Type, Domain}=Result) ->
    spawn_link(fun() ->
            case dnssd:resolve_sync(Name, Type, Domain) of
                {ok, {Host, Port, Txt}}  ->
                    MyId = refuge_util:node_id(),
                    case proplists:get_value(<<"node_id">>, Txt) of
                        undefined ->
                            ok; %% invalid informatoin, just ignore it.
                        NodeId when NodeId =:= MyId ->
                            ok; %% I don't care about myself
                        NodeId ->
                            ?LOG_INFO(?MODULE_STRING "got ~p, ~p, ~p~n",
                                [Host, Port, Txt]),
                            Node = #node{id=NodeId, name=Name, host=Host,
                                port=Port, type=Type,
                                last_seen=refuge_util:get_unix_timestamp(now())},
                            ok
                    end;
                {ok, _} ->
                    ok;
                Error ->
                    ?LOG_ERROR("error resolving ~p : ~p", [Result,
                            Error]),
                    ok
            end
    end).

remove_node(Result) ->
   ok.

scheme(<<"_http._tcp, _refuge">>) ->
    <<"http">>;
scheme(_) ->
    <<"https">>.
