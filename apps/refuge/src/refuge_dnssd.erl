%% @doc advertise and discover nodes using dnnsd

-module(refuge_dnssd).
-behaviour(gen_server).

-include("refuge.hrl").

-export([start_link/0, list_services/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {local_only, reg_ref, browse_ref}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


list_services() ->
    gen_server:call(?SERVER, list_services).

%% --------------------
%% gen_server callbacks
%% --------------------

init([]) ->
    {ok, BrowseRef} = dnssd:browse("_refuge._tcp"),
    NewState = case can_register() of
        true ->
            io:format("register dnssd~n", []),
            {ok, RegRef} = dnssd:register(service_name(), "_refuge._tcp",
                                       couch_util:get_port(https)),
            #state{local_only = true, reg_ref = RegRef,
                   browse_ref = BrowseRef};
        false ->
            io:format("can't register dnssd~n", []),
            #state{local_only = true, browse_ref = BrowseRef}
    end,
    {ok, NewState}.

handle_call(list_services, _From, #state{} = State) ->
    {ok, RegResults} = dnssd:results(State#state.reg_ref),
    {ok, BrowseResults} = dnssd:results(State#state.browse_ref),
    Services = BrowseResults -- RegResults,
    Reply = {ok, Services},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({dnssd, Ref, {browse, add, Result}}, #state{browse_ref =
                                                           Ref}) ->
    spawn_link(fun() ->
                maybe_notify(dnssd_nodeup, Result)
        end),
    lager:info(?MODULE_STRING " browse ~s: ~p~n", [remove, Result]),
    {noreply, state};
handle_info({dnssd, Ref, {browse, remove, Result}}, #state{browse_ref =
                                                           Ref}) ->
    spawn_link(fun() ->
                maybe_notify(dnssd_nodedown, Result)
        end),

    lager:info(?MODULE_STRING " browse ~s: ~p~n", [remove, Result]),
    {noreply, state};
handle_info({dnssd, Ref, {register, Change, Result}}, #state{reg_ref = Ref}) ->
    lager:info(?MODULE_STRING " register ~s: ~p~n", [Change, Result]),
    {noreply, state};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{browse_ref=BrowseRef, reg_ref=RegRef}) ->
    ok = dnssd:stop(BrowseRef),
    ok = dnssd:stop(RegRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -----------------------
%% private functions
%% -----------------------

maybe_notify(Type, Result) ->
    case make_node_info(Result) of
        {error, _} -> ok;
        Node ->
            lager:info(?MODULE_STRING " ~p ~p~n", [Type, Node]),
            refuge_event:notify({Type, Node})
    end.


make_node_info({Name, Type, Domain}=Result) ->
    case dnssd:resolve_sync(Name, Type, Domain) of
        {ok, {Host, Port, _Txt}} ->
            #node{name=Name, host=Host, port=Port,
                  time=refuge_util:get_unix_timestamp(erlang:now())};
        Error ->
            lager:error("error resolving ~p : ~p", [Result, Error]),
            Error
    end.

service_name() ->
    refuge_util:new_id().

is_local() ->
    case couch_config:get(<<"httpd">>, <<"bind_address">>) of
        "127." ++ _ ->
            true;
        "::1" ->
            true;
        _ ->
            false
    end.

can_register() ->
    Advertise = couch_config:get("refuge", "advertise_dnssd", "true"),
    case {Advertise, is_local()} of
        {"true", false} ->
            true;
        Else ->
            io:format("got else ~p~n", [Else]),
            false
    end.
