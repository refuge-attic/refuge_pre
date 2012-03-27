%% @author Edward Wang <yujiangw@gmail.com>
%% @doc Implements a HTTP server for UPnP services to callback into.
%% @end
%%
%% @todo: Use supervisor_bridge instead?
%%        And rename to refuge_upnp_httpd_sup?
-module(refuge_upnp_handler).


-export([init/3, handle/2, terminate/2]).

%% API
-export([get_port/0]).

%%===================================================================
%% API
%%===================================================================
-define(UPNP_PORT, 0).
get_port() ->
    ?UPNP_PORT.

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    case cowboy_http_req:method(Req0) of
        {<<"NOTIFY">>, _} ->
            {ok, ReqBody, Req} = cowboy_http_req:body(Req0),
            %% @todo: intention here is to use eventing to monitor if there's
            %%        someone else steals refuge's port mapping. but seems
            %%        the router used to test against it (linksys srt54g) doesn't
            %%        send out notifition for port mapping. so the port mapping
            %%        protection is yet to be implemented. only the eventing
            %%        subscribe / unsubscribe skeleton is done.
            case refuge_upnp_proto:parse_notify_msg(ReqBody) of
                undefined ->
                    ignore
                %%Content ->
                %%    refuge_upnp_entity:notify(Content)
            end,
            {ok, Reply} = cowboy_http_req:reply(200,[
                        {<<"Content-type">>, <<"text/plain">>}], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) ->
    ok.
