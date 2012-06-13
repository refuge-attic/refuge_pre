-module(refuge_main_handler).

-export([init/3, handle/2, terminate/2]).

init(_Info, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"zelcome to refuge">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
