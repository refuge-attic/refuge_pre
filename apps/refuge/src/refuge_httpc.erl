-module(refuge_httpc).

-export([request/4, request/5, request/6,
         request_stream/4, request_stream/5, request_stream/6]).

-define(TIMEOUT, infinity).

%% @doc send an ibrowse request
request(Method, Url, Expect, Options) ->
    request(Method, Url, Expect, Options, [], []).
request(Method, Url, Expect, Options, Headers) ->
    request(Method, Url, Expect, Options, Headers, []).
request(Method, Url, Expect, Options, Headers, Body) ->
    Accept = {"Accept", "application/json, */*;q=0.9"},
    {Headers1, Options1} = maybe_oauth_header(Method, Url, Headers, Options),
    case ibrowse:send_req(Url, [Accept|Headers1], Method, Body,
            [{response_format, binary}|Options1], ?TIMEOUT) of
        Resp={ok, Status, _, _} when Expect /= [] ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Resp={ok, _, _, _} ->
            Resp;
        Error -> Error
    end.

%% @doc stream an ibrowse request
request_stream(Pid, Method, Url, Options) ->
    request_stream(Pid, Method, Url, Options, []).
request_stream(Pid, Method, Url, Options, Headers) ->
    request_stream(Pid, Method, Url, Options, Headers, []).
request_stream(Pid, Method, Url, Options, Headers, Body) ->
    {Headers1, Options1} = maybe_oauth_header(Method, Url, Headers,
        Options),
    {ok, ReqPid} = ibrowse_http_client:start_link(Url),
    case ibrowse:send_req_direct(ReqPid, Url, Headers1, Method, Body,
                          [{stream_to, Pid},
                           {response_format, binary},
                           {inactivity_timeout, infinity}|Options1],
                           ?TIMEOUT) of
        {ibrowse_req_id, ReqId} ->
            {ok, ReqId};
        Error ->
            Error
    end.

maybe_oauth_header(Method, Url, Headers, Options) ->
    case refuge_util:get_value(oauth, Options) of
        undefined ->
            {Headers, Options};
        OauthProps ->
            Hdr = refuge_util:oauth_header(Url, Method, OauthProps),
            {[Hdr|Headers], proplists:delete(oauth, Options)}
    end.


