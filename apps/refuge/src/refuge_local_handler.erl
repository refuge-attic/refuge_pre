-module(refuge_local_handler).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("cowboy/include/http.hrl").

-record(state, {mochi_req, args}).

init(_Info, Req, Opts) ->
    {args, Args} = proplists:lookup(args, Opts),
    #http_req{socket=Socket,
              transport=Transport,
              buffer=Buffer,
              method=Method,
              version=Version,
              raw_path=Path,
              raw_qs=QS,
              headers=Headers,
              raw_host=Host,
              port=Port} = Req,

    MochiSocket = mochiweb_socket(Transport, Socket),
    DefaultPort = default_port(Transport:name()),
    MochiHost = case Port of
        DefaultPort ->
            Port;
        _ ->
            %% fix raw host
            binary_to_list(Host) ++ ":" ++ integer_to_list(Port)
    end,

    MochiHeaders = lists:foldl(fun
                ({'Host'=K, _V}, T) ->
                    mochiweb_headers:insert(K, MochiHost, T);
                ({K, V}, T) when is_binary(K) ->
                    mochiweb_headers:insert(binary_to_list(K),
                                         binary_to_list(V), T);
                ({K, V}, T) ->
                    mochiweb_headers:insert(K, binary_to_list(V), T)
            end, mochiweb_headers:empty(), Headers),

    MochiHeaders1 = mochiweb_headers:enter("x-couchdb-vhost-path",
                                           binary_to_list(Path),
                                           MochiHeaders),

    %% fix raw path
    Prefix = << "/rc_refuge/_design/ui/_rewrite" >>,
    Path1 = case Path of
        <<>> -> << Prefix/binary, "/" >>;
        _ -> << Prefix/binary, Path/binary >>
    end,
    RawPath = case QS of
        <<>> ->
            Path1;
        _ ->
            << Path1/binary, "?", QS/binary >>
    end,

    MochiReq = mochicow_request:new(MochiSocket,
                                    Method,
                                    binary_to_list(RawPath),
                                    Version,
                                    MochiHeaders1,
                                    Buffer),
    {ok, Req, #state{mochi_req=MochiReq, args=Args}}.


handle(Req, #state{mochi_req=MochiReq, args=Args}=State) ->
    couch_httpd:handle_request(MochiReq, Args),

    Connection =MochiReq:get_header_value("connection"),
    Req2 = Req#http_req{connection = list_to_connection(Connection),
                        resp_state = done,
                        body_state = done,
                        buffer = MochiReq:get(buffer) },

    case MochiReq:should_close() of
        true ->
            {ok, Req2#http_req{connection=close}, State};
        false ->
            MochiReq:cleanup(),
            erlang:garbage_collect(),
            {ok, Req2, State}
    end.

terminate(_Req, _State) ->
    ok.


mochiweb_socket(cowboy_ssl_transport, Socket) ->
    {ssl, Socket};
mochiweb_socket(_Transport, Socket) ->
    Socket.

list_to_connection(Connection) when is_binary(Connection) ->
    list_to_connection(binary_to_list(Connection));
list_to_connection(Connection) when is_atom(Connection) ->
    Connection;
list_to_connection("keep-alive") ->
    keepalive;
list_to_connection(_) ->
    close.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.
