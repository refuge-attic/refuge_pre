-module(refuge_web).

-export([init/0, get_port/1]).

init() ->
    Port = list_to_integer(couch_config:get("refuge", "port", "9000")),
    NbAcceptors = list_to_integer(
            couch_config:get("refuge", "nb_acceptors", "100")
    ),

    {ok, ProtoOpts} = get_protocol_options(),
    couch_httpd:set_auth_handlers(),

    BindAddress = couch_config:get("refuge", "ui_address", "127.0.0.1"),

    ParsedIp = case BindAddress of
        any ->
            any;
        Ip when is_tuple(Ip) ->
            Ip;
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            IpTuple
    end,

    TransOpts = [{port, Port}, {ip, ParsedIp}],
    cowboy:child_spec(refuge_ui, NbAcceptors, cowboy_tcp_transport,
                      TransOpts, cowboy_http_protocol, ProtoOpts).

get_protocol_options() ->
    DefaultSpec = "{couch_httpd_db, handle_request}",
    DefaultFun = couch_httpd:make_arity_1_fun(
        couch_config:get("httpd", "default_handler", DefaultSpec)
    ),

    UrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {list_to_binary(UrlKey), couch_httpd:make_arity_1_fun(SpecStr)}
        end, couch_config:get("httpd_global_handlers")),

    DbUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {list_to_binary(UrlKey), couch_httpd:make_arity_2_fun(SpecStr)}
        end, couch_config:get("httpd_db_handlers")),

    DesignUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {list_to_binary(UrlKey), couch_httpd:make_arity_3_fun(SpecStr)}
        end, couch_config:get("httpd_design_handlers")),

    UrlHandlers = dict:from_list(UrlHandlersList),
    DbUrlHandlers = dict:from_list(DbUrlHandlersList),
    DesignUrlHandlers = dict:from_list(DesignUrlHandlersList),

    {ok, SocketOptions} = couch_util:parse_term(
            couch_config:get("httpd", "socket_options", "[]")
    ),

    Args = {DefaultFun, UrlHandlers, DbUrlHandlers, DesignUrlHandlers,
             SocketOptions},
    CouchDBOptions = [{args, Args}],

    Dispatch = [
        {'_', [{[<<"local">>, '...'], refuge_local_handler, CouchDBOptions},
               {'_', refuge_main_handler, []}]}
    ],

    {ok, [{dispatch, Dispatch}]}.


%% @doc Return the port used by a listener.
%%
-spec get_port(any()) -> inet:port_number().
get_port(Ref) ->
    case ref_to_listener_pid(Ref) of
        false ->
            undefined;
        ListenerPid ->
            cowboy_listener:get_port(ListenerPid)
    end.

-spec ref_to_listener_pid(any()) -> pid().
ref_to_listener_pid(Ref) ->
	Children = supervisor:which_children(refuge_web_sup),
    io:format("children", []),
	{_, ListenerSupPid, _, _} = lists:keyfind(
		{cowboy_listener_sup, Ref}, 1, Children),
	ListenerSupChildren = supervisor:which_children(ListenerSupPid),
	{_, ListenerPid, _, _} = lists:keyfind(
		cowboy_listener, 1, ListenerSupChildren),
	ListenerPid.
