-module(refuge_util).

-export([sh/1, sh/2,
         find_executable/1,
         new_id/0,
         merge_proplists/2,
         get_app_env/2,
         wait/1,
         shutdown/1]).
-export([oauth_header/3]).
-export([get_value/2, get_value/3]).
-export([to_list/1, to_binary/1, to_integer/1, to_atom/1]).
-export([ssl_ip/0]).

sh(Command) ->
    sh(Command, []).

sh(Command, Options0) ->
    DefaultOptions = [collect_output, return_on_error],
    Options = [make_sh_options(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],

    Port = open_port({spawn_executable, Command}, PortSettings),
    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            ErrorHandler(Command, Err)
    end.


find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            filename:nativename(Path)
    end.

new_id() ->
    Bin = crypto:sha(term_to_binary({make_ref(), os:timestamp()})),
    new_id(Bin).
new_id(Bin) when is_binary(Bin) ->
    << <<(new_id(I))>> || <<I:5>> <= Bin >>;
new_id(Int)
  when is_integer(Int) andalso Int >= 0 andalso Int =< 9 -> Int + 48;
new_id(Int)
  when is_integer(Int) andalso Int >= 10 andalso Int =< 31 -> Int + 87.

%% @doc Returns a proplist formed by merging OldProp and NewProp. If a key
%%      presents only in OldProp or NewProp, the tuple is picked. If a key
%%      presents in both OldProp and NewProp, the tuple from NewProp is
%%      picked.
%% @end
-spec merge_proplists(proplists:proplist(), proplists:proplist()) ->
    proplists:proplist().
merge_proplists(OldProp, NewProp) ->
    lists:ukeymerge(1, lists:ukeysort(1, NewProp), lists:ukeysort(1, OldProp)).


get_app_env(Env, Default) ->
    case application:get_env(refuge, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

%% @doc emulate proplists:get_value/2,3 but use faster lists:keyfind/3
-spec(get_value/2 :: (Key :: term(), Prop :: [term()] ) -> term()).
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

-spec(get_value/3 :: (Key :: term(), Prop :: [term()], Default :: term() ) -> term()).
get_value(Key, Prop, Default) ->
    case lists:keyfind(Key, 1, Prop) of
	false ->
	    case lists:member(Key, Prop) of
		true -> true;
		false -> Default
	    end;
	{Key, V} -> % only return V if a two-tuple is found
	    V;
	Other when is_tuple(Other) -> % otherwise return the default
	    Default
    end.

% build oauth header
oauth_header(Url, Action, OauthProps) ->
    {_, _, _, QS, _} = mochiweb_util:urlsplit(Url),
    QSL = mochiweb_util:parse_qs(QS),

    % get oauth paramerers
    ConsumerKey = to_list(get_value(consumer_key, OauthProps)),
    Token = to_list(get_value(token, OauthProps)),
    TokenSecret = to_list(get_value(token_secret, OauthProps)),
    ConsumerSecret = to_list(get_value(consumer_secret, OauthProps)),
    SignatureMethodStr = to_list(get_value(signature_method,
            OauthProps, "HMAC-SHA1")),

    SignatureMethodAtom = case SignatureMethodStr of
        "PLAINTEXT" ->
            plaintext;
        "HMAC-SHA1" ->
            hmac_sha1;
        "RSA-SHA1" ->
            rsa_sha1
    end,
    Consumer = {ConsumerKey, ConsumerSecret, SignatureMethodAtom},
    Method = case Action of
        delete -> "DELETE";
        get -> "GET";
        post -> "POST";
        put -> "PUT";
        head -> "HEAD"
    end,
    Params = oauth:signed_params(Method, Url, QSL, Consumer, Token, TokenSecret)
    -- QSL,
    {"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}.

ssl_ip() ->
    {ok, Ip} = inet_parse:address(
            couch_config:get("httpd", "bind_address", "0.0.0.0")
    ),
    parse_ip([Ip]).


parse_ip([]) ->
    nil;
parse_ip([{0, 0, 0, 0}]) ->
    {ok, Ifs} = inet:getif(),
    parse_ip([Ip || {Ip, _, _} <- Ifs]);
parse_ip([{192, 168, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip([{172, 16, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip([{10, _, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip([_|Rest]) ->
    parse_ip(Rest).

%% @doc Wait for a monitored process to exit
%% @end
-spec wait(reference() | pid) -> ok.
wait(MRef) when is_reference(MRef) ->
    receive {'DOWN', MRef, process, _, _} -> ok end;

wait(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    wait(MRef).

%% @doc Shutdown a child process
%% @end
-spec shutdown(pid()) -> ok.
shutdown(Pid) ->
    MRef = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    wait(MRef).

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _ ->
            list_to_binary(io_lib:format("~p", [V]))
    end;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    V.

to_integer(V) when is_integer(V) ->
    V;
to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:list_to_integer(binary_to_list(V)).

to_list(V) when is_list(V) ->
    V;
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) ->
    V.

to_atom(V) when is_atom(V) ->
    V;
to_atom(V) when is_list(V) ->
    list_to_atom(V);
to_atom(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V));
to_atom(V) ->
    list_to_atom(lists:flatten(io_lib:format("~p", [V]))).


%%% private

make_sh_options(return_on_error) ->
    {error_handler,
 fun(_Command, Err) ->
             {error, Err}
     end};
make_sh_options(collect_output) ->
    {output_handler,
     fun(Line, Acc) ->
             [Line | Acc]
     end};
make_sh_options({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
make_sh_options({env, _EnvArg} = Env) ->
    {port_settings, Env};
make_sh_options({args, _Arg} = Args) ->
    {port_settings, Args}.

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.
