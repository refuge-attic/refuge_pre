-module(refuge_util).

-export([sh/1, sh/2,
         find_executable/1]).


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
