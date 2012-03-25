-module(refuge_console).

-export([makecert/1]).

makecert(Path) ->
    application:start(crypto),
    application:start(public_key),

    case refuge_cert:make_cert(Path) of
        ok ->
            io:format("SSL Certificate created:\n", []),
            io:format("Private Key: ~p\n", [filename:join(Path,
                                                          "refuge.key")]),
            io:format("x509 Certificate: ~p\n", [filename:join(Path,
                                                               "refuge.crt")]);
        {error, Error} ->
            io:format("error: ~p.\n", [Error]),
            halt(1)
    end.
