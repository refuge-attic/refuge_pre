-module(refuge_ident).

-export([make_cert/1]).

-record(dn, {commonName, organizationName = "refuge"}).


make_cert(Id) ->
    Etc = couch:get_app_env(config_dir,
                            filename:join([code:root_dir(), "./etc"])),

    Cnf = req_cnf(#dn{commonName = Id}),
    CnfFile = filename:join(Etc, "refuge.cnf"),
    file:write_file(CnfFile, Cnf),

    KeyFile = filename:join(Etc, "refuge.key"),
    CsrFile = filename:join(Etc, "refuge.csr"),
    CertFile = filename:join(Etc, "refuge.crt"),

    Commands = [
        ["genrsa", "-out", KeyFile, "2048"],
        ["req", "-new", "-key", KeyFile, "-out", CsrFile, "-config", CnfFile],
        ["x509", "-req", "-days", "14600", "-in", CsrFile,
         "-signkey", KeyFile, "-out", CertFile]],

    case apply_openssl_commands(Commands) of
        ok ->
            couch_config:set("identity", "key_file", KeyFile, true),
            couch_config:set("identity", "cert_file", CertFile, true),
            decode_key(KeyFile);
        Error ->
            Error
    end.

%% private
%%

decode_key(PemFile) ->
    {ok, PemBin} = file:read_file(PemFile),
    [Entry] = public_key:pem_decode(PemBin),
    {'RSAPrivateKey', 'two-prime', N , E, D, _P, _Q, _E1, _E2, _C,
        _Other} = public_key:pem_entry_decode(Entry),
    PrivKey = [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)],
    PubKey = [crypto:mpint(E), crypto:mpint(N)],
    {PrivKey, PubKey}.

apply_openssl_commands([]) ->
    ok;
apply_openssl_commands([Args | Rest]) ->
    case openssl_cmd(Args) of
        {ok, _} ->
            apply_openssl_commands(Rest);
        Error ->
            Error
    end.

openssl_cmd(Args) ->
    case refuge_util:find_executable("openssl") of
        false ->
            {error, openssl_notfound};
        Cmd ->
            refuge_util:sh(Cmd, [{args, Args}])
    end.


req_cnf(DN) ->
    ["# refuge generated config\n"
     "\n"

     "[req]\n"
     "default_bits=1024\n"
     "encrypt_key=no\n"
     "default_md=sha1\n"
     "#string_mask=pkix\n"
     "x509_extensions=ca_ext\n"
     "prompt=no\n"
     "distinguished_name=name\n"
     "\n"

     "[name]\n"
     "commonName=", DN#dn.commonName, "\n"
     "organizationName=", DN#dn.organizationName, "\n",
     "\n"

     "[ca_ext]\n"
     "basicConstraints=CA:FALSE\n"
     "subjectKeyIdentifier=hash\n"
     "authorityKeyIdentifier=keyid,issuer:always\n"
     "\n"
    ].
