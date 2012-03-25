-module(refuge_cert).


-include_lib("public_key/include/public_key.hrl").

-export([make_cert/1, make_cert/2,
         pkeys/1,
         publickey/1,
         fingerprint/1]).

-record(dn, {commonName, organizationName = "refuge"}).

make_cert(Path) ->
    make_cert(1024, Path).

make_cert(NbBytes, Path) ->
    Cnf = req_cnf(#dn{commonName = refuge_util:new_id()}),
    CnfFile = filename:join(Path, "refuge.cnf"),
    file:write_file(CnfFile, Cnf),

    KeyFile = filename:join(Path, "refuge.key"),
    CsrFile = filename:join(Path, "refuge.csr"),
    CertFile = filename:join(Path, "refuge.crt"),

    Commands = [
        ["genrsa", "-out", KeyFile, integer_to_list(NbBytes)],
        ["req", "-new", "-key", KeyFile, "-out", CsrFile, "-config", CnfFile],
        ["x509", "-req", "-days", "14600", "-in", CsrFile,
         "-signkey", KeyFile, "-out", CertFile]],

    apply_openssl_commands(Commands).

%% @doc return Public & Private keys needed for crypto RSA functions
pkeys(PemFile) ->
    {ok, PemBin} = file:read_file(PemFile),
    [Entry|_] = public_key:pem_decode(PemBin),
    {'RSAPrivateKey', 'two-prime', N , E, D, _P, _Q, _E1, _E2, _C,
        _Other} = public_key:pem_entry_decode(Entry),

    PrivKey = [crypto:mpint(E), crypto:mpint(N), crypto:mpint(D)],
    PubKey = [crypto:mpint(E), crypto:mpint(N)],
    {PrivKey, PubKey}.

%% @doc return a public key entry
publickey(KeyFile) when is_list(KeyFile) ->
    {ok, PemBin} = file:read_file(KeyFile),
    [Entry|_] = public_key:pem_decode(PemBin),
    publickey(Entry);
publickey({'RSAPrivateKey', _, _}=Entry) ->
    publickey(public_key:pem_entry_decode(Entry));
publickey({'RSAPrivateKey', 'two-prime', N , E, _D, _P, _Q, _E1, _E2, _C,
        _Other}) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    public_key:pem_entry_encode('RSAPublicKey', Public).

%% @doc return the fingerprint of a certfile.
fingerprint(CertFile) when is_list(CertFile) ->
    {ok, PemBin} = file:read_file(CertFile),
    [CertEntry|_] = public_key:pem_decode(PemBin),
    fingerprint(CertEntry);
fingerprint({'Certificate', _, _}=CertEntry) ->
    fingerprint(public_key:pem_entry_decode(CertEntry));
fingerprint(Cert) ->
    Digest = crypto:sha(public_key:pkix_encode('Certificate', Cert,
                                               plain)),
    couch_util:to_hex(Digest).

%% private
%%
%%
%%

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
     "\n"
    ].

