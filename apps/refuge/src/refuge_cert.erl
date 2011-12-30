%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of refuge released under the Apache license 2.
%% See the NOTICE for more information.
%%
%% @author Benoît Chesneau <benoitc@refuge.io>
%% @doc module in charge of dnssd registration of refuge.


-module(refuge_cert).

-export([make_cert/0, make_cert/2, make_cert/3,
         cert_to_binary/1, write_pem/3,
         gen_dsa/2]).

-include_lib("public_key/include/public_key.hrl").


make_cert() ->
    make_cert(128, 20). %% Bytes i.e. {1024, 160}

make_cert(LSize, NSize) ->
    User = refuge_util:user(),
    Opts = [{name, User}, {org, "refuge.io"}, {org_unit, "refuge"}],
    make_cert(LSize, NSize, Opts).

make_cert(LSize, NSize, Opts) ->
    SubjectPrivateKey = gen_dsa2(LSize,NSize),
    {TBSCert, IssuerKey} = make_tbs(SubjectPrivateKey, Opts),
    Cert = public_key:pkix_sign(TBSCert, IssuerKey),
    true = verify_signature(Cert, IssuerKey), %% verify that the keys where ok
    {Cert, encode_key(SubjectPrivateKey)}.

cert_to_binary({Cert, Key = {_,_,not_encrypted}})
        when is_binary(Cert) ->
    PemBin = public_key:pem_encode([{'Certificate', Cert,
                not_encrypted}]),
    KeyPemBin = public_key:pem_encode([Key]),
    {PemBin, KeyPemBin}.


%%--------------------------------------------------------------------
%% @doc Writes pem files in Dir with FileName ++ ".pem" and FileName ++ "_key.pem"
%% @spec (::string(), ::string(), {Cert,Key}) -> ok
%% @end
%%--------------------------------------------------------------------
write_pem(Dir, FileName, {Cert, Key = {_,_,not_encrypted}}) when is_binary(Cert) ->
    ok = der_to_pem(filename:join(Dir, FileName ++ ".pem"),
			       [{'Certificate', Cert, not_encrypted}]),
    ok = der_to_pem(filename:join(Dir, FileName ++ "_key.pem"), [Key]).


%%--------------------------------------------------------------------
%% @doc Creates a dsa key (OBS: for testing only)
%%   the sizes are in bytes
%% @spec (::integer()) -> {::atom(), ::binary(), ::opaque()}
%% @end
%%--------------------------------------------------------------------
gen_dsa(LSize,NSize) when is_integer(LSize), is_integer(NSize) ->
    Key = gen_dsa2(LSize, NSize),
    {Key, encode_key(Key)}.


%%--------------------------------------------------------------------
%% @doc Verifies cert signatures
%% @spec (::binary(), ::tuple()) -> ::boolean()
%% @end
%%--------------------------------------------------------------------
verify_signature(DerEncodedCert, #'DSAPrivateKey'{p=P, q=Q, g=G, y=Y}) ->
    public_key:pkix_verify(DerEncodedCert, {Y, #'Dss-Parms'{p=P, q=Q, g=G}}).

encode_key(Key = #'DSAPrivateKey'{}) ->
    {ok, Der} = 'OTP-PUB-KEY':encode('DSAPrivateKey', Key),
    {'DSAPrivateKey', list_to_binary(Der), not_encrypted}.

make_tbs(SubjectKey, Opts) ->
    Version = list_to_atom("v"++integer_to_list(proplists:get_value(version, Opts, 3))),

    Issuer = subject(Opts),
    {Algo, Parameters} = sign_algorithm(SubjectKey),

    SignAlgo = #'SignatureAlgorithm'{algorithm  = Algo,
				     parameters = Parameters},

    {#'OTPTBSCertificate'{serialNumber = trunc(random:uniform()*100000000)*10000 + 1,
			  signature    = SignAlgo,
			  issuer       = Issuer,
			  validity     = validity(Opts),
			  subject      = Issuer,
			  subjectPublicKeyInfo = publickey(SubjectKey),
			  version      = Version,
			  extensions   = asn1_NOVALUE
			 }, SubjectKey}.


validity(Opts) ->
    DefFrom0 = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())-1),
    DefTo0   = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())+7),
    {DefFrom, DefTo} = proplists:get_value(validity, Opts, {DefFrom0, DefTo0}),
    Format = fun({Y,M,D}) -> lists:flatten(io_lib:format("~w~2..0w~2..0w000000Z",[Y,M,D])) end,
    #'Validity'{notBefore={generalTime, Format(DefFrom)},
		notAfter ={generalTime, Format(DefTo)}}.

sign_algorithm(#'DSAPrivateKey'{p=P, q=Q, g=G}) ->
    {?'id-dsa-with-sha1', {params,#'Dss-Parms'{p=P, q=Q, g=G}}}.

publickey(#'DSAPrivateKey'{p=P, q=Q, g=G, y=Y}) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-dsa',
				 parameters={params, #'Dss-Parms'{p=P, q=Q, g=G}}},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo, subjectPublicKey = Y}.


subject(Opts) ->
    Encode = fun(Opt) ->
		     {Type,Value} = subject_enc(Opt),
		     [#'AttributeTypeAndValue'{type=Type, value=Value}]
	     end,
    {rdnSequence, [Encode(Opt) || Opt <- Opts]}.

%% Fill in the blanks
subject_enc({name,  Name}) ->       {?'id-at-commonName', {printableString, Name}};
subject_enc({email, Email}) ->      {?'id-emailAddress', Email};
subject_enc({city,  City}) ->       {?'id-at-localityName', {printableString, City}};
subject_enc({state, State}) ->      {?'id-at-stateOrProvinceName', {printableString, State}};
subject_enc({org, Org}) ->          {?'id-at-organizationName', {printableString, Org}};
subject_enc({org_unit, OrgUnit}) -> {?'id-at-organizationalUnitName', {printableString, OrgUnit}};
subject_enc({country, Country}) ->  {?'id-at-countryName', Country};
subject_enc({serial, Serial}) ->    {?'id-at-serialNumber', Serial};
subject_enc({title, Title}) ->      {?'id-at-title', {printableString, Title}};
subject_enc({dnQualifer, DnQ}) ->   {?'id-at-dnQualifier', DnQ};
subject_enc(Other) ->               Other.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DSA key generation  (OBS: for testing only)
%% See http://en.wikipedia.org/wiki/Digital_Signature_Algorithm
%% and the fips_186-3.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_dsa2(LSize, NSize) ->
    Q  = prime(NSize),  %% Choose N-bit prime Q
    X0 = prime(LSize),
    P0 = prime((LSize div 2) +1),

    %% Choose L-bit prime modulus P such that p–1 is a multiple of q.
    case dsa_search(X0 div (2*Q*P0), P0, Q, 1000) of
	error ->
	    gen_dsa2(LSize, NSize);
	P ->
	    G = crypto:mod_exp(2, (P-1) div Q, P), % Choose G a number whose multiplicative order modulo p is q.
	    %%                 such that This may be done by setting g = h^(p–1)/q mod p, commonly h=2 is used.

	    X = prime(20),               %% Choose x by some random method, where 0 < x < q.
	    Y = crypto:mod_exp(G, X, P), %% Calculate y = g^x mod p.

	    #'DSAPrivateKey'{version=0, p=P, q=Q, g=G, y=Y, x=X}
    end.

%% See fips_186-3.pdf
dsa_search(T, P0, Q, Iter) when Iter > 0 ->
    P = 2*T*Q*P0 + 1,
    case is_prime(crypto:mpint(P), 50) of
	true -> P;
	false -> dsa_search(T+1, P0, Q, Iter-1)
    end;
dsa_search(_,_,_,_) ->
    error.


%%%%%%% Crypto Math %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prime(ByteSize) ->
    Rand = odd_rand(ByteSize),
    crypto:erlint(prime_odd(Rand, 0)).

prime_odd(Rand, N) ->
    case is_prime(Rand, 50) of
	true ->
	    Rand;
	false ->
	    NotPrime = crypto:erlint(Rand),
	    prime_odd(crypto:mpint(NotPrime+2), N+1)
    end.

%% see http://en.wikipedia.org/wiki/Fermat_primality_test
is_prime(_, 0) -> true;
is_prime(Candidate, Test) ->
    CoPrime = odd_rand(<<0,0,0,4, 10000:32>>, Candidate),
    case crypto:mod_exp(CoPrime, Candidate, Candidate) of
	CoPrime -> is_prime(Candidate, Test-1);
	_       -> false
    end.

odd_rand(Size) ->
    Min = 1 bsl (Size*8-1),
    Max = (1 bsl (Size*8))-1,
    odd_rand(crypto:mpint(Min), crypto:mpint(Max)).

odd_rand(Min,Max) ->
    Rand = <<Sz:32, _/binary>> = crypto:rand_uniform(Min,Max),
    BitSkip = (Sz+4)*8-1,
    case Rand of
	Odd  = <<_:BitSkip,  1:1>> -> Odd;
	Even = <<_:BitSkip,  0:1>> ->
	    crypto:mpint(crypto:erlint(Even)+1)
    end.

pem_to_der(File) ->
    {ok, PemBin} = file:read_file(File),
    public_key:pem_decode(PemBin).

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).
