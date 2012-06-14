%% @doc module to handle the refuge couchapp

-module(refuge_couchapp).

-export([couchapp_from_fs/1, make_doc/1,
         save_couchapp/3]).

-include("refuge.hrl").
-include_lib("couch/include/couch_db.hrl").

save_couchapp(DbName, #couchapp{}=Couchapp, Options)->
    CreateDb = proplists:get_value(create_db, Options),
    ForceUpdate = proplists:get_value(force_update, Options, false),

    DbOptions = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
    case couch_db:open_int(DbName, DbOptions) of
        {ok, Db} ->
            maybe_save_couchapp(Db, Couchapp, ForceUpdate);
        _ when CreateDb =:= true ->
            {ok, Db} = couch_server:create(DbName, DbOptions),
            maybe_save_couchapp(Db, Couchapp, ForceUpdate);
        _ ->
            ok
    end;
save_couchapp(DbName, Path, Options) when is_binary(Path) ->
    save_couchapp(DbName, binary_to_list(Path), Options);
save_couchapp(DbName, Path, Options) ->
    {ok, Couchapp} = couchapp_from_fs(Path),
    save_couchapp(DbName, Couchapp, Options).

%% get a couchapp from the fs
couchapp_from_fs(Path) ->
    DocId = id_from_path(Path),
    AttDir = filename:join(Path, "_attachments"),

    %% get patterns to ignore
    IgnoreFile = filename:join(Path, ".ericaignore"),
    Ignore = case filelib:is_regular(IgnoreFile) of
        true ->
            load(IgnoreFile);
        _ ->
            []
    end,
    Ignore1 = Ignore ++ ?DEFAULT_IGNORE,

    %% initial couchapp record
    Couchapp = #couchapp{path=Path,
                         ignore=Ignore1,
                         att_dir=AttDir,
                         docid=DocId,
                         doc={[{<<"_id">>, DocId}]}},

    %% get the couchapp from the filesystem
    process_path(filelib:wildcard("*", Path), Path, Couchapp).


%% create final doc to save
make_doc(#couchapp{old_doc=nil}=Couchapp) ->
    make_doc(Couchapp#couchapp{old_doc={[]}});
make_doc(Couchapp) ->
    %% clean attachments and process signatures
    Couchapp2 = process_attachments(process_signatures(Couchapp)),

    #couchapp{
        doc = Doc,
        old_doc = {OldDocProps} = OldDoc,
        manifest = Manifest,
        signatures = Signatures} = Couchapp2,

    {DocProps1} = Doc1 = case OldDoc of
        {[]} ->
            Doc;
        _ ->
            DocRev = refuge_util:get_value(<<"_rev">>, OldDocProps),
            refuge_util:set_value(<<"_rev">>, DocRev, Doc)
    end,

    %% set manifest an signatures in couchapp object
    case refuge_util:get_value(<<"couchapp">>, DocProps1) of
        undefined ->
            refuge_util:set_value(<<"couchapp">>, {[
                        {<<"manifest">>, Manifest},
                        {<<"signatures">>, {Signatures}}
            ]}, Doc1);
        Meta ->
            Meta1 = refuge_util:set_value(<<"manifest">>, Manifest, Meta),
            FinalMeta = refuge_util:set_value(<<"signatures">>,
                {Signatures}, Meta1),
            refuge_util:set_value(<<"couchapp">>, FinalMeta, Doc1)
    end.


maybe_save_couchapp(Db, #couchapp{docid=DocId}=Couchapp, ForceUpdate) ->
    try
        case couch_db:open_doc(Db, DocId, [ejson_body]) of
            {ok, _Doc} when ForceUpdate /= true ->
                ok;
            {ok, Doc} ->
                Doc2 = couch_doc:to_json_obj(Doc, []),
                do_save_couchapp(Db, Couchapp#couchapp{old_doc=Doc2});
            _ ->
                do_save_couchapp(Db, Couchapp)
        end

    after
        catch couch_db:close(Db)
    end.

do_save_couchapp(Db, #couchapp{}=Couchapp) ->
    Doc = couch_doc:from_json_obj(make_doc(Couchapp)),
    case couch_db:update_doc(Db, Doc, []) of
        {ok, _NewRev} ->
            ok;
        Error ->
            Error
    end.

id_from_path(Path) ->
    IdFile = filename:join(Path, "_id"),
    case filelib:is_regular(IdFile) of
        true ->
            {ok, Bin} = file:read_file(IdFile),
            [Id|_] = binary:split(Bin, <<"\n">>, [trim]),
            Id;
        false ->
            Fname = list_to_binary(filename:basename(Path)),
            <<"_design/", Fname/binary>>
    end.

process_signatures(#couchapp{attachments=[]}=Couchapp) ->
    Couchapp;
process_signatures(#couchapp{att_dir=AttDir, doc=Doc, old_doc={OldDoc},
                             attachments=Atts}=Couchapp) ->

    Signatures = case refuge_util:get_value(<<"couchapp">>, OldDoc) of
        undefined ->
            [];
        {Meta} ->
            case refuge_util:get_value(<<"signatures">>, Meta) of
                undefined ->
                    %% not defined.
                    [];
                {Signatures1} ->
                    Signatures1
            end
    end,
    {Removed, NewAtts} = process_signatures1(Signatures, [], Atts,
        AttDir),

    NewSignatures = [{list_to_binary(refuge_util:relpath(F, AttDir)), S}
        || {F, S} <- Atts],

    {OldAtts} = refuge_util:get_value(<<"_attachments">>, OldDoc, {[]}),
    case Removed of
        [] ->
            Doc1 = refuge_util:set_value(<<"_attachments">>, {OldAtts},
                                         Doc),
            Couchapp#couchapp{
                doc=Doc1,
                attachments=NewAtts,
                signatures=NewSignatures
            };
        _Else ->
            OldAtts1 = clean_old_attachments(Removed, OldAtts),
            Doc1 = refuge_util:set_value(<<"_attachments">>, {OldAtts1},
                                         Doc),
            Couchapp#couchapp{
                doc=Doc1,
                attachments=NewAtts,
                signatures=NewSignatures
            }
    end.

process_attachments(#couchapp{att_dir=AttDir, doc=Doc,
        attachments=Atts}=Couchapp) ->
    NewDoc = attach_files(Atts, Doc, AttDir),
    Couchapp#couchapp{doc=NewDoc}.

clean_old_attachments([],OldAtts) ->
    OldAtts;
clean_old_attachments([F|Rest], OldAtts) ->
    OldAtts1 = proplists:delete(F, OldAtts),
    clean_old_attachments(Rest, OldAtts1).

process_signatures1([], Removed, Attachments, _AttDir) ->
    {Removed, Attachments};
process_signatures1([{F, S}|Rest], Removed, Attachments, AttDir) ->
    F1 = filename:join(AttDir, binary_to_list(F)),
    case proplists:get_value(F1, Attachments) of
        undefined ->
            process_signatures1(Rest, [F|Removed], Attachments, AttDir);
        S1 when S =:= S1 ->
            Attachments1 = proplists:delete(F1, Attachments),
            process_signatures1(Rest, Removed, Attachments1, AttDir);
        _S1 ->
            process_signatures1(Rest, [F|Removed], Attachments, AttDir)
    end.


attach_files([], Doc, _AttDir) ->
    Doc;
attach_files([{Fname, _Signature}|Rest], Doc, AttDir) ->
    {ok, Content} = file:read_file(Fname),
    RelPath = refuge_util:relpath(Fname, AttDir),
    Doc1 = add_inline(Doc, Content, encode_path(RelPath)),
    attach_files(Rest, Doc1, AttDir).

process_path([], _Dir, Couchapp) ->
    {ok, Couchapp};
process_path([".couchapprc"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([".ericaignore"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path(["_id"|Rest], Dir, Couchapp) ->
    process_path(Rest, Dir, Couchapp);
process_path([File|Rest], Dir, #couchapp{path=Path, ignore=Ignore,
                                         doc=Doc,
                                         manifest=Manifest}=Couchapp) ->
    Fname = filename:join(Dir, File),
    case ignore(Ignore, refuge_util:relpath(Fname, Path)) of
        true ->
            process_path(Rest, Dir, Couchapp);
        false ->
            File1 = list_to_binary(File),
            RelPath = list_to_binary(refuge_util:relpath(Fname, Path)),
            Couchapp1 = case filelib:is_dir(Fname) of
                true ->
                    case File1 of
                        <<"_attachments">> ->
                            attachments_from_fs(Couchapp);
                        <<"_", _/binary>> ->
                            Couchapp;
                        _ ->
                            Files = filelib:wildcard("*", Fname),
                            {SubDoc, SubManifest} = process_dir(Files,
                                Fname, Path, Ignore, {[]}, []),
                            Doc1 = refuge_util:set_value(File1, SubDoc,
                                Doc),
                            Manifest1 = [<<RelPath/binary, "/">>|Manifest]
                                ++ SubManifest,
                            Couchapp#couchapp{doc=Doc1, manifest=Manifest1}
                    end;
                false ->
                    {PropName, Value} = process_file(File, Fname),
                    Doc1 = refuge_util:set_value(PropName, Value, Doc),
                    Couchapp#couchapp{doc=Doc1, manifest=[RelPath|Manifest]}

            end,
            process_path(Rest, Dir, Couchapp1)
    end.

process_dir([], _Dir, _Path, _Ignore, Doc, Manifest) ->
    {Doc, Manifest};
process_dir([File|Rest], Dir, Path, Ignore, Doc, Manifest) ->
    Fname = filename:join(Dir, File),
    case ignore(Ignore, refuge_util:relpath(Fname, Path)) of
        true ->
            process_dir(Rest, Dir, Path, Ignore, Doc, Manifest);
        false ->
            File1 = list_to_binary(File),

            RelPath = list_to_binary(refuge_util:relpath(Fname, Path)),
            {Doc1, Manifest1} = case filelib:is_dir(Fname) of
                true ->
                    Files = filelib:wildcard("*", Fname),
                    {SubDoc, SubManifest} = process_dir(Files, Fname,
                        Path, Ignore, {[]}, []),
                    NewDoc = refuge_util:set_value(File1, SubDoc, Doc),
                    NewManifest = [<<RelPath/binary, "/">> | Manifest]
                                  ++ SubManifest,
                    {NewDoc, NewManifest};
                false ->
                    {PropName, Value} = process_file(File, Fname),
                    NewDoc = refuge_util:set_value(PropName, Value, Doc),
                    {NewDoc, [RelPath|Manifest]}
            end,
            process_dir(Rest, Dir, Path, Ignore, Doc1, Manifest1)
    end.


process_file("language", FName) ->
    {ok, Bin} = file:read_file(FName),
    [Value|_] = binary:split(Bin, <<"\n">>, [trim]),
    {<<"language">>, Value};
process_file(File, Fname) ->
    case filename:extension(Fname) of
        [] ->
            {ok, Value} = file:read_file(Fname),
            {list_to_binary(File), Value};
        Ext ->
            {ok, Bin} = file:read_file(Fname),
            Value = case Ext of
                ".json" ->
                    jiffy:decode(Bin);
                _ ->
                    Bin
            end,
            PropName = filename:basename(Fname, Ext),
            {list_to_binary(PropName), Value}
    end.

attachments_from_fs(#couchapp{path=Path}=Couchapp) ->
    AttPath = filename:join(Path, "_attachments"),
    Files = filelib:wildcard("*", AttPath),
    Attachments = attachments_from_fs1(Files, AttPath, Couchapp, []),
    Couchapp#couchapp{attachments=Attachments}.

attachments_from_fs1([], _Dir, _Couchapp, Att) ->
    Att;
attachments_from_fs1([F|R], Dir, #couchapp{path=Root,
                                           ignore=Ignore}=Couchapp,
                     Att) ->
    Path = filename:join(Dir, F),
    case ignore(Ignore, refuge_util:relpath(Path, Root)) of
        true ->
            attachments_from_fs1(R, Dir, Couchapp, Att);
        false ->
            Att1 = case filelib:is_dir(Path) of
                       true ->
                           Files = filelib:wildcard("*", Path),
                           SubAtt = attachments_from_fs1(Files, Path, Couchapp, []),
                           Att ++ SubAtt;
                       false ->
                           {ok, Md5} = refuge_util:md5_file(Path),
                           Md5Hash = lists:flatten([io_lib:format("~.16b",[N])
                                                    || N <-binary_to_list(Md5)]),
                           [{Path, list_to_binary(Md5Hash)}|Att]
                   end,
            attachments_from_fs1(R, Dir, Couchapp, Att1)
    end.

is_utf8(S) ->
    try lists:all(fun(C) -> xmerl_ucs:is_incharset(C, 'utf-8') end, S)
    catch
        exit:{ucs, {bad_utf8_character_code}} -> false
    end.

encode_path(P) ->
    case is_utf8(P) of
        true ->
            P;
        false ->
            Parts = lists:foldl(fun(P1, Acc) ->
                    [mochiweb_util:quote_plus(P1)|Acc]
                end, [], string:tokens(P, "/")),
            string:join(lists:reverse(Parts), "/")
    end.

%% @doc add attachment  to a doc and encode it. Give possibility to send attachments inline.
add_inline(Doc, Content, AName) ->
    ContentType = mochiweb_util:guess_mime(AName),
    add_inline(Doc, Content, AName, ContentType).


%% @doc add attachment  to a doc and encode it with ContentType fixed.
add_inline(Doc, Content, AName, ContentType) ->
    {Props} = Doc,
    Data = base64:encode(Content),
    Attachment = {refuge_util:to_binary(AName), {[
                    {<<"content_type">>, refuge_util:to_binary(ContentType)},
                    {<<"data">>, Data}
                    ]}
                 },

    Attachments1 = case refuge_util:get_value(<<"_attachments">>, Props) of
        undefined ->
            [Attachment];
        {Attachments} ->
            case set_attachment(Attachments, [], Attachment) of
                notfound ->
                    [Attachment|Attachments];
                A ->
                    A
                end
        end,
    refuge_util:set_value(<<"_attachments">>, {Attachments1}, Doc).

set_attachment(Attachments, NewAttachments, Attachment) ->
    set_attachment(Attachments, NewAttachments, Attachment, false).
set_attachment([], Attachments, _Attachment, Found) ->
    case Found of
        true ->
            Attachments;
        false ->
            notfound
        end;
set_attachment([{Name, V}|T], Attachments, Attachment, Found) ->
    {AName, _} = Attachment,
    {Attachment1, Found1} = if
        Name =:= AName, Found =:= false ->
            {Attachment, true};
        true ->
            {{Name, V}, Found}
        end,
    set_attachment(T, [Attachment1|Attachments], Attachment, Found1).


load(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Bin1 = remove_comments(Bin),
            jiffy:decode(Bin1);
        Error ->
            lager:error("can't read '~p' [~p]~n", [File, Error]),
            []
    end.

ignore([], _Path) ->
    false;
ignore([Pattern|Rest], Path) ->
    case re:run(Path, Pattern, [global, caseless, unicode, multiline,
                {capture, all, binary}]) of
            nomatch ->
                ignore(Rest, Path);
            _ ->
                lager:debug("File '~p' ignored.~n", [Path]),
                true
    end.

remove_comments(Content) ->
    P = "(?:/\\*(?:[^*]|(?:\\*+[^*/]))*\\*+/)|(?://.*)",
    re:replace(Content, P, "").
