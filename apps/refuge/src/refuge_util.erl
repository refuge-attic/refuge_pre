%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of farmer released under the Apache license 2.
%% See the NOTICE for more information.
%%
%% @author Benoît Chesneau <benoitc@refuge.io>


-module(refuge_util).

-export([is_daemon/1, new_id/0, node_id/0]).

is_daemon(Name) when is_atom(Name) ->
    is_daemon(atom_to_list(Name));
is_daemon(Name) ->
    case couch_config:get("daemons", Name) of
        undefined -> false;
        _ -> true
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

node_id() ->
    case couch_config:get("refuge", "nodeid") of
        undefined ->
            NodeId = new_id(),
            couch_config:set("refuge", "nodeid",
                binary_to_list(NodeId)),
            NodeId;
        NodeId when length(NodeId) > 63 ->
            throw({invalid_id, <<"Node Id length > 63">>});
        NodeId ->
            list_to_binary(NodeId)
    end.

