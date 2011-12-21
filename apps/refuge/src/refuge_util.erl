%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of farmer released under the Apache license 2.
%% See the NOTICE for more information.
%%
%% @author Benoît Chesneau <benoitc@refuge.io>


-module(refuge_util).

-export([is_daemon/1]).

is_daemon(Name) when is_atom(Name) ->
    is_daemon(atom_to_list(Name));
is_daemon(Name) ->
    case couch_config:get("daemons", Name) of
        undefined -> false;
        _ -> true
    end.
