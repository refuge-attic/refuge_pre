%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%
%% This file is part of refuge released under the Apache license 2.
%% See the NOTICE for more information.


-record(node, {
        id,
        name,
        host,
        port,
        type,
        last_seen}).
