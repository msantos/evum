%% Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(evm_cfg).

-export([
        path/0,
        config/1,

        network/1,
        image/1,

        val/2,
        net/2,
        dist/2
    ]).

-define(CONFIG, "evm.cfg").


path() ->
    filename:join([
            filename:dirname(code:which(?MODULE)),
            "..",
            "priv",
            ?CONFIG
        ]).

config(Key) when is_atom(Key) ->
    {ok, Cfg} = file:consult(path()),
    proplists:get_value(Key, Cfg).

network(Location) when is_atom(Location) ->
    proplists:get_value(Location, config(network)).

image(Dist) when is_atom(Dist) ->
    proplists:get_value(Dist, config(image)).

val(Type, {Key, Location}) when is_atom(Type), is_atom(Key), is_atom(Location) ->
    proplists:get_value(Key, ?MODULE:Type(Location)).

net(Key, Location) ->
    val(network, {Key, Location}).

dist(Key, Dist) ->
    val(image, {Key, Dist}).
