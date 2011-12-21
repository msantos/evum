%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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
-module(evm).

-export([
        args/1,
        create/1
    ]).
-export([
        check_image/1,
        name/1,
        path/1
    ]).

-define(LABEL, "default").
-define(DIST, buildroot).
-define(PROMPT_TIMEOUT, 10000).


args(Dist) ->
    evum_srv:make_args(evm_cfg:dist(args, Dist)).

create(Arg) when is_list(Arg) ->
    Label = proplists:get_value(label, Arg, ?LABEL),
    Dist = proplists:get_value(dist, Arg, ?DIST),
    Location = proplists:get_value(location, Arg),
    Timeout = proplists:get_value(timeout, Arg, ?PROMPT_TIMEOUT),
    Verbose = proplists:get_value(verbose, Arg, false),

    Image = check_image(Dist),
    start_evum(),
    {ok,Ref} = start_vm(Label, Image, Dist),

    wait_prompt(Timeout, Verbose),

    mount_proc(Ref),
    start_net(Ref, Location),
    {ok, Ref}.

wait_prompt(Timeout, Verbose) ->
    receive
        % buildroot
        <<"#", _/binary>> -> ok;
        Line ->
            case Verbose of
                true -> error_logger:info_report([{boot, Line}]);
                false -> ok
            end,
            wait_prompt(Timeout, Verbose)
    after
        % Random Linux system image
        Timeout ->
            error_logger:info_report([{prompt, timeout}, {timeout, Timeout}]),
            ok
    end.

start_evum() ->
    evum_sup:start_link(),
    Path = evum_ctl:socket(),
    case file:read_file_info(Path) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            error_logger:info_report([{starting, switch}]),
            {ok,Switch} = evum_switch:start(),
            evum_ctl:start(Switch)
    end.

start_vm(Label, Image, Dist) ->
    Arg = evm_cfg:dist(args, Dist),
    Cow = Image ++ ".cow-" ++ atom_to_list(Label) ++ "," ++ Image,
    Pid = self(),
    supervisor:start_child(evum_sup, [Pid, Label, Arg ++ [{ubd, Cow}]]).

mount_proc(Ref) ->
    evum:send(Ref, <<"mount -t proc /proc /proc">>).

start_net(_Ref, undefined) ->
    ok;
start_net(Ref, Location) ->
    Base = evm_cfg:net(base_ip, Location),
    First = evm_cfg:net(first_ip, Location),
    Mask = evm_cfg:net(netmask, Location),
    GW = evm_cfg:net(gw, Location),

    Ping = case evm_cfg:net(ping, Location) of
        undefined -> GW;
        IP -> IP
    end,

    evum:ifconfig(Ref, {
            Base ++ integer_to_list(First),
            Mask,
            GW
        }),

    evum:ping(Ref, Ping).


check_image(Dist) ->
    URI = evm_cfg:dist(uri, Dist),
    Path = path(filename:basename(URI)),
    File = path(evm_cfg:dist(file, Dist)),

    case file:read_file_info(File) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            Path1 = download_image(URI, Path),
            error_logger:info_report([{path, Path1}]),
            ok = file:rename(Path1, File)
    end,

    File.

download_image(URI, File) ->
    inets:start(),
    ssl:start(),
    {ok, _} = httpc:request(get, {URI, []}, [], [{stream, File}]),
    decompress_image(File).

decompress_image(File) ->
    decompress(name(File)).

decompress({File, ".tar"}) ->
    os:cmd("tar xf " ++ File ++ ".tar"),
    decompress(name(File));
decompress({File, ".bz2"}) ->
    os:cmd("bzip2 -d " ++ File ++ ".bz2"),
    decompress(name(File));
decompress({File, ".gz"}) ->
    os:cmd("gzip -d " ++ File ++ ".gz"),
    decompress(name(File));
decompress({File, Ext}) ->
    File ++ Ext.

name(File) ->
    {filename:rootname(File), filename:extension(File)}.

path(File) ->
    Path = filename:join([
            filename:dirname(
                code:which(?MODULE)),
                "..",
                "priv",
                "img",
                File
        ]),

    ok = filelib:ensure_dir(Path),
    Path.
