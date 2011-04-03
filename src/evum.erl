%% Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
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
-module(evum).

-include_lib("kernel/include/inet.hrl").

-export([ping/2, sniff/1, ifconfig/2]).
-export([start/0, start/1, start/2, stop/1, start_link/2, start_link/3]).
-export([send/2, send/3, puts/2, system/2, reboot/1]).
-export([defaults/0]).

-define(DEFAULT_BOOT_OPTIONS, [
        net,
        {mem, 64},
        {root, "/dev/root"},
        {rootflags, "/"},
        {rootfstype, "hostfs"},
        {verbose, false},
        write,
        {init, "/bin/bash"}
    ]).


ifconfig(Ref, {IP, Netmask, Default}) ->
    send(Ref, "ifconfig eth0 " ++ IP ++ " netmask " ++ Netmask),
    send(Ref, "route add default gw " ++ Default).


ping(Ref, Hostname) when is_list(Hostname) ->
    Addr = case inet_res:gethostbyname(Hostname) of
        {ok, #hostent{h_addr_list = [IP|_IPs]}} -> IP;
        _ -> throw(badarg)
    end,
    ping(Ref, Addr);
ping(Ref, {A,B,C,D} = IP) when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    send(Ref, "ping -w 5 -c 1 " ++ inet_parse:ntoa(IP)).


sniff(Ref) ->
    send(Ref, "tcpdump -s 0 -w /tmp/sniff.out -i eth0 > /dev/null 2>&1 &").


defaults() -> ?DEFAULT_BOOT_OPTIONS.


start() ->
    Pid = self(),
    start(Pid, defaults()).
start(Options) ->
    start(self(), Options).
start(Pid, Options) ->
    start_link(Pid, Options).

start_link(Pid, Options) when is_pid(Pid), is_list(Options) ->
    evum_srv:start_link([Pid, Options]).
start_link(Pid, Label, Options) when is_pid(Pid), is_list(Options) ->
    evum_srv:start_link([Pid, Label, Options]).

stop(Ref) ->
    gen_server:call(Ref, stop).

reboot(Ref) ->
    system(Ref, <<"reboot">>).

system(Ref, Command) when is_list(Command) ->
    system(Ref, list_to_binary(Command));
system(Ref, Command) when is_binary(Command) ->
    gen_server:call(Ref, {system, Command}, infinity).

send(Ref, Data) ->
    send(Ref, Data, infinity).
send(Ref, Data, Timeout) when ( is_list(Data) orelse is_binary(Data) ) ->
    case gen_server:call(Ref, {send, list_to_binary([Data, "\n"])}, infinity) of
        ok ->
            receive
                N -> N
            after
                Timeout -> timeout
            end;
        Error -> Error
    end.

puts(Ref, Data) when ( is_list(Data) orelse is_binary(Data) ) ->
    gen_server:call(Ref, {send, Data}, infinity).
