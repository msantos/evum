%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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
-module(evum_mcons).
-include("procket.hrl").
-include("evum.hrl").
-export([
        open/0, open/1, close/1,
        help/1,
        send/2,
        cmd/1, response/1,
        write/2, read/1
    ]).

-define(MCONSOLE_MAGIC, 16#cafebabe).
-define(MCONSOLE_MAX_DATA, 512).
-define(MCONSOLE_VERSION, 2).

-record(socket, {
        s,
        sun
    }).


open() ->
    case filelib:wildcard("*/mconsole", ?UML_DIR) of
        [] -> none;
        [Console|_] -> open(?UML_DIR ++ "/" ++ Console)
    end.

open(Path) when is_list(Path) ->
    open(list_to_binary(Path));
open(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_DGRAM, 0),
    Pid = list_to_binary(string:right(os:getpid(), 5)),
    {_,_,USec} = erlang:now(),
    Sun = <<?PF_LOCAL:16/native,        % sun_family
            0:8,                        % abstract address
            Pid/binary,                 % address
            USec:32,                    % timestamp: to allow
                                        %  multiple connects from 1 VM
            0:((?UNIX_PATH_MAX-(1+byte_size(Pid)+4))*8)
            >>,
    ok = procket:bind(Socket, Sun),
    {ok, #socket{
            s = Socket,
            sun = <<?PF_LOCAL:16/native,
            Path/binary,
            0:((?UNIX_PATH_MAX-byte_size(Path))*8)
            >>
        }}.

close(#socket{s = Socket}) ->
    procket:close(Socket).

help(Socket) ->
    {0, Output} = send(Socket, <<"help">>),
    io:format("~s", [Output]).


% version - Get kernel version 
% help - Print this message 
% halt - Halt UML 
% reboot - Reboot UML 
% config <dev>=<config> - Add a new device to UML;  
% same syntax as command line 
% config <dev> - Query the configuration of a device 
% remove <dev> - Remove a device from UML 
% sysrq <letter> - Performs the SysRq action controlled by the letter 
% cad - invoke the Ctrl-Alt-Del handler 
% stop - pause the UML; it will do nothing until it receives a 'go' 
% go - continue the UML after a 'stop' 
% log <string> - make UML enter <string> into the kernel log
% proc <file> - returns the contents of the UML's /proc/<file>
send(Socket, Command) when is_list(Command) ->
    send(Socket, list_to_binary(Command));
send(#socket{} = Socket, Command) when is_binary(Command) ->
    case  write(Socket, cmd(Command)) of
        ok -> recv(Socket);
        Error -> Error
    end.


recv(Socket) ->
    recv(Socket, []).
recv(Socket, Acc) ->
    {ok, Response} = read(Socket),
    case response(Response) of
        {0, 0, _Len, <<0>>} ->
            ok;
        {0, 0, _Len, Data} ->
            {ok, list_to_binary(lists:reverse([binary:part(Data,0,byte_size(Data)-1)|Acc]))};
        {_Err, 0, _Len, Data} ->
            {error, list_to_binary(lists:reverse([binary:part(Data,0,byte_size(Data)-1)|Acc]))};
        {_Err, _More, _Len, Data} ->
            recv(Socket, [binary:part(Data,0,byte_size(Data)-1)|Acc])
    end.


write(#socket{s = FD, sun = Sun}, Cmd) when is_binary(Cmd) ->
    procket:sendto(FD, Cmd, 0, Sun).

read(Socket) ->
    read(Socket, 0).
read(#socket{s = FD} = Socket, X) ->
    case procket:recvfrom(FD, 4+4+4+?MCONSOLE_MAX_DATA) of
        {error, eagain} when X < 10 -> timer:sleep(10), read(Socket, X+1);
        {error, eagain} -> {error, eagain};
        {ok, _} = N -> N
    end.


% struct mconsole_request {
%         uint32_t magic;
%         uint32_t version;
%         uint32_t len;
%         char data[MCONSOLE_MAX_DATA];
% };
cmd(Cmd) when byte_size(Cmd) < 4+4+4+?MCONSOLE_MAX_DATA ->
    Size = byte_size(Cmd),

    <<?MCONSOLE_MAGIC:32/native,
    ?MCONSOLE_VERSION:32/native,
    Size:32/native,
    Cmd/bytes,
    0:((?MCONSOLE_MAX_DATA-Size)*8)
    >>.


% struct mconsole_reply {
%         uint32_t err;
%         uint32_t more;
%         uint32_t len;
%         char data[MCONSOLE_MAX_DATA];
% };
response(<<Err:4/unsigned-integer-unit:8, More:4/unsigned-integer-unit:8,
    Len:4/unsigned-integer-unit:8, Data/bytes>>) ->
    {Err, More, Len, Data}.


