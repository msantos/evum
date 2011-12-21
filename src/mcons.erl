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
-module(mcons).
-include("evum.hrl").

-export([
        open/0, open/1,
        version/1, help/1, halt/1,
        reboot/1,
        config/2, config/3,
        remove/2,
        sysrq/2, cad/1, stop/1, go/1,
        log/2, proc/2
    ]).

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

open() ->
    evum_mcons:open().
open(Path) ->
    evum_mcons:open(Path).

version(Socket) ->
    evum_mcons:send(Socket, <<"version">>).
help(Socket) ->
    evum_mcons:send(Socket, <<"help">>).
halt(Socket) ->
    evum_mcons:send(Socket, <<"halt">>).
reboot(Socket) ->
    evum_mcons:send(Socket, <<"reboot">>).

cad(Socket) ->
    evum_mcons:send(Socket, <<"cad">>).
stop(Socket) ->
    evum_mcons:send(Socket, <<"stop">>).
go(Socket) ->
    evum_mcons:send(Socket, <<"go">>).

config(Socket, Dev) ->
    evum_mcons:send(Socket, <<"config ", Dev>>).
config(Socket, Dev, Config) ->
    evum_mcons:send(Socket, <<"config ", Dev, "=", Config>>).

remove(Socket, Config) ->
    evum_mcons:send(Socket, <<"remove ", Config>>).

sysrq(Socket, Config) ->
    evum_mcons:send(Socket, <<"sysrq ", Config>>).

log(Socket, Config) ->
    evum_mcons:send(Socket, <<"log ", Config>>).
proc(Socket, Config) ->
    evum_mcons:send(Socket, <<"proc ">>, Config).
