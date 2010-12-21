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
-module(evum).
-behaviour(gen_server).

-include("evum.hrl").
-include_lib("kernel/include/inet.hrl").

-define(SERVER, ?MODULE).
-define(PROGNAME, "priv/linux").

-export([ping/2, sniff/1, ifconfig/2]).
-export([start/0, start/1, start/2, stop/1]).
-export([send/2, send/3, puts/2, system/2, reboot/1]).
-export([umid/1, make_args/1, defaults/0]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {
    uml_dir = ?UML_DIR,
    cons,
    pid,
    port
}).

-define(DEFAULT_BOOT_OPTIONS, [
        net,
        {mem, 64},
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
    send(Ref, "ping -c 1 " ++ inet_parse:ntoa(IP)).

sniff(Ref) ->
    send(Ref, "tcpdump -s 0 -w /tmp/sniff.out -i eth0 > /dev/null 2>&1 &").


defaults() -> ?DEFAULT_BOOT_OPTIONS.

start() ->
    Pid = self(),
    start_link(Pid, defaults()).
start(Options) ->
    start_link(self(), Options).
start(Pid, Options) when is_pid(Pid), is_list(Options) ->
    start_link(Pid, Options).

stop(Ref) ->
    gen_server:call(Ref, stop).

reboot(Ref) ->
    system(Ref, <<"reboot">>).

system(Ref, Command) when is_list(Command) ->
    system(Ref, list_to_binary(Command));
system(Ref, Command) when is_binary(Command) ->
    gen_server:call(Ref, {system, Command}, infinity).

send(Ref, Data) when is_list(Data); is_binary(Data) ->
    send(Ref, Data, infinity).
send(Ref, Data, Timeout) when is_list(Data); is_binary(Data) ->
    case gen_server:call(Ref, {send, list_to_binary([Data, "\n"])}, infinity) of
        ok ->
            receive
                N -> N
            after
                Timeout -> timeout
            end;
        Error -> Error
    end.

puts(Ref, Data) when is_list(Data); is_binary(Data) ->
    gen_server:call(Ref, {send, Data}, infinity).

start_link(Pid, Options) ->
    {ok, Ref} = gen_server:start_link(?MODULE, [Pid, Options], []),
    boot(Ref, Options),
    {ok,Ref}.

init([Pid, Options]) ->
    process_flag(trap_exit, true),
    Default = #state{},
    UmlId = umid(Pid),

    % Start network
    {ok,_} = evum_data:start(),
    {ok,_} = evum_ctl:start(),

    Args = case proplists:get_value(net, Options, false) of
        false -> Options;
        true ->
            [Dev] = packet:default_interface(),
            {ok,[{hwaddr, HW}]} = inet:ifget(Dev, [hwaddr]),
            MAC = lists:flatten(string:join([ io_lib:format("~.16B", [N]) || N <- HW ], ":")),
            [{eth, 0, ?UML_DIR ++ "/evum.ctl", MAC}] ++ Options
    end,
    Cmd = make_args([
            {umid, UmlId},
            {uml_dir, Default#state.uml_dir}
        ] ++ Args),
    Port = open_port({spawn, Cmd}, [
            {line, 2048},
            binary,
            stderr_to_stdout,
            exit_status
        ]),
    {ok, Cons} = evum_mcons:open(Default#state.uml_dir ++ "/" ++
        UmlId ++ "/mconsole"),
    {ok, #state{
            cons = Cons,
            pid = Pid,
            port = Port
        }}.


handle_call({send, Data}, {Pid,_}, #state{pid = Pid, port = Port} = State) ->
    Reply = case erlang:port_command(Port, Data) of
        true -> ok;
        Error -> Error
    end,
    {reply, Reply, State};

handle_call({system, Command}, {Pid,_}, #state{pid = Pid, cons = Cons} = State) ->
    Reply = evum_mcons:send(Cons, Command),
    {reply, Reply, State};
handle_call(stop, {Pid,_}, #state{pid = Pid} = State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{cons = Cons, port = Port}) ->
    ok = evum_mcons:send(Cons, <<"halt">>),
    error_logger:info_report([{terminate, closed}]),
    evum_mcons:close(Cons),
    try erlang:port_close(Port) of
        true -> ok
    catch
        _:_ -> ok
    end.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Port communication
%%--------------------------------------------------------------------
handle_info({Port, {data, {Eol, Data}}}, #state{port = Port, pid = Pid} = State) when Eol == eol; Eol == noeol ->
    Pid ! Data,
    {noreply, State};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) when Status > 128 ->
    {stop, {port_terminated, Status-128}, State};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, {port_terminated, Status}, #state{port = Port} = State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.


%%--------------------------------------------------------------------
%%% Boot up
%%--------------------------------------------------------------------
boot(Ref, _Options) ->
    send(Ref, <<"mount -t proc /proc /proc">>),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
make_args(PL) ->
    proplists:get_value(progname, PL, ?PROGNAME) ++ " " ++
    string:join([ get_switch(proplists:lookup(Arg, PL)) || Arg <- [
                disk,
                eth,
                init,
                mem,
                rootfstype,
                umid,
                uml_dir,
                write
            ], proplists:lookup(Arg, PL) /= none ],
        " ").

get_switch({disk, Dev, Image})              -> Dev ++ "=" ++ Image;
get_switch({disk, Dev, Image, Cow})         -> Dev ++ "=" ++ Cow ++ "," ++ Image;

% eth[0-9]+=daemon,ethernet_address,type,control_socket,data_socket
% eth0=daemon,,unix,/var/run/uml-utilities/uml_switch.ctl
get_switch({eth, Dev, Path})                -> get_switch({eth, Dev, Path, ""});
get_switch({eth, Dev, Path, MAC})           ->
    "eth" ++ integer_to_list(Dev) ++ "=daemon," ++ MAC ++ ",unix," ++ Path;

get_switch({init, Arg})                     -> "init=" ++ Arg;

get_switch({mem, Arg}) when is_integer(Arg) -> "mem=" ++ integer_to_list(Arg) ++ "M";
get_switch({mem, Arg}) when is_list(Arg)    -> "mem=" ++ Arg;

get_switch({rootfstype, Arg})               -> "rootfstype=" ++ Arg;

get_switch({umid, Arg})                     -> "umid=" ++ Arg;

get_switch({uml_dir, Arg})                  -> "uml_dir=" ++ Arg;

get_switch({verbose, true})                 -> "";
get_switch({verbose, false})                -> "quiet";

get_switch({write, true})                   -> "rw".


umid(Pid) when is_pid(Pid) ->
    {match,[A,B,C]} = re:run(pid_to_list(Pid), "<(\\d+)\.(\\d+)\.(\\d+)>", [{capture, [1,2,3], list}]),
    "pid-" ++ os:getpid() ++ "-" ++ A ++ "-" ++ B ++ "-" ++ C.


