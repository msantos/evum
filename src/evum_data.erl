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
-module(evum_data).
-behaviour(gen_server).

-include("epcap_net.hrl").
-include("procket.hrl").
-include("evum.hrl").

-export([start/0, start/1, stop/1]).
-export([name/0,data/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ETH_ALEN, 6).

-record(state, {
        uml_dir = ?UML_DIR,
        mac,
        ip,
        arp,
        ifindex,
        pid,
        sun,
        s
    }).


start() ->
    start_link(self()).
start(Pid) when is_pid(Pid) ->
    start_link(Pid).

stop(Ref) ->
    gen_server:call(Ref, stop).

name() ->
    gen_server:call(?SERVER, name).

data(net, Data) ->
    gen_server:call(?SERVER, {net, Data});
data(unix, {Sun, Data}) ->
    gen_server:call(?SERVER, {unix, Sun, Data}).

start_link(Pid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Pid], []).

init([Pid]) ->
    process_flag(trap_exit, true),

    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_DGRAM, 0),

    %struct {
    %   char zero;
    %   int pid;
    %   int usecs;
    %} name;
    Proc = list_to_integer(os:getpid()),
    {_,_,USecs} = erlang:now(),

    Sun = <<
        ?PF_LOCAL:16/native,
        0:8,
        Proc:32/native,
        USecs:32/native,
        0:((?UNIX_PATH_MAX-9)*8)
        >>,
    ok = procket:bind(Socket, Sun),

    % Raw socket
    {ok, IP} = packet:socket(),
    {ok, ARP} = packet:socket(?ETH_P_ARP),

    [If] = packet:default_interface(),
    Ifindex = packet:ifindex(IP, If),

    State = #state{
            mac = gb_trees:empty(),
            ip = IP,
            arp = ARP,
            ifindex = Ifindex,
            s = Socket,
            sun = Sun,
            pid = Pid
        },

    % Monitor the network for IP events
    spawn_link(fun() -> net(IP) end),

    % Monitor the network for ARP events
    spawn_link(fun() -> net(ARP) end),

    % Monitor the Unix socket for events
    spawn_link(fun() -> unix(Socket) end),

    {ok, State}.


handle_call(name, _From, #state{sun = Sun} = State) ->
    {reply, Sun, State};

handle_call({net, Data}, _From, #state{s = Socket, mac = MAC} = State) ->
    {#ether{dhost = Dhost, type = Type}, _Packet} = epcap_net:ether(Data),
    % pretend we're switched
    case gb_trees:lookup(Dhost, MAC) of
        none when Type == ?ETH_P_ARP ->
            [ ok = procket:sendto(Socket, Data, 0, Sun) || {_,Sun} <- gb_trees:to_list(MAC) ];
        none when Dhost == <<255,255,255,255>> ->
            [ ok = procket:sendto(Socket, Data, 0, Sun) || {_,Sun} <- gb_trees:to_list(MAC) ];
        none ->
            ok;
        {value, Sun} ->
            procket:sendto(Socket, Data, 0, Sun)
    end,
    % pretend we're hubbed
%    [ ok = procket:sendto(Socket, Data, 0, Sun) || {_,Sun} <- gb_trees:to_list(MAC) ],
    {reply, ok, State};

handle_call({unix, Sun, Data}, _From, #state{ip = IP, arp = ARP, ifindex = Ifindex, mac = MAC} = State) ->
    {#ether{shost = Ether, type = Type}, _Packet} = epcap_net:ether(Data),
    Socket = case Type of
        ?ETH_P_ARP -> ARP;
        ?ETH_P_IP -> IP
    end,
    ok = packet:send(Socket, Ifindex, Data),
    {reply, ok, State#state{mac = gb_trees:enter(Ether, Sun, MAC)}};

handle_call(stop, {Pid,_}, #state{pid = Pid} = State) ->
    {stop, shutdown, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, #state{s = Socket}) ->
    % abstract Unix socket, no file to unlink
    procket:close(Socket),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
net(Socket) ->
    case procket:recvfrom(Socket, 65535) of
        {error, eagain} ->
            timer:sleep(10),
            net(Socket);
        {ok, Buf} ->
            data(net, Buf),
            net(Socket);
        Error ->
            error_logger:error_report(Error),
            procket:close(Socket)
    end.


unix(Socket) ->
    case procket:recvfrom(Socket, 65535, 0, 110) of
        {error, eagain} ->
            timer:sleep(10),
            unix(Socket);
        {ok, Buf, Sun} ->
            data(unix, {Sun, Buf}),
            unix(Socket);
        Error ->
            error_logger:error_report(Error),
            procket:close(Socket)
    end.


