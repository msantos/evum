%% Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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
-module(evum_srv).
-behaviour(gen_server).

-include("evum.hrl").

-define(PROGNAME, "priv/linux").

-export([start/2, stop/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {
    cons,
    pid,
    port
}).



start(Pid, Options) when is_pid(Pid), is_list(Options) ->
    start_link([Pid, Options]).

stop(Ref) ->
    gen_server:call(Ref, stop).


start_link([Pid, Options]) ->
    gen_server:start_link(?MODULE, [Pid, Options], []);
start_link([Pid, Label, Options]) ->
    gen_server:start_link({local, Label}, ?MODULE, [Pid, Options], []).


init([Pid, Options]) ->
    process_flag(trap_exit, true),
    UmlId = umid(self()),

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
            {uml_dir, ?UML_DIR}
        ] ++ Args),
    error_logger:info_report([{cmd, Cmd}]),
    Port = open_port({spawn, Cmd}, [
            {line, 2048},
            binary,
            stderr_to_stdout,
            exit_status
        ]),
    error_logger:info_report([{mcons, ?UML_DIR ++ "/" ++ UmlId ++ "/mconsole"}]),
    {ok, Cons} = evum_mcons:open(?UML_DIR ++ "/" ++ UmlId ++ "/mconsole"),
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
    {stop, normal, ok, State};

handle_call(Msg, Pid, State) ->
    error_logger:error_report([
            {error, unhandled_message},
            {msg, Msg},
            {pid, Pid},
            {state, State}
        ]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, #state{cons = Cons, port = Port}) ->
    evum_mcons:send(Cons, <<"halt">>),
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
%%% Internal functions
%%--------------------------------------------------------------------
make_args(PL) ->
    proplists:get_value(progname, PL, ?PROGNAME) ++ " " ++
    string:join([ get_switch(N) || N <- PL ], " ").

get_switch({con, Arg})                      -> "con=" ++ Arg;
get_switch({con, Dev, Arg})                 -> "con" ++ integer_to_list(Dev) ++ "=" ++ Arg;

get_switch({disk, Dev, Image})              -> Dev ++ "=" ++ Image;
get_switch({disk, Dev, Image, Cow})         -> Dev ++ "=" ++ Cow ++ "," ++ Image;

% eth[0-9]+=daemon,ethernet_address,type,control_socket,data_socket
% eth0=daemon,,unix,/var/run/uml-utilities/uml_switch.ctl
get_switch({eth, Dev, Path})                -> get_switch({eth, Dev, Path, ""});
get_switch({eth, Dev, Path, MAC})           ->
    "eth" ++ integer_to_list(Dev) ++ "=daemon," ++ MAC ++ ",unix," ++ Path;

get_switch({init, Arg})                     -> "init=" ++ Arg;

get_switch({initrd, Arg})                   -> "initrd=" ++ Arg;

get_switch({mem, Arg}) when is_integer(Arg) -> "mem=" ++ integer_to_list(Arg) ++ "M";
get_switch({mem, Arg}) when is_list(Arg)    -> "mem=" ++ Arg;

get_switch(net)                             -> "";
get_switch({net, _})                        -> "";

get_switch({root, Arg})                     -> "root=" ++ Arg;
get_switch({rootflags, Arg})                -> "rootflags=" ++ Arg;
get_switch({rootfstype, Arg})               -> "rootfstype=" ++ Arg;

get_switch({ssl, Arg})                      -> "ssl=" ++ Arg;
get_switch({ssl, Dev, Arg})                 -> "ssl" ++ integer_to_list(Dev) ++ "=" ++ Arg;

get_switch({ubd, Arg})                      -> "ubda=" ++ Arg;
get_switch({ubd, Dev, Arg})                 -> "ubd" ++ Dev ++ "=" ++ Arg;

get_switch({umid, Arg})                     -> "umid=" ++ Arg;

get_switch({uml_dir, Arg})                  -> "uml_dir=" ++ Arg;

get_switch({verbose, true})                 -> "";
get_switch({verbose, false})                -> "quiet";

get_switch({write, true})                   -> "rw";

get_switch(Arg) ->
    error_logger:warning_report([{ignored, Arg}]),
    "".


umid(Pid) when is_pid(Pid) ->
    {match,[A,B,C]} = re:run(pid_to_list(Pid), "<(\\d+)\.(\\d+)\.(\\d+)>", [{capture, [1,2,3], list}]),
    "pid-" ++ os:getpid() ++ "-" ++ A ++ "-" ++ B ++ "-" ++ C.
