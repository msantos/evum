
# Spawn Linux VMs as Erlang processes in the Erlang VM.

evum makes a Linux virtual machine into an Erlang actor. While the Linux
VM has its own state like any other process (memory, system processes,
disk), all I/O with the outside world is sent as messages through the
Erlang VM (currently working: network, system console).


# REQUIREMENTS

    sudo apt-get install user-mode-linux


# EXAMPLE

## Quick Start

* Download and create a buildroot instance

    1> {ok, Ref} = evm:create([
            {label, test1},     % Label identifying this vm
            {dist, buildroot}   % Linux distribution image, defined in priv/evm.cfg
            ]).
    {ok,<0.60.0>}

    2> flush(). % See the boot messages

    3> evum:send(Ref, "hostname").
    4> flush().
    Shell got <<"evm">>
    ok



* Download and create an OpenWRT instance with network access

Set the network configuration for your VM in priv/evm.cfg:

    {network, [
        {home, [
            {base_ip, "192.168.213."},
                {first_ip, 92},
                {netmask, "255.255.255.0"},
                {gw, "192.168.213.1"}
        ]}
        ]}.

Create the OpenWRT VM:

    1> {ok, Ref} = evm:create([
            {label, test1},     % Label for this vm
            {dist, openwrt},    % Defined in priv/evm.cfg
            {net, coffeeshop}   % Set up networking: see priv/evm.cfg
            ]).
    {ok,<0.60.0>}

From another host (NOTE: can't be the system running the evum), telnet
to the IP address of the OpenWRT VM:

    telnet 192.168.213.92 # or: nc -t 192.168.213.92 22

Run some commands in the VM. All the network data is proxied through
Erlang:

    opkg update # may need to adjust /etc/resolv.conf first
    opkg install erlang
    erl


## Using the Host OS

    % Start the network
    1> {ok,Switch} = evum_switch:start().
    {ok,<0.43.0>}
    
    % Start the system console server
    2> evum_ctl:start(Switch).
    {ok,<0.58.0>}
    
    % Start a Linux VM
    3> {ok,R} = evum:start().
    {ok,<0.61.0>}
    
    % And another VM ...
    3> {ok,R1} = evum:start().
    4ok,<0.64.0>}
    
    % One more VM ...
    5> {ok,R2} = evum:start().
    {ok,<0.67.0>}
    
    % See the Linux boot messages
    6> flush().
    
    % Set up the network interfaces
    7> evum:ifconfig(R, {"192.168.213.92","255.255.255.0","192.168.213.1"}).
    
    8> evum:ifconfig(R, {"192.168.213.93","255.255.255.0","192.168.213.1"}).
    
    9> evum:ifconfig(R, {"192.168.213.94","255.255.255.0","192.168.213.1"}).
    
    10> evum:send(R, "ifconfig -a").
    <<"root@(none):/# ifconfig -a">>
    
    11> flush().
    Shell got <<"eth0      Link encap:Ethernet  HWaddr 00:21:5d:7c:11:11  ">>
    Shell got <<"          inet addr:192.168.213.92  Bcast:192.168.213.255  Mask:255.255.255.0">>
    Shell got <<"          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1">>
    Shell got <<"          RX packets:0 errors:0 dropped:0 overruns:0 frame:0">>
    Shell got <<"          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0">>
    Shell got <<"          collisions:0 txqueuelen:1000 ">>
    Shell got <<"          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)">>
    Shell got <<"          Interrupt:5 ">>
    Shell got <<>>
    Shell got <<"lo        Link encap:Local Loopback  ">>
    Shell got <<"          LOOPBACK  MTU:16436  Metric:1">>
    Shell got <<"          RX packets:0 errors:0 dropped:0 overruns:0 frame:0">>
    Shell got <<"          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0">>
    Shell got <<"          collisions:0 txqueuelen:0 ">>
    Shell got <<"          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)">>
    Shell got <<>>
    
    % Ping something, so our fake switch can learn of the VM's prescence
    12> evum:ping(R, {192,168,213,7}).
    13> evum:ping(R1, {192,168,213,7}).
    14> evum:ping(R2, {192,168,213,7}).
    
    8> flush().
    Shell got <<"PING 192.168.213.7 (192.168.213.7) 56(84) bytes of data.">>
    Shell got <<"64 bytes from 192.168.213.7: icmp_seq=1 ttl=64 time=53.9 ms">>
    Shell got <<>>
    Shell got <<"--- 192.168.213.7 ping statistics ---">>
    Shell got <<"1 packets transmitted, 1 received, 0% packet loss, time 0ms">>
    Shell got <<"rtt min/avg/max/mdev = 53.918/53.918/53.918/0.000 ms">>
    
    % start up a shell (of sorts) on a port
    10> evum:send(R, "socat tcp-l:7777,reuseaddr,fork system:'/bin/bash',stderr &").
    <<"root@(none):/# socat tcp-l:7777,reuseaddr,fork system:'/bin/bash',stderr &">>

From another host:

    $ nc 192.168.213.92 7777
    cat /proc/cpuinfo
    processor       : 0
    vendor_id       : User Mode Linux
    model name      : UML
    mode            : skas
    host            : Linux rst 2.6.32-26-generic #48-Ubuntu SMP Wed Nov 24 09:00:03 UTC 2010 i686
    bogomips        : 1468.00


# SCREENSHOT: Erlang in Erlang!

    root@OpenWrt:/tmp# opkg install erlang
    Installing erlang (R13A-1) to root...
    Downloading http://downloads.openwrt.org/backfire/10.03/x86/packages/erlang_R13A-1_x86.ipk.
    Installing libncurses (5.7-2) to root...
    Downloading http://downloads.openwrt.org/backfire/10.03/x86/packages/libncurses_5.7-2_x86.ipk.
    Installing librt (0.9.30.1-42) to root...
    Downloading http://downloads.openwrt.org/backfire/10.03/x86/packages/librt_0.9.30.1-42_x86.ipk.
    Configuring libncurses.
    Configuring librt.
    Configuring erlang.
    root@OpenWrt:/tmp# erl
    Erlang R13A (erts-5.7) [source] [rq:1] [kernel-poll:false]
    
    Eshell V5.7  (abort with ^G)
    1> spawn(io, format, ["Erlang process spawned in an Erlang VM in a Linux VM in an Erlang process in an Erlang VM~n"]).
    Erlang process spawned in an Erlang VM in a Linux VM in an Erlang process in an Erlang VM
    <0.35.0>
    2>

