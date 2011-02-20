
# Spawn Linux VMs as Erlang processes in the Erlang VM.

evum makes a Linux virtual machine into an Erlang actor. While the Linux
VM has its own state like any other process (memory, system processes,
disk), all I/O with the outside world is sent as messages through the
Erlang VM (currently working: network, system console).


# TODO

Create a Linux system image rather than relying on the host filesystem.

Like an Erlang process the state of the Linux VM will be immutable. While
it may store data on its filesystem, the data will be wiped after the
VM reboots.  System binaries will not be writable. To store persistent
data, the Linux VM will save its state by messaging some other Erlang
process, maybe by writing to a FUSE filesystem.

The FUSE filesystem should be user definable. By default, it should act
as a passthrough: read() -> file:read/2, write() -> file:write/2, etc.
A distributed backend would allow failover of VM's.

Virtualization is done using User Mode Linux, but probably Linux
Containers (lxc) will also be supported in the future, and maybe others
using Erlang bindings to libvirt.


# EXAMPLE

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

