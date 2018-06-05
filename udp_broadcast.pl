/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2013, Jeffrey Rosenwald
		   2018, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(udp_broadcast,
          [ udp_host_to_address/2,             % ?Host, ?Address
            udp_broadcast_initialize/2,        % +IPAddress, +Options
            udp_broadcast_service/2            % ?Scope, ?Address
          ]).
:- use_module(library(socket)).
:- use_module(library(broadcast)).
:- use_module(library(time)).
:- use_module(library(option)).
:- use_module(library(apply)).


/** <module> A UDP Broadcast Bridge

SWI-Prolog's broadcast library provides a  means   that  may  be used to
facilitate publish and subscribe communication regimes between anonymous
members of a community of interest.  The   members  of the community are
however, necessarily limited to a  single   instance  of Prolog. The UDP
broadcast library removes that restriction.   With  this library loaded,
any member on your local IP subnetwork that also has this library loaded
may hear and respond to your broadcasts.

This  module  has  only  two  public  predicates.  When  the  module  is
initialized using udp_broadcast_initialize/2, it starts   a two listener
threads  that  listen  for  broadcasts  from  others,  received  as  UDP
datagrams.

Unlike TIPC broadcast, UDP broadcast has only one scope, =udp_subnet=. A
broadcast/1 or broadcast_request/1 that is not  directed to the listener
above, behaves as usual and is confined   to the instance of Prolog that
originated it. But when so directed, the   broadcast will be sent to all
participating systems, including  itself,  by   way  of  UDP's multicast
addressing facility. A UDP broadcast  or   broadcast  request  takes the
typical form: =|broadcast(udp_subnet(+Term, +Timeout))|=. To prevent the
potential for feedback loops, the scope   qualifier is stripped from the
message before transmission. The timeout is   optional. It specifies the
amount to time  to  wait  for  replies   to  arrive  in  response  to  a
broadcast_request. The default period is 0.250   seconds. The timeout is
ignored for broadcasts.

An example of three separate processes cooperating on the same Node:

==
Process A:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?-

Process B:

   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Process C:

   ?- findall(X, broadcast_request(udp_subnet(number(X))), Xs).
   Xs = [1, 2, 3, 4, 5, 7, 8, 9].

   ?-
==

It is also  possible  to  carry  on   a  private  dialog  with  a single
responder. To do this, you supply a   compound of the form, Term:PortId,
to a UDP scoped broadcast/1 or  broadcast_request/1, where PortId is the
ip-address and port-id of  the  intended   listener.  If  you  supply an
unbound variable, PortId, to broadcast_request, it  will be unified with
the address of the listener  that  responds   to  Term.  You  may send a
directed broadcast to a specific member by simply providing this address
in a similarly structured compound  to   a  UDP  scoped broadcast/1. The
message is sent via unicast to that member   only by way of the member's
broadcast listener. It is received by  the   listener  just as any other
broadcast would be. The listener does not know the difference.

For example, in order to discover who responded with a particular value:

==
Host B Process 1:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?-

Host A Process 1:


   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Host A Process 2:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?- bagof(X, broadcast_request(udp_subnet(number(X):From,1)), Xs).
   From = ip(192, 168, 1, 103):34855,
   Xs = [7, 8, 9] ;
   From = ip(192, 168, 1, 103):56331,
   Xs = [1, 2, 3, 4, 5] ;
   From = ip(192, 168, 1, 104):3217,
   Xs = [1, 2, 3, 4, 5].

==

## Caveats {#udp-broadcase-caveats}

While the implementation is mostly transparent, there are some important
and subtle differences that must be taken into consideration:

    * UDP broadcast requires an initialization step in order to
    launch the broadcast listener daemon. See udp_broadcast_initialize/2.

    * Prolog's broadcast_request/1 is nondet. It sends the request,
    then evaluates the replies synchronously, backtracking as needed
    until a satisfactory reply is received. The remaining potential
    replies are not evaluated. This is not so when UDP is involved.

    * A UDP broadcast/1 is completely asynchronous.

    * A  UDP broadcast_request/1 is partially synchronous. A
    broadcast_request/1 is sent, then the sender balks for a period of
    time (default: 250 ms) while the replies are collected. Any reply
    that is received after this period is silently discarded. A
    optional second argument is provided so that a sender may specify
    more (or less) time for replies.

    * Replies are presented to the user as a choice point on arrival,
    until the broadcast request timer finally expires. This
    allows traffic to propagate through the system faster and provides
    the requestor with the opportunity to terminate a broadcast request
    early if desired, by simply cutting choice points.

    * Please beware that broadcast request transactions remain active
    and resources consumed until broadcast_request finally fails on
    backtracking, an uncaught exception occurs, or until choice points
    are cut. Failure to properly manage this will likely result in
    chronic exhaustion of UDP sockets.

    * If a listener is connected to a generator that always succeeds
    (e.g. a random number generator), then the broadcast request will
    never terminate and trouble is bound to ensue.

    * broadcast_request/1 with =|udp_subnet|= scope is _not_ reentrant.
    If a listener performs a broadcast_request/1 with UDP scope
    recursively, then disaster looms certain. This caveat does not apply
    to a UDP scoped broadcast/1, which can safely be performed from a
    listener context.

    * UDP broadcast's capacity is not infinite. While it can tolerate
    substantial bursts of activity, it is designed for short bursts of
    small messages. Unlike TIPC, UDP is unreliable and has no QOS
    protections. Congestion is likely to cause trouble in the form of
    non-Byzantine failure. That is, late, lost (e.g. infinitely late),
    or duplicate datagrams. Caveat emptor.

    * A UDP broadcast_request/1 term that is grounded is considered to
    be a broadcast only. No replies are collected unless the there is at
    least one unbound variable to unify.

    * A UDP broadcast/1 always succeeds, even if there are no
    listeners.

    * A UDP broadcast_request/1 that receives no replies will fail.

    * Replies may be coming from many different places in the network
    (or none at all). No ordering of replies is implied.

    * Prolog terms are sent to others after first converting them to
    atoms using term_string/3.  Serialization does not deal with cycles,
    attributes or sharing.

    * The broadcast model is based on anonymity and a presumption of
    trust--a perfect recipe for compromise. UDP is an Internet protocol.
    A UDP broadcast listener exposes a public port (20005), which is
    static and shared by all listeners, and a private port, which is
    semi-static and unique to the listener instance. Both can be seen
    from off-cluster nodes and networks. Usage of this module exposes
    the node and consequently, the cluster to significant security
    risks. So have a care when designing your application. You must talk
    only to those who share and contribute to your concerns using a
    carefully prescribed protocol.

    * UDP broadcast categorically and silently ignores all message
    traffic originating from or terminating on nodes that are not
    members of the local subnet. This security measure only keeps honest
    people honest!

@author    Jeffrey Rosenwald (JeffRose@acm.org), Jan Wielemaker
@license   BSD-2
@see       tipc.pl
*/

:- multifile
    udp_term_string_hook/2,                     % ?Term, ?String
    black_list/1,                               % +Term
    udp:host_to_address/2.                      % +Host, -Address

%!   ~>(:P, :Q) is nondet.
%
%    asserts temporal Liveness (something good  happens, eventually) and
%    Safety (nothing bad ever  happens)   properties.  Analogous  to the
%    "leads-to" operator of Owicki and Lamport, 1982. Provides a sort of
%    lazy implication described informally as:
%
%      - Liveness: For all possible outcomes, P -> Q, eventually.
%      - Safety: For all possible outcomes, (\+P ; Q), is invariant.
%
%    Described practically:
%
%    P ~> Q, declares that if P is true, then Q must be true, now or at
%    some point in the future.

:- meta_predicate ~>(0,0).
:- op(950, xfy, ~>).

~>(P, Q) :-
    setup_call_cleanup(P,
                       (true; fail),
                       (   Q -> true
                       ;   throw(error(goal_failed(Q), context(~>, _))))
                       ).

:- meta_predicate safely(0).

safely(Predicate) :-
    catch(Predicate, Err,
          (   Err == '$aborted'
          ->  !, fail
          ;   print_message(error, Err), fail
          )).

:- meta_predicate make_thread(0, +).

% You can't thread_signal a thread that isn't running.

join_thread(Id) :-
    catch(thread_signal(Id, abort),
          error(existence_error(thread, Id), _Context),
          true),

    thread_join(Id, exception('$aborted')).

make_thread(Goal, Options) :-
    thread_create(safely(Goal), Id, Options)
      ~> join_thread(Id).

udp_broadcast_address(IPAddress, Subnet, BroadcastAddress) :-
    IPAddress = ip(A1, A2, A3, A4),
    Subnet = ip(S1, S2, S3, S4),
    BroadcastAddress = ip(B1, B2, B3, B4),

    B1 is A1 \/ (S1 xor 255),
    B2 is A2 \/ (S2 xor 255),
    B3 is A3 \/ (S3 xor 255),
    B4 is A4 \/ (S4 xor 255).

%!  udp_broadcast_service(?Scope, ?Address) is nondet.
%
%   provides the UDP broadcast address for   a  given Scope. At present,
%   only one scope is supported, =|udp_subnet|=.

%  The following are defined at initialization:
:- dynamic
    udp_subnet_member/1,      % +IpAddress:Port
    udp_broadcast_service/2.  % ?Scope, ?BroadcastAddress:Port

:- volatile
    udp_subnet_member/1,      % +IpAddress:Port
    udp_broadcast_service/2.  % ?Scope, ?BroadcastAddress:Port
%
%  Here's a UDP bridge to Prolog's broadcast library
%
%  A sender may extend a broadcast  to  a   subnet  of  a UDP network by
%  specifying a =|udp_subnet|= scoping qualifier   in his/her broadcast.
%  The qualifier has the effect of  selecting the appropriate multi-cast
%  address for the transmission. Thus,  the   sender  of the message has
%  control over the scope of his/her traffic on a per-message basis.
%
%  All in-scope listeners receive the   broadcast and simply rebroadcast
%  the message locally. All broadcast replies, if any, are sent directly
%  to the sender via the port-id that   was received with the broadcast.
%
%  Each listener exposes two UDP ports,  a   shared  public port that is
%  bound to a well-known port number and   a  private port that uniquely
%  indentifies the listener. Broadcasts are received  on the public port
%  and replies are  sent  on  the   private  port.  Directed  broadcasts
%  (unicasts) are received on the private port   and replies are sent on
%  the private port.

%  Thread 1 listens for directed traffic on the private port.
%
udp_listener_daemon1(S) :-
    repeat,
    safely(dispatch_traffic(S, S)).

%  Thread 2 listens for broadcast traffic on the well-known public port
%  (S). All replies are originated from the private port (S1).
%
udp_listener_daemon2(Parent) :-
    udp_socket(S) ~> tcp_close_socket(S),
    udp_socket(S1) ~> tcp_close_socket(S1),

    tcp_bind(S1, _PrivatePort),   % bind him to a private port now

    make_thread(udp_listener_daemon1(S1),
                [ alias(udp_listener_daemon1)]),

    tcp_setopt(S, reuseaddr),

    udp_broadcast_service(udp_subnet, _Address:Port),
    tcp_bind(S, Port),      % bind to our public port

    listen(udp_broadcast, Head, broadcast_listener(Head))
         ~> unlisten(udp_broadcast),

    thread_send_message(Parent, udp_listener_daemon_ready),

    repeat,
    safely(dispatch_traffic(S, S1)).

%!  dispatch_traffic(+PublicSocket, +PrivateSocket)
%
%   Listen for UDP traffic  on   PublicSocket.  Distribute  the received
%   events using broadcast/1 or broadcast_request/1.  In the latter case
%   the responses are sent via PrivateSocket.

dispatch_traffic(Public, Private) :-
    udp_receive(Public, Data, From, [max_message_size(65535)]),
    (   udp_subnet_member(From),    % ignore traffic that is foreign to my subnet
        udp_term_string(Term, Data) % only accept valid data
    ->  with_mutex(udp_broadcast, ld_dispatch(Private, Term, From))
    ;   true
    ),
    dispatch_traffic(Public, Private).

%!  ld_dispatch(+PrivateSocket, +Term, +From)
%
%   Locally dispatch Term received from From. If it concerns a broadcast
%   request, send the replies to PrivateSocket   to  From. The multifile
%   hook black_list/1 can be used to ignore certain messages.

ld_dispatch(_S, '$udp_request'(Term), _From) :-
    black_list(Term), !, fail.
ld_dispatch(_S, Term, _From) :-
    black_list(Term), !, fail.
ld_dispatch(S, '$udp_request'(Term), From) :-
    !,
    forall(broadcast_request(Term),
           ( udp_term_string(Term, Message),
             udp_send(S, Message, From, [])
           )).
ld_dispatch(_S, Term, _From) :-
    safely(broadcast(Term)).

%!  start_udp_proxy
%
%   Start the UDP relaying proxy service.   The  proxy consists of three
%   forwarding mechanisms:
%
%     - Listen on our _scope_.  If any messages are received, hand them
%       to udp_broadcast/3 to be broadcasted to _scope_ or sent to a
%       specific recipient.
%     - Listen on the _scope_ public port. Incomming messages are
%       relayed to the internal broadcast mechanism and replies are sent
%       to from our private socket.
%     - Listen on our private port and reply using the same port.

start_udp_proxy :-
    catch(thread_property(udp_listener_daemon2, status(running)),
          error(_,_),
          fail),
    !.
start_udp_proxy :-
    thread_self(Self),
    thread_create(udp_listener_daemon2(Self), _,
                  [ alias(udp_listener_daemon2),
                    detached(true)
                  ]),
    (   thread_get_message(Self, udp_listener_daemon_ready,
                           [ timeout(10)
                           ])
    ->  true
    ;   throw(error(thread_starting_error(udp_listener_daemon2), _))
    ).

broadcast_listener(udp_host_to_address(Host, Addr)) :-
    udp:host_to_address(Host, Addr).
broadcast_listener(udp_broadcast_service(Scope, Addr)) :-
    udp_broadcast_service(Scope, Addr).
broadcast_listener(udp_subnet(X)) :-
    udp_broadcast(X, udp_subnet, 0.250).
broadcast_listener(udp_subnet(X, Timeout)) :-
    udp_broadcast(X, udp_subnet, Timeout).

%!  udp_broadcast(+What, +Scope, +TimeOut)
%
%   Send a broadcast request to my UDP peers in Scope. What is either of
%   the shape `Term:Address` to send Term to a specific address or query
%   the address from which term is answered or it is a plain `Term`.
%
%   If `Term` is  nonground,  it  is   considered  is  a  _request_ (see
%   broadcast_request/1) and the predicate  succeeds   for  each  answer
%   received within TimeOut seconds. If Term is ground it is considered
%   an asynchronous broadcast and udp_broadcast/3 is deterministic.

udp_broadcast(Term:To, _Scope, _Timeout) :-
    ground(Term), ground(To),           % broadcast to single listener
    !,
    udp_basic_broadcast(_S, _Port, Term, To),
    !.
udp_broadcast(Term, Scope, _Timeout) :-
    ground(Term),                       % broadcast to all listeners
    !,
    udp_broadcast_service(Scope, Address),
    udp_basic_broadcast(_S, _Port, Term, Address),
    !.
udp_broadcast(Term:Address, _Scope, Timeout) :-
    ground(Address),                    % request to single listener
    !,
    udp_basic_broadcast(S, Port, '$udp_request'(Term), Address),
    udp_br_collect_replies(S, Port, Timeout, Term:Address).
udp_broadcast(Term:From, Scope, Timeout) :-
    !,                                  % request to all listeners, collect sender
    udp_broadcast_service(Scope, Address),
    udp_basic_broadcast(S, Port, '$udp_request'(Term), Address),
    udp_br_collect_replies(S, Port, Timeout, Term:From).
udp_broadcast(Term, Scope, Timeout) :-  % request to all listeners
    udp_broadcast(Term:_, Scope, Timeout).


%!  udp_basic_broadcast(-S, ?Port, +Term, +Address)
%
%   Create a UDP private socket and use it   to send Term to Address. If
%   Address is our broadcast address, set the socket in broadcast mode.
%
%   This predicate succeeds with a choice   point. Committing the choice
%   point closes S.

udp_basic_broadcast(S, Port, Term, Address) :-
    udp_socket(S)
      ~> tcp_close_socket(S),

    (   udp_broadcast_service(udp_subnet, Address)
    ->  tcp_setopt(S, broadcast)
    ;   true
    ),

    tcp_bind(S, Port),  % find our own ephemeral Port
    udp_term_string(Term, String),

    (   udp_subnet_member(Address)  % talk only to your local subnet
    ->  safely(udp_send(S, String, Address, []))
    ;   true
    ).

%!  udp_br_collect_replies(+Socket, +Port, +TimeOut, -TermAndFrom) is nondet.
%
%   Collect replies on Socket for  TimeOut   seconds.  Succeed  for each
%   received message.

udp_br_collect_replies(S, _Port, Timeout, Term:From) :-
    tcp_getopt(S, file_no(Fd)),
    get_time(Start),
    Deadline is Start+Timeout,
    repeat,
       get_time(Now),
       (   SingleTMO is Deadline - Now,
           SingleTMO > 0,
           wait_for_input([Fd], [Fd], SingleTMO)
       ->  udp_receive(S, String, From, [max_message_size(65535)]),
           safely(udp_term_string(Term, String))
       ;   !,
           fail
       ).

%!  udp_host_to_address(?Service, ?Address) is nondet.
%
%   locates a UDP service by name. Service   is an atom or grounded term
%   representing the common name  of  the   service.  Address  is  a UDP
%   address structure. A server may advertise   its  services by name by
%   including   the   fact,   udp:host_to_address(+Service,   +Address),
%   somewhere in its source. This predicate can  also be used to perform
%   reverse searches. That is it  will  also   resolve  an  Address to a
%   Service name.

udp_host_to_address(Host, Address) :-
    broadcast_request(udp_subnet(udp_host_to_address(Host, Address))).

%!  udp_broadcast_initialize(+IPAddress, +Options) is semidet.
%
%   Initialized UDP broadcast bridge. IPAddress is the IP address on the
%   network we want to broadcast on.  IP addresses are terms ip(A,B,C,D)
%   or an atom or string of the format =|A.B.C.D|=.   Options processed:
%
%     - subnet_mask(+SubNet)
%     Subnet to broadcast on.  This uses the same syntax as IPAddress.
%     Default classifies the network as class A, B or C depending on
%     the the first octet and applies the default mask.
%     - port(+Port)
%     Public port to use.  Default is 20005.
%
%   For compatibility reasons Options may be the subnet mask.

udp_broadcast_initialize(IP, Options) :-
    nonvar(Options),
    Options = ip(_,_,_,_),
    !,
    udp_broadcast_initialize(IP, [subnet_mask(Options)]).
udp_broadcast_initialize(IP, Options) :-
    to_ip4(IP, IPAddress),
    option(subnet_mask(Subnet), Options, _),
    mk_subnet(Subnet, IPAddress, Subnet4),
    option(port(Port), Options, 20005),

    retractall(udp_broadcast_service(_,_)),
    retractall(udp_subnet_member(_)),

    udp_broadcast_address(IPAddress, Subnet4, BroadcastAddr),
    assert(udp_broadcast_service(udp_subnet, BroadcastAddr:Port)),
    assert((udp_subnet_member(Address:_Port) :-
               udp_broadcast_address(Address, Subnet4, BroadcastAddr))),

    start_udp_proxy.

to_ip4(Atomic, ip(A,B,C,D)) :-
    atomic(Atomic),
    !,
    (   split_string(Atomic, ".", "", Strings),
        maplist(number_string, [A,B,C,D], Strings)
    ->  true
    ;   syntax_error(illegal_ip_address)
    ).
to_ip4(IP, IP).

mk_subnet(Var, IP, Subnet) :-
    var(Var),
    !,
    (   default_subnet(IP, Subnet)
    ->  true
    ;   domain_error(ip_with_subnet, IP)
    ).
mk_subnet(Subnet, _, Subnet4) :-
    to_ip4(Subnet, Subnet4).

default_subnet(ip(A,_,_,_), ip(A,0,0,0)) :-
    between(1,126, A), !.
default_subnet(ip(A,B,_,_), ip(A,B,0,0)) :-
    between(128,191, A), !.
default_subnet(ip(A,B,C,_), ip(A,B,C,0)) :-
    between(192,223, A), !.


		 /*******************************
		 *             HOOKS		*
		 *******************************/

%!  udp_term_string(+Term, -String) is det.
%!  udp_term_string(-Term, +String) is semidet.
%
%   Serialize an arbitrary Prolog  term  as   a  string.  The  string is
%   prefixed by a magic key to ensure   we only accept messages that are
%   meant for us.
%
%   In mode (+,-), Term is written with the options ignore_ops(true) and
%   quoted(true).
%
%   This predicate first calls  udp_term_string_hook/2   with  the  same
%   signature. This hook  may  use   alternative  serialization  such as
%   fast_term_serialized/2,  use  library(ssl)  to    realise  encrypted
%   messages, etc.

udp_term_string(Term, String) :-
    udp_term_string_hook(Term, String),
    !.
udp_term_string(Term, String) :-
    (   var(String)
    ->  format(string(String), '%-prolog-\n~W',
               [ Term,
                 [ ignore_ops(true),
                   quoted(true)
                 ]
               ])
    ;   sub_string(String, 0, _, _, '%-prolog-\n'),
        term_string(Term, String,
                    [ syntax_errors(quiet)
                    ])
    ).

