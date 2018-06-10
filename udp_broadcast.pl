/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald and Jan Wielemaker
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
          [ udp_broadcast_initialize/2,         % +IPAddress, +Options
            udp_peer_add/2,                     % +Scope, +IP
            udp_peer_del/2,                     % +Scope, ?IP
            udp_peer/2                          % +Scope, -IP
          ]).
:- use_module(library(socket)).
:- use_module(library(broadcast)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).

% :- debug(udp(broadcast)).

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
typical form: =|broadcast(udp(+Scope, +Term,   +Timeout))|=.  To prevent
the potential for feedback loops, the   scope qualifier is stripped from
the message before transmission. The timeout   is optional. It specifies
the amount to time to wait  for  replies   to  arrive  in  response to a
broadcast_request/1. The default period is 0.250 seconds. The timeout is
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

   ?- findall(X, broadcast_request(udp(subnet, number(X))), Xs).
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

   ?- bagof(X, broadcast_request(udp(subnet,number(X):From,1)), Xs).
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
    launch the broadcast listener bridge. See udp_broadcast_initialize/2.

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
    udp_term_string_hook/3,                     % +Scope, ?Term, ?String
    black_list/1.                               % +Term

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

%!  udp_scope(?ScopeName, ?ScopeDef)

:- dynamic
    udp_scope/2,
    udp_scope_peer/2.
:- volatile
    udp_scope/2,
    udp_scope_peer/2.
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

:- dynamic
    udp_private_socket/3,                       % Port, Socket, FileNo
    udp_public_socket/4.

udp_inbound_proxy :-
    make_private_socket,
    forall(udp_scope(Scope, ScopeData),
           make_public_socket(ScopeData, Scope)),
    findall(FileNo, udp_socket_file_no(FileNo), FileNos),
    dispatch_inbound(FileNos).

%!  make_private_socket is det.
%
%   Create our private socket. This socket is used for messages that are
%   directed to me. Note that we only  need this for broadcast networks.
%   If we use a unicast network we use   our public port to contact this
%   specific server.

make_private_socket :-
    udp_private_socket(_,_,_),
    !.
make_private_socket :-
    udp_scope(_, broadcast(_,_,_)),
    !,
    udp_socket(S),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    tcp_setopt(S, broadcast),
    assertz(udp_private_socket(Port, S, F)).
make_private_socket.

%!  make_public_socket(+ScopeData, +Scope)
%
%   Create the public port Scope.

make_public_socket(_, Scope) :-
    udp_public_socket(Scope, _, _, _),
    !.
make_public_socket(broadcast(_SubNet, _Broadcast, Port), Scope) :-
    udp_socket(S),
    tcp_setopt(S, reuseaddr),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    assertz(udp_public_socket(Scope, Port, S, F)).
make_public_socket(unicast(Port), Scope) :-
    udp_socket(S),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    assertz(udp_public_socket(Scope, Port, S, F)).

udp_socket_file_no(FileNo) :-
    udp_private_socket(_,_,FileNo).
udp_socket_file_no(FileNo) :-
    udp_public_socket(_,_,_,FileNo).

%!  dispatch_inbound(+FileNos)
%
%   Dispatch inbound traffic. This loop   uses  wait_for_input/3 to wait
%   for one or more UDP sockets and   dispatches  the requests using the
%   internal broadcast service. For an  incomming broadcast _request_ we
%   send the reply only to the  requester   and  therefore we must use a
%   socket that is not in broadcast mode.

dispatch_inbound(FileNos) :-
    debug(udp(broadcast), 'Waiting for ~p', [FileNos]),
    wait_for_input(FileNos, Ready, infinite),
    debug(udp(broadcast), 'Ready: ~p', [Ready]),
    maplist(dispatch_ready, Ready),
    dispatch_inbound(FileNos).

dispatch_ready(FileNo) :-
    udp_private_socket(_Port, Private, FileNo),
    !,
    udp_receive(Private, Data, From, [max_message_size(65535)]),
    debug(udp(broadcast), 'Inbound on private port', []),
    (   in_scope(Scope, From),
        udp_term_string(Scope, Term, Data) % only accept valid data
    ->  with_mutex(udp_broadcast, ld_dispatch(Private, Term, From, Scope))
    ;   true
    ).
dispatch_ready(FileNo) :-
    udp_public_socket(Scope, _PublicPort, Public, FileNo),
    !,
    udp_receive(Public, Data, From, [max_message_size(65535)]),
    debug(udp(broadcast), 'Inbound on public port from ~p for scope ~p',
          [From, Scope]),
    (   in_scope(Scope, From),
        udp_term_string(Scope, Term, Data) % only accept valid data
    ->  (   udp_scope(Scope, unicast(_))
        ->  with_mutex(udp_broadcast, ld_dispatch(Public, Term, From, Scope))
        ;   udp_private_socket(_PrivatePort, Private, _FileNo),
            with_mutex(udp_broadcast, ld_dispatch(Private, Term, From, Scope))
        )
    ;   true
    ).

in_scope(Scope, Address) :-
    udp_scope(Scope, ScopeData),
    in_scope(ScopeData, Scope, Address),
    !.
in_scope(Scope, From) :-
    debug(udp(broadcast), 'Ignore out-of-scope ~p datagram from ~p',
          [Scope, From]),
    fail.

in_scope(broadcast(Subnet, Broadcast, _PublicPort), _Scope, IP:_FromPort) :-
    udp_broadcast_address(IP, Subnet, Broadcast).
in_scope(unicast(_PublicPort), Scope, IP:_) :-
    udp_peer(Scope, IP:_).


%!  ld_dispatch(+PrivateSocket, +Term, +From, +Scope)
%
%   Locally dispatch Term received from From. If it concerns a broadcast
%   request, send the replies to PrivateSocket   to  From. The multifile
%   hook black_list/1 can be used to ignore certain messages.

ld_dispatch(_S, Term, _From, _Scope) :-
    blacklisted(Term), !.
ld_dispatch(_S, Term, From, _Scope) :-
    debug(udp(broadcast), 'ld_dispatch(~p) from ~p', [Term, From]),
    fail.
ld_dispatch(S, request(Key, Term), From, Scope) :-
    !,
    forall(safely(broadcast_request(Term)),
           safely((udp_term_string(Scope, reply(Key,Term), Message),
                   udp_send(S, Message, From, [])))).
ld_dispatch(_S, send(Term), _From, _Scope) :-
    safely(broadcast(Term)).
ld_dispatch(_S, reply(Key, Term), From, _Scope) :-
    (   reply_queue(Key, Queue)
    ->  thread_send_message(Queue, Term:From)
    ;   true
    ).

blacklisted(send(Term))      :- black_list(Term).
blacklisted(request(_,Term)) :- black_list(Term).
blacklisted(reply(_,Term))   :- black_list(Term).


%!  reload_udp_proxy
%
%   Update the UDP relaying proxy service.   The proxy consists of three
%   forwarding mechanisms:
%
%     - Listen on our _scope_.  If any messages are received, hand them
%       to udp_broadcast/3 to be broadcasted to _scope_ or sent to a
%       specific recipient.
%     - Listen on the _scope_ public port. Incomming messages are
%       relayed to the internal broadcast mechanism and replies are sent
%       to from our private socket.
%     - Listen on our private port and reply using the same port.

reload_udp_proxy :-
    reload_outbound_proxy,
    reload_inbound_proxy.

reload_outbound_proxy :-
    listening(udp_broadcast, udp(_,_), _),
    !.
reload_outbound_proxy :-
    listen(udp_broadcast, udp(Scope,Message),
           udp_broadcast(Message, Scope, 0.25)),
    listen(udp_broadcast, udp(Scope,Message,Timeout),
           udp_broadcast(Message, Scope, Timeout)),
    listen(udp_broadcast, udp_subnet(Message),  % backward compatibility
           udp_broadcast(Message, subnet, 0.25)),
    listen(udp_broadcast, udp_subnet(Message,Timeout),
           udp_broadcast(Message, subnet, Timeout)).

reload_inbound_proxy :-
    catch(thread_signal(udp_inbound_bridge, reload_inbound),
          error(existence_error(thread, _),_),
          fail),
    !.
reload_inbound_proxy :-
    thread_create(udp_inbound_proxy, _,
                  [ alias(udp_inbound_bridge),
                    detached(true)
                  ]).

%!  udp_broadcast_close(+Scope)
%
%   Close a UDP broadcast scope.

udp_broadcast_close(Scope) :-
    udp_scope(Scope, _ScopeData),
    !,
    format(user_error, 'Need to close UDP scope ~p~n', [Scope]).
udp_broadcast_close(_).


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

udp_broadcast(Term:To, Scope, _Timeout) :-
    ground(Term), ground(To),           % broadcast to single listener
    !,
    udp_basic_broadcast(send(Term), Scope, single(To)).
udp_broadcast(Term, Scope, _Timeout) :-
    ground(Term),                       % broadcast to all listeners
    !,
    udp_basic_broadcast(send(Term), Scope, broadcast).
udp_broadcast(Term:To, Scope, Timeout) :-
    ground(To),                         % request to single listener
    !,
    setup_call_cleanup(
        request_queue(Id, Queue),
        ( udp_basic_broadcast(request(Id, Term), Scope, single(To)),
          udp_br_collect_replies(Queue, Timeout, Term:To)
        ),
        destroy_request_queue(Queue)).
udp_broadcast(Term:From, Scope, Timeout) :-
    !,                                  % request to all listeners, collect sender
    setup_call_cleanup(
        request_queue(Id, Queue),
        ( udp_basic_broadcast(request(Id, Term), Scope, broadcast),
          udp_br_collect_replies(Queue, Timeout, Term:From)
        ),
        destroy_request_queue(Queue)).
udp_broadcast(Term, Scope, Timeout) :-  % request to all listeners
    udp_broadcast(Term:_, Scope, Timeout).

:- dynamic
    reply_queue/2.

request_queue(Id, Queue) :-
    Id is random(1<<63),
    message_queue_create(Queue),
    asserta(reply_queue(Id, Queue)).

destroy_request_queue(Queue) :-         % leave queue to GC
    retractall(reply_queue(_, Queue)).


%!  udp_basic_broadcast(+Term, +Dest) is multi.
%
%   Create a UDP private socket and use it   to send Term to Address. If
%   Address is our broadcast address, set the socket in broadcast mode.
%
%   This predicate succeeds with a choice   point. Committing the choice
%   point closes S.
%
%   @arg Dest is one of single(Target) or `broadcast`.

udp_basic_broadcast(Term, Scope, Dest) :-
    debug(udp(broadcast), 'UDP proxy outbound ~p to ~p', [Term, Dest]),
    udp_term_string(Scope, Term, String),
    udp_send_message(Dest, String, Scope).

udp_send_message(single(Address), String, Scope) :-
    (   udp_scope(Scope, unicast(_))
    ->  udp_public_socket(Scope, _Port, S, _)
    ;   udp_private_socket(_Port, S, _F)
    ),
    safely(udp_send(S, String, Address, [])).
udp_send_message(broadcast, String, Scope) :-
    (   udp_scope(Scope, unicast(_))
    ->  udp_public_socket(Scope, _Port, S, _),
        forall(udp_peer(Scope, Address),
               ( debug(udp(broadcast), 'Unicast to ~p', [Address]),
                 safely(udp_send(S, String, Address, []))))
    ;   udp_scope(Scope, broadcast(_SubNet, Broadcast, Port))
    ->  udp_private_socket(_PrivatePort, S, _F),
        udp_send(S, String, Broadcast:Port, [])
    ).

% ! udp_br_collect_replies(+Queue, +TimeOut, -TermAndFrom) is nondet.
%
%   Collect replies on Socket for  TimeOut   seconds.  Succeed  for each
%   received message.

udp_br_collect_replies(Queue, Timeout, Reply) :-
    get_time(Start),
    Deadline is Start+Timeout,
    repeat,
       (   thread_get_message(Queue, Reply,
                              [ deadline(Deadline)
                              ])
       ->  true
       ;   !,
           fail
       ).

%!  udp_broadcast_initialize(+IPAddress, +Options) is semidet.
%
%   Initialized UDP broadcast bridge. IPAddress is the IP address on the
%   network we want to broadcast on.  IP addresses are terms ip(A,B,C,D)
%   or an atom or string of the format =|A.B.C.D|=.   Options processed:
%
%     - scope(+ScopeName)
%     Name of the scope.  Default is `subnet`.
%     - subnet_mask(+SubNet)
%     Subnet to broadcast on.  This uses the same syntax as IPAddress.
%     Default classifies the network as class A, B or C depending on
%     the the first octet and applies the default mask.
%     - port(+Port)
%     Public port to use.  Default is 20005.
%     - method(+Method)
%     Method to send a message to multiple peers.  One of
%       - broadcast
%       Use UDP broadcast messages to the LAN.  This is the
%       default
%       - multicast
%       Use UDP multicast messages.  This can be used on WAN networks,
%       provided the intermediate routers understand multicast.
%       - unicast
%       Send the messages individually to all registered peers.
%
%   For compatibility reasons Options may be the subnet mask.

udp_broadcast_initialize(IP, Options) :-
    with_mutex(udp_broadcast,
               udp_broadcast_initialize_sync(IP, Options)).

udp_broadcast_initialize_sync(IP, Options) :-
    nonvar(Options),
    Options = ip(_,_,_,_),
    !,
    udp_broadcast_initialize(IP, [subnet_mask(Options)]).
udp_broadcast_initialize_sync(IP, Options) :-
    to_ip4(IP, IPAddress),
    option(method(Method), Options, broadcast),
    must_be(oneof([broadcast, multicast, unicast]), Method),
    udp_broadcast_initialize_sync(Method, IPAddress, Options),
    reload_udp_proxy.

udp_broadcast_initialize_sync(broadcast, IPAddress, Options) :-
    option(subnet_mask(Subnet), Options, _),
    mk_subnet(Subnet, IPAddress, Subnet4),
    option(port(Port), Options, 20005),
    option(scope(Scope), Options, subnet),

    udp_broadcast_address(IPAddress, Subnet4, Broadcast),
    udp_broadcast_close(Scope),
    assertz(udp_scope(Scope, broadcast(Subnet4, Broadcast, Port))).
udp_broadcast_initialize_sync(unicast, _IPAddress, Options) :-
    option(port(Port), Options, 20005),
    option(scope(Scope), Options, subnet),
    udp_broadcast_close(Scope),
    assertz(udp_scope(Scope, unicast(Port))).


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
		 *          UNICAST PEERS	*
		 *******************************/

%!  udp_peer_add(+Scope, +Address) is det.
%!  udp_peer_del(+Scope, +Address) is det.
%!  udp_peer(?Scope, ?Address) is nondet.
%
%   Manage and query the set  of  known   peers  for  a unicast network.
%   Address is either a term  IP:Port  or   a  plain  IP address. In the
%   latter case the default port registered with the scope is used.
%
%   @arg Address has canonical form ip(A,B,C,D):Port.

udp_peer_add(Scope, Address) :-
    peer_address(Address, Scope, Canonical),
    (   udp_scope_peer(Scope, Canonical)
    ->  true
    ;   assertz(udp_scope_peer(Scope, Canonical))
    ).

udp_peer_del(Scope, Address) :-
    peer_address(Address, Scope, Canonical),
    retractall(udp_scope_peer(Scope, Canonical)).

udp_peer(Scope, IPAddress) :-
    udp_scope_peer(Scope, IPAddress).

peer_address(IP:Port, _Scope, IPAddress:Port) :-
    !,
    must_be(nonneg, Port),
    to_ip4(IP, IPAddress).
peer_address(IP, Scope, IPAddress:Port) :-
    (   udp_scope(Scope, unicast(Port))
    ->  true
    ;   existence_error(udp_scope, Scope)
    ),
    to_ip4(IP, IPAddress).



		 /*******************************
		 *             HOOKS		*
		 *******************************/

%!  udp_term_string(+Scope, +Term, -String) is det.
%!  udp_term_string(+Scope, -Term, +String) is semidet.
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
%
%   @arg Scope is the scope for which the message is broadcasted.  This
%   can be used to use different serialization for different scopes.

udp_term_string(Scope, Term, String) :-
    udp_term_string_hook(Scope, Term, String),
    !.
udp_term_string(_Scope, Term, String) :-
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

