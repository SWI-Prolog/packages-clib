/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, Jeffrey Rosenwald

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(udp_broadcast,
             [
	     udp_host_to_address/2,    % ? Host, ? Address
	     udp_broadcast_initialize/2,   % +IPAddress, +Subnet
	     udp_broadcast_service/2   % ? Domain, ? Address
	     ]).

/** <module> A UDP Broadcast Bridge

SWI-Prolog's broadcast library provides a  means   that  may  be used to
facilitate publish and subscribe communication regimes between anonymous
members of a community of interest.  The   members  of the community are
however, necessarily limited to a  single   instance  of Prolog. The UDP
broadcast library removes that restriction.   With  this library loaded,
any member on your local IP subnetwork that also has this library loaded
may hear and respond to your broadcasts.

This  module  has  only  two  public  predicates.  When  the  module  is
initialized, it starts a two listener threads that listen for broadcasts
from others, received as UDP datagrams.

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
    atoms using term_to_atom/2. Passing real numbers this way may
    result in a substantial truncation of precision.

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

@author    Jeffrey Rosenwald (JeffRose@acm.org)
@license   LGPL
@see       tipc.pl
*/

:- use_module(library(socket)).
:- use_module(library(broadcast)).
:- use_module(library(time)).

:- require([ thread_self/1
	   , forall/2
	   , term_to_atom/2
	   , thread_send_message/2
	   , catch/3
	   , setup_call_cleanup/3
	   , thread_create/3
	   ]).

% %	~>(:P, :Q) is nondet.
% %     eventually_implies(P, Q) is nondet.
%    asserts temporal Liveness (something good happens, eventually) and
%    Safety (nothing bad ever happens) properties. Analogous to the
%    "leads-to" operator of Owicki and Lamport, 1982. Provides a sort of
%    lazy implication described informally as:
%
%    * Liveness: For all possible outcomes, P -> Q, eventually.
%    * Safety: For all possible outcomes, (\+P ; Q), is invariant.
%
%  Described practically:
%
%    P ~> Q, declares that if P is true, then Q must be true, now or at
%    some point in the future.
%

:- meta_predicate ~>(0,0).
:- op(950, xfy, ~>).

~>(P, Q) :-
	setup_call_cleanup(P,
			   (true; fail),
			   (   Q -> true;
			   throw(error(goal_failed(Q), context(~>, _))))
			  ).

:- meta_predicate safely(0).

safely(Predicate) :-
	catch(Predicate, Err,
	      (Err == '$aborted' -> (!, fail);
	      print_message(error, Err), fail)).

:- meta_predicate make_thread(0, +).

% You can't thread_signal a thread that isn't running.

join_thread(Id) :-
       catch(thread_signal(Id, abort),
	     error(existence_error(thread, Id), _Context),
	     true),

       thread_join(Id, exception('$aborted')).

make_thread(Goal, Options) :-
	thread_create(safely(Goal), Id, [ detached(false) | Options ])
	  ~> join_thread(Id).

udp_broadcast_address(IPAddress, Subnet, BroadcastAddress) :-
	IPAddress = ip(A1, A2, A3, A4),
	Subnet = ip(S1, S2, S3, S4),
	BroadcastAddress = ip(B1, B2, B3, B4),

	B1 is A1 \/ (S1 xor 255),
	B2 is A2 \/ (S2 xor 255),
	B3 is A3 \/ (S3 xor 255),
	B4 is A4 \/ (S4 xor 255).

%%	udp_broadcast_service(?Domain, ?Address) is nondet.
%   provides the UDP broadcast address for a given Domain. At present,
%   only one domain is supported, =|udp_subnet|=.
%

%  The following are defined at initialization:
:- dynamic
	udp_subnet_member/1,      % +IpAddress:Port
	udp_broadcast_service/2.  % ?Domain, ?BroadcastAddress:Port

:- volatile
	udp_subnet_member/1,      % +IpAddress:Port
	udp_broadcast_service/2.  % ?Domain, ?BroadcastAddress:Port
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
%
%  Interactions with TIPC Broadcast
%
%  As a security precaution, we do   not allow unsupervised transactions
%  directly between UDP and TIPC broadcast. These terms are black-listed
%  and ignored when received via UDP   listeners.  A UDP enabled service
%  that wishes to use TIPC resources on  the cluster must have a sponsor
%  on the TIPC cluster to filter   incoming  UDP broadcast traffic. This
%  can be as simple as loading  and initializing both tipc_broadcast and
%  udp_broadcast within the same TIPC broadcast service.
%
%  Because the UDP  and  TIPC  broadcast   listeners  are  operating  in
%  separate threads of execution, a  UDP   broadcast  sponsor can safely
%  perform a broadcast_request with  TIPC  scope   from  within  the UDP
%  broadcast listener context. This is one of the few scenarios where a
%  recursive broadcast_request with TIPC scope is safe.
%

black_list(tipc_node(_)).
black_list(tipc_node(_,_)).
black_list(tipc_cluster(_)).
black_list(tipc_cluster(_,_)).
black_list(tipc_zone(_)).
black_list(tipc_zone(_,_)).
%
ld_dispatch(_S, '$udp_request'(Term), _From) :-
	black_list(Term), !, fail.

ld_dispatch(_S, Term, _From) :-
	black_list(Term), !, fail.

ld_dispatch(S, '$udp_request'(wru(Name)), From) :-
	!, gethostname(Name),
	term_to_atom(wru(Name), Atom),
	udp_send(S, Atom, From, []).

ld_dispatch(S, '$udp_request'(Term), From) :-
	!, forall(broadcast_request(Term),
	      (   term_to_atom(Term, Atom),
		  udp_send(S, Atom, From, []))).

ld_dispatch(_S, Term, _From) :-
	safely(broadcast(Term)).

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

dispatch_traffic(S, S1) :-
	udp_receive(S, Data, From,
		    [as(atom), max_message_size(65535)]),
	udp_subnet_member(From),  % ignore all traffic that is foreign to my subnet
	term_to_atom(Term, Data),
	with_mutex(udp_broadcast, ld_dispatch(S1, Term, From)), !,
	dispatch_traffic(S, S1).

start_udp_listener_daemon :-
	catch(thread_property(udp_listener_daemon2, status(running)),_, fail),
	!.

start_udp_listener_daemon :-
	thread_self(Self),
	thread_create(udp_listener_daemon2(Self), _,
	       [alias(udp_listener_daemon2), detached(true)]),
	call_with_time_limit(6.0,
			     thread_get_message(udp_listener_daemon_ready)).

:- multifile udp:host_to_address/2.
%
broadcast_listener(udp_host_to_address(Host, Addr)) :-
	udp:host_to_address(Host, Addr).

broadcast_listener(udp_broadcast_service(Class, Addr)) :-
	udp_broadcast_service(Class, Addr).

broadcast_listener(udp_subnet(X)) :-
	udp_broadcast(X, udp_subnet, 0.250).

broadcast_listener(udp_subnet(X, Timeout)) :-
	udp_broadcast(X, udp_subnet, Timeout).

%
%
udp_basic_broadcast(S, Port, Term, Address) :-
	udp_socket(S)
	  ~> tcp_close_socket(S),

	(   udp_broadcast_service(udp_subnet, Address)
	       -> tcp_setopt(S, broadcast)
	       ; true
	),

	tcp_bind(S, Port),  % find our own ephemeral Port
	term_to_atom(Term, Atom),

	(   udp_subnet_member(Address)  % talk only to your local subnet
	    -> safely(udp_send(S, Atom, Address, []))
	    ;  true).

% directed broadcast to a single listener
udp_broadcast(Term:To, _Scope, _Timeout) :-
	ground(Term), ground(To), !,
	udp_basic_broadcast(_S, _Port, Term, To),
	!.

% broadcast to all listeners
udp_broadcast(Term, Scope, _Timeout) :-
	ground(Term), !,
	udp_broadcast_service(Scope, Address),
	udp_basic_broadcast(_S, _Port, Term, Address),
	!.

% directed broadcast_request to a single listener
udp_broadcast(Term:Address, _Scope, Timeout) :-
	ground(Address), !,
        udp_basic_broadcast(S, Port, '$udp_request'(Term), Address),
	udp_br_collect_replies(S, Port, Timeout, Term:Address).

% broadcast_request to all listeners returning responder port-id
udp_broadcast(Term:From, Scope, Timeout) :-
	!, udp_broadcast_service(Scope, Address),
        udp_basic_broadcast(S, Port, '$udp_request'(Term), Address),
	udp_br_collect_replies(S, Port, Timeout, Term:From).

% broadcast_request to all listeners ignoring responder port-id
udp_broadcast(Term, Scope, Timeout) :-
	udp_broadcast(Term:_, Scope, Timeout).

udp_br_send_timeout(Port) :-
	udp_socket(S)
	  ~> tcp_close_socket(S),
	udp_send(S, '$udp_br_timeout', localhost:Port, []),
	!.

udp_br_collect_replies(S, Port, Timeout, Term:From) :-
	alarm(Timeout, udp_br_send_timeout(Port), Id, [remove(false)])
	  ~> remove_alarm(Id),

	tcp_setopt(S, dispatch(false)),

	repeat,
        udp_receive(S, Atom, From1, [as(atom)]),
        (   (Atom \== '$udp_br_timeout')
	    -> (From1 = From, safely(term_to_atom(Term, Atom)))
	    ;  (!, fail)).

%%	udp_host_to_address(?Service, ?Address) is nondet.
%
%   locates a UDP service by name. Service  is an atom or grounded term
%   representing the common name  of  the   service.  Address  is a UDP
%   address structure. A server may advertise   its  services by name by
%   including  the  fact,    udp:host_to_address(+Service,   +Address),
%   somewhere in its source. This predicate can  also be used to perform
%   reverse searches. That is it  will  also   resolve  an  Address to a
%   Service name.
%

udp_host_to_address(Host, Address) :-
	broadcast_request(udp_subnet(udp_host_to_address(Host, Address))).

%%	udp_initialize is semidet.
%   See udp:udp_initialize/0
%
%:- multifile udp:udp_stack_initialize/0.


%%      udp_broadcast_initialize(+IPAddress, +SubnetMask) is semidet.
%   causes any required runtime initialization to occur. At present,
%   proper operation of UDP broadcast depends on local information
%   that is not easily obtained mechanically. In order to determine
%   the appropriate UDP broadcast address, you must supply the
%   IPAddress and SubnetMask for the node that is running this module.
%   These data are supplied in the form of ip/4 terms. This is now
%   required to be included in an applications intialization directive.
%

udp_broadcast_initialize(IPAddress, Subnet) :-
	retractall(udp_broadcast_service(_,_)),
	retractall(udp_subnet_member(_)),

	udp_broadcast_address(IPAddress, Subnet, BroadcastAddr),
	assert(udp_broadcast_service(udp_subnet, BroadcastAddr:20005)),
	assert(udp_subnet_member(Address:_Port) :- udp_broadcast_address(Address, Subnet, BroadcastAddr)),

	start_udp_listener_daemon.
