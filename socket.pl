/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(socket,
	  [ tcp_socket/1,		% -Socket
	    tcp_close_socket/1,		% +Socket
	    tcp_open_socket/3,		% +Socket, -Read, -Write
	    tcp_connect/2,		% +Socket, +Address
	    tcp_connect/3,		% +Socket, +Address, -StreamPair
	    tcp_connect/4,		% +Socket, +Address, -Read, -Write)
	    tcp_bind/2,			% +Socket, +Address
	    tcp_accept/3,		% +Master, -Slave, -PeerName
	    tcp_listen/2,		% +Socket, +BackLog
	    tcp_fcntl/3,		% +Socket, +Command, ?Arg
	    tcp_setopt/2,		% +Socket, +Option
	    tcp_host_to_address/2,	% ?HostName, ?Ip-nr
	    tcp_select/3,		% +Inputs, -Ready, +Timeout
	    gethostname/1,		% -HostName

	    tcp_open_socket/2,		% +Socket, -StreamPair

	    udp_socket/1,		% -Socket
	    udp_receive/4,		% +Socket, -Data, -Sender, +Options
	    udp_send/4,			% +Socket, +Data, +Sender, +Options

            negotiate_socks_connection/2% +DesiredEndpoint, +StreamPair
	  ]).
:- use_module(library(shlib)).
:- use_module(library(debug)).

/** <module> Network socket (TCP and UDP) library

This library forms the core of SWI-Prolog's support for networking.

@see These predicates are documented in   the source-distribution of the
package   `clib'.   See    also    the     SWI-Prolog    home-page    at
http://www.swi-prolog.org
*/

:- multifile
	tcp_connect_hook/3,
	tcp_connect_hook/4,
	try_proxy/4.

:- use_foreign_library(foreign(socket), install_socket).
:- public tcp_debug/1.			% set debugging.

%%	tcp_open_socket(+Socket, -Stream) is det.
%
%	Create streams to communicate to Socket.   If Socket is a master
%	socket (see tcp_bind/2), Stream should be used for tcp_accept/3.
%	If Socket is a connected (see  tcp_connect/2) or accepted socket
%	(see tcp_accept/3), Stream is unified  to   a  stream  pair (see
%	stream_pair/3) that can be used  for   reading  and writing. The
%	pair must be closed with close/1, which also closes the Socket.

tcp_open_socket(Socket, Stream) :-
	tcp_open_socket(Socket, In, Out),
	(   var(Out)
	->  Stream = In
	;   stream_pair(Stream, In, Out)
	).


		 /*******************************
		 *	HOOKABLE CONNECT	*
		 *******************************/

%%	tcp_connect(+Socket, +Address, -Read, -Write) is det.
%
%	Connect a (client) socket to Address and return a bi-directional
%	connection through the  stream-handles  Read   and  Write.  This
%	predicate may be hooked   by  defining socket:tcp_connect_hook/4
%	with the same signature. Hooking can be  used to deal with proxy
%	connections. E.g.,
%
%	    ==
%	    :- multifile socket:tcp_connect_hook/4.
%
%	    socket:tcp_connect_hook(Socket, Address, Read, Write) :-
%	        proxy(ProxyAdress),
%		tcp_connect(Socket, ProxyAdress),
%		tcp_open_socket(Socket, Read, Write),
%		proxy_connect(Address, Read, Write).
%	    ==
%
%	@deprecated New code should use tcp_connect/3 called as
%	tcp_connect(+Address, -StreamPair, +Options).

tcp_connect(Socket, Address, Read, Write) :-
	tcp_connect_hook(Socket, Address, Read, Write), !.
tcp_connect(Socket, Address, Read, Write) :-
	tcp_connect(Socket, Address),
	tcp_open_socket(Socket, Read, Write).



:-multifile
        network_proxy:find_proxy_for_url/3.



%%      tcp_connect(+Socket, +Address, -StreamPair) is det.
%%      tcp_connect(+Address, -StreamPair, +Options) is det.
%
%	This predicate has  two  modes   which  actually  perform  quite
%	different tasks. The +,+,-  mode  is   deprecated  and  does not
%	support proxies. It behaves like   tcp_connect/4,  but creates a
%	stream pair (see stream_pair/3). The main  advantage of having a
%	single handle is that it is  much   easier  to  safely close the
%	handles. If two handles need to  be   closed,  the  user must be
%	careful to close the second  handle   if  closing  the first one
%	raises an exception.
%
%	The +,-,+ mode does not  return   the  socket,  but does support
%	communication  via  proxies.  To   use    a   proxy,   the  hook
%	network_proxy:find_proxy_for_url/3 must be   defined.  Permitted
%	options are:
%
%          * bypass_proxy(+Boolean)
%	     Defaults to =false=. If =true=, do not attempt to use any
%	     proxies to obtain the connection
%
%          * nodelay(+Boolean)
%	     Defaults to =false=. If =true=, set nodelay on the
%	     resulting socket using tcp_setopt(Socket, nodelay)


tcp_connect(Address, StreamPair, Options) :-
        var(StreamPair), !,
        (   memberchk(bypass_proxy(true), Options)
	->  tcp_connect_direct(Address, Socket, StreamPair)
        ;   format(atom(URL), 'socket://~w', [Address]),
	    (   Address = Host:_
	    ->  true
	    ;   Host = Address
	    ),
	    (   network_proxy:find_proxy_for_url(URL, Host, ProxyList)
	    ->  try_proxies(ProxyList, Address, Socket, StreamPair)
	    ;   tcp_connect_direct(Address, Socket, StreamPair)
	    )
	),
        (   memberchk(nodelay(true), Options)
	->  tcp_setopt(Socket, nodelay)
	;   true
        ).


tcp_connect(Socket, Address, StreamPair) :-
	tcp_connect_hook(Socket, Address, StreamPair0), !,
	StreamPair = StreamPair0.
tcp_connect(Socket, Address, StreamPair) :-
	tcp_connect(Socket, Address, Read, Write),
	stream_pair(StreamPair, Read, Write).


tcp_connect_direct(Address, Socket, StreamPair):-
        tcp_socket(Socket),
        catch(tcp_connect(Socket, Address, StreamPair),
              Error,
              ( tcp_close_socket(Socket),
                throw(Error)
              )).

		 /*******************************
		 *	  PROXY SUPPORT		*
		 *******************************/

%%	try_proxies(ProxySpec, +Address, -Socket, -StreamPair)
%
%	Try a to establish a proxied connection to Address.  ProxySpec
%	is one of:
%
%	  $ A list of proxies :
%	  Each proxy is tried by calling the hook try_proxy/4 using
%	  a member of the list as first argument and passing the
%	  remaining arguments.  Errors from earlier proxies are
%	  printed using print_message/3 at level `warning`.  An
%	  error from the last proxy is passed to the caller.
%	  $ `direct` :
%	  $ socks(Host, Port) :


try_proxies([Last], Address, Socket, StreamPair) :- !,
        debug(proxy, 'Socket connecting via ~w~n', [Last]),
        try_proxy(Last, Address, Socket, StreamPair).
try_proxies([Proxy|_Proxies], Address, Socket, StreamPair) :-
        debug(proxy, 'Socket connecting via ~w~n', [Proxy]),
        catch(try_proxy(Proxy, Address, Socket, StreamPair),
              Error,
              ( print_message(warning, proxy_failed_to_respond(Proxy, Error)),
                fail
              )), !.
try_proxies([_Proxy|Proxies], Address, Socket, StreamPair) :-
        try_proxies(Proxies, Address, Socket, StreamPair).
try_proxy(direct, Address, Socket, StreamPair) :- !,
        tcp_connect_direct(Address, Socket, StreamPair).
try_proxy(socks(Host, Port), Address, Socket, StreamPair) :- !,
        tcp_connect_direct(Host:Port, Socket, StreamPair),
        catch(negotiate_socks_connection(Address, StreamPair),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).



		 /*******************************
		 *	   COMPATIBILITY	*
		 *******************************/

tcp_fcntl(Socket, setfl, nonblock) :- !,
	tcp_setopt(Socket, nonblock).



		 /*******************************
		 *	      SOCKS	        *
		 *******************************/

%%      negotiate_socks_connection(+DesiredEndpoint, +StreamPair) is det.
%
%	Negotiate  a  connection  to  DesiredEndpoint  over  StreamPair.
%	DesiredEndpoint should be in the form of either:
%
%          * hostname : port
%          * ip/4 : port
%
%       @error socks_error(Details) if the SOCKS negotiation failed.

negotiate_socks_connection(Host:Port, StreamPair):-
        format(StreamPair, '~s', [[0x5,    % Version 5
                                   0x1,    % 1 auth method supported
                                   0x0]]), % which is 'no auth'
        flush_output(StreamPair),
        get_byte(StreamPair, ServerVersion),
        get_byte(StreamPair, AuthenticationMethod),
        (   ServerVersion =\= 0x05
	->  throw(error(socks_error(invalid_version(5, ServerVersion)), _))
        ;   AuthenticationMethod =:= 0xff
	->  throw(error(socks_error(invalid_authentication_method(
					0xff,
					AuthenticationMethod)), _))
        ;   true
        ),
        (   Host = ip(A,B,C,D)
	->  AddressType = 0x1,			% IPv4 Address
            format(atom(Address), '~s', [[A, B, C, D]])
        ;   AddressType = 0x3,			% Domain
	    atom_length(Host, Length),
	    format(atom(Address), '~s~w', [[Length], Host])
        ),
        P1 is Port /\ 0xFF,
        P2 is Port >> 8,
        format(StreamPair, '~s~w~s', [[0x5,   % Version 5
                                       0x1,   % Please establish a connection
                                       0x0,   % reserved
                                       AddressType],
				      Address,
                                      [P2, P1]]),
        flush_output(StreamPair),
        get_byte(StreamPair, _EchoedServerVersion),
        get_byte(StreamPair, Status),
        (   Status =:= 0			% Established!
	->  get_byte(StreamPair, _Reserved),
            get_byte(StreamPair, EchoedAddressType),
            (   EchoedAddressType =:= 0x1
	    ->  get_byte(StreamPair, _),	% read IP4
                get_byte(StreamPair, _),
                get_byte(StreamPair, _),
                get_byte(StreamPair, _)
            ;   get_byte(StreamPair, Length),	% read host name
		forall(between(1, Length, _),
		       get_byte(StreamPair, _))
            ),
            get_byte(StreamPair, _),		% read port
            get_byte(StreamPair, _)
        ;   throw(error(socks_error(negotiation_rejected(Status)), _))
        ).


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The C-layer generates exceptions of the  following format, where Message
is extracted from the operating system.

	error(socket_error(Message), _)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	prolog:message//1,
	prolog:error_message//1.

prolog:message(proxy_failed_to_respond(Proxy, Error)) -->
	[ 'Network proxy ~p failed to respond: '-[Proxy] ],
	'$messages':translate_message(Error).

prolog:error_message(socket_error(Message)) -->
	[ 'Socket error: ~w'-[Message] ].
prolog:error_message(socks_error(Error)) -->
	socks_error(Error).

socks_error(invalid_version(Supported, Got)) -->
	[ 'SOCKS: unsupported version: ~p (supported: ~p)'-
	  [ Got, Supported ] ].
socks_error(invalid_authentication_method(Supported, Got)) -->
	[ 'SOCKS: unsupported authentication method: ~p (supported: ~p)'-
	  [ Got, Supported ] ].
socks_error(negotiation_rejected(Status)) -->
	[ 'SOCKS: connection failed: ~p'-[Status] ].

