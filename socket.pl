/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
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
	    udp_send/4			% +Socket, +Data, +Sender, +Options
	  ]).
:- use_module(library(shlib)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These predicates are documented in the source-distribution of the package
`clib'.  See also the SWI-Prolog home-page at http://www.swi-prolog.org
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
%	@deprecated New code should use tcp_connect/3.

:- multifile
	tcp_connect_hook/3,
	tcp_connect_hook/4.


tcp_connect(Socket, Address, Read, Write) :-
	tcp_connect_hook(Socket, Address, Read, Write), !.
tcp_connect(Socket, Address, Read, Write) :-
	tcp_connect(Socket, Address),
	tcp_open_socket(Socket, Read, Write).

%%	tcp_connect(+Socket, +Address, -StreamPair) is det.
%
%	As tcp_connect/4, but creates a stream pair (see stream_pair/3).
%	The main advantage of having a single  handle is that it is much
%	easier to safely close the handles. If   two  handles need to be
%	closed, the user must be careful to   close the second handle if
%	closing the first one raises an exception.

tcp_connect(Socket, Address, StreamPair) :-
	tcp_connect_hook(Socket, Address, StreamPair0), !,
	StreamPair = StreamPair0.
tcp_connect(Socket, Address, StreamPair) :-
	tcp_connect(Socket, Address, Read, Write),
	stream_pair(StreamPair, Read, Write).


		 /*******************************
		 *	   COMPATIBILITY	*
		 *******************************/

tcp_fcntl(Socket, setfl, nonblock) :- !,
	tcp_setopt(Socket, nonblock).


		 /*******************************
		 *	  HANDLE MESSAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The C-layer generates exceptions of the  following format, where Message
is extracted from the operating system.

	error(socket_error(Message), _)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- multifile
	prolog:message/3.

prolog:message(error(socket_error(Message), _)) -->
	[ 'Socket error: ~w'-[Message] ].
