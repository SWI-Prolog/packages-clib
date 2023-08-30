/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2023, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_socket,
          [ test_socket/0
          ]).
:- use_module(library(socket)).
:- use_module(library(streampool)).
:- use_module(library(debug)).
:- use_module(library(plunit)).

test_socket :-
    run_tests([udp, tcp, ip_name]).

:- begin_tests(tcp).

test(hello) :-
    tcp_test_run(echo(hello)).
test(large) :-
    tcp_test_run(echo(large)).
test(wait) :-
    tcp_test_run(slow(wait)).
test(wait_large) :-
    tcp_test_run(slow(wait_large)).

tcp_test_run(Test) :-
    make_server(Port, Socket),
    thread_create(run_server(Socket), Server, []),
    client(Test, localhost:Port),
    client(quit, localhost:Port),
    thread_join(Server).

:- end_tests(tcp).

                 /*******************************
                 *             SERVER           *
                 *******************************/

server(Port) :-
    make_server(Port, Socket),
    run_server(Socket).

run_server(Socket) :-
    tcp_open_socket(Socket, In, _Out),
    add_stream_to_pool(In, accept(Socket)),
    stream_pool_main_loop.

make_server(Port, Socket) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5).

accept(Socket) :-
    debug(pool, 'Calling ~p from pool', [accept(Socket)]),
    tcp_accept(Socket, Slave, Peer),
    debug(connection, 'connect(~p)', [Peer]),
    tcp_open_socket(Slave, In, Out),
    set_stream(In, encoding(utf8)),
    set_stream(Out, encoding(utf8)),
    add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, Peer) :-
    debug(pool, 'Calling ~p from pool', [client(In, Out, Peer)]),
    read(In, Term),
    (   Term == end_of_file
    ->  debug(connection, 'close(~p)', [Peer]),
        close(In),
        close(Out)
    ;   (   catch(action(Term, In, Out), E, true)
        ->  (   var(E)
            ->  true
            ;   tcp_send(Out, exception(E))
            )
        ;   tcp_send(Out, no)
        )
    ).

                 /*******************************
                 *            ACTION            *
                 *******************************/

action(echo(X), _In, Out) :-
    tcp_send(Out, X).
action(wait(X), _In, Out) :-
    sleep(X),
    tcp_send(Out, yes).
action(slow_read, In, Out) :-
    sleep(2),
    read(In, Term),
    tcp_send(Out, Term).
action(quit, _In, Out) :-
    close_stream_pool,
    tcp_send(Out, quitted).


                 /*******************************
                 *          CLIENT SIDE         *
                 *******************************/

:- dynamic
    client_streams/2.

client(Action, Address) :-
    tcp_socket(S),
    tcp_connect(S, Address),
    tcp_open_socket(S, In, Out),
    set_stream(In, encoding(utf8)),
    set_stream(Out, encoding(utf8)),
    asserta(client_streams(In, Out)),
    call(Action),
    retract(client_streams(In, Out)),
    close(Out),
    close(In).

echo(hello) :-
    X = 'Hello World',
    client_streams(In, Out),
    tcp_send(Out, echo(X)),
    tcp_reply(In, X).
echo(large) :-
    findall(A, between(0, 100000, A), X),
    client_streams(In, Out),
    tcp_send(Out, echo(X)),
    tcp_reply(In, X).

slow(wait) :-
    client_streams(In, Out),
    tcp_send(Out, wait(2)),
    tcp_reply(In, yes).
slow(wait_large) :-
    client_streams(In, Out),
    tcp_send(Out, slow_read),
    findall(A, between(0, 100000, A), X),
    tcp_send(Out, X),
    tcp_reply(In, X).

quit :-
    client_streams(In, Out),
    tcp_send(Out, quit),
    tcp_reply(In, quitted).


                 /*******************************
                 *            UTIL              *
                 *******************************/

tcp_send(Out, Term) :-
    format(Out, '~q.~n', [Term]),
    flush_output(Out).

tcp_reply(In, Reply) :-
    read(In, Term),
    reply(Term, In, Reply).

reply(exception(E), _, _) :-
    throw(E).
reply(T, _, T).

                 /*******************************
                 *             UDP              *
                 *******************************/

receive_loop(Socket, Queue) :-
    thread_send_message(Queue, ready),
    repeat,
        udp_receive(Socket, Data, From, [as(atom)]),
        debug(udp, 'received ~p from ~p', [Data, From]),
        thread_send_message(Queue, got(Data, From)),
        Data == quit,
        !,
        tcp_close_socket(Socket).

receiver(Port, ThreadId) :-
    thread_self(Me),
    udp_socket(S),
    tcp_bind(S, Port),
    thread_create(receive_loop(S, Me), ThreadId, []),
    thread_get_message(ready).

run_udp :-
    receiver(Port, ThreadId),
    setup_call_cleanup(
        udp_socket(S),
        ( udp_ping(S, Port, 'Hello world'),
          udp_ping(S, Port, quit)
        ),
        ( tcp_close_socket(S),
          thread_join(ThreadId)
        )).

udp_ping(S, Port, Msg) :-
    thread_self(Me),
    between(0, 2, _),
      udp_send(S, Msg, '127.0.0.1':Port, []),
      debug(udp, 'Sent ~p to port ~p', [Msg, Port]),
      thread_get_message(Me, got(Reply, FromHost:FromPort), [timeout(0.1)]),
      !,
      assertion(Reply == Msg),
      assertion(integer(FromPort)),
      assertion(FromHost = ip(_,_,_,_)).

:- begin_tests(udp).

test(udp) :-
    run_udp.

:- end_tests(udp).

:- begin_tests(ip_name).

term_expansion(
    ip(Name, IP),
    [ (test(ip_name, N == Name) :- ip_name(IP, N)),
      (test(ip_name, I == IP)   :- ip_name(I, Name))
    ]).

ip('1.2.3.4',      ip(1,2,3,4)).
ip('::',           ip(0,0,0,0,0,0,0,0)).
ip('1::',          ip(1,0,0,0,0,0,0,0)).
ip('::1',          ip(0,0,0,0,0,0,0,1)).
ip('::2',          ip(0,0,0,0,0,0,0,2)).
ip('1::2',         ip(1,0,0,0,0,0,0,2)).
ip('1::3:0:0:2',   ip(1,0,0,0,3,0,0,2)).
ip('1::3:4:0:0:2', ip(1,0,0,3,4,0,0,2)).
ip('abcd::',       ip(43981,0,0,0,0,0,0,0)).

:- end_tests(ip_name).
