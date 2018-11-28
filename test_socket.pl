/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
                              VU University Amsterdam
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
          [ test_socket/0,
            server/1,                   % +Port
            client/1                    % +Address
          ]).

:- asserta(user:file_search_path(foreign, '.')).

:- prolog_load_context(directory, D),
   asserta(user:file_search_path(library, D)),
   atom_concat(D, '/..', DD),
   asserta(user:file_search_path(library, DD)).
:- use_module(library(socket)).
:- use_module(library(streampool)).
:- use_module(library(debug)).

test_socket :-
    test_udp,
    test_tcp.

test_tcp :-
    make_server(Port, Socket),
    thread_create(run_server(Socket), Server, []),
    client(localhost:Port),
    thread_join(Server, Status),
    (   Status == true
    ->  true
    ;   format(user_error, 'Server exit-status: ~w~n', [Status]),
        fail
    ).


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
    tcp_accept(Socket, Slave, Peer),
    debug(connection, 'connect(~p)', [Peer]),
    tcp_open_socket(Slave, In, Out),
    set_stream(In, encoding(utf8)),
    set_stream(Out, encoding(utf8)),
    add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, Peer) :-
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
    client/2.

client(Address) :-
    tcp_socket(S),
    tcp_connect(S, Address),
    tcp_open_socket(S, In, Out),
    set_stream(In, encoding(utf8)),
    set_stream(Out, encoding(utf8)),
    asserta(client(In, Out)),
    test,
    retract(client(In, Out)),
    close(Out),
    close(In).

echo(echo-1) :-
    X = 'Hello World',
    client(In, Out),
    tcp_send(Out, echo(X)),
    tcp_reply(In, X).
echo(echo-2) :-
    findall(A, between(0, 100000, A), X),
    client(In, Out),
    tcp_send(Out, echo(X)),
    tcp_reply(In, X).

slow(slow-1) :-
    client(In, Out),
    tcp_send(Out, wait(2)),
    tcp_reply(In, yes).
slow(slow-1) :-
    client(In, Out),
    tcp_send(Out, slow_read),
    findall(A, between(0, 100000, A), X),
    tcp_send(Out, X),
    tcp_reply(In, X).

quit(quit-1) :-
    client(In, Out),
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
    repeat,
        udp_receive(Socket, Data, From, [as(atom)]),
        thread_send_message(Queue, got(Data, From)),
        Data == quit,
        !,
        tcp_close_socket(Socket).

receiver(Port, ThreadId) :-
    thread_self(Me),
    udp_socket(S),
    tcp_bind(S, Port),
    thread_create(receive_loop(S, Me), ThreadId, []).

test_udp :-
    format(user_error, 'Running test set "udp"', []),
    (   catch(run_udp, E, true)
    ->  (   var(E)
        ->  format(user_error, ' . done~n', [])
        ;   print_message(error, E)
        )
    ;   format(user_error, 'FAILED~n', [])
    ).

run_udp :-
    receiver(Port, ThreadId),
    udp_socket(S),
    udp_send(S, 'hello world', localhost:Port, []),
    thread_get_message(got(X, _)),
    udp_send(S, 'quit', localhost:Port, []),
    thread_get_message(got(Q, _)),
    thread_join(ThreadId, Exit),
    tcp_close_socket(S),
    assertion(X=='hello world'),
    assertion(Q=='quit'),
    assertion(Exit==true),
    !.


                 /*******************************
                 *        TEST MAIN-LOOP        *
                 *******************************/

testset(echo).
testset(slow).
testset(quit).

:- dynamic
    failed/1,
    blocked/2.

test :-
    retractall(failed(_)),
    retractall(blocked(_,_)),
    forall(testset(Set), runtest(Set)),
    report_blocked,
    report_failed.

report_blocked :-
    findall(Head-Reason, blocked(Head, Reason), L),
    (   L \== []
    ->  format('~nThe following tests are blocked:~n', []),
        (   member(Head-Reason, L),
            format('    ~p~t~40|~w~n', [Head, Reason]),
            fail
        ;   true
        )
    ;   true
    ).
report_failed :-
    findall(X, failed(X), L),
    length(L, Len),
    (   Len > 0
    ->  format('~n*** ~w tests failed ***~n', [Len]),
        fail
    ;   format('~nAll tests passed~n', [])
    ).

runtest(Name) :-
    format('Running test set "~w" ', [Name]),
    flush,
    functor(Head, Name, 1),
    nth_clause(Head, _N, R),
    clause(Head, _, R),
    (   catch(Head, Except, true)
    ->  (   var(Except)
        ->  put(.), flush
        ;   Except = blocked(Reason)
        ->  assert(blocked(Head, Reason)),
            put(!), flush
        ;   test_failed(R, Except)
        )
    ;   test_failed(R, fail)
    ),
    fail.
runtest(_) :-
    format(' done.~n').

test_failed(R, Except) :-
    clause(Head, _, R),
    functor(Head, Name, 1),
    arg(1, Head, TestName),
    clause_property(R, line_count(Line)),
    clause_property(R, file(File)),
    (   Except == failed
    ->  format('~N~w:~d: Test ~w(~w) failed~n',
               [File, Line, Name, TestName])
    ;   message_to_string(Except, Error),
        format('~N~w:~d: Test ~w(~w):~n~t~8|ERROR: ~w~n',
               [File, Line, Name, TestName, Error])
    ),
    assert(failed(Head)).

blocked(Reason) :-
    throw(blocked(Reason)).


