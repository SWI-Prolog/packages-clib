/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(test_af_unix,
          [ test_af_unix/0
          ]).
:- use_module(library(socket)).

:- if(current_predicate(unix_domain_socket/1)).
:- use_module(library(debug)).
:- use_module(library(readutil)).

test_af_unix :-
    File = "/tmp/x",
    server(File, Tid),
    client(File),
    thread_signal(Tid, throw(stop)),
    thread_join(Tid, Status),
    assertion(Status == exception(stop)).

server(File, Thread) :-
    (   access_file(File, exist)
    ->  delete_file(File)
    ;   true
    ),
    unix_domain_socket(S),
    tcp_bind(S, File),
    !,
    tcp_listen(S, 5),
    tcp_open_socket(S, AcceptFd, _),
    thread_create(dispatch(AcceptFd), Thread).

dispatch(AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    thread_create(process_client(Socket, Peer), _,
                  [ detached(true)
                  ]),
    dispatch(AcceptFd).

process_client(Socket, Peer) :-
    debug(af_unix, "Connected from ~p~n", [Peer]),
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        handle_service(StreamPair),
        close(StreamPair)).

handle_service(Stream) :-
    read_line_to_string(Stream, String),
    format(Stream, '~s~n', [String]),
    flush_output(Stream),
    (   String == "bye"
    ->  true
    ;   handle_service(Stream)
    ).

client(File) :-
    unix_domain_socket(Socket),
    tcp_connect(Socket, File),
    tcp_open_socket(Socket, Stream),
    ping(Stream, "Hello world"),
    ping(Stream, "bye"),
    close(Stream).

ping(Stream, Data) :-
    format(Stream, '~s~n', [Data]),
    flush_output(Stream),
    read_line_to_string(Stream, Reply),
    assertion(Data == Reply).

:- else.

test_af_unix.

:- endif.
