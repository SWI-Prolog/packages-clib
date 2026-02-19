/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(test_udp_sockets,
          [ test_udp_sockets/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(socket)).

test_udp_sockets :-
    run_tests([ udp_sockets
              ]).

:- begin_tests(udp_sockets).

test(atom, Got == got(hello)) :-
    trip(hello, Got, [as(atom)]).
test(atom, Got == got('he\u0000llo')) :-
    trip('he\u0000llo', Got, [as(atom)]).
test(string, Got == got("hello")) :-
    trip("hello", Got, [as(string)]).
test(string, Got == got(`hello`)) :-
    trip(`hello`, Got, [as(codes)]).
test(string, Got == got(String)) :-
    numlist(0, 1000, Codes),
    string_codes(String, Codes),
    trip(String, Got, [as(string),encoding(utf8)]).
test(string, Got == got(hello(world))) :-
    trip(hello(world), Got, [as(term)]).
test(string, Got == got(Term)) :-
    Term = 'hel\\o',
    trip(Term, Got, [as(term)]).
test(string, Got == got(hello(String))) :-
    numlist(0, 1000, Codes),
    string_codes(String, Codes),
    trip(hello(String), Got, [as(term),encoding(utf8)]).

:- end_tests(udp_sockets).

trip(In, Out, Options) :-
    start_receiver(Port, Options),
    send_rec(Port, In, Out, Options),
    stop_receiver(Port).

send_rec(Port, Message, Reply, Options) :-
    udp_socket(S),
    udp_send(S, Message, localhost:Port, Options),
    tcp_close_socket(S),
    thread_self(Me),
    thread_get_message(Me, Reply, [timeout(60)]).

:- dynamic
    receiver/3.

start_receiver(Port, As) :-
    udp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    thread_self(Me),
    thread_create(receive_loop(Me, Socket, As), Id, []),
    asserta(receiver(Socket, Port, Id)).

receive_loop(Me, Socket, Options) :-
    repeat,
      catch(udp_receive(Socket, Data, _From, Options), E, true),
      (   var(E)
      ->  thread_send_message(Me, got(Data))
      ;   thread_send_message(Me, error(E))
      ),
      ends(Data),
      !.

ends('quit').
ends("quit").
ends(`quit`).

stop_receiver(Port) :-
    send_rec(Port, quit, _Reply, []),
    (   receiver(Socket, Port, Thread),
        tcp_close_socket(Socket),
        thread_join(Thread),
        fail
    ;   true
    ).
