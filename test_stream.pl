/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(test_stream,
	  [ test_stream/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(prolog_stream).

test_stream :-
	run_tests([ prolog_stream
		  ]).

:- thread_local
	msg/1,
	data/2,
	error/2.

stream_read(Stream, _) :-
	retract(error(Stream, E)), !,
	throw(E).
stream_read(Stream, String) :-
	debug(event, "Got ~q", [stream_read(Stream, String)]),
	(   retract(data(Stream, String))
	->  true
	;   String = ""
	),
	debug(event, "Reply ~q", [stream_read(Stream, String)]).

stream_write(Stream, _String) :-
	retract(error(Stream, E)), !,
	throw(E).
stream_write(Stream, String) :-
	debug(event, "Got ~q", [stream_write(Stream, String)]),
	assertz(msg(write(Stream, String))).

stream_close(Stream) :-
	retract(error(Stream, E)), !,
	throw(E).
stream_close(Stream) :-
	assertz(msg(close(Stream))).

messages(Messages) :-
	findall(Msg, retract(msg(Msg)), Messages).

set_data(Stream, Data) :-
	assertz(data(Stream, Data)).

set_error(Stream, Error) :-
	assertz(error(Stream, Error)).

clean :-
	retractall(msg(_)),
	retractall(data(_,_)),
	retractall(error(_,_)).

:- begin_tests(prolog_stream, [setup(clean),sto(rational_trees)]).

test(write, Messages == [write(Out, "Hello world"), close(Out)]) :-
	open_prolog_stream(test_stream, write, Out, []),
	write(Out, 'Hello world'),
	close(Out),
	messages(Messages).
test(read, Reply == "Hello world") :-
	open_prolog_stream(test_stream, read, In, []),
	set_data(In, "Hello world"),
	read_string(In, _, Reply),
	close(In).
test(error, Messages = [close(_)]) :-
	open_prolog_stream(test_stream, write, Out, []),
	set_error(Out, error(type_error(atom, 1),_)),
	catch((write(Out, 'Hello world'),
	       close(Out)),
	       Error, true),
	assertion(subsumes_term(error(type_error(atom,1),_), Error)),
	messages(Messages).
test(error, Messages == []) :-
	open_prolog_stream(test_stream, write, Out, []),
	set_error(Out, error(type_error(atom, 1),_)),
	catch(close(Out), Error, true),
	assertion(subsumes_term(error(type_error(atom,1),_), Error)),
	messages(Messages).

:- end_tests(prolog_stream).
