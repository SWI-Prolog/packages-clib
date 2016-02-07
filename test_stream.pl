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
