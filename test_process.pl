:- module(test_process,
	  [ test_process/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(process).

test_process :-
	run_tests([ process_create,
		    process_wait,
		    process_threads
		  ]).

read_process(In, Text) :-
	read_stream_to_codes(In, Codes),
	close(In),
	atom_codes(Text, Codes).

:- begin_tests(process_create, [sto(rational_trees)]).

test(echo, true) :-
	process_create(path(true), [], []).
test(null_input, Codes == []) :-
	process_create(path(cat), [], [stdin(null), stdout(pipe(Out))]),
	read_stream_to_codes(Out, Codes),
	close(Out).
test(null_output, true) :-
	process_create(path(sh),
		       ['-c', 'echo THIS IS AN ERROR'],
		       [stdout(null)]).
test(null_error, true) :-
	process_create(path(sh),
		       ['-c', 'echo "THIS IS AN ERROR" 1>&2'],
		       [stderr(null)]).
test(read_error, X == 'error\n') :-
	process_create(path(sh),
		       ['-c', 'echo "error" 1>&2'],
		       [stderr(pipe(Out))]),
	read_process(Out, X).
test(echo, X == 'hello\n') :-
	process_create(path(sh),
		       ['-c', 'echo hello'],
		       [ stdout(pipe(Out))
		       ]),
	read_process(Out, X).
test(lwr, X == 'HELLO') :-
	process_create(path(tr), [hello, 'HELLO'], % a-z A-Z is non-portable
		       [ stdin(pipe(In)),
			 stdout(pipe(Out))
		       ]),
	format(In, hello, []),
	close(In),
	read_process(Out, X).
test(cwd, [true, condition(\+current_prolog_flag(windows, true))]) :-
	tmp_dir(Tmp),
	process_create(path(pwd), [],
		       [ stdout(pipe(Out)),
			 cwd(Tmp)
		       ]),
	read_process(Out, CWD0),
	normalize_space(atom(CWD), CWD0),
	same_file(CWD, Tmp).
test(cwd, [true, condition(current_prolog_flag(windows, true))]) :-
	tmp_dir(Tmp),
	getenv('COMSPEC', Shell),
	process_create(Shell, ['/c', cd],
		       [ stdout(pipe(Out)),
			 cwd(Tmp)
		       ]),
	read_process(Out, CWD0),
	normalize_space(atom(CWD), CWD0),
	same_file(CWD, Tmp).

tmp_dir(Dir) :-
	getenv('TEMP', Dir), !.
tmp_dir('/tmp').

:- end_tests(process_create).


:- begin_tests(process_wait, [sto(rational_trees)]).

test(wait_ok, X == exit(0)) :-
	process_create(path(sh), ['-c', 'exit 0'], [process(PID)]),
	process_wait(PID, X).
test(wait_ok, X == exit(42)) :-
	process_create(path(sh), ['-c', 'exit 42'], [process(PID)]),
	process_wait(PID, X).
test(kill_ok, [ X == killed(9),
		condition(\+current_prolog_flag(windows, true))]) :-
	process_create(path(sleep), [2], [process(PID)]),
	process_kill(PID, 9),
	process_wait(PID, X).
test(kill_ok, [ X = exit(_),
		condition(current_prolog_flag(windows, true))]) :-
	process_create(path(sleep), [2], [process(PID)]),
	process_kill(PID, 9),
	process_wait(PID, X).
test(wait_timeout, [ X = timeout ]) :-
	process_create(path(sleep), [2], [process(PID)]),
	(   current_prolog_flag(windows, true)
	->  TMO = 0.1
	;   TMO = 0
	),
	process_wait(PID, X, [timeout(TMO)]),
	process_kill(PID, 9),
	process_wait(PID, _).

:- end_tests(process_wait).

:- begin_tests(process_threads, [sto(rational_trees)]).

join(Id) :-
	thread_join(Id, Status),
	Status == true.

thread_create_and_wait(Id) :-
	thread_create(create_and_wait, Id, []).

create_and_wait :-
	process_create(path(cat), [],
		       [ stdin(pipe(ToDOT)),
			 stdout(pipe(XDotOut))
		       ]),
	Term = hello(world),
	format(ToDOT, '~q.~n', [Term]),
	close(ToDOT),
	read(XDotOut, Term2),
	assertion(Term2 =@= Term),
	read(XDotOut, EOF),
	assertion(EOF == end_of_file),
	close(XDotOut).

create_and_wait_once :-
	length(List, 2),
	maplist(thread_create_and_wait, List),
	maplist(join, List).

/* See create_pipes() in process.c */

test(concurr, true) :-
	forall(between(1, 50, _),
	       create_and_wait_once).

:- end_tests(process_threads).


