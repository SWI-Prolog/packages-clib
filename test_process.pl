/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2014, University of Amsterdam,
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


:- module(test_process,
	  [ test_process/0
	  ]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(process)).

test_process :-
    run_tests([ process_create,
		process_wait,
		process_threads
	      ]).

read_process(In, Text) :-
    read_stream_to_codes(In, Codes),
    close(In),
    atom_codes(Text, Codes).

has_exe(Name) :-
    process_which(path(Name), _).

%!  prolog_command(+Goal, -Exe, -Argv) is det.
%
%   Run Goal using a fresh Prolog process.  Used to create a process
%   without relying on the tools installed on the target platform.

prolog_command(Goal, Exe, ['-f', none, '-g', Goal, '-t', halt]) :-
    current_prolog_flag(executable, Exe).

:- begin_tests(process_create, [sto(rational_trees)]).

test(echo, [condition(has_exe(true))]) :-
    process_create(path(true), [], []).
test(null_input, [condition(has_exe(cat)), Codes == []]) :-
    process_create(path(cat), [], [stdin(null), stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    close(Out).
test(null_output, [condition(has_exe(sh))]) :-
    process_create(path(sh),
		   ['-c', 'echo THIS IS AN ERROR'],
		   [stdout(null)]).
test(null_error, [condition(has_exe(sh))]) :-
    process_create(path(sh),
		   ['-c', 'echo "THIS IS AN ERROR" 1>&2'],
		   [stderr(null)]).
test(stream_input, [condition(has_exe(wc)), X == "0"]) :-
    process_create(path(wc),
		   ['-c'],
		   [stdin(null), stdout(pipe(Out))]),
    read_process(Out, X0),
    split_string(X0, "", " \r\n", [X]).
test(read_error, [condition(has_exe(sh)),X == 'error\n']) :-
    process_create(path(sh),
		   ['-c', 'echo "error" 1>&2'],
		   [stderr(pipe(Out))]),
    read_process(Out, X).
test(echo, [condition(has_exe(sh)), X == 'hello\n']) :-
    process_create(path(sh),
		   ['-c', 'echo hello'],
		   [ stdout(pipe(Out))
		   ]),
    read_process(Out, X).
test(lwr, [condition(has_exe(tr)), X == 'HELLO']) :-
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
test(cwd, [true, condition(( current_prolog_flag(windows, true),
			     \+current_prolog_flag(wine_version, _)))]) :-
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
    current_prolog_flag(tmp_dir, Dir).

test(detached) :-                       % must not wait for the process
    prolog_command('sleep(10)', Exe, Argv),
    get_time(T0),
    process_create(Exe, Argv,
		   [ detached(true),
		     stdin(null),
		     stdout(null),
		     stderr(null)
		   ]),
    get_time(T1),
    assertion(T1-T0 < 5).
test(stream_keeps_fd, [condition(has_exe(sh)), X == 'a\nb\n']) :-
    Goal = 'process_create(path(sh), [\'-c\', \'echo a\'], \c
                           [stdout(stream(user_error))]), \c
            process_create(path(sh), [\'-c\', \'echo b >&2\'], [])',
    process_create(prolog(swipl),
                   ['-q', '-f', none, '-g', 'use_module(library(process))',
                    '-g', Goal, '-t', halt],
                   [ stdout(null),
                     stderr(pipe(Err))
                   ]),
    read_process(Err, X).
test(nul_arg, [condition(has_exe(true)),
               error(representation_error(nul_terminated_string(_)))]) :-
    process_create(path(true), ['a\u0000b'], []).
test(kill_0, error(domain_error(not_less_than_one, 0))) :-
    process_kill(0).
% On Windows the arguments are joined into a command line that the
% program splits again.  Quotes and backslashes must survive this.
test(args_round_trip, Got == Args) :-
    Args = [ 'a b', 'a"b', 'a\\', 'a b\\', next, '', 'a"" b',
             'x\\"y', 'it''s', '&|<>^()'
           ],
    current_prolog_flag(executable, Exe),
    process_create(Exe,
                   [ '-q', '-f', none,
                     '-g', 'current_prolog_flag(argv,A),writeq(A),write(.),nl',
                     '-t', halt, '--'
                   | Args
                   ],
                   [ stdout(pipe(Out)) ]),
    call_cleanup(read_term(Out, Got, []), close(Out)).
% A batch file is run by cmd.exe: metacharacters are quoted and
% characters that cannot be passed safely are refused.
test(batch_file, [ condition(current_prolog_flag(windows, true)),
                   X == '["x&echo INJECTED"]'
                 ]) :-
    with_batch_file(Bat,
                    ( process_create(Bat, ['x&echo INJECTED'],
                                     [stdout(pipe(Out))]),
                      read_process(Out, X0),
                      split_string(X0, "", " \r\n", [S]),
                      atom_string(X, S)
                    )).
test(batch_file, [ condition(current_prolog_flag(windows, true)),
                   error(domain_error(batch_file_argument, _))
                 ]) :-
    with_batch_file(Bat, process_create(Bat, ['%PATH%'], [])).

:- end_tests(process_create).

:- meta_predicate with_batch_file(-, 0).

with_batch_file(Bat, Goal) :-
    tmp_file_stream(Bat, S, [extension(bat), encoding(ascii)]),
    format(S, "@echo off\r\necho [%1]\r\n", []),
    close(S),
    call_cleanup(Goal, delete_file(Bat)).


:- begin_tests(process_wait, [sto(rational_trees)]).

test(wait_ok, [condition(has_exe(sh)), X == exit(0)]) :-
    process_create(path(sh), ['-c', 'exit 0'], [process(PID)]),
    process_wait(PID, X).
test(wait_ok, [condition(has_exe(sh)), X == exit(42)]) :-
    process_create(path(sh), ['-c', 'exit 42'], [process(PID)]),
    process_wait(PID, X).
test(kill_ok, [ X == killed(9),
		condition(\+current_prolog_flag(windows, true))]) :-
    process_create(path(sleep), [2], [process(PID)]),
    process_kill(PID, 9),
    process_wait(PID, X).
test(kill_ok, [ X = exit(_),
		condition((current_prolog_flag(windows, true),
			   has_exe(sleep)))
	      ]) :-
    process_create(path(sleep), [2], [process(PID)]),
    process_kill(PID, 9),
    process_wait(PID, X).
% Seems to be able to kill itself on MacOS if this is ran
% concurrently.  Too quick reuse of PIDs?
test(kill_gone, [ error(existence_error(process, PID)),
		  condition((\+ current_prolog_flag(windows, true), \+ current_prolog_flag(apple,true)))
		]) :-
    process_create(path(sleep), [2], [process(PID)]),
    process_kill(PID),
    process_wait(PID, X),
    assertion(X == killed(15)),
    process_kill(PID).
test(wait_timeout, [ condition(has_exe(sleep)), X = timeout ]) :-
    process_create(path(sleep), [2], [process(PID)]),
    (   current_prolog_flag(windows, true)
    ->  TMO = 0.1
    ;   TMO = 0
    ),
    process_wait(PID, X, [timeout(TMO)]),
    process_kill(PID, 9),
    process_wait(PID, _).

:- end_tests(process_wait).

:- begin_tests(process_threads,
	       [ sto(rational_trees),
		 condition(current_prolog_flag(threads, true))
	       ]).

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

test(concurr, [condition(has_exe(cat))]) :-
    forall(between(1, 50, _),
	   create_and_wait_once).

:- end_tests(process_threads).
