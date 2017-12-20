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

has_exe(Name) :-
    process:exe_options(Options),
    absolute_file_name(path(Name), _, [file_errors(fail)|Options]).

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
    getenv('TEMP', Dir),
    !.
tmp_dir('/tmp').

:- end_tests(process_create).


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
test(kill_gone, [ error(existence_error(process, PID)),
                  condition(\+current_prolog_flag(windows, true))]) :-
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

test(concurr, [condition(has_exe(cat))]) :-
    forall(between(1, 50, _),
           create_and_wait_once).

:- end_tests(process_threads).


