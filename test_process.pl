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
:- use_module(library(process)).

test_process :-
    run_tests([ process_create,
                'process_create[fork]',
                'process_create[vfork]',
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

:- dynamic recorded_clause/3.

record_clause(Tag, Position, Clause) :-
    \+ recorded_clause(Tag, Position, _),
    Clause \= (:- _Directive),
    assertz(recorded_clause(Tag, Position, Clause)).

record_clauses_as(Tag) :-
    nb_setval(record_clause_tag, Tag).

stop_recording_clauses :-
    nb_delete(record_clause_tag).

term_expansion(Clause, _) :-
    nb_current(record_clause_tag, Tag),
    prolog_load_context(term_position, TermPos),
    once(record_clause(Tag, TermPos, Clause)),
    fail.
term_expansion(replay_recorded_clauses(Tag), Clauses) :-
    findall(Clause, recorded_clause(Tag, _, Clause), Clauses).

:- begin_tests(process_create, [sto(rational_trees)]).
:- record_clauses_as(process_create).

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
    open('/dev/null', read, In, [type(binary)]),
    process_create(path(wc),
                   ['-c'],
                   [stdin(stream(In)), stdout(pipe(Out))]),
    close(In),
    read_process(Out, X0),
    split_string(X0, "", " \r\n", [X]).
test(read_error, [condition(has_exe(sh)),X == 'error\n']) :-
    process_create(path(sh),
                   ['-c', 'echo "error" 1>&2'],
                   [stderr(pipe(Out))]),
    read_process(Out, X).
test(read_extra, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true)),X == 'fd-3\n']) :-
    process_create(path(sh),
                   ['-c', 'echo "fd-3" 1>&3'],
                   [extra_streams([from_child(pipe(Out))])]),
    read_process(Out, X).
test(null_extra, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true))]) :-
    process_create(path(sh),
                   ['-c', 'echo "THIS IS AN ERROR" 1>&3'],
                   [extra_streams([from_child(null)])]).
test(write_extra, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true)),X == 'hello fd4\n']) :-
    process_create(path(sh),
                   ['-c', 'cat 0<&4'],
                   [stdout(pipe(Out)), extra_streams([_,to_child(pipe(In))])]),
    format(In, 'hello fd4~n', []),
    close(In),
    read_process(Out, X).
test(separate_error, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true)),X == 'separated error\n']) :-
    process_create(path(sh),
                   ['-c', 'echo "THIS IS AN ERROR" 1>&2; echo "separated error" 1>&3'],
                   [stderr(null), extra_streams([from_child(pipe(Out))])]),
    read_process(Out, X).
test(auto_detect_read, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true)),X == "0"]) :-
    open('/dev/null', read, In, [type(binary)]),
    process_create(path(sh),
                   ['-c', 'wc -c 0<&3'],
                   [stdout(pipe(Out)), extra_streams([stream(In)])]),
    close(In),
    read_process(Out, X0),
    split_string(X0, "", " \r\n", [X]).
test(auto_detect_write, [condition(has_exe(sh)),condition(\+current_prolog_flag(windows, true))]) :-
    open('/dev/null', write, Out, [type(binary)]),
    process_create(path(sh),
                   ['-c', 'echo "THIS IS AN ERROR" 1>&3'],
                   [extra_streams([stream(Out)])]),
    close(Out).
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
test(std_env, [condition((has_exe(sh), getenv('USER', _))),
               forall(member(Opts,[ [], [environment([])], [environment(['TEST'=testing])] ])), X == User]) :-
    getenv('USER', User),
    process_create(path(sh),
                   ['-c', 'echo -n $USER'],
                   [ stdout(pipe(Out))|Opts ]),
    read_process(Out, X).
test(set_env, [condition(has_exe(sh)), forall(member(Opt,[environment(['TEST'=test_set_env]),
                                                          environment(['FOO'=bar, 'TEST'=test_set_env]),
                                                          env(['TEST'=test_set_env]),
                                                          env(['FOO'=bar, 'TEST'=test_set_env]) ])), X == test_set_env]) :-
    process_create(path(sh),
                   ['-c', 'echo -n $TEST'],
                   [ stdout(pipe(Out)), Opt ]),
    read_process(Out, X).
test(replace_env, [condition((has_exe(sh), getenv('USER', _))),
               forall(member(Opt,[ env([]), env(['TEST'=testing]) ])), X == '']) :-
    process_create(path(sh),
                   ['-c', 'echo -n $USER'],
                   [ stdout(pipe(Out)), Opt ]),
    read_process(Out, X).

tmp_dir(Dir) :-
    current_prolog_flag(tmp_dir, Dir).

:- stop_recording_clauses.
:- end_tests(process_create).

:- begin_tests('process_create[fork]', [sto(rational_trees), setup(process_set_method(fork)), cleanup(process_set_method(spawn))]).
replay_recorded_clauses(process_create).
:- end_tests('process_create[fork]').

:- begin_tests('process_create[vfork]', [sto(rational_trees), setup(process_set_method(vfork)), cleanup(process_set_method(spawn))]).
replay_recorded_clauses(process_create).
:- end_tests('process_create[vfork]').


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


