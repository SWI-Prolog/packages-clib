/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_filesex,
          [ test_filesex/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(filesex)).

test_filesex :-
    run_tests([ directory_file_path,
                copy_file
              ]).


:- begin_tests(directory_file_path).

test(here, Path == 'test.pl') :-
    directory_file_path('.', 'test.pl', Path).
test(here, Path == 'test.pl') :-
    directory_file_path('', 'test.pl', Path).

:- end_tests(directory_file_path).

:- begin_tests(copy_file, [condition(\+ current_prolog_flag(windows, true))]).

test(mode, Mode == 0o600) :-
    tmp_file(copy, Dir), make_directory(Dir),
    directory_file_path(Dir, src, Src),
    directory_file_path(Dir, dst, Dst),
    setup_call_cleanup(open(Src, write, Out), format(Out, secret, []), close(Out)),
    chmod(Src, 0o600),
    copy_file(Src, Dst),
    files_ex:file_mode_(Dst, Mode0),
    Mode is Mode0 /\ 0o777,
    delete_directory_and_contents(Dir).
test(self, Size-E == 6-permission_error(overwrite, source_file, Dst)) :-
    tmp_file(copy, Dir), make_directory(Dir),
    directory_file_path(Dir, src, Src),
    setup_call_cleanup(open(Src, write, Out), format(Out, secret, []), close(Out)),
    catch(copy_file(Src, Dir), error(E, _), true),
    directory_file_path(Dir, src, Dst),
    size_file(Src, Size),
    delete_directory_and_contents(Dir).
test(symlink_dir, Entries == [l]) :-
    tmp_file(copy, Dir), make_directory(Dir),
    directory_file_path(Dir, src, Src), make_directory(Src),
    directory_file_path(Dir, dst, Dst),
    directory_file_path(Src, l, Link),
    link_file('..', Link, symbolic),
    copy_directory(Src, Dst),
    directory_files(Dst, All),
    subtract(All, ['.', '..'], Entries),
    directory_file_path(Dst, l, DLink),
    assertion(read_link(DLink, '..', _)),
    delete_directory_and_contents(Dir).

:- end_tests(copy_file).
