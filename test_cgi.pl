/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2022, VU University, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_cgi, [test_cgi/0]).

:- use_module(library(plunit)).
:- use_module(library(cgi)).
:- use_module(library(uri)).
:- use_module(library(process)).
:- use_module(library(readutil)).

test_cgi :-
    run_tests([ cgi
              ]).

trip(FormIn, FormOut) :-
    uri_query_components(String, FormIn),
    setenv('QUERY_STRING', String),
    cgi_get_form(FormOut).

n_list_of(0, _, []) :- !.
n_list_of(I, H, [H|T]) :-
    I2 is I - 1,
    n_list_of(I2, H, T).

:- begin_tests(cgi, [condition(\+ current_prolog_flag(windows, true))]).

test(atom, In == Out) :-
    In = [name(value)],
    trip(In, Out).
test(atom, In == Out) :-
    numlist(32, 126, Chars),
    n_list_of(10, Chars, ListOfStrings),
    append(ListOfStrings, AllChars),
    atom_codes(Value, AllChars),
    In = [name(Value)],
    trip(In, Out).
test(unicode, In == Out) :-
    numlist(32, 1100, Chars),
    atom_codes(Value, Chars),
    In = [name(Value)],
    trip(In, Out).
test(integer, In == Out) :-
    In = [age(2394395340490340)],
    trip(In, Out).
test(float, In == Out) :-
    In = [age(42.0)],
    trip(In, Out).
test(bad_escape, error(syntax_error(cgi_value))) :-
    setenv('QUERY_STRING', 'a=%zz'),
    cgi_get_form(_).
test(bad_escape, error(syntax_error(cgi_value))) :-
    setenv('QUERY_STRING', 'a=%4'),
    cgi_get_form(_).
test(short_post, Out = error(_)) :-
    post('a=1', 10, [], Out).
test(post, Out == [a(1),b(x)]) :-
    post('a=1&b=x', 7, [], Out).
test(multipart, Out == [a(''),b(x),c('')]) :-
    Body = '--XX\r\n\c
            Content-Disposition: form-data; name="a"\r\n\r\n\c
            \r\n--XX\r\n\c
            Content-Disposition: form-data; name="b"\r\n\r\n\c
            x\r\n--XX\r\n\c
            Content-Disposition: form-data; filename="f"; name="c"\r\n\r\n\c
            --XX--\r\n',
    atom_length(Body, Len),
    post(Body, Len, ['CONTENT_TYPE'='multipart/form-data; boundary=XX'], Out).

%!  post(+Body, +ContentLength, +Env, -Result)
%
%   Run cgi_get_form/1 in a child process that reads Body as POST
%   data from standard input.

post(Body, Len, Env, Result) :-
    Goal = '(catch(cgi_get_form(F),E,F=error(E))->writeq(F);true),write(.),nl,halt',
    process_create(prolog(swipl),
                   ['-q', '-g', 'use_module(library(cgi))', '-g', Goal, '-t', 'halt(1)'],
                   [ stdin(pipe(In)),
                     stdout(pipe(Out)),
                     environment(['REQUEST_METHOD'='POST',
                                  'CONTENT_LENGTH'=Len|Env]),
                     process(Pid)
                   ]),
    call_cleanup(
        ( format(In, '~w', [Body]),
          close(In),
          read_term(Out, Result, [])
        ),
        ( close(Out),
          process_wait(Pid, _, [timeout(10)])
        )).

:- end_tests(cgi).

