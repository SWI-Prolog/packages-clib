/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam,
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

:- module(test_time,
          [ test_time/0,
            list_alarms/0
          ]).

:- use_module(library(debug)).
:- use_module(library(plunit)).
:- use_module(library(time)).

dbg :-
    time:time_debug(1).

test_time :-
    run_tests([ time
              ]).

:- begin_tests(time).

test(current) :-
    setup_call_cleanup(alarm(5.0, writeln(ouch), ID, []),
                       current_alarm(_At, _Goal, ID, _Status),
                       remove_alarm(ID)).
test(current) :-
    setup_call_cleanup(alarm(5.0, writeln(ouch), ID, []),
                       current_alarm(_At, writeln(ouch), _ID, _Status),
                       remove_alarm(ID)).
test(current) :-
    setup_call_cleanup(alarm(5.0, writeln(ouch), ID, []),
                       current_alarm(_At, _Goal, _ID, scheduled),
                       remove_alarm(ID)).
test(bg) :-
    bg(5, 4).
test(flood) :-
    flood_test.
test(removed, Listed == false) :-
    fired_alarm(Id),
    remove_alarm(Id),
    (   current_alarm(_, _, Id, _)
    ->  Listed = true
    ;   Listed = false
    ).
test(removed, error(existence_error(alarm, Id))) :-
    fired_alarm(Id),
    install_alarm(Id).
test(stale_id) :-
    stale_id_test(2000).

test(reinstall, error(permission_error(install, alarm, Id))) :-
    alarm(100, true, Id, [install(false)]),
    install_alarm(Id, 50),
    call_cleanup(install_alarm(Id, 10),
                 remove_alarm(Id)).
test(reinstall_list, N == 1) :-
    alarm(100, true, Id, [install(false)]),
    install_alarm(Id, 50),
    catch(install_alarm(Id, 10), error(permission_error(_,_,_),_), true),
    aggregate_all(count, current_alarm(_,_,Id,_), N),
    remove_alarm(Id).
test(inf, Status == scheduled) :-
    Inf is inf,
    alarm(Inf, true, Id, []),
    current_alarm(_, _, Id, Status),
    remove_alarm(Id).
test(nan, error(domain_error(time, _))) :-
    NaN is nan,
    alarm(NaN, true, _, []).

:- end_tests(time).


                 /*******************************
                 *     MULTI-THREAD TIMEOUT     *
                 *******************************/

bg(Times, N) :-
    findall(Id, (between(1, N, _),
                 thread_create(t(Times, 0.05), Id, [])),
            IDS),
    join_all(IDS).

join_all([]).
join_all([H|T]) :-
    thread_join(H, Status),
    assertion(Status == true),
    join_all(T).


t(N, Time) :-
    thread_create(worker, Worker, []),
    t(N, Time, Worker),
    thread_send_message(Worker, done),
    thread_join(Worker, Status),
    assertion(Status == true).

t(0, _, _) :- !.
t(N, Time, Worker) :-
    thread_self(Me),
    thread_send_message(Worker, work(Time, Me)),
    thread_get_message(done(E)),
    assertion(E == time_limit_exceeded),
    N2 is N - 1,
    t(N2, Time, Worker).

worker :-
    thread_get_message(Msg),
    (   Msg = work(N, Sender)
    ->  thread_self(Me),
        debug(work, '[~w] Start working ~w sec', [Me, N]),
        r(N, E),
        thread_send_message(Sender, done(E)),
        worker
    ;   true
    ).

r(N, E) :-
    catch(call_with_time_limit(N, (repeat, fail)),
          E, true).

w(N) :-
    alarm(N, writeln(hello), Id),
    writeln(Id).


                 /*******************************
                 *           FLOODING           *
                 *******************************/

:- dynamic
    x/1.

flood_test :-
    retractall(x(_)),
    forall(between(1, 100, X),
           alarm(0.1, got(X), _,
                 [ remove(true)
                 ])),
    get_time(Now),
    repeat,
       get_time(End),
       End - Now > 0.5,
    !,
    (   forall(between(1, 100, X), x(X))
    ->  retractall(x(_))
    ;   forall(between(1, 100, X),
               (   x(X)
               ->  true
               ;   format('Failed: ~D~n', [X])
               ))
    ).

got(X) :-
    assert(x(X)).


                 /*******************************
                 *         STALE ALARM IDS      *
                 *******************************/

%   Cancelling a remove(true) alarm that may have fired already must
%   not affect the alarms of call_with_time_limit/2 in another thread.

stale_id_test(N) :-
    thread_create(cancel_fired(N), A),
    thread_create(time_limited(N), B),
    thread_join(A, SA),
    thread_join(B, SB),
    assertion(SA == true),
    assertion(SB == true).

fired_alarm(Id) :-
    alarm(0.01, true, Id, [remove(true)]),
    sleep(0.1).

cancel_fired(N) :-
    forall(between(1, N, _),
           ( alarm(0.0001, true, Id, [remove(true)]),
             sleep(0.0005),
             remove_alarm(Id)
           )).

time_limited(N) :-
    forall(between(1, N, _),
           catch(call_with_time_limit(0.01, sleep(0.0003)),
                 time_limit_exceeded, true)).


                 /*******************************
                 *            DEBUG             *
                 *******************************/

list_alarms :-
    (   current_alarm(At, Callable, Id, Status),
        format('~p ~p ~p ~p~n', [At, Callable, Id, Status]),
        fail
    ;   true
    ).
