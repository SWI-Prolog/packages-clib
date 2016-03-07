/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam,
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

:- module(test_uri,
	  [ test_uri/0
	  ]).

:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(plunit)).

test_uri :-
	run_tests([ uri,
		    iri,
		    uri_authority,
		    uri_query,
		    uri_encode
		  ]).

trip_uri_iri(IRI, X) :-
	uri_iri(URI, IRI),
	uri_iri(URI, X).

resolve(In, Out) :-
	uri_resolve(In, 'http://a/b/c/d;p?q', Out).

:- begin_tests(uri).

test(unicode_trip, X == IRI) :-
	IRI = 'http://a.b/\u041a',
	trip_uri_iri(IRI, X).
test(unicode_uri, IRI == URI) :-
	URI = 'http://a.b/\u041a',
	uri_iri(URI, IRI).
test(latin_uri, IRI == URI) :-
	URI = 'http://a.b/\u00a8',
	uri_iri(URI, IRI).

test(resolve, URI == 'g:h')		     :-	resolve('g:h', URI).
test(resolve, URI == 'http://a/b/c/g')	     :-	resolve('g', URI).
test(resolve, URI == 'http://a/b/c/g')	     :-	resolve('./g', URI).
test(resolve, URI == 'http://a/b/c/g/')	     :-	resolve('g/', URI).
test(resolve, URI == 'http://a/g')	     :-	resolve('/g', URI).
test(resolve, URI == 'http://g')	     :-	resolve('//g', URI).
test(resolve, URI == 'http://a/b/c/d;p?y')   :-	resolve('?y', URI).
test(resolve, URI == 'http://a/b/c/g?y')     :-	resolve('g?y', URI).
test(resolve, URI == 'http://a/b/c/d;p?q#s') :-	resolve('#s', URI).
test(resolve, URI == 'http://a/b/c/g#s')     :-	resolve('g#s', URI).
test(resolve, URI == 'http://a/b/c/g?y#s')   :-	resolve('g?y#s', URI).
test(resolve, URI == 'http://a/b/c/;x')	     :-	resolve(';x', URI).
test(resolve, URI == 'http://a/b/c/g;x')     :-	resolve('g;x', URI).
test(resolve, URI == 'http://a/b/c/g;x?y#s') :-	resolve('g;x?y#s', URI).
test(resolve, URI == 'http://a/b/c/d;p?q')   :-	resolve('', URI).
test(resolve, URI == 'http://a/b/c/')	     :-	resolve('.', URI).
test(resolve, URI == 'http://a/b/c/')	     :-	resolve('./', URI).
test(resolve, URI == 'http://a/b/')	     :-	resolve('..', URI).
test(resolve, URI == 'http://a/b/')	     :-	resolve('../', URI).
test(resolve, URI == 'http://a/b/g')	     :-	resolve('../g', URI).
test(resolve, URI == 'http://a/')	     :-	resolve('../..', URI).
test(resolve, URI == 'http://a/')	     :-	resolve('../../', URI).
test(resolve, URI == 'http://a/g')	     :-	resolve('../../g', URI).

:- end_tests(uri).

:- begin_tests(iri).

test(normalise_uri, NormalURI == 'example://a/b/c/%7Bfoo%7D') :-
	uri_normalized('eXAMPLE://a/./b/../b/%63/%7bfoo%7d', NormalURI).
test(normalise_iri, NormalIRI == 'example://a/b/c/%7Bfoo%7D') :-
	uri_normalized_iri('eXAMPLE://a/./b/../b/%63/%7bfoo%7d', NormalIRI).
test(normalise_iri, NormalIRI == 'http://a.b/a%3F?x') :-	% 3F = '?'
	uri_normalized_iri('http://a.b/a%3f?x', NormalIRI).
test(normalise_iri, NormalIRI == 'http://a.b/a?x=1&y=2') :-
	uri_normalized_iri('http://a.b/a?x=1&y=2', NormalIRI).
test(normalise_iri, NormalIRI == 'http://a.b/a?x=1&y=2#aap') :-
	uri_normalized_iri('http://a.b/a?x=1&y=2#aap', NormalIRI).
test(normalise_iri, NormalIRI == 'http://a.b/a?x=1&y=2#aap%20noot') :-
	uri_normalized_iri('http://a.b/a?x=1&y=2#aap+noot', NormalIRI).
test(normalise_iri, NormalIRI == 'http://a.b:3020/') :-
	uri_normalized_iri('http://a.b:3020/', NormalIRI).

:- end_tests(iri).

:- begin_tests(uri_query).

test(break, Q == [a=b,c=d]) :-
	uri_query_components('a=b&c=d', Q).
test(construct, QS == 'a=b&c=d') :-
	uri_query_components(QS, [a=b,c=d]).
test(encode, Q == [name=Value]) :-
	numlist(1, 1050, VL),
	atom_codes(Value, VL),
	uri_query_components(QS, [name=Value]),
	uri_query_components(QS, Q).

:- end_tests(uri_query).

:- begin_tests(uri_authority).

test(break, [User,Host,Port] == [jan,'swi-prolog.org', 3040]) :-
	uri_authority_components('jan@swi-prolog.org:3040', C),
	uri_authority_data(user, C, User),
	uri_authority_data(host, C, Host),
	uri_authority_data(port, C, Port).
test(break, [User,Pwd,Host,Port] == [jan,xxx,'swi-prolog.org', 3040]) :-
	uri_authority_components('jan:xxx@swi-prolog.org:3040', C),
	uri_authority_data(user, C, User),
	uri_authority_data(password, C, Pwd),
	uri_authority_data(host, C, Host),
	uri_authority_data(port, C, Port).
test(construct, Auth == 'jan@swi-prolog.org:3040') :-
	uri_authority_data(user, C, jan),
	uri_authority_data(host, C, 'swi-prolog.org'),
	uri_authority_data(port, C, 3040),
	uri_authority_components(Auth, C).

:- end_tests(uri_authority).

:- begin_tests(uri_encode).

test(query, X == '%3D%26') :-
	uri_encoded(query_value, '=&', X).
test(query, X == 'a%2Bb') :-
	uri_encoded(query_value, 'a+b', X).
test(query, X == 'a%3Ab') :-
	uri_encoded(query_value, 'a:b', X).
test(query, X == 'a b') :-
	uri_encoded(query_value, X, 'a+b').
test(path, X == 'a+b') :-
	uri_encoded(path, 'a+b', X).
test(path, X == 'a+b') :-
	uri_encoded(path, X, 'a+b').
test(path, X == 'a%3Ab') :-
	uri_encoded(path, 'a:b', X).
test(path, X == '=&') :-
	uri_encoded(path, '=&', X).
test(path, X == '/a%20b%3F') :-
	uri_encoded(path, '/a b?', X).
test(path, X == '%25') :-
	uri_encoded(path, '%', X).
test(path, X == '%C3%B6%C3%A4%C3%BC%C3%B5') :-
	atom_codes(Path, [246, 228, 252, 245]),
	uri_encoded(path, Path, X).

:- end_tests(uri_encode).
