/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(malloc_info,
	  [
	  ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(sgml)).

:- use_foreign_library(foreign(mallocinfo)).

/** <module> Memory allocation details

This library is provided of the clib   package  is compiled on a _glibc_
based system, typically Linux. It provides  access to the glibc ptmalloc
informational  functions  for  diagnosing  memory  usage.  This  library
exports

  * mallinfo/1
  * malloc_info/1
*/

:- if(current_predicate('$mallinfo'/1)).
:- export(mallinfo/1).

%%	mallinfo(-Info:dict) is det.
%
%	Return the content  of  the   =|struct  mallinfo|=  returned  by
%	=|mallinfo()|= as a dict. See =|man mallinfo|= for an
%	explanation of the fields.
%
%	@bug	The =|struct mallinfo|= contains =int= fields and is thus
%		incapable of expressing the memory sizes of 64-bit
%		machines.  The fields are interpreted as _unsigned_ and
%		thus represent the true value modulo 2**32 (4Gb).

mallinfo(Info) :-
	'$mallinfo'(List),
	dict_create(Info, malinfo, List).
:- endif.

:- if(current_predicate('$malloc_info'/1)).
:- export(malloc_info/1).

%%	malloc_info(-Info:dict) is det.
%
%	Interface to =|malloc_info()|=, which provides   an XML document
%	describing the status of the   GNU  glibc malloc implementation.
%	The XML document is parsed and  translated   into  a dict with a
%	similar structure. The  malloc_info()  XML   is  supposed  to be
%	self-explanatory.

malloc_info(Info) :-
	'$malloc_info'(XML),
	setup_call_cleanup(
	    open_string(XML, In),
	    load_xml(In, DOM, [space(remove)]),
	    close(In)),
	malloc_dom_prolog(DOM, Info).

malloc_dom_prolog([element(malloc, _, DOM)], Info) :-
	maplist(malloc_prolog, DOM, List),
	partition(is_dict, List, Heaps, Rest),
	dict_create(Info, malloc, [heaps:Heaps|Rest]).

malloc_prolog(element(heap, [nr=NRA], DOM), Heap) :- !,
	atom_number(NRA, NR),
	maplist(heap_prolog, DOM, HeapProperties),
	dict_create(Heap, heap, [nr-NR|HeapProperties]).
malloc_prolog(Element, Pair) :-
	misc_field(Element, Pair).

heap_prolog(element(sizes, _, DOM), sizes-Sizes) :- !,
	maplist(chunk_size, DOM, Sizes).
heap_prolog(Element, Pair) :-
	misc_field(Element, Pair).

misc_field(element(Name, Attrs0, []), Key-Value) :-
	selectchk(type=Type, Attrs0, Attrs1),
	atomic_list_concat([Name, '_', Type], Key),
	maplist(attr_value, Attrs1, Attrs),
	(   Attrs = [_=Value]
	->  true
	;   dict_create(Value, Name, Attrs)
	).

chunk_size(element(size, Attrs0, []), Dict) :- !,
	maplist(attr_value, Attrs0, Attrs),
	dict_create(Dict, size, Attrs).
chunk_size(element(unsorted, Attrs0, []), Dict) :-
	maplist(attr_value, Attrs0, Attrs),
	dict_create(Dict, unsorted, Attrs).

attr_value(Name=In, Name=Out) :-
	atom_number(In, Out), !.
attr_value(Name=In, Name=Out) :-
	atom_string(In, Out), !.

:- endif.
