/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2002-2015, University of Amsterdam
			      Vu University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(files_ex,
	  [ set_time_file/3,		% +File, -OldTimes, +NewTimes
	    link_file/3,		% +OldPath, +NewPath, +Type
	    relative_file_name/3,	% +AbsPath, +RelTo, -RelPath
	    directory_file_path/3,	% +Dir, +File, -Path
	    copy_file/2,		% +From, +To
	    make_directory_path/1,	% +Directory
	    copy_directory/2,		% +Source, +Destination
	    delete_directory_and_contents/1, % +Dir
	    delete_directory_contents/1 % +Dir
	  ]).

/** <module> Extended operations on files

This module provides additional operations on   files.  This covers both
more  obscure  and  possible  non-portable    low-level  operations  and
high-level utilities.

Using these Prolog primitives is typically   to  be preferred over using
operating system primitives through shell/1  or process_create/3 because
(1) there are no potential file  name   quoting  issues, (2) there is no
dependency  on  operating   system   commands    and   (3)   using   the
implementations from this library is usually faster.
*/


:- use_foreign_library(foreign(files), install_files).

%%	set_time_file(+File, -OldTimes, +NewTimes) is det.
%
%	Query and set POSIX time attributes of a file. Both OldTimes and
%	NewTimes are lists of  option-terms.   Times  are represented in
%	SWI-Prolog's standard floating point numbers.   New times may be
%	specified as =now= to indicate the current time. Defined options
%	are:
%
%	    * access(Time)
%	    Describes the time of last access   of  the file. This value
%	    can be read and written.
%
%	    * modified(Time)
%	    Describes the time  the  contents  of   the  file  was  last
%	    modified. This value can be read and written.
%
%	    * changed(Time)
%	    Describes the time the file-structure  itself was changed by
%	    adding (link()) or removing (unlink()) names.
%
%	Below  are  some  example  queries.   The  first  retrieves  the
%	access-time, while the second sets the last-modified time to the
%	current time.
%
%	    ==
%	    ?- set_time_file(foo, [access(Access)], []).
%	    ?- set_time_file(foo, [], [modified(now)]).
%	    ==

%%	link_file(+OldPath, +NewPath, +Type) is det.
%
%	Create a link in the filesystem   from  NewPath to OldPath. Type
%	defines the type of link and is one of =hard= or =symbolic=.
%
%	With some limitations, these  functions   also  work on Windows.
%	First of all, the unerlying filesystem  must support links. This
%	requires NTFS. Second, symbolic  links   are  only  supported in
%	Vista and later.
%
%	@error	domain_error(link_type, Type) if the requested link-type
%		is unknown or not supported on the target OS.

%%	relative_file_name(+Path:atom, +RelTo:atom, -RelPath:atom) is det.
%%	relative_file_name(-Path:atom, +RelTo:atom, +RelPath:atom) is det.
%
%	True when RelPath is Path, relative to RelTo. Path and RelTo are
%	first handed to absolute_file_name/2, which   makes the absolute
%	*and* canonical. Below are two examples:
%
%	==
%	?- relative_file_name('/home/janw/nice',
%			      '/home/janw/deep/dir/file', Path).
%	Path = '../../nice'.
%	==
%
%	==
%	?- relative_file_name(Path, '/home/janw/deep/dir/file', '../../nice').
%	Path = '/home/janw/deep/dir/../../nice'.
%	==
%
%	@param	All paths must be in canonical POSIX notation, i.e.,
%		using / to separate segments in the path.  See
%		prolog_to_os_filename/2.
%	@bug	This predicate is defined as a _syntactical_ operation.

relative_file_name(Path, RelTo, RelPath) :-
	atom(Path), atom(RelTo), !,
	absolute_file_name(Path, AbsPath),
	absolute_file_name(RelTo, AbsRelTo),
        atomic_list_concat(PL, /, AbsPath),
        atomic_list_concat(RL, /, AbsRelTo),
        delete_common_prefix(PL, RL, PL1, PL2),
        to_dot_dot(PL2, DotDot, PL1),
        atomic_list_concat(DotDot, /, RelPath).
relative_file_name(Path, RelTo, RelPath) :-
	atom(RelTo), atom(RelPath), !,
	file_directory_name(RelTo, RelToDir),
	start_with_slash(RelPath, RelPathSlash),
	atomic_concat(RelToDir, RelPathSlash, Path).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
        delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
        to_dot_dot(T0, T, Tail).

start_with_slash(Atom, Atom):-
	sub_atom(Atom, 0, 1, _, /), !.
start_with_slash(Atom0, Atom):-
	atomic_concat(/, Atom0, Atom).


%%	directory_file_path(+Directory, +File, -Path) is det.
%%	directory_file_path(?Directory, ?File, +Path) is det.
%
%	True when Path is the full path-name   for  File in Dir. This is
%	comparable to atom_concat(Directory, File, Path), but it ensures
%	there is exactly one / between the two parts.  Notes:
%
%	  * In mode (+,+,-), if File is given and absolute, Path
%	  is unified to File.
%	  * Mode (-,-,+) uses file_directory_name/2 and file_base_name/2

directory_file_path(Dir, File, Path) :-
	nonvar(Dir), nonvar(File), !,
	(   (   is_absolute_file_name(File)
	    ;	Dir == '.'
	    )
	->  Path = File
	;   sub_atom(Dir, _, _, 0, /)
	->  atom_concat(Dir, File, Path)
	;   atomic_list_concat([Dir, /, File], Path)
	).
directory_file_path(Dir, File, Path) :-
	nonvar(Path), !,
	(   nonvar(Dir)
	->  (   sub_atom(Dir, _, _, 0, /)
	    ->  atom_concat(Dir, File, Path)
	    ;	atom_concat(Dir, /, TheDir),
		atom_concat(TheDir, File, Path)
	    )
	;   nonvar(File)
	->  atom_concat(Dir, File, Path)
	;   file_directory_name(Path, Dir),
	    file_base_name(Path, File)
	).
directory_file_path(_, _, _) :-
	throw(error(instantiation_error(_), _)).

%%	copy_file(From, To) is det.
%
%	Copy a file into a new file or  directory. The data is copied as
%	binary data.

copy_file(From, To) :-
	destination_file(To, From, Dest),
	setup_call_cleanup(open(Dest, write, Out, [type(binary)]),
			   copy_from(From, Out),
			   close(Out)).

copy_from(File, Stream) :-
	setup_call_cleanup(
	    open(File, read, In, [type(binary)]),
	    copy_stream_data(In, Stream),
	    close(In)).

destination_file(Dir, File, Dest) :-
	exists_directory(Dir), !,
	file_base_name(File, Base),
	directory_file_path(Dir, Base, Dest).
destination_file(Dest, _, Dest).


%%	make_directory_path(+Dir) is det.
%
%	Create Dir and all required  components   (like  mkdir  -p). Can
%	raise various file-specific exceptions.

make_directory_path(Dir) :-
	make_directory_path_2(Dir), !.
make_directory_path(Dir) :-
	permission_error(create, directory, Dir).

make_directory_path_2(Dir) :-
	exists_directory(Dir), !.
make_directory_path_2(Dir) :-
	Dir \== (/), !,
	file_directory_name(Dir, Parent),
	make_directory_path_2(Parent),
	make_directory(Dir).

%%	copy_directory(+From, +To) is det.
%
%	Copy the contents of the directory  From to To (recursively). If
%	To is the name of an existing  directory, the _contents_ of From
%	are copied into To. I.e., no  subdirectory using the basename of
%	From is created.

copy_directory(From, To) :-
	(   exists_directory(To)
	->  true
	;   make_directory(To)
	),
	directory_files(From, Entries),
	maplist(copy_directory_content(From, To), Entries).

copy_directory_content(_From, _To, Special) :-
	special(Special), !.
copy_directory_content(From, To, Entry) :-
	directory_file_path(From, Entry, Source),
	directory_file_path(To, Entry, Dest),
	(   exists_directory(Source)
	->  copy_directory(Source, Dest)
	;   copy_file(Source, Dest)
	).

special(.).
special(..).

%%	delete_directory_and_contents(+Dir) is det.
%
%	Recursively remove the directory Dir and its contents. If Dir is
%	a symbolic link or symbolic links   inside  Dir are encountered,
%	the links are removed rather than their content. Use with care!

delete_directory_and_contents(Dir) :-
	read_link(Dir, _, _), !,
	delete_file(Dir).
delete_directory_and_contents(Dir) :-
	directory_files(Dir, Files),
	maplist(delete_directory_contents(Dir), Files),
	delete_directory(Dir).

delete_directory_contents(_, Entry) :-
	special(Entry), !.
delete_directory_contents(Dir, Entry) :-
	directory_file_path(Dir, Entry, Delete),
	(   exists_directory(Delete)
	->  delete_directory_and_contents(Delete)
	;   delete_file(Delete)
	).

%%	delete_directory_contents(+Dir) is det.
%
%	Remove all content from  directory   Dir,  without  removing Dir
%	itself. Similar to delete_directory_and_contents/2,  if symbolic
%	links are encountered in Dir, the  links are removed rather than
%	their content.

delete_directory_contents(Dir) :-
	directory_files(Dir, Files),
	maplist(delete_directory_contents(Dir), Files).

