/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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

:- module(prolog_stream,
	  [ open_prolog_stream/4	% +Module, +Mode, -Stream, +Data
	  ]).
:- use_module(library(shlib)).
:- use_foreign_library(foreign(prolog_stream)).

/** <module> A stream with Prolog callbacks

This library defines a Prolog  stream   that  realises its low-level I/O
with callbacks to Prolog.  The  library   was  developed  to bind normal
Prolog I/O to Pengines I/O. This type of I/O redirection is probably the
primary use case.
*/

%%	open_prolog_stream(+Module, +Mode, -Stream, +Options)
%
%	Create  a  new  stream  that  implements   its  I/O  by  calling
%	predicates in Module.  The called predicates are:
%
%	  - Module:stream_write(+Stream, +String)
%	  Called for a `Mode = write` stream if data is available.
%	  String contains the (textual) data that is written
%	  to Stream.  The callback is called if the buffer of
%	  Stream overflows, the user calls flush_output(Stream)
%	  or Stream is closed and there is buffered data.
%	  - Module:stream_read(+Stream, -Term)
%	  Called for a `Mode == read` stream to get new data.  On
%	  success the stream extracts text from the provided Term.
%	  Term is typically a string, atom, code or character list.
%	  If term is not one of the above, it is handed to writeq/1.
%	  To signal end-of-file, unify stream with an empty text,
%	  e.g., `stream_read(Stream, "")`.
%	  - Module:stream_close(+Stream)
%	  Called when the stream is closed.  This predicate must
%	  succeed.  The callback can be used to cleanup associated
%	  resources.
%
%	The current implementation only  deals   with  text streams. The
%	stream uses the =wchar_t= encoding. The   buffer  size must be a
%	multiple of =wchar_t=, i.e., a multiple of four for portability.
%	The _newline_ mode of the stream   is  =posix= on all platforms,
%	disabling the translation `"\n" --> "\r\n"`.
%
%	@arg Options is currently ignored.
%	@bug	Futher versions might require additional callbacks.  As we
%		demand all callbacks to be defined, existing code needs
%		to implement the new callbacks.
