/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

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

:- module(uuid,
	  [ uuid/1,			% -UUID
	    uuid/2			% -UUID, +Options
	  ]).

/** <module> Universally Unique Identifier (UUID) Library

The library provides operations on UUIDs.   Please consult other sources
for understanding UUIDs and  the  implications   of  the  different UUID
versions.  Some typical calls are given below:

  ==
  ?- uuid(X).
  X = 'ea6589fa-19dd-11e2-8a49-001d92e1879d'.

  ?- uuid(X, [url('http://www.swi-prolog.org')]).
  X = '73a07870-6a90-3f2e-ae2b-ffa538dc7c2c'.
  ==

@tbd Compare UUIDs, extract time and version from UUIDs
@see http://www.ossp.org/pkg/lib/uuid/
*/

:- use_foreign_library(foreign(uuid)).

%%	uuid(-UUID) is det.
%
%	UUID is an atom representing a  new   UUID.  This is the same as
%	calling uuid(UUID, []).  See uuid/2 for options.

uuid(UUID) :-
	uuid(UUID, []).

%%	uuid(-UUID, +Options) is det.
%
%	Create a new UUID according to   Options.  The following options
%	are defined:
%
%	  * version(+Versions)
%	  Integer in the range 1..5, which specifies the UUID version
%	  that is created.  Default is 1.
%
%	  * dns(DNS)
%	  * url(URL)
%	  * oid(OID)
%	  * x500(X500)
%	  Provide additional context information for UUIDs using version
%	  3 or 5.  If there is no explicit version option, UUID version
%	  3 is used.
%
%	  * format(+Format)
%	  Representation of the UUID.  Default is =atom=, yielding atoms
%	  such as =|8304efdd-bd6e-5b7c-a27f-83f3f05c64e0|=. The
%	  alternative is =integer=, returning a large integer that
%	  represents the 128 bits of the UUID.
