/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(mime,
	  [ mime_parse/2,		% +Data, -Mime
	    mime_default_charset/2	% -Old, +New
	  ]).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(mime), install_mime).
:- license(gpl).

/** <module> Parse MIME documents

This module defines an interface to   the rfc2045 (MIME) parsing library
by Double Precision, Inc, part of the   maildrop system. This library is
distributed under the GPL and  therefore   all  code  using this library
should comply to the GPL.


@license GPL
*/

%%	mime_parse(+Data, -Parsed) is det.
%
%	True when Parsed is a parsed  representation of the MIME message
%	in Data. Data is one of
%
%	  * stream(In)
%	  * stream(In, Length)
%	  * an Atom, String or list of characters.
%
%	Parsed is a structure of this form:
%
%	  * mime(Attributes, Data, SubMimeList)
%
%	Where Data is the (decoded) field data returned as an atom. If a
%	part is of type =|text/...|=,  the   charset  is  interpreted as
%	follows: if charset contains =|UTF-8|= or  an alias thereof, the
%	text  is  interpreted  as  UTF-8.  If  it  the  charset  can  be
%	interpreted as ISO-8859-1 or US-ASCII, no conversion is applied.
%	Otherwise, default locale specific conversion   is  applied. See
%	also mime_default_charset/2.
%
%	Attributes is a property-list  and  SubMimeList   is  a  list of
%	mime/3 terms reflecting the sub-parts.   Attributes contains the
%	following members:
%
%	  * id(Atom)
%	  Identifier of the message-part.
%	  * description(Atom)
%	  Descriptive text for the \arg{Data}.
%	  * language(Atom)
%	  Language in which the text-data is written.
%	  * md5(Atom)
%	  * type(Atom)
%	  Denotes the Content-Type, how the \arg{Data} should be interpreted.
%	  * character_set(Atom)
%	  The character set used for text data.  See above.
%	  * transfer_encoding(Atom)
%	  How the \arg{Data} was encoded.  This is not very
%	  interesting as the library decodes the content of the message.
%	  * disposition(Atom)
%	  Where the data comes from.  The current library only deals
%	  with `inline' data.
%	  * filename(Atom)
%	  Name of the file the data should be stored in.
%	  * name(Atom)
%	  Name of the part.

%%	mime_default_charset(-Old, +New) is det.
%
%	True when Old reflects the old and new the new default character
%	set of the library. The  system   default  is =|us-ascii|=. This
%	value  is  returned  into  the  attribute  =character_set=  (see
%	mime_parse/2) if the message  does   not  explicitly  specifythe
%	character set.  It is used for translating the message content.
%
%	@bug This setting is global and shared between threads.
