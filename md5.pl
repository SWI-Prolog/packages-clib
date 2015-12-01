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

:- module(md5,
	  [ md5_hash/3			% +Input, -Hash, +Options
	  ]).

:- use_foreign_library(foreign(md54pl)).

/** <module> MD5 hashes

Compute MD5 hashes from a Prolog  string.   This  is a rather short-term
solution waiting for a more general interface to the libcrypto functions
of OpenSSL.
*/

%%	md5_hash(+Data, -Hash, +Options) is det.
%
%	Hash is the MD5 hash of Data,   The  conversion is controlled by
%	Options:
%
%	  * encoding(+Encoding)
%	  If Data is a sequence of character _codes_, this must be
%	  translated into a sequence of _bytes_, because that is what
%	  the hashing requires.  The default encoding is =utf8=.  The
%	  other meaningful value is =octet=, claiming that Data contains
%	  raw bytes.
%
%	@arg Data is either an atom, string, code-list or char-list.
%	@arg Hash is an atom holding 32 characters, representing the
%	hash in hexadecimal notation

