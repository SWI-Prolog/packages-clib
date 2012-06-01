/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2012, University of Amsterdam
			      VU University Amsterdam

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

:- module(crypto_hash,
	  [ sha_hash/3,			% +Data, -Hash, +Options
	    sha_new_ctx/2,		% -NewContext, +Options
	    sha_hash_ctx/4,		% +OldCtx, +Data, -NewCtx, -Hash
	    hmac_sha/4,			% +Key, +Data, -Hash, +Options
	    hash_atom/2			% +Hash, -HexAtom
	  ]).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(sha4pl)).

%%	sha_hash(+Data, -Hash, +Options) is det
%
%	Hash is the SHA hash of Data, The conversion is controlled
%	by Options:
%
%	  * algorithm(+Algorithm)
%	  One of =sha1= (default), =sha224=, =sha256=, =sha384= or
%	  =sha512=
%
%	@param	Data is either an atom, string or code-list
%	@param  Hash is a packed string

%%	sha_new_ctx(-NewContext, +Options) is det
%
%	NewContext is unified with the empty SHA computation context
%	(which includes the Options.)  It could later be passed to
%	sha_hash_ctx/4.
%
%	For Options, see sha_hash/3.

%%	sha_hash_ctx(+OldContext, +Data, -NewContext, -Hash) is det
%
%	Hash is the SHA hash of Data.  NewContext is the new SHA
%	computation context, while OldContext is the old.  OldContext
%	may be produced by a prior invocation of either sha_new_ctx/3 or
%	sha_hash_ctx/4 itself.
%
%	This predicate allows a SHA function to be computed in chunks,
%	which may be important while working with Metalink (RFC 5854),
%	BitTorrent or similar technologies, or simply with big files.

%%	hmac_sha(+Key, +Data, -Hash, +Options) is det
%
%	For Options, see sha_hash/3.

%%	hash_atom(+HashCodes, -HexAtom) is det.
%
%	Convert a list of bytes (integers 0..255) into the usual
%	hexadecimal notation.  E.g.
%
%	  ==
%	  ?- sha_hash('SWI-Prolog', Hash, []),
%	     hash_atom(Hash, Hex).
%	  Hash = [61, 128, 252, 38, 121, 69, 229, 85, 199|...],
%	  Hex = '3d80fc267945e555c730403bd0ab0716e2a68c68'.
%	  ==

hash_atom(Codes, Hash) :-
	phrase(bytes_hex(Codes), HexCodes),
	atom_codes(Hash, HexCodes).

bytes_hex([]) --> [].
bytes_hex([H|T]) -->
	{ High is H>>4,
	  Low is H /\ 0xf,
	  code_type(C0, xdigit(High)),
	  code_type(C1, xdigit(Low))
	},
	[C0,C1],
	bytes_hex(T).
