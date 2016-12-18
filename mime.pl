/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2012, University of Amsterdam
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

:- module(mime,
          [ mime_parse/2,               % +Data, -Mime
            mime_default_charset/2      % -Old, +New
          ]).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(mime), install_mime).

/** <module> Parse MIME documents

This module defines an interface to   the rfc2045 (MIME) parsing library
by Double Precision, Inc, part of the   maildrop system. This library is
distributed under the GPL and  therefore   all  code  using this library
should comply to the GPL.
*/

%!  mime_parse(+Data, -Parsed) is det.
%
%   True when Parsed is a parsed  representation of the MIME message
%   in Data. Data is one of
%
%     * stream(In)
%     * stream(In, Length)
%     * an Atom, String or list of characters.
%
%   Parsed is a structure of this form:
%
%     * mime(Attributes, Data, SubMimeList)
%
%   Where Data is the (decoded) field data returned as an atom. If a
%   part is of type =|text/...|=,  the   charset  is  interpreted as
%   follows: if charset contains =|UTF-8|= or  an alias thereof, the
%   text  is  interpreted  as  UTF-8.  If  it  the  charset  can  be
%   interpreted as ISO-8859-1 or US-ASCII, no conversion is applied.
%   Otherwise, default locale specific conversion   is  applied. See
%   also mime_default_charset/2.
%
%   Attributes is a property-list  and  SubMimeList   is  a  list of
%   mime/3 terms reflecting the sub-parts.   Attributes contains the
%   following members:
%
%     * id(Atom)
%     Identifier of the message-part.
%     * description(Atom)
%     Descriptive text for the \arg{Data}.
%     * language(Atom)
%     Language in which the text-data is written.
%     * md5(Atom)
%     * type(Atom)
%     Denotes the Content-Type, how the \arg{Data} should be interpreted.
%     * character_set(Atom)
%     The character set used for text data.  See above.
%     * transfer_encoding(Atom)
%     How the \arg{Data} was encoded.  This is not very
%     interesting as the library decodes the content of the message.
%     * disposition(Atom)
%     Where the data comes from.  The current library only deals
%     with `inline' data.
%     * filename(Atom)
%     Name of the file the data should be stored in.
%     * name(Atom)
%     Name of the part.

%!  mime_default_charset(-Old, +New) is det.
%
%   True when Old reflects the old and new the new default character
%   set of the library. The  system   default  is =|us-ascii|=. This
%   value  is  returned  into  the  attribute  =character_set=  (see
%   mime_parse/2) if the message  does   not  explicitly  specifythe
%   character set.  It is used for translating the message content.
%
%   @bug This setting is global and shared between threads.
