/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(syslog,
	  [ openlog/3,			% +Ident, +Options, +Facility
	    syslog/2,			% +Priority, +Message
	    syslog/3,			% +Priority, +Format, +Args
	    closelog/0
	  ]).

/** <module> Unix syslog interface

This library provides an interface to   the  Unix syslog() facility. The
interface is an almost direct translation of  the POSIX syslog API, with
two additions:

  - syslog/3 exploits format/3 to format syslog messages
  - The library integrates into library(debug) using
    prolog:debug_print_hook/3, where debug _topics_ are mapped to
    syslog _priorities_ and remaining debug _topics_ are mapped
    to the syslog _priority_ =debug=.

Note that this interface  makes  no   attempt  to  abstract over logging
facilities of operating systems. We expect   that such abstractions will
be implemented at the Prolog  level   using  multiple  integrations into
library(debug).

@see	detach_IO/1 to detach normal I/O of the process and remove it
	from the process group.
@see	fork/1 to create a daemon process.
@see	library(uid) to manage user identifiers (e.g., drop root
	privileges).
*/

:- use_foreign_library(foreign(syslog)).

:- dynamic syslog/1.

%%	openlog(+Ident:atom, +Options:list(atom), +Facility:atom) is det.
%
%	Open system log. This predicate provides a direct interface into
%	the openlog() library call. If the   library call is successful,
%	it runs at_halt(closelog) to ensure closing   the  system log on
%	clean exit.
%
%	@param	Ident prepended to every message, and is typically set
%		to the program name.
%	@param	Options is a list of options.  Values are corresponding
%		C options, after removing =LOG_= and translation to
%		lower case: =cons=, =ndelay=, =nowait=, =odelay=,
%		=perror=, =pid=.
%	@param	Facility is one of =auth=, =authpriv=, =cron=, =daemon=,
%		=ftp=, =kern=, =local0= ... =local7=, =lpr=, =mail=,
%		=news=, =syslog=, =user= or =uucp=.

openlog(Ident, Options, Facility) :-
	'$openlog'(Ident, Options, Facility),
	asserta(syslog(Ident)),
	at_halt(closelog).

%%	syslog(+Priority, +Message) is det.
%
%	Send a message to the system log.
%
%	@param	Priority is one of =emerg=, =alert=, =crit=, =err=,
%		=warning=, =notice=, =info= or =debug=.

%%	syslog(+Priority, +Format, +Args) is det.
%
%	Send a formatted message to the system log.  This predicate
%	combined format/3 with syslog/2.

syslog(Priority, Format, Args) :-
	format(string(Msg), Format, Args),
	syslog(Priority, Msg).

%%	closelog is det.
%
%	Close the system log.

closelog :-
	retractall(syslog(_)),
	'$closelog'.


		 /*******************************
		 *     DEBUG INTEGRATION	*
		 *******************************/

:- multifile
	prolog:debug_print_hook/3.

%%	prolog:debug_print_hook(+Topic, +Format, +Args) is semidet.
%
%	Integration of debug/3 with the syslog   facility.  If syslog is
%	enabled, debug/3 is re-routed to use   the syslog facilities. If
%	the _topic_ of the  debug  message   matches  one  of  the sylog
%	_priority_ values (see syslog/2), the message   is sent with the
%	corresponding syslog priority. Otherwise  it   it  sent with the
%	=debug= priority.

prolog:debug_print_hook(Topic, Format, Args) :-
	syslog(_),
	debug_priority(Topic, Priority),
	syslog(Priority, Format, Args).

debug_priority(Topic, Priority) :-
	(   syslog_priority(Topic)
	->  Priority = Topic
	;   Priority = debug
	).

syslog_priority(emerg).
syslog_priority(alert).
syslog_priority(crit).
syslog_priority(err).
syslog_priority(warning).
syslog_priority(notice).
syslog_priority(info).
syslog_priority(debug).


		 /*******************************
		 *	MESSAGE INTEGRATION	*
		 *******************************/

user:message_hook(Term, Kind, _) :-
	kind_syslog_priority(Kind, Level),
	message_to_string(Term, Message),
	syslog(Level, Message),
	fail.

kind_syslog_priority(error,	    err).
kind_syslog_priority(warning,	    warning).
kind_syslog_priority(informational, info).
