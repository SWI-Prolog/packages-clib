/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(uid,
	  [ getuid/1,			% -UID
	    getgid/1,			% -GID
	    geteuid/1,			% -UID
	    getegid/1,			% -GID
	    user_info/2,		% +User, -UserInfo
	    group_info/2,		% +Group, -GroupInfo
	    user_data/3,		% +Field, +UserInfo, -Value
	    group_data/3,		% +Field, +GroupInfo, -Value
	    setuid/1,			% +UID
	    setgid/1,			% +GID
	    seteuid/1,			% +UID
	    setegid/1,			% +GID

	    set_user_and_group/1,	% +User
	    set_user_and_group/2	% +User, +Group
	  ]).

:- use_foreign_library(foreign(uid)).

/** <module> User and group management on Unix systems

This module provides and interface  to   user  and  group information on
Posix systems. In addition, it allows for changing user and group ids.

@see	Please check the documentation of your OS for details on the
	semantics of this predicates.
*/

%%	getuid(-UID) is det.
%
%	UID is the real user ID of the calling process.

%%	getgid(-GID) is det.
%
%	GID is the real group ID of the calling process.

%%	geteuid(-UID) is det.
%
%	UID is the effective user ID of the calling process.

%%	getegid(-GID) is det.
%
%	GID is the effective group ID of the calling process.

%%	user_info(+User, -UserData) is det.
%
%	UserData represent the passwd  information   for  User.  User is
%	either a numeric UID or a   user name. The predicate user_data/3
%	can be used to extract information from UserData.

%%	user_data(?Field, ?UserData, ?Value)
%
%	Value is the value for Field in UserData.  Defined fields are:
%
%	  * name
%	  Name of the user
%	  * password
%	  Password hash of the user (or =x= if this is not accessible)
%	  * uid
%	  Numeric user id of the user
%	  * gid
%	  Numeric primary group id of the user
%	  * comment
%	  The _gecos_ field
%	  * home
%	  Home directory of the user
%	  * shell
%	  Default (login) shell of the user.

user_data(name,	    user_info(Nam, _, _, _, _, _, _), Nam).
user_data(password, user_info(_, PWD, _, _, _, _, _), PWD).
user_data(uid,	    user_info(_, _, UID, _, _, _, _), UID).
user_data(gid,	    user_info(_, _, _, GID, _, _, _), GID).
user_data(comment,  user_info(_, _, _, _, GEC, _, _), GEC).
user_data(home,	    user_info(_, _, _, _, _, HOM, _), HOM).
user_data(shell,    user_info(_, _, _, _, _, _, SHE), SHE).

%%	group_info(+Group, -GroupData) is det.
%
%	GroupData represent the group information   for  Group. Group is
%	either a numeric GID or a group name. The predicate group_data/3
%	can be used to extract information from GroupData.

%%	group_data(?Field, ?GroupData, ?Value)
%
%	Value is the value for Field  GroupData.  Defined fields are:
%
%	  * name
%	  Name of the user
%	  * password
%	  Password hash of the user (or =x= if this is not accessible)
%	  * gid
%	  Numeric group id of the group
%	  * members
%	  List of user-names that are member of this group.

group_data(name,     group_info(Nam, _, _, _), Nam).
group_data(password, group_info(_, PWD, _, _), PWD).
group_data(gid,	     group_info(_, _, GID, _), GID).
group_data(members,  group_info(_, _, _, MBR), MBR).

		 /*******************************
		 *	       SETTING		*
		 *******************************/

%%	setuid(+UID)
%
%	Set the user id of the calling process.

%%	seteuid(+UID)
%
%	Set the effective user id of the calling process.


%%	setgid(+GID)
%
%	Set the group id of the calling process.

%%	setegid(+GID)
%
%	Set the effective group id of the calling process.


%%	set_user_and_group(+User) is det.
%%	set_user_and_group(+User, +Group) is det.
%
%	Set the UID and GID to the User. User  is either a UID or a user
%	name. If Group is not specified, the   primary  group of User is
%	used.

set_user_and_group(User) :-
	user_info(User, Data),
	user_data(uid, Data, UID),
	user_data(gid, Data, GID),
	setgid(GID),
	setuid(UID).

set_user_and_group(User, Group) :-
	user_info(User, Data),
	group_info(Group, GData),
	user_data(uid, Data, UID),
	group_data(gid, GData, GID),
	setgid(GID),
	setuid(UID).
