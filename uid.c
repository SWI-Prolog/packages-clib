/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, VU University Amsterdam
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

#include <config.h>
#include <SWI-Prolog.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>

static int
error(int error, const char *op, const char *type, term_t culprit)
{ if ( error == EAGAIN )
    return PL_resource_error("rlimit_nproc");
  else if ( error == EMFILE )
    return PL_resource_error("max_files");
  else if ( error == ENOMEM )
    return PL_resource_error("memory");
  else if ( error == ERANGE )
    return PL_resource_error("buffer_space");
  else
    return PL_permission_error(op, type, culprit);
}


static foreign_t
pl_getuid(term_t uid)
{ return PL_unify_integer(uid, getuid());
}

static foreign_t
pl_geteuid(term_t uid)
{ return PL_unify_integer(uid, geteuid());
}

static foreign_t
pl_getgid(term_t gid)
{ return PL_unify_integer(gid, getgid());
}

static foreign_t
pl_getegid(term_t gid)
{ return PL_unify_integer(gid, getegid());
}

/* Deal with the result of a get*_r() call.  Returns true to retry,
   false on error or -1 if we have a result.  On ERANGE, the buffer is
   enlarged.
*/

#define MAX_INFO_BUFSIZE (1024*1024)

static int
retry_info(int rc, char **buf, size_t *size,
	   const char *type, term_t obj)
{ if ( rc == 0 )
    return -1;
  if ( rc == EINTR )
    return PL_handle_signals() < 0 ? false : true;
  if ( rc == ERANGE && *size < MAX_INFO_BUFSIZE )
  { char *nbuf = realloc(*buf, *size*2);

    if ( !nbuf )
      return PL_resource_error("memory");
    *buf = nbuf;
    *size *= 2;
    return true;
  }

  return error(rc, "info", type, obj);
}


static foreign_t
pl_user_info(term_t user, term_t info)
{ int uid;
  struct passwd pwd, *pwdp = NULL;
  size_t size = 1024;
  char *buf = malloc(size);
  char *name;
  int rc;

  if ( !buf )
    return PL_resource_error("memory");

  if ( PL_get_integer(user, &uid) )
  { do
    { rc = retry_info(getpwuid_r(uid, &pwd, buf, size, &pwdp),
		      &buf, &size, "user", user);
    } while(rc == true);
  } else if ( PL_get_chars(user, &name, CVT_ATOMIC|REP_MB) )
  { do
    { rc = retry_info(getpwnam_r(name, &pwd, buf, size, &pwdp),
		      &buf, &size, "user", user);
    } while(rc == true);
  } else
  { free(buf);
    return PL_type_error("user", user);
  }

  if ( rc == -1 )
  { if ( !pwdp )
      rc = PL_existence_error("user", user);
    else
      rc = PL_unify_term(info,
			 PL_FUNCTOR_CHARS, "user_info", 7,
			   PL_MBCHARS, pwdp->pw_name,
			   PL_MBCHARS, pwdp->pw_passwd,
			   PL_INT, (int)pwdp->pw_uid,
			   PL_INT, (int)pwdp->pw_gid,
			   PL_MBCHARS, pwdp->pw_gecos,
			   PL_MBCHARS, pwdp->pw_dir,
			   PL_MBCHARS, pwdp->pw_shell
			);
  }
  free(buf);

  return rc;
}


static foreign_t
pl_group_info(term_t group, term_t info)
{ int gid;
  struct group grp, *pgrp = NULL;
  size_t size = 1024;
  char *buf = malloc(size);
  char *name;
  term_t members = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(members);
  term_t head = PL_new_term_ref();
  char **memp;
  int rc;

  if ( !buf )
    return PL_resource_error("memory");

  if ( PL_get_integer(group, &gid) )
  { do
    { rc = retry_info(getgrgid_r(gid, &grp, buf, size, &pgrp),
		      &buf, &size, "group", group);
    } while(rc == true);
  } else if ( PL_get_chars(group, &name, CVT_ATOMIC|REP_MB) )
  { do
    { rc = retry_info(getgrnam_r(name, &grp, buf, size, &pgrp),
		      &buf, &size, "group", group);
    } while(rc == true);
  } else
  { free(buf);
    return PL_type_error("group", group);
  }

  if ( rc == -1 )
  { if ( !pgrp )
    { rc = PL_existence_error("group", group);
    } else
    { rc = true;
      for(memp=pgrp->gr_mem; *memp; memp++)
      { if ( !PL_unify_list(tail, head, tail) ||
	     !PL_unify_chars(head, PL_ATOM|REP_MB, -1, *memp) )
	{ rc = false;
	  break;
	}
      }
      rc = ( rc &&
	     PL_unify_nil(tail) &&
	     PL_unify_term(info,
			   PL_FUNCTOR_CHARS, "group_info", 4,
			     PL_MBCHARS, pgrp->gr_name,
			     PL_MBCHARS, pgrp->gr_passwd,
			     PL_INT, (int)pgrp->gr_gid,
			     PL_TERM, members
			) );
    }
  }
  free(buf);

  return rc;
}


static foreign_t
pl_setuid(term_t uid)
{ int id;

  if ( !PL_get_integer_ex(uid, &id) )
    return false;

  if ( setuid(id) == 0 )
    return true;

  return error(errno, "setuid", "uid", uid);
}


static foreign_t
pl_setgid(term_t gid)
{ int id;

  if ( !PL_get_integer_ex(gid, &id) )
    return false;

  if ( setgid(id) == 0 )
    return true;

  return error(errno, "setgid", "gid", gid);
}


static foreign_t
pl_seteuid(term_t uid)
{ int id;

  if ( !PL_get_integer_ex(uid, &id) )
    return false;

  if ( seteuid(id) == 0 )
    return true;

  return error(errno, "seteuid", "uid", uid);
}


static foreign_t
pl_setegid(term_t gid)
{ int id;

  if ( !PL_get_integer_ex(gid, &id) )
    return false;

  if ( setegid(id) == 0 )
    return true;

  return error(errno, "setegid", "gid", gid);
}


#ifdef HAVE_INITGROUPS
static foreign_t
pl_initgroups(term_t user, term_t group)
{ char *u;
  int g;

  if ( !PL_get_integer_ex(group, &g) ||
       !PL_get_chars(user, &u, CVT_ATOMIC|REP_MB|CVT_EXCEPTION) )
    return false;

  if ( initgroups(u, g) == 0 )
    return true;

  return error(errno, "initgroups", "user", user);
}
#endif

static foreign_t
pl_getgroups(term_t Groups)
{ gid_t buf[32];
  gid_t *list = buf;
  int size = sizeof(buf)/sizeof(gid_t);
  int rc;

  for(;;)
  { rc = getgroups(size, list);

    if ( rc == -1 && errno == EINVAL )
    { size *= 2;
      gid_t *l2;

      if ( list == buf )
	l2 = malloc(sizeof(gid_t)*size);
      else
	l2 = realloc(list, sizeof(gid_t)*size);

      if ( !l2 )
      { if ( list != buf )
	  free(list);
	return PL_resource_error("memory");
      }

      list = l2;
    } else
      break;
  }

  if ( rc >= 0 )
  { term_t tail = PL_copy_term_ref(Groups);
    term_t head = PL_new_term_ref();
    int i;

    for(i=0; i<rc; i++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_integer(head, list[i]) )
      { rc = false;
	goto out;
      }
    }
    rc = PL_unify_nil(tail);
  } else
  { rc = error(errno, "getgroups", "", Groups);
  }

out:
  if ( list != buf )
    free(list);

  return rc;
}


#ifdef HAVE_SETGROUPS
static foreign_t
pl_setgroups(term_t Groups)
{ size_t len;

  if ( PL_skip_list(Groups, 0, &len) == PL_LIST )
  { gid_t *list;
    int rc;

    if ( (list=malloc((len ? len : 1)*sizeof(gid_t))) )
    { term_t tail = PL_copy_term_ref(Groups);
      term_t head = PL_new_term_ref();
      int i = 0;

      while((rc=PL_get_list_ex(tail,head,tail)))
      { int gid;

	if ( (rc=PL_get_integer_ex(head, &gid)) )
	  list[i++] = gid;
	else
	  goto out;
      }
      rc = PL_get_nil_ex(tail);

      if ( rc )
      { if ( setgroups(i, list) < 0 )
	  rc = error(errno, "setgroups", "", Groups);
	else
	  rc = true;
      }
    } else
      rc = PL_resource_error("memory");

  out:
    if ( list )
      free(list);
    return rc;
  }

  return PL_type_error("list", Groups);
}
#endif /*HAVE_SETGROUPS*/


install_t
install_uid()
{ PL_register_foreign("getuid", 1, pl_getuid, 0);
  PL_register_foreign("geteuid", 1, pl_geteuid, 0);
  PL_register_foreign("getgid", 1, pl_getgid, 0);
  PL_register_foreign("getegid", 1, pl_getegid, 0);
  PL_register_foreign("getgroups", 1, pl_getgroups, 0);
  PL_register_foreign("user_info", 2, pl_user_info, 0);
  PL_register_foreign("group_info", 2, pl_group_info, 0);
  PL_register_foreign("setuid", 1, pl_setuid, 0);
  PL_register_foreign("setgid", 1, pl_setgid, 0);
  PL_register_foreign("seteuid", 1, pl_seteuid, 0);
  PL_register_foreign("setegid", 1, pl_setegid, 0);
#ifdef HAVE_INITGROUPS
  PL_register_foreign("initgroups", 2, pl_initgroups, 0);
#endif
#ifdef HAVE_SETGROUPS
  PL_register_foreign("setgroups", 1, pl_setgroups, 0);
#endif
}

