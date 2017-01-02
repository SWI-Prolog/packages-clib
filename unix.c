/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
                              VU University Amsterdam
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <assert.h>
#include "clib.h"
#include <signal.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_CRT_EXTERNS_H
#include <crt_externs.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unix process management.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static IOSTREAM *
name_to_stream(const char *name)
{ IOSTREAM *s;
  term_t t = PL_new_term_ref();

  PL_put_atom_chars(t, name);
  if ( PL_get_stream_handle(t, &s) )
    return s;

  return NULL;
}


static void
flush_stream(const char *name)
{ IOSTREAM *s;

  if ( (s = name_to_stream(name)) )
    Sflush(s);

  PL_release_stream(s);
}



static foreign_t
pl_fork(term_t a0)
{ pid_t pid;

  flush_stream("user_output");		/* general call to flush all IO? */

  if ( (pid = fork()) < 0 )
    return PL_resource_error("memory");

  if ( pid > 0 )
  { return PL_unify_integer(a0, pid);
  } else
  { PL_set_prolog_flag("pid", PL_INTEGER|FF_FORCE, (intptr_t)getpid());

    return PL_unify_atom_chars(a0, "child");
  }
}


#define free_argv(n) \
	{ int _k; \
	  for( _k=1; _k <= n; _k++) \
	    free(argv[_k]); \
	  free(argv); \
	}

static foreign_t
pl_exec(term_t cmd)
{ size_t argc;
  atom_t name;

  if ( PL_get_name_arity(cmd, &name, &argc) )
  { term_t a = PL_new_term_ref();
    char **argv = malloc(sizeof(char*) * (argc + 2));
    size_t i;

    argv[0] = (char *)PL_atom_chars(name);

    for(i=1; i<=argc; i++)
    { char *s;

      if ( PL_get_arg(i, cmd, a) &&
	   PL_get_chars(a, &s, CVT_ALL|REP_MB|BUF_MALLOC) )
	argv[i] = s;
      else
      { free_argv(i-1);
	return pl_error("exec", 1, NULL, ERR_ARGTYPE, i, a, "atomic");
      }
    }
    argv[argc+1] = NULL;

    execvp(argv[0], argv);
    free_argv(argc);
    return pl_error("exec", 1, NULL, ERR_ERRNO, errno, "execute", "command", cmd);
  }

  return pl_error("exec", 1, NULL, ERR_ARGTYPE, 1, cmd, "compound");
}


static foreign_t
pl_wait(term_t Pid, term_t Status)
{ int status;
  pid_t pid = -1;
  int pid_i;

  if ( PL_is_variable(Pid) )
  { pid = -1;
  } else if ( PL_get_integer_ex(Pid, &pid_i) )
  { if ( pid_i <= 0 )
      return PL_domain_error("process_id", Pid);
    pid = pid_i;
  } else
    return FALSE;

  for(;;)
  { pid = waitpid(pid, &status, 0);
    if ( pid == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
    { break;
    }
  }

  if ( pid == -1 )
    return pl_error("wait", 2, NULL, ERR_ERRNO, errno, "wait", "process", Pid);

  if ( PL_unify_integer(Pid, pid) )
  { if ( WIFEXITED(status) )
      return PL_unify_term(Status,
			   CompoundArg("exited", 1),
			   IntArg(WEXITSTATUS(status)));
    if ( WIFSIGNALED(status) )
      return PL_unify_term(Status,
			   CompoundArg("signaled", 1),
			   IntArg(WTERMSIG(status)));
    if ( WIFSTOPPED(status) )
      return PL_unify_term(Status,
			   CompoundArg("stopped", 1),
			   IntArg(WSTOPSIG(status)));
    assert(0);
  }

  return FALSE;
}


static foreign_t
pl_kill(term_t Pid, term_t Sig)
{ int pid;
  int sig;

  if ( !PL_get_integer(Pid, &pid) )
    return pl_error("kill", 2, NULL, ERR_ARGTYPE, 1, Pid, "pid");
  if ( !PL_get_signum_ex(Sig, &sig) )
    return FALSE;

  if ( kill(pid, sig) < 0 )
    return pl_error("kill", 2, NULL, ERR_ERRNO, errno,
		    "kill", "process", Pid);

  return TRUE;
}


		 /*******************************
		 *	   STREAM STUFF		*
		 *******************************/

static foreign_t
pl_pipe(term_t Read, term_t Write)
{ int fd[2];
  IOSTREAM *in, *out;

  if ( pipe(fd) != 0 )
    return pl_error("pipe", 2, NULL, ERR_ERRNO, errno, "create", "pipe", 0);

  in  = Sfdopen(fd[0], "r");
  out = Sfdopen(fd[1], "w");

  if ( PL_open_stream(Read, in) &&
       PL_open_stream(Write, out) )
    return TRUE;

  return FALSE;
}


static int
get_stream_no(term_t t, IOSTREAM **s, int *fn)
{ if ( PL_get_integer(t, fn) )
    return TRUE;
  if ( PL_get_stream_handle(t, s) )
  { *fn = Sfileno(*s);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_dup(term_t from, term_t to)
{ IOSTREAM *f = NULL, *t = NULL;
  int rval = FALSE;
  int fn, tn;

  if ( !get_stream_no(from, &f, &fn) ||
       !get_stream_no(to, &t, &tn) )
    goto out;

  if ( dup2(fn, tn) < 0 )
  { pl_error("dup", 2, NULL, ERR_ERRNO, errno, "dup", "stream", from);
    goto out;
  } else
  { rval = TRUE;
  }

out:
  if ( f )
    PL_release_stream(f);
  if ( t )
    PL_release_stream(t);

  return rval;
}


static foreign_t
pl_environ(term_t l)
{
#ifdef HAVE__NSGETENVIRON
  char **environ = *_NSGetEnviron();
#else
  extern char **environ;
#endif
  char **e;
  term_t t = PL_copy_term_ref(l);
  term_t t2 = PL_new_term_ref();
  term_t nt = PL_new_term_ref();
  term_t vt = PL_new_term_ref();
  functor_t FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);

  for(e = environ; *e; e++)
  { char *s = strchr(*e, '=');

    if ( !s )
      s = *e + strlen(*e);

    { int len = s-*e;
      char *name = alloca(len+1);

      strncpy(name, *e, len);
      name[len] = '\0';
      PL_put_atom_chars(nt, name);
      PL_put_atom_chars(vt, s+1);
      if ( !PL_cons_functor(nt, FUNCTOR_equal2, nt, vt) ||
	   !PL_unify_list(t, t2, t) ||
	   !PL_unify(t2, nt) )
	return FALSE;
    }
  }

  return PL_unify_nil(t);
}


		 /*******************************
		 *	    DEAMON IO		*
		 *******************************/

static IOSTREAM *log_stream = NULL;		/* Write log output here */

static ssize_t
read_eof(void *handle, char *buf, size_t count)
{ return 0;
}


static ssize_t
write_null(void *handle, char *buf, size_t count)
{ if ( log_stream )
  { Slock(log_stream);
    Sfwrite(buf, count, sizeof(char), log_stream);
    Sunlock(log_stream);
  }

  return count;
}


static long
seek_error(void *handle, long pos, int whence)
{ return -1;
}


static int
close_null(void *handle)
{ return 0;
}


static IOFUNCTIONS dummy =
{ read_eof,
  write_null,
  seek_error,
  close_null,
  NULL
};


static void
close_underlying_fd(IOSTREAM *s)
{ if ( s )
  { int fd;

    if ( (fd = Sfileno(s)) >= 0 && (s->flags & SIO_ISATTY) )
    { close(fd);

      s->functions = &dummy;
      s->flags &= ~SIO_FILE|SIO_ISATTY;	/* no longer a file */
      s->flags |= SIO_LBUF;		/* do line-buffering */
    }
  }
}


static void
detach_process(void)
{
#ifdef HAVE_SETSID
  setsid();
#else
  int fd;

  if ( (fd = open("/dev/tty", 2)) )
  { ioctl(fd, TIOCNOTTY, NULL);		/* detach from controlling tty */
    close(fd);
  }
#endif
}

static foreign_t
pl_detach_IO(term_t stream)
{ if ( !log_stream )
  { IOSTREAM *s;

    if ( !PL_get_stream_handle(stream, &s) )
      return FALSE;
    log_stream = s;
    PL_release_stream(s);

    close_underlying_fd(Serror);
    close_underlying_fd(Soutput);
    close_underlying_fd(Sinput);

    detach_process();
  }

  return TRUE;
}

#if defined(HAVE_PRCTL) && defined(HAVE_SYS_PRCTL_H)
#include <sys/prctl.h>

static foreign_t
pl_prctl(term_t option)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(option, &name, &arity) )
  { const char *opt = PL_atom_chars(name);
    term_t a = PL_new_term_refs(4);

    if ( arity <= 4 )
    { size_t i;

      for(i=0; i<arity; i++)
	_PL_get_arg(i+1, option, a+i);

      if ( strcmp(opt, "set_dumpable") == 0 && arity == 1 )
      { int i;

	if ( !PL_get_bool_ex(a+0, &i) )
	  return FALSE;
	if ( prctl(PR_SET_DUMPABLE, i, 0, 0, 0) >= 0 )
	  return TRUE;
	return pl_error("prctl", 1, NULL, ERR_ERRNO,
			errno, "set_dumpable", "process", a+0);
      } else if ( strcmp(opt, "get_dumpable") == 0 && arity == 1 )
      { int rc = prctl(PR_GET_DUMPABLE, 0,0,0,0);

	if ( rc >= 0 )
	  return PL_unify_bool(a+0, rc);
	return pl_error("prctl", 1, NULL, ERR_ERRNO,
			errno, "get_dumpable", "process", a+0);
      }
    }

    return PL_domain_error("prctl_option", option);
  } else
  { return PL_type_error("compound", option);
  }
}


#else
#undef HAVE_PRCTL
#endif

#ifdef HAVE_SYSCONF
#include <unistd.h>

struct conf
{ int         code;
  const char *name;
};

static const struct conf confs[] =
{ {_SC_ARG_MAX, "arg_max" },
  {_SC_CHILD_MAX, "child_max" },
  {_SC_CLK_TCK, "clk_tck" },
  {_SC_OPEN_MAX, "open_max" },
  {_SC_PAGESIZE, "pagesize" },
#ifdef _SC_PHYS_PAGES
  {_SC_PHYS_PAGES, "phys_pages" },
#endif
#ifdef _SC_AVPHYS_PAGES
  {_SC_AVPHYS_PAGES, "avphys_pages" },
#endif
#ifdef _SC_NPROCESSORS_CONF
  {_SC_NPROCESSORS_CONF, "nprocessors_conf" },
#endif
#ifdef _SC_NPROCESSORS_ONLN
  {_SC_NPROCESSORS_ONLN, "nprocessors_onln" },
#endif
  {0, NULL}
};

static foreign_t
pl_sysconf(term_t conf)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(conf, &name, &arity) )
  { const char *s = PL_atom_chars(name);
    const struct conf *c = confs;

    for(c=confs; c->name; c++)
    { if ( strcmp(c->name, s) == 0 )
      { term_t a;

	return ( (a = PL_new_term_ref()) &&
		 PL_get_arg(1, conf, a) &&
		 PL_unify_integer(a, sysconf(c->code))
	       );
      }
    }

    return FALSE;
  }
  return PL_type_error("compound", conf);
}
#endif



install_t
install_unix()
{ PL_register_foreign("fork_",     1, pl_fork, 0);
  PL_register_foreign("exec",      1, pl_exec, 0);
  PL_register_foreign("wait",      2, pl_wait, 0);
  PL_register_foreign("kill",      2, pl_kill, 0);
  PL_register_foreign("pipe",      2, pl_pipe, 0);
  PL_register_foreign("dup",       2, pl_dup, 0);
  PL_register_foreign("detach_IO", 1, pl_detach_IO, 0);
  PL_register_foreign("environ",   1, pl_environ, 0);
#ifdef HAVE_PRCTL
  PL_register_foreign("prctl",     1, pl_prctl, 0);
#endif
#ifdef HAVE_SYSCONF
  PL_register_foreign("sysconf",   1, pl_sysconf, 0);
#endif
}







