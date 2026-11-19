/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2025, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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
#define _CRT_SECURE_NO_WARNINGS 1
#define SWIPL_WINDOWS_NATIVE_ACCESS 1

#ifdef _WIN32				/* STARTUPINFOEXW, see */
#if !defined(_WIN32_WINNT) || _WIN32_WINNT < 0x0600 /* do_create_process() */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#endif

/*#define O_DEBUG 1*/
#undef O_DEBUG
#define _GNU_SOURCE			/* get pipe2() */
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include "error.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_PRCTL
#include <sys/prctl.h>
#endif

#ifdef _REENTRANT
#include <pthread.h>
#endif

#ifdef HAVE_CRT_EXTERNS_H
#include <crt_externs.h>	/* _NSGetEnviron() on MacOS */
#endif

static atom_t ATOM_stdin;
static atom_t ATOM_stdout;
static atom_t ATOM_stderr;
static atom_t ATOM_std;
static atom_t ATOM_null;
static atom_t ATOM_timeout;
static atom_t ATOM_infinite;
static atom_t ATOM_text;
static atom_t ATOM_binary;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_process_error2;
static functor_t FUNCTOR_system_error2;
static functor_t FUNCTOR_pipe1;
static functor_t FUNCTOR_pipe2;
static functor_t FUNCTOR_stream1;
static functor_t FUNCTOR_exit1;
#ifndef __WINDOWS__
static functor_t FUNCTOR_killed1;
#endif
static functor_t FUNCTOR_eq2;		/* =/2 */
static functor_t FUNCTOR_type1;
static functor_t FUNCTOR_encoding1;

#define MAYBE 2

#if O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif

#ifdef __WINDOWS__
#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>

typedef wchar_t echar;			/* environment character */
#define ecslen(s) wcslen(s)

#ifndef CREATE_BREAKAWAY_FROM_JOB
#define CREATE_BREAKAWAY_FROM_JOB 0x1000000
#endif

#else
typedef char echar;
#define ecslen(s) strlen(s)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISSUES:
	- Deal with child errors (no cwd, cannot execute, etc.)
	- Complete test suite
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	       ADMIN		*
		 *******************************/

typedef enum std_type
{ std_std,
  std_null,
  std_pipe,
  std_stream
} std_type;


typedef struct p_stream
{ term_t   term;			/* P in pipe(P) */
  std_type type;			/* type of stream */
  IOENC	   encoding;			/* Encoding for the stream */
#ifdef __WINDOWS__
  HANDLE   fd[2];			/* pipe handles */
#else
  int      fd[2];			/* pipe handles */
#endif
  int	   cloexec;			/* close on exec activated */
} p_stream;


typedef struct ecbuf
{ echar *buffer;
  size_t size;
  size_t allocated;
} ecbuf;


typedef struct p_options
{ atom_t exe_name;			/* exe as atom */
#ifdef __WINDOWS__
  wchar_t *exe;				/* Executable */
  wchar_t *cmdline;			/* Command line */
  wchar_t *cwd;				/* CWD of new process */
#else
  char *exe;				/* Executable */
  char **argv;				/* argument vector */
  char *cwd;				/* CWD of new process */
  char **envp;				/* New environment */
#endif
  ecbuf  envbuf;			/* environment buffer */
  term_t pid;				/* process(PID) */
  int pipes;				/* #pipes found */
  p_stream streams[3];
  int   detached;			/* create as detached */
  int   window;				/* Show a window? */
  int   priority;			/* Process priority */
} p_options;


typedef struct wait_options
{ double timeout;
  int	 has_timeout;
  int	 release;
} wait_options;


/* True when we start the process and never look at it again: detached(true)
   without process(PID) and without pipes.  In this case process_create/3
   returns as soon as the process is started.  On POSIX systems we fork
   twice, such that the process is inherited by init and we do not have to
   reclaim it.  On Windows we simply close the handles.
*/

static bool
fire_and_forget(const p_options *info)
{ return info->detached && info->pid == 0 && info->pipes == 0;
}


typedef enum create_method
{ PCREATE_SPAWN,
  PCREATE_VFORK,
  PCREATE_FORK
} create_method;

#ifndef __WINDOWS__
static create_method create_process_method = PCREATE_SPAWN;
#endif

#ifdef __WINDOWS__
static bool win_command_line(term_t t, size_t arity,
			    const wchar_t *exepath, wchar_t **cmdline);
#endif

		 /*******************************
		 *	  STRING BUFFER		*
		 *******************************/

static void
free_ecbuf(ecbuf *b)
{ if ( b->buffer )
  { PL_free(b->buffer);
    b->buffer = NULL;
  }
}


static bool
add_ecbuf(ecbuf *b, echar *data, size_t len)
{ if ( b->size + len > b->allocated )
  { size_t newsize = (b->allocated ? b->allocated * 2 : 2048);

    while( b->size + len > newsize )
      newsize *= 2;

    if ( b->buffer )
    { b->buffer = PL_realloc(b->buffer, newsize*sizeof(echar));
    } else
    { b->buffer = PL_malloc(newsize*sizeof(echar));
    }

    b->allocated = newsize;
  }

  memcpy(b->buffer+b->size, data, len*sizeof(echar));
  b->size += len;

  return true;
}

		 /*******************************
		 *	ENVIRONMENT PARSING	*
		 *******************************/

static bool
get_echars_arg_ex(int i, term_t from, term_t arg, echar **sp, size_t *lenp)
{ if ( !PL_get_arg(i, from, arg) )
    return false;

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, NULL, sp, CVT_ATOMIC|CVT_EXCEPTION) )
    return false;
  *lenp = wcslen(*sp);
#else
  if ( !PL_get_chars(arg, sp, CVT_ATOMIC|CVT_EXCEPTION|REP_FN) )
    return false;
  *lenp = strlen(*sp);
#endif

  return true;
}

#ifdef __WINDOWS__
#define ECHARS(s) L##s
#else
#define ECHARS(s) s
#endif

static bool
already_in_env(const echar *env, int count, const echar *e)
{ for(; count-- > 0; env += ecslen(env)+1)
  { const echar *s, *q;

    for(s=env, q=e; *s && *q && *s == *q && *s != '=' && *q != '='; s++,q++)
      ;
    if (*s == *q && *s == '=' )
      return true;
  }

  return false;
}

static bool
parse_environment(term_t t, p_options *info, int pass)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  ecbuf *eb   = &info->envbuf;
  int count = 0;
#ifndef __WINDOWS__
  echar *q;
  char **ep;
  int c = 0;
#endif

  if ( eb->buffer )
    return PL_permission_error("redefine", "environment", t);

  while( PL_get_list(tail, head, tail) )
  { echar *s;
    size_t len;

    if ( !PL_is_functor(head, FUNCTOR_eq2) )
      return PL_type_error("environment_variable", head);

    if ( !get_echars_arg_ex(1, head, tmp, &s, &len) )
      return false;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("="), 1);
    if ( !get_echars_arg_ex(2, head, tmp, &s, &len) )
      return false;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("\0"), 1);

    count++;
  }

  if ( !PL_get_nil_ex(tail) )
    return false;

  if ( pass && count == 0 )
    return true;			/* environment([]) is a no-op */

  if ( pass )
  {
#ifndef __WINDOWS__
#ifdef HAVE__NSGETENVIRON
    char **environ = *_NSGetEnviron();
#else
    extern char **environ;
#endif
    char **e;
    int count0 = count;

    for(e=environ; e && *e; e++)
    { if ( !already_in_env(eb->buffer, count0, *e) )
      { add_ecbuf(eb, *e, strlen(*e));
	add_ecbuf(eb, ECHARS("\0"), 1);
	count++;
      }
    }
#else /*__WINDOWS__*/
    echar *env = GetEnvironmentStringsW();

    if ( env )
    { int count0 = count;

      while(*env)
      { size_t len = wcslen(env);
	if ( !already_in_env(eb->buffer, count0, env) )
	{ add_ecbuf(eb, env, len+1);
	  count++;
	}
	env += len+1;
      }
    }
#endif /*__WINDOWS__*/
  }

#ifdef __WINDOWS__
  add_ecbuf(eb, ECHARS("\0"), 1);
  if ( count == 0 )
    add_ecbuf(eb, ECHARS("\0"), 1);
#else
  info->envp = PL_malloc((count+1)*sizeof(char*));

  for(ep=info->envp, c=0, q=eb->buffer; c<count; c++, ep++)
  { *ep = q;
    q += strlen(q)+1;
  }
  assert((size_t)(q-eb->buffer) == eb->size);
  *ep = NULL;
#endif

  return true;
}


static bool
get_type(term_t head, IOENC *enc)
{ atom_t a;

  if ( PL_get_atom_ex(head, &a) )
  { if ( a == ATOM_text )
      *enc = ENC_ANSI;
    else if ( a == ATOM_binary )
      *enc = ENC_OCTET;
    else
      return PL_domain_error("stream_type", head);

    return true;
  }

  return false;
}

/* TBD: provide a public API for translating encoding names to
 * the IOENC enum
 */

static bool
get_encoding(term_t head, IOENC *enc)
{ atom_t a;

  if ( PL_get_atom_ex(head, &a) )
  { IOENC e;

    if ( (e=PL_atom_to_encoding(a)) != ENC_UNKNOWN )
    { *enc = e;
      return true;
    }

    return PL_domain_error("encoding", head);
  }

  return false;
}


static bool
get_stream(term_t t, p_options *info, p_stream *stream, atom_t name)
{ atom_t a;
  int i;
  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { stream->type = std_null;
      return true;
    } else if ( a == ATOM_std )
    { stream->type = std_std;
      return true;
    } else
    { return PL_domain_error("process_stream", t);
    }
  } else if ( PL_is_functor(t, FUNCTOR_pipe1) ||
	      PL_is_functor(t, FUNCTOR_pipe2) )
  { stream->term = PL_new_term_ref();
    stream->encoding = ENC_ANSI;
    _PL_get_arg(1, t, stream->term);
    if ( !PL_is_variable(stream->term) )
    { for (i = 0; i < info->pipes; i++)
      { if (PL_compare(info->streams[i].term, t) == 0)
          break;
      }
      if (i == info->pipes)
        return PL_uninstantiation_error(stream->term);
    }
    if ( PL_is_functor(t, FUNCTOR_pipe2) )
    { term_t tail = PL_new_term_ref();
      term_t head = PL_new_term_ref();

      _PL_get_arg(2, t, tail);
      while(PL_get_list_ex(tail, head, tail))
      { if ( PL_is_functor(head, FUNCTOR_type1) )
	{ _PL_get_arg(1, head, head);
	  if ( !get_type(head, &stream->encoding) )
	    return false;
	} else if ( PL_is_functor(head, FUNCTOR_encoding1) )
	{ _PL_get_arg(1, head, head);
	  if ( !get_encoding(head, &stream->encoding) )
	    return false;
	} else
	  return PL_domain_error("pipe_option", head);
      }
      if ( !PL_get_nil_ex(tail) )
	return false;
    }
    stream->type = std_pipe;
    info->pipes++;
    return true;
  } else if ( PL_is_functor(t, FUNCTOR_stream1) )
  { IOSTREAM *s;
    stream->term = PL_new_term_ref();
    _PL_get_arg(1, t, stream->term);
    if ( !PL_get_stream(stream->term, &s,
			name == ATOM_stdin ? SIO_INPUT : SIO_OUTPUT) )
      return false;
    stream->type = std_stream;

#ifdef __WINDOWS__
    HANDLE h = Swinhandle(s);
    if ( h )
      stream->fd[0] = stream->fd[1] = h;
    else
      return PL_domain_error("file_stream", stream->term);
#else
    int fd = Sfileno(s);
    if ( fd >= 0 )
      stream->fd[0] = stream->fd[1] = fd;
    else
      return PL_domain_error("file_stream", stream->term);
#endif

    return true;
  } else
    return PL_type_error("process_stream", t);
}


static PL_option_t process_options[] =
{ PL_OPTION("stdin",	   OPT_TERM),
  PL_OPTION("stdout",	   OPT_TERM),
  PL_OPTION("stderr",	   OPT_TERM),
  PL_OPTION("process",	   OPT_TERM),
  PL_OPTION("detached",	   OPT_BOOL),
  PL_OPTION("cwd",	   OPT_TERM),
  PL_OPTION("window",	   OPT_BOOL),
  PL_OPTION("env",	   OPT_TERM),
  PL_OPTION("environment", OPT_TERM),
  PL_OPTION("priority",	   OPT_INT),
  PL_OPTIONS_END
};

static bool
parse_options(term_t options, p_options *info)
{ term_t stdin_t = 0, stdout_t = 0, stderr_t = 0, process_t = 0;
  term_t cwd_t = 0, env_t = 0, environment_t = 0;

  info->window = MAYBE;

  if ( !PL_scan_options(options, OPT_UNKNOWN_ERROR, "process_option",
			process_options,
			&stdin_t, &stdout_t, &stderr_t, &process_t,
			&info->detached, &cwd_t, &info->window, &env_t,
			&environment_t, &info->priority) )
    return false;

  if ( stdin_t && !get_stream(stdin_t, info, &info->streams[0], ATOM_stdin) )
    return false;
  if ( stdout_t && !get_stream(stdout_t, info, &info->streams[1], ATOM_stdout) )
    return false;
  if ( stderr_t && !get_stream(stderr_t, info, &info->streams[2], ATOM_stderr) )
    return false;
  if ( process_t )
    info->pid = PL_copy_term_ref(process_t);
  if ( cwd_t )
  {
#ifdef __WINDOWS__
    if ( !PL_get_wchars(cwd_t, NULL, &info->cwd,
		       CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC) )
      return false;
#else
    if ( !PL_get_chars(cwd_t, &info->cwd,
		       CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
      return false;
#endif
  }
  if ( env_t && !parse_environment(env_t, info, false) )
    return false;
  if ( environment_t && !parse_environment(environment_t, info, true) )
    return false;
  if ( info->priority != 255 &&
       (info->priority < -20 || info->priority > 19) )
  { term_t t;

    return ( (t=PL_new_term_ref()) &&
	     PL_put_integer(t, info->priority) &&
	     PL_domain_error("priority_option", t) );
  }

  return true;
}


static bool
get_exe(term_t exe, p_options *info)
{ size_t arity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_name_arity(exe, &info->exe_name, &arity) )
    return PL_type_error("callable", exe);

  PL_put_atom(arg, info->exe_name);

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, NULL, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC) )
    return false;
  if ( !win_command_line(exe, arity, info->exe, &info->cmdline) )
    return false;
#else /*__WINDOWS__*/
  if ( !PL_get_chars(arg, &info->exe,
		     CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
    return false;

  if ( !(info->argv = PL_malloc((arity+2)*sizeof(char*))) )
    return PL_resource_error("memory");
  memset(info->argv, 0, (arity+2)*sizeof(char*));
  if ( !(info->argv[0] = PL_malloc(strlen(info->exe)+1)) )
    return PL_resource_error("memory");
  strcpy(info->argv[0], info->exe);

  { size_t i;

    for(i=1; i<=arity; i++)
    { _PL_get_arg(i, exe, arg);

      if ( !PL_get_chars(arg, &info->argv[i],
			 CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return false;
    }
    info->argv[i] = NULL;
  }
#endif /*__WINDOWS__*/

  return true;
}


static void
free_options(p_options *info)		/* TBD: close streams */
{ if ( info->exe )
  { PL_free(info->exe);
    info->exe = NULL;
  }
  if ( info->cwd )
  { PL_free(info->cwd);
    info->cwd = NULL;
  }
#ifndef __WINDOWS__
  if ( info->envp )
  { PL_free(info->envp);
    info->envp = NULL;
  }
#endif
  free_ecbuf(&info->envbuf);
#ifdef __WINDOWS__
  if ( info->cmdline )
  { PL_free(info->cmdline);
    info->cmdline = NULL;
  }

#else /*__WINDOWS__*/

  if ( info->argv )
  { char **a;
    for(a=info->argv; *a; a++)
    { if ( *a )
	PL_free(*a);
    }
    PL_free(info->argv);

    info->argv = NULL;
  }

#endif /*__WINDOWS__*/
}


		 /*******************************
		 *	   PROCESS READS	*
		 *******************************/

#define	PROCESS_MAGIC	0x29498001

typedef struct process_context
{ int	magic;				/* PROCESS_MAGIC */
#ifdef __WINDOWS__
  HANDLE handle;			/* process handle */
  IOSTREAM *console;			/* terminal whose console it holds */
#else
  pid_t	pid;				/* the process id */
#endif
  int   open_mask;			/* Open streams */
  int   pipes[3];			/* stdin/stdout/stderr */
  atom_t exe_name;			/* exe as atom */
} process_context;

static int wait_for_process(process_context *pc);

static int
process_fd(void *handle, process_context **PC)
{ process_context *pc = (process_context*) ((uintptr_t)handle&~(uintptr_t)0x3);
  int pipe = (int)(uintptr_t)handle & 0x3;

  if ( pc->magic == PROCESS_MAGIC )
  { if ( PC )
      *PC = pc;
    return pc->pipes[pipe];
  }

  return -1;
}


static ssize_t
Sread_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.read)((void*)(uintptr_t)fd, buf, size);
}


static ssize_t
Swrite_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.write)((void*)(uintptr_t)fd, buf, size);
}


static int
Sclose_process(void *handle)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  if ( fd >= 0 )
  { int which = (int)(uintptr_t)handle & 0x3;
    int rc;

    rc = (*Sfilefunctions.close)((void*)(uintptr_t)fd);
    pc->open_mask &= ~(1<<which);

    DEBUG(Sdprintf("Closing fd[%d]; mask = 0x%x\n", which, pc->open_mask));

    if ( !pc->open_mask )
    { int rcw = wait_for_process(pc);

      return rcw ? 0 : -1;
    }

    return rc;
  }

  return -1;
}


static int
Scontrol_process(void *handle, int action, void *arg)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  switch(action)
  { case SIO_GETFILENO:
    { int *fdp = arg;
      *fdp = fd;
      return 0;
    }
    default:
      return (*Sfilefunctions.control)((void*)(uintptr_t)fd, action, arg);
  }
}


static IOFUNCTIONS Sprocessfunctions =
{ Sread_process,
  Swrite_process,
  NULL,					/* seek */
  Sclose_process,
  Scontrol_process,
  NULL					/* seek64 */
};


static IOSTREAM *
open_process_pipe(process_context *pc, p_options *info, int which, int fdn)
{ void *handle;
#ifdef __WINDOWS__
  HANDLE fd = info->streams[which].fd[fdn];
#else
  int fd = info->streams[which].fd[fdn];
#endif
  int flags = SIO_RECORDPOS|SIO_FBUF;
  IOSTREAM *s;

  pc->open_mask |= (1<<which);
#ifdef __WINDOWS__
  pc->pipes[which] = Swin_open_osfhandle(fd, _O_BINARY);
#else
  pc->pipes[which] = fd;
#endif

  if ( info->streams[which].encoding != ENC_OCTET )
    flags |= SIO_TEXT;

  if ( which == 0 )
    flags |= SIO_OUTPUT;
  else
    flags |= SIO_INPUT;

  handle = (void *)((uintptr_t)pc | (uintptr_t)which);

  if ( (s=Snew(handle, flags, &Sprocessfunctions)) )
    s->encoding = info->streams[which].encoding;

  return s;
}


		 /*******************************
		 *	       OS STUFF		*
		 *******************************/


#ifdef __WINDOWS__

CRITICAL_SECTION process_lock;
#define LOCK()   EnterCriticalSection(&process_lock);
#define UNLOCK() LeaveCriticalSection(&process_lock);

/* We need a root job to put non-detached processes into */
static HANDLE rootJob = (HANDLE)0;

static IOFUNCTIONS Sprocessfilefunctions = { 0, };

static void
win_init(void)
{ InitializeCriticalSection(&process_lock);
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;

  rootJob = CreateJobObject(NULL, NULL);
  if (rootJob == NULL) /* Already in a job */
    return;

  memset(&jeli, 0, sizeof(jeli));
  jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  /* This will fail (silently) if we dont have permission to manage processes */
  SetInformationJobObject(rootJob, JobObjectExtendedLimitInformation,
			  &jeli, sizeof(jeli));

  memcpy(&Sprocessfilefunctions, &Sfilefunctions, sizeof(IOFUNCTIONS));
  Sprocessfilefunctions.seek = NULL;
  Sprocessfilefunctions.seek64 = NULL;
}


#include "win_error.c"


/* Build a Windows command line from the arguments.  Windows programs
 * split the command line using the rules of the C runtime
 * (CommandLineToArgvW()): an argument is quoted using "..." if it is
 * empty or contains white space or a ", backslashes are literal unless
 * they precede a ", where 2n backslashes give n backslashes and 2n+1
 * give n backslashes and a literal ".  Batch files (.bat, .cmd) are run
 * by cmd.exe, which does not use these rules: there we also quote the
 * metacharacters of cmd.exe and refuse characters that cannot be passed
 * safely.
 */

typedef struct wbuf
{ wchar_t *base;
  size_t   len;
  size_t   size;
} wbuf;

static bool
wbuf_add(wbuf *b, wchar_t c)
{ if ( b->len+1 >= b->size )
  { size_t nsize = b->size ? b->size*2 : 256;
    wchar_t *n = PL_realloc(b->base, nsize*sizeof(wchar_t));

    if ( !n )
      return false;
    b->base = n;
    b->size = nsize;
  }
  b->base[b->len++] = c;
  b->base[b->len] = 0;
  return true;
}

static bool
wbuf_add_n(wbuf *b, wchar_t c, size_t n)
{ while(n-- > 0)
  { if ( !wbuf_add(b, c) )
      return false;
  }
  return true;
}

static bool
is_batch_file(const wchar_t *exe)
{ size_t len = wcslen(exe);

  return ( len > 4 &&
	   ( _wcsicmp(exe+len-4, L".bat") == 0 ||
	     _wcsicmp(exe+len-4, L".cmd") == 0 ) );
}

static bool
add_quoted_arg(wbuf *b, const wchar_t *s, size_t len, int batch)
{ int needq = (len == 0);
  size_t i;

  for(i=0; i<len; i++)
  { wchar_t c = s[i];

    if ( c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '"' )
      needq = true;
    else if ( batch && wcschr(L"&|<>^()", c) )
      needq = true;
  }

  if ( !needq )
  { for(i=0; i<len; i++)
    { if ( !wbuf_add(b, s[i]) )
	return false;
    }
    return true;
  }

  if ( !wbuf_add(b, '"') )
    return false;
  for(i=0; i<len; )
  { size_t bs = 0;

    while(i < len && s[i] == '\\')
    { bs++;
      i++;
    }
    if ( i == len )			/* backslashes before closing quote */
    { if ( !wbuf_add_n(b, '\\', bs*2) )
	return false;
      break;
    } else if ( s[i] == '"' )		/* backslashes before a quote */
    { if ( !wbuf_add_n(b, '\\', bs*2+1) ||
	   !wbuf_add(b, '"') )
	return false;
      i++;
    } else
    { if ( !wbuf_add_n(b, '\\', bs) ||
	   !wbuf_add(b, s[i]) )
	return false;
      i++;
    }
  }
  return wbuf_add(b, '"');
}

/* Characters that cmd.exe interprets also inside "...": quotes end the
   quoted string, % and ! expand variables, CR and LF end the command. */

static bool
batch_safe_arg(const wchar_t *s, size_t len)
{ size_t i;

  for(i=0; i<len; i++)
  { if ( wcschr(L"\"%!\r\n", s[i]) )
      return false;
  }
  return true;
}

static bool
win_command_line(term_t t, size_t arity, const wchar_t *exe, wchar_t **cline)
{ if ( arity > 0 )
  { term_t arg = PL_new_term_ref();
    wbuf b = {0};
    const wchar_t *base;
    int batch = is_batch_file(exe);
    size_t i;

    if ( batch )			/* cmd.exe runs the command line */
      base = exe;
    else if ( (base=wcsrchr(exe, '\\')) )
      base++;
    else
      base = exe;
					/* argv[0]: quotes only */
    if ( wcschr(base, ' ') || wcschr(base, '\t') )
    { if ( !wbuf_add(&b, '"') )
	goto nomem;
      for(const wchar_t *s=base; *s; s++)
      { if ( !wbuf_add(&b, *s) )
	  goto nomem;
      }
      if ( !wbuf_add(&b, '"') )
	goto nomem;
    } else
    { for(const wchar_t *s=base; *s; s++)
      { if ( !wbuf_add(&b, *s) )
	  goto nomem;
      }
    }

    for( i=1; i<=arity; i++)
    { size_t len;
      wchar_t *text;

      _PL_get_arg(i, t, arg);
      if ( !PL_get_wchars(arg, NULL, &text,
			  CVT_ATOMIC|CVT_EXCEPTION|BUF_STACK) )
	goto error;
      len = wcslen(text);
      if ( batch && !batch_safe_arg(text, len) )
      { PL_domain_error("batch_file_argument", arg);
	goto error;
      }
      if ( !wbuf_add(&b, ' ') ||
	   !add_quoted_arg(&b, text, len, batch) )
	goto nomem;
    }

    *cline = b.base;
    return true;

  nomem:
    PL_resource_error("memory");
  error:
    if ( b.base )
      PL_free(b.base);
    return false;
  } else
  { *cline = NULL;
  }

  return true;
}

typedef struct win_process
{ DWORD pid;
  HANDLE handle;
  HANDLE job;
  IOSTREAM *console;			/* terminal whose console it holds */
  struct win_process *next;
} win_process;


static win_process *processes;

static void
register_process(DWORD pid, HANDLE h, HANDLE job, IOSTREAM *console)
{ win_process *wp = PL_malloc(sizeof(*wp));

  wp->pid = pid;
  wp->handle = h;
  wp->job = job;
  wp->console = console;
  LOCK();
  wp->next = processes;
  processes = wp;
  UNLOCK();
}


static bool
unregister_process(DWORD pid)
{ win_process **wpp, *wp;

  LOCK();
  for(wpp=&processes, wp=*wpp; wp; wpp=&wp->next, wp=*wpp)
  { if ( wp->pid == pid )
    { IOSTREAM *console = wp->console;

      *wpp = wp->next;
      PL_free(wp);
      UNLOCK();
      if ( console )			/* the terminal takes it back */
	Swinrelease_pseudoconsole(console);
      return true;
    }
  }

  UNLOCK();
  return false;
}


static HANDLE
find_process_from_pid(DWORD pid, const char *pred)
{ win_process *wp;

  LOCK();
  for(wp=processes; wp; wp=wp->next)
  { if ( wp->pid == pid )
    { HANDLE h = wp->handle;
      UNLOCK();
      return h;
    }
  }

  UNLOCK();

  if ( pred )
  { term_t ex = PL_new_term_ref();

    if ( PL_put_integer(ex, pid) )
      PL_existence_error("process", ex);
  }

  return (HANDLE)0;
}

static HANDLE
find_job_from_pid(DWORD pid, const char *pred)
{ win_process *wp;

  LOCK();
  for(wp=processes; wp; wp=wp->next)
  { if ( wp->pid == pid )
    { HANDLE h = wp->job;
      UNLOCK();
      return h;
    }
  }

  UNLOCK();

  if ( pred )
  { term_t ex = PL_new_term_ref();

    if ( PL_put_integer(ex, pid) )
      PL_existence_error("process", ex);
  }

  return (HANDLE)0;
}


#define WP_TIMEOUT 2

static int
wait_process_handle(HANDLE process, ULONG *rc, DWORD timeout)
{ DWORD wc;

retry:
  wc = MsgWaitForMultipleObjects(1,
				 &process,
				 false,	/* return on any event */
				 timeout,
				 QS_ALLINPUT);

  switch(wc)
  { case WAIT_OBJECT_0:
      if ( !GetExitCodeProcess(process, rc) )
      { win_error("GetExitCodeProcess");
	CloseHandle(process);
	return false;
      }
      CloseHandle(process);
      return true;
    case WAIT_OBJECT_0+1:
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
	if ( PL_handle_signals() < 0 )
	  return false;
      }
      goto retry;
    }
    case WAIT_TIMEOUT:
      return WP_TIMEOUT;
    default:
      win_error("WaitForSingleObject");
      CloseHandle(process);
      return false;
  }
}


static bool
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ HANDLE *h;

  if ( (h=find_process_from_pid(pid, "process_wait")) )
  { ULONG rc;
    DWORD timeout;
    int wc;

    if ( opts->has_timeout )
      timeout = (DWORD)(opts->timeout * 1000.0);
    else
      timeout = INFINITE;

    if ( !(wc=wait_process_handle(h, &rc, timeout)) )
      return false;
    if ( wc == WP_TIMEOUT )
      return PL_unify_atom(code, ATOM_timeout);

    unregister_process(pid);

    return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_LONG, rc);
  } else
  { return false;
  }
}


static bool
win_wait_success(atom_t exe, HANDLE process)
{ ULONG rc;

  if ( !wait_process_handle(process, &rc, INFINITE) )
    return false;

  if ( rc != 0 )
  { term_t ex = PL_new_term_ref();

    if ( PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
			 PL_FUNCTOR, FUNCTOR_process_error2,
			   PL_ATOM, exe,
			   PL_FUNCTOR, FUNCTOR_exit1,
			     PL_LONG, rc,
			 PL_VARIABLE) )
      return PL_raise_exception(ex);
    return false;
  }

  return true;
}


static int
wait_for_process(process_context *pc)
{ int rc = win_wait_success(pc->exe_name, pc->handle);

  if ( pc->console )			/* the terminal takes it back */
    Swinrelease_pseudoconsole(pc->console);
  PL_unregister_atom(pc->exe_name);
  PL_free(pc);

  return rc;
}


static bool
create_pipes(p_options *info)
{ int i;
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term && s->type == std_pipe )
    { if ( i == 2 && info->streams[1].term &&
	   PL_compare(info->streams[1].term, info->streams[2].term) == 0 )
      { s->fd[0] = info->streams[1].fd[0];
	s->fd[1] = info->streams[1].fd[1];
      } else
      { if ( !CreatePipe(&s->fd[0], &s->fd[1], &sa, 1<<13) )
	{ return win_error("CreatePipe");
	}
      }
    }
  }

  return true;
}


static IOSTREAM *
Sopen_handle(HANDLE h, const char *mode)
{ IOSTREAM *s = Swin_open_handle(h, mode);

  if ( s->functions == &Sfilefunctions )
    s->functions = &Sprocessfilefunctions;

  return s;
}


static HANDLE
open_null_stream(DWORD access)
{ SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  return CreateFile("nul",
		    access,
		    FILE_SHARE_READ|FILE_SHARE_WRITE,
		    &sa,		/* security */
		    OPEN_EXISTING,
		    0,
		    NULL);
}


static bool
console_app(void)
{ HANDLE h;

  if ( (h = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE )
  { DWORD mode;

    if ( GetConsoleMode(h, &mode) )
      return true;
  }

  return false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Where the child gets its terminal from.

An Epilog window has no console: it talks to its Prolog thread over two
pipes, and a child handed those gets no terminal -- nothing echoes what it
reads, it sees no window size and ^C does not reach it.  A pager therefore
sat waiting for a key from a console nobody types on.  So put the child on
the window's own pseudo console, as System() in src/pl-nt.c does for
shell/1 and terminal_image->launch does for the process it starts.

Only for the streams the caller left alone.  A child that is fully
redirected has no business on the terminal, and holding the console is
what tells the window that its keys are the child's rather than its own.
Nor for a detached child, which outlives the goal that started it, or one
the caller asked to give a window of its own.

The console is claimed here and handed back when the child is reaped:
below for a child we wait for, in wait_for_process() for one whose pipes
we hold, and in unregister_process() for one with a process(PID).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
on_terminal(p_options *info)
{ int i;

  if ( info->detached || info->window == true )
    return false;

  for(i=0; i<3; i++)
  { if ( info->streams[i].type == std_std )
      return true;
  }

  return false;
}


static int
do_create_process(p_options *info)
{ int flags = 0;
  PROCESS_INFORMATION pi;
  STARTUPINFOEXW siEx;
  STARTUPINFOW *si = &siEx.StartupInfo;
  IOSTREAM *console = NULL;		/* terminal we hold the console of */
  void *attrs = NULL;
  int redirected = 0;			/* streams that are not the terminal */

  memset(&siEx, 0, sizeof(siEx));
  si->cb = sizeof(*si);

  if ( on_terminal(info) )
  { HANDLE hpc;

    if ( (hpc=Swinpseudoconsole(Suser_input)) )
    { if ( (attrs=Swinpseudoconsole_attributes(hpc)) )
      { console	           = Suser_input;
	siEx.lpAttributeList = attrs;
	si->cb		     = sizeof(siEx);
	/* Only EXTENDED_STARTUPINFO_PRESENT: CREATE_NO_WINDOW asks for a
	 * console of the child's own, which is the opposite of what the
	 * attribute says and wins.
	 */
	flags		     = EXTENDED_STARTUPINFO_PRESENT;
	Sflush(Suser_output);		/* or it lands after the child's */
	Sflush(Suser_error);
      } else
	Swinrelease_pseudoconsole(Suser_input);
    }
  }

  if ( !console )
  { switch(info->window)
    { case MAYBE:
	if ( !console_app() )
	  flags |= CREATE_NO_WINDOW;
	break;
      case true:
	break;
      case false:
	flags |= CREATE_NO_WINDOW;
	break;
    }
  }

  if ( info->detached )
    flags |= CREATE_BREAKAWAY_FROM_JOB;
  if ( info->envbuf.buffer )
    flags |= CREATE_UNICODE_ENVIRONMENT;

  si->dwFlags = STARTF_USESTDHANDLES;

				      /* stdin */
  switch( info->streams[0].type )
  { case std_stream:
      si->hStdInput = info->streams[0].fd[0];
      SetHandleInformation(si->hStdInput,
			   HANDLE_FLAG_INHERIT, true);
      redirected++;
      break;
    case std_pipe:
      si->hStdInput = info->streams[0].fd[0];
      SetHandleInformation(info->streams[0].fd[1],
			   HANDLE_FLAG_INHERIT, false);
      redirected++;
      break;
    case std_null:
      si->hStdInput = open_null_stream(GENERIC_READ);
      redirected++;
      break;
    case std_std:
      /* NULL leaves it to the pseudo console, which is the one thing
       * STARTF_USESTDHANDLES and a console attribute agree on.
       */
      si->hStdInput = console ? NULL : GetStdHandle(STD_INPUT_HANDLE);
      break;
  }
				      /* stdout */
  switch( info->streams[1].type )
  { case std_stream:
      si->hStdOutput = info->streams[1].fd[1];
      SetHandleInformation(si->hStdOutput,
			   HANDLE_FLAG_INHERIT, true);
      redirected++;
      break;
    case std_pipe:
      si->hStdOutput = info->streams[1].fd[1];
      SetHandleInformation(info->streams[1].fd[0],
			   HANDLE_FLAG_INHERIT, false);
      redirected++;
      break;
    case std_null:
      si->hStdOutput = open_null_stream(GENERIC_WRITE);
      redirected++;
      break;
    case std_std:
      si->hStdOutput = console ? NULL : GetStdHandle(STD_OUTPUT_HANDLE);
      break;
  }
				      /* stderr */
  switch( info->streams[2].type )
  { case std_stream:
      si->hStdError = info->streams[2].fd[1];
      SetHandleInformation(si->hStdError,
			   HANDLE_FLAG_INHERIT, true);
      redirected++;
      break;
    case std_pipe:
      si->hStdError = info->streams[2].fd[1];
      SetHandleInformation(info->streams[2].fd[0],
                           HANDLE_FLAG_INHERIT, false);
      redirected++;
      break;
    case std_null:
      si->hStdError = open_null_stream(GENERIC_WRITE);
      redirected++;
      break;
    case std_std:
      si->hStdError = console ? NULL : GetStdHandle(STD_ERROR_HANDLE);
      break;
  }

  /* Nothing to hand over: let the console hand the child all three, which
   * is the path shell/1 takes and the only one the ConPTY samples show.
   */
  if ( console && !redirected )
    si->dwFlags &= ~STARTF_USESTDHANDLES;

  if ( CreateProcessW(info->exe,
		      info->cmdline,
		      NULL,		/* Process security */
		      NULL,		/* Thread security */
		      true,		/* Inherit handles */
		      flags,		/* Creation flags */
		      info->envbuf.buffer, /* Environment */
		      info->cwd,	/* Directory */
		      si,		/* Startup info */
		      &pi) )		/* Process information */
  { int rc = true;

    Swinfree_pseudoconsole_attributes(attrs);
    HANDLE hJob = (HANDLE)0;
    if ( !info->detached )
    {
      hJob = rootJob;
    }
    CloseHandle(pi.hThread);

    if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic    = PROCESS_MAGIC;
      pc->handle   = pi.hProcess;
      pc->console  = console;		/* handed back in wait_for_process() */
      pc->exe_name = info->exe_name;
      PL_register_atom(pc->exe_name);

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	if ( (s = open_process_pipe(pc, info, 0, 1)) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  CloseHandle(info->streams[0].fd[1]);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	if ( rc && (s = open_process_pipe(pc, info, 1, 0)) )
	  PL_unify_stream(info->streams[1].term, s);
	else
	  CloseHandle(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { CloseHandle(info->streams[2].fd[1]);
	if ( rc && (s = open_process_pipe(pc, info, 2, 0)) )
	  rc = PL_unify_stream(info->streams[2].term, s);
	else
	  CloseHandle(info->streams[2].fd[0]);
      }

      return rc;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	if ( (s = Sopen_handle(info->streams[0].fd[1], "w")) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  CloseHandle(info->streams[0].fd[1]);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	if ( rc && (s = Sopen_handle(info->streams[1].fd[0], "r")) )
	  rc = PL_unify_stream(info->streams[1].term, s);
	else
	  CloseHandle(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { CloseHandle(info->streams[2].fd[1]);
	if ( rc && (s = Sopen_handle(info->streams[2].fd[0], "r")) )
	  rc = PL_unify_stream(info->streams[2].term, s);
	else
	  CloseHandle(info->streams[2].fd[0]);
      }
    }

    if ( !rc )
    { Sdprintf("FATAL ERROR: create_process/3\n");
      PL_halt(1);
    }
    if ( info->detached )
    { hJob = CreateJobObject(NULL, NULL);
      AssignProcessToJobObject(hJob, pi.hProcess);
    }
    else
    { AssignProcessToJobObject(rootJob, pi.hProcess);
    }
    if ( info->pid )
    { /* handed back in unregister_process(), i.e. from process_wait/2 */
      register_process(pi.dwProcessId, pi.hProcess, hJob, console);
      return PL_unify_integer(info->pid, pi.dwProcessId);
    }

    if ( fire_and_forget(info) )	/* let the process go its own way */
    { CloseHandle(pi.hProcess);
      if ( hJob )
	CloseHandle(hJob);
      if ( console )
	Swinrelease_pseudoconsole(console);

      return true;
    }

    rc = win_wait_success(info->exe_name, pi.hProcess);
    if ( console )
      Swinrelease_pseudoconsole(console);

    return rc;
  } else
  { Swinfree_pseudoconsole_attributes(attrs);
    if ( console )
      Swinrelease_pseudoconsole(console);

    return win_error("CreateProcess");
  }
}

#else /*__WINDOWS__*/

#ifndef HAVE_PRCTL
#ifdef _REENTRANT
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK()   pthread_mutex_lock(&mutex)
#define UNLOCK() pthread_mutex_unlock(&mutex)
#else
#define LOCK() {}
#define UNLOCK() {}
#endif

typedef struct posix_process
{ pid_t pid;
  struct posix_process *next;
} posix_process;

static posix_process *processes = NULL;

static void
register_process(pid_t pid)
{ posix_process *pp = PL_malloc(sizeof(*pp));

  pp->pid = pid;
  LOCK();
  pp->next = processes;
  processes = pp;
  UNLOCK();
}

static bool
unregister_process(pid_t pid)
{ posix_process **ppp, *pp;
  LOCK();
  for(ppp=&processes, pp=*ppp; pp; ppp=&pp->next, pp=*ppp)
  { if ( pp->pid == pid )
    { *ppp = pp->next;
      PL_free(pp);
      UNLOCK();
      return true;
    }
  }

  UNLOCK();
  return false;
}

static int
kill_all_processes(int rc, void* arg)
{ posix_process* pp;

  LOCK();
  for(pp=processes; pp; pp=pp->next)
    kill(pp->pid, SIGTERM);
  UNLOCK();

  return 0;
}

static void
posix_init()
{ PL_exit_hook(kill_all_processes, NULL);
}

#else /*HAVE_PRCTL*/
#define unregister_process(pid) ((void)0)
#endif /*HAVE_PRCTL*/

/* Children that made our terminal theirs (see adopt_ctty()) and the
 * PL_save_ctty() state to repair it with once they are gone.  On BSD
 * systems, MacOS among them, the terminal is revoked when such a child
 * exits.  Reaping it is our first chance to notice, and the caller is
 * about to write to that terminal.
 */

typedef struct ctty_child
{ pid_t pid;
  void *ctty;				/* from PL_save_ctty() */
  struct ctty_child *next;
} ctty_child;

static ctty_child *ctty_children = NULL;
#ifdef _REENTRANT
static pthread_mutex_t ctty_mutex = PTHREAD_MUTEX_INITIALIZER;
#define CTTY_LOCK()   pthread_mutex_lock(&ctty_mutex)
#define CTTY_UNLOCK() pthread_mutex_unlock(&ctty_mutex)
#else
#define CTTY_LOCK()   ((void)0)
#define CTTY_UNLOCK() ((void)0)
#endif

static void
register_ctty_child(pid_t pid, void *ctty)
{ ctty_child *cc;

  if ( !ctty )
    return;
  if ( !(cc = PL_malloc(sizeof(*cc))) )
  { PL_restore_ctty(ctty);		/* just releases it */
    return;
  }
  cc->pid  = pid;
  cc->ctty = ctty;
  CTTY_LOCK();
  cc->next = ctty_children;
  ctty_children = cc;
  CTTY_UNLOCK();
}

/* Called after waitpid() reported pid as gone. */

static void
process_reaped(pid_t pid)
{ ctty_child **ccp, *cc;
  void *ctty = NULL;

  unregister_process(pid);

  CTTY_LOCK();
  for(ccp=&ctty_children, cc=*ccp; cc; ccp=&cc->next, cc=*ccp)
  { if ( cc->pid == pid )
    { *ccp = cc->next;
      ctty = cc->ctty;
      PL_free(cc);
      break;
    }
  }
  CTTY_UNLOCK();

  if ( ctty )
    PL_restore_ctty(ctty);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note the descriptors created using pipe()   are  inherited by the child.
This implies that if two threads  call process_create/3 using pipes, the
pipe created for one thread may also  end   up  in  the child created by
another thread. We  can  avoid  this   using  FD_CLOEXEC  on  the pipe's
descriptor. Note that we can do that on   both  because the flag will be
cleared on the duplicated  descriptor  after   dup2  (which  is executed
before the exec, so the descriptors are still valid).

This can be implemented safely on systems   that  have pipe2(). On other
systems, safety can be enhanced by   reducing  the window using fcntl(),
but this needs to be synced with fork() in other threads.

(*) It seems that some systems implement  the function, but the function
returns ENOSYS to indicate it is not   implemented. Hence, we need to go
for a runtime solution.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
create_pipes(p_options *info)
{ int i;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term && s->type == std_pipe )
    { if ( i == 2 && info->streams[1].term &&
	   PL_compare(info->streams[1].term, info->streams[2].term) == 0 )
      { s->fd[0] = info->streams[1].fd[0];
	s->fd[1] = info->streams[1].fd[1];
      } else
      {
#if defined(HAVE_PIPE2) && !defined(__APPLE__) /* weak symbol on macOS */
	if ( pipe2(s->fd, O_CLOEXEC) == 0 )
	{ s->cloexec = true;
	  continue;
	} else if ( errno != ENOSYS )	/* See (*) */
	{ if ( errno != EMFILE )
	    Sdprintf("pipe2(): unexpected error: %s\n", strerror(errno));
	  return PL_resource_error("open_files");
	}
#endif
	if ( pipe(s->fd) )
	{ if ( errno != EMFILE )
	    Sdprintf("pipe(): unexpected error: %s\n", strerror(errno));
	  return PL_resource_error("open_files");
	}
#ifdef F_SETFD				/* both: dup2() clears the flag */
        if ( fcntl(s->fd[0], F_SETFD, FD_CLOEXEC) == 0 &&
	     fcntl(s->fd[1], F_SETFD, FD_CLOEXEC) == 0 )
	  s->cloexec = true;
#endif
      }
    }				/* std_stream: do not modify the user's fd */
  }

  return true;
}


static bool
unify_exit_status(term_t code, int status)
{ if ( WIFEXITED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_INT, (int)WEXITSTATUS(status));
  } else if ( WIFSIGNALED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_killed1,
			   PL_INT, (int)WTERMSIG(status));
  } else
  { assert(0);
    return false;
  }
}


static bool
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ pid_t p2;
  int status;

  if ( opts->has_timeout && opts->timeout == 0.0 )
  { if ( (p2=waitpid(pid, &status, WNOHANG)) == pid )
    { process_reaped(pid);
      return unify_exit_status(code, status);
    }
    else if ( p2 == 0 )
      return PL_unify_atom(code, ATOM_timeout);
    else
    { term_t PID;

    error:
      return ((PID = PL_new_term_ref()) &&
	      PL_put_integer(PID, pid) &&
	      pl_error(NULL, 0, "waitpid", ERR_ERRNO,
		       errno, "wait", "process", PID));
    }
  }

  for(;;)
  { if ( (p2=waitpid(pid, &status, 0)) == pid )
    { process_reaped(pid);
      return unify_exit_status(code, status);
    }

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return false;
    } else
    { goto error;
    }
  }
}


static bool
wait_success(atom_t name, pid_t pid)
{ pid_t p2;

  for(;;)
  { int status;

    if ( (p2=waitpid(pid, &status, 0)) == pid )
    { process_reaped(pid);
      if ( WIFEXITED(status) && WEXITSTATUS(status) == 0 )
      { return true;
      } else
      { term_t code, ex;

	if ( (code = PL_new_term_ref()) &&
	     (ex = PL_new_term_ref()) &&
	     unify_exit_status(code, status) &&
	     PL_unify_term(ex,
			   PL_FUNCTOR, FUNCTOR_error2,
			     PL_FUNCTOR, FUNCTOR_process_error2,
			       PL_ATOM, name,
			       PL_TERM, code,
			     PL_VARIABLE) )
	  return PL_raise_exception(ex);
	return false;
      }
    }

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return false;
    } else
    { term_t PID;

      return ((PID = PL_new_term_ref()) &&
	      PL_put_integer(PID, pid) &&
	      pl_error(NULL, 0, "waitpid", ERR_ERRNO,
		       errno, "wait", "process", PID));
    }
  }
}


static int
wait_for_process(process_context *pc)
{ int rc = wait_success(pc->exe_name, pc->pid);

  PL_unregister_atom(pc->exe_name);
  PL_free(pc);

  return rc;
}


static int
close_ok(int fd)
{ int rc;

  do
  { rc = close(fd);
  } while ( rc == -1 && errno == EINTR );

  DEBUG(if ( rc == -1 )
	  perror("close"));

  return rc;
}

static IOSTREAM *
p_fdopen(p_options *info, int which, int fdn, char *mode)
{ IOSTREAM *s;
  char m[10];
  char *mp = m;

  *mp++ = mode[0];
  if ( info->streams[which].encoding == ENC_OCTET )
    *mp++ = 'b';
  *mp = 0;

  if ( (s=Sfdopen(info->streams[which].fd[fdn], m)) )
    s->encoding = info->streams[which].encoding;

  return s;
}


static bool
process_parent_side(p_options *info, int pid)
{ int rc = true;

  if ( info->pipes > 0 && info->pid == 0 )
  { IOSTREAM *s;			/* no pid(Pid): wait */
    process_context *pc = PL_malloc(sizeof(*pc));
    DEBUG(Sdprintf("Wait on pipes\n"));

    memset(pc, 0, sizeof(*pc));
    pc->magic = PROCESS_MAGIC;
    pc->pid = pid;
    pc->exe_name = info->exe_name;
    PL_register_atom(pc->exe_name);

    if ( info->streams[0].type == std_pipe )
    { close_ok(info->streams[0].fd[0]);
      if ( (s = open_process_pipe(pc, info, 0, 1)) )
	rc = PL_unify_stream(info->streams[0].term, s);
      else
	close_ok(info->streams[0].fd[1]);
    }
    if ( info->streams[1].type == std_pipe )
    { close_ok(info->streams[1].fd[1]);
      if ( rc && (s = open_process_pipe(pc, info, 1, 0)) )
	rc = PL_unify_stream(info->streams[1].term, s);
      else
	close_ok(info->streams[1].fd[0]);
    }
    if ( info->streams[2].type == std_pipe &&
	 ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
    { close_ok(info->streams[2].fd[1]);
      if ( rc && (s = open_process_pipe(pc, info, 2, 0)) )
	rc = PL_unify_stream(info->streams[2].term, s);
      else
	close_ok(info->streams[2].fd[0]);
    }

    return rc;
  } else if ( info->pipes > 0 )
  { IOSTREAM *s;

    if ( info->streams[0].type == std_pipe )
    { close_ok(info->streams[0].fd[0]);
      if ( (s = p_fdopen(info, 0, 1, "w")) )
	rc = PL_unify_stream(info->streams[0].term, s);
      else
	close_ok(info->streams[0].fd[1]);
    }
    if ( info->streams[1].type == std_pipe )
    { close_ok(info->streams[1].fd[1]);
      if ( rc && (s = p_fdopen(info, 1, 0, "r")) )
	rc = PL_unify_stream(info->streams[1].term, s);
      else
	close_ok(info->streams[1].fd[0]);
    }
    if ( info->streams[2].type == std_pipe &&
	 ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
    { close_ok(info->streams[2].fd[1]);
      if ( rc && (s = p_fdopen(info, 2, 0, "r")) )
	PL_unify_stream(info->streams[2].term, s);
      else
	close_ok(info->streams[2].fd[0]);
    }
  }

  assert(rc);				/* What else? */
#ifndef HAVE_PRCTL /* Then we must do this manually */
  if ( !info->detached )
    register_process(pid);
#endif
  if ( info->pid )
    return PL_unify_integer(info->pid, pid);

  return wait_success(info->exe_name, pid);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If profiling is used, fork() may not   work succeed as it is interrupted
before completion. This problem was  diagnosed   by  Edward Schwartz. It
seems to affect at least Ubuntu 18.04, but not later versions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(SIGPROF) && defined(HAVE_SIGPROCMASK)

static void
blockSignal(int sig, sigset_t *old)
{ sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, sig);

  sigprocmask(SIG_BLOCK, &set, old);
}

static void
restoreSignals(sigset_t *old)
{ sigprocmask(SIG_SETMASK, old, NULL);
}

#else
#define blockSignal(sig, old) (void)set
#define restoreSignals(set) (void)0
#endif

#ifndef HAVE_VFORK
#define vfork fork
#endif

		 /*******************************
		 *      CONTROLLING TERMINAL	*
		 *******************************/

/* A thread need not run on the terminal the process was started from:
 * an Epilog window gives its Prolog thread a pty of its own, and
 * process_create/3 hands that pty to the child.  Inheriting the fd is
 * not enough there.  With no session owning that pty it has no
 * foreground process group, so resizing the window raises no SIGWINCH
 * in the child and ^C sends it no SIGINT.
 *
 * The child therefore calls setsid() and TIOCSCTTY when the terminal
 * it is handed belongs to another session.  An ordinary
 * process_create/3 is unaffected: there the terminal already is this
 * session's controlling terminal.  Mirrors adopt_ctty() in
 * src/os/pl-os.c, which does the same for shell/1.
 */

#include <termios.h>			/* tcgetsid() */
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>			/* TIOCSCTTY */
#endif

#if defined(HAVE_SETSID) && defined(HAVE_TCGETSID) && defined(TIOCSCTTY)
#define O_ADOPT_CTTY 1

/* Called in the child, after the dup2()s and before the exec. */

static void
adopt_ctty(void)
{ if ( isatty(0) && tcgetsid(0) != getsid(0) )
  { if ( setsid() != -1 )
      ioctl(0, TIOCSCTTY, 0);
  }
}
#endif

#ifdef O_ADOPT_CTTY

/* The fd that becomes the child's stdin, or -1 if it has none. */

static int
child_stdin_fd(p_options *info)
{ switch( info->streams[0].type )
  { case std_pipe:
    case std_stream:
      return info->streams[0].fd[0];
    case std_std:
      return Sfileno(Suser_input);
    default:
      return -1;
  }
}

/* Must the child adopt its terminal?  Asked in the parent, because
 * posix_spawn() has no hook to do so and we must fork() instead, and
 * because we must then be ready to repair the terminal after the child
 * is gone (see process_reaped()).
 */

static bool
needs_ctty(p_options *info)
{ int fd;

  return ( !info->detached &&
	   (fd=child_stdin_fd(info)) >= 0 &&
	   isatty(fd) &&
	   tcgetsid(fd) != getsid(0) );
}

#else
#define needs_ctty(info) 0
#endif

/* Make fd available as target in the child.  If fd is already target,
   dup2() is a no-op and does not clear the close-on-exec flag.
*/

static void
dup_to(int fd, int target)
{ if ( fd == target )
  {
#ifdef F_SETFD
    fcntl(fd, F_SETFD, 0);
#endif
  } else
  { dup2(fd, target);
  }
}

static int
do_create_process_fork(p_options *info, create_method method)
{ int pid;
  sigset_t set;
  void *ctty = needs_ctty(info) ? PL_save_ctty() : NULL;

  blockSignal(SIGPROF, &set);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  if ( method == PCREATE_VFORK )
    pid = vfork();				/* As long as it is there we use it */
  else
    pid = fork();
  if ( pid != 0 )				/* parent */
    restoreSignals(&set);
#pragma GCC diagnostic pop

  if ( pid == 0 )				/* child */
  { int fd;

    restoreSignals(&set);
    PL_cleanup_fork();

    if ( fire_and_forget(info) )
    { pid_t p2 = fork();			/* fork again and die, such */
						/* that init inherits p2 */
      if ( p2 != 0 )
	_exit(p2 > 0 ? 0 : 1);
    }

#if defined(HAVE_SYS_RESOURCE_H) && defined(PRIO_PROCESS)
    if ( info->priority != 255 )
      setpriority(PRIO_PROCESS, pid, info->priority);
#endif

    if ( info->detached )
    { setsid();
#ifdef HAVE_SETPGID
      setpgid(pid, pid);
#elif defined(HAVE_SETPGRP)
#ifdef SETPGRP_VOID
      setpgrp();
#else
      setpgrp(pid, pid);
#endif
#endif
    } else
    {
#ifdef HAVE_PRCTL
      prctl(PR_SET_PDEATHSIG, SIGTERM);
#endif
    }

    if ( info->cwd )
    { if ( chdir(info->cwd) )
      { perror(info->cwd);
	_exit(1);
      }
    }

					/* stdin */
    switch( info->streams[0].type )
    { case std_pipe:
      case std_stream:
        dup_to(info->streams[0].fd[0], 0);
        if ( info->streams[0].type == std_pipe && !info->streams[0].cloexec )
	  close(info->streams[0].fd[1]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_RDONLY)) >= 0 )
	  dup2(fd, 0);
        break;
      case std_std:
      { int fd = Sfileno(Suser_input);
        if ( fd > 0 )
	  dup2(fd, 0);
	break;
      }
    }
					/* stdout */
    switch( info->streams[1].type )
    { case std_pipe:
      case std_stream:
	dup_to(info->streams[1].fd[1], 1);
        if ( info->streams[1].type == std_pipe && !info->streams[1].cloexec )
	  close(info->streams[1].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 1);
        break;
      case std_std:
      { int fd = Sfileno(Suser_output);
        if ( fd >= 0 && fd != 1 )
	  dup2(fd, 1);
	break;
      }
    }
					/* stderr */
    switch( info->streams[2].type )
    { case std_pipe:
      case std_stream:
	dup_to(info->streams[2].fd[1], 2);
	if ( info->streams[2].type == std_pipe && !info->streams[2].cloexec )
	  close(info->streams[2].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 2);
        break;
      case std_std:
      { int fd = Sfileno(Suser_error);
        if ( fd >= 0 && fd != 2 )
	  dup2(fd, 2);
	break;
      }
    }

    for(int i=0; i<3; i++)		/* close user stream(S) fds */
    { if ( info->streams[i].type == std_stream &&
	   info->streams[i].fd[0] > 2 )
	close(info->streams[i].fd[0]);
    }

#ifdef O_ADOPT_CTTY
    if ( !info->detached )
      adopt_ctty();
#endif

    if ( info->envp )
    { execve(info->exe, info->argv, info->envp);
    } else
    {
#ifdef HAVE__NSGETENVIRON
      char **environ = *_NSGetEnviron();
#else
      extern char **environ;
#endif
      execve(info->exe, info->argv, environ);
    }

    perror(info->exe);
    _exit(1);
  } else if ( pid < 0 )			/* parent */
  { term_t exe = PL_new_term_ref();
    PL_put_atom_chars(exe, info->exe);

    PL_restore_ctty(ctty);		/* just releases it */
    return pl_error(NULL, 0, "fork", ERR_ERRNO, errno, "fork", "process", exe);
  } else
  { register_ctty_child(pid, ctty);
    return process_parent_side(info, pid);
  }
}


#ifdef HAVE_POSIX_SPAWN
#include <spawn.h>

/* Can we use posix_spawn()?  Anything that needs code to run in the
   child (changing directory, priority, controlling terminal, the Linux
   parent death signal) requires fork().  If a pipe descriptor is
   already at its target, dup2() does not clear close-on-exec, so we
   also need fork().
*/

static bool
can_spawn(p_options *info)
{ if ( info->cwd || needs_ctty(info) || info->priority != 255 ||
       create_process_method != PCREATE_SPAWN )
    return false;
#ifndef POSIX_SPAWN_SETSID
  if ( info->detached )
    return false;
#endif
#ifdef HAVE_PRCTL
  if ( !info->detached )		/* needs PR_SET_PDEATHSIG */
    return false;
#endif

  for(int i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->type == std_pipe && s->fd[i==0 ? 0 : 1] == i )
      return false;
  }

  return true;
}


static int
spawn_std_stream(posix_spawn_file_actions_t *fa, p_options *info, int i)
{ p_stream *s = &info->streams[i];
  int rc = 0;

  switch( s->type )
  { case std_pipe:
    case std_stream:
    { int fd    = (i == 0 ? s->fd[0] : s->fd[1]);
      int other = (i == 0 ? s->fd[1] : s->fd[0]);

      if ( fd != i )
	rc = posix_spawn_file_actions_adddup2(fa, fd, i);
      if ( rc == 0 && s->type == std_pipe && !s->cloexec )
	rc = posix_spawn_file_actions_addclose(fa, other);
      break;
    }
    case std_null:
      rc = posix_spawn_file_actions_addopen(fa, i, "/dev/null",
					    i == 0 ? O_RDONLY : O_WRONLY, 0);
      break;
    case std_std:
    { IOSTREAM *us = (i == 0 ? Suser_input : i == 1 ? Suser_output
					      : Suser_error);
      int fd = Sfileno(us);

      if ( fd >= 0 && fd != i )
	rc = posix_spawn_file_actions_adddup2(fa, fd, i);
      break;
    }
  }

  return rc;
}


static int
do_create_process(p_options *info)
{ pid_t pid;
  posix_spawn_file_actions_t file_actions;
  posix_spawnattr_t attr;
  char **envp = info->envp;
  int rc;

  if ( fire_and_forget(info) )		/* needs the double fork */
    return do_create_process_fork(info, PCREATE_FORK);
  if ( !can_spawn(info) )
    return do_create_process_fork(info, create_process_method);

  if ( (rc=posix_spawn_file_actions_init(&file_actions)) != 0 )
    goto error;
  if ( (rc=posix_spawnattr_init(&attr)) != 0 )
  { posix_spawn_file_actions_destroy(&file_actions);
    goto error;
  }

#ifdef POSIX_SPAWN_SETSID
  if ( info->detached )
    rc = posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSID);
#endif

  for(int i=0; rc == 0 && i<3; i++)
    rc = spawn_std_stream(&file_actions, info, i);

  for(int i=0; rc == 0 && i<3; i++)	/* close user stream(S) fds */
  { int fd = info->streams[i].fd[0];

    if ( info->streams[i].type == std_stream && fd > 2 )
    { int done = false;

      for(int j=0; j<i; j++)
      { if ( info->streams[j].type == std_stream &&
	     info->streams[j].fd[0] == fd )
	  done = true;
      }
      if ( !done )
	rc = posix_spawn_file_actions_addclose(&file_actions, fd);
    }
  }

  if ( !envp )
  {
#ifdef HAVE__NSGETENVIRON
    envp = *_NSGetEnviron();
#else
    extern char **environ;
    envp = environ;
#endif
  }

  if ( rc == 0 )
    rc = posix_spawn(&pid, info->exe, &file_actions, &attr, info->argv, envp);

  posix_spawn_file_actions_destroy(&file_actions);
  posix_spawnattr_destroy(&attr);

  if ( rc == 0 )
    return process_parent_side(info, pid);

error:
  { term_t exe = PL_new_term_ref();
    PL_put_atom_chars(exe, info->exe);

    return pl_error(NULL, 0, "spawn", ERR_ERRNO, rc, "fork", "process", exe);
  }
}

#else /*HAVE_POSIX_SPAWN*/

static int
do_create_process(p_options *info)
{ if ( fire_and_forget(info) )		/* needs the double fork */
    return do_create_process_fork(info, PCREATE_FORK);

  return do_create_process_fork(info, create_process_method);
}

#endif /*HAVE_POSIX_SPAWN*/

#endif /*__WINDOWS__*/


		 /*******************************
		 *	      BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Running out of resources after we created  the process is really hard to
handle gracefully, so we first make  a   list  to  ensure we have enough
resources to bind the return value.

Ideally, we need a call to ask for sufficient resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
ensure_stack_resources(int count)
{ fid_t fid = PL_open_foreign_frame();
  term_t list = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(list);

  while ( count-- > 0 )
  { term_t head;

    if ( !(head = PL_new_term_ref()) ||
	 !PL_unify_list(tail, head, tail) )
    { PL_close_foreign_frame(fid);
      return false;
    }
  }

  PL_discard_foreign_frame(fid);
  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Basic process creation interface takes

	* Exe file
	* List of arguments
	* standard streams		% std, null, pipe(S)
	* Working directory
	* detached
	* window			% Windows
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
process_create(term_t exe, term_t options)
{ p_options info;
  int rc = false;

  if ( !ensure_stack_resources(10) )	/* max 3 stream structures */
    return false;

  memset(&info, 0, sizeof(info));

  info.priority = 255;			/* zero is a valid priority */

  if ( !get_exe(exe, &info) )
    goto out;
  if ( !parse_options(options, &info) )
    goto out;
  if ( !create_pipes(&info) )
    goto out;

  rc = do_create_process(&info);

out:
  free_options(&info);

  return rc;
}


static bool
get_pid(term_t pid, pid_t *p)
{ int n;

  if ( !PL_get_integer_ex(pid, &n) )
    return false;
  if ( n <= 0 )				/* 0 and -1 are process groups */
    return PL_domain_error("not_less_than_one", pid),false;

  *p = n;
  return true;
}


static PL_option_t process_wait_options[] =
{ PL_OPTION("timeout", OPT_TERM),
  PL_OPTION("release", OPT_TERM),
  PL_OPTIONS_END
};

static foreign_t
process_wait(term_t pid, term_t code, term_t options)
{ pid_t p;
  wait_options opts;
  term_t timeout = 0, release = 0;

  if ( !get_pid(pid, &p) )
    return false;

  memset(&opts, 0, sizeof(opts));
  if ( !PL_scan_options(options, OPT_UNKNOWN_ERROR, "process_wait_option",
			process_wait_options, &timeout, &release) )
    return false;
  if ( timeout )
  { atom_t a;

    if ( !(PL_get_atom(timeout, &a) && a == ATOM_infinite) )
    { if ( !PL_get_float(timeout, &opts.timeout) )
	return PL_type_error("timeout", timeout);
      opts.has_timeout = true;
    }
  }
  if ( release )
  { if ( !PL_get_bool_ex(release, &opts.release) )
      return false;
    if ( opts.release == false )
      return PL_domain_error("true", release);
  }

  return wait_for_pid(p, code, &opts);
}

#ifndef __WINDOWS__
static foreign_t
process_kill_posix(term_t pid_term, pid_t pid, int sig)
{ if ( kill(pid, sig) == 0 )
    return true;

  switch(errno)
  { case EPERM:
      return pl_error("process_kill", 2, NULL, ERR_PERMISSION,
		      pid_term, "kill", "process");
    case ESRCH:
      return pl_error("process_kill", 2, NULL, ERR_EXISTENCE,
		      "process", pid_term);
    default:
      return pl_error("process_kill", 2, "kill", ERR_ERRNO, errno,
		      "kill", "process", pid_term);
  }
}
#endif

static foreign_t
process_kill(term_t pid, term_t signal)
{ pid_t p;

  if ( !get_pid(pid, &p) )
    return false;

{
#ifdef __WINDOWS__
  HANDLE h;

  if ( !(h=find_process_from_pid(p, "process_kill")) )
    return false;

  if ( TerminateProcess(h, 255) )
    return true;

  return win_error("TerminateProcess");
#else /*__WINDOWS__*/
  int sig;

  if ( !PL_get_signum_ex(signal, &sig) )
    return false;

  return process_kill_posix(pid, p, sig);
#endif /*__WINDOWS__*/
}
}

static foreign_t
process_group_kill(term_t pid, term_t signal)
{ pid_t p;

  if ( !get_pid(pid, &p) )
    return false;

{
#ifdef __WINDOWS__
  HANDLE hJob;
  hJob = find_job_from_pid(p, "process_group_kill");
  TerminateJobObject(hJob, 255);
  /* For consistency, if you try to kill the process group of the
     root process then exit. Windows does not do this for us.
  */
  if (hJob == rootJob)
    exit(255);
  return true;
#else /*__WINDOWS__*/
   int sig;
   if ( !PL_get_signum_ex(signal, &sig) )
     return false;
   return process_kill_posix(pid, -p, sig);
#endif /*__WINDOWS__*/
 }
}


static foreign_t
process_set_method(term_t how)
{ char *s;

  if ( PL_get_chars(how, &s, CVT_ATOM|CVT_EXCEPTION) )
  { create_method m;

    if ( strcmp(s, "fork") == 0 )
      m = PCREATE_FORK;
    else if ( strcmp(s, "vfork") == 0 )
      m = PCREATE_VFORK;
    else if ( strcmp(s, "spawn") == 0 )
      m = PCREATE_SPAWN;
    else
      return PL_domain_error("process_create_method", how);

#ifndef __WINDOWS__			/* Windows has only CreateProcess() */
    create_process_method = m;
#else
    (void)m;
#endif
    return true;
  }

  return false;
}

#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_process()
{
#ifdef __WINDOWS__
  win_init();
#elif !defined(HAVE_PRCTL)
  posix_init();
#endif

  MKATOM(stdin);
  MKATOM(stdout);
  MKATOM(stderr);
  MKATOM(std);
  MKATOM(null);
  MKATOM(timeout);
  MKATOM(infinite);
  MKATOM(text);
  MKATOM(binary);

  MKFUNCTOR(pipe, 1);
  MKFUNCTOR(pipe, 2);
  MKFUNCTOR(type, 1);
  MKFUNCTOR(encoding, 1);
  MKFUNCTOR(stream, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(process_error, 2);
  MKFUNCTOR(system_error, 2);
  MKFUNCTOR(exit, 1);
#ifndef __WINDOWS__
  MKFUNCTOR(killed, 1);
#endif

  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("process_create",	    2, process_create,	   0);
  PL_register_foreign("process_wait",	    3, process_wait,	   0);
  PL_register_foreign("process_kill",	    2, process_kill,	   0);
  PL_register_foreign("process_group_kill", 2, process_group_kill, 0);
  PL_register_foreign("process_set_method", 1, process_set_method, 0);
}
