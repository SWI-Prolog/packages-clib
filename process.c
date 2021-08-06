/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2021, University of Amsterdam
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

/*#define O_DEBUG 1*/
#undef O_DEBUG
#define _GNU_SOURCE			/* get pipe2() */
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
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
static atom_t ATOM_extra_streams;
static atom_t ATOM_std;
static atom_t ATOM_null;
static atom_t ATOM_process;
static atom_t ATOM_detached;
static atom_t ATOM_cwd;
static atom_t ATOM_env;
static atom_t ATOM_environment;
static atom_t ATOM_priority;
static atom_t ATOM_window;
static atom_t ATOM_timeout;
static atom_t ATOM_release;
static atom_t ATOM_infinite;
static atom_t ATOM_text;
static atom_t ATOM_binary;
static atom_t ATOM_octet;
static atom_t ATOM_utf8;
static atom_t ATOM_ascii;
static atom_t ATOM_iso_latin_1;
static atom_t ATOM_unicode_be;
static atom_t ATOM_unicode_le;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_process_error2;
static functor_t FUNCTOR_system_error2;
static functor_t FUNCTOR_pipe1;
static functor_t FUNCTOR_pipe2;
static functor_t FUNCTOR_stream1;
static functor_t FUNCTOR_from_child1;
static functor_t FUNCTOR_to_child1;
static functor_t FUNCTOR_exit1;
static functor_t FUNCTOR_killed1;
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

#if !defined(__MINGW32__)
typedef DWORD  pid_t;
#endif

typedef wchar_t echar;			/* environment character */

#ifndef CREATE_BREAKAWAY_FROM_JOB
#define CREATE_BREAKAWAY_FROM_JOB 0x1000000
#endif

#else
typedef char echar;
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
  std_stream,
  std_unbound,
} std_type;


typedef struct p_stream
{ term_t   term;			/* P in pipe(P) */
  std_type type;			/* type of stream */
  IOENC	   encoding;			/* Encoding for the stream */
  int      mode;			/* SIO_INPUT/SIO_OUTPUT, from parent's view */
#ifdef __WINDOWS__
  HANDLE   fd[2];			/* pipe handles */
#else
  int      fd[2];			/* pipe handles */
#endif
  int	   cloexec;			/* close on exec activated */
} p_stream;

# define PARENT_OPEN_FLAGS(mode) ((mode & SIO_INPUT) ? O_RDONLY : O_WRONLY)
# define CHILD_OPEN_FLAGS(mode) ((mode & SIO_OUTPUT) ? O_RDONLY : O_WRONLY)

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
  p_stream *extra_streams;		/* stream definitions beyond [0,1,2] */
  int   extra_stream_count;		/* #extra streams past 2 */
  int   detached;			/* create as detached */
  int   window;				/* Show a window? */
  int   priority;			/* Process priority */
} p_options;

#define INFO_STREAM(info, fd) ((fd <= 2) ? &(info->streams[fd]) : &(info->extra_streams[fd - 3]))
#define FOREACH_STREAM(info, fdvar, streamvar) \
  for (p_stream *streamvar = info->streams, *__loopctr = NULL; __loopctr == NULL; __loopctr++) \
    for (int fdvar = 0; fdvar < 3 + info->extra_stream_count; fdvar++, streamvar = (fdvar == 3 ? info->extra_streams : streamvar+1))
typedef struct wait_options
{ double timeout;
  int	 has_timeout;
  int	 release;
} wait_options;


typedef enum create_method
{ PCREATE_SPAWN,
  PCREATE_VFORK,
  PCREATE_FORK
} create_method;

static create_method create_process_method = PCREATE_SPAWN;

#ifdef __WINDOWS__
static int win_command_line(term_t t, int arity,
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


static int
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

  return TRUE;
}

		 /*******************************
		 *	ENVIRONMENT PARSING	*
		 *******************************/

static int
get_echars_arg_ex(int i, term_t from, term_t arg, echar **sp, size_t *lenp)
{ const echar *s, *e;

  if ( !PL_get_arg(i, from, arg) )
    return FALSE;

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, lenp, sp,
		      CVT_ATOMIC|CVT_EXCEPTION) )
#else
  if ( !PL_get_nchars(arg, lenp, sp,
		      CVT_ATOMIC|CVT_EXCEPTION|REP_FN) )
#endif
    return FALSE;

  for(s = *sp, e = s+*lenp; s<e; s++)
  { if ( !*s )
      return PL_domain_error("text_non_zero_code", arg);
  }

  return TRUE;
}

#ifdef __WINDOWS__
#define ECHARS(s) L##s
#else
#define ECHARS(s) s
#endif

#ifndef __WINDOWS__
static int
already_in_env(const char *env, int count, const char *e)
{ for(; count-- > 0; env += strlen(env)+1)
  { const char *s, *q;

    for(s=env, q=e; *s && *q && *s == *q && *s != '=' && *q != '='; s++,q++)
      ;
    if (*s == *q && *s == '=' )
      return TRUE;
  }

  return FALSE;
}
#endif

static int
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

  assert(eb->size == 0);
  assert(eb->allocated == 0);
  assert(eb->buffer == NULL);

  while( PL_get_list(tail, head, tail) )
  { echar *s;
    size_t len;

    if ( !PL_is_functor(head, FUNCTOR_eq2) )
      return PL_type_error("environment_variable", head);

    if ( !get_echars_arg_ex(1, head, tmp, &s, &len) )
      return FALSE;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("="), 1);
    if ( !get_echars_arg_ex(2, head, tmp, &s, &len) )
      return FALSE;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("\0"), 1);

    count++;
  }

  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( pass && count == 0 )
    return TRUE;			/* environment([]) is a no-op */

#ifndef __WINDOWS__
  if ( pass )
  {
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
  }
#endif /*__WINDOWS__*/

#ifdef __WINDOWS__
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

  return TRUE;
}


static int
get_type(term_t head, IOENC *enc)
{ atom_t a;

  if ( PL_get_atom_ex(head, &a) )
  { if ( a == ATOM_text )
      *enc = ENC_ANSI;
    else if ( a == ATOM_binary )
      *enc = ENC_OCTET;
    else
      return PL_domain_error("stream_type", head);

    return TRUE;
  }

  return FALSE;
}

/* TBD: provide a public API for translating encoding names to
 * the IOENC enum
 */

static int
get_encoding(term_t head, IOENC *enc)
{ atom_t a;

  if ( PL_get_atom_ex(head, &a) )
  { if ( a == ATOM_octet )
      *enc = ENC_OCTET;
    else if ( a == ATOM_ascii )
      *enc = ENC_ASCII;
    else if ( a == ATOM_iso_latin_1 )
      *enc = ENC_ISO_LATIN_1;
    else if ( a == ATOM_text )
      *enc = ENC_ANSI;
    else if ( a == ATOM_utf8 )
      *enc = ENC_UTF8;
    else if ( a == ATOM_unicode_be )
      *enc = ENC_UNICODE_BE;
    else if ( a == ATOM_unicode_le )
      *enc = ENC_UNICODE_LE;
    else
      return PL_domain_error("encoding", head);

    return TRUE;
  }

  return FALSE;
}


static p_stream *
find_matching_stream(p_options *info, p_stream *stream)
{ FOREACH_STREAM(info, fd2, stream2)
  { if ( stream2 == stream || fd2 >= info->extra_stream_count + 3 )
      break;
    if ( stream2->mode == stream->mode && stream2->term && PL_compare(stream->term, stream2->term) == 0 )
    { return stream2;
      break;
    }
  }
  return NULL;
}


static int
get_stream(term_t t, p_options *info, p_stream *stream, atom_t name)
{ atom_t a;
  IOSTREAM *s = NULL;

  assert(stream->mode == 0);
  assert(stream->term == 0);
  if ( PL_is_functor(t, FUNCTOR_from_child1) )
    stream->mode = SIO_INPUT;	/* we are reading, child is writing */
  else if ( PL_is_functor(t, FUNCTOR_to_child1) )
    stream->mode = SIO_OUTPUT;	/* we are writing, child is reading  */

  if ( stream->mode )
  { _PL_get_arg(1, t, t);
    /* Parse the argument of read/1 or write/1 as a standard stream item */
  } else
  { /* determine input/output from stream name */
    if ( name == ATOM_stdin )
      stream->mode = SIO_OUTPUT; /* parent writes to child's stdin */
    else if ( name == ATOM_stdout || name == ATOM_stderr )
      stream->mode = SIO_INPUT; /* parent reads from child's stdout/stderr */
    else if ( PL_is_functor(t, FUNCTOR_stream1) )
    { /* Determine mode from provided stream */
      stream->term = PL_new_term_ref();
      _PL_get_arg(1, t, stream->term);
      if ( !PL_get_stream_handle(stream->term, &s) )
        return FALSE; /* invalid stream OR stream pair (can't autodetect direction) */
      if ( s->flags & SIO_INPUT )
        stream->mode = SIO_INPUT;
      else
        stream->mode = SIO_OUTPUT;
    }
    else
      return PL_type_error("explicit_rw_specification", t);
  }

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { stream->type = std_null;
      return TRUE;
    } else if ( a == ATOM_std )
    { stream->type = std_std;
      return TRUE;
    } else
    { return PL_domain_error("process_stream", t);
    }
  } else if ( PL_is_functor(t, FUNCTOR_pipe1) ||
	      PL_is_functor(t, FUNCTOR_pipe2) )
  { stream->term = PL_new_term_ref();
    stream->encoding = ENC_ANSI;
    _PL_get_arg(1, t, stream->term);
    if ( !PL_is_variable(stream->term) && !find_matching_stream(info, stream) )
      return PL_uninstantiation_error(stream->term);
    if ( PL_is_functor(t, FUNCTOR_pipe2) )
    { term_t tail = PL_new_term_ref();
      term_t head = PL_new_term_ref();

      _PL_get_arg(2, t, tail);
      while(PL_get_list_ex(tail, head, tail))
      { if ( PL_is_functor(head, FUNCTOR_type1) )
	{ _PL_get_arg(1, head, head);
	  if ( !get_type(head, &stream->encoding) )
	    return FALSE;
	} else if ( PL_is_functor(head, FUNCTOR_encoding1) )
	{ _PL_get_arg(1, head, head);
	  if ( !get_encoding(head, &stream->encoding) )
	    return FALSE;
	} else
	  return PL_domain_error("pipe_option", head);
      }
      if ( !PL_get_nil_ex(tail) )
	return FALSE;
    }
    stream->type = std_pipe;
    info->pipes++;
    return TRUE;
  } else if ( PL_is_functor(t, FUNCTOR_stream1) )
  { int fd;
    if ( !stream->term ) /* could have been fetched above */
    { stream->term = PL_new_term_ref();
      _PL_get_arg(1, t, stream->term);
      if ( !PL_get_stream(stream->term, &s, stream->mode) )
        return FALSE;
    }
    stream->type = std_stream;
    if ( (fd = Sfileno(s)) >= 0 )
    {
#ifdef __WINDOWS__
      stream->fd[0] = stream->fd[1] = (HANDLE)_get_osfhandle(fd);
#else
      stream->fd[0] = stream->fd[1] = fd;
#endif
    } else
    { return PL_domain_error("file_stream", stream->term);
    }
    return TRUE;
  } else
    return PL_type_error("process_stream", t);
}


static int
get_extra_streams(term_t t, p_options *info, int *count, p_stream **streams)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  int allocated = 0;

  assert(*count == 0);
  assert(*streams == NULL);

  allocated = 8;
  *streams = PL_malloc(sizeof(p_stream) * allocated);

  while( PL_get_list(tail, head, tail) )
  { if ( *count >= allocated )
    { allocated <<= 1;
      *streams = PL_realloc(*streams, sizeof(p_stream) * allocated);
    }
    memset(&(*streams)[*count], 0, sizeof(p_stream));
    if ( PL_is_variable(head) )
      (*streams)[*count].type = std_unbound;
    else if ( !get_stream(head, info, &(*streams)[*count], ATOM_extra_streams) )
      return FALSE;
    (*count)++;
  }

  if ( *count < allocated )
  { *streams = PL_realloc(*streams, sizeof(p_stream) * *count);
  }

  return TRUE;
}


static int
parse_options(term_t options, p_options *info)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();

  info->window = MAYBE;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_stdin )
    { if ( !get_stream(arg, info, &info->streams[0], name) )
	return FALSE;
    } else if ( name == ATOM_stdout )
    { if ( !get_stream(arg, info, &info->streams[1], name) )
	return FALSE;
    } else if ( name == ATOM_stderr )
    { if ( !get_stream(arg, info, &info->streams[2], name) )
	return FALSE;
#ifndef __WINDOWS__
    } else if ( name == ATOM_extra_streams )
    { if ( !get_extra_streams(arg, info, &info->extra_stream_count, &info->extra_streams) )
	return FALSE;
#endif
    } else if ( name == ATOM_process )
    { info->pid = PL_copy_term_ref(arg);
    } else if ( name == ATOM_detached )
    { if ( !PL_get_bool_ex(arg, &info->detached) )
	return FALSE;
    } else if ( name == ATOM_cwd )
    {
#ifdef __WINDOWS__
      if ( !PL_get_wchars(arg, NULL, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;
#else
      if ( !PL_get_chars(arg, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
#endif
    } else if ( name == ATOM_window )
    { if ( !PL_get_bool_ex(arg, &info->window) )
	return FALSE;
    } else if ( name == ATOM_env )
    { if ( !parse_environment(arg, info, FALSE) )
	return FALSE;
    } else if ( name == ATOM_environment )
    { if ( !parse_environment(arg, info, TRUE) )
	return FALSE;
    } else if ( name == ATOM_priority )
    { int tmp;

      if ( !PL_get_integer_ex(arg, &tmp) )
	return FALSE;
      if ( tmp < -20 || tmp > 19 )
	return PL_domain_error("priority_option", arg);

      info->priority = tmp;
    } else
      return PL_domain_error("process_option", head);
  }

  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  return TRUE;
}


static int
get_exe(term_t exe, p_options *info)
{ size_t arity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_name_arity(exe, &info->exe_name, &arity) )
    return PL_type_error("callable", exe);

  PL_put_atom(arg, info->exe_name);

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, NULL, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC) )
    return FALSE;
  if ( !win_command_line(exe, arity, info->exe, &info->cmdline) )
    return FALSE;
#else /*__WINDOWS__*/
  if ( !PL_get_chars(arg, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
    return FALSE;

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
	return FALSE;
    }
    info->argv[i] = NULL;
  }
#endif /*__WINDOWS__*/

  return TRUE;
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
  if ( info->extra_streams )
  { PL_free(info->extra_streams);
    info->extra_streams = NULL;
    info->extra_stream_count = 0;
  }
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
#define	PROCESSFD_MAGIC	0x29498002

struct process_context;
typedef struct process_extrafd
{ int magic;				/* PROCESSFD_MAGIC */
  int fd;				/* extension of pipes[] array */
  int fdnum;				/* effective index into extended pipes[] array */
} process_extrafd;

typedef struct process_context
{ int	magic;				/* PROCESS_MAGIC */
#ifdef __WINDOWS__
  HANDLE handle;			/* process handle */
#else
  pid_t	pid;				/* the process id */
#endif
  int   open_count;			/* Open streams */
  int   pipes[3];			/* stdin/stdout/stderr */
  atom_t exe_name;			/* exe as atom */
  int   extra_count;			/* number of entries in extra_pipes[] array */
  process_extrafd extra_pipes[];	/* extension of pipes[] for more than three fds */
} process_context;

#define PROCESS_CONTEXT_SIZE(extra_count) (sizeof(process_context) + (sizeof(process_extrafd) * extra_count))
#define PROCESS_CONTEXT_FROM_EXTRAFD(efd) \
	(process_context*)(((void*)efd) - \
			   (sizeof(process_extrafd) * (efd->fdnum - 3)) - \
			   offsetof(process_context, extra_pipes))

static int wait_for_process(process_context *pc);

static process_context*
create_process_context(p_options *info, intptr_t pid_handle)
{ process_context *pc;

  pc = PL_malloc(PROCESS_CONTEXT_SIZE(info->extra_stream_count));
  memset(pc, 0, PROCESS_CONTEXT_SIZE(info->extra_stream_count));
  *pc = (process_context)
  	{ .magic = PROCESS_MAGIC,
#ifdef __WINDOWS__
	  .handle = (HANDLE)pid_handle,
#else
	  .pid = (int)pid_handle,
#endif
	  .exe_name = info->exe_name,
	  .pipes = {-1, -1, -1},
	  .extra_count = info->extra_stream_count,
	};
  PL_register_atom(pc->exe_name);
  return pc;
}


static int
process_fd_ex(void *handle, process_context **PC, int **FD)
{ process_context *pc = (process_context*) ((uintptr_t)handle&~(uintptr_t)0x3);
  process_extrafd *efd = (process_extrafd *)pc;
  int pipe = (int)(uintptr_t)handle & 0x3;

  if ( pc->magic == PROCESS_MAGIC )
  { if ( PC )
      *PC = pc;
    if ( FD )
      *FD = &pc->pipes[pipe];
    return pipe;
  } else if ( efd->magic == PROCESSFD_MAGIC )
  { if ( PC )
      *PC = PROCESS_CONTEXT_FROM_EXTRAFD(efd);
    if ( FD )
      *FD = &efd->fd;
    return efd->fdnum;
  }

  return -1;
}

static int
process_fd(void *handle, process_context **PC)
{ int *FD, rval;
  rval = process_fd_ex(handle, PC, &FD);
  return rval >= 0 ? *FD : -1;
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
  int *FD;
  int which = process_fd_ex(handle, &pc, &FD);
  int fd = which >= 0 ? *FD : -1;

  if ( fd >= 0 )
  { int rc;

    assert(pc->open_count > 0);

    rc = (*Sfilefunctions.close)((void*)(uintptr_t)fd);
    pc->open_count--;
    *FD = -1; /* Make sure no one tries to decrement open_count again */

    DEBUG(Sdprintf("Closing fd[%d] (%d); count = %d\n", which, fd, pc->open_count));

    if ( !pc->open_count )
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
  p_stream *stream = INFO_STREAM(info, which);
#ifdef __WINDOWS__
  HANDLE fd = stream->fd[fdn];
#else
  int fd = stream->fd[fdn];
#endif
  int flags = SIO_RECORDPOS|SIO_FBUF;
  IOSTREAM *s;
  int *FD;

  if ( which < 3 )
  { FD = &pc->pipes[which];
    handle = (void *)((uintptr_t)pc | (uintptr_t)which);
  } else
  { assert(which - 3 < pc->extra_count);
    process_extrafd *efd = &pc->extra_pipes[which - 3];
    if ( efd->magic != PROCESSFD_MAGIC )
    { *efd = (process_extrafd){.magic = PROCESSFD_MAGIC, .fdnum = which, .fd = -1};
    }
    FD = &efd->fd;
    handle = efd;
  }
  assert(*FD == -1);
  pc->open_count++;
#ifdef __WINDOWS__
  *FD = _open_osfhandle((intptr_t)fd, _O_BINARY);
#else
  *FD = fd;
#endif

  if ( stream->encoding != ENC_OCTET )
    flags |= SIO_TEXT;

  flags |= stream->mode;

  if ( (s=Snew(handle, flags, &Sprocessfunctions)) )
    s->encoding = stream->encoding;

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


typedef struct arg_string
{ size_t  len;
  wchar_t *text;
  wchar_t quote;
} arg_string;

#define QMISC	0x1
#define QDBLQ	0x2
#define QSBLQ	0x4

static int
set_quote(arg_string *as)
{ int needq = 0;
  const wchar_t *s = as->text;

  for(; *s; s++)
  { if ( !iswalnum(*s) )
    { if ( *s == '"' )
	needq |= QDBLQ;
      else if ( *s == '\'' )
	needq |= QSBLQ;
      else
	needq |= QMISC;
    }
  }

  if ( !needq )
  { as->quote = 0;
    return TRUE;
  }
  needq &= ~QMISC;
  switch( needq )
  { case QDBLQ:
      as->quote = '\'';
      return TRUE;
    case 0:
    case QSBLQ:
      as->quote = '"';
      return TRUE;
    default:
      return FALSE;
  }
}


static int
win_command_line(term_t t, int arity, const wchar_t *exe, wchar_t **cline)
{ if ( arity > 0 )
  { arg_string *av = PL_malloc((arity+1)*sizeof(*av));
    term_t arg = PL_new_term_ref();
    size_t cmdlen;
    wchar_t *cmdline, *o;
    const wchar_t *b;
    int i;

    if ( (b=wcsrchr(exe, '\\')) )
      b++;
    else
      b = exe;
    av[0].text = (wchar_t*)b;
    av[0].len = wcslen(av[0].text);
    set_quote(&av[0]);
    cmdlen = av[0].len+(av[0].quote?2:0)+1;

    for( i=1; i<=arity; i++)
    { _PL_get_arg(i, t, arg);

      if ( !PL_get_wchars(arg, &av[i].len, &av[i].text,
			  CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;

      if ( wcslen(av[i].text) != av[i].len )
	return PL_domain_error("no_zero_code_atom", arg);

      if ( !set_quote(&av[i]) )
	return PL_domain_error("dos_quotable_atom", arg);

      cmdlen += av[i].len+(av[i].quote?2:0)+1;
    }

    cmdline = PL_malloc(cmdlen*sizeof(wchar_t));
    for( o=cmdline,i=0; i<=arity; )
    { wchar_t *s = av[i].text;

      if ( av[i].quote )
	*o++ = av[i].quote;
      wcsncpy(o, s, av[i].len);
      o += av[i].len;
      if ( i > 0 )
	PL_free(s);			/* do not free shared exename */
      if ( av[i].quote )
	*o++ = av[i].quote;

      if (++i <= arity)
	*o++ = ' ';
    }
    *o = 0;
    PL_free(av);

    *cline = cmdline;
  } else
  { *cline = NULL;
  }

  return TRUE;
}

typedef struct win_process
{ DWORD pid;
  HANDLE handle;
  HANDLE job;
  struct win_process *next;
} win_process;


static win_process *processes;

static void
register_process(DWORD pid, HANDLE h, HANDLE job)
{ win_process *wp = PL_malloc(sizeof(*wp));

  wp->pid = pid;
  wp->handle = h;
  wp->job = job;
  LOCK();
  wp->next = processes;
  processes = wp;
  UNLOCK();
}


static int
unregister_process(DWORD pid)
{ win_process **wpp, *wp;

  LOCK();
  for(wpp=&processes, wp=*wpp; wp; wpp=&wp->next, wp=*wpp)
  { if ( wp->pid == pid )
    { *wpp = wp->next;
      PL_free(wp);
      UNLOCK();
      return TRUE;
    }
  }

  UNLOCK();
  return FALSE;
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
				 FALSE,	/* return on any event */
				 timeout,
				 QS_ALLINPUT);

  switch(wc)
  { case WAIT_OBJECT_0:
      if ( !GetExitCodeProcess(process, rc) )
      { win_error("GetExitCodeProcess");
	CloseHandle(process);
	return FALSE;
      }
      CloseHandle(process);
      return TRUE;
    case WAIT_OBJECT_0+1:
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
	if ( PL_handle_signals() < 0 )
	  return FALSE;
      }
      goto retry;
    }
    case WAIT_TIMEOUT:
      return WP_TIMEOUT;
    default:
      win_error("WaitForSingleObject");
      CloseHandle(process);
      return FALSE;
  }
}


static int
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
      return FALSE;
    if ( wc == WP_TIMEOUT )
      return PL_unify_atom(code, ATOM_timeout);

    unregister_process(pid);

    return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_LONG, rc);
  } else
  { return FALSE;
  }
}


static int
win_wait_success(atom_t exe, HANDLE process)
{ ULONG rc;

  if ( !wait_process_handle(process, &rc, INFINITE) )
    return FALSE;

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
    return FALSE;
  }

  return TRUE;
}


static int
wait_for_process(process_context *pc)
{ int rc = win_wait_success(pc->exe_name, pc->handle);

  PL_unregister_atom(pc->exe_name);
  PL_free(pc);

  return rc;
}


static int
create_pipes(p_options *info)
{ SECURITY_ATTRIBUTES sa;

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

  return TRUE;
}


static IOSTREAM *
Sopen_handle(HANDLE h, const char *mode)
{ IOSTREAM *s = Sfdopen(_open_osfhandle((intptr_t)h, _O_BINARY), mode);
  if ( s->functions == &Sfilefunctions )
  { s->functions = &Sprocessfilefunctions;
  }
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


static int
console_app(void)
{ HANDLE h;

  if ( (h = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE )
  { DWORD mode;

    if ( GetConsoleMode(h, &mode) )
      return TRUE;
  }

  return FALSE;
}


static int
do_create_process(p_options *info)
{ int flags = 0;
  PROCESS_INFORMATION pi;
  STARTUPINFOW si;

  switch(info->window)
  { case MAYBE:
      if ( !console_app() )
	flags |= CREATE_NO_WINDOW;
      break;
    case TRUE:
      break;
    case FALSE:
      flags |= CREATE_NO_WINDOW;
      break;
  }

  if ( info->detached )
    flags |= CREATE_BREAKAWAY_FROM_JOB;
  if ( info->envbuf.buffer )
    flags |= CREATE_UNICODE_ENVIRONMENT;

  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

				      /* stdin */
  switch( info->streams[0].type )
  { case std_pipe:
    case std_stream:
      si.hStdInput = info->streams[0].fd[0];
      SetHandleInformation(info->streams[0].fd[1],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdInput = open_null_stream(GENERIC_READ);
      break;
    case std_std:
      si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
      break;
  }
				      /* stdout */
  switch( info->streams[1].type )
  { case std_pipe:
    case std_stream:
      si.hStdOutput = info->streams[1].fd[1];
      SetHandleInformation(info->streams[1].fd[0],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdOutput = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
      break;
  }
				      /* stderr */
  switch( info->streams[2].type )
  { case std_pipe:
    case std_stream:
      si.hStdError = info->streams[2].fd[1];
      SetHandleInformation(info->streams[2].fd[0],
                           HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdError = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
      break;
  }

  if ( CreateProcessW(info->exe,
		      info->cmdline,
		      NULL,		/* Process security */
		      NULL,		/* Thread security */
		      TRUE,		/* Inherit handles */
		      flags,		/* Creation flags */
		      info->envbuf.buffer, /* Environment */
		      info->cwd,	/* Directory */
		      &si,		/* Startup info */
		      &pi) )		/* Process information */
  { int rc = TRUE;
    HANDLE hJob = (HANDLE)0;
    if ( !info->detached )
    {
      hJob = rootJob;
    }
    CloseHandle(pi.hThread);

    if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = create_process_context(info, (intptr_t)pi.hProcess);

      DEBUG(Sdprintf("Wait on pipes\n"));

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
    { register_process(pi.dwProcessId, pi.hProcess, hJob);
      return PL_unify_integer(info->pid, pi.dwProcessId);
    }

    return win_wait_success(info->exe_name, pi.hProcess);
  } else
  { return win_error("CreateProcess");
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

static int
unregister_process(pid_t pid)
{ posix_process **ppp, *pp;
  LOCK();
  for(ppp=&processes, pp=*ppp; pp; ppp=&pp->next, pp=*ppp)
  { if ( pp->pid == pid )
    { *ppp = pp->next;
      PL_free(pp);
      UNLOCK();
      return TRUE;
    }
  }

  UNLOCK();
  return FALSE;
}

static int
kill_all_processes(int rc, void* arg)
{ posix_process* pp;
  for(pp=processes; pp; pp=pp->next)
    kill(pp->pid, SIGTERM);
  return 0;
}

static void
posix_init()
{ PL_exit_hook(kill_all_processes, NULL);
}

#endif


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

static int
create_pipes(p_options *info)
{ FOREACH_STREAM(info, i, s)
  { if ( s->term && s->type == std_pipe )
    { p_stream *matched_stream = find_matching_stream(info, s);
      if ( matched_stream )
      { s->fd[0] = matched_stream->fd[0];
	s->fd[1] = matched_stream->fd[1];
      } else
      { int my_side;

#ifdef HAVE_PIPE2
	if ( pipe2(s->fd, O_CLOEXEC) == 0 )
	{ s->cloexec = TRUE;
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
	my_side = ((s->mode & SIO_OUTPUT) ? s->fd[1] : s->fd[0]);
#ifdef F_SETFD
        if ( fcntl(my_side, F_SETFD, FD_CLOEXEC) == 0 )
	  s->cloexec = TRUE;
#endif
      }
    } else if ( s->term && s->type == std_stream )
    { if ( fcntl(s->fd[0], F_SETFD, FD_CLOEXEC) == 0 )
        s->cloexec = TRUE;
    }
  }

  return TRUE;
}


static int
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
    return FALSE;
  }
}


static int
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ pid_t p2;
  int status;

  if ( opts->has_timeout && opts->timeout == 0.0 )
  { if ( (p2=waitpid(pid, &status, WNOHANG)) == pid )
    {
#ifndef HAVE_PRCTL
      unregister_process(pid);
#endif
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
      return unify_exit_status(code, status);

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
    { goto error;
    }
  }
}


static int
wait_success(atom_t name, pid_t pid)
{ pid_t p2;

  for(;;)
  { int status;

    if ( (p2=waitpid(pid, &status, 0)) == pid )
    { if ( WIFEXITED(status) && WEXITSTATUS(status) == 0 )
      {
#ifndef HAVE_PRCTL
        unregister_process(pid);
#endif
        return TRUE;
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
	return FALSE;
      }
    }

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
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
p_fdopen(p_options *info, p_stream *stream, int fdn)
{ IOSTREAM *s;
  char m[10];
  char *mp = m;

  *mp++ = (stream->mode & SIO_INPUT) ? 'r' : 'w';
  if ( stream->encoding == ENC_OCTET )
    *mp++ = 'b';
  *mp = 0;

  if ( (s=Sfdopen(stream->fd[fdn], m)) )
    s->encoding = stream->encoding;

  return s;
}


static int
process_parent_side(p_options *info, int pid)
{ int rc = TRUE;
  process_context *pc = NULL;

  if ( info->pipes > 0 && info->pid == 0 )
  { /* no pid(Pid): wait */
    pc = create_process_context(info, pid);
    DEBUG(Sdprintf("Wait on pipes\n"));
  }

  if ( info->pipes > 0 )
  { IOSTREAM *s;
    FOREACH_STREAM(info, fd, stream)
    { if ( stream->type == std_pipe && !find_matching_stream(info, stream) )
      { int myside = (stream->mode & SIO_OUTPUT) ? 1 : 0;
        close_ok(stream->fd[1 - myside]);
	if ( (s = pc
		? open_process_pipe(pc, info, fd, myside)
		: p_fdopen(info, stream, myside)) )
	  rc = PL_unify_stream(stream->term, s);
	else
	  close_ok(stream->fd[myside]);
      }
    }

    if ( info->pid == 0 )
      return rc;
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

static int
do_create_process_fork(p_options *info, create_method method)
{ int pid;
  sigset_t set;

  blockSignal(SIGPROF, &set);
  if ( method == PCREATE_VFORK )
    pid = vfork();
  else
    pid = fork();
  if ( pid != 0 )				/* parent */
    restoreSignals(&set);

  if ( pid == 0 )				/* child */
  { int fd;

    PL_cleanup_fork();

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
	exit(1);
      }
    }

    FOREACH_STREAM(info, streamfd, stream)
    { switch( stream->type )
      { case std_pipe:
	case std_stream:
	{ int pairhalf = (stream->mode & SIO_OUTPUT) /* parent writes, child reads */
		       ? 0 : 1;
	  dup2(stream->fd[pairhalf], streamfd);
	  if ( !stream->cloexec )
	    close(stream->fd[1 - pairhalf]);
	  break;
	}
	case std_null:
	  if ( (fd = open("/dev/null", CHILD_OPEN_FLAGS(stream->mode))) >= 0 )
	    dup2(fd, streamfd);
	  break;
	case std_std:
	  fd = streamfd == 0 ? Sfileno(Suser_input)
		: streamfd == 1 ? Sfileno(Suser_output)
		: streamfd == 2 ? Sfileno(Suser_error)
		: -1;
          if ( fd >= 0 && fd != streamfd )
	    dup2(fd, streamfd);
	  break;
	case std_unbound:
	  close(streamfd);
	  break;
      }
    }

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
    exit(1);
  } else if ( pid < 0 )			/* parent */
  { term_t exe = PL_new_term_ref();
    PL_put_atom_chars(exe, info->exe);

    return pl_error(NULL, 0, "fork", ERR_ERRNO, errno, "fork", "process", exe);
  } else
  { return process_parent_side(info, pid);
  }
}


#ifdef HAVE_POSIX_SPAWN
#include <spawn.h>

static int
do_create_process(p_options *info)
{ pid_t pid;
  posix_spawn_file_actions_t file_actions;
  posix_spawnattr_t attr;
  int rc;

  if ( info->cwd || create_process_method != PCREATE_SPAWN )
    return do_create_process_fork(info, create_process_method);

  posix_spawn_file_actions_init(&file_actions);
  posix_spawnattr_init(&attr);

  if ( info->detached )
    posix_spawnattr_setpgroup(&attr, 0);

  FOREACH_STREAM(info, fd, stream)
  { switch( stream->type )
    { case std_pipe:
      case std_stream:
      { int pairhalf = (stream->mode & SIO_OUTPUT ) /* parent writes, child reads */
		     ? 0 : 1;
        posix_spawn_file_actions_adddup2(&file_actions, stream->fd[pairhalf], fd);
        if ( !stream->cloexec )
	  posix_spawn_file_actions_addclose(&file_actions, stream->fd[1 - pairhalf]);
        break;
      }
      case std_null:
        posix_spawn_file_actions_addopen(&file_actions, fd,
	  				 "/dev/null", CHILD_OPEN_FLAGS(stream->mode), 0);
        break;
      case std_std:
        break;
      case std_unbound:
        posix_spawn_file_actions_addclose(&file_actions, fd);
	break;
    }
  }

  rc = posix_spawn(&pid, info->exe,
		   &file_actions, &attr,
		   info->argv, info->envp);

  posix_spawn_file_actions_destroy(&file_actions);
  posix_spawnattr_destroy(&attr);

  if ( rc == 0 )
  { return process_parent_side(info, pid);
  } else
  { term_t exe = PL_new_term_ref();
    PL_put_atom_chars(exe, info->exe);

    return pl_error(NULL, 0, "spawn", ERR_ERRNO, errno, "fork", "process", exe);
  }
}

#else /*HAVE_POSIX_SPAWN*/

static int
do_create_process(p_options *info)
{ return do_create_process_fork(info, create_process_method);
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

static int
ensure_stack_resources(int count)
{ fid_t fid = PL_open_foreign_frame();
  term_t list = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(list);

  while ( count-- > 0 )
  { term_t head;

    if ( !(head = PL_new_term_ref()) ||
	 !PL_unify_list(tail, head, tail) )
    { PL_close_foreign_frame(fid);
      return FALSE;
    }
  }

  PL_discard_foreign_frame(fid);
  return TRUE;
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
  int rc = FALSE;

  if ( !ensure_stack_resources(10) )	/* max 3 stream structures */
    return FALSE;

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


static int
get_pid(term_t pid, pid_t *p)
{ int n;

  if ( !PL_get_integer_ex(pid, &n) )
    return FALSE;
  if ( n < 0 )
    return PL_domain_error("not_less_than_zero", pid),FALSE;

  *p = n;
  return TRUE;
}


static foreign_t
process_wait(term_t pid, term_t code, term_t options)
{ pid_t p;
  wait_options opts;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();

  if ( !get_pid(pid, &p) )
    return FALSE;

  memset(&opts, 0, sizeof(opts));
  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);
    if ( name == ATOM_timeout )
    { atom_t a;

      if ( !(PL_get_atom(arg, &a) && a == ATOM_infinite) )
      { if ( !PL_get_float(arg, &opts.timeout) )
	  return PL_type_error("timeout", arg);
	opts.has_timeout = TRUE;
      }
    } else if ( name == ATOM_release )
    { if ( !PL_get_bool_ex(arg, &opts.release) )
	return FALSE;
      if ( opts.release == FALSE )
	return PL_domain_error("true", arg);
    } else
      return PL_domain_error("process_wait_option", head);
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  return wait_for_pid(p, code, &opts);
}

#ifndef __WINDOWS__
static foreign_t
process_kill_posix(term_t pid_term, pid_t pid, int sig)
{ if ( kill(pid, sig) == 0 )
    return TRUE;

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
    return FALSE;

{
#ifdef __WINDOWS__
  HANDLE h;

  if ( !(h=find_process_from_pid(p, "process_kill")) )
    return FALSE;

  if ( TerminateProcess(h, 255) )
    return TRUE;

  return win_error("TerminateProcess");
#else /*__WINDOWS__*/
  int sig;

  if ( !PL_get_signum_ex(signal, &sig) )
    return FALSE;

  return process_kill_posix(pid, p, sig);
#endif /*__WINDOWS__*/
}
}

static foreign_t
process_group_kill(term_t pid, term_t signal)
{ pid_t p;

  if ( !get_pid(pid, &p) )
    return FALSE;

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
  return TRUE;
#else /*__WINDOWS__*/
   int sig;
   if ( !PL_get_signum_ex(signal, &sig) )
     return FALSE;
   return process_kill_posix(pid, -p, sig);
#endif /*__WINDOWS__*/
 }
}


static foreign_t
process_set_method(term_t how)
{ char *s;

  if ( PL_get_chars(how, &s, CVT_ATOM|CVT_EXCEPTION) )
  { if ( strcmp(s, "fork") == 0 )
      create_process_method = PCREATE_FORK;
    else if ( strcmp(s, "vfork") == 0 )
      create_process_method = PCREATE_VFORK;
    else if ( strcmp(s, "spawn") == 0 )
      create_process_method = PCREATE_SPAWN;
    else
      return PL_domain_error("process_create_method", how);

    return TRUE;
  }

  return FALSE;
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
  MKATOM(extra_streams);
  MKATOM(std);
  MKATOM(null);
  MKATOM(process);
  MKATOM(detached);
  MKATOM(cwd);
  MKATOM(env);
  MKATOM(environment);
  MKATOM(priority);
  MKATOM(window);
  MKATOM(timeout);
  MKATOM(release);
  MKATOM(infinite);
  MKATOM(text);
  MKATOM(binary);
  MKATOM(octet);
  MKATOM(utf8);
  MKATOM(ascii);
  MKATOM(iso_latin_1);
  MKATOM(unicode_be);
  MKATOM(unicode_le);

  MKFUNCTOR(pipe, 1);
  MKFUNCTOR(pipe, 2);
  MKFUNCTOR(type, 1);
  MKFUNCTOR(encoding, 1);
  MKFUNCTOR(stream, 1);
  MKFUNCTOR(from_child, 1);
  MKFUNCTOR(to_child, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(process_error, 2);
  MKFUNCTOR(system_error, 2);
  MKFUNCTOR(exit, 1);
  MKFUNCTOR(killed, 1);

  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("process_create",	    2, process_create,	   0);
  PL_register_foreign("process_wait",	    3, process_wait,	   0);
  PL_register_foreign("process_kill",	    2, process_kill,	   0);
  PL_register_foreign("process_group_kill", 2, process_group_kill, 0);
  PL_register_foreign("process_set_method", 1, process_set_method, 0);
}
