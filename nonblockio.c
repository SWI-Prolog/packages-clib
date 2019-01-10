/*  Part of SWI-Prolog
    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

#define O_DEBUG 1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is extracted from socket.c to   provide  a common ground for
accessing sockets and  possibly  other   devices  in  non-blocking mode,
allowing  for  GUI  (XPCE)  event   dispatching,  timeout  handling  and
multi-threaded signal and timeout handling.

Besides dealing with nonblocking aspects,  an   important  facet of this
library is to hide OS differences.


API
---

The API is completely the same as for   blocking IO. It is however built
on top of sockets used in non-blocking   mode which enables the layer to
listen to Prolog events such  as   timeouts,  GUI  processing and thread
interaction. The functions are modelled  after   the  POSIX  socket API,
prefixed with nbio_*:

	nbio_socket()
	nbio_connect()
	nbio_bind()
	nbio_listen()
	nbio_accept()
	nbio_closesocket()

and IO is realised using

	nbio_read()		See also below
	nbio_write()

Overall control of the library:

	nbio_init()
	nbio_cleanup()
	nbio_debug()

Error handling

	nbio_error()		Raises a Prolog exception

Settings

	nbio_setopt()
	nbio_get_flags()

Address Converstion

	nbio_get_sockaddr()
	nbio_get_ip4()

Alternative to nbio_read() and nbio_write(), the application program may
call  the  low-level  I/O  routines  in    non-blocking  mode  and  call
nbio_wait(int socket, nbio_request request). This  function returns 0 if
it thinks the call might  now  succeed   and  -1  if  an error occurred,
leaving the exception context in Prolog. On  receiving -1, the user must
return an I/O error as soon as possible.


Windows issues
--------------

Winsock is hard to handle in blocking   mode  without blocking the whole
lot, notably (timeout) signals. Old versions uses WSAASyncSelect() and a
separate thread. This is slow and   complicated. The current version was
written     by     Keri     Harris     and     uses     WSAEvent     and
MsgWaitForMultipleObjects(). See wait_socket().

Unix issues
-----------

In the Unix version we simply call PL_dispatch() before doing recv() and
leave the details to this function.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <config.h>

#if defined(__MINGW32__)
#define __try
#define __except(_) if (0)
#define __finally
#endif

#if defined(__MINGW32__)
#define WINVER 0x0501
#include <ws2tcpip.h>
#endif

#include "nonblockio.h"

#include <SWI-Stream.h>
#include "clib.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>
#ifdef __WINDOWS__
#include <malloc.h>
#endif
#if defined(HAVE_POLL_H)
#include <poll.h>
#endif

#ifdef __WINDOWS__
#define GET_ERRNO WSAGetLastError()
#define GET_H_ERRNO WSAGetLastError()
#else
#define GET_ERRNO errno
#define GET_H_ERRNO h_errno
#endif

#ifndef __WINDOWS__
#define closesocket(n) close((n))	/* same on Unix */
#endif

#ifndef __WINDOWS__
#define INVALID_SOCKET -1
#endif

#ifndef SD_SEND
#define SD_RECEIVE 0			/* shutdown() parameters */
#define SD_SEND    1
#define SD_BOTH    2
#endif

#ifndef SOCKET_ERROR
#define SOCKET_ERROR (-1)
#endif

#define set(s, f)   ((s)->flags |= (f))
#define clear(s, f) ((s)->flags &= ~(f))
#define true(s, f)  ((s)->flags & (f))
#define false(s, f) (!true(s, f))

#define PLSOCK_MAGIC  0x38da3f2c
#define PLSOCK_CMAGIC 0x38da3f2d

typedef struct _plsocket
{ int		    magic;		/* PLSOCK_MAGIC */
  SOCKET	    socket;		/* The OS socket */
  int		    flags;		/* Misc flags */
  atom_t	    symbol;		/* <socket>(%p) */
  IOSTREAM *	    input;		/* input stream */
  IOSTREAM *	    output;		/* output stream */
#ifdef __WINDOWS__
  WSAEVENT          event;		/* Winsock event */
#endif
} plsocket;

#define VALID_SOCKET_RET(s, r) \
	do						\
	{ if ( !(s && (s)->magic == PLSOCK_MAGIC) )	\
	  { errno = EINVAL;				\
	    return (r);					\
	  }						\
	} while(0)
#define VALID_SOCKET(s) VALID_SOCKET_RET(s, -1)


static plsocket *allocSocket(SOCKET socket);
#ifdef __WINDOWS__
static const char *WinSockError(unsigned long eno);
#endif
static int need_retry(int error);

#ifdef O_DEBUG
static int debugging;

int
nbio_debug(int level)
{ int old = debugging;

  if ( level >= 0 )			/* -1 --> return current setting */
    debugging = level;

  return old;
}

#define DEBUG(l, g) if ( debugging >= l ) g
#else
#define DEBUG(l, g) (void)0

int
nbio_debug(int level)
{ return 0;
}

#endif /*O_DEBUG*/

		 /*******************************
		 *	  COMPATIBILITY		*
		 *******************************/

#ifdef __WINDOWS__

#if O_DEBUG
static char *
sepstrcatskip(char *buf, char *dest, char *src)
{ if ( dest != buf )
  { *dest++ = '|';
    *dest = '\0';
  }
  strcat(dest, src);
  dest += strlen(dest);

  return dest;
}

static char *
event_name(int ev)
{ char buf[256];
  char *o = buf;

  o[0] = '\0';
  if ( (ev & FD_READ) )    o=sepstrcatskip(buf, o, "FD_READ");
  if ( (ev & FD_WRITE) )   o=sepstrcatskip(buf, o, "FD_WRITE");
  if ( (ev & FD_ACCEPT) )  o=sepstrcatskip(buf, o, "FD_ACCEPT");
  if ( (ev & FD_CONNECT) ) o=sepstrcatskip(buf, o, "FD_CONNECT");
  if ( (ev & FD_CLOSE) )   o=sepstrcatskip(buf, o, "FD_CLOSE");
  if ( (ev & FD_OOB) )	   o=sepstrcatskip(buf, o, "FD_OOB");
  if ( (ev & ~(FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE)) )
    sepstrcatskip(buf, o, "FD_???");

  return strdup(buf);
}

#endif /*O_DEBUG*/

#define F_SETFL		0
#define O_NONBLOCK	0

static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ VALID_SOCKET(socket);

  switch(op)
  { case F_SETFL:
      switch(arg)
      { case O_NONBLOCK:
	{ int rval;
#if defined(__MINGW32__)
	  u_long non_block;
#else
          /* FIXME: is this really `int' for MSC? */
	  int non_block;
#endif
	  non_block = 1;
	  rval = ioctlsocket(socket->socket, FIONBIO, &non_block);
	  if ( rval )
	  { set(socket, PLSOCK_NONBLOCK);
	    return 0;
	  }

	  return -1;
	}
	default:
	  return -1;
      }
    break;
    default:
      return -1;
  }
}


static int
need_retry(int error)
{ if ( error == WSAEINTR || error == WSAEWOULDBLOCK )
  { DEBUG(1, Sdprintf("need_retry(%d): %s\n", error, WinSockError(error)));
    return TRUE;
  }

  return FALSE;
}


static int
wait_socket(plsocket *s)
{ int index;

  for(;;)
  { DEBUG(2, Sdprintf("waiting on socket: %d\n", s->socket));
    index = MsgWaitForMultipleObjects(1, &s->event, FALSE, INFINITE, QS_ALLINPUT);

    if ( index == WAIT_FAILED )
    { nbio_error(GetLastError(), TCP_ERRNO);
      return FALSE;
    } else if ( index == WAIT_OBJECT_0+0 ) /* socket event */
    { WSANETWORKEVENTS events;

      if ( WSAEnumNetworkEvents(s->socket, s->event, &events) == SOCKET_ERROR )
      { nbio_error(GET_ERRNO, TCP_ERRNO);
        return FALSE;
      }
      DEBUG(2,
            { char *nm = event_name(events.lNetworkEvents);
              Sdprintf("WM_SOCKET on %d: ev=(%s)\n", s->socket, nm);
	      free(nm);
            });
      if ( events.lNetworkEvents & FD_CONNECT )
      { if ( events.iErrorCode[FD_CONNECT_BIT] )
        { nbio_error(events.iErrorCode[FD_CONNECT_BIT], TCP_ERRNO);
          return FALSE;
        }
      }
      if ( events.lNetworkEvents & FD_ACCEPT )
      { if ( events.iErrorCode[FD_ACCEPT_BIT] )
        { nbio_error(events.iErrorCode[FD_ACCEPT_BIT], TCP_ERRNO);
          return FALSE;
        }
      }
      if ( events.lNetworkEvents & FD_READ )
      { if ( events.iErrorCode[FD_READ_BIT] )
        { nbio_error(events.iErrorCode[FD_READ_BIT], TCP_ERRNO);
          return FALSE;
        }
      }
      if ( events.lNetworkEvents & FD_WRITE )
      { if ( events.iErrorCode[FD_WRITE_BIT] )
        { nbio_error(events.iErrorCode[FD_WRITE_BIT], TCP_ERRNO);
          return FALSE;
        }
      }
      break;
    } else if ( index == WAIT_OBJECT_0+1 ) /* message event */
    { MSG msg;

      DEBUG(2, Sdprintf("interrupted socket: %p\n", s->socket));
      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
        DispatchMessage(&msg);

        if ( PL_handle_signals() < 0 )
          return FALSE;
        continue;
      }
    }
  }
  return TRUE;
}


int
nbio_wait(nbio_sock_t socket, nbio_request request)
{ VALID_SOCKET(socket);

  return wait_socket(socket) ? 0 : -1;
}


#else /*__WINDOWS__*/

static int
need_retry(int error)
{ if ( error == EINTR || error == EAGAIN || error == EWOULDBLOCK )
  { DEBUG(1, Sdprintf("need_retry(%d): %s\n", error, strerror(error)));
    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
wait_socket() is the Unix way  to  wait   for  input  on  the socket. By
default event-dispatching on behalf of XPCE is performed. If this is not
desired, you can use tcp_setopt(Socket,  dispatch(false)), in which case
this call returns immediately, assuming the   actual TCP call will block
without dispatching if no input is available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
wait_socket(plsocket *s)
{ if ( true(s, PLSOCK_DISPATCH) )
  { int fd = s->socket;

    if ( true(s, PLSOCK_NONBLOCK) && !PL_dispatch(fd, PL_DISPATCH_INSTALLED) )
    {
#ifdef HAVE_POLL
      struct pollfd fds[1];

      fds[0].fd = fd;
      fds[0].events = POLLIN;

      poll(fds, 1, 250);
      return TRUE;
#else
      if ( fd < FD_SETSIZE )		/* Unix only, so ok */
      { fd_set rfds;
	struct timeval tv;

	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);
	tv.tv_sec = 0;
	tv.tv_usec = 250000;

	select(fd+1, &rfds, NULL, NULL, &tv);
	return TRUE;
      }
#endif
    } else
    { return PL_dispatch(fd, PL_DISPATCH_WAIT);
    }
  }

  return TRUE;
}


int
nbio_wait(nbio_sock_t socket, nbio_request request)
{ VALID_SOCKET(socket);

  return wait_socket(socket) ? 0 : -1;
}


static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ int rc;

  VALID_SOCKET(socket);

  rc = fcntl(socket->socket, op, arg);

  if ( rc == 0 )
  { if ( op == F_SETFL && arg == O_NONBLOCK )
      set(socket, PLSOCK_NONBLOCK);
  } else
    nbio_error(GET_ERRNO, TCP_ERRNO);

  return rc;
}

#endif /*__WINDOWS__*/


		 /*******************************
		 *	 ADMINISTRATION		*
		 *******************************/

static functor_t FUNCTOR_module2;
static functor_t FUNCTOR_ip4;
static functor_t FUNCTOR_ip1;
static atom_t ATOM_any;
static atom_t ATOM_broadcast;
static atom_t ATOM_loopback;

static int initialised = FALSE;		/* Windows only */

SOCKET
nbio_fd(nbio_sock_t socket)
{ VALID_SOCKET(socket);

  return socket->socket;
}

void
nbio_set_symbol(nbio_sock_t socket, atom_t symbol)
{ socket->symbol = symbol;
}

int
is_nbio_socket(nbio_sock_t socket)
{ return socket && socket->magic == PLSOCK_MAGIC;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a wrapper for an OS  socket.   The  wrapper  is allocated in an
array of pointers, to keep small  integer   identifiers  we can use with
FD_SET, etc. for implementing a compatible nbio_select().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static plsocket *
allocSocket(SOCKET socket)
{ plsocket *p;

  if ( !(p = malloc(sizeof(*p))) )
  { PL_resource_error("memory");
    return NULL;
  }

  memset(p, 0, sizeof(*p));
  p->socket = socket;
  p->flags  = PLSOCK_DISPATCH|PLSOCK_VIRGIN;	/* by default, dispatch */
  p->magic  = PLSOCK_MAGIC;
  p->input = p->output = (IOSTREAM*)NULL;

#ifdef __WINDOWS__
  { WSAEVENT event = WSACreateEvent();
    WSAEventSelect(socket, event, FD_READ|FD_WRITE|FD_ACCEPT|FD_CONNECT|FD_CLOSE);
    p->event = event;
  }
#endif

  DEBUG(2, Sdprintf("[%d]: allocSocket(%d) --> %p\n",
		    PL_thread_self(), socket, p));

  return p;
}


void
freeSocket(nbio_sock_t s)
{ if ( s )
  { if ( s->magic == PLSOCK_CMAGIC )
      free(s);
    else
      s->symbol = 0;
  }
}


static int
closeSocket(plsocket *s)
{ int rval;
  SOCKET sock;

  DEBUG(2, Sdprintf("Closing %p (%d)\n", s, s->socket));
  if ( !s || s->magic != PLSOCK_MAGIC )
  { DEBUG(1, Sdprintf("OOPS: closeSocket(%p) s->magic = %ld\n",
		      s, s ? s->magic : 0));
    errno = EINVAL;
    return -1;
  }

  sock = s->socket;
  s->magic = PLSOCK_CMAGIC;

#ifdef __WINDOWS__
  if ( s->event )
    WSACloseEvent(s->event);
#endif

  if ( sock != INVALID_SOCKET )
  { again:
    if ( (rval=closesocket(sock)) == SOCKET_ERROR )
    { if ( errno == EINTR )
	goto again;
    }
    DEBUG(2, Sdprintf("closeSocket(%p=%d): closesocket() returned %d\n",
		      s, (int)sock, rval));
  } else
  { DEBUG(2, Sdprintf("closeSocket(%p=%d): already closed\n",
		      s, (int)sock));
    rval = 0;				/* already closed.  Use s->error? */
  }

  if ( !s->symbol )
    free(s);

  return rval;
}


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

#ifdef __WINDOWS__

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code in BILLY_GETS_BETTER is, according to various documents the
right code, but it doesn't work, so we do it by hand.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef BILLY_GETS_BETTER
static const char *
WinSockError(unsigned long eno)
{ char buf[1024];
  static HMODULE netmsg = 0;
  static int netmsg_loaded = FALSE;
  unsigned long flags = (FORMAT_MESSAGE_FROM_SYSTEM|
			 FORMAT_MESSAGE_IGNORE_INSERTS);

  if ( !netmsg_loaded )
  { netmsg_loaded = TRUE;
    netmsg = LoadLibraryEx("netmsg.dll", 0, LOAD_LIBRARY_AS_DATAFILE);
    if ( !netmsg )
      Sdprintf("failed to load netmsg.dll\n");
    else
      Sdprintf("Loaded netmsg.dll as %p\n", netmsg);
  }

  if ( netmsg )
    flags |= FORMAT_MESSAGE_FROM_HMODULE;

  if ( !FormatMessage(flags,
		      netmsg,
		      eno,
		      GetUserDefaultLangID(),
		      /*MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),*/
		      buf, sizeof(buf),
		      0))
  { sprintf(buf, "Unknown socket error (%u)", eno);
  }

  buf[sizeof(buf)-1]='\0';

  return strdup(buf);
}

#else /*BILLY_GETS_BETTER*/

static const char *
WinSockError(unsigned long error)
{ struct
  { int index;
    const char *string;
  } *ep, edefs[] =
  { { WSAEACCES, "Permission denied" },
    { WSAEADDRINUSE, "Address already in use" },
    { WSAEADDRNOTAVAIL, "Cannot assign requested address" },
    { WSAEAFNOSUPPORT, "Address family not supported by protocol family" },
    { WSAEALREADY, "Operation already in progress" },
    { WSAECONNABORTED, "Software caused connection abort" },
    { WSAECONNREFUSED, "Connection refused" },
    { WSAECONNRESET, "Connection reset by peer" },
    { WSAEDESTADDRREQ, "Destination address required" },
    { WSAEFAULT, "Bad address" },
    { WSAEHOSTDOWN, "Host is down" },
    { WSAEHOSTUNREACH, "No route to host" },
    { WSAEINPROGRESS, "Operation now in progress" },
    { WSAEINTR, "Interrupted function call" },
    { WSAEINVAL, "Invalid argument" },
    { WSAEISCONN, "Socket is already connected" },
    { WSAEMFILE, "Too many open files" },
    { WSAEMSGSIZE, "Message too long" },
    { WSAENETDOWN, "Network is down" },
    { WSAENETRESET, "Network dropped connection on reset" },
    { WSAENETUNREACH, "Network is unreachable" },
    { WSAENOBUFS, "No buffer space available" },
    { WSAENOPROTOOPT, "Bad protocol option" },
    { WSAENOTCONN, "Socket is not connected" },
    { WSAENOTSOCK, "Socket operation on non-socket" },
    { WSAEOPNOTSUPP, "Operation not supported" },
    { WSAEPFNOSUPPORT, "Protocol family not supported" },
    { WSAEPROCLIM, "Too many processes" },
    { WSAEPROTONOSUPPORT, "Protocol not supported" },
    { WSAEPROTOTYPE, "Protocol wrong type for socket" },
    { WSAESHUTDOWN, "Cannot send after socket shutdown" },
    { WSAESOCKTNOSUPPORT, "Socket type not supported" },
    { WSAETIMEDOUT, "Connection timed out" },
    { WSAEWOULDBLOCK, "Resource temporarily unavailable" },
    { WSAEDISCON, "Graceful shutdown in progress" },
    { WSANOTINITIALISED, "Socket layer not initialised" },
					/* WinSock 2 errors */
    { WSAHOST_NOT_FOUND, "Host not found" },
    { WSANO_DATA, "Valid name, no data record of requested type" },
    { 0, NULL }
  };
  char tmp[100];
  for(ep=edefs; ep->string; ep++)
  { if ( ep->index == (int)error )
      return ep->string;
  }

  sprintf(tmp, "Unknown error %ld", error);
  return strdup(tmp);			/* leaks memory on unknown errors */
}

#endif /*BILLY_GETS_BETTER*/
#endif /*__WINDOWS__*/

		 /*******************************
		 *     POSIX SOCKET ERRORS	*
		 *******************************/

#include "esymbols.ic"

#ifndef __WINDOWS__
#ifndef HAVE_HSTRERROR
#define hstrerror my_hstrerror

static const char *
hstrerror(int code)
{ return error_symbol(code, h_errno_symbols);
}
#endif /*HAVE_HSTRERROR*/

#ifndef HAVE_GAI_STRERROR
#define gai_strerror my_gai_strerror

static const char *
gai_strerror(int code)
{ return error_symbol(code, gai_errno_symbols);
}
#endif /*HAVE_HAVE_GAI_STRERROR*/
#endif /*__WINDOWS__*/

int
nbio_error(int code, nbio_error_map mapid)
{ const char *msg;
  const char *symbol;
  term_t ex;

  if ( code == EPLEXCEPTION || PL_exception(0) )
    return FALSE;

#ifdef __WINDOWS__
  msg = WinSockError(code);
  symbol = error_symbol(code, wsa_errno_symbols);
#else
  switch( mapid )
  { case TCP_ERRNO:
      msg = strerror(code);
      symbol = error_symbol(code, errno_symbols);
      break;
    case TCP_HERRNO:
      msg = hstrerror(code);
      symbol = error_symbol(code, h_errno_symbols);
      break;
    case TCP_GAI_ERRNO:
      msg = gai_strerror(code);
      symbol = error_symbol(code, gai_errno_symbols);
      break;
    default:
      assert(0);
      msg = NULL;
      break;
  }
#endif

  return ( (ex = PL_new_term_ref()) &&
	   PL_unify_term(ex,
			 CompoundArg("error", 2),
			   CompoundArg("socket_error", 2),
			     AtomArg(symbol),
			     AtomArg(msg),
			   PL_VARIABLE) &&
	   PL_raise_exception(ex)
	 );
}


const char *
nbio_last_error(nbio_sock_t socket)
{ return NULL;
}


		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

int
nbio_init(const char *module)
{ if ( initialised )			/* called from install handlers, which */
    return TRUE;			/* are serialized by the compiler mutex */
  initialised = TRUE;

  FUNCTOR_module2  = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_ip4	   = PL_new_functor(PL_new_atom("ip"), 4);
  FUNCTOR_ip1	   = PL_new_functor(PL_new_atom("ip"), 1);
  ATOM_any	   = PL_new_atom("any");
  ATOM_broadcast   = PL_new_atom("broadcast");
  ATOM_loopback	   = PL_new_atom("loopback");

#ifdef __WINDOWS__
{ WSADATA WSAData;

  if ( WSAStartup(MAKEWORD(2,0), &WSAData) )
    return PL_warning("nbio_init() - WSAStartup failed.");
}
#endif /*__WINDOWS__*/

  return TRUE;
}


int
nbio_cleanup(void)
{ if ( initialised )
  {
#ifdef __WINDOWS__
    WSACleanup();
#endif
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
socket(-Socket)
    Create a stream inet socket.  The socket is represented by a term of
    the format $socket(Id).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

nbio_sock_t
nbio_socket(int domain, int type, int protocol)
{ SOCKET sock;
  plsocket *s;

  assert(initialised);

  if ( (sock = socket(domain, type , protocol)) == INVALID_SOCKET )
  { nbio_error(GET_ERRNO, TCP_ERRNO);
    return NULL;
  }
  if ( !(s=allocSocket(sock)) )		/* register it */
  { closesocket(sock);
    return NULL;
  }

  return s;
}


int
nbio_closesocket(nbio_sock_t socket)
{ int rc = 0;

  VALID_SOCKET(socket);

  clear(socket, PLSOCK_VIRGIN);

  if ( true(socket, PLSOCK_OUTSTREAM|PLSOCK_INSTREAM) )
  { int flags = socket->flags;		/* may drop out! */

    if ( flags & PLSOCK_INSTREAM )
    { assert(socket->input);
      if ( Slock(socket->input) == 0 )
	rc += Sclose(socket->input);
      else
	rc--;
    }
    if ( flags & PLSOCK_OUTSTREAM )
    { assert(socket->output);
      if ( Slock(socket->output) == 0 )
	rc += Sclose(socket->output);
      else
	rc--;
    }
  } else
  { rc = 0;
#ifdef __WINDOWS__
    shutdown(socket->socket, SD_BOTH);
#endif
    closeSocket(socket);
  }

  return rc;
}

int
nbio_setopt(nbio_sock_t socket, nbio_option opt, ...)
{ va_list args;
  int rc;

  VALID_SOCKET(socket);

  va_start(args, opt);

  switch(opt)
  { case TCP_NONBLOCK:
      rc = nbio_fcntl(socket, F_SETFL, O_NONBLOCK);
      break;
    case TCP_REUSEADDR:
    { int val = va_arg(args, int);

      if( setsockopt(socket->socket, SOL_SOCKET, SO_REUSEADDR,
		     (const char *)&val, sizeof(val)) == -1 )
      { nbio_error(GET_ERRNO, TCP_ERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case SCK_BINDTODEVICE:
    { const char *dev = va_arg(args, char*);

#ifdef SO_BINDTODEVICE
      if ( setsockopt(socket->socket, SOL_SOCKET, SO_BINDTODEVICE,
		      dev, strlen(dev)) == 0 )
      { rc = 0;
        break;
      }

      nbio_error(GET_ERRNO, TCP_ERRNO);
      rc = -1;
#else
      (void)dev;
      rc = -2;
#endif
      break;
    }
    case TCP_NO_DELAY:
#ifdef TCP_NODELAY
    { int val = va_arg(args, int);

#ifndef IPPROTO_TCP			/* Is this correct? */
#define IPPROTO_TCP SOL_SOCKET
#endif
      if ( setsockopt(socket->socket, IPPROTO_TCP, TCP_NODELAY,
		      (const char *)&val, sizeof(val)) == -1 )
      { nbio_error(GET_ERRNO, TCP_ERRNO);
	rc = -1;
      } else
      { rc = 0;
      }

      break;
    }
#else
    { rc = -2;				/* not implemented */
      break;
    }
#endif
    case UDP_BROADCAST:
    { int val = va_arg(args, int);

      if ( setsockopt(socket->socket, SOL_SOCKET, SO_BROADCAST,
		     (const char *)&val, sizeof(val)) == -1 )
      { nbio_error(GET_ERRNO, TCP_ERRNO);
	rc = -1;
      } else
	rc = 0;

      break;
    }
    case TCP_DISPATCH:
    { int val = va_arg(args, int);

      if ( val )
	set(socket, PLSOCK_DISPATCH);
      else
	clear(socket, PLSOCK_DISPATCH);

      rc = 0;

      break;
    }
    case TCP_INSTREAM:
    { IOSTREAM *in = va_arg(args, IOSTREAM*);

      socket->flags |= PLSOCK_INSTREAM;
      socket->flags &= ~PLSOCK_VIRGIN;
      socket->input = in;
      if ( socket->symbol )
	PL_register_atom(socket->symbol);

      rc = 0;

      break;
    }
    case TCP_OUTSTREAM:
    { IOSTREAM *out = va_arg(args, IOSTREAM*);

      socket->flags |= PLSOCK_OUTSTREAM;
      socket->flags &= ~PLSOCK_VIRGIN;
      socket->output = out;
      if ( socket->symbol )
	PL_register_atom(socket->symbol);

      rc = 0;

      break;
    }
    default:
      rc = -1;
      assert(0);
  }

  va_end(args);

  return rc;
}


int
nbio_get_flags(nbio_sock_t socket)
{ VALID_SOCKET(socket);

  return socket->flags;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a host + port-number into a sockaddr structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
nbio_get_port(term_t Port, int *port)
{ char *name;

  if ( PL_get_atom_chars(Port, &name) )
  { struct servent *service;

    if ( !(service = getservbyname(name, "tcp")) )
      return pl_error(NULL, 0, NULL, ERR_EXISTENCE, "service", Port);

    *port = ntohs(service->s_port);
    DEBUG(1, Sdprintf("Service %s at port %d\n", name, *port));
    return TRUE;
  }

  if ( PL_get_integer(Port, port) )
    return TRUE;

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, Port, "port");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert a term Host:Port to a socket address.  Port is either an integer
or the name of a registered port (e.g. 'smtp').

(*) TBD: Supply the port/service here too, simplifying the rest
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_get_sockaddr(term_t Address, struct sockaddr_in *addr, term_t *varport)
{ int port;

  addr->sin_family = AF_INET;
  addr->sin_addr.s_addr = INADDR_ANY;

  if ( PL_is_functor(Address, FUNCTOR_module2) )	/* Host:Port */
  { char *hostName;
    term_t arg = PL_new_term_ref();

    _PL_get_arg(1, Address, arg);
    if ( PL_get_atom_chars(arg, &hostName) )
    { struct addrinfo hints;
      struct addrinfo *res;
      int rc;

      memset(&hints, 0, sizeof(hints));
      hints.ai_family = AF_INET;
      if ( (rc=getaddrinfo(hostName, NULL, &hints, &res)) !=  0) /* see (*) */
	return nbio_error(rc, TCP_GAI_ERRNO);
      assert(res->ai_family == AF_INET);
      memcpy(&addr->sin_addr,
	     &((struct sockaddr_in*)res->ai_addr)->sin_addr,
	     sizeof(addr->sin_addr));
      freeaddrinfo(res);
    } else if ( !nbio_get_ip(arg, &addr->sin_addr) )
    { return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, arg, "atom|ip/4");
    }

    _PL_get_arg(2, Address, arg);
    Address = arg;
  }

  if ( varport && PL_is_variable(Address) )
  { port = 0;
    *varport = Address;
  } else if ( !nbio_get_port(Address, &port) )
    return FALSE;

  addr->sin_port = htons((short)port);

  return TRUE;
}


int
nbio_get_ip(term_t ip4, struct in_addr *ip)
{ uint32_t hip = 0;

  if ( PL_is_functor(ip4, FUNCTOR_ip4) )
  { int i, ia;
    term_t a = PL_new_term_ref();

    for(i=1; i<=4; i++)
    { _PL_get_arg(i, ip4, a);
      if ( PL_get_integer(a, &ia) )
	hip |= ia << ((4-i)*8);
      else
	return FALSE;
    }
    hip = htonl(hip);
    memcpy(ip, &hip, sizeof(hip));

    return TRUE;
  } else if ( PL_is_functor(ip4, FUNCTOR_ip1) )
  { term_t a = PL_new_term_ref();
    atom_t id;

    _PL_get_arg(1, ip4, a);
    if ( PL_get_atom(a, &id) )
    { if ( id == ATOM_any )
	ip->s_addr = INADDR_ANY;
      else if ( id == ATOM_broadcast )
	ip->s_addr = INADDR_BROADCAST;
      else if ( id == ATOM_loopback )
	ip->s_addr = INADDR_LOOPBACK;
      else
	return FALSE;

      return TRUE;
    }
  }

  return FALSE;
}


int
nbio_unify_ip4(term_t Ip, unsigned long hip)
{ return PL_unify_term(Ip,
		       PL_FUNCTOR, FUNCTOR_ip4,
		         IntArg((hip >> 24) & 0xff),
		         IntArg((hip >> 16) & 0xff),
		         IntArg((hip >>  8) & 0xff),
		         IntArg((hip >>  0) & 0xff));
}


int
nbio_bind(nbio_sock_t socket, struct sockaddr *my_addr, size_t addrlen)
{ VALID_SOCKET(socket);

#ifdef __WINDOWS__
  if ( bind(socket->socket, my_addr, (int)addrlen) )
#else
  if ( bind(socket->socket, my_addr, addrlen) )
#endif
  {
    nbio_error(GET_ERRNO, TCP_ERRNO);
    return -1;
  }

  set(socket, PLSOCK_BIND);

  return 0;
}


int
nbio_connect(nbio_sock_t socket,
	     const struct sockaddr *serv_addr,
	     size_t addrlen)
{ VALID_SOCKET(socket);

  for(;;)
  { if ( connect(socket->socket, serv_addr, addrlen) )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	  return -1;
#ifdef __WINDOWS__
        if ( !wait_socket(socket) )
          return -1;
#endif
	continue;
      }
#ifdef __WINDOWS__
      if ( GET_ERRNO == WSAEISCONN )
        break;
#endif
      nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    } else
      break;
  }

  set(socket, PLSOCK_CONNECT);

  return 0;
}


nbio_sock_t
nbio_accept(nbio_sock_t master, struct sockaddr *addr, socklen_t *addrlen)
{ SOCKET slave;
  plsocket *s;

  VALID_SOCKET_RET(master, NULL);

  for(;;)
  {
#ifndef __WINDOWS__
    if ( !wait_socket(master) )
      return NULL;
#endif

    slave = accept(master->socket, addr, addrlen);

    if ( slave == SOCKET_ERROR )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	  return NULL;
#ifdef __WINDOWS__
        if ( !wait_socket(master) )
          return NULL;
#endif
	continue;
      } else
      { nbio_error(GET_ERRNO, TCP_ERRNO);
	return NULL;
      }
    } else
      break;
  }

  s = allocSocket(slave);
  s->flags |= PLSOCK_ACCEPT;
#ifndef __WINDOWS__
  if ( true(s, PLSOCK_NONBLOCK) )
    nbio_setopt(s, TCP_NONBLOCK);
#endif

  return s;
}


int
nbio_listen(nbio_sock_t socket, int backlog)
{ VALID_SOCKET(socket);

  if( listen(socket->socket, backlog) == -1 )
  { nbio_error(GET_ERRNO, TCP_ERRNO);
    return -1;
  }

  set(socket, PLSOCK_LISTEN);

  return 0;
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((intptr_t)(p)))

ssize_t
nbio_read(nbio_sock_t socket, char *buf, size_t bufSize)
{ int n;

  VALID_SOCKET(socket);

  for(;;)
  {
#ifndef __WINDOWS__
    if ( !wait_socket(socket) )
    { errno = EPLEXCEPTION;
      return -1;
    }
#endif

    n = recv(socket->socket, buf, bufSize, 0);

    if ( n == -1 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
        { errno = EPLEXCEPTION;
          return -1;
        }
#ifdef __WINDOWS__
        if ( !wait_socket(socket) )
          return -1;
#endif
        continue;
      }
      nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    }

    break;
  }

  return n;
}


ssize_t
nbio_write(nbio_sock_t socket, char *buf, size_t bufSize)
{ size_t len = bufSize;
  char *str = buf;

  VALID_SOCKET(socket);

  while( len > 0 )
  { int n;

    n = send(socket->socket, str, len, 0);
    if ( n < 0 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	{ errno = EPLEXCEPTION;
	  return -1;
	}
#ifdef __WINDOWS__
        if ( !wait_socket(socket) )
          return -1;
#endif
	continue;
      }
      nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    }
    if ( n < len )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
        return -1;
      }
    }

    len -= n;
    str += n;
  }

  return bufSize;
}


#if defined(__WINDOWS__) && !defined(SHUT_RD)
#define SHUT_RD SD_RECEIVE
#define SHUT_WR SD_SEND
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to [1], we need to run:

 1. Call WSAEventSelect to register for FD_CLOSE notification.
 2. Call shutdown with how=SD_SEND.
 3. When FD_CLOSE received, call the recv or WSARecv until the function
    completes with success and indicates that zero bytes were received.
    If SOCKET_ERROR is returned, then the graceful disconnect is not
    possible.
 4. Call closesocket.

We  operationalize  this   by   using    shutdown()   for   sending   in
nbio_close_output(). A well behaved client  should subsequently read the
input until EOF. That, in the Windows case, will also consume FD_CLOSE.

[1]
https://docs.microsoft.com/en-us/windows/desktop/api/winsock/nf-winsock-shutdown
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_close_input(nbio_sock_t socket)
{ int rc = 0;

  VALID_SOCKET(socket);

  DEBUG(2, Sdprintf("[%d]: nbio_close_input(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, socket->flags));
  if ( true(socket, PLSOCK_INSTREAM) )
  { clear(socket, PLSOCK_INSTREAM);

    socket->input = NULL;
    if ( false(socket, (PLSOCK_INSTREAM|PLSOCK_OUTSTREAM)) )
      rc = closeSocket(socket);

    if ( socket->symbol )
      PL_unregister_atom(socket->symbol);
  }

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) This gives an endpoint not connected   error  for the http:proxy and
http:websocket tests. I'm not sure whether the   test  is broken or not,
but error checking will probably cause many  applications to fail and it
is not that clear that it is an error if the other side closed its input
and we only want to send TCP FIN.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_close_output(nbio_sock_t socket)
{ int rc = 0;

  VALID_SOCKET(socket);

  DEBUG(2, Sdprintf("[%d]: nbio_close_output(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, socket->flags));

  if ( true(socket, PLSOCK_OUTSTREAM) )
  { clear(socket, PLSOCK_OUTSTREAM);

    if ( socket->socket != INVALID_SOCKET )
    { /* if ( (rc = shutdown(socket->socket, SHUT_WR)) )
	nbio_error(GET_ERRNO, TCP_ERRNO);		See (*) */
      shutdown(socket->socket, SHUT_WR);
    }

    socket->output = NULL;
    if ( false(socket, (PLSOCK_INSTREAM|PLSOCK_OUTSTREAM)) )
      rc = (rc + closeSocket(socket)) ? -1 : 0;

    if ( socket->symbol )
      PL_unregister_atom(socket->symbol);
  }

  return rc;
}


		 /*******************************
		 *	    UDP SUPPORT		*
		 *******************************/

ssize_t
nbio_recvfrom(nbio_sock_t socket, void *buf, size_t bufSize, int flags,
	     struct sockaddr *from, socklen_t *fromlen)
{ int n;

  VALID_SOCKET(socket);

  for(;;)
  {
#ifndef __WINDOWS__
    if ( (flags & MSG_DONTWAIT) == 0 && !wait_socket(socket) )
    { errno = EPLEXCEPTION;
      return -1;
    }
#endif

    n = recvfrom(socket->socket, buf, bufSize, flags, from, fromlen);

    if ( n == -1 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
        { errno = EPLEXCEPTION;
          return -1;
        }
#ifdef __WINDOWS__
        if ( !wait_socket(socket) )
          return -1;
#else
        if((flags & MSG_DONTWAIT) != 0)
          break;
#endif
        continue;
      }
      nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    }

    break;
  }

  return n;
}


ssize_t
nbio_sendto(nbio_sock_t socket, void *buf, size_t bufSize, int flags,
	    const struct sockaddr *to, socklen_t tolen)
{ ssize_t n;

  VALID_SOCKET(socket);

  for(;;)
  { n = sendto(socket->socket, buf, bufSize, flags, to, tolen);

    if ( n < 0 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	{ errno = EPLEXCEPTION;
	  return -1;
	}
#ifdef __WINDOWS__
        if ( !wait_socket(socket) )
          return -1;
#endif
        continue;
      }
      nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    }
    break;
  }

  return n;
}
