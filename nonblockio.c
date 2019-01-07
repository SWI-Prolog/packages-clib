/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2017, University of Amsterdam
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

Waiting

	nbio_select()

Alternative to nbio_read() and nbio_write(), the application program may
call  the  low-level  I/O  routines  in    non-blocking  mode  and  call
nbio_wait(int socket, nbio_request request). This  function returns 0 if
it thinks the call might  now  succeed   and  -1  if  an error occurred,
leaving the exception context in Prolog. On  receiving -1, the user must
return an I/O error as soon as possible.


Windows issues
--------------

Winsock is hard to handle in blocking   mode  without blocking the whole
lot, notably (timeout) signals. We  therefore   have  a  seperate thread
dealing with I/O and  operating   the  sockets  through WSAAsyncSelect()
generated events. Requests are registered   with the plsocket structure,
handled and handled in the socket thread.  Upon completion, a message is
sent back to the waiting thread.

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

#ifdef _REENTRANT
#if __WINDOWS__
static CRITICAL_SECTION mutex;

#define LOCK()			EnterCriticalSection(&mutex)
#define UNLOCK()		LeaveCriticalSection(&mutex)
#define LOCK_FREE()		(void)0
#define UNLOCK_FREE()		(void)0
#define INITLOCK()		InitializeCriticalSection(&mutex)
#define LOCK_SOCKET(s)		(void)0
#define UNLOCK_SOCKET(s)	(void)0
#define INIT_SOCKET_LOCK(s)	(void)0
#define FREE_SOCKET_LOCK(s)	(void)0
#else /*__WINDOWS__*/
#include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

#define LOCK()			pthread_mutex_lock(&mutex)
#define UNLOCK()		pthread_mutex_unlock(&mutex)
#define LOCK_FREE()		(void)0
#define UNLOCK_FREE()		(void)0
#define INITLOCK()		(void)0
#define FREE_SOCKET_LOCK(s)	(void)0
#endif /*__WINDOWS__*/
#else /* _REENTRANT */
#define LOCK()			(void)0
#define UNLOCK()		(void)0
#define LOCK_FREE()		(void)0
#define UNLOCK_FREE()		(void)0
#define INITLOCK()		(void)0
#define FREE_SOCKET_LOCK(s)	(void)0
#if __WINDOWS__
#define LOCK_SOCKET(s)		(void)0
#define UNLOCK_SOCKET(s)	(void)0
#define INIT_SOCKET_LOCK(s)	(void)0
#endif
#endif /* _REENTRANT */

#define set(s, f)   ((s)->flags |= (f))
#define clear(s, f) ((s)->flags &= ~(f))
#define true(s, f)  ((s)->flags & (f))
#define false(s, f) (!true(s, f))

#define PLSOCK_MAGIC 0x38da3f2c

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: We must lock  the  structure   to  avoid  freeSocket() called from
Prolog deleting the socket while there are   still  pending events on it
that are concurrently executed in the socket thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct _plsocket
{ int		    magic;		/* PLSOCK_MAGIC */
  nbio_sock_t	    id;			/* Integer id */
  SOCKET	    socket;		/* The OS socket */
  int		    flags;		/* Misc flags */
  IOSTREAM *	    input;		/* input stream */
  IOSTREAM *	    output;		/* output stream */
#ifdef __WINDOWS__
  WSAEVENT          event;		/* Winsock event */
#endif
} plsocket;

static plsocket *allocSocket(SOCKET socket);
#ifdef __WINDOWS__
static const char *WinSockError(unsigned long eno);
#endif
static int need_retry(int error);
static int freeSocket(plsocket *s);

#ifdef O_DEBUG
static int debugging;

NBIO_EXPORT(int)
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

#endif

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

#endif

#define F_SETFL		0
#define O_NONBLOCK	0

static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

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
	  rval = ioctlsocket(s->socket, FIONBIO, &non_block);
	  if ( rval )
	  { s->flags |= PLSOCK_NONBLOCK;
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
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  return wait_socket(s) ? 0 : -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
nbio_select() selects using a set of socket streams.

NOTE: The Windows versions uses our   nbio_sock_t abstraction, while the
other version uses  the  raw  Unix   file  descriptors  referencing  the
underlying socket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
nbio_select(int n,
	    fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
	    struct timeval *timeout)
{ SOCKET sockets[WSA_MAXIMUM_WAIT_EVENTS];
  WSAEVENT events[WSA_MAXIMUM_WAIT_EVENTS];
  int fds[WSA_MAXIMUM_WAIT_EVENTS];
  int i, index;
  int count = 0;
  int n_ready = 0;
  DWORD t_end;

  if ( readfds )
  { for(i=0; i<n; i++)
    { if ( FD_ISSET(i, readfds) )
      { plsocket *s = nbio_to_plsocket(i);

	if ( s )
        { WSANETWORKEVENTS network_events;
          WSAEVENT event = WSACreateEvent();
          int event_type = (s->flags & PLSOCK_LISTEN) ? FD_ACCEPT : FD_READ;

          WSAEventSelect(s->socket, event, event_type);
          fds[count] = i;
          sockets[count] = s->socket;
          events[count] = event;
          count++;

          if ( WSAEnumNetworkEvents(s->socket, NULL, &network_events) == SOCKET_ERROR )
          { nbio_error(GET_ERRNO, TCP_ERRNO);
            return -1;
          }
          DEBUG(2,
                if ( network_events.lNetworkEvents )
                { char *nm = event_name(network_events.lNetworkEvents);
                  Sdprintf("WM_SOCKET on %d: ev=(%s)\n", s->socket, nm);
	          free(nm);
                });
          if ( (network_events.lNetworkEvents & FD_ACCEPT) &&
               !network_events.iErrorCode[FD_ACCEPT_BIT] )
          { n_ready++;
          } else if ( (network_events.lNetworkEvents & FD_READ) &&
		      !network_events.iErrorCode[FD_READ_BIT] )
          { n_ready++;
          } else
          { FD_CLR(i, readfds);
          }
        } else
        { DEBUG(2, Sdprintf("nbio_select(): no socket for %d\n", i));
          FD_CLR(i, readfds);
	}
      }
    }
  }

  if ( writefds )
    return -1;				/* not yet implemented */
  if ( exceptfds )
    return -1;				/* idem (might never be) */

  if ( timeout )
  { t_end = GetTickCount();
    t_end += timeout->tv_sec*1000;
    t_end += timeout->tv_usec/1000;
  }

  if ( n_ready )
  { return n_ready;
  }

  FD_ZERO(readfds);

  for(;;)
  { DWORD t, msecs;

    DEBUG(2, Sdprintf("waiting on %d sockets\n", count));

    if ( timeout )
    { t = GetTickCount();
      msecs = t_end - t;
      if ( msecs < 0 )
        msecs = -msecs; /* wrapped around */
    } else
    { msecs = INFINITE;
    }

    index = MsgWaitForMultipleObjects(count, events, FALSE, msecs, QS_ALLINPUT);

    if ( index == WAIT_TIMEOUT )
    { DEBUG(2, Sdprintf("nbio_select() timed out\n"));
      return 0;
    }

    if ( index == WAIT_FAILED )
    { nbio_error(GET_ERRNO, TCP_ERRNO);
      return -1;
    } else if ( index < WAIT_OBJECT_0+count ) /* socket event */
    { WSANETWORKEVENTS network_events;
      if ( WSAEnumNetworkEvents(sockets[index-WSA_WAIT_EVENT_0], NULL, &network_events) == SOCKET_ERROR )
      { nbio_error(GET_ERRNO, TCP_ERRNO);
        return -1;
      }
      DEBUG(2,
            { char *nm = event_name(network_events.lNetworkEvents);
              Sdprintf("WM_SOCKET on %d: ev=(%s)\n", sockets[index-WSA_WAIT_EVENT_0], nm);
	      free(nm);
            });
      if ( (network_events.lNetworkEvents & FD_ACCEPT) &&
           !network_events.iErrorCode[FD_ACCEPT_BIT] )
      { FD_SET(fds[index-WSA_WAIT_EVENT_0], readfds);
        return 1;
      } else if ( (network_events.lNetworkEvents & FD_READ) &&
		  !network_events.iErrorCode[FD_READ_BIT] )
      { FD_SET(fds[index-WSA_WAIT_EVENT_0], readfds);
        return 1;
      }
    } else if ( index == WAIT_OBJECT_0+count ) /* message event */
    { MSG msg;
      DEBUG(2, Sdprintf("nbio_select() interrupted\n"));
      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
        DispatchMessage(&msg);

        if ( PL_handle_signals() < 0 )
          return -1;
        continue;
      }
    }
  }

  return -1;
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
{ plsocket *s;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  return wait_socket(s) ? 0 : -1;
}


static int
nbio_fcntl(nbio_sock_t socket, int op, int arg)
{ plsocket *s;
  int rc;

  if ( !(s=nbio_to_plsocket(socket)) )
    return -1;

  rc = fcntl(s->socket, op, arg);

  if ( rc == 0 )
  { if ( op == F_SETFL && arg == O_NONBLOCK )
      s->flags |= PLSOCK_NONBLOCK;
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

static plsocket **sockets = NULL;	/* id --> plsocket* */
static size_t	socks_count = 0;	/* #registered sockets */
static size_t	socks_allocated = 0;	/* #allocated entries */
static int initialised = FALSE;		/* Windows only */


static plsocket *
nbio_to_plsocket_nolock(nbio_sock_t socket)
{ plsocket *p;

  if ( socket < 0 || (size_t)socket >= socks_allocated )
  { errno = EINVAL;
    return NULL;
  }

  p = sockets[socket];

  if ( !p || p->magic != PLSOCK_MAGIC )
  { DEBUG(1, Sdprintf("Invalid NBIO socket: %d\n", socket));
    errno = EINVAL;
    return NULL;
  }

  return p;
}


SOCKET
plsocket_handle(plsocket_ptr pls)
{ return pls->socket;
}


static plsocket *
nbio_to_plsocket_raw(nbio_sock_t socket)
{ plsocket *s;

  LOCK();
  s = nbio_to_plsocket_nolock(socket);
  UNLOCK();

  return s;
}


plsocket *
nbio_to_plsocket(nbio_sock_t socket)
{ plsocket *p;

  if ( !(p=nbio_to_plsocket_raw(socket)) )
    return NULL;

  return p;
}


NBIO_EXPORT(SOCKET)
nbio_fd(nbio_sock_t socket)
{ plsocket *p;

  if ( !(p=nbio_to_plsocket_nolock(socket)) )
    return -1;

  return p->socket;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a wrapper for an OS  socket.   The  wrapper  is allocated in an
array of pointers, to keep small  integer   identifiers  we can use with
FD_SET, etc. for implementing a compatible nbio_select().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static plsocket *
allocSocket(SOCKET socket)
{ plsocket *p = NULL;
  size_t i;

  LOCK();
  if ( socks_count+1 > socks_allocated )
  { if ( socks_allocated )
    { size_t newa = socks_allocated*2;

      sockets = PL_realloc(sockets, sizeof(plsocket*)*newa);
      for(i=socks_allocated; i<newa; i++)
	sockets[i] = NULL;
      socks_allocated = newa;
    } else
    { socks_allocated = 32;
      sockets = PL_malloc(sizeof(plsocket*)*socks_allocated);
      memset(sockets, 0, sizeof(plsocket*)*socks_allocated);
    }
  }

  for(i=0; i<socks_allocated; i++)
  { if ( (p=sockets[i]) == NULL )
    { sockets[i] = p = PL_malloc(sizeof(*p));
      socks_count++;
      break;
    }
  }
  UNLOCK();

  assert(i<socks_allocated);

  memset(p, 0, sizeof(*p));
  p->id     = (int)i;			/* place in the array */
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

  DEBUG(2, Sdprintf("[%d]: allocSocket(%d): bound to %d\n",
		    PL_thread_self(), socket, p->id));

  return p;
}


static int
freeSocket(plsocket *s)
{ int rval;
  nbio_sock_t socket;
  SOCKET sock;

  DEBUG(2, Sdprintf("Closing %p (%d)\n", s, s ? s->id : 0));
  if ( !s || s->magic != PLSOCK_MAGIC )
  { DEBUG(1, Sdprintf("OOPS: freeSocket(%p) s->magic = %ld\n", s, s ? s->magic : 0));
    errno = EINVAL;
    return -1;
  }

  LOCK_FREE();
  LOCK();
  sockets[s->id] = NULL;
  socks_count--;
  UNLOCK();

  sock = s->socket;
  socket = s->id;
  s->magic = 0;
  FREE_SOCKET_LOCK(s);
  PL_free(s);
  UNLOCK_FREE();

  if ( sock != INVALID_SOCKET )
  { again:
    if ( (rval=closesocket(sock)) == SOCKET_ERROR )
    { if ( errno == EINTR )
	goto again;
    }
    DEBUG(2, Sdprintf("freeSocket(%d=%d): closesocket() returned %d\n",
		      socket, (int)sock, rval));
  } else
  { DEBUG(2, Sdprintf("freeSocket(%d=%d): already closed\n",
		      socket, (int)sock));
    rval = 0;				/* already closed.  Use s->error? */
  }

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

#ifdef HAVE_H_ERRNO
typedef struct
{ int code;
  const char *string;
} error_codes;

#ifndef HAVE_HSTRERROR
#define hstrerror my_hstrerror

static error_codes h_errno_codes[] = {
#ifdef HOST_NOT_FOUND
    { HOST_NOT_FOUND, "Host not found" },
#endif
#ifdef TRY_AGAIN
    { TRY_AGAIN, "Try Again" },
#endif
#ifdef NO_RECOVERY
    { NO_RECOVERY, "No Recovery" },
#endif
#ifdef NO_DATA
    { NO_DATA, "No Data" },
#endif
#ifdef NO_ADDRESS
    { NO_ADDRESS, "No Address" },
#endif
    { 0, NULL }
};

static const char *
hstrerror(int code)
{ static char msgbuf[100];
  const error_codes *map = h_errno_codes;
  const char *msg;

  while( map->code && map->code != code )
    map++;

  if ( map->code )
  { msg = map->string;
  } else
  { sprintf(msgbuf, "Unknown error %d", code);
    msg = msgbuf;
  }

  return msg;
}
#endif /*HAVE_HSTRERROR*/

#else /*HAVE_H_ERRNO*/
#define hstrerror my_hstrerror

static const char *
hstrerror(int code)
{ static char msgbuf[100];

  sprintf(msgbuf, "Unknown error %d", code);
  msg = msgbuf;

  return msg;
}

#endif /*HAVE_H_ERRNO*/

int
nbio_error(int code, nbio_error_map mapid)
{ const char *msg;
  term_t except = PL_new_term_ref();

  if ( code == EPLEXCEPTION )
    return FALSE;

#ifdef __WINDOWS__
  msg = WinSockError(code);
#else
  switch( mapid )
  { case TCP_HERRNO:
      msg = strerror(code);
      break;
    case TCP_ERRNO:
      msg = hstrerror(code);
      break;
    case TCP_GAI_ERRNO:
      msg = gai_strerror(code);
      break;
    default:
      assert(0);
      msg = NULL;
      break;
  }
#endif

  return ( PL_unify_term(except,
			 CompoundArg("error", 2),
			   CompoundArg("socket_error", 1),
			     AtomArg(msg),
			   PL_VARIABLE) &&
	   PL_raise_exception(except)
	 );
}


const char *
nbio_last_error(nbio_sock_t socket)
{ return NULL;
}


		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

NBIO_EXPORT(int)
nbio_init(const char *module)
{ if ( initialised )			/* called from install handlers, which */
    return TRUE;			/* are serialized by the compiler mutex */
  initialised = TRUE;

  INITLOCK();

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


NBIO_EXPORT(int)
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

NBIO_EXPORT(nbio_sock_t)
nbio_socket(int domain, int type, int protocol)
{ SOCKET sock;
  plsocket *s;

  assert(initialised);

  if ( (sock = socket(domain, type , protocol)) == INVALID_SOCKET )
  { nbio_error(GET_ERRNO, TCP_ERRNO);
    return -1;
  }
  if ( !(s=allocSocket(sock)) )		/* register it */
  { closesocket(sock);
    return -1;
  }

  return s->id;
}


NBIO_EXPORT(int)
nbio_closesocket(nbio_sock_t socket)
{ plsocket *s;
  int rc = 0;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
  { DEBUG(1, Sdprintf("nbio_closesocket(%d): no plsocket\n", socket));
    return -1;
  }

  s->flags &= ~PLSOCK_VIRGIN;

  if ( true(s, PLSOCK_OUTSTREAM|PLSOCK_INSTREAM) )
  { int flags = s->flags;		/* may drop out! */

    if ( flags & PLSOCK_INSTREAM )
    { assert(s->input);
      if ( Slock(s->input) == 0 )
	rc += Sclose(s->input);
      else
	rc--;
    }
    if ( flags & PLSOCK_OUTSTREAM )
    { assert(s->output);
      if ( Slock(s->output) == 0 )
	rc += Sclose(s->output);
      else
	rc--;
    }
  } else
  { rc = 0;
#ifdef __WINDOWS__
    shutdown(s->socket, SD_BOTH);
#endif
    freeSocket(s);
  }

  return rc;
}

NBIO_EXPORT(int)
nbio_setopt(nbio_sock_t socket, nbio_option opt, ...)
{ plsocket *s;
  va_list args;
  int rc;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  va_start(args, opt);

  switch(opt)
  { case TCP_NONBLOCK:
      rc = nbio_fcntl(socket, F_SETFL, O_NONBLOCK);
      break;
    case TCP_REUSEADDR:
    { int val = va_arg(args, int);

      if( setsockopt(s->socket, SOL_SOCKET, SO_REUSEADDR,
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
      if ( setsockopt(s->socket, SOL_SOCKET, SO_BINDTODEVICE,
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
      if ( setsockopt(s->socket, IPPROTO_TCP, TCP_NODELAY,
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

      if ( setsockopt(s->socket, SOL_SOCKET, SO_BROADCAST,
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
	set(s, PLSOCK_DISPATCH);
      else
	clear(s, PLSOCK_DISPATCH);

      rc = 0;

      break;
    }
    case TCP_INSTREAM:
    { IOSTREAM *in = va_arg(args, IOSTREAM*);

      s->flags |= PLSOCK_INSTREAM;
      s->flags &= ~PLSOCK_VIRGIN;
      s->input = in;

      rc = 0;

      break;
    }
    case TCP_OUTSTREAM:
    { IOSTREAM *out = va_arg(args, IOSTREAM*);

      s->flags |= PLSOCK_OUTSTREAM;
      s->flags &= ~PLSOCK_VIRGIN;
      s->output = out;

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
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  return s->flags;
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
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

#ifdef __WINDOWS__
  if ( bind(s->socket, my_addr, (int)addrlen) )
#else
  if ( bind(s->socket, my_addr, addrlen) )
#endif
  {
    nbio_error(GET_ERRNO, TCP_ERRNO);
    return -1;
  }

  s->flags |= PLSOCK_BIND;

  return 0;
}


int
nbio_connect(nbio_sock_t socket,
	     const struct sockaddr *serv_addr,
	     size_t addrlen)
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  for(;;)
  { if ( connect(s->socket, serv_addr, addrlen) )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	  return -1;
#ifdef __WINDOWS__
        if ( !wait_socket(s) )
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

  s->flags |= PLSOCK_CONNECT;

  return 0;
}


nbio_sock_t
nbio_accept(nbio_sock_t master, struct sockaddr *addr, socklen_t *addrlen)
{ SOCKET slave;
  plsocket *m, *s;

  if ( !(m = nbio_to_plsocket(master)) )
    return -1;

  for(;;)
  {
#ifndef __WINDOWS__
    if ( !wait_socket(m) )
      return -1;
#endif

    slave = accept(m->socket, addr, addrlen);

    if ( slave == SOCKET_ERROR )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	  return -1;
#ifdef __WINDOWS__
        if ( !wait_socket(m) )
          return -1;
#endif
	continue;
      } else
      { nbio_error(GET_ERRNO, TCP_ERRNO);
	return -1;
      }
    } else
      break;
  }

  s = allocSocket(slave);
  s->flags |= PLSOCK_ACCEPT;
#ifndef __WINDOWS__
  if ( true(s, PLSOCK_NONBLOCK) )
    nbio_setopt(slave, TCP_NONBLOCK);
#endif

  return s->id;
}


int
nbio_listen(nbio_sock_t socket, int backlog)
{ plsocket *s;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  if( listen(s->socket, backlog) == -1 )
  { nbio_error(GET_ERRNO, TCP_ERRNO);
    return -1;
  }

  s->flags |= PLSOCK_LISTEN;

  return 0;
}


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

#define fdFromHandle(p) ((int)((intptr_t)(p)))

ssize_t
nbio_read(int socket, char *buf, size_t bufSize)
{ plsocket *s;
  int n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  for(;;)
  {
#ifndef __WINDOWS__
    if ( !wait_socket(s) )
    { errno = EPLEXCEPTION;
      return -1;
    }
#endif

    n = recv(s->socket, buf, bufSize, 0);

    if ( n == -1 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
        { errno = EPLEXCEPTION;
          return -1;
        }
#ifdef __WINDOWS__
        if ( !wait_socket(s) )
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
{ plsocket *s;
  size_t len = bufSize;
  char *str = buf;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  while( len > 0 )
  { int n;

    n = send(s->socket, str, len, 0);
    if ( n < 0 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	{ errno = EPLEXCEPTION;
	  return -1;
	}
#ifdef __WINDOWS__
        if ( !wait_socket(s) )
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


int
nbio_close_input(nbio_sock_t socket)
{ int rc = 0;
  plsocket *s;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
    return -1;

  DEBUG(2, Sdprintf("[%d]: nbio_close_input(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, s->flags));
  s->flags &= ~PLSOCK_INSTREAM;

  s->input = NULL;
  if ( !(s->flags & (PLSOCK_INSTREAM|PLSOCK_OUTSTREAM)) )
  {
#ifdef __WINDOWS__
    return shutdown(s->socket, SD_BOTH);
#endif
    return freeSocket(s);
  }

  return rc;
}


int
nbio_close_output(nbio_sock_t socket)
{ int rc = 0;
  plsocket *s;

  if ( !(s = nbio_to_plsocket_raw(socket)) )
    return -1;

  DEBUG(2, Sdprintf("[%d]: nbio_close_output(%d, flags=0x%x)\n",
		    PL_thread_self(), socket, s->flags));
  s->flags &= ~PLSOCK_OUTSTREAM;

  s->output = NULL;
  if ( !(s->flags & (PLSOCK_INSTREAM|PLSOCK_OUTSTREAM)) )
  {
#ifdef __WINDOWS__
    return shutdown(s->socket, SD_BOTH);
#endif
    return freeSocket(s);
  }

  return rc;
}


		 /*******************************
		 *	    UDP SUPPORT		*
		 *******************************/

ssize_t
nbio_recvfrom(int socket, void *buf, size_t bufSize, int flags,
	     struct sockaddr *from, socklen_t *fromlen)
{ plsocket *s;
  int n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  for(;;)
  {
#ifndef __WINDOWS__
    if ( (flags & MSG_DONTWAIT) == 0 && !wait_socket(s) )
    { errno = EPLEXCEPTION;
      return -1;
    }
#endif

    n = recvfrom(s->socket, buf, bufSize, flags, from, fromlen);

    if ( n == -1 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
        { errno = EPLEXCEPTION;
          return -1;
        }
#ifdef __WINDOWS__
        if ( !wait_socket(s) )
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
{ plsocket *s;
  ssize_t n;

  if ( !(s = nbio_to_plsocket(socket)) )
    return -1;

  for(;;)
  { n = sendto(s->socket, buf, bufSize, flags, to, tolen);

    if ( n < 0 )
    { if ( need_retry(GET_ERRNO) )
      { if ( PL_handle_signals() < 0 )
	{ errno = EPLEXCEPTION;
	  return -1;
	}
#ifdef __WINDOWS__
        if ( !wait_socket(s) )
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
