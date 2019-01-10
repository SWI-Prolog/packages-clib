/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Shared stuff between the normal socket interface and the TIPC one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	  IO-STREAM STUFF	*
		 *******************************/

static ssize_t
tcp_read_handle(void *handle, char *buf, size_t bufSize)
{ nbio_sock_t sock = handle;

  return nbio_read(sock, buf, bufSize);
}


static ssize_t
tcp_write_handle(void *handle, char *buf, size_t bufSize)
{ nbio_sock_t sock = handle;

  return nbio_write(sock, buf, bufSize);
}


static long
tcp_seek_null(void *handle, long offset, int whence)
{ return -1;
}


static int
tcp_close_input_handle(void *handle)
{ nbio_sock_t socket = handle;

  return nbio_close_input(socket);
}


static int
tcp_close_output_handle(void *handle)
{ nbio_sock_t socket = handle;

  return nbio_close_output(socket);
}


static int
tcp_control(void *handle, int action, void *arg)
{ nbio_sock_t socket = handle;

  switch(action)
  {
#ifdef __WINDOWS__
    case SIO_GETFILENO:
      return -1;
    case SIO_GETWINSOCK:
#else
    case SIO_GETFILENO:
#endif
    { SOCKET fd = nbio_fd(socket);
      SOCKET *fdp = arg;
      *fdp = fd;
      return 0;
    }
    case SIO_LASTERROR:
    { const char *s;

      if ( (s=nbio_last_error(socket)) )
      { const char **sp = arg;
	*sp = s;
	return 0;
      }

      return -1;
    }
    case SIO_SETENCODING:
    case SIO_FLUSHOUTPUT:
      return 0;
    default:
      return -1;
  }
}


static IOFUNCTIONS readFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_input_handle,
  tcp_control
};


static IOFUNCTIONS writeFunctions =
{ tcp_read_handle,
  tcp_write_handle,
  tcp_seek_null,
  tcp_close_output_handle,
  tcp_control
};


static foreign_t
pl_open_socket(term_t Socket, term_t Read, term_t Write)
{ IOSTREAM *in, *out;
  nbio_sock_t socket;
  void *handle;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;
  handle = socket;

  in  = Snew(handle, SIO_INPUT|SIO_RECORDPOS|SIO_FBUF,  &readFunctions);
  in->encoding = ENC_OCTET;
  if ( !PL_unify_stream(Read, in) )
    return FALSE;
  nbio_setopt(socket, TCP_INSTREAM, in);

  if ( !(nbio_get_flags(socket) & PLSOCK_LISTEN) )
  { out = Snew(handle, SIO_OUTPUT|SIO_RECORDPOS|SIO_FBUF, &writeFunctions);
    out->encoding = ENC_OCTET;
    if ( !PL_unify_stream(Write, out) )
      return FALSE;
    nbio_setopt(socket, TCP_OUTSTREAM, out);
  }

  return TRUE;
}


static int
get_socket_from_stream(term_t t, IOSTREAM **stp, nbio_sock_t *sp)
{ IOSTREAM *s;
  int rc;

  if ( (rc=PL_get_stream(t, &s, SIO_INPUT|SIO_OUTPUT|SIO_NOERROR)) )
  { if ( s->functions == &readFunctions ||
	 s->functions == &writeFunctions )
    { *sp = s->handle;
      if ( stp )
	*stp = s;
      else
	PL_release_stream(s);
    } else
    { rc = FALSE;
      PL_release_stream(s);
    }
  }

  return rc;
}


static foreign_t
pl_listen(term_t Sock, term_t BackLog)
{ nbio_sock_t socket;
  int backlog;

  if ( !tcp_get_socket(Sock, &socket) )
    return FALSE;

  if ( !PL_get_integer(BackLog, &backlog) )
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, -1, BackLog, "integer");

  if ( nbio_listen(socket, backlog) < 0 )
    return FALSE;

  return TRUE;
}


static foreign_t
pl_close_socket(term_t socket)
{ nbio_sock_t sock;

  if ( !tcp_get_socket(socket, &sock) )
    return FALSE;

  if ( nbio_closesocket(sock) < 0 )
    return nbio_error(errno, TCP_ERRNO);;

  return TRUE;
}
