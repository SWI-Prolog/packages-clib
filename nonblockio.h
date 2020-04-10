/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, University of Amsterdam
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

#ifndef H_NONBLOCKIO_INCLUDED
#define H_NONBLOCKIO_INCLUDED


		 /*******************************
		 *     GET REQUIRED HEADERS	*
		 *******************************/

#include <config.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#ifdef __WINDOWS__

#include <io.h>
#include <winsock2.h>
#include <ws2tcpip.h>

#else /*__WINDOWS__*/

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif
#ifdef HAVE_H_ERRNO
extern int h_errno;
#else
#define h_errno errno
#endif
#ifndef HAVE_SOCKLEN_T
#define socklen_t size_t
#endif

typedef int SOCKET;

#endif /*__WINDOWS__*/

typedef enum
{ TCP_ERRNO,
  TCP_HERRNO,
  TCP_GAI_ERRNO
} nbio_error_map;

typedef enum				/* nbio_setopt() commands */
{ TCP_NONBLOCK,
  TCP_REUSEADDR,
  TCP_NO_DELAY,
  TCP_DISPATCH,
  TCP_INSTREAM,
  TCP_OUTSTREAM,
  UDP_BROADCAST,
  SCK_BINDTODEVICE,
  NBIO_END,
  TCP_SNDBUF
} nbio_option;

typedef enum
{ REQ_NONE = 0,				/* no request pending */
  REQ_ACCEPT,
  REQ_CONNECT,
  REQ_READ,
  REQ_WRITE,
  REQ_RECVFROM,
  REQ_SENDTO
#ifdef __WINDOWS__
  , REQ_DEALLOCATE
#endif
} nbio_request;

typedef struct _plsocket *nbio_sock_t;	/* socket handle */

#define PLSOCK_INSTREAM	  0x0001	/* tcp_open_socket/3 bound input */
#define PLSOCK_OUTSTREAM  0x0002	/* tcp_open_socket/3 bound output */
#define PLSOCK_BIND	  0x0004	/* What have we done? */
#define PLSOCK_LISTEN	  0x0008
#define PLSOCK_CONNECT	  0x0010
#define PLSOCK_ACCEPT	  0x0020	/* Set on accepted sockets */
#define PLSOCK_NONBLOCK	  0x0040	/* Set to non-blocking mode */
#define PLSOCK_DISPATCH   0x0080	/* do not dispatch events */
#define PLSOCK_CLOSE_SEEN 0x0100	/* FD_CLOSE seen */
#define PLSOCK_EOF_SEEN   0x0200	/* Seen end-of-file */
#define PLSOCK_WAITING	  0x0400	/* using nbio_wait() */
#define PLSOCK_VIRGIN	  0x0800	/* created, but not opened */
#define PLSOCK_SHUTDOWN	  0x1000	/* shutdown, but not freed */

		 /*******************************
		 *	 BASIC FUNCTIONS	*
		 *******************************/

extern void	nbio_set_symbol(nbio_sock_t socket, atom_t symbol);
extern int	is_nbio_socket(nbio_sock_t socket);
extern void	freeSocket(nbio_sock_t s);

extern int	nbio_init(const char *module);
extern int	nbio_cleanup(void);
extern int	nbio_debug(int level);

extern nbio_sock_t nbio_socket(int domain, int type, int protocol);
extern int	nbio_connect(nbio_sock_t socket,
			     const struct sockaddr *serv_addr,
			     size_t addrlen);
extern int	nbio_bind(nbio_sock_t socket,
			  struct sockaddr *my_addr,
			  size_t addrlen);
extern int	nbio_listen(nbio_sock_t socket, int backlog);
extern nbio_sock_t
		nbio_accept(nbio_sock_t master,
			    struct sockaddr *addr,
			    socklen_t *addrlen);

extern ssize_t	nbio_read(nbio_sock_t socket, char *buf, size_t bufSize);
extern ssize_t	nbio_write(nbio_sock_t socket, char *buf, size_t bufSize);
extern int	nbio_closesocket(nbio_sock_t socket);
extern int	nbio_close_input(nbio_sock_t socket);
extern int	nbio_close_output(nbio_sock_t socket);
extern ssize_t	nbio_recvfrom(nbio_sock_t socket, void *buf, size_t bufSize,
			      int flags,
			      struct sockaddr *from, socklen_t *fromlen);
extern ssize_t	nbio_sendto(nbio_sock_t socket, void *buf, size_t bufSize,
			    int flags,
			    const struct sockaddr *to, socklen_t tolen);

extern int	nbio_wait(nbio_sock_t socket, nbio_request);
extern SOCKET	nbio_fd(nbio_sock_t socket);
extern int	nbio_domain(nbio_sock_t socket);

extern int	nbio_unify_ip4(term_t ip4, unsigned long hip);
extern int	nbio_get_ip(term_t ip4, struct in_addr *ip);

extern int	nbio_error(int code, nbio_error_map map);
extern const char*
		nbio_last_error(nbio_sock_t socket);
extern int	nbio_setopt(nbio_sock_t socket, nbio_option opt, ...);
extern int	nbio_get_flags(nbio_sock_t socket);


		 /*******************************
		 *	    CONVERSION		*
		 *******************************/

extern int	nbio_get_sockaddr(term_t Address,
				  struct sockaddr_in *addr,
				  term_t *varport);
extern int	nbio_get_ip4(term_t ip4, struct in_addr *ip);

#endif /*H_NONBLOCKIO_INCLUDED*/
