\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{plpage}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}				% Output directory
\htmlmainfile{clib}			% Main document file
\bodycolor{white}			% Page colour

\renewcommand{\runningtitle}{CLIB -- System interfaces}

\begin{document}

\title{SWI-Prolog C-library}
\author{Jan Wielemaker \\
	VU University of Amsterdam \\
	The Netherlands \\
	E-mail: \email{J.Wielemaker@vu.nl}}

\maketitle

\begin{abstract}
This document describes commonly used foreign language extensions to
\href{http://www.swi-prolog.org}{SWI-Prolog}
distributed as a package known under the name {\em clib}. The package
defines a number of Prolog libraries with accompagnying foreign
libraries.

On Windows systems, the \pllib{unix} library can only be used if the
whole SWI-Prolog suite is compiled using
\href{http://www.cygwin.com}{Cygwin}. The other libraries have been
ported to native Windows.
\end{abstract}

\vfill

\pagebreak
\tableofcontents

\vfill
\vfill

\newpage

\section{Introduction}
\label{sec:clib-intro}

Many useful facilities offered by one or more of the operating systems
supported by SWI-Prolog are not supported by the SWI-Prolog kernel
distribution.  Including these would enlarge the {\em footprint} and
complicate portability matters while supporting only a limited part of
the user-community.

This document describes \pllib{unix} to deal with the Unix process API,
\pllib{socket} to deal with inet-domain TCP and UDP sockets, \pllib{cgi}
to deal with getting CGI form-data if SWI-Prolog is used as a CGI
scripting language, \pllib{crypt} to provide password encryption and
verification, \pllib{sha} providing cryptographic hash functions and
\pllib{memfile} providing in-memorty pseudo files.

\input{process.tex}
\input{filesex.tex}
\InputIfFileExists{uid.tex}{}{}
\InputIfFileExists{syslog.tex}{}{}
\input{socket.tex}

\section{The stream_pool library}
\label{sec:stream-pools}

The \pllib{streampool} library dispatches input from multiple streams
based on wait_for_input/3.  It is part of the clib package as it is used
most of the time together with the \pllib{socket} library.  On non-Unix
systems it often can only be used with socket streams.

With SWI-Prolog 5.1.x, multi-threading often provides a good alternative
to using this library. In this schema one thread watches the listening
socket waiting for connections and either creates a thread per
connection or processes the accepted connections with a pool of
\jargon{worker threads}. The library \pllib{http/thread_httpd} provides
an example realising a mult-threaded HTTP server.

\begin{description}
    \predicate{add_stream_to_pool}{2}{+Stream, :Goal}
Add \arg{Stream}, which must be an input stream and ---on non-unix
systems--- connected to a socket to the pool.  If input is available
on \arg{Stream}, \arg{Goal} is called.

    \predicate{delete_stream_from_pool}{1}{+Stream}
Delete the given stream from the pool.  Succeeds, even if \arg{Stream}
is no member of the pool.  If \arg{Stream} is unbound the entire pool
is emtied but unlike close_stream_pool/0 the streams are not closed.

    \predicate{close_stream_pool}{0}{}
Empty the pool, closing all streams that are part of it.

    \predicate{dispatch_stream_pool}{1}{+TimeOut}
Wait for maximum of \arg{TimeOut} for input on any of the streams in
the pool.  If there is input, call the \arg{Goal} associated with
add_stream_to_pool/2.  If \arg{Goal} fails or raises an exception a
message is printed.  \arg{TimeOut} is described with wait_for_input/3.

If \arg{Goal} is called, there is \emph{some} input on the associated
stream.  \arg{Goal} must be careful not to block as this will block
the entire pool.%
    \footnote{This is hard to achieve at the moment as none of the
	      Prolog read-commands provide for a timeout.}

    \predicate{stream_pool_main_loop}{0}{}
Calls dispatch_stream_pool/1 in a loop until the pool is empty.
\end{description}

Below is a very simple example that reads the first line of input and
echos it back.

\begin{code}
:- use_module(library(streampool)).

server(Port) :-
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	tcp_open_socket(Socket, In, _Out),
	add_stream_to_pool(In, accept(Socket)),
	stream_pool_main_loop.

accept(Socket) :-
	tcp_accept(Socket, Slave, Peer),
	tcp_open_socket(Slave, In, Out),
	add_stream_to_pool(In, client(In, Out, Peer)).

client(In, Out, _Peer) :-
	read_line_to_codes(In, Command),
	close(In),
	format(Out, 'Please to meet you: ~s~n', [Command]),
	close(Out),
	delete_stream_from_pool(In).
\end{code}


\input{uri.tex}

\section{CGI Support library}
\label{sec:cgi}

This is currently a very simple library, providing support for obtaining
the form-data for a CGI script:

\begin{description}
    \predicate{cgi_get_form}{1}{-Form}
Decodes standard input and the environment variables to obtain a list
of arguments passed to the CGI script.  This predicate both deals with
the CGI {\bf GET} method as well as the {\bf POST} method.  If the
data cannot be obtained, an \const{existence_error} exception is
raised.
\end{description}

Below is a very simple CGI script that prints the passed parameters.
To test it, compile this program using the command below, copy it to
your cgi-bin directory (or make it otherwise known as a CGI-script) and
make the query \verb$http://myhost.mydomain/cgi-bin/cgidemo?hello=world$

\begin{code}
% pl -o cgidemo --goal=main --toplevel=halt -c cgidemo.pl
\end{code}

\begin{code}
:- use_module(library(cgi)).

main :-
	set_stream(current_output, encoding(utf8)),
	cgi_get_form(Arguments),
	format('Content-type: text/html; charset=UTF-8~n~n', []),
	format('<html>~n', []),
	format('<head>~n', []),
	format('<title>Simple SWI-Prolog CGI script</title>~n', []),
	format('</head>~n~n', []),
	format('<body>~n', []),
	format('<p>', []),
	print_args(Arguments),
	format('</body>~n</html>~n', []).

print_args([]).
print_args([A0|T]) :-
	A0 =.. [Name, Value],
	format('<b>~w</b>=<em>~w</em><br>~n', [Name, Value]),
	print_args(T).
\end{code}

\subsection{Some considerations}
\label{sec:cgi-considerations}

Printing an HTML document using format/2 is not a neat way of producing
HTML because it is vulnerable to required escape sequences. A high-level
alternative is provided by \pllib{http/html_write} from the HTTP
library.

The startup-time of Prolog is relatively long, in particular if the
program is large. In many cases it is much better to use the SWI-Prolog
HTTP server library and make the main web-server relay requests to the
SWI-Prolog webserver. See the SWI-Prolog
\href{http://www.swi-prolog.org/pldoc/package/http.html}{HTTP package}
for details.


The CGI standard is unclear about handling Unicode data. The above two
declarations ensure the CGI script will send all data in UTF-8 and thus
provide full support of Unicode. It is assumed that browsers generally
send form-data using the same encoding as the page in which the form
appears, UTF-8 or ISO Latin-1. The current version of cgi_get_form/1
assumes the CGI data is in UTF-8.

\section{Password encryption library}
\label{sec:crypt}

The \pllib{crypt} library defines crypt/2 for encrypting and testing
passwords.  The clib package also provides crytographic hashes as
described in \secref{sha}

\begin{description}
    \predicate{crypt}{2}{+Plain, ?Encrypted}
This predicate can be used in three modes. To test whether a password
matches an encrypted version thereof, simply run with both arguments
fully instantiated. To generate a default encrypted version of
\arg{Plain}, run with unbound \arg{Encrypted} and this argument is
unified to a list of character codes holding an encrypted version.

The library supports two encryption formats: traditional Unix
DES-hashes\footnote{On non-Unix systems, crypt() is provided by the
NetBSD library. The license header is added at the end of this
document.} and FreeBSD compatible MD5 hashes (all platforms). MD5 hashes
start with the magic sequence \verb|$1$|, followed by an up to 8
character \jargon{salt}. DES hashes start with a 2 character
\jargon{salt}. Note that a DES hash considers only the first 8
characters. The MD5 considers the whole string.

Salt and algorithm can be forced by instantiating the start of
\arg{Encrypted} with it.  This is typically used to force MD5 hashes:

\begin{code}
?- phrase("$1$", E, _),
   crypt("My password", E),
   format('~s~n', [E]).

$1$qdaDeDZn$ZUxSQEESEHIDCHPNc3fxZ1
\end{code}

\arg{Encrypted} is always a list of ASCII character codes. \arg{Plain}
only supports ISO-Latin-1 passwords in the current implementation.

\arg{Plain} is either an atom, SWI-Prolog string, list of characters
or list of character-codes.  It is not advised to use atoms, as this
implies the password will be available from the Prolog heap as a
defined atom.

\textbf{NOTE}: crypt/2 provides an interface to the Unix password
hashing API. Above we already introduced support for classical DES and
MD5 hashes, both hashes that are considered \emph{insecure} by today's
standards.\footnote{\emph{Insecure} means that the password can
realistically be derived from the password hash using a brute-force
attack. This implies that leaking the password database is an immediate
security risk.} The crypt() API of modern Unix systems typically support
more secure hashes. Using crypt/2 is suitable if compatibility with OS
passwords is required. If strong hashes and platform independence are
important to you, use crypto_password_hash/2 provided by library
\pllib{crypto} from the
\href{http://www.swi-prolog.org/pldoc/package/ssl.html}{ssl~package}.
\end{description}

\InputIfFileExists{uuid.tex}{}{}

\section{SHA* Secure Hash Algorithms}
\label{sec:sha}

The library \pllib{sha} provides \jargon{Secure Hash Algorihms} approved
by FIPS (\jargon{Federal Information Processing Standard}). Quoting
\href{http://en.wikipedia.org/wiki/SHA-1}{Wikipedia}: \textit{``The
SHA (Secure Hash Algorithm) hash functions refer to five FIPS-approved
algorithms for computing a condensed digital representation (known as a
message digest) that is, to a high degree of probability, unique for a
given input data sequence (the message). These algorithms are called
`secure' because (in the words of the standard), ``for a given
algorithm, it is computationally infeasible 1) to find a message that
corresponds to a given message digest, or 2) to find two different
messages that produce the same message digest. Any change to a message
will, with a very high probability, result in a different message
digest.''}

The current library supports all 5 approved algorithms, both computing
the hash-key from data and the \jargon{hash Message Authentication Code}
(HMAC).

A general comprehensive hash interface is provided by \pllib{crypto},
part of the
\href{http://www.swi-prolog.org/pldoc/package/ssl.html}{ssl~package}.

Input is text, represented as an atom, packed string object or
code-list. Note that these functions operate on byte-sequences and
therefore are not meaningful on Unicode text. The result is returned as
a list of byte-values. This is the most general format that is
comfortable supported by standard Prolog and can easily be transformed
in other formats.  Commonly used text formats are ASCII created by
encoding each byte as two hexadecimal digits and ASCII created using
\jargon{base64} encoding.  Representation as a large integer can be
desirable for computational processing.

\begin{description}
    \predicate{sha_hash}{3}{+Data, -Hash, +Options}
Hash is the SHA hash of Data. \arg{Data} is either an atom, packed
string or list of character codes. \arg{Hash} is unified with a list of
bytes (integers in the range 0..255) representing the hash. See
hash_atom/2 to convert this into the more commonly seen hexadecimal
representation. The conversion is controlled by Options:

    \begin{description}
        \termitem{algorithm}{+Algorithm}
One of \const{sha1} (default), \const{sha224}, \const{sha256},
\const{sha384} or \const{sha512}
	\termitem{encoding}{+Encoding}
This option defines the mapping from Prolog (Unicode) text to bytes on
which the SHA algorithm is performed.  It has two values.  The defualt
is \const{utf8}, which implies that Unicode text is encoded as UTF-8
bytes. This option can deal with any atom. The alternative is
\const{octet}, which implies that the text is considered as a sequence
of bytes.  This is suitable for e.g., atoms that represent binary data.
An error is raised if the text contains code-points outside the range
0..255.
    \end{description}

    \predicate{hmac_sha}{4}{+Key, +Data, -HMAC, +Options}
Quoting \href{http://en.wikipedia.org/wiki/HMAC}{Wikipedia}:
\textit{``A keyed-hash message authentication code, or HMAC, is a type
of message authentication code (MAC) calculated using a cryptographic
hash function in combination with a secret key. As with any MAC, it may
be used to simultaneously verify both the data integrity and the
authenticity of a message. Any iterative cryptographic hash function,
such as MD5 or SHA-1, may be used in the calculation of an HMAC; the
resulting MAC algorithm is termed HMAC-MD5 or HMAC-SHA-1 accordingly.
The cryptographic strength of the HMAC depends upon the cryptographic
strength of the underlying hash function, on the size and quality of the
key and the size of the hash output length in bits.''}

\arg{Key} and \arg{Data} are either an atom,   packed  string or list of
character  codes.  \arg{HMAC}  is  unified  with   a  list  of  integers
representing the authentication code. \arg{Options} is   the same as for
sha_hash/3, but currently only \const{sha1} and \const{sha256} are
supported.

See also crypto_data_hash/3 from \pllib{crypto} library provided by
the SSL package.

    \predicate{hash_atom}{2}{+Hash, -HexAtom}
True when \arg{HexAtom} is the commonly used hexadecimal encoding of
the hash code.  E.g.,

\begin{code}
?- sha_hash('SWI-Prolog', Hash, []),
   hash_atom(Hash, Hex).
Hash = [61, 128, 252, 38, 121, 69, 229, 85, 199|...],
Hex = '3d80fc267945e555c730403bd0ab0716e2a68c68'.
\end{code}
\end{description}

\subsection{License terms}
\label{sec:sha-license}

The underlying SHA-2 library is an unmodified copy created by Dr Brian
Gladman, Worcester, UK.  It is distributed under the license conditions
below.

The free distribution and use of this software in both source and binary
form is allowed (with or without changes) provided that:

\begin{enumerate}
\item
      distributions of this source code include the above copyright
      notice, this list of conditions and the following disclaimer;

\item
      distributions in binary form include the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other associated materials;

\item
      the copyright holder's name is not used to endorse products
      built using this software without specific written permission.
\end{enumerate}

ALTERNATIVELY, provided that this notice is retained in full, this
product may be distributed under the terms of the GNU General Public
License (GPL), in which case the provisions of the GPL apply INSTEAD OF
those given above.


\input{md5.tex}
\input{hashstream.tex}

\section{Memory files}
\label{sec:memory-files}

The \pllib{memfile} provides an alternative to temporary files, intended
for temporary buffering of data. Memory files in general are faster than
temporary files and do not suffer from security risks or naming
conflicts associated with temporary-file management.

There is no limit to the number of memory streams, nor the size of them.
However, a single memory file cannot have multiple streams at the same
time, i.e., a memory file cannot be opened multiple times, not even for
reading.  Memory files are thread-safe and subject to (atom) garbage
collection.

These predicates are first of all intended for building higher-level
primitives such as open_codes_stream/3. See also format/3,
atom_to_term/3, term_to_atom/2, term_string/2, etc.


\begin{description}
    \predicate{new_memory_file}{1}{-Handle}
Create a new memory file and return a unique opaque handle to it.

    \predicate{free_memory_file}{1}{+Handle}
Discard the memory file and its contents.  If the file is open it
is first closed.

    \predicate{open_memory_file}{3}{+Handle, +Mode, -Stream}
Open the memory-file. \arg{Mode} is one of \const{read}, \const{write},
\const{append}, \const{update} or \const{insert}. The resulting
\arg{Stream} must be closed using close/1. When opened for
\const{update} or \const{insert}, the current location is initialized
at the start of the data and can be modified using seek/4 or
set_stream_position/2.  In \const{update} mode, existing content is
replaced, while the size is enlarged after hitting the end of the
data.  In \const{insert} mode, the new data is inserted at the
current point.

    \predicate{open_memory_file}{4}{+Handle, +Mode, -Stream, +Options}
Open a memory-file as open_memory_file/3.  Options:

\begin{description}
    \termitem{encoding}{+Encoding}
Set the encoding for a memory file and the created stream. Encoding
names are the same as used with open/4. By default, memoryfiles
represent UTF-8 streams, making them capable of storing arbitrary
Unicode text. In practice the only alternative is \const{octet}, turning
the memoryfile into binary mode.  Please study SWI-Prolog Unicode and
encoding issues before using this option.
    \termitem{free_on_close}{+Bool}
If \const{true} (default \const{false}) and the memory file is opened
for reading, discard the file (see free_memory_file/1) if the input
is closed.  This is used to realise open_chars_stream/2 in
library(charsio).
\end{description}

    \predicate{size_memory_file}{2}{+Handle, -Size}
Return the content-length of the memory-file in characters in the
current encoding of the memory file. The file should be closed and
contain data.

    \predicate{size_memory_file}{3}{+Handle, -Size, +Encoding}
Return the content-length of the memory-file in characters in the given
\arg{Encoding}. The file should be closed and contain data.

    \predicate{atom_to_memory_file}{2}{+Atom, -Handle}
Turn an atom into a read-only memory-file containing the (shared)
characters of the atom.  Opening this memory-file in mode \const{write}
yields a permission error.

    \predicate{insert_memory_file}{3}{+Handle, +Offset, +Data}
Insert \arg{Data} into the memory file at location \arg{Offset}. The
offset is specified in characters.  \arg{Data} can be an atom, string,
code or character list. Other terms are first serialized using writeq/1.
This predicate raises a domain_error exception if \arg{Offset} is out
of range and a permission_error if the memory file is read-only or
opened.

    \predicate{delete_memory_file}{3}{+Handle, +Offset, +Length}
Delete a \arg{Length} characters from the memory file, starting at
\arg{Offset}. This predicate raises a domain_error exception if
\arg{Offset} or \arg{Offset+Length} is out of range and a
permission_error if the memory file is read-only or opened.

    \predicate{memory_file_to_atom}{2}{+Handle, -Atom}
Return the content of the memory-file in \arg{Atom}.

    \predicate{memory_file_to_atom}{3}{+Handle, -Atom, +Encoding}
Return the content of the memory-file in \arg{Atom}, pretending the data
is in the given \arg{Encoding}. This can be used to convert from one
encoding into another, typically from/to bytes. For example, if we
must convert a set of bytes that contain text in UTF-8, open the memory
file as octet stream, fill it, and get the result using \arg{Encoding}
is \const{utf8}.  Currently only supported if \arg{Encoding} is one
of \const{iso_latin_1}, \const{octed} (the same as \const{iso_latin_1}),
\const{wchar} or \const{utf8}.   Use with another encoding raises a
domain error.

    \predicate{memory_file_to_codes}{2}{+Handle, -Codes}
Return the content of the memory-file as a list of character-codes
in \arg{Codes}.

    \predicate{memory_file_to_codes}{3}{+Handle, -Codes, +Encoding}
Return the content of the memory-file as a list of character-codes
in \arg{Codes}, pretending the data is in the given \arg{Encoding}.

    \predicate{memory_file_to_string}{2}{+Handle, -String}
Return the content of the memory-file as a string in \arg{-String}.

    \predicate{memory_file_to_string}{3}{+Handle, -String, +Encoding}
Return the content of the memory-file as a string in \arg{String},
pretending the data is in the given \arg{Encoding}.

    \predicate{memory_file_substring}{5}{+Handle, ?Before, ?Length, ?After, -SubString}
\arg{SubString} is a substring of the memory file. There are
\arg{Before} characters in the memory file before \arg{SubString},
\arg{SubString} contains \arg{Length} character and is followed by
\arg{After} characters in the memory file. The signature is the same as
sub_string/5 and sub_atom/5, but currently at least two of the 3
position arguments must be specified. Future versions might implement
the full functionality of sub_string/5.

    \predicate{memory_file_line_position}{4}{+MF, ?Line, ?LinePos, ?Offset}
True if the character offset \arg{Offset} corresponds with the
\arg{LinePos} character on line \arg{Line}. Lines are counted from one
(1). Note that \arg{LinePos} is \emph{not} the \jargon{column} as each
character counts for one, including backspace and tab.
\end{description}

\InputIfFileExists{time.tex}{}{}
\InputIfFileExists{unix.tex}{}{}

\section{Limiting process resources}
\label{sec:limit-process-resources}

The \pllib{rlimit} library provides an interface to the POSIX
getrlimit()/setrlimit() API that control the maximum resource-usage
of a process or group of processes.  This call is especially useful
for servers such as CGI scripts and inetd-controlled servers to avoid
an uncontrolled script claiming too much resources.

\begin{description}
    \predicate{rlimit}{3}{+Resource, -Old, +New}
Query and/or set the limit for \arg{Resource}.  Time-values are
in seconds and size-values are counted in bytes.  The following
values are supported by this library.  Please note that not all
resources may be available and accessible on all platforms.  This
predicate can throw a variety of exceptions.  In portable code this
should be guarded with catch/3.  The defined resources are:

\begin{quote}
\begin{tabular}{ll}
\const{as}	& Max address space \\
\const{cpu}	& CPU time in seconds \\
\const{fsize}	& Maximum filesize \\
\const{data}	& max data size \\
\const{stack}	& max stack size \\
\const{core}	& max core file size \\
\const{rss}	& max resident set size \\
\const{nproc}	& max number of processes \\
\const{nofile}	& max number of open files \\
\const{memlock}	& max locked-in-memory address \\
\end{tabular}
\end{quote}

When the process hits a limit POSIX systems normally send the process a
signal that terminates it. These signals may be caught using
SWI-Prolog's on_signal/3 primitive. The code below illustrates this
behaviour. Please note that asynchronous signal handling is dangerous,
especially when using threads.  100\% fail-safe operation cannot be
guaranteed, but this procedure will inform the user properly `most of
the time'.

\begin{code}
rlimit_demo :-
	rlimit(cpu, _, 2),
	on_signal(xcpu, _, cpu_exceeded),
	( repeat, fail ).

cpu_exceeded(_Sig) :-
	format(user_error, 'CPU time exceeded~n', []),
	halt(1).
\end{code}
\end{description}

\input{udpbroadcast.tex}
\input{prologstream.tex}

\section*{NetBSD Crypt license}

\begin{code}
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tom Truscott.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
\end{code}

\printindex

\end{document}

