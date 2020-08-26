#lang scribble/manual

@require[@for-label[loci
                    racket/base]]

@title{loci}
@author[(author+email "Paulo Matos" "pmatos@linki.tools")]

@emph{Locus (pl. loci): the place where something is situated or occurs;} - @hyperlink["https://www.merriam-webster.com/dictionary/locus"]{Merriam-Webster Dictionary}

@tech{Loci} enables the development of parallel programs that can take
advantage of machines with multiple processors, cores or hardware
threads... but this time for real!

@margin-note{Currently, the loci library only works on Linux and
MacOS due to the use of unix channels for communication between loci.}

This library attempts to follow the places API very closely. When this
is not the case, feel free to
@hyperlink["https://github.com/LinkiTools/racket-loci"]{report a
bug}. As opposed to places from the standard library, loci use OS
processes instead of OS threads to allow the parallelization of
multiple tasks. Up until 16 simultaneous places there's very little
different between loci and places but once you go into 32, 64, 144,
256, etc. core machines then places will break down and stop working
properly due to memory allocation inter-thread locking.

@section[#:tag "documentation"]{API Documentation}
@defmodule[loci]

@defproc[(locus-enabled?) boolean?]{
  Returns @racket[#t]. It exists for compatibility with the place API.
  Loci are always enabled.
  }

@defproc[(locus? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @deftech{locus descriptor}
  value, @racket[#f] otherwise. Every @tech{locus descriptor} is also
  a @tech{locus channel}.
  }

@defproc[(dynamic-locus [module-path (or/c module-path? path?)]
                        [start-name symbol?])
         locus?]{
  Creates a @tech{locus} to run the procedure that is identified by
  @racket[module-path] and @racket[start-name]. The result is a
  @tech{locus descriptor} value that represents the new parallel task;
  the locus descriptor is returned immediatelly. The locus descriptor is also a
  @tech{locus channel} that permits communication with the locus.

  The module indicated by @racket[module-path] must export a function
  with the name @racket[start-name]. The function must accept a single
  argument, which is a @tech{locus channel} that corresponds to the
  other end of communication for the @tech{locus descriptor} returned
  by @racket[locus].

  When the @tech{locus} is created, the initial @tech{exit handler}
  terminates the locus, using the argument to the exit handler as the
  locus' @deftech{completion value}. Use @racket[(exit _v)] to
  immediatelly terminate a locus with the completion value
  @racket[_v]. Since a completion value is limited to an exact integer
  between @racket[0] and @racket[255], any other value for @racket[v]
  is converted to @racket[0].

  If the function indicated by @racket[module-path] and
  @racket[start-name] returns, the the locus terminates with the
  @tech{completion value} @racket[0].

  In the created locus, the @racket[current-input-port] parameter is
  set to an empty input port, while the values of the
  @racket[current-output-port] and @racket[current-error-port]
  parameters are connected to the ports in the creating locus. 
}

@section[#:tag "motivation"]{Motivation}

Given a problem that requires parallelism, where you use all the
available cores in your machine, the Racket answer as been places. 
I happily used places for a long time until I started stressing tests
by getting larger and larger machines (up to 144 cores) and starting
144 places simultaneously. I noticed that once I exceeded 12 cores
something started to go wrong, in particular the cores were idle most
of the time and there was a lot of kernel locking taking place. This
triggered me sending a
@hyperlink["https://groups.google.com/d/msg/racket-users/oE72JfIKDO4/zbFI6knhAQAJ"]{message}
to the Racket mailing list for help. 



