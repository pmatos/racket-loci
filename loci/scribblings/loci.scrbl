#lang scribble/manual

@require[@for-label[loci
                    racket/base]]

@title{loci}
@author{@hyperlink["mailto:pmatos@linki.tools"]{Paulo Matos}}

@emph{Locus (pl. loci): the place where something is situated or occurs;} - @hyperlink["https://www.merriam-webster.com/dictionary/locus"]{Merriam-Webster Dictionary}

@section[#:tag "documentation"]{API Documentation}
@defmodule[loci]

To do...

@section[#:tag "exp-documentation"]{Experimental API Documentation}
@defmodule[loci-dev]

There are currently no differences between the development and the stable
version of the @deftech{loci} library. Please refer to the @secref{documentation} for
more information.

@section[#:tag "motivation"]{Motivation}

Given a problem that requires parallelism, where you use all the available cores in your machine, the Racket answer as been places.
I happily used places for a long time until I started stressing tests by getting larger and larger machines (up to 144 cores) and starting 144 places simultaneously. I noticed that once I exceeded 12 cores something started to go wrong, in particular the cores were idle most of the time and there was a lot of kernel locking taking place. This triggered me sending a @hyperlink["https://groups.google.com/d/msg/racket-users/oE72JfIKDO4/zbFI6knhAQAJ"]{message} to the Racket mailing list for help.



