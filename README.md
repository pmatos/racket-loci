# racket-loci 

![CI](https://github.com/pmatos/racket-loci/workflows/CI/badge.svg?branch=master)

Implementation of local and remote loci for Racket.

> **locus** (noun)
> pl. *loci*
> 
> Definition of locus 
> 1. the place where something is situated or occurs
> 
> from [Merriam-Webster Dictionary](https://www.merriam-webster.com/dictionary/locus)

This library implements `loci`, which resemble racket `places` but do not have the same problematic behaviour as `places` when used on many-core machines. `places` are based on OS threads, while `loci` are based on OS processes. The reason to implement `loci` is that `places` do not scale above 12 core machines due to memory allocation locking. More information can be found in the [mailing list](https://groups.google.com/d/msg/racket-users/oE72JfIKDO4/zbFI6knhAQAJ).

A `locus` is a distinct racket instance running on a different process communicating via OS pipes. Hopefully there will be more documentation soon... this is still the early stages and this library **is not yet usable**. Happy to receive questions, comments, complaints and pull requests.

Most of this work was inspired on the implementation of racket places from which ideas and code were copied shamelessly. Thanks to the original authors.

## Future Work

* Implement remote loci

## Sponsor

This work is sponsored by [Linki Tools](https://linki.tools).
