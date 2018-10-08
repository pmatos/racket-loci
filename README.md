# racket-loci ![CI Stamp](https://travis-ci.org/LinkiTools/racket-loci.svg?branch=master)

Implementation of local and remote loci for Racket.

> **locus** (noun)
> pl. *loci*
> 
> Definition of locus 
> 1. the place where something is situated or occurs
> 
> from [Merriam-Webster Dictionary](https://www.merriam-webster.com/dictionary/locus)

This library attempts to implement something resembling racket `places` (based on OS threads) by `loci` (places but based on OS processes). The reason to implement this is due to the fact that `places` to not scale above 12 core machines due to memory allocation locking. More information can be found in the [mailing list](https://groups.google.com/d/msg/racket-users/oE72JfIKDO4/zbFI6knhAQAJ).

A `locus` is a distinct racket instance running on a different process communicating via OS pipes. Hopefully there will be more documentation soon... this is still the early stages and this library **is not yet usable**. Happy to receive questions, comments, complaints and pull requests.

## Future Work

* Implement remote loci

## Sponsor

This work is sponsored by [Linki Tools](https://linki.tools).
