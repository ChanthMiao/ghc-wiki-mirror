# FreeBSD Support for GHC

## Versions Supported


Since FreeBSD 8.0, both `i386` (x86) and `amd64` (x86_64) are fully supported.

## Building on FreeBSD


Build instructions for FreeBSD are incorporated in the [Building Guide](building).  In particular, here is how to [set up your FreeBSD system for building GHC](building/preparation/free-bsd).

## Installing on FreeBSD


The recommend way to install GHC on FreeBSD is just using the FreeBSD's packages system, i.e. [lang/ghc](https://www.freshports.org/lang/ghc). This can be done via pkg:

```wiki
# pkg install ghc
```

Or via old pkg_tools:

```wiki
# pkg_add -r ghc
```





Note that you can find ports for many popular Haskell software on FreeBSD.  For the complete listing, please consult [the haskell category](http://www.freshports.org/haskell) in the Ports Collection.


An experimental developer repository can be also found on [GitHub](https://github.com/freebsd-haskell/freebsd-haskell) and you read more about the FreeBSD status on its [ FreeBSD wiki page](https://wiki.freebsd.org/Haskell).