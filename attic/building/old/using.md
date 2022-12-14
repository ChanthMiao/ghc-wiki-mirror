# Using the Build System


    
This rest of this guide is designed to help you even if you aren't
really interested in Makefiles and systems configurations, but you
need a mental model of the interlocking pieces so that they can make
them work, extend them consistently when adding new software, and lay
hands on them gently when they don't work.

## History


First, a historical note.  The GHC build system used to be
called "fptools": a generic build system used to build multiple
projects (GHC, Happy, GreenCard, H/Direct, etc.).  It had a
concept of the generic project-independent parts, and
project-specific parts that resided in a project
subdirectory.


Nowadays, most of these other projects are using
[Cabal](http://www.haskell.org/cabal/), or have faded away, and GHC is
the only regular user of the fptools build system.  We decided
therefore to simplify the situation for developers, and specialise the
build system for GHC.  This resulted in a simpler organisation of the
source tree and the build system, which hopefully makes the whole
thing easier to understand.


You might find old comments that refer to "projects" or "fptools" in
the documentation and/or source; please let us know if you do.

## Build trees


If you just want to build the software once on a single platform, then
your source tree can also be your build tree, and you can skip the
rest of this section.


We often want to build multiple versions of our software for different
architectures, or with different options (e.g. profiling).  It's very
desirable to share a single copy of the source code among all these
builds.


So for every source tree we have zero or more *build trees*.  Each
build tree is initially an exact copy of the source tree, except that
each file is a symbolic link to the source file, rather than being a
copy of the source file.  There are "standard" Unix utilities that
make such copies, so standard that they go by different names:
`lndir` and `mkshadowdir` are two (If you don't have either, the
source distribution includes sources for the X11
`lndir` ??? check out `utils/lndir`). See 
[The story so far](#Thestorysofar) 
for a typical invocation.


The build tree does not need to be anywhere near the source tree in
the file system.  Indeed, one advantage of separating the build tree
from the source is that the build tree can be placed in a
non-backed-up partition, saving your systems support people from
backing up untold megabytes of easily-regenerated, and
rapidly-changing, gubbins.  The golden rule is that (with a single
exception ??? [Getting the build you want](#Gettingthebuildyouwant)) *absolutely
everything in the build tree is either a symbolic link to the source
tree, or else is mechanically generated*.  It should be perfectly OK
for your build tree to vanish overnight; an hour or two compiling and
you're on the road again.


You need to be a bit careful, though, that any new files you create
(if you do any development work) are in the source tree, not a build
tree!


Remember, that the source files in the build tree are *symbolic
links* to the files in the source tree.  (The build tree soon
accumulates lots of built files like `Foo.o`, as well.)  You can
*delete* a source file from the build tree without affecting the
source tree (though it's an odd thing to do).  On the other hand, if
you *edit* a source file from the build tree, you'll edit the
source-tree file directly.  (You can set up Emacs so that if you edit
a source file from the build tree, Emacs will silently create an
edited copy of the source file in the build tree, leaving the source
file unchanged; but the danger is that you think you've edited the
source file whereas actually all you've done is edit the build-tree
copy.  More commonly you do want to edit the source file.)


Like the source tree, the top level of your build tree must be (a
linked copy of) the root directory of the GHC source tree.  Inside
Makefiles, the root of your build tree is called
`$(TOP)`.  In
the rest of this document path names are relative to `$(TOP)`
unless otherwise stated.  For example, the file `mk/target.mk` is
actually `$(TOP)/mk/target.mk`.

## Getting the build you want


When you build GHC you will be compiling code on a particular *host
platform*, to run on a particular *target platform* (usually the
same as the host platform).  The difficulty is that there are minor
differences between different platforms; minor, but enough that the
code needs to be a bit different for each.  There are some big
differences too: for a different architecture we need to build GHC
with a different native-code generator.


There are also knobs you can turn to control how the software is
built.  For example, you might want to build GHC optimised (so that it
runs fast) or unoptimised (so that you can compile it fast after
you've modified it.  Or, you might want to compile it with debugging
on (so that extra consistency-checking code gets included) or off.
And so on.


All of this stuff is called the *configuration* of your build.  You
set the configuration using a three-step process.

### Step 1: get ready for configuration


NOTE: if you're starting from a source distribution, rather than darcs
sources, you can skip this step.


Change directory to `$(TOP)` and issue the command

```wiki
$ sh boot
```


(with no arguments). This GNU program (recursively) converts
`$(TOP)/configure.ac` and `$(TOP)/aclocal.m4` to a
shell script called `$(TOP)/configure`.  If `boot`
bleats that it can't write the file `configure`, then delete the
latter and try again.  Note that you must use `sh boot`, and
not the old `autoreconf` or `autoconf`!  If you erroneously
use `autoreconf` then building the libraries will fail, and it
you use `autoconf` you'll
get a message like `No rule to make target 'mk/config.h.in'`.


Some parts of the source tree, particularly libraries, have their own
configure script.  `sh boot` takes care of that, too, so all
you have to do is calling `sh boot` in the top-level directory
`$(TOP)`.


These steps are completely platform-independent; they just mean that
the human-written files (`configure.ac` and `aclocal.m4`) can
be short, although the resulting files (the `configure` shell
scripts and the C header template `mk/config.h.in`) are long.

### Step 2: system configuration.


Run the newly-created `configure` script, thus:

```wiki
$ ./configure <args>
```

`configure`'s mission is to scurry round your computer working out
what architecture it has, what operating system, whether it has the
`vfork` system call, where `tar` is kept, whether `gcc` is
available, where various obscure `#include` files are, whether
it's a leap year, and what the systems manager had for lunch.  It
communicates these snippets of information in two ways:

- It translates `mk/config.mk.in` to `mk/config.mk`,
  substituting for things between "`@`" brackets.  So,
  "`@HaveGcc@`" will be replaced by "`YES`" or "`NO`"
  depending on what `configure` finds.  `mk/config.mk` is
  included by every Makefile (directly or indirectly), so the
  configuration information is thereby communicated to all Makefiles.

- It translates `mk/config.h.in` to `mk/config.h`.  The
  latter is `#include`d by various C programs, which can
  thereby make use of configuration information.

`configure` takes some optional arguments.  Use `./configure --help`
to get a list of the available arguments.  Here are some of
the ones you might need:


<table><tr><th><tt>--with-ghc=<path></tt></th>
<td>
Specifies the path to an installed GHC which you would like to use.
This compiler will be used for compiling GHC-specific code (eg. GHC
itself).  This option <i>cannot</i> be specified using <tt>build.mk</tt>
(see later), because <tt>configure</tt> needs to auto-detect the
version of GHC you&apos;re using.  The default is to look for a compiler
named <tt>ghc</tt> in your path.
              
</td></tr>
<tr><th><tt>--with-hc=<path></tt></th>
<td>
Specifies the path to any installed Haskell compiler.  This compiler
will be used for compiling generic Haskell code.  The default is to
use <tt>ghc</tt>. (NOTE: I&apos;m not sure it actually works to specify a
compiler other than GHC here; unless you really know what you&apos;re
doing I suggest not using this option at all.)
              
</td></tr>
<tr><th><tt>--with-gcc=<path></tt></th>
<td>
Specifies the path to the installed GCC. This compiler will be used
to compile all C files, <i>except</i> any generated by the installed
Haskell compiler, which will have its own idea of which C compiler
(if any) to use.  The default is to use <tt>gcc</tt>.
</td></tr></table>


### Step 3: build configuration.



Next, you say how this build of GHC is to differ from the standard
defaults by creating a new file `mk/build.mk` *in the build
tree*.  This file is the one and only file you edit in the build
tree, precisely because it says how this build differs from the
source.  (Just in case your build tree does die, you might want to
keep a private directory of `build.mk` files, and use a symbolic
link in each build tree to point to the appropriate one.)  So
`mk/build.mk` never exists in the source tree ??? you create one
in each build tree from the template.  We'll discuss what to put in it
shortly.


And that's it for configuration. Simple, eh?



What do you put in your build-specific configuration file
`mk/build.mk`?  *For almost all purposes all you will do is put
make variable definitions that override those in*
`mk/config.mk.in`.  The whole point of
`mk/config.mk.in` ??? and its derived counterpart
`mk/config.mk` ??? is to define the build configuration. It is
heavily commented, as you will see if you look at it.  So generally,
what you do is look at `mk/config.mk.in`, and add definitions in
`mk/build.mk` that override any of the `config.mk` definitions
that you want to change.  (The override occurs because the main
boilerplate file, `mk/boilerplate.mk`, includes `build.mk`
after `config.mk`.)


For your convenience, there's a file called `build.mk.sample` that
can serve as a starting point for your `build.mk`.



For example, `config.mk.in` contains the definition:


<table><tr><th><tt>GhcHcOpts = -Rghc-timing</tt></th>
<td>
The accompanying comment explains that this is the list of flags
passed to GHC when building GHC itself.  For doing development, it
is wise to add <tt>-DDEBUG</tt>, to enable debugging code.  So you
would add the following to <tt>build.mk</tt>:
      
</td></tr>
<tr><th><tt>GhcHcOpts += -DDEBUG</tt></th>
<td>
GNU <tt>make</tt> allows existing definitions to have new text appended
using the &quot;<tt>+=</tt>&quot; operator, which is quite a convenient feature.
<br><br>
Haskell compilations by default have <tt>-O</tt> turned on, by virtue
of this setting from <tt>config.mk</tt>:
</td></tr></table>


<table><tr><th><tt>SRC_HC_OPTS += -H16m -O</tt></th>
<td>
<tt>SRC_HC_OPTS</tt> means &quot;options for HC from the source tree&quot;, where
HC stands for Haskell Compiler.  <tt>SRC_HC_OPTS</tt> are added to
every Haskell compilation.  To turn off optimisation, you could add
this to <tt>build.mk</tt>:
</td></tr></table>


<table><tr><th><tt>SRC_HC_OPTS = -H16m -O0</tt></th>
<td>
Or you could just add <tt>-O0</tt> to <tt>GhcHcOpts</tt> to turn off
optimisation for the compiler.  See <a href="building/hacking">Building/Hacking</a>
for some more suggestions.
<br><br>
When reading <tt>config.mk.in</tt>, remember that anything between
&quot;@...@&quot; signs is going to be substituted by <tt>configure</tt> later.
You <i>can</i> override the resulting definition if you want, but you
need to be a bit surer what you are doing.  For example, there&apos;s a
line that says:
</td></tr></table>


<table><tr><th><tt>TAR = @TarCmd@</tt></th>
<td>
This defines the Make variables <tt>TAR</tt> to the pathname for a
<tt>tar</tt> that <tt>configure</tt> finds somewhere.  If you have your
own pet <tt>tar</tt> you want to use instead, that&apos;s fine. Just add
this line to <tt>mk/build.mk</tt>:
</td></tr></table>


<table><tr><th><tt>TAR = mytar</tt></th>
<td>
You do not <i>have</i> to have a <tt>mk/build.mk</tt> file at all; if you
don&apos;t, you&apos;ll get all the default settings from
<tt>mk/config.mk.in</tt>.
</td></tr></table>


>
>
> You can also use `build.mk` to override anything that
> `configure` got wrong.  One place where this happens often is
> with the definition of `FPTOOLS_TOP_ABS`: this variable is supposed
> to be the canonical path to the top of your source tree, but if your
> system uses an automounter then the correct directory is hard to
> find automatically.  If you find that `configure` has got it
> wrong, just put the correct definition in `build.mk`.
>
>

## The story so far


Let's summarise the steps you need to carry to get yourself a
fully-configured build tree from scratch.

- Get your source tree from somewhere (darcs repository or source
  distribution).  Say you call the root directory `myghc` (it
  does not have to be called `ghc`).

- (Optional) Use `lndir` or `mkshadowdir` to create a build
  tree.

  ```wiki
  $ cd myghc
  $ mkshadowdir . /scratch/joe-bloggs/myghc-x86
  ```

  (N.B. `mkshadowdir`'s first argument is taken relative to its
  second.) You probably want to give the build tree a name that
  suggests its main defining characteristic (in your mind at least),
  in case you later add others.

- Change directory to the build tree.  Everything is going to happen
  there now.

  ```wiki
  $ cd /scratch/joe-bloggs/myghc-x86
  ```

- Prepare for system configuration:

  ```wiki
  $ sh boot
  ```

  (You can skip this step if you are starting from a source
  distribution, and you already have `configure` and
  `mk/config.h.in`.)

- Do system configuration:

  ```wiki
  $ ./configure
  ```

  Don't forget to check whether you need to add any arguments to
  `configure`; for example, a common requirement is to specify
  which GHC to use with
  `--with-ghc=<path>`.

- Create the file `mk/build.mk`, adding definitions for your
  desired configuration options.


You can make subsequent changes to `mk/build.mk` as often as you
like.  You do not have to run any further configuration programs to
make these changes take effect. In theory you should, however, say
`make clean; make`, because configuration option changes could
affect anything ??? but in practice you are likely to know what's
affected.

## Making things


At this point you have made yourself a fully-configured build tree, so
you are ready to start building real things.


The first thing you need to know is that *you must use GNU
`make`*.  On some systems (eg. FreeBSD) this is called
`gmake`, whereas on others it is the standard `make` command.
In this document we will always refer to it as `make`; please
substitute with `gmake` if your system requires it.  If you use a
the wrong `make` you will get all sorts of error messages (but no
damage) because the GHC `Makefiles` use GNU `make`'s
facilities extensively.


To just build the whole thing, `cd` to the top of your build tree
and type `make`.  This will prepare the tree and build the various
parts in the correct order, resulting in a complete build of GHC that
can even be used directly from the tree, without being installed
first.

## Bootstrapping GHC


GHC requires a 2-stage bootstrap in order to provide full
functionality, including GHCi.  By a 2-stage bootstrap, we mean that
the compiler is built once using the installed GHC, and then again
using the compiler built in the first stage.  You can also build a
stage 3 compiler, but this normally isn't necessary except to verify
that the stage 2 compiler is working properly.


Note that when doing a bootstrap, the stage 1 compiler must be built,
followed by the runtime system and libraries, and then the stage 2
compiler.  The correct ordering is implemented by the top-level
`Makefile`, so if you want everything to work automatically it's
best to start `make` from the top of the tree.  The top-level
`Makefile` is set up to do a 2-stage bootstrap by default (when
you say `make`).  Some other targets it supports are:


<table><tr><th><tt>stage1</tt></th>
<td>
Build everything as normal, including the stage 1 compiler.
</td></tr></table>


<table><tr><th><tt>stage2</tt></th>
<td>
Build the stage 2 compiler only.
</td></tr></table>


<table><tr><th><tt>stage3</tt></th>
<td>
Build the stage 3 compiler only.
</td></tr></table>


<table><tr><th><tt>bootstrap</tt>, <tt>bootstrap2</tt></th>
<td>
Build stage 1 followed by stage 2.
</td></tr></table>


<table><tr><th><tt>bootstrap3</tt></th>
<td>
Build stages 1, 2 and 3.
</td></tr></table>


<table><tr><th><tt>install</tt></th>
<td>
Install everything, including the compiler built in stage 2.  To
override the stage, say <tt>make install stage=n</tt> where <tt>n</tt> is
the stage to install.
</td></tr></table>


<table><tr><th><tt>binary-dist</tt></th>
<td>
make a binary distribution.  This is the target we
use to build the binary distributions of GHC.
</td></tr></table>


<table><tr><th><tt>dist</tt></th>
<td>
make a source distribution.  Note that this target
does <tt>make distclean</tt> as part of its work;
don&apos;t use it if you want to keep what you&apos;ve built.
</td></tr></table>


The top-level `Makefile` also arranges
to do the appropriate `make boot` steps (see
below) before actually building anything.


The `stage1`, `stage2` and `stage3` targets also work in
the `compiler` directory, but don't forget that each stage
requires its own `make boot` step: for example, you must do

```wiki
$ make boot stage=2
```


before `make stage2` in `compiler`.

## Standard Targets



In any directory you should be able to make the following:


<table><tr><th><tt>boot</tt></th>
<td>
does the one-off preparation required to get ready for the real
work, e.g. building the module dependency graph.
<br><br>
Invoking the <tt>boot</tt> target explicitly is not normally necessary.
From the top-level directory, invoking <tt>make</tt> causes <tt>make boot</tt>
to be invoked in various subdirectories, in the right
order.  Unless you really know what you are doing, it is best to
always say <tt>make</tt> from the top level first.
<br><br>
If you&apos;re working in a subdirectory somewhere and need to update the
dependencies, <tt>make boot</tt> is a good way to do it.
</td></tr></table>


<table><tr><th><tt>all</tt></th>
<td>
makes all the final target(s) for this Makefile.  Depending on which
directory you are in a &quot;final target&quot; may be an executable program,
a library archive, a shell script, or a Postscript file.  Typing
<tt>make</tt> alone is generally the same as typing <tt>make all</tt>.
</td></tr></table>


<table><tr><th><tt>install</tt></th>
<td>
installs the things built by <tt>all</tt> (except for the
documentation).  Where does it install them?  That is specified by
<tt>mk/config.mk.in</tt>; you can override it in <tt>mk/build.mk</tt>, or
by running <tt>configure</tt> with command-line arguments like
<tt>--bindir=/home/simonpj/bin</tt>; see <tt>./configure --help</tt> for
the full details.
</td></tr></table>


<table><tr><th><tt>install-docs</tt></th>
<td>
installs the documentation. Otherwise behaves just like
<tt>install</tt>.
</td></tr></table>


<table><tr><th><tt>clean</tt></th>
<td>
Delete all files from the current directory that are normally
created by building the program.  Don&apos;t delete the files that record
the configuration, or files generated by <tt>make boot</tt>.  Also
preserve files that could be made by building, but normally aren&apos;t
because the distribution comes with them.
</td></tr></table>


<table><tr><th><tt>distclean</tt></th>
<td>
Delete all files from the current directory that are created by
configuring or building the program. If you have unpacked the source
and built the program without creating any other files, <tt>make distclean</tt>
should leave only the files that were in the distribution.
</td></tr></table>


<table><tr><th><tt>mostlyclean</tt></th>
<td>
Like <tt>clean</tt>, but may refrain from deleting a few files that
people normally don&apos;t want to recompile.
</td></tr></table>


<table><tr><th><tt>maintainer-clean</tt></th>
<td>
Delete everything from the current directory that can be
reconstructed with this Makefile.  This typically includes
everything deleted by <tt>distclean</tt>, plus more: C source files
produced by Bison, tags tables, Info files, and so on.
<br><br>    
One exception, however: <tt>make maintainer-clean</tt> should not
delete <tt>configure</tt> even if <tt>configure</tt> can be remade using a
rule in the <tt>Makefile</tt>. More generally, <tt>make maintainer-clean</tt>
should not delete anything that needs to exist in order to run
<tt>configure</tt> and then begin to build the program.
<br><br>    
After a <tt>maintainer-clean</tt>, a <tt>configure</tt> will be necessary
before building again.
</td></tr></table>


All of these standard targets automatically recurse into
sub-directories.  Certain other standard targets do not:


<table><tr><th><tt>depend</tt></th>
<td>
make a <tt>.depend</tt> file in each directory that needs it. This
<tt>.depend</tt> file contains mechanically-generated dependency
information; for example, suppose a directory contains a Haskell
source module <tt>Foo.lhs</tt> which imports another module <tt>Baz</tt>.
Then the generated <tt>.depend</tt> file will contain the dependency:

```wiki
Foo.o : Baz.hi
```

which says that the object file <tt>Foo.o</tt> depends on the interface
file <tt>Baz.hi</tt> generated by compiling module <tt>Baz</tt>.  The
<tt>.depend</tt> file is automatically included by every Makefile.
Now that we are using Cabal for most of the building, most directories
don&apos;t support the <tt>depend</tt> target any more. Use <tt>boot</tt> instead.
</td></tr></table>


Some `Makefile`s have targets other
than these.  You can discover them by looking in the
`Makefile` itself.

## Using GHC from the build tree


If you want to build GHC and just use it direct from the build tree
without doing `make install` first, you can run the in-place
driver script.  To run the stage 1 compiler, use
`ghc/stage1-inplace/ghc`, stage 2 is
`ghc/stage2-inplace/ghc`, and so on.


Utils like `ghc-pkg` can be found under
`utils/ghc-pkg/install-inplace/bin/ghc-pkg` etc.

## Fast Making


Sometimes the dependencies get in the way: if you've made a small
change to one file, and you're absolutely sure that it won't affect
anything else, but you know that `make` is going to rebuild
everything anyway, the following hack may be useful:

```wiki
$ make FAST=YES
```


This tells the make system to ignore dependencies and just build what
you tell it to.  In other words, it's equivalent to temporarily
removing the `.depend` file in the current directory (where
`mkdependHS` and friends store their dependency information).


A bit of history: GHC used to come with a `fastmake` script that
did the above job, but GNU make provides the features we need to do it
without resorting to a script.  Also, we've found that fastmaking is
less useful since the advent of GHC's recompilation checker (see the
User's Guide section on "Separate Compilation").
