This page concerns the debugging information which is provided by `-finfo-table-map`.
The ultimate goal is to be able to distribute debugging information for the compiler
and base libraries in a manner which doesn't adversely affect normal users.

The plan roughly goes as follows:

1. Make sure the optimisation pipeline is really robust in the presence of SourceNote ticks.
2. Separate debug information from the executable.
  - Allow the RTS to dynamically load debug information at runtime to support whereFrom#
  - Teach eventlog2html and other tools to read debug information, rather than bloat each
    eventlog?
3. Packaging, distribute debugging information with releases.
  - Teach ghcup about this debug info, where to download them from and to
4. Generalise this process to allow external tools to optionally distribute their
   own debugging information.

## Invariance under source notes

In order to optionally distribute debug information the original build must
contain the debug information. At the moment the introduction of source notes
is known to affect the optimiser (#21471) and so we can't with confidence distribute
artifacts which are compiled with the info table map.

There are also the issue that the built interface files will contain SourceNote
information. Which is probably not what we want to distribute? So after the build
is finished we will need to rewrite all the interface files to scrub out the ticks.

## Separate Debug Information

Executables compiled with -finfo-table-map are bloated due to the large size of the
map. It would be better to store the information separately and dynamically load
the debug info in if it was available.

This also raises the possibility of tools like eventlog2html being able to read separate
debug information rather than relying on the information being present in the eventlog.

## Packaging

Once the information is separate it can then be distributed with our binary distributions
to allow users to opt into this debugging style is they prefer.

## Generalisation

This process should be generalisable so that other haskell applications (such as HLS)
can optionally distribute their own debugging information which can be used to produce
accurate user profiles.




