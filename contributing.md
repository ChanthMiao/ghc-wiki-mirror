# Contributing to GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better. This page and the side bar on the right have pointers to information you'll need.

- [Information for newcomers](contributing#newcomers-to-ghc). This the first stop for those people who say, "I want to contribute to GHC, but I don't know quite where to begin." Begin here.

- [How to contribute a patch to GHC](/Contributing-a-Patch). For [adding features](working-conventions/adding-features), there are a few extra steps to follow.

- [How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Working conventions

- **Using Git**: read [how to use git with GHC](working-conventions/git). Information about our submodule setup is in [WorkingConventions/Git/Submodules](working-conventions/git/submodules), and some useful Git tricks are in [WorkingConventions/Git/Tricks](working-conventions/git/tricks).

- **GitLab conventions**:
    - [Label usage](gitlab/labels)
    - [Issue tracker conventions](gitlab/issues)
    - [Merge request conventions](gitlab/merge-requests)

- **Releases and branches**:
    - Our conventions for making releases and how the branches are managed: [Releases](working-conventions/releases)
    - Instructions for [backporting](contributing/Backporting)

- **Useful tools**: [Various tools](working-conventions/useful-tools) which exist to make working on GHC more pleasant.

- **Coding style**: When you are editing GHC's source code, please follow our coding guidelines:
    - [Coding style in the compiler](commentary/coding-style)
    - [Coding style in the runtime system](commentary/rts/conventions)

- **Linting in the CI process**: Here are listed the Hadrian rules that run HLint on your patch.
    - `lint:base`: This runs HLint on the `base` library
    - `lint:compiler`: This runs HLint on the `./compiler` codebase

- **Testing against Hackage**: Patches that change the language GHC accepts may want to test against a subset of Hackage, kept up-to-date with GHC HEAD. See the [head.hackage repo](https://gitlab.haskell.org/ghc/head.hackage) for more information.

- **Licensing**: make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code. However, we do not ask for copyright attribution; you retain copyright on any contributions you make, so feel free to add your copyright to the top of any file in which you make non-trivial changes.

- **For boot libraries**: GHC ships with a number of [boot libraries](commentary/libraries/version-history) maintained outside of GHC itself. Maintainers of such libraries should read [WorkingConventions/BootLibraries](working-conventions/boot-libraries) for guidance on how to maintain their libraries.

## Tips and Tricks

- [Loading GHC into GHCi](building/in-ghci) can provide a more iterative development experience. 

- To have an easier time looking up tickets and searching GitLab, use [the browser tips page](browser-tips) to make your search and lookups for GitLab tickets substantially easier.

- If you use Emacs, see [Emacs](emacs) for some useful stuff to put in your `.emacs` file.

- If you use Spacemacs or VSCode and Nix: The [Spacemacs](spacemacs) and [Visual Studio Code](Visual-Studio-Code) pages explain how to use them as an IDE.

- If you have lots of Haskell installations, you may find Edsko's blog post [Comprehensive Haskell Sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/) useful.

- [ghc-artefact-nix](https://github.com/mpickering/ghc-artefact-nix) is a Nix expression, which downloads a recent artifact from gitlab.haskell.org and enters a nix shell with the artefact available. This is a quick way to try some code out on a recent `HEAD` or a merge request.

## Newcomers to GHC


This section is intended to have the details you will need to get rolling. In addition, note the [building guide](building), [commentary](commentary) and [debugging](debugging) pages have great information that will come in handy while you're working on your first patch(es) and are always available from the sidebar.

If you have any questions along the way don't hesitate to reach out to the community. There are people on the [mailing lists and IRC](mailing-lists-and-irc) who will gladly help you, though you may need to be patient. Don't forget all GHC developers are still learning; your question is never too silly to ask.

### First steps

- See [Building/QuickStart](building/quick-start) to get started building GHC. Expect it all to take roughly between 30 minutes and a couple of hours, depending on your CPU speed, and the number of jobs you can run in parallel. Note that [building older versions of GHC may require having an older version of GHC on your path](https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation/tools).

- While you are waiting for your build to finish, orient yourself to the general architecture of GHC. This [article](http://www.aosabook.org/en/ghc.html) is written by two of the chief architects of GHC, Simon Marlow and Simon Peyton-Jones, is excellent and current (2012).

- After a successful build, you should have your brand new compiler in `./inplace/bin/ghc-stage2`. (GHCi is launched with `./inplace/bin/ghc-stage2 --interactive`). Try it out.

### Finding a ticket

Now that you can build GHC, let's get hacking. But first, you'll need to identify a goal. GHC's GitLab tickets are a great place to find starting points. You are encouraged to ask for a starting point on IRC or the `ghc-devs` [mailing list](mailing-lists-and-irc). There someone familiar with the process can help you find a ticket that matches your expertise and help you when you get stuck.

If you want to get a taste for possible starting tasks, the ~newcomer label collects tickets that appear to be "low-hanging fruit" -- things that might be reasonable for a newcomer to GHC hacking. Of course, we can't ever be sure of how hard a task is before doing it, so apologies if one of these is too hard.

### Advice

- Read up on the steps you are expected to take for [contributing a patch to GHC](/Contributing-a-Patch).

- Need help? You can email the [ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) list, or ask on IRC in `#ghc`.

- Don't get scared. GHC is a big codebase, but it makes sense when you stare at it long enough!

- You can [change](https://gitlab.haskell.org/help/user/profile/index?target=_blank#change-the-email-displayed-on-your-commits) in your profile your commit email to private, e.g. `00001-username@users.gitlab.noreply.gitlab.haskell.org`.

- Don't hesitate to ask questions. We have all been beginners at some point and understand that diving in to GHC can be a challenge. Asking questions will help you make better use of your hacking time.

- Be forewarned that many pages on the GHC Wiki are somewhat out-of-date. Always check the last modification date. Email `ghc-devs` if you're not sure.

- You may want to look at these "how it went for me" blog posts.

  - [Hacking on GHC (is not that hard)](http://rawgit.com/gibiansky/4c54f767bf21a6954b23/raw/67c62c5555f40c6fb67b124307725df168201361/exp.html) by Andrew Gibiansky
  - [Contributing to GHC](http://anniecherkaev.com/projects/contributing-to-ghc) by Annie Cherkaev
  - Andreas Herrmann's [???GHC Hacking Newcomer Guide???](https://youtu.be/s9DkByHSdOg) talk is a wealth of information communicated clearly and succinctly. You can also just browse [the slides](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20180531-GHC-Newcomers-Guide/slides.md).

- There is a blog post series by Stephen Diehl that provides an overview of many important data structures and contains links to other sources of information: [Dive into GHC](http://www.stephendiehl.com/posts/ghc_01.html)

### Further reading
 There are many places in GHC where acronyms and terms are used to refer to specific optimizations. If you are new to working on the compiler these can be a barrier. See the [Reading list](https://gitlab.haskell.org/ghc/ghc/-/wikis/reading-list) wiki page for answers.

Happy hacking!