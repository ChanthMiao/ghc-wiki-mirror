# Continuous Integration


This page is to support the discussion of GHC DevOps Group on the CI solution for GHC to provide continuous testing and release artefact generation. See also [\#13716](https://gitlab.haskell.org//ghc/ghc/issues/13716).

## Requirements

**Primary**

- Build GHC for Tier 1 platforms (Windows, Linux & macOS), run `./validate`, and produce release artefacts (distributions and documentation).
- Build PRs (differentials) and run `./validate` on Linux/x86-64.
- Security: PRs builds run arbitrary user code; this must not be able to compromise other builds and especially not release artefacts.
- Infrastructure reproducibility (infrastructure can be spun up and configured automatically)
- Infrastructure forkability (devs forking the GHC repo can run their own CI without additional work)
- Low maintenance overhead
- Low set up costs

**Secondary**

- Build PRs (differentials) and run `./validate` on non-Linux/x86-64 Tier 1 platforms.
- Build GHC for non-Tier 1 platforms & run `./validate`

## Possible solutions

### Jenkins


Pros

- We can run build nodes on any architecture and OS we choose to set up.


Cons

- Security is low on PR builds unless we spend further effort to sandbox builds properly. Moreover, even with sandboxing, Jenkins security record is troublesome.
- Jenkins is well known to be time consuming to set up.
- Additional time spent setting up servers.
- Additional time spent maintaining servers.
- It is unclear how easy it is to make the set up reproducible.
- The set up is not forkable (a forker would need to set up their own servers).

### CircleCI & Appveyor


Pros

- Easy to set up
- Excellent security
- Low maintenance cost
- Infrastructure reproducible 
- Infrastructure forkable
- Easy to integrate with GitHub


Cons

- Direct support only for Linux, Windows, macOS, iOS & Android, everything else needs to rely on cross-compiling and QEMU, or building on remote drones.
- We are limited to CircleCI's & Appveyor's current feature set and have to rely on them for feature development.
- We need to deal with two different CI providers.

### Usage estimate


A quick back-of-the-envelope calculation suggests that to simply keep up
with our current average commit rate (around 200 commits/month) for the
four environments that we currently build we need a bare minimum of:

```wiki
   200 commit/month
 * 4 build/commit             (Linux/i386, Linux/amd64,
                               OS X, Windows/amd64)
 * 2.5 CPU-hour/build         (approx. average across platforms
                               for a validate)
 / (2 CPU-hour/machine-hour)  (CircleCI appears to use 2 vCPU instances)
 / (30*24 machine-hour/month)
 ~ 2 machines
```


Note that this doesn't guarantee reasonable wait times but rather merely
ensure that we can keep up on the mean. On top of this, we see around
300 differential revisions per month. This requires another 3 machines
to keep up.


So, we need at least five machines but, again, this is a minimum;
modelling response times is hard but I expect we would likely need to
add at least two more machines to keep response times in the
contributor-friendly range, especially considering that under Circle CI
we will lose the ability to prioritize jobs (by contrast, with Jenkins
we can prioritize pull requests as this is the response time that we
really care about). Now consider that we would like to add at least
three more platforms (FreeBSD, OpenBSD, Linux/aarch64, all of which may
be relatively slow to build due to virtualisation overhead) as well as a
few more build configurations on amd64 (LLVM, unregisterised, at least
one cross-compilation target) and a periodic slow validation and we may
be at over a dozen machines.

## Todo list