Backporting a merge request (MR) is the action of merging it in the branch of an already-released major GHC version, instead of `master`. For example, if we produce a fix for a bug that has been present in the 8.10 and 9.0 series, whilst the current GHC version in 9.2, we will merge the MR in 9.2 and backport it to 8.10 and 9.0.

## Finding MRs to backport

MRs are typically labelled with *~"backport needed:x.xx"*, like [~"backport needed:8.10"](https://gitlab.haskell.org/ghc/ghc/-/merge_requests?scope=all&utf8=%E2%9C%93&state=closed&label_name[]=backport%20needed%3A8.10).

## Opening the backporting MR

Open the MR as you would do with any other MR, except for the target branch. Do not forget the following points:

1. MRs should be milestoned to the appropriate version (backports for 9.0.2 should be milestoned %9.0.2)
2. The MR should have the *~backport* label
3. After the MR is merged, leave a comment on each of the backported MRs mentioning the backport with a reference to the backport MR 