# Merge request conventions

This page describes GHC's conventions for handling merge requests and code review.

## Field meanings

 * **Title:** GitLab maintains a convention where MRs whose title begins with `Draft:` are marked as work in progress. This marker is understood to mean that review is not yet required. Perhaps a contributor will offer a review regardless, and contributors of Draft patches can request reviews from individuals. But if you mark your patch as Draft and it gets no reviews, that is why.
 * **Milestone:** The first release which should contain the merge request (or a backported version of it)
 * **Labels:** This encodes a number of things including:
    * The state of the merge request (e.g. ~"backport needed")
    * The topical areas the merge request affects (e.g. ~simplifier)
    * Whether the merge request affects the user-facing libraries shipped with GHC (the ~"user facing" label)

## Merge request checklist

See the [merge request description template](https://gitlab.haskell.org/ghc/ghc/blob/master/.gitlab/merge_request_templates/merge-request.md) for checklist of requirements for a merge request.

## Review checklist

When reviewing a merge request here are a few things to check for:

 * Are the commits logically structure? Are their commit messages descriptive?
 * Are ticket numbers referenced as appropriate?
 * Is a GHC release notes entry included (e.g. `docs/users_guide/*-notes.rst`)?
 * Have changelog entries been added to any changed packages (e.g. `libraries/*/changelog.md`)?
 * Has a test case been added?
 * Milestone and ~"backport needed" label set as appropriate
 * Does the patch add a significant new user-facing feature to GHC? If so perhaps a [GHC proposal](https://github.com/ghc-proposals/ghc-proposals) is in order.
 * Does the patch change GHC's core libraries (e.g. `base`, `template-haskell`, `ghc-prim`)? If so:
    * Has the [core libraries committee](https://wiki.haskell.org/Core_Libraries_Committee) consented
    * Has the ~"user-facing" label been applied?
    * Has the [head.hackage job](https://gitlab.haskell.org/ghc/head.hackage/) been run to characterise the effect of the change on user code?
    * Changelog and release notes entries are mandatory
    * Have package versions been bumped as appropriate?
    * Has an entry been added to the next release's migration guide?

To ensure that all interested reviewers have an opportunity to comment, please leave at least 24 hours between the time an MR is opened and assigning to @marge-bot.

## Backports

After a patch has made it to `master` it might be appropriate to backport it to the stable branch (e.g. `ghc-8.8`). If backporting is desired first ensure that the issue is milestoned for the next minor release and mark the merge request containing the fix with the appropriate "backport needed" label(s) (e.g. ~"backport needed:8.8").

While the release manager can perform the backport on your behalf, it is appreciated if you open a merge request with the backported patches yourself. There are two ways to backport a merge request:

 * Via the web interface using the "Cherry-pick" button on the merged MR. While convenient, this is only possible if there are no merge conflicts with the stable branch. Be sure to **select the correct target branch**.
 * Via the command-line using the `git cherry-pick` command. In this case select the appropriate "backport" template (e.g. `backport-for-ghc-8.8`) when creating your merge request.

After the merge request is created ensure it has the ~backport label applied and that its milestone is set appropriately.

Once the backport MR lands the ~"backport needed" label can be removed from the source MR.

## Draft Status

This is a binary status for MR's (either marked `Draft` or not).

### What does it mean for an MR to be marked as Draft

* It generally means the MR in it's current form is not fit to be merged into master.
* The automated merge system (marge-bot) won't merge patches marked as draft into master.
* Maintainers are less likely to fix CI issues,review or rebase such patches.
* There is no guarantee a maintainer will look at an MR marked as Draft at all.

###  Why was my MR marked as Draft

There are a number of reasons why an MR might be marked as draft:

* The patch has been reviewed and some aspect of it needs to be addressed by the author.
* The patch is causing tests to fail on CI which need to be addressed. Either by fixing the patch or accepting new test output.
* The patch needs outside input to proceed (ghc-proposal approval, clc-proposal approval, upstream changes, community feedback)
* The patch is still being worked on.

###  When should I mark my MR as Ready

* If you want review or help on design and implementation aspects of your MR.
* If you think there are no remaining issues with your MR and it's ready to be merged into master.
* If CI fails but you suspect the failures are unrelated to the MR.

If you are in doubt it's **always ok to mark your MR as ready**. A MR not marked ready might never be seen by a maintainer. So it's better to mark your MR as ready than not.  

Worst case a maintainer will mark it as Draft again if they think it's up to you to make changes to the MR before moving forward.

###  Why do we use a draft status

GHC sees a good number patches many of which change between needing maintainer attention and waiting on actions from the author frequently. In order to keep better track of in flight MRs which need the attention of maintainers we started used the Draft status as an indication of MRs which require maintainer attention verses MRs which do not.