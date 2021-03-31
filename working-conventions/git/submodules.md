# Workflows for Handling GHC's Git Submodules

GHC is a large project with several external dependencies. We use git submodules to track these repositories, and here you'll learn a bit about how to manage them.

General information about Git's submodule support:

- ["git submodule" manual page](http://git-scm.com/docs/git-submodule)
- [Pro Git "6.6 Git Tools - Submodules" chapter](http://git-scm.com/book/en/Git-Tools-Submodules)
- [Submodule Tutorial](http://www.vogella.com/tutorials/Git/article.html#submodules)


## Cloning a fresh GHC source tree

Initial cloning of GHC HEAD (into the folder `./ghc`) is a simple as:

```
git clone --recursive https://gitlab.haskell.org/ghc/ghc
```

(Obviously, the clone URL can be replaced by any of the supported `ghc.git` URLs as listed on [http://git.haskell.org/ghc.git](http://git.haskell.org/ghc.git))
See [getting the sources](building/getting-the-sources) for more ways of getting the sources (e.g., from the GitHub mirror), and specifically this [note on using forks](building/getting-the-sources#using-a-fork-of-ghc).

## Updating an existing GHC source tree clone

Sometimes when you pull in new commits, the authors updated a submodule. After pulling, you'll also need to update your submodules, or you'll get errors.

At the top-level of `ghc.git` working copy:

```
git pull --rebase
git submodule update --init
```

In seldom cases it can happen that `git submodule update` aborts with an error similar to the following one

```wiki
fatal: Needed a single revision
Unable to find current revision in submodule path 'libraries/parallel'
```

This means that for some (unknown) reason, the Git submodule in question is in an unexpected/corrupted state. The easiest remedy is remove the named path (or just move it out of the way in case it contains unsaved work), and retry. E.g.

```wiki
rm -rf libraries/parallel
git submodule update --init
```

## `git status` and dirty submodules

By default, git will consider your submodule as "dirty" when you do `git status` if it has any changes or any untracked files.  Sometimes this can be inconvenient, especially when using [Phabricator](phabricator) which won't allow you to upload a diff if there are dirty submodules.  Phabricator will let you ignore untracked files in the main GHC repo, but to ignore untracked files in a submodule you'll need a change to `.git/config` in the GHC repo.  For example, to ignore untracked files in the `nofib` repo, add the line `ignore = untracked` to the section for `nofib` in `.git/config`:

```wiki
[submodule "nofib"]
	url = /home/simon/ghc-mirror/nofib.git
        ignore = untracked
```


## Making changes to GHC's submodules

It's very important to keep in mind that Git submodules track commits, not branches. Therefore, `git submodule update` will result in submodules having checked out a so-called [detached HEAD](http://alblue.bandlem.com/2011/08/git-tip-of-week-detached-heads.html).

To make a change to a submodule (here we use `Cabal` for concreteness), start by ensuring that your tree's submodules are up-to-date:
```bash
git submodule update --init
```
Next, create a branch  for your change, ensuring that the name contains the `wip/` prefix (it is common to name the submodule branches similarly to the GHC branch with which they are associated),
```bash
git -C libraries/Cabal checkout -b wip/$BRANCH_NAME
```
We can now proceed with making the desired changes in the submodule and commit them as usual:
```
# perform modifications and as many `git {add,rm}`s as you deem necessary
$EDITOR libraries/Cabal/src/somefile.hs
# commit
git -C libraries/Cabal commit
```
Finally, we commit the submodule change in the `ghc` repository.
```bash
git commit
```
In the commit message be sure to mention any submodule changes made; a [linter](https://gitlab.haskell.org/ghc/git-haskell-org-hooks/-/blob/master/src/validate-submod-refs.hs) in GHC's CI process checks that any commits containing submodule changes mention the word "submodule" to prevent unintentional submodule changes from accidentally being merged.

To push the changes, we first push the submodule changes to the GHC mirror repository (e.g. ghc/packages/Cabal>). GHC developers have permission to push branches to these mirrors under the `wip/` branch namespace (if you see encounter a permission denied error, ask someone in `#ghc` to grant you Developer rights in the `ghc` group).
```bash
git -C libraries/Cabal remote add gitlab git@gitlab.haskell.org:ghc/packages/Cabal
git -C libraries/Cabal push gitlab wip/$BRANCH_NAME
```
Now create a draft merge request against the upstream projeect (e.g. <https://github.com/haskell/Cabal>) proposing to merge your branch into the branch being tracked by GHC (typically `master`).

Finally, we can push the GHC branch as well:
```bash
git push origin wip/$BRANCH_NAME
```
We can now open a GHC merge request [as usual](https://gitlab.haskell.org/ghc/ghc/-/wikis/Contributing-a-Patch#merge-request-workflow). When doing so, include a list of the associated upstream merge requests for any submodule changes made in the MR.

### Merging an MR containing submodule changes

When an MR containing submodule changes has passed review, it is important that any upstream submodule MRs have been merged *before* the MR is added to the merge queue. While the submodule linting job will catch any MRs which refer to submodule commits not reachable via a persistent branch (e.g. not a `wip/` branch), this will cause @marge-bot jobs to fail, stalling the merge process.


## Upstream repositories

Check out the [Repositories](repositories) page for a full breakdown of all the repositories GHC uses.


## TODO

- Sort out what to do about `transformers`
- Describe status of `pretty` which is one-off at the moment and doesn't exactly track upstream.


## Mirror configuration

GHC maintains mirrors of its core libraries in the GHC/Packages> namespace. There are a few reasons for these mirrors:

 * to provide GHC contributors with a place other than upstream to push their changes for testing in CI
 * to provide a level of safety against forced pushes by upstream
 * to provide a single trusted source for GHC and its dependencies

To accomplish this we configure the submodule projects as follows:

 * The submodule project is configured with pull mirroring from the upstream repository with the "Only mirror protected branches" option enabled
 * All of the "interesting" branches we want to mirror from upstream are added as "Protected branches" (e.g. `master`, and `1.24`)
 * A push rule is added to restrict pushes to the interesting branches and `wip/.*`

This ensures that commits can only be pushed to `wip/.*`, which the submodule check linter does not consider as roots.


## Submodule-specific policies

### Haddock

`Haddock` is quite closely tied to GHC. Consequently, there is one `haddock` branch for each GHC major release series (e.g. haddock's `ghc-8.6` for GHC's `ghc-8.6` branch). Meanwhile, GHC's `master` branch follows `haddock`'s `ghc-head` branch.

Meanwhile, development of `haddock` itself occurs on GHC's current major release branch. This is then periodically merged into `ghc-head`. 