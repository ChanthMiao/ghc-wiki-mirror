# Git for Darcs Users


Just like Darcs, every Git command comes with a `--help` option. For example `git add --help`, or even better `git help add`.


Please also read the [Git Working Conventions](working-conventions/git) wiki page. You should also check out the [official Git documentation](http://git.or.cz/gitwiki/GitDocumentation). Also see "General Notes" below for features present in Git but not in Darcs.

# Git Concepts


In order to understand some commands it is important to compare both Git's and Darcs' internal model.  A Darcs repository is a collection of patches with some dependencies between them.  The working directory is what you get when you apply all those patches in a valid order.


Git on the other hand tracks states of the working directory.  Each commit refers to a particular "version" of the working tree.  So far, this is just a different view of the same thing, in fact, Git internally stores some sort of diffs for space efficiency.  The important difference is that a Git commit also remembers which version we make our patch against, i.e., the parent of the new commit.  Several commits can have the same parent or multiple parents, in which case they are merges.  As a result, a Git repository forms a directed acyclic graph.  These are often depicted in ASCII art like this:

```wiki
          o---o---o---o
         /             \
o---o---A---o---o---o---B---o
```


After commit "A" two developers (or the same developer in different branches) performed different commits on top of the same original version.  The "B" commit is a merge.  If the two branches contained conflicting changes "B" would contain its resolution. 

## Branches


A **branch** in Git is now merely *a pointer to a commit*.  For example, a typical situation is this:

```wiki
                o---o---o <-- feature1
               /
          o---o <-- develop  
         /
o---o---A---o---o---o <-- master
```


Here we have three branches "feature1", "develop", and "master".  "master" is the default branch that is created automatically, when you initialise a new Git repository.  Some commands also default to the master branch for certain actions, but otherwise it is not special in any way.


Note that Git branches all exist in the same repository.  You can have several physical clones of the same repository like in Darcs, but it is often more convenient to work with multiple branches in the same directory.  The most important commands for working with branch are `git checkout` and `git branch`, see below.


The typical Git workflow is to use one branch per feature and merge into their original branch once they are considered ready.  For example, if we think that "feature1" is ready, we can `git merge` it into master:

```wiki
$ git checkout develop   # switch to develop branch
$ git merge feature1     # merge feature1 into current branch

          o---o---o---o---o <-- develop, feature1  
         /
o---o---A---o---o---o <-- master
```


The above merge is actually a **fast-forward**, meaning that no new commit is necessary, since "develop" didn't contain any other changes--the pointer for the "develop" branch is merely set to point to the same commit that feature1 pointed to.  On the other hand, if we now merge "devel" into "master" we get a new commit:

```wiki
$ git checkout master
$ git merge devel
          o---o---o---o---o <-- develop, feature1  
         /                 \
o---o---A---o---o---o-------o <-- master
```

## Remote Repositories


XXX: talk about `remote/origin/master`, `fetch`, and how it's mostly just already known `merge`

## Rebase


First of all, `git rebase` is a very dangerous feature, *it should never be done in shared repositories* (this is like `darcs amend-record` and `darcs unpull`, but even harder to fix.)

`git rebase` is a way to "rewrite history". For example, if you have developed feature1 in its own branch:

```wiki
      x---x---x---x <-- feature1
     /
o---o---o---o---o <-- master
```


then you can run

```wiki
git checkout feature1
git rebase master
```


to end up with

```wiki
                   /- master
o---o---o---o---o---x---x---x---x <-- feature1
```


To include the patches in master:

```wiki
git checkout master
git reset --hard feature1
```

```wiki
o---o---o---o---o---x---x---x---x <-- {master, feature1}
```


You can also use this to remove bad patches or to move a branch. For example, consider the following history:

```wiki
      A---B---C---D <-- feature1
     /
o---o---o---o---o <-- master
```


Now, it turns out that `B` was not a good patch.  With `git rebase`, you can choose to just remove `B` completely (similar to `darcs unpull`)

```wiki
      A---C'---D' <-- feature1
     /
o---o---o---o---o <-- master
```


or you could edit `B` and replace it with another change `B'` (similar to `darcs amend-record`).

```wiki
      A---B'---C'---D' <-- feature1
     /
o---o---o---o---o <-- master
```


The easiest way is to use `rebase` for these use cases is via `git rebase -i` (see its documentation).


Note in the above examples how patches `C` and `D` got renamed to `C'`, `D'`.  This indicates that they now have different commit ids because they are now are the same patches but based upon a different history.  That means that another user that already has the original patches `C` and `D` and tries to update from the rebased branch will get conflicts!  (This is comparable to how an amended patch in darcs will lead to conflicts, if someone else already has the original patch in the repository.)


Therefore: **Never rebase a published branch**.


Another use case is to move a branch.  In the above example we can rebase `feature1` onto `master`, resulting in this history:

```wiki
                  A'---B'---C'---D' <-- feature1
                 /
o---o---o---o---o <-- master
```


You can do this if the changes in the feature branch semantically aren't really a branch, but make equally sense on top of the master's history.  In the above example, merging `feature1` into `master` will now be a simple fast-forward and will not introduce a merge commit.


Finally, `rebase` can be used to move a feature branch onto another branch.  For example, say we have a big feature and we want to implement a smaller feature:

```wiki
                o---o---o <-- small_feature
               /
          o---o <-- big_feature  
         /
o---o---A---o---o---o <-- master
```


Now we realise that `small_feature` is already useful and want to merge it into master without the commits that were made on the `big_feature` branch.  We can therefore rebase `small_feature` onto master:

```wiki
$ git rebase --onto master big_feature small_feature
          o---o <-- big_feature
         /
        |             o'--o'--o' <-- small_feature
        |            / 
o---o---A---o---o---o <-- master
```


See `git rebase --help` for more usage information and more examples.

### Uh-oh, I trashed my repo with a rebase! What do I do?


Your repository's old state is almost certainly still present, and you have two ways of getting it back. To motivate the easier approach, we'll first consider the more meticulous.

#### The hard way


Git actually has a bit of a functional philosophy. Objects in git are *immutable*. Although rebase appears to be a destructive update, it creates new commits and leaves the original ones to be cleaned up later by the garbage collector. Destroying work after you've committed it to git requires deliberate effort. Even if you forget to do it the easy way, which you'll see in just a moment, [git means never having to say ???you should have ??????](http://tomayko.com/writings/the-thing-about-git)


Say you began work on a new feature three days ago. In the meantime, other commits have gone into `master`, producing a history whose structure is

```wiki
      A---B---C---D <-- feature1
     /
V---W---X---Y---Z <-- master
```


That is, you've made four commits (referred to as `A` through `D` above) toward `feature1`, and `master` has added three commits since (`X`, `Y`, and `Z`). We can use [Scott Chacon's handy git lol alias](http://blog.kfish.org/2010/04/git-lola.html) to have git draw the history. Assuming `feature1` is our current branch

```wiki
$ git lol --after=3.days.ago HEAD master
* b0f2a28 (HEAD, feature1) D
* 68f87b0 C
* d311c65 B
* a092126 A
| * 83052e6 (origin/master, master) Z
| * 90c3d28 Y
| * 4165a42 X
| * 37844cb W
|/
* f8ba9ea V
```

**N.B.** The commits above have single-letter commit messages for expository benefit. You'd never want to do this in a real repo.


The initial bright idea (that we'll regret later) is to remove `B` from our history.

```wiki
$ git rebase --onto feature1~3 feature1~2 feature1
```


(If this command seems opaque, you can achieve the same effect with [interactive rebase](http://www.kernel.org/pub/software/scm/git/docs/git-rebase.html#_interactive_mode).)


Now the repository looks like this:

```wiki
        C'--D' <-- feature1
       /
      A---B---C---D
     /
V---W---X---Y---Z <-- master
```


Notice that no branch head points to the old `feature1` branch and that `feature1` contains commits resembling `C` and `D` but distinct from them because they have different histories. We can see this with `git lol`:

```wiki
$ git lol --after=3.days.ago HEAD master
* 32ee6f7 (HEAD, feature1) D
* a62b28d C
* a092126 A
| * 83052e6 (origin/master, master) Z
| * 90c3d28 Y
| * 4165a42 X
| * 37844cb W
|/
* f8ba9ea V
```


We see here that `A` has the same commit id or SHA1 as in the earlier `git lol` output, but `C`'s and `D`'s SHA1s are different.

**Oh no! ** It turns out we really *did* need `B` and would like to get it back without having to reimplement the change.


The old branch is still in the repository as unreferenced garbage, but we can dig it out via the reflog, which git updates each time it updates the tip of any branch.

```wiki
$ git reflog --after=3.days.ago
32ee6f7 HEAD@{0}: rebase: D
a62b28d HEAD@{1}: rebase: C
a092126 HEAD@{2}: checkout: moving from feature1 to a092126e339d4c7b9a3c9afb5d456cc1ddf4be4a^0
b0f2a28 HEAD@{3}: checkout: moving from master to feature1
83052e6 HEAD@{4}: pull : Fast-forward
f8ba9ea HEAD@{5}: checkout: moving from feature1 to master
b0f2a28 HEAD@{6}: commit: D
68f87b0 HEAD@{7}: commit: C
d311c65 HEAD@{8}: commit: B
a092126 HEAD@{9}: commit: A
f8ba9ea HEAD@{10}: checkout: moving from master to feature1
```


Reading bottom-to-top, these artifacts tell the repository's recent history:

1. created `feature1` branch
1. did some hacking
1. switched to `master` and pulled updates
1. performed surgery back on `feature1` (`a092...` is the full 160-bit SHA1 of commit `A`)


The old `D` (`b0f2a28`) is still around, and we can see it too:

```wiki
$ git lol --after=3.days.ago HEAD master b0f2a28
* 32ee6f7 (HEAD, feature1) D
* a62b28d C
| * b0f2a28 D
| * 68f87b0 C
| * d311c65 B
|/
* a092126 A
| * 83052e6 (origin/master, master) Z
| * 90c3d28 Y
| * 4165a42 X
| * 37844cb W
|/
* f8ba9ea V
```


If for some reason you'd like to keep both of the feature branches (i.e., `D` and `D'`), create a new branch that points at `D`.

```wiki
$ git branch old-feature1 b0f2a28
```


This produces the following history.

```wiki
$ git lol --after=3.days.ago HEAD master
* eeb9cdb (HEAD, feature1) D
* e7e613e C
| * b0f2a28 (old-feature1) D
| * 68f87b0 C
| * d311c65 B
|/  
* a092126 A
| * 83052e6 (origin/master, master) Z
| * 90c3d28 Y
| * 4165a42 X
| * 37844cb W
|/  
* f8ba9ea V
```


To instead discard the botched rebase and try again, use `git reset` while still on the `feature1` branch:

```wiki
$ git reset --hard b0f2a28
```


Beware that hard reset is a sharp tool. Like `rm -rf` it has its uses, but, ???measure twice; cut once,??? as carpenters say.


See also the [Data Recovery section](http://progit.org/book/ch9-7.html#data_recovery) of *Pro Git* by Scott Chacon.

#### The easy way


By this point, you've probably figured out how to avoid all this fuss. If you're not sure about a complex rebase (or merge), give yourself an easy mulligan by creating a new branch to act as a checkpoint. Then proceed boldly!

1. Create a temporary branch that points to the `HEAD` of the current branch, assumed to be our feature branch.

  ```wiki
  $ git branch tmp
  ```
1. Run `git rebase` with the appropriate arguments.
1. If you're happy with the result, delete the temporary branch.

  ```wiki
  $ git branch -D tmp
  ```
1. Otherwise, rewind and try again.

  ```wiki
  $ git reset --hard tmp
  ```

# General Settings


Just like Darcs, Git has global and per-repository configuration options.  To globally set your committer name and email use

```wiki
git config --global user.name "Haskell Curry"
git config --global user.email haskell@example.com
```


Since Git keeps several branches in the same repository it is very useful to [show the current branch in your shell prompt](http://unboundimagination.com/Current-Git-Branch-in-Bash-Prompt). 

# Git Overview



For an overview of what repositories (or parts of repositories) are modified by various git commands:


>
>
> [http://osteele.com/images/2008/git-transport.png](http://osteele.com/images/2008/git-transport.png)
>
>


The git "local repository" corresponds to darcs internal store of patches.  The git "workspace" corresponds to your normal code tree in darcs. The git "index" is a staging area within the current repository, for storing partially-committed hunks.  It has no equivalent in darcs.

# Commands

## darcs init

```wiki
git init
```

## darcs get

```wiki
git clone <repo-url> [<local-name>]
```


Possible repo URLs look like this:

```wiki
git clone http://git.haskell.org/ghc.git  # via HTTP (slowest)
git clone git://git.haskell.org/ghc.git   # git's protocol (fast, read-only)
git clone ssh://git@git.haskell.org/ghc.git  # via SSH
```

## darcs put


There's no default command to do that, but the following should work:

```wiki
ssh me@remote
cd /some/directory.git
git init --bare
exit
cd my/local/repo
git push me@remote:/some/directory
```


The `--bare` option disables a checkout of the working copy, i.e., only the contents of the `.git` directory are stored.

**Note**: If the repository is supposed to be shared by several users, it's best to init it with either of these commands:

```wiki
git init --bare --shared=group   # everyone in the same group can read and write
git init --bare --shared=all     # same group can read/write, others can read
```


You can also set this after the fact, by setting the configuration variable `core.sharedRepository`.  See `git config --help` for more information.

## darcs add

```wiki
git add <dir-or-file>
```

## darcs record


Git supports interactive recording very similar to darcs.

```wiki
git add -p
```


or

```wiki
git add -i
```


The main difference is that Git does not automatically commit the changes.  You have to do that manually using

```wiki
git commit [-m "commit message"]
```


If you do not supply a commit message, it will open your default editor.  If you want to abort the commit, use an empty commit message.


To see what will be committed, use

```wiki
git diff --cached
```

**Tip**: If you want to see the diff when you edit the commit message, use

```wiki
git commit -v
```

### darcs record -a

```wiki
git commit -a
```


This will add and commit all (not ignored) files.  It will *not* add newly created files. (To do this call `git add .` before in the repo root directory.)

## darcs apply

```wiki
git am
```

## darcs pull


There is no direct mapping for the interactive `darcs pull`.  Cherry-picking is not as streamlined as in Darcs.

### darcs pull -a


Here is how you update (everything) from the source repo:

```wiki
git pull
```


If all you want to do is to keep updated then this is fine.  The above is actually a shortcut for

```wiki
git pull origin
```


where `origin` is the name of your default remote branch.  (You can name it as you like, but certain Git commands will use `origin` as the default if no argument is specified.)


Git allows you to create symbolic references to multiple remote branches.  You can also associate a remote branch with a specific local branch.  For more info see [Distributed Workflows](http://book.git-scm.com/3_distributed_workflows.html) and [ Tracking Branches](http://book.git-scm.com/4_tracking_branches.html).


Note, that `git pull` is actually a shortcut for:

```wiki
git fetch   # download data from remote branches
# for each remote branch:
git merge <remote/branch> <my-local-branch>
```


To find out which branch merges into which local branch see the output of `get remote show <remote-name>`.


Like in Darcs, you may get conflicts.  To resolve conflicts, edit the conflicting file, `git add` it, and `git commit` it.  Alternatively, you can use `git fetch` and then manually `git merge`.

## darcs push


Selectively pushing patches is not available directly in Git.  `git push` does the same as `darcs push -a`.


A comparable interactive workflow is to merge a selection of patches from a local branch into the local master branch and then `git push` that.


In general, even though a central repository is possible, Git promotes a pull model.  That is, to work on a project you typically "fork" (`git clone`) the source repository, add your changes, publish *your* repository, and send a pull-request to the upstream maintainer.  The reasoning behind that is that you don't have something akin to a list of committers, but rather the maintainer has a set of trusted peers.  This model is very different than what seems to be common among darcs users, but it has its advantages.


Obviously, this requires that it's made easy to publish your version of the repository easily.  This is where websites like [GitHub](http://github.com) come into play.  GitHub is free for open source projects (it offers a paid service with private repos), and makes it particularly easy to share with Git.  GitHub automates things like forking and sending pull requests.  GitHub has a quota of 100 MB, but *a forked repository will not count on your quota*.  This is particularly useful for large code bases like GHC.  (The GitHub quota isn't always correct; so if it seems wrong check again the next day.)

`darcs push` is also used to exchange patches between local repositories.  See "Local Branches" below for how to work with branches in Git.


Of course, you need to be able to publish your local changes to a remote repo (even if it's not the main repo).  This is done using `git push` which is largely equivalent to `darcs push -a`

### darcs push -a

```wiki
git push [<repo-url-or-alias>]
```


Without argument this will push to your `origin`.

## darcs send

```wiki
git send-email --to=ghc-devs@haskell.org hash-id -1
```

```wiki
git format-patch origin
```

## darcs changes

```wiki
git log
git log <file-or-directory>
```

### darcs changes --last \<N\>

```wiki
git log -n <N>
```

### darcs changes --summary

```wiki
git log --stat
```

### darcs changes --match

```wiki
git log --grep="something"
```


(the `=`-sign is important)

### Other useful variants

```wiki
git log -p
```


Shows the patch for each commit.

```wiki
git grep <text>
```


Look for something anywhere in the repository's history (tag names, commit messages, file contents).

```wiki
git show <commit-id>
```


Show the changes by the given patch


More examples.

```wiki
git log v2.5..v2.6            # commits between v2.5 and v2.6
git log v2.5..                # commits since v2.5
git log --since="2 weeks ago" # commits from the last 2 weeks
git log v2.5.. Makefile       # commits since v2.5 which modify
                              # Makefile
```


See `git log --help` for a lot of extra options, to refine the output.

## darcs tag

```wiki
git tag <tagname>
```


This will fail if the tag already exists.  If you want to move an existing tag use `git tag -f <tagname>`, but **never move a tag in a public repo/branch**.  Use this only on local branches, and only if the tag exists nowhere else.  `git tag --help` contains a discussion of this.

## darcs whatsnew

```wiki
git status
git diff
```

## darcs diff

```wiki
git diff
```

```wiki
git diff <commit1>..<commit2>  # show diff between two commits
```

## darcs revert


Not sure if this gives you fine-grained reversion of individual hunks:

```wiki
git reset --hard
```

**Note**: `git reset` only resets the staged files, i.e., the things added with `git add`.


To revert one file back to its original state:

```wiki
git checkout <thefile>
```

## darcs unrecord


To unrecord the most recent patch:

```wiki
git reset --soft HEAD^
```


To unrecord a patch other than the most recent one, first you need to re-order patches so that the one you want to unrecord is at the top:

```wiki
git rebase -i <commit>
```


where `<commit>` is the hash of the commit you want to unrecord.  This pops up an editor with the list of patches since `<commit>`, and you can re-order them, squash them together, or remove some altogether.  (Note dangers of rebase, see above).

## darcs unpull


This can be done with `git rebase`, but consider the consequences of what you are about to do (see "Git Concepts" above).


Given a (partial) history like this

```wiki
A---B---C---D---E <-- mybranch
```


you want to remove patch `C`.  Using interactive rebase, it can be done like this:

```wiki
git rebase -i C^ mybranch
```


This opens your editor with a list of the commits `C`, `D`, and `E`.  By default they are labelled with `pick`.  Now remove the line for patch `C` and save your changes.  Git rebase will now check out version `B` and apply the remaining patches `D` and `E` in order, resulting in the desired outcome:

```wiki
A---B---D'---E' <-- mybranch
```

## darcs amend-record


If the change to be amended is the latest commit

```wiki
git commit --amend
```


If the patch is not the last commit you can also use `git rebase -i` as above, but instead of deleting the line, change `pick` to `edit`.  Rebase will then stop and let you edit the files.  Then do your edits, `git commit --amend`, then continue the rebase with `git rebase --continue`.


Here's a walkthrough for amending changes you have already made into the second-to-last patch:

```wiki
# Put the local changes out of the way
git stash
# We want to swap the order of HEAD~0 and HEAD~1 (the two most recent patches)
# We therefore rebase from HEAD~2
git rebase --interactive HEAD~2
# You'll get 2 lines like
# pick 2a37b86 Some commit message
# pick 4aaaded Another commit message
<Swap their order, save and quit>
# Now get our changes back
git stash pop
# and amend the commit
git commit --amend -a
```


You can do a lot more things with `git rebase -i`, like reordering changes or joining or splitting patches.  See the "Interactive Mode" section of `git rebase --help` for more information.

TODO add note for merge commits

## darcs rollback

```wiki
git revert <commit-id>
```


Working directory must be clean.  (You can use `git stash` to save local changes).

## darcs annotate

```wiki
git blame
```

# General Notes

## The Index

## Local Branches

TODO

```wiki
git branch
git branch <name>
git branch -b <name>
git checkout
git branch -d <name>
git branch -D <name>
git stash
git show-branch
```

```wiki
git pull
git fetch
git merge
```

## Suggested Workflow

TODO

- feature branches
- `git rerere`

## Example Workflows

### Fix a bug

```wiki
git pull <upstream>  # get latest changes
git checkout -b fix_bug    # start a branch to fix the bug
# ... hack ...
git add -i           # select the proper changes
git diff --cached    # verify what will be committed
git commit
# ... test ... oops, forgot something
git add -i           # add the new patches
git commit --amend   # add them to previous commit
# ... test ... looks fine now
git checkout master  # back to main branch
git pull             # make sure it's up to date
git merge fix_bug    # merge in our local changes
# ... if we get a conflict here, edit the file then
# ... add the changed files with git add, then git commit
git push             # push changes to personal public repo
                     # or directly to <upstream>
git branch -d fix_foo # delete the branch we no longer need
```


Yes, it is a bit more complicated than using darcs to do the same thing:

```wiki
darcs pull <upstream>  # get latest changes
# ... hack ...
darcs record           # select the proper changes
# ... test ... oops, forgot something
darcs amend-record     # add the new patches
# ... test ... looks fine now
darcs pull             # make sure it's up to date
# ... if we get a conflict here, edit the file then
# ... darcs amend-record
darcs push             # push changes  <upstream>
```