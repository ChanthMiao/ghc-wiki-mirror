# GIT notes by Simon PJ


Here are notes about using Git that Simon PJ has found useful.

---

## Understanding the model

### Submodules


See [WorkingConventions/Git/Submodules](working-conventions/git/submodules) for the GHC specific stuff.


Suppose you have a submodule `utils/haddock` in the main GHC repo.

- The submodule `utils/haddock` is a full-on, independent Git repo

- You should think of the GHC repo as having a file that contains the commit-id (e.g. `ac60bd1`) for the `utils/haddock` repo.  That's *all* that the GHC repo remembers about the `utils/haddock` repo.  But it means that the GHC repo knows the exact state of the `utils/haddock` repo that matches the state of the GHC repo.

- In the GHC repo, `git submodule update utils/haddock` checks out the `utils/haddock` repo to match the GHC repo's recorded commit-id (in this case `ac60bd1`).  If you do this for all submodules, they'll all line up with a particular state of the GHC repo.

---

## Configuration

### Pager

Git's default pager is `less`, which doesn't work well in an emacs window.  So configure it to be `cat` instead:
```
git config --global core.pager cat
```

### Three way merge

```
git config --global merge.conflictstyle diff3
```
See https://www.programming-books.io/essential/git/view-a-three-way-merge-including-the-common-ancestor-0087a008d96b42beb2de8c2be403e067


### Authentication


* To support 'push' make sure you are using a `git@` URL for the repo, not `https`:
```wiki
git remote set-url origin git@gitlab.haskell.org:ghc/ghc.git
```

* If you are having trouble with authentication, this environment-variable setting will help you see what ssh is doing
```wiki
export GIT_SSH_COMMAND="ssh -v -v" 
```

* To test your ssh connection to the repo, do this:
```wiki
bash$ ssh git@gitlab.haskell.org
setsockopt IPV6_TCLASS 16: Operation not permitted:
PTY allocation request failed on channel 0
Welcome to GitLab, @simonpj!
Connection to gitlab.haskell.org closed.
```

### Push only the current branch


When you say `git push` (with no arguments), push only patches on
the *current branch*.  If you have un-pushed commits on other branches, leave them be.

```wiki
git config --global remote.origin.push HEAD
```

### Creating tracking branches


Here is very useful [info on tracking branches](http://book.git-scm.com/4_tracking_branches.html).


Suppose you create a new branch on your local machine. Now you want to push
it up to the global repo.  You almost certainly want your local branch to become
a tracking option of the remote one, so that `git pull` will merge changes to
the remote copy into your local copy.

```wiki
git config --global branch.autosetupmerge true
```

### Global .gitignore file


Here's how to set up a global (across all your projets) `.gitignore` file.

- Put the file somewhere, eg `$(HOME)/.gitignore`.  The patterns are one per line; e.g. `*.~[0-9]*~` to ignore emacs backup files
- Execute this command

  ```wiki
  git config --global core.excludesfile $(HOME)/.gitignore
  ```


---

## Cleaning up

Sometimes your tree has leftover gubbins that confuses the build system.  To get back to a "just checked out" state use
```
git clean -dfxq && git submodule foreach git clean -dfxq
```
NB: this deletes all untracked files, so use `git status` first to list the untracked files.

Sometimes a submodule seems to be out of sync
```
Changes not staged for commit:
	modified:   libraries/Cabal (modified content)
	modified:   libraries/unix (modified content)
```
To fix this
```
  cd libraries/Cabal; git clean -xdf .; git checkout .
```
------------

## Looking at the current state of affairs

### Show one-line-per-file diff summary


Show a one-line-per-file summary of diffs between working files and the local repo:

```wiki
git diff --stat
```

### Show delta between branch and trunk


Show the commits that are on branch `my-test` but not on the main trunk:

```wiki
git log `git merge-base master my-test`..my-test
```


The `git merge-base b1 b2` thing returns the name of the commit that is the common ancestor of branches `b1` and `b2`.

### Show commits affecting a particular file

```
git log -4 --follow compiler/GHC/Core/Opt/Specialise.hs
```
shows the most recent 4 commits to that file.  It accounts for file renamings.

Maybe use `-M` too.  See [this post](https://kgrz.io/use-git-log-follow-for-file-history.html).

---

## Working with branches

### Create a branch after doing some edits


You are sitting on a branch (say master), and do some edits. Now you decide it wasn't as simple as you thought so you want to create a branch to keep your edits safe while you do something else. 

```wiki
git checkout -b <new-branch-name>
```


This creates the new branch and switches to it, but **does not change your working files**.  Now you can safely commit on the branch [Stackoverflow link](http://stackoverflow.com/questions/2569459/git-create-a-branch-from-unstagged-uncommited-changes-on-master).  Then to push to the master repo:

```wiki
git push origin <new-branch-name>
```


That will create `<new-branch-name>` in the master repo if it does not already exist.

### Work on a branch gotten from the main repo


You have done a `git fetch` to get the upstream repo, which has a branch `origin/experiment`.  You want a local `experiment` branch which tracks `origin/experiment`:

```wiki
git checkout --track origin/experiment
```


This 

- creates a local branch `experiment`, 
- sets it up to track `origin/experiment`, and 
- checks it out


It will fail if local branch `experiment` already exists -- in that case you may want to do the `--set-upstream` thing (see next item). 


If git doesn't know that `origin/experiment` is a branch, the `git checkout` command interprets `origin/experiment` as a filename, and outputs the confusing message

```wiki
fatal: git checkout: updating paths is incompatible with switching branches.
Did you intend to checkout 'origin/experiment' which can not be resolved as commit?
```


You probably want to do a `git fetch`, then `git branch -r` to see what remote branches there are.

### Connect up a local branch with its remote counterpart


You are on local branch `experiment` and do `git pull` to pull down changes from `origin/experiment`, but you get this:

```wiki
You asked me to pull without telling me which branch you
want to merge with, and 'branch.experiment.merge' in
your configuration file does not tell me, either. Please
```


Somehow you in a state where `experiment` isn't tracking `origin/experiment`.  To make it tracking, use `--set-upstream`:

```wiki
-- We are on local branch 'experiment'
git branch --set-upstream-to origin/experiment
```

### Undoing a commit you wish you hadn't made


If you have not yet pushed, you can just back up the head pointer one step:

- `git reset --soft HEAD^`


If you have pushed, you make a commit that exactly undoes what you did, and push that:

- `git revert HEAD`


See [the Git manual](http://www.kernel.org/pub/software/scm/git/docs/user-manual.html#fixing-mistakes) and [ the Git Book](http://book.git-scm.com/4_undoing_in_git_-_reset,_checkout_and_revert.html) on this topic.

### Rebase

`git rebase <base>` 
* Takes `A`, the nearest common ancestor of `HEAD` and `base`
* Transfers commits `A..HEAD` to the tip of `base`

`git rebase --onto <newbase> <base>`
* Takes `A`, the nearest common ancestor of `HEAD` and `base`
* Transfers commits `A..HEAD` to the tip of `newbase`

### Using someone else's GHC repo


Suppose John says "my GHC repo here; clone it and try X". Trouble is: the locations of the submodules are specified using relative paths from the main GHC repository, and chances are that John has only made a fork of the main GHC repo, not all the submodules.  So do this

- Clone from the main GHC repo (including submodules) however you usually do it (e.g., `git clone --recursive https://git.haskell.org/ghc.git`)
- Add John's ghc repo as a remote: `git remote add john git@github.com:John/ghc.git` (or whatever repo URL John gave you).
- Fetch from the new remote: `git fetch john`
- Check out the branch you want: `git checkout john/<branch-name>`

---

## `git gui` on Windows


I???ve been using `git gui` (on Windows at least) as a way to examine and stage changes.  But I suddenly found that it wasn???t displaying the diff in the main pane.  


A google search [http://code.google.com/p/msysgit/issues/detail?id=394](http://code.google.com/p/msysgit/issues/detail?id=394) suggested that (bizarrely) it might have something to do with ???nice???.  


So I renamed `c:/cygwin/bin/nice.exe to c:/cygwin/bin/cygin-nice.exe`, and that made `git gui` worked fine.  Wierd.