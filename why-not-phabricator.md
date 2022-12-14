# Why not Phabricator


Related: [WhyNotGitHub](why-not-github).


Note: a `revision` or `Diff` is a unit of code review, and `arc` is the tool
to create them. See [Phabricator](phabricator) for more information.

## Drawbacks of using Phabricator

### Contributing a single commit

- You have to install `PHP` and clone 2 extra git repositories. Then in perpetuum keep your copy of `arc` in sync with GHC's Phabricator instance, by running `arc upgrade` when you see strange errors. This is *not* a feature:

  - "'arc patch' issues ERR-CONDUIT-CORE errors locally. am i alone?"
  - "It gives an exception: ERR-CONDUIT-CORE: Invalid parameter information was passed to method 'conduit.connect'"
  - " 'arc diff' is bombing out on me : \[2015-07-13 11:47:09\] EXCEPTION: (ConduitClientException) ERR-CONDUIT-CORE: Invalid parameter information was passed to method 'differential.parsecommitmessage'. at \[\<phutil\>/src/conduit/ConduitFuture.php:58\]"
  - "ERR-CONDUIT-CALL: Conduit API method "arcanist.projectinfo" does not exist."
  - "arc upgrade failed with another thoroughly mistifying error."

- You have to tell `arc diff` which commit to use as the base commit (e.g. `HEAD^`), when updating a revision after a rebase. This seems to confuse everyone at least once. (Todo: this isn't mentioned on the wiki).

- You have to answer the following questions when creating/updating a revision:

  - "Ignore this untracked file and continue? \[y/N\]"
  - "Do you want to amend this change to the current commit? \[y/N\]"
  - "You have not specified any reviewers. Continue anyway? \[y/N\]"


 


- You have to edit revision summaries manually on [https://phabricator.haskell.org/D1234](https://phabricator.haskell.org/D1234). Updating the commit message and running `arc diff --update D1234 HEAD^` doesn't work.

- Creating a revision in the browser doesn't trigger the continuous integration system.


  


### Contributing multiple commits


- You have to say `Depends on <revision_id>` in the commit message, to make a revision depend on another revision, or `arc patch` doesn't work on it. `arc diff` can't infer this from git history. (Todo: this isn't mentioned on the wiki yet).

- When you have have a stack of commits, each linked to a different revision, `arc diff` will tell you to use `--update` to choose one of those revisions. Running `arc diff --update` with a different revision (say a typo) results in that revision getting overwritten without warning.

- After updating a revision that other revisions depend on, you have to update those dependent revisions as well (running `arc diff HEAD^` for each of them). This triggers another validate run for each of them (arguably a good thing), but forgetting to do so may result in those revisions becoming out-of-date, not validated at all, and impossible to run `arc patch` on.

- `Depends on` clauses in your commit message are reapplied when running `arc diff --update D1234`, even if in the meantime you made changes to the list of dependencies manually on [https://phabricator.haskell.org/D1234](https://phabricator.haskell.org/D1234).


Compare this to a Github workflow, where you just push your updated branch (with `git push -f`), and be done with it.

### Code Review and Git History

- Revisions are more expensive to create than commits (see above). As a result, contributors tend to include style changes and other refactorings in the same revision as the "real" changes. This makes it more difficult to do code review.

>
>
> Here is an example: [https://phabricator.haskell.org/D1081\#inline-8264](https://phabricator.haskell.org/D1081#inline-8264).
>
>

- All commits in a revision get squashed upon landing. This makes it more difficult to use tools such as `git blame`. The `history.immutable` setting might be a solution here.

>
>
> Todo: try to quantify this, perhaps by comparing the number of consecutive commits by the same author (Phabricator will have less such commits, since they're all squashed together, and contributors don't bother with creating multiple revisions using `Depends on`), or by looking at the number of lines changed for each commit (Phabricator commits change more lines).
>
>

- Commit messages contain useless entries by default:

  - Subscribers
  - Reviewers ('Reviewed by' would do)
  - 'Summary: ' prefix to the commit body. 

### Continuous Integration

- Trying to let Harbormaster validate a revision that depends on another
  revision [results in](https://phabricator.haskell.org/D1188):

>
>
> `"ERR-CONDUIT-CALL: API Method "differential.query" does not define these parameters: 'arcanistProjects'.".`
>
>

- Harbormaster is unreliable. Upgrades cause all validates to fail every now and then.

- Harbormaster doesn't show full build logs.

### One year of `arc` on \#ghc

- "hmm, arc mangled my commit message"
- "man, I don't understand this arc patch nonsense"
- "I'm fighting with 'arc diff' myself often"
- "just copy the Code-rev meta-field lines into your commit, and arc should be happy"
- "just wishing that arc would do the Right Thing all by itself"
- "arc-diff doesn't seem to handle commit-stacks very satisfyingly"
- "god, why is arc so slow"
- **"I am never ever ever separating a 20-patchset onto phabricator ever again"**
- "I have a pile of arc bugs I need to file"
- "I couldn't remember the right syntax for getting the Differential number into the arc thingum"
- "Why is arc playing games with me?" "because 'arc' likes to troll you"
- "what does 'arc diff --trace' show you?"
- "I've been using `arc` somewhat creatively, so this could be my fault."
- "I can probably easily rebase my changes on top of yours, but I dunno how to submit an arc differential for that?"
- "how do I delete an arc diff?"
- "so am I \*supposed\* to run it as arc diff `HEAD^`, whatever that means, or something else?"
- **"arc is a frickin' mystery."**
- "I don't get along very well with arc." 
- "will I absolutely need arc to creating a diff for phab?"
- "but seriously, we can't require this for contributing to GHC... the entry barrier is already high enough"
- "I guess I was also thrown off by the fact that ubuntu telling me that I could just apt-get install arc, but that didn't work"
- "(if you're paranoid, or 'arc diff' insists on picking up more than the one commit you  want to submit, run 'arc diff `HEAD^`'"
- "just types ???arc diff??? and hopes for the best"
- "I still think it's abysmal we require PHP of all things though"
- "I did "git pull" + "git rebase master" and now arc diff yells at me"
- "the real problem here is 'arc diff' has some GOD KNOWS WHAT algorithm for figuring out what patches to send"
- "I basically never use arc diff bare, always arc diff HEAD\~"
- "do you sometimes experience weird results with 'arc patch' as well, such as getting weirdly named files out of nowhere?"
- "I got into a really confusing mess after rebasing with just "arc diff""
- "How do I get arc to ignore 'untracked content' in nofib? arc diff --update just aborts"
- **"is arc crashing for anyone else, or is it just me?"**
- "will arc use summary from the last diff, or from the first one?"
- "I see what I did wrong before, apparently 'arc patch --nobranch \<differential revision\>' checks out the submodules to the versions they were at when the revision was created? I wonder why that is"
- "how can I remove the check for unstaged changes in 'arc diff'?"
- "so shouldn't the docs say "don't use 'arc land'" 
- "that what? arc diff `^HEAD`? what does that even do?"
- "'arc patch D123' doesn't work, it says I have uncommitted changes (it also takes ages and gives me no feedback on what it's doing)"
- "I think I totally confused arc by creating D759 on top of a branch named arcpatch-D758"
- "I have generally found, that if you try to use Phab without arc, "u gonna have a bad time""
- "mumbles something to himself about 'arc land' being evil"
- "'arc land' does know nothing about submodules"
- "i have a theory that i've managed to create a revision that phab can't handle."
- "I find that arc gets confused when landing patches if the submodules have been updated in the meantime"
- "is arc broken today?"
- "oh do I need to update my arc?"
- "I use arc elsewhere too, I hope I can upgrade it there without breaking anything or my head might explode"
- "when you have the exact string "Reviewers:" in your description, arc crashes"
- "I was somehow under the impression that arc would manage submodules"
- "I think you'll need to rebase and upload a diff, arc patch (which is what the build script uses) does not rebase your patch"
- "arc patch utterly fails to checkout a differential with changes which require updating a submodule's upstream repo"
- "the commit msg resulting from phab/arc almost never is acceptable to me"
- "Should I specify reviewers?"
- **"Diffs just seem to be too expensive to create ... I can't blame contributors for not wanting to do this for every atomic change"**
- "Attempt to connect to phabricator\@127.0.0.1 failed with error #2013: Lost connection to MySQL server at 'reading initial communication packet', system error: 0."
- "run 'arc which' before updating a  code-rev to get a feeling for arcanist's weird heuristic"
- "arcanist kinda makes stuff even more confusing than Git by itself =)"
- "My browser and everything works fine, arcanist just falls on its face with a libcurl issue"
- "arcanist is complaining"
- "I rebased onto master, but now arcanist is asking me confusing shit when I try to update my differential..."
- "arcanist is yelling at me, despite not changing anything"
- "Oh, I don't really expect arcanist to get submodules right"
- **"the main obstacle for me as a new contributor is that I need to learn phab, etc. instead of just hacking and sending a pr"**

- "you were going to rebase into a nice set of commits.. this might not be necessary as phab squashes all commits anyway"
- "Good thing you told me, now I'll just be as sloppy with commits as I always am if it's squashed anyway."

- "I really like the whole arc stuff, much better than attaching patches to trac tickets"

## References

1. [Help! I'm getting a strange error when running arc that I didn't get yesterday](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#HelpImgettingastrangeerrorwhenrunningarcthatIdidntgetyesterday)
1. ["I think the amout of contributions would increase significantly if GHC migrated to GitHub and started accepting pull requests." (Sep 2014 @ reddit)](https://www.reddit.com/r/haskell/comments/2hes8m/the_ghc_source_code_contains_1088_todos_please/ckrzyec)
1. ["a little phrustrated" (Jul 2014 @ ghc-devs)](https://mail.haskell.org/pipermail/ghc-devs/2014-July/005614.html)
1. ["Phabricator for patches and code review" (Jun 2014 @ ghc-devs)](http://thread.gmane.org/gmane.comp.lang.haskell.ghc.devel/4829/focus=4861)
1. [GHC chatlog](https://phabricator.haskell.org/chatlog/channel/3/)
1. [http://phabricator.org/comparison/](http://phabricator.org/comparison/)
