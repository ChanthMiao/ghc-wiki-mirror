# How to contribute a patch to GHC


Here's how to submit a bug fix to GHC. For [changing the documentation](working-conventions/documentation-changes), there is a simpler procedure.  For [adding features](working-conventions/adding-features), there are a few extra steps to follow.

1. **Make sure the bug has a GitLab issue**.  Usually it is (that's why you are working on it), but if it's a bug you have found yourself, add it to GitLab before you start work. It's important to have a ticket, because it makes sure that the bug report, discussion about the fix, the regression test that checks it, and the eventual conclusion, are all recorded together.  Comments in the code can refer to the ticket (e.g. `See issue #2382 for an example`). And so on.  If there's no ticket, there is every chance that it'll get lost.

1. **[Add a test case](building/running-tests/adding)** in the [testsuite](building/running-tests) that shows up the bug. 

   - Name the test after the bug number - this is helpful for finding the bug again in the future. 
   - Put the directory and name of the test in the issue description (on the old trac system this was the "Test Case" field of the Trac report).

1. **Fix the bug**! 

   - If your proposed fix has non-local consequences, please consult us before investing too much of your time. 
   - Please follow our [coding conventions](commentary/coding-style). 
   - Comment your fix in the source code, and include a reference to the bug ticket number, e.g. "`#1466`" (this helps when grepping for the fix later). 
   - GHC is guaranteed to compile and validate with the last two GHC releases; write your patch with this in mind. For instance, if you are working on what will be GHC 7.12 your patch should compile with GHC 7.8 and GHC 7.10.

1. **Make one or several commits** that embody your fix. 

   - Separate changes that affect functionality from those that just affect
    code layout, indentation, whitespace, filenames etc.  This means that
    when looking at patches later, we don't have to wade through loads of
    non-functional changes to get to the important parts of the patch. 
   - Please try to follow the general convention for the [Git commit message structure](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) as many Git tools rely on this. Moreover, take into account that the commit message text is interpreted as [GitLab Flavored Markdown](https://docs.gitlab.com/ee/user/markdown.html). 
   - include the ticket number in the form "`#NNNN`" in the commit message, e.g.

     ```wiki
     withMVar family have a bug (fixes #767)
     ```

     Git will then add a link to the commit from the ticket (as soon as the commit becomes reachable from the master HEAD), so that people watching the ticket can see that a fix has been committed, and in the future we can easily find the patch that addressed the ticket. When navigating the Git history on GitLab, you will also be able to jump directly to the ticket from the commit. 
   - Please make sure you have setup git to use the correct name and email for your commits. Use the same name and email on all machines you may commit from, or add an entry to the `.mailmap` file in the ghc root directory.

     ```wiki
     $ git config --global user.name "Firstname Lastname"
     $ git config --global user.email "your_email@youremail.com"
     ```

   - Files in GHC repos should use Unix conventions for line endings.
     If you are on Windows, ensure that git handles line-endings sanely by running:

     ```wiki
     git config --global core.autocrlf false
     ```

1. **Test your commits** using the [validation script](testing-patches); or using CI on gitlab after creating a merge request (see next step). A pipeline job for your branch will start automatically once you open a merge request.

1. **Create a Merge Request** follow the [merge request workflow](/Contributing-a-Patch#merge-request-workflow).