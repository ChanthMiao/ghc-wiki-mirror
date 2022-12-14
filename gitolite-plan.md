# The Gitolite Switch

**In case of TLDR, go straight to [\#Developerchanges](gitolite-plan#developer-changes).**

## Motivation


Currently, the developer setup on `ghc.haskell.org` is a bit complicated and unfortunate. Most importantly, it hosts all of the Git repositories we use. When a developer for GHC is inducted, we:

- Create them a user (i.e. a full shell account) on `ghc.haskell.org`

- Add their SSH public key

- Add them access to the darcs group (which owns the canonical, public facing Git repositories.)


Then, that user can clone from the server over SSH, and also push to the repositories directly with their new permissions.


This unfortunately has some downsides:

- Every user needs a full shell account. While we probably *won't* be forkbombed by someone, few people actually *need* a full shell account, and the *principle of least privilege* applies here. Really, everybody is just pushing to Git.

- Because of the previous point, group and access permissions on the repositories regularly get screwed up, causing situations where people don't have access (and can't push,) or we have to have `post-receive` hooks that modify the permissions. Both of these suck (this doesn't affect e.g. the Linux kernel developers, who have pull-based development models, because they can afford to.)

  - Due to the current permission scheme, all users in the `darcs` group are effectively at the level of Trac admins, which can manipulate the `trac.db` database (this is needed by the Git hooks to update the Trac tickets). Again, the *principle of least privilege* should apply here.
  - Moreover, people invariably fix this but it's always slightly patchy, and so the repositories that need 'fixing' for things like permissions are inconsistent, and it's hard to keep track of what needs to be maintained.

- All users can willy nilly create (and delete!) tags and branches, and perform some risky Git operations. Ideally, only release maintainers should have permission to do things like cut a release tag.

- Git admins need to perform risky direct manipulations in the file-system, even for the simplest Git repository administration tasks. Also, Git hook scripts are not centrally managed right now, but placed individually in each git repository that needs them; thus to fix/improve a git hook script, one needs to update all git repositories using the affected script.


The proposed remedy is to use the Git-access wrapper [''Gitolite''](https://github.com/sitaramc/gitolite/wiki) which provides an authorization layer on top of Git, and is executed as a separate system user (thus accessing the git repositories with only one Unix UID).  Users accessing git repositories via ssh are discriminated by their ssh public key.


Gitolite also greatly simplifies user management, as a user management is little more than adding/removing a file containing the user's public key, and pushing that the administrative "`gitolite-admin`" git repository (Gitolite is administrated via git itself!). Similarly, adding a new git repository comes down to adding a few lines to the central Gitolite repository config file and pushing that file to the "`gitolite-admin`" Git repository.

## Proposed plan


Below are some notes about how we (Austin & Herbert) would like to go about doing this.

### The switch


Ideally, most of the new setup can occur concurrently with the normal one undisturbed. Presumably 'the big switch' can happen in an hour or so downtime, in which we take the old URIs offline, bring Gitolite online and tell people this is the time to fix your push URLs.

### Developer changes

~~Note: ***The `git.haskell.org` DNS CNAME alias for `ghc.haskell.org` has not been activated yet.*** If you want to try out the new smart-protocol `http://` urls, you can append "`88.198.224.241 git.haskell.org`" to your `/etc/hosts` file. For the non-`http://` protocols, you can just substitute `git.haskell.org` by `ghc.haskell.org` in the following instructions.~~

#### Anonymous read access

**No immediate action needed**


The (old) `http://darcs.haskell.org/<repo-name>.git` Git URLs will continue to work (and still use [The Dumb Protocol](http://git-scm.com/book/en/Git-Internals-Transfer-Protocols#The-Dumb-Protocol)) for some time.


New `git clone` will use the new `http://git.haskell.org/<repo-name>.git` Git URLs and provide the much faster ["smart" Git/HTTP protocol](http://git-scm.com/book/en/Git-Internals-Transfer-Protocols#The-Smart-Protocol). Moreover, also access via `git://git.haskell.org/<repo-name>.git` is provided now.


To rewrite a checked out to the new access URLs, you can use the `sync-all` script, e.g.:

```wiki
./sync-all -r git://git.haskell.org remote set-url origin
```

#### Authenticated read/write access



For developers (with push permissions) who have already checked out repositories, the only change needed is to go over their repositories and update their git uris from


>
>
> `ssh://<user>@darcs.haskell.org/srv/darcs/<repo-name>.git`
>
>


to


>
>
> `ssh://git@git.haskell.org/<repo-name>.git`
>
>


This can be accomplished by invoking the `sync-all` script. But **before** you do this, you need to make sure you have the latest version of `sync-all` as well - a few tiny changes were needed.


Many developers use older trees that may not have been updated before the switch. But never fear, it's quite easy to fix everything, using the GitHub mirror.

```
# make sure you're on a clean copy of master without any outstanding changes
git checkout master
# get the latest version of `./sync-all` from GitHub
git pull https://github.com/ghc/ghc.git master
./sync-all -r git://git.haskell.org remote set-url origin
./sync-all -r ssh://git@git.haskell.org remote set-url --push origin
```


This will correctly set up both push and pull URLs (use `http://` instead of `git://` if you're behind a firewall blocking port 9418).

### Additional user-visible changes

- Ssh public keys for git access are separate from shell account, and will be managed by Gitolite admins (the initial public keys will be populated from the currently authorized shell accounts' `.ssh/authorized_keys` files)

- Developers no longer need a shell account for being able to push to git repositories; so ideally, as ghc.haskell.org hosts the critical Trac&Git resources, only \*required\* shell accounts should remain.

- 'ssh git@???' shows list of currently accessible repositories (+ respective ACLs)

- Optionally later-on: users can manage their pubkeys via "sskm" (self-service key management)

### Setup (to be done by the Admins)

- Install Gitolite

  - `apt-get install Gitolite & dpkg-reconfigure`
    (Debian7 ships with Gitolite version 2.3 in its main-pool);
    `dpkg-reconfigure` will ask about:

    - username: `git`
    - HOME: `/home/git`
  - Set up Gitolite's permissions (-\> umask!) in such a way that Gitolite-owned repositories can only be modified by Gitolite's `git` user, and that Gitolite-owned git repositories remain world readable.
  - old git-repository locations in `/home/darcs` are made symlinks to the new Gitolite-owned git repositories (which are to be moved to `/home/git/repositories` and permission-reset by Gitolite's management scripts)
  - add `www-data` user to `git` group and vice versa, so that Trac and Gitolite can interact with each other.
  - Git hook scripts need to be made Gitolite-friendly
  - Populate Gitolite users & Ssh pubkeys from the `~/.ssh/authorized_keys` files of members of `darcs` group

- Register new CNAME `git.haskell.org` (tangential/optional)

  - HTTP vhost `darcs.haskell.org` stays as-is
  - new HTTP vhost `git.haskell.org` serving /home/git/repositories
    (optionally enable smart git http transport); Git uris will be `http://git.haskell.org/<repo-name>`
  - Cgit could be used as front-page on '/', c.f. [http://www.kernel.org/](http://www.kernel.org/)

## Current status


See #8121

### Questions

- Tangential: should we deprecate the darcs.haskell.org URL? Who uses it? The name was known to be a funny misnomer from the Git switchover times, but As Far As Austin Knows, only GHC developers really use it these days. Perhaps we could just retire it.

  - **Austin**: both nhc and yhc use it, so Malcolm and Neil will need to be asked, at least.
  - **Herbert**: We can leave `darcs.haskell.org` as-is for legacy reasons, while creating new VHOST on `http://git.haskell.org/` which would expose only the Gitolite-owned git repositories (and maybe also a Cgit front-page as e.g. on [http://git.kernel.org](http://git.kernel.org))

- Who's actively committing, and does anybody beyond that actually *need* a shell account? It's unclear who uses `ghc.haskell.org` for what at the moment, beyond push access.

- Tangential: The current directory setup is a total mess on `darcs.haskell.org`, especially since the old darcs repos hang around there (having both `testsuite` and `testsuite.git` is confusing.) Maybe we should clean it up if we're going to use it for a browseable directory.

### Contact points


Austin Seipp (thoughtpolice) and Herbert Valerio Riedel (hvr) can be contacted about details or specifics. If you encounter any issues, you can also comment on #8121.
