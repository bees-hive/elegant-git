# `git-elegant`
```bash

An assistant who carefully automates routine work with Git.

usage: git elegant [-h | --help | help | --version | version]
   or: git elegant <command> [-h | --help | help]
   or: git elegant <command> [--no-workflows] [args]
   or: git elegant [--no-workflows] <command> [args]

    -h, --help, help    displays help
    --version, version  displays program version
    --no-workflows      disables available workflows

There are commands used in various situations such as

 act with a repository
    clone-repository     Clones a repository and configures it.
    init-repository      Initializes a new repository and configures it.
    acquire-repository   Configures current repository.
    prune-repository     Removes useless local branches.

 manage a personal work
    start-work           Creates a new branch.
    save-work            Commits current modifications.
    amend-work           Amends some changes to the most recent commit.
    show-work            Shows a state of current work in progress.
    polish-work          Reapplies branch commits interactively.
    deliver-work         Publishes current branch to a remote repository.

 operate a flow of work management
    obtain-work          Checkouts a remote branch matching by a name.
    accept-work          Applies a branch on top of `master` branch.

 release new versions
    show-release-notes   Prints a release log between two references.
    release-work         Releases available work as a new annotated tag.

 and others
    show-commands        Prints available Elegant Git commands.
    acquire-git          Configures a Git installation.


Please visit https://elegant-git.bees-hive.org to find out more.

```

# `accept-work`

```bash
usage: git elegant accept-work <remote branch>
```

Checkouts given branch using `git elegant obtain-work` into a temporary one.
Then, it makes a rebase of the latest version of default upstream branch
(`master`) with current changes. If there is a rebase in progress and it is
initiated by this command, it will be continued instead. The final rebased index
merges using fast-forward strategy into the default local branch and pushes into
the default upstream branch (`origin/master`). After a successful push, the
temporary branch is removed as well as given branch if it locates in `origin`
remote.

The command uses branch and stash pipes to preserve the current Git state prior
to execution and restore after.

Approximate commands flow is
```bash
==>> git elegant accept-work task-123
git fetch --all
git checkout --force -B __eg origin/task-123
git status
git rebase origin/master
git checkout master
git merge --ff-only __eg
git push origin master:master
git branch --delete --force __eg
git push origin --delete task-123
```

# `acquire-git`

```bash
usage: git elegant acquire-repository
```

Applies the "basics", "standards", and "aliases" configurations to the current
Git installation using `git config --global`.

During the first execution, you will be asked to provide some information.
After, Elegant Git will automatically detect what should be changed.

To find out what will be configured, please visit
https://elegant-git.bees-hive.org/en/latest/configuration/

# `acquire-repository`

```bash
usage: git elegant acquire-repository
```

Applies the "basics", "standards", and "aliases" configurations to the current
Git repository using `git config --local`.

To find out what will be configured, please visit
https://elegant-git.bees-hive.org/en/latest/configuration/

# `amend-work`

```bash
usage: git elegant amend-work
```

Updates the most recent commit by adding some changes. The command will fail if
you'll try to modify history of the default local branch.

Approximate commands flow is
```bash
==>> git elegant amend-work
git add --interactive
git diff --cached --check
git commit --amend
```

# `clone-repository`

```bash
usage: git elegant clone-repository <repository>
```

Clones a repository into a new directory and runs its configuration.

Approximate commands flow is
```bash
==>> git elegant clone-repository git@github.com:bees-hive/elegant-git.git
git clone git@github.com:bees-hive/elegant-git.git
cd elegant-git
git elegant acquire-repository
```

# `deliver-work`

```bash
usage: git elegant deliver-work [branch-name]
```

Updates the current branch by rebasing the default upstream branch. If there is
a rebase in progress, the command will continue it instead of initiation a new
one. Then, it pushes HEAD to the appropriate upstream branch.

By default, the name of remote branch is equal to the local one. If a local
branch has an upstream branch configured, it will be used as a remote branch.
If you provide a custom name of the remote branch, it will be used as a remote
branch.

The command uses stash pipe to preserve the current Git state prior to execution
and restore after.

If the push output contains an URL (like a link to create a pull request), it
will be open (in case if `open` command is available) in a default browser.

Approximate commands flow is
```bash
==>> git elegant deliver-work
git fetch
git rebase origin/master
git push --set-upstream --force origin task-123:task-123
```

# `init-repository`

```bash
usage: git elegant init-repository
```

Creates an empty Git repository (or reinitialize an existing one) and runs its
configuration.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
```

# `obtain-work`

```bash
usage: git elegant obtain-work <remote branch> [local branch]
```

Seeks across all upstreams for a branch matching by a given full or partial
name. If there are more then 1 matching branch, execution stops with a
corresponding error message. By default, the name of the local branch responds
to the remote one. However, it can be overridden by giving a second argument.

Approximate commands flow is
```bash
==>> git elegant obtain-work new-feature task-133
git fetch --all
git checkout -B task-133 custom-remote/new-feature
```

# `polish-work`

```bash
usage: git elegant polish-work
```

Reapplies branch commits using interactive rebase. It uses only new commits that
aren't in `master` branch. If there is a rebase in progress, the command will
continue it.

Prior to the execution and in the case of rebase initiation, all uncommitted
tracked modifications will be temporally stashed. And they will be uncovered if
the rebase completes without errors, otherwise, you need to apply them manually
by running `git stash pop`.

The command raises error 42 if it runs against `master` branch.

The command uses stash pipe to preserve the current Git state prior to execution
and restore after.

Approximate commands flow is
```bash
==>> git elegant polish-work
git rebase --interactive @~5
```

# `prune-repository`

```bash
usage: git elegant prune-repository
```

Identifies useless branches within the current repository and removes them. A
branch is useless if it either has configured an unavailable upstream branch (1)
or does not have new commits comparing to `master` branch (2).

1 - Usually, a local branch has this state when an appropriate remote branch was
merged to a remote target branch and was removed. Since these manipulations were
made on server side, the local branch is still present, but useless.

2 - This kind of branches appears when a branch is created for some purposes but
does not have any commits nowadays. So, it is useless.

Approximate commands flow is
```bash
==>> git elegant prune-repository
git checkout master
git fetch --all
git branch --delete --force task-24
git branch --delete --force 2349
git branch --delete --force task-1
```

# `release-work`

```bash
usage: git elegant release-work [tag name]
```

Annotates the latest commit of `master` branch with a given tag and publishes
it. The tag's message will be prepopulated using commits subjects (from oldest
to newest) between the last available tag and HEAD. The release notes will be
either copied to clipboard (if `pbcopy` or `xclip` is available) or printed
to standard output using `git elegant show-release-notes`.

The command uses branch and stash pipes to preserve the current Git state prior
to execution and restore after.

Approximate commands flow is
```bash
==>> git elegant release-work 1.2.0
git checkout master
git pull --tags
git tag --annotate --file tag-message --edit 1.2.0
git push --tags
git elegant show-release-notes smart 1.1.12 1.2.0
```

# `save-work`

```bash
usage: git elegant save-work
```

Saves available changes as a new commit. You can choose which modifications
should be added. If there are trailing whitespaces, the commit won't be
performed.

Approximate commands flow is
```bash
==>> git elegant save-work
git add --interactive
git diff --cached --check
git commit
```

# `show-commands`

```bash
usage: git elegant show-commands
```

Displays all available Elegant Git commands. This is useful for completion
functions as well as for other cases when you need iteration over the available
commands.

Approximate commands flow is
```bash
==>> git elegant show-commands
start-work
show-commands
....
```

# `show-release-notes`

```bash
usage: git elegant show-release-notes [<layout> | <layout> <from-reference> | <layout> <from-reference> <to-reference>]
```

Generates a release notes using commits subjects between the given references.
The commits are ordered from oldest to newest. By default, the `from-reference`
is the last available tag, and the `to-reference` is a HEAD revision.

There are two options for a `layout`:

1. `simple` prints the messages as a plain text (default one)
2. `smart` prints the messages in a form of adopted for a git hosting. If the
hosting is unknown, the default layout is used. Now only GitHub is supported
(an HTML output).

Approximate commands flow is
```bash
==>> git elegant show-release-notes
# produces output like this
Release notes
- Add `show-release-notes` command
- Add `release-work` command
```

# `show-work`

```bash
usage: git elegant show-work
```

Shows a state of current work by displaying branch information, new commits
comparing to `master` branch, uncommitted modifications, and available
stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```

# `start-work`

```bash
usage: git elegant start-work <name>
```

Creates a new local branch based on the latest version of the default upstream
branch. If there are some uncommitted changes, they will be moved to the new
branch.

The command uses stash pipe to preserve the current Git state prior to execution
and restore after.

Approximate commands flow is
```bash
==>> git elegant start-work task-123
git stash save elegant-git
git checkout master
git pull
git checkout -b task-123
git stash apply stash^{/elegant-git}
git stash drop stash@{0}
```
