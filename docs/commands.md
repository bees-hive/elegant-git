# `git-elegant`
```bash

An assistant who carefully makes routine work with Git.

usage: git elegant [-h|--help|help]
   or: git elegant <command> [args]
   or: git elegant <command> [-h|--help|help]


There are commands used in various situations such as

 act with a repository
    clone-repository     Clones a repository and configures it.
    init-repository      Initializes a new repository and configures it.
    acquire-repository   Configures current repository.
    clear-local          Removes obsolete local branches.

 manage a personal work
    start-work           Creates a new branch.
    save-work            Commits current modifications.
    amend-work           Amends some changes to the most recent commit.
    deliver-work         Publishes current branch to a remote repository.

 operate a flow of work management
    obtain-work          Checkouts a remote branch matching by a name.
    accept-work          Applies a branch on top of upstream branch.

 and others
    commands             Prints available Elegant Git commands.


Please visit https://elegant-git.bees-hive.org to find out more.

```

# `accept-work`

```bash
usage: git elegant accept-work <remote branch>
```

Checkouts given branch using `git elegant obtain-work` into a temporary one.
Then, it makes a rebase of the latest version of default upstream branch with
current changes. The final index merges using fast-forward strategy into the
default local branch and pushes into the default upstream branch. After a
successful push, the given and temporary branches are removed.

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

# `acquire-repository`

```bash
usage: git elegant acquire-repository
```

A bunch of git configuration applies to current repository using
`git config --local`. The first part is an interactive configuration which
aims to set all user-specific options (identity, editor, etc.). The second batch
applies all options which are required for successful using of Elegant Git.
Finally, the last part is aliasing Elegant Git commands by making them available
as regular Git commands.

Approximate commands flow is
```bash
==>> git elegant acquire-repository
################ User-specific options ################
git config --local user.name "Dmytro Serdiuk"
git config --local user.email "dmytro.serdiuk@email.com"
git config --local core.editor vim
################ Mandatory options ################
# "|" char starts non-message lines while writing commit message
git config --local core.commentChar |
# Remove whitespaces when apply a patch
git config --local apply.whitespace fix
# Aliasing Elegant Git commands by making them available as Git commands
git config --local "alias.<command>" "elegant <command>"
# Keep remote-tracking references up-to-date
git config --local fetch.prune true
# Don't prune tags by default
git config --local fetch.pruneTags false
# Line ending configuration
## on MAC or Linux
git config --local core.autocrlf input
## on Windows
git config --local core.autocrlf true
# Always rebase when pull
git config --local pull.rebase true
# Never autostash if rebase
git config --local rebase.autoStash false
# Specify an external helper to be called when a username
# or password credential is needed (MAC only)
git config --local credential.helper osxkeychain
################ Aliases ################
# Remove local aliases which contain Elegant Git commands
git config --local --unset <alias>
# Add aliases for current commands
git config --local alias.<command> "elegant <command>"
```

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

# `clear-local`

```bash
usage: git elegant clear-local
```

Identifies local branches for which remote branches were removed. Then, it
removes them by invoking `git branch -d`. If there are unmerged branches, you
have to choose either batch or one-by-one deletion procedure using
`git branch -D`.

Approximate commands flow is
```bash
==>> git elegant clear-local
git branch -d task-24
git branch -d 2349
git branch -D task-1
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

# `commands`

```bash
usage: git elegant commands
```

Displays all available commands. This is useful for completion functions as well
as for other cases when you need iteration over the available commands.

Approximate commands flow is
```bash
==>> git elegant commands
echo <command>
echo ...
```

# `deliver-work`

```bash
usage: git elegant deliver-work
```

Updates the current branch by rebasing the default upstream branch. Then,
it pushes HEAD to appropriate upstream branch. The name of remote branch is
equal to the local one.

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

# `start-work`

```bash
usage: git elegant start-work <name>
```

Creates a new local branch based on the latest version of the default upstream
branch. If there are some uncommitted changes, they will be moved to the new
branch.

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
