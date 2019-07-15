`git elegant <command>` where `<command>` is one of

- `start-work`
- `pull`
- `push`
- `push-after-rebase`
- `rebase`
- `init`
- `acquire-repository`
- `add`
- `clear-local`
- `configure-repository`
- `check`
- `save`

# `start-work`
Creates a new local branch based on latest version of `master`. If there are some uncommitted
changes, they will be moved to the new branch.

```bash
usage: git elegant start-work <branch-name>
```

A sequence of original `git` commands:
```bash
git checkout master
git fetch --tags
git pull
git checkout -b <branch-name>
```

# `pull`
Downloads new updates for a local branch.

# `push`
Upload current local branch to a remote one using `force` push. If the remote branch is absent, it will be created. Pushing to remote `master` isn't allowed.

# `rebase`
Reapplies commits on top of the latest `origin/master`.

# `push-after-rebase`
Executes [git elegant push](#push) after [git elegant rebase](#rebase).

# `init`
Creates an empty Git repository or reinitialize an existing one. Then runs local repository configuration.

# `acquire-repository`
Clone a repository into a new directory. Then runs local repository configuration.

# `add`
Adds file contents to the index interactively.

# `clear-local`
Removes all local branches which don't have remote tracking branches.

# `configure-repository`
Defines some settings for _local_ `git config`.

# `check`
Shows trailing whitespaces of uncommitted changes.

# `commands`
Displays all available commands.

# `save`
Saves the changes to the current branch as a commit.
