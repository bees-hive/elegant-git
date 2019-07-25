`git elegant <command>` where `<command>` is one of

- `clone-repository`
- `start-work`
- `save-work`
- `deliver-work`
- `accept-work`
- `pull`
- `init`
- `add`
- `clear-local`
- `configure-repository`
- `check`
- `save`

# `clone-repository`
Clones a repository into a new directory and runs its configuration.

```bash
usage: git elegant clone-repository <URL>
```

A sequence of original `git` commands:
```bash
git clone <URL>
cd <repo root directory>
# execute commands provided by `git elegant configure-repository`
```

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

# `save-work`
Saves available changes (or a part of them) making a `git` commit.

```bash
usage: git save-work
```

A sequence of original `git` commands:
```bash
git add --interactive
git diff --cached --check
git commit
```

# `deliver-work`
Updates the current branch using the latest remote `master` and updates remote refs.

```bash
usage: git deliver-work
```

A sequence of original `git` commands:
```bash
git fetch
git rebase origin/master
git push --set-upstream --force origin {local branch name}:{local branch name}
```

# `accept-work`
Accepts proposed work (remote work branch) on top of the fresh history of remote `master` (using
`rebase`) and publishes work to `origin/master`. Also, it removes the remote work branch in case of
successful work acceptance.

```bash
usage: git elegant accept-work <remote-branch>
```
A sequence of original `git` commands:
```bash
git fetch --all --tags --prune
git checkout -b eg origin/master
git rebase --merge --strategy ff-only <remote-branch-name>
git checkout master
git merge --ff-only eg
git push origin master:master
git branch -d eg
git push origin --delete <remote-branch-name>
```

# `pull`
Downloads new updates for a local branch.

# `init`
Creates an empty Git repository or reinitialize an existing one. Then runs local repository configuration.

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
