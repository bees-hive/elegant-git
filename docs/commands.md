# `git elegant`
Run `git elegant <command>` where `<command>` is one of

- [`acquire-repository`](#acquire-repository)
- [`clone-repository`](#clone-repository)
- [`init-repository`](#init-repository)
- [`start-work`](#start-work)
- [`save-work`](#save-work)
- [`deliver-work`](#deliver-work)
- [`accept-work`](#accept-work)
- [`pull`](#pull)
- [`clear-local`](#clear-local)
- [`commands`](#commands)

# `acquire-repository`
Configures current repository using `git config --local`. It includes:
    
- defining a user identity (name and email)
- applying git settings which are required for correct work of Elegant Git
- aliasing Elegant Git commands by making them available as Git commands

```bash
usage: git elegant acquire-repository
```

A sequence of original `git` commands:
```bash
git config --local user.name {provided name}
git config --local user.email {provided email}
git config --local core.editor {provided editor}
# "|" char starts non-message lines while writing commit message
git config --local core.commentChar |
# Remove whitespaces when apply a patch
git config --local apply.whitespace fix
# Aliasing Elegant Git commands by making them available as Git commands
git config --local "alias.<command>" "elegant <command>"
# Keeping up-to-date with both branches and tags on the remote
git config --local fetch.prune true
git config --local fetch.pruneTags true
# Rebase local changes while puling remotes refs
git config --local fetch.prune true
git config --local fetch.pruneTags
# Line ending configuration
## on MAC or Linux
git config --local core.autocrlf input
## on Windows
git config --local core.autocrlf true
# Always rebase when pull
git config --local pull.rebase true
# Always autostash if rebase
git config --local rebase.autoStash true
# Specify an external helper to be called when a username 
# or password credential is needed (MAC only)
git config --local credential.helper osxkeychain
# Remove local aliases which contain Elegant Git commands
git config --local --unset <alias>
# Add aliases for current commands
git config --local alias.<command> "elegant <command>"
```

# `clone-repository`
Clones a repository into a new directory and runs its configuration.

```bash
usage: git elegant clone-repository <URL>
```

A sequence of original `git` commands:
```bash
git clone <URL>
cd <repo root directory>
# execute commands provided by `git elegant acquire-repository`
```

# `init-repository`
Creates an empty Git repository (or reinitialize an existing one) and runs its configuration.

```bash
usage: git elegant init-repository
```

A sequence of original `git` commands:
```bash
git init
# execute commands provided by `git elegant acquire-repository`
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
git fetch --all --tags
git checkout -b __eg <remote-branch-name>
git rebase origin/master
git checkout master
git merge --ff-only __eg
git push origin master:master
git branch --delete --force __eg
git push origin --delete <remote-branch-name>
```

# `pull`
Downloads new updates for a local branch.

# `clear-local`
Removes all local branches which don't have remote tracking branches.

# `commands`
Displays all available commands.
