Elegant git
===========
`git elegant` is a `git` extension which allows simplifying of `git` interaction.

`git elegant` always says which original git command will be executed.

Philosophy
----------
- declarative interaction
- no merge commits
- no pushes to `master` branch

Limitations
-----------
Support only one default remote - `origin`.

Commands
--------
### feature
Creates a new branch based on `master`. If there are some uncommitted changes, they will be moved to the new branch.

### pull
Downloads new updates for a local branch.

### push
Upload current local branch to a remote one. If the remote branch is absent, it will be created. Pushing to remote `master` aren't allowed.

### rebase
Reapplies commits on top of the latest `origin/master`.

### push-after-rebase
Executes [git elegant push](#push) after [git elegant rebase](#rebase).

### init
Creates an empty Git repository or reinitialize an existing one. Then runs local repository configuration.

### clone
Clone a repository into a new directory. Then runs local repository configuration.

### add
Adds file contents to the index interactively.

### clear-local
Removes all local branches which don't have remote tracking branches.

### configure
Defines some settings for both _global_ and _local_ `git config`.

### check
Shows trailing whitespaces of uncommitted changes.

### commands
Displays all available commands.

Exit codes
----------
`81` - a philosophical constraint
