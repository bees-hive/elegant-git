# `git-elegant`
```

An assistant who carefully automates routine work with Git.

usage: git elegant [-h | --help | help | --version | version]
   or: git elegant <command> [-h | --help | help]
   or: git elegant <command> [--no-workflows] [args]
   or: git elegant [--no-workflows] <command> [args]

    -h, --help, help    displays help
    --version, version  displays program version
    --no-workflows      disables available workflows

There are commands used in various situations such as

 enable Elegnat Git services
    acquire-git          Configures your Git installation.
    acquire-repository   Configures the current local Git repository.
    clone-repository     Clones a remote repository and configures it.
    init-repository      Initializes a new repository and configures it.

 serve a repository
    prune-repository     Removes useless local branches.

 enhance contribution rules
    show-workflows       Prints file locations of the configured workflows.
    make-workflow        Makes a new workflow file.
    polish-workflow      Opens a given workflow file.

 make day-to-day contributions
    start-work           Creates a new branch.
    save-work            Commits current modifications.
    amend-work           Amends the last commit.
    show-work            Prints HEAD state.
    polish-work          Rebases HEAD interactively.

 interact with others
    deliver-work         Publishes HEAD to a remote repository.
    obtain-work          Checkouts a remote-tracking branch.

 manage contributions
    accept-work          Adds modifications to the default development branch.
    release-work         Releases the default development branch.
    show-release-notes   Prints a release log between two refs.

 and others
    show-commands        Prints Elegant Git commands.


Please visit https://elegant-git.bees-hive.org to find out more.

```

# `accept-work`

```bash
usage: git elegant accept-work <branch>
```

Checkouts a given branch into a temporary one and rebases the head of the
default development branch, checkouts the default development branch and
makes a fast-forward merge of the temporary branch, and removes the temporary
branch.

A `<branch>` can be either a local branch or a remote-tracking branch or a
pattern. The pattern should be a string that is a part of the remote-tracking
branch name. It is used to search the desired branch across the remotes for
processing (as under the hood the pattern is passed to
`git elegant obtain-work`, please refer to that command for the details).

If the local repository is associated with a remote one, then the remote
default development branch is used instead of local one as well as it gets
fetched prior to the command execution and pushed after.

If a given branch name has a full match with a local one, then the local branch
is used, otherwise, the command acts with a remote one to perform checkout and
deletion. As the command works with all configured remote repositories, the
deletion is performed only for `origin`.

If there is a rebase in progress initiated by this command, it will be
continued instead, otherwise, the command stops.

The command uses branch and stash pipes to preserve the current Git state prior
to execution and restore after.

Approximate commands flow is
```bash
==>> git elegant accept-work task-123
git fetch --all
git checkout --force -B __eg origin/task-123
git rebase origin/master
git checkout master
git merge --ff-only __eg
git push origin master:master
git branch --delete --force __eg
git push origin --delete task-123
```

# `acquire-git`

```bash
usage: git elegant acquire-git
```

Applies the "basics", "standards", and "aliases" configurations to the current
Git installation using `git config --global`.

The command may ask to provide some information that is needed for Git
configuration. All further executions require fewer inputs as Elegant Git
reuses previous values and will require inputs in case of new configuration
options are added.

To find out what will be configured, please visit
<https://elegant-git.bees-hive.org/en/latest/configuration/>

# `acquire-repository`

```bash
usage: git elegant acquire-repository
```

Applies the "basics", "standards", "aliases", and "signature" configurations
to the current Git repository using `git config --local`. The command asks to
provide information that is needed for the current repository configuration.

The behavior of the command varies depend on `git elegant acquire-git`
execution (a global configuration). If the global configuration is applied,
then this command configures repository-related staffs only, otherwise, it
applies all configurations to the current local repository.

To find out what will be configured, please visit
<https://elegant-git.bees-hive.org/en/latest/configuration/>

# `amend-work`

```bash
usage: git elegant amend-work
```

Amends the last commit by incorporating the current state of the repository.
The command provides full control of what modifications should be included by
starting an interactive commit process.

The command doesn't allow to modify the history of the default development branch.

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

A `<repository>` is an Git URL to the repository to clone from (see
`git clone --help` for the details).

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

Rebases the head of the remote default development branch into the current one
and pushes the branch to the remote default upstream repository.

A `[branch-name]` name is a name of a remote-tracking branch in the remote
default upstream repository. By default, it is equal to the name of the current
local branch. However, if HEAD has a configured remote-tracking branch, it will
be used. But if the `[branch-name]` argument is provided, it will be used as
the name of the remote-tracking branch.

If there is a rebase in progress, the command will pick it up as a part of the
execution flow.

If the push output contains an URL (like a link to create a pull request), it
will be open (in case if `open` command is available) in a default browser.

The command uses stash pipe to preserve the current Git state prior to execution
and restore after.

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

Creates an empty Git repository (or reinitialize an existing one), runs its
configuration, and creates an initial empty commit.

Approximate commands flow is
```bash
==>> git elegant init-repository
git init
git elegant acquire-repository
git commit --allow-empty --file a-message-of-initial-commit
git show
```

# `make-workflow`

```bash
usage: git elegant make-workflow <command> <type> <location>
```

Makes a new workflow file for a given Elegant Git command and opens it for the
further scripting in the default text editor. The created file will have a
shebang line pointing to `#!/usr/bin/env sh -e` and executable permissions.
That's why the created file becomes an executable one by default. All needed
directories will be created if any.

A `<command>` is a name of Elegant Git command.

A `<type>` defines whether it should be executed `ahead` or `after` the
given command.

A `<location>` defines the availability such as personal (located at
`.git/.workflows`) or common (located at `.workflows`).

Approximate commands flow is
```bash
==>> git elegant make-workflow show-work ahead common
mkdir -p .workflows
touch .workflows/show-work-ahead
chmod +x .workflows/show-work-ahead
vim .workflows/show-work-ahead
```

# `obtain-work`

```bash
usage: git elegant obtain-work <name> [local branch]
```

Seeks across all remote repositories for a branch matching with a given name
and checkouts it.

A `<name>` is a full or a partial name (a pattern) of the remote-tracking
branch. If the pattern is specified and there are more then 1 matching branch,
the execution stops with a corresponding error message.

A `[local branch]` is the name of the local branch in the local repository
to checkout into. By default, it responds to the name of the remote-tracking
branch. However, if this argument is provided, the given name is set for the
local branch.

Approximate commands flow is
```bash
==>> git elegant obtain-work 133 task-133
git fetch --all
git checkout -B task-133 custom-remote/feature/133
```

# `polish-work`

```bash
usage: git elegant polish-work
```

Finds the new commits in HEAD (a delta in commits between the current and the
default development branches) and runs interactive rebase against them.

If there is a rebase in progress, the command will pick it up and continue.

The command doesn't allow to modify the history of the default development
branch.

The command uses stash pipe to preserve the current Git state prior to
execution and restore after. This means that the uncommitted local
modifications, if they are present, will be stashed. And they will be
uncovered if the rebase completes without errors or interruptions like for
solving conflicts, etc. Otherwise, please run `git stash pop` to get them.

Approximate commands flow is
```bash
==>> git elegant polish-work
git rebase --interactive @~5
```

# `polish-workflow`

```bash
usage: git elegant polish-workflow <file path>
```

Opens a given workflow file in the default text editor. If given file is not
present, the command raises an error.

A `<file path>` is a path to the desired workflow file.

Approximate commands flow is
```bash
==>> git elegant polish-workflow .workflows/show-work-ahead
vim .workflows/show-work-ahead
```

# `prune-repository`

```bash
usage: git elegant prune-repository
```

Identifies useless branches within the current repository and removes them. A
branch is useless if it either has an unavailable remote-tracking branch (1)
or does not have new commits comparing to the default development branch (2).

1 - Usually, a local branch has this state when an appropriate remote-tracking
branch was merged and removed. As these manipulations were made on the server
side, the local branch is still present, but useless.

2 - This kind of branches appears when a branch is created for some purposes but
does not have any commits nowadays. So, it is useless.

Approximate commands flow is
```bash
==>> git elegant prune-repository
git checkout master
git fetch --all
git rebase
git branch --delete --force task-24
git branch --delete --force 2349
git branch --delete --force task-1
```

The command works even if the remotes are unavailable.

# `release-work`

```bash
usage: git elegant release-work [name]
```

Annotates the head commit of the default development branch by adding anotated
tag, publishes it to the remote repository, and prepares release notes.

A `[name]` is a desired name for the new tag. If it isn't provided, the
command will ask it interactively.

The command generates a tag's message that has to be polished and saved. The
message is a list of commits titles generated based on commits between the last
tagged commit (not included) and head one (included) ordered from oldest to
newest. If there are no tags in the repository, then all commits are used to
generate the tag's message.

The release notes are the output of `git elegant show-release-notes smart ...`
command. They are either copied to the clipboard (if `pbcopy` or `xclip`
commands are available) or printed to the standard output.

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

Saves available modifications as a new commit. The command provides full
control of what modifications should be included by starting an interactive
commit process.

The command doesn't allow to add a commit to the default development branch.

If there are trailing whitespaces in the modifications, the commit is rejected.

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

Prints all available Elegant Git commands to the standard output. This command
is useful for completion functions as well as for other cases when you need
iteration over the available commands.

Approximate command's output is
```bash
==>> git elegant show-commands
start-work
show-commands
....
```

# `show-release-notes`

```bash
usage: git elegant show-release-notes [<layout>] [<from-ref>] [<to-ref>]
```

Prints commit titles of the commits between the given refs ordered from oldest
to newest.

A `layout` defines a formatting for the output - `simple` or `smart`. The
`simple` layout prints the messages as a plain text (the default one) while
the `smart` one prints the messages in an adopted form for a Git hosting
service. If the hosting service is unknown, the default layout is used.

For Github, the `smart` layout prints an HTML that includes HTML links to the
commits and issues if possible.

A `from-ref` is a ref to start work from (not included to the output). By
default, it is the last tag. If there are no tags, the command uses the first
commit in HEAD.

A `to-ref` is a ref to stop work with (included to the output). By default,
it is a HEAD.

Approximate command's output is
```bash
==>> git elegant show-release-notes
Release notes
- Add `show-release-notes` command
- Add `release-work` command
```

# `show-work`

```bash
usage: git elegant show-work
```

Prints HEAD state by displaying local and remote-tracking (if available) refs,
commits that aren't in the default development branch, uncommitted
modifications, and available stashes.

Approximate commands flow is
```bash
==>> git elegant show-work
git log --oneline master..@
git status --short
git stash list
```

# `show-workflows`

```bash
usage: git elegant show-workflows
```

Prints all personal and common workflows files that are available in the
repository.

There are two types of workflows. The personal workflows are located in
`.git/.workflows` directory while the common ones are in \'.workflows`.
All directories are located relatively to the repository root directory.

Approximate command's output is
```bash
==>> git elegant show-workflows
.git/.workflows/accept-work-after
.workflows/amend-work-ahead
.workflows/amend-work-after
.workflows/release-work-after
.workflows/save-work-ahead
.workflows/save-work-after
....
```

# `start-work`

```bash
usage: git elegant start-work <name> [from-ref]
```

Creates a new local branch based on the head of the default development branch.

A `<name>` is the name of the new branch.

A `[from-ref]` overrides the default development branch with a given one.

If there is a remote repository, the default development branch is updated
prior to creating a new one. However, if the remote repository is unavailable,
the local branch is used.

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
