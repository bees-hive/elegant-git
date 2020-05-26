# Approach

Elegant Git aims to standardize how a work environment should be configured. It operates several
levels of configurations (see below) that can be applied to a Git repository (local configuration)
and/or to a Git installation globally (global configuration). So,

- the local configuration applies by running
[`git elegant acquire-repository`](commands.md#acquire-repository) and configures current Git
repository by using `git config --local <key> <value>`
- the global configuration invokes by running [`git elegant acquire-git`](commands.md#acquire-git)
and uses `git config --global <key> <value>` for Git configuration

If you've applied a global configuration, there is no sense to repeat some options for a local one.
That's why the following markers explain how each particular option will be configured:

- [`b`] - configures for both configurations
- [`l`] - configures only for a local configuration
- [`g`] - configures only for a global configuration
- [`i`] - if a global configuration is applied, this option isn't used in local configuration;
otherwise, uses in local configuration

Also, there are defined [the custom configuration keys](#custom-keys) in addition to
[the standard `git config` options](https://git-scm.com/docs/git-config). These keys will be configured
automatically during `acquire-git` or `acquire-repository` execution, so, you don't need to set them
manually.

# Level: Basics

The basics configuration sets the mandatory options for the correct user-focused operation of Git and
Elegant Git. During the configuration, you will be asked to provide appropriate values. Furthermore,
if you `acquire-repository`, it proposes defaults that are set by `acquire-git`. The basics includes:

1. setting your full name usign `user.name` [`b`]
2. setting your email usign `user.email` [`b`]
3. setting a default editor using `core.editor` [`b`]
4. setting protected branches using `elegant-git.protected-branches` [`l`]
5. setting a default development branch using `elegant-git.default-branch` [`l`]

# Level: Standards

The standards configuration adopts the Git setting for painless and user-oriented commands execution
for both Git and Elegant Git. It takes into account OS-specific stuff while configuring specific
options. All Git options in this configuration have the defined values and any changes to them may
affect the designed behavior of the Elegant Git. However, it should not degrade your Git-related
experience. So, the following configuration is applied automatically:

1. `core.commentChar |` [`i`] enables lines in commit messages starting from `#` (`|` prefixes lines that should be ignored)
2. `apply.whitespace fix` [`i`] removes whitespaces when applying a patch
3. `fetch.prune true` [`i`] keeps remote-tracking references up-to-date
4. `fetch.pruneTags false` [`i`] does not remove tags while fetching until you specify it explicitly with
`git fetch --tags`
5. `core.autocrlf input` [`i`] solves issues with line endings on either MacOS/Linux with `input` or
Windows with `true`
6. `pull.rebase true` [`i`] uses `rebase` when `git pull`
7. `rebase.autoStash false` [`i`] don't use `autostash` when `git rebase`
8. `credential.helper osxkeychain` [`i`] configures default credentials storage on MacOS only
9. `elegant-git.acquired true` [`g`] identifies that Elegant Git global configuration is applied

# Level: Aliases

In order to make Elegant Git command like a native Git command, each Elegant Git command will have
an appropriate alias like `git elegant save-work` will become `git save-work`. This should
significantly improve user experience.

The configuration is a call of `git config "alias.<command>" "elegant <command>"` [`i`] for each Elegant
Git command.

# Level: Signature

This configuration aims to say Git how to sign commits, tags, and other objects you create. It starts after
all other configurations. In the beginning, all available signing keys will be shown. Then, you need to choose
the key that will be used to make signatures. If the key is provided, the configuration triggers, otherwise,
it does not apply. The signing configuration consists of

1. setting `user.signingkey` [`l`] to a provided value
2. setting `gpg.program` [`l`] to a full path of `gpg` program
3. setting `commit.gpgsign` [`l`] to `true`
4. setting `tag.forceSignAnnotated` [`l`] to `true`
5. setting `tag.gpgSign` [`l`] to `true`

For now, only `gpg` is supported. If you need other tools, please [create a new feature request][https://github.com/bees-hive/elegant-git/issues/new/choose].

# Custom keys

The Elegant Git configuration keys:

- `elegant-git.protected-branches` defines the protected branches (if there are multiple values, they
should be separarated with space). By default, the `master` branch treats as protected. The "protected"
means that Elegant Git commands for a branch state modification (such as `save-work`, `polish-work`,
etc.) are prohibited to work if the current branch is protected. Also, the protected branches cannot
be removed while running Elegant Git commands for serving a repository.
- `elegant-git.acquired` defines whether a user was applied global configuration or not (see
[approach](#approach) for the details).
- `elegant-git.default-branch` defines the name of the default development branch. By default, the `master`
branch treats as the default one. Depending on an Elegant Git command, it can be a source branch to create
a new branch from, a destination branch to rebase work in, a base branch to compare with, a release branch,
and can be used in other ways. In other words, the default development branch is "a golden source" of
modifications (commits) for any manipulation that needs them.
