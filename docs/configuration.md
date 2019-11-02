# Approach
Elegant Git aims to standardize how a work environment should be configured. It operates 3 levels
of configurations (basics, standards, and aliases - see below) which can be applied to a Git
repository (local configuration) and (or) to a Git installation globally (global configuration).

The local configuration applies by running
[`git elegant acquire-repository`](commands.md#acquire-repository) and configures current Git
repository by using `git config --local <key> <value>`.

The global configuration invokes by [`git elegant acquire-git`](commands.md#acquire-git) and uses
`git config --global <key> <value>` for Git configuration.

If you've applied a global configuration, there is no sense to repeat some options for a local one.
That's why the following markers explain how each particular option will be configured:

- `b` - configures for both configurations
- `l` - configures only for a local configuration
- `g` - configures only for a global configuration
- `i` - if a global configuration is applied, an option isn't used in local configuration;
otherwise, uses in local configuration

# Basics
The basics configuration configures the following options `git config` options:

1. `user.name` (`b`)
2. `user.email` (`b`)
3. `core.editor` (`i`)

During the configuration, you will be asked to provide appropriate values.

# Standards
The standards configuration configures a set of the `git config` options which both handle
OS-specific configurations and add specific options for the correct execution of Elegant Git
commands. It consists of

1. setting `core.commentChar` (`i`) to `|` enables commit messages starting from `#`
2. setting `apply.whitespace` (`i`) to `fix` removes whitespaces when applying a patch
3. setting `fetch.prune` (`i`) to `true` keeps remote-tracking references up-to-date
4. setting `fetch.pruneTags` (`i`) to `false` does not remove tags until you specify it explicitly
(`git fetch --tags`)
5. setting `core.autocrlf` (`i`) to either `input` on MacOS/Linux or `true` on Windows solves issues with
line endings
6. setting `pull.rebase` (`i`) to `true` uses `rebase` when `git pull`
7. setting `rebase.autoStash` (`i`) to `false` uses `autostash` never when `git rebase`
8. setting `credential.helper` (`i`) to `osxkeychain` on MacOS configures default credentials storage
9. setting `elegant.acquired` (`g`) to `true` identifies that Elegant Git global configuration is applied

# Aliases
In order to make Elegant Git command like a native Git command, each Elegant Git command will have
an appropriate alias like `git elegant save-work` will become `git save-work`. This should
significantly improve user experience.

The configuration is a call of `git config "alias.<command>" "elegant <command>"` (`i`) for each Elegant
Git command.
