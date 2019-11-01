# Approach
Elegant Git aims to standardize how a Git repository should be configured. It operates 3 levels of
configurations (basics, standards, and aliases - see below) which are applied only within a
current Git repository by using `git config --local <key> <value>`.

# Basics
The basics configuration configures the following options `git config` options:

1. `user.name`
2. `user.email`
3. `core.editor`

During the configuration, you will be asked to provide appropriate values.

# Standards
The standards configuration configures a set of the `git config` options which both handle
OS-specific configurations and add specific options for the correct execution of Elegant Git
commands. It consists of

1. setting `core.commentChar` to `|` enables commit messages starting from `#`
2. setting `apply.whitespace` to `fix` removes whitespaces when applying a patch
3. setting `fetch.prune` to `true` keeps remote-tracking references up-to-date
4. setting `fetch.pruneTags` to `false` does not remove tags until you specify it explicitly
(`git fetch --tags`)
5. setting `core.autocrlf` to either `input` on MacOS/Linux or `true` on Windows solves issues with
line endings
6. setting `pull.rebase` to `true` uses `rebase` when `git pull`
7. setting `rebase.autoStash` to `false` uses `autostash` never when `git rebase`
8. setting `credential.helper` to `osxkeychain` on MacOS configures default credentials storage

# Aliases
In order to make Elegant Git command like a native Git command, each Elegant Git command will have
an appropriate alias like `git elegant save-work` will become `git save-work`. This should
significantly improve user experience.

The configuration is a call of `git config "alias.<command>" "elegant <command>"` for each Elegant
Git command.
