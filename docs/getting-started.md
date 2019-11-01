# Several important words
You can install Elegant Git by either executing `bash` script or using `homebrew`. After the
installation, please run [`git elegant acquire-repository`](commands.md#acquire-repository)
for an existing repository to apply [the needed Git configuration](configuration.md).

Run in CLI any of
```bash
git <command>
git elegant <command>
git-elegant <command>
```
where `<command>` is one of the available commands which are described on [commands](commands.md)
page or are printed in a terminal by running `git elegant`.

Also, please use [`git elegant clone-repository`](commands.md#clone-repository) or 
[`git elegant init-repository`](commands.md#init-repository) instead of regular `clone` or `init` 
Git's commands in order to get Elegant Git working by default.

# `bash` installation
Run `curl https://raw.githubusercontent.com/bees-hive/elegant-git/master/install.bash | $(which bash)`
and follow the provided instructions to install the tool. `${HOME}/.elegant-git` directory will host
all required files. That's why if you want to remove installation, you need to remove this directory
only (`rm -r ${HOME}/.elegant-git`).

Elegant Git's BASH completion does not work without regular Git BASH completion. If you don't have
it, please follow <https://github.com/bobthecow/git-flow-completion/wiki/Install-Bash-git-completion>
in addition to Elegant Git's configuration.

# Homebrew installation
On macOS, you can install [Homebrew](https://brew.sh/) if you haven't already, then run
`brew install git` (we must install Git with Homebrew in order to have a working BASH completion)
and `brew install bees-hive/hive/elegant-git` (please visit
<https://github.com/bees-hive/homebrew-hive> if you need details).
