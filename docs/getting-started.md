# Installation via Bash script
Run the follwing command and follow the instructions
```bash
curl https://raw.githubusercontent.com/bees-hive/elegant-git/master/install.bash | $(which bash)
```

Elegant Git will be installed in `~/.elegant-git` directory. That's why if you want to remove
the installation, just remove the directory.

# Installation via Homebrew
On macOS, you can install [Homebrew](https://brew.sh/) if you haven't already, then run
```
brew install git bees-hive/hive/elegant-git
```
The command will

- install Git as it has to be installed with Homebrew for enabling Bash/Zsh completion
- install Elegant Git
- configure Bash completion for both Git and Elegant Git

P.S. If you need Zsh completion for all Git commands, consider using
<https://raw.githubusercontent.com/zsh-users/zsh/master/Completion/Unix/Command/_git>
(see <https://github.com/bees-hive/elegant-git/blob/master/install.bash> for the details).

# Post-installation actions
Configure your environment by running [`git elegant acquire-git`](commands.md#acquire-git)
and follow the instructions. To find out more, please read
[the configuration approach](configuration.md).

You can access Elegant Git in CLI using any of
```bash
git <command>
git elegant <command>
git-elegant <command>
```
where `<command>` is one of the commands described on the [commands](commands.md) page or
printed in a terminal after running `git elegant`.

Also, please use [`git elegant clone-repository`](commands.md#clone-repository) or 
[`git elegant init-repository`](commands.md#init-repository) instead of regular `clone` or `init` 
Git's commands in order to get Elegant Git working by default.
