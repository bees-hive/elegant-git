# About
Elegant Git is an assistant who carefully automates routine work with Git.

Main capabilities

- unified local full-cycle work management for any Git repository
- local flexible continuous integration (aka [workflows](#workflows))

Used philosophy

- actual work is allowed in custom branches only
- all pushes to `master` are strictly controlled
- all commands, which modify a state of Git, are printed
- declarative interaction matters

<center><iframe width="560" height="315" src="https://www.youtube.com/embed/Py6bpwJw30I"
frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope;
picture-in-picture" allowfullscreen></iframe></center>

> _Looks interesting? Go to [getting started](getting-started.md) guide or take a look for
available [commands](commands.md)._

# Workflows
While developing something, it may be required to format code prior to committing changes or to open
several URLs to report release notes after a new release. All these and similar actions, which you
have to perform in addition to git actions, are your **_workflows_**. And Elegant Git allows
automating them (it's like [Git Hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks),
but for Elegant Git commands).

There are personal and common actions. The personal actions are stored in `.git/.workflows/`
directory while the common actions are located in `.workflows/` directory within the repository.
That's why there is an ability to split what should be configured for any contributor and what's for
you personally.

An action will be automatically triggered if an executable file is placed in a directory for either
personal or common actions. The executable file name has `<Git Elegant command>-<type>` pattern
where `<Git Elegant command>` is one of the [commands](commands.md) and `<type>` is either `ahead`
(runs prior to the command) or `after` (runs after to the command).

A sample workflow execution:
```bash
==>> git elegant save-work
.git/.workflows/save-work-ahead
.workflows/save-work-ahead
# the command itself
.git/.workflows/save-work-after
.workflows/save-work-after
```

❗Please take into that if an action returns a non-zero exit code, the execution of the workflow
will be interrupted.

If you want to skip workflows for the current command execution, just use `--no-workflows` option
like `git elegant --no-workflows save-work`.

# Known limitations
Support only one default remote - `origin`.

# Exit codes
Usually, Elegant Git translates exit codes of original Git commands. However, in some cases,
it may face its own errors and raises them as

- `0` - a successful execution
- `42` - a philosophical constraint
- `43` - a logical constraint
- `45` - a required parameter is empty
- `46` - an unknown command
