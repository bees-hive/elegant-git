# About
Elegant Git is an assistant who carefully makes routine work with Git.

Used philosophy

- actual work is allowed in custom branches only
- all pushes to `master` are strictly controlled
- all Git commands being executed are printed
- new work applies on top of latest available (`rebase` instead of `merge`)
- declarative interaction matters

> _Sounds interesting? Go to [getting started](getting-started.md) guide or take a look for
available [commands](commands.md)._

Elegant Git encourages using of
[GitHub flow](https://guides.github.com/introduction/flow/). However, you can try to apply it to
your branching strategy as well.

# Known limitations
Support only one default remote - `origin`.

# Exit codes
Usually, Elegant Git translates exit codes of original Git commands. However, in some cases,
it may face its own errors and raises them as

- `81` - a philosophical constraint
