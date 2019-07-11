# Overview
"Elegant git" is an extension which simplifies daily routine with `git`. It encourages using of
[GitHub flow](https://guides.github.com/introduction/flow/). However, you can try to apply it to
your branching strategy as well.

# Philosophy
- declarative interaction (usually each command consists of several `git` commands)
- flat commits tree (no merge commits; `git rebase` instead of `git merge`) 
- frozen main branch (disallow direct pushes to `master`)

In addition, "Elegant git" is always saying which original `git` commands were executed.

# Known limitations
Support only one default remote - `origin`.

# Exit codes
Usually "Elegant git" translates exit codes of original `git` commands. However, in some cases,
it may face its own error and raises them as

- `81` - a philosophical constraint
