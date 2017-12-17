Usage
=====
Installation
------------
Use `./install.bash dev` to get an installation from the current sources.

Debug mode
----------
Use `export GED=1` to switch on the debug of `git elegant` or `unset GED` to switch off. 

Unit tests
==========
[bats](https://github.com/sstephenson/bats) is used for unit testing. 

Addons
------
Add the following line to the test file if the extension is required:
- `load addons-common` to have the working test (**mandatory**)
- `load addons-git` to interact with real git repository (**optional**)
- `load addons-fake` to fake a Linux command (**optional**)
- `load addons-cd` to fake `cd` command (**optional**)
- `load addons-read` to fake `read` command (**optional**)

Writing tests
-------------
1. **Use `setup()` or `teardown()`** bats methods only in the tests.
2. Use **`check` instead of bats `run`** to execute a command to be tested.
3. If `addons-fake` or `addons-git` is used, call `clean-fake` or `clean-git` within a `teardown()` method.

Assertions
----------
- `[ "${lines[0]}" = "+ the space " ]` for a output line (index starts from 0)
- `[ "$status" -eq 2 ]` for a command status
- `[ "${#lines[@]}" -eq 0 ]` for an empty command output

Test name template
------------------
Use the following test name template - `'<command args>': <describe what will be tested>` like `'check -s': trailing spaces in the staged changes`.

Run
---
Use one of the following commands to run the unit tests:
- `bats src/test`
- `./run-tests`
- `docker run -t --rm -v $PWD:/src -w /src extsoft/rultor-runtime:r24b04p0g2 ./run-tests`
