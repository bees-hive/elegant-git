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
- `load addons-cd` to fake `cd` unix command (**optional**)
- `load addons-read` to fake `read` unix command (**optional**)

Bats restrictions
-----------------
1. **Don't use `setup()` or `teardown()`** bats methods.
2. Use **`check` instead of bats `run`** to execute a command to test.

Assertions
----------
- `[ "${lines[0]}" = "+ the space " ]` for a output line (index starts from 0)
- `[ "$status" -eq 2 ]` for a command status
- `[ "${#lines[@]}" -eq 0 ]` for an empty command output

Test name template
------------------
Use the following test name template - `'<command args>': <describe what will be tested>` like `'check -s': trailing spaces in the staged changes`.

@todo #89 Apply test name template for all unit tests. Please update assertions if required.

Run
---
Use one of the following commands to run the unit tests:
- `bats src/test`
- `./run-tests`
- `docker run -t --rm -v $PWD:/src -w /src extsoft/rultor-runtime:r24b04p0g2 ./run-tests`
