Elegant git
===========
"Elegant git" is an extension which simplifies daily routine with `git`. Please visit <https://elegant-git.bees-hive
.org/> to read more.

[![MIT license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/bees-hive/elegant-git/blob/master/LICENSE)
[![Build Status](https://travis-ci.org/bees-hive/elegant-git.svg?branch=master)](https://travis-ci.org/bees-hive/elegant-git)
[![PDD status](http://www.0pdd.com/svg?name=bees-hive/elegant-git)](http://www.0pdd.com/p?name=bees-hive/elegant-git)

[![Rultor.com](http://www.rultor.com/b/bees-hive/elegant-git)](http://www.rultor.com/p/bees-hive/elegant-git)

## Contributing

### Coding rules
We enforce having a consistent implementation by following the next strict rules:
- add `#!/usr/bin/env bash` at the beginning of each script
- use `boxtee` to execute each original `git` command 

### Debug mode
You can enable debug mode by running `export GED=1` (the equivalent of `set -x` for `bash`). 
Run `unset GED` to switch debug off. 

### Testing procedure
A testing procedure consists of 3 steps:
1. unit testing using [bats](https://github.com/sstephenson/bats)
2. installation testing
3. validation of todo' correctness (for [0pdd](http://www.0pdd.com/p?name=bees-hive/elegant-git))

All these steps can be executed by
`docker run -it --rm -v $PWD:/eg beeshive/elegant-git-ci:2 ./run-tests`.

`beeshive/elegant-git-ci` Docker image is also used on CI. If the image requires modifications,
it has to be updated manually by the following instructions:
1. update `Dockerfile` (including `version` and `description`)
2. `docker build -t beeshive/elegant-git-ci:<version> .`
3. `docker push beeshive/elegant-git-ci:<version>`

### Unit testing
#### Addons
In order to have a working unit tests, you need to add `load addons-common` line to each `.bats`
file. This addon configures right access to executables (`libexec` directory) and defines mandatory
functions.

Also, there are several optional addons which can be useful in some circumstances:
- add `load addons-git`  to interact with real git repository
- add `load addons-fake` to fake a Linux command
- add `load addons-cd`   to fake `cd` command
- add `load addons-read` to fake `read` command

#### Writing tests
1. **Use `setup()` or `teardown()`** bats methods only in the tests.
2. Use **`check` instead of bats `run`** to execute a command to be tested.
3. If `addons-fake` or `addons-git` is used, call `clean-fake` or `clean-git` within a `teardown()` method.

#### Assertions
- `[ "${lines[0]}" = "+ the space " ]` for a output line (index starts from 0)
- `[ "$status" -eq 2 ]` for a command status
- `[ "${#lines[@]}" -eq 0 ]` for an empty command output

#### Test name template
Use the following test name template - `'<command args>': <describe what will be tested>` like
`'acquire-repository': raise an error if cloneable URL isn't set`.

### Documentation preview
In order to get the documentation preview locally, please install required dependencies with 
`pip install -r docs/requirements.txt`. After, run `mkdocs serve` and open <http://127.0.0.1:8000/> 
in a browser. That's it!
