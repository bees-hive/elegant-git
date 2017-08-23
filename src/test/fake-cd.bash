# This is a mock for system function 'cd'
#!/usr/bin/env bash
set -e

cd() {
    sleep 0
}

export -f cd
