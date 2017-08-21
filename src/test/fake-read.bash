# This is a mock for system function 'read'
#!/usr/bin/env bash
set -e

read() {
    sleep 0
}

export -f read
