# This is a mock for system function 'read'
#!/usr/bin/env bash
set -e

read() {
    echo ""
}

export -f read
