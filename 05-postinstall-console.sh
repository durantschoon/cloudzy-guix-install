#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# ... logged on from console for first time ...

passwd 
herd start ssh-daemon

# then can ssh in as root@<ip-addr> with new password from my own terminal 
