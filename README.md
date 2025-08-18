# cloudzy-guix-install

## Preparation

On local machine where repo is cloned, get the shasum256 of the main run-remote-steps.sh

Note: We can't put the actual shasum in this readme because the act of commiting to main will change the shasum value.
So this is how we'll do it:

On a mac:

`shasum -a 256 run-remote-steps.sh | pbcopy`

## Quick Start

Download and verify the script (do preparation above first)

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
echo "PASTE-YOUR-SHASUM-HERE" > rrs-checksum.txt
shasum -a 256 -c rrs-checksum.txt
chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```

## Manual Verification

If you prefer to verify manually:

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
shasum -a 256 run-remote-steps.sh
# Should output: 0dd2152dc1d1133ec4eba5a72516b1d59fc1223a365340ffe313019d1fcee8238  run-remote-steps.sh
chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```

## Prerequisites

- `curl` (install with `guix install curl` if not available)
- `shasum` (install with `guix install perl` if not available)
- Internet connection to download scripts
