# cloudzy-guix-install

## Preparation

### On local machine

where repo is cloned, get the shasum256 of the main run-remote-steps.sh

Substitute your command for pbcopy if not on a mac

To check and record on the iso instance:

`shasum -a 256 run-remote-steps.sh | pbcopy`

Preparation to manually check

`shasum -a 256 run-remote-steps.sh`

### In guix iso environment

perl for shasum

```bash
guix install curl
guix install perl
```

## Quick Start

### To leave copy of shasum in temp iso environment

Download and verify the script (do preparation above first)

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
echo " PASTE-YOUR-SHASUM-HERE-WITH-NO-SPACES-INSIDE-THESE-QUOTES " > rrs-checksum.txt
shasum -a 256 -c rrs-checksum.txt
chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```

### Manual Verification Instead

If you prefer to verify manually:

```bash
curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/run-remote-steps.sh -o run-remote-steps.sh
shasum -a 256 run-remote-steps.sh
chmod +x run-remote-steps.sh
bash ./run-remote-steps.sh
```