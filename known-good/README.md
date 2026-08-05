# known-good/

Configurations that **actually booted a real machine**, captured from that
machine's own record of itself.

## Why this is separate from the rest of the repo

Everything else here answers *"how do we install Guix on this class of
hardware, generically and repeatably?"* Those files are written for a machine
nobody has looked at yet: the device is auto-detected, the hostname is a
template, the partition layout is discovered.

This directory answers a narrower question: *"what exactly was on the disk of
the one machine where this demonstrably worked?"*

Keeping the two apart matters in both directions.

- A generic installer that quietly grows machine-specific facts stops being
  generic, and the breakage surfaces on someone else's laptop.
- A known-good record that gets "improved" stops being a record. Its entire
  value is that it is the artifact that booted -- not the artifact we
  currently believe is best.

**Nothing here is an input to the installer.** No build step reads this
directory. It exists to be compared against, quoted from, and rolled back to.

This is the same line `CHECKLIST.md` draws when it scopes keyd, the FHS loader
shim, and personal dotfiles out of the generic installer: those are one
machine's answers, not the project's.

## These are captured, not written

The files here are not hand-maintained copies. They come from Guix's own
`provenance-service-type`, which records into every system closure:

| File | What it is |
|---|---|
| `configuration.scm` | the exact `operating-system` config that built the generation |
| `channels.scm` | the exact channel commits it was built from |
| `provenance` | the s-expression tying those two together |

That is a much stronger claim than "the config file that happened to be in my
home directory at the time." A home-directory copy drifts the moment you edit
it for the next attempt. The closure's copy is immutable, and it is what the
kernel actually booted.

**Capture is therefore time-sensitive.** A generation's provenance lives only
inside that generation. Delete the generation -- `guix system
delete-generations`, or a `guix gc` once it is no longer current -- and the
record goes with it. Capture before pruning, not after.

## Capturing

On the target machine, from a checkout of this repo:

    known-good/capture-provenance.scm --name framework-dual-geeeks

Defaults to the *current* generation. To capture an older one still on disk:

    known-good/capture-provenance.scm --name framework-dual-geeeks --generation 1

Then commit the result. The script writes an `ATTESTATION.md` stub next to the
captured files. Fill it in by hand: "it booted" and "it works" are different
claims, and only a person who used the machine can make the second one.

## Layout

    known-good/
      README.md                      this file
      capture-provenance.scm         the capture tool (runs on the target)
      <machine>/
        configuration.scm            captured, never edited
        channels.scm                 captured, never edited
        provenance                   captured, never edited
        ATTESTATION.md               hand-written: what was verified, and what was not

One directory per machine-and-milestone. If the same machine reaches a second
milestone worth recording, capture it under a new name rather than overwriting
the first. A known-good record that moves is not a record.
