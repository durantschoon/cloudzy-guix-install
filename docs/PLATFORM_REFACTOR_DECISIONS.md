# Platform Vocabulary and Modality Refactor — Decisions

Decided 2026-08-08. This is the binding record for the vocabulary and structural
refactor of this repo. It supersedes the reasoning that lived only in a side
channel; nothing here depends on that channel remaining readable.

Status: decisions are final unless marked **OPEN**. The checklist at the bottom
is the implementation order. Items should migrate into `CHECKLIST.md` as they are
picked up — they are held here for now to avoid colliding with in-flight edits to
that file.

## The problem being fixed

A vendor name was doing duty as a name for a class of installs, at two levels.
`cloudzy-guix-install` named a repo that covers four platforms; `cloudzy` names a
platform whose actual defining properties are "x86_64 VPS, whole-disk wipe, free
software only, ISO boot with console access" — Cloudzy is merely the first vendor
that instantiated it. The `C:`/`F:`/`R:` feedback shorthand is a third instance:
a parallel table that has already drifted out of agreement with the installer set
it routes to.

The failure mode is always the same, and it is the one `make check-system-hosts`
exists to prevent in the `dot_files` repo: **a name that quietly points at the
wrong thing.**

## The axis set

Four independent axes. This replaces an earlier draft that carried "form factor"
and "licensing" as axes.

| Axis | Values |
|---|---|
| **modality** | `iso-interactive` \| `image-prebuilt` |
| **arch** | `x86_64` \| `aarch64` |
| **target** | vendor/board identity — the profile key |
| **disk policy** | `wipe` \| `preserve` |

Three deliberate exclusions:

- **Form factor dropped.** "laptop / VPS / board" carries no information the
  target identity does not already imply. Redundant coordinates are what let
  labels disagree with reality.
- **Licensing demoted** to a field in the profile record. Whether nonguix is
  needed is mostly a consequence of the target — a Framework needs firmware
  blobs, a virtio VPS does not. It is a per-profile decision, not a free
  coordinate that multiplies the space.
- **Disk policy added**, because the earlier draft missed it. `framework` versus
  `framework-dual` differ on *exactly* this and nothing else. That is the axis
  the missing fourth shorthand letter was hiding.

**Modality is the top-level structural axis.** `iso-interactive` and
`image-prebuilt` share almost none of the install *sequence* — there is no
interactive partitioning in the image case — while sharing config generation, the
postinstall layer, and verification/manifest/provenance. Factor along that seam
rather than building a generic `cloud/` that pretends the sequences match.

**Vendor differences are data, not code paths.** A profile record (disk device
pattern, firmware mode UEFI/BIOS, serial-console kernel arguments, network
configuration method, whether nonguix is needed, arch, minimum RAM) consumed by
shared steps, rather than a per-vendor fork of those steps. This extends the
existing `CLAUDE.md` rule about factoring common code into `lib/common.go`; it
does not introduce a new convention.

**Define the space, enumerate only realized points.** The profile list *is* the
enumeration. Filling in the cross product produces mostly nonsense
(`image-prebuilt` × Framework laptop, `iso-interactive` × Pi) and recreates the
combinatorial sprawl this refactor exists to prevent.

## Naming: shortest unambiguous

Not a fixed `prefix-body-suffix` template. Fixed slots force you to spell every
coordinate, and `iso-cloudzy-x86-wipe` is a name nobody will type — a shorthand
nobody types is worse than no shorthand.

**Rule: body = target; suffix = only the coordinates needed to disambiguate;
prefix reserved for axis-scoped reports.**

| Slug | Why that shape |
|---|---|
| `cloudzy` | single realized point — no suffix earned |
| `oracle-a1` / `oracle-amd` | suffix required: Oracle spans both arches |
| `fw-dual` | see the note below on single-boot Framework |
| `pi3` / `pi4` / `pi5` | board identity |
| `image:` / `iso:` / `aarch64:` | prefixes, for feedback about a shared path rather than one target |

Do not encode "free" in any name. It is a business fact with a short half-life
(Always Free versus trial terms shift). The vendor is `oracle`; free-tier shape
details belong in docs where staleness is visible, the way a channel pin records
its resolution date.

### One name per profile

The short label is **a field in the profile record, not a second table in
`CLAUDE.md`.** A separate abbreviation table is a duplicate that drifts, and
drift in a routing table means feedback silently lands on the wrong installer —
which is precisely today's `F:` bug.

That single name serves four uses at once:

1. the bootstrap `<platform>` argument,
2. the profile key,
3. the feedback prefix,
4. the `known-good/` key.

Today the same set has two names — `cloudzy|framework|framework-dual` on the
command line, `C:`/`F:`/`R:` in feedback. Unifying them means one loop can
validate that every alias resolves to exactly one profile and every profile has
at most one alias, checkable the same way `check-system-hosts` is.

`C:`/`F:`/`R:` survive as **deprecated aliases** for their current meanings: they
live in muscle memory and in past conversation history, the same back-compat
argument as the bootstrap `<platform>` argument. **Keep the confirmation echo
regardless of scheme** — echoing the resolved profile is what makes a misroute
visible at the moment it happens, and it is the one part of the current design
that is actively working.

## Per-target decisions

### Single-boot `framework` is not maintained

It was added for completeness. In practice the requirement is the opposite: a
reliable backup OS to recover into while exploring, which on that machine is
Pop!_OS. So `wipe` on Framework hardware is not a realized point.

Consequence for naming: with single-boot gone, "shortest unambiguous" would
reduce the slug to `fw`. **Keep `fw-dual` anyway.** Disk policy is
safety-critical, the slug is what you type at the moment you invoke a
whole-disk-capable installer, and `preserve` is the property that protects the
recovery OS. This is the same argument as the host-class file name in
`dot_files`: when the name is the only signal, spend the four characters.

**OPEN:** whether `framework/` is deleted or marked deprecated in place. Deleting
it is removing code, which this repo's `CLAUDE.md` requires an explicit
instruction to do. Not required for any step below.

### `known-good/` is keyed by modality

Rationale: if that evidence is needed in future, it is to run on *that platform*
— not to recover a set of user preferences. Preferences are the `dot_files`
repo's concern.

This does not change the character of the directory: it stays capture-only
evidence, never edited after capture, nothing reading it at build time.

### `oracle` is owned by a separate work stream

An `oracle` image is being developed in parallel. As of 2026-08-08 that path
**works**: the image builds, boots to a serial-console login in QEMU, and
key-only SSH login is verified end-to-end. The conceptual refactor here is to be
applied to it *after* it lands, not concurrently.

Consequence for sequencing: `image-prebuilt` has a working reference
implementation. Pi is joining a working pattern rather than co-founding one.

### Raspberry Pi: aspirational, and what to do about it later

Verified 2026-08-08, so the next reader does not have to re-derive it:

- **No install pipeline exists.** `raspberry-pi/install/` contains only a
  `README.md`. Neither `lib/bootstrap-installer.sh` nor `run-remote-steps.go`
  mentions Pi at all, which is why the platform table marks it "N/A
  (Image-based)" while the other three take `bash -s -- <platform>`.
- **What does exist** is documentation plus a postinstall layer:
  `raspberry-pi/postinstall/customize` and `templates/config-pi{3,4,5}.txt`.
- **It requires an Apple Silicon Mac to build** — an aarch64 build host, so no
  cross-compilation or emulation. This is a *build-host* requirement, and it is
  not the same thing as modality: `image-prebuilt` does not by itself say where
  the image is built. Oracle builds its image on x86_64 Linux. Treat build host
  as a profile field, not an axis.
- **Pi 3 is 1 GB RAM.** It shares a low-memory constraint with `oracle-amd`
  (below), which is evidence that minimum RAM belongs in the profile record
  rather than being rediscovered per platform.

**How to treat it:** ignore it for now; do not fold it into the modality refactor
as if it were working. When Pi is picked up, it is a *consumer* of the
`image-prebuilt` path that Oracle establishes, and the aarch64 knowledge
currently written as Pi-specific becomes shared material at that point. Do not
delete the postinstall templates in the meantime — they are the part that will
still be valid.

## Verified facts

Recorded with sources because each one was speculation before being checked.

### Oracle Always Free compute shapes

| Shape | Arch | OCPU | RAM |
|---|---|---|---|
| `VM.Standard.E2.1.Micro` | AMD / x86_64 | 1/8 (burstable) | **1 GB** |
| `VM.Standard.A1.Flex` | Ampere A1 / aarch64 | 2 | **12 GB** |

Source: Oracle Cloud Infrastructure documentation, "Always Free Resources"
(fetched 2026-08-08).

Consequence: `README.md` states a 2 GB minimum, so **`oracle-amd` is below this
repo's own stated floor**, and `guix system init` is where that bites. The
constraint is not "Oracle" — `oracle-a1` has 12 GB, six times the minimum. Treat
the two Oracle profiles as genuinely different targets on the memory axis, and
prefer `oracle-a1` as the viable free-tier target.

### Pre-existing validation failures

`lib/validate-before-deploy.sh` reports two failures that are **not** caused by
the rename, verified by running the same checks on a pristine `main` worktree on
2026-08-08:

- `TestDetectDevice` and `TestDetectDeviceFromState` fail identically on `main`.
  They assert that device autodetection returns an error, which does not hold on
  a host that has a real `/dev/nvme0n1`. They are environment-dependent, not
  regressions.
- `lib/bootstrap-installer.sh` carries the same 9 Unicode characters on `main`.
  This one is a **real** finding against the repo's own ISO constraint, just an
  old one: it will display broken glyphs on the Guix ISO terminal.

Do not treat a red validation run as blocking the rename. Do treat the Unicode
finding as worth fixing on its own.

## Implementation checklist

Ordered. Each step's blast radius is larger than the last, which is why they do
not merge.

### Step 1 — repo rename (in progress)

- [x] `cloudzy-guix-install` → `guix-platform-install` across 48 files
- [x] Go module path and all imports; `GUIX_INSTALL_REPO` default; bootstrap
      tarball glob; docker volume names; Guile mirrors in
      `tools/converted-scripts/`
- [x] Fix the repo-root lookup in `lib/common.go` that matched the repo *name*
      against the working directory (broke on rename, and never worked for the
      bootstrap tarball)
- [x] `SOURCE_MANIFEST.txt` regenerated
- [ ] Land the remote side. The in-repo pass and the remote name must land close
      together: `raw.githubusercontent.com` paths in already-published docs do
      **not** redirect the way a git remote does. **OPEN:** rename in place
      versus push this history to a new repo and leave the old one as a
      deprecated pointer. The second option makes the change additive and breaks
      no published URL.
- [ ] Fix the Unicode in `lib/bootstrap-installer.sh` (pre-existing; unrelated to
      the rename but cheap to clear while validation is being read)

### Step 2 — platform rename to profile slugs

- [ ] Introduce the profile record with the short label as a field
- [ ] `cloudzy` → profile slug; keep the old bootstrap argument as a
      back-compat alias
- [ ] Replace `C:`/`F:`/`R:` with profile slugs; keep the letters as deprecated
      aliases; keep the confirmation echo
- [ ] Add the validation loop: every alias resolves to exactly one profile,
      every profile has at most one alias
- [ ] Redefine "platform" in the hypothesis-ID policy. `CLAUDE.md` currently says
      "same letter = same hypothesis across platforms", written against
      platforms-as-directories. Under the new axes, state explicitly whether
      "across platforms" means across modality, arch, or profile — and make the
      whole repo consistent with that answer. This is a vocabulary decision, not
      a mechanical edit
- [ ] `known-good/` keyed by modality

### Step 3 — modality refactor

- [ ] `iso-interactive` versus `image-prebuilt` as the top-level structural split
- [ ] `VendorProfile` record consumed by shared steps: disk device pattern,
      firmware mode, serial-console kernel arguments, network method, nonguix
      needed, arch, minimum RAM, build host
- [ ] Keep shared what is shared: config generation, postinstall, verification /
      manifest / provenance
- [ ] Apply to `oracle` once that work stream lands (`oracle-a1`, `oracle-amd`)
- [ ] Fold Pi in as an `image-prebuilt` consumer; aarch64 material becomes shared

### Cross-repo consistency pass (after the remote name is real)

- [ ] `dot_files` docs still carrying hardcoded paths: `GUIX_MIGRATION_PLAN.md`,
      `DEPLOY_TO_GUIX_SYSTEM.md`, `DOCKER_FULL_TIME_SETUP.md`,
      `DOCKER_GUIX_DEBUG.md`, `QUICK_START_DOCKER.md`. These are *literal command
      lines a reader pastes*, so they need a placeholder convention (`$HOME`, or
      `/Users/<you>`) rather than the descriptive phrasing that suits prose
- [ ] Reconcile doc genre conventions between the two repos. This repo's
      `CLAUDE.md` mandates 90% technical / 10% narrative under `docs/`, with
      `docs/STORY.md` and `known-good/**/ATTESTATION.md` as deliberate
      exceptions. That is close kin to the evaluative / operational / traps split
      applied to `dot_files`' `system/README.md`, so the two repos can end up with
      compatible genre boundaries instead of each inventing one
- [ ] A full doc-voice pass over `docs/` is a much larger job than the rename
      (`INSTALLATION_KNOWLEDGE.md` alone is 3000+ lines). Separable; do not
      bundle it
