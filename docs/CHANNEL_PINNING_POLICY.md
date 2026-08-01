# Channel Pinning Policy for Framework-Dual

## The rule

Framework-dual pins **both** the `guix` and `nonguix` channels to an explicit
commit pair. The pin exists for **reproducibility**, and it must always be
**newer than the hardware being installed on**.

The pin lives in `lib/common.go`:

```go
FrameworkDualGuixCommit    = "df2d121208127ac22f10e0f7c2f38d6c74e106a3"
FrameworkDualNonguixCommit = "73baab37361b3a81f326aa3fdec78840f5acc577"
FrameworkDualPinDate       = "2026-08-01"
```

`SetupNonguixChannel(platform)` emits these as an explicit `(list ...)` with both
channels pinned. Other platforms still use `(cons* ... %default-channels)`.

## History: this policy used to say the opposite

Until 2026-08-01 this document said framework-dual **MUST** use wingolog-era
commits (2024-02-16) and must never be changed. That was wrong for the hardware
this repo actually targets, and the mistake is recorded here so it is not
repeated.

### What happened

`docs/GNOME_LOGIN_TROUBLESHOOTING.md` recorded the real failure:

```
Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2
amdgpu: Fatal error during GPU init
```

The response was to pin both channels back to the commits from
[Wingo's "Guix on the Framework 13 AMD" post](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd),
on the theory that a known-good historical snapshot would fix the mismatch.

### Why that could not work

The target machine is a **Framework Laptop 13 (AMD Ryzen AI 300 Series)** —
Strix Point, GPU PCI ID `1002:1114` (Radeon 890M, gfx11.5).

| Requirement | Needs | wingolog pin provides |
| --- | --- | --- |
| amdgpu gfx11.5 IP support | Linux >= 6.10 | kernel 6.6.16 (Feb 2024) |
| `psp_14_0_4_toc.bin` | linux-firmware mid-2024+ | Feb 2024 linux-firmware |
| `gc_11_5_*`, `dcn_3_5_*` | linux-firmware mid-2024+ | Feb 2024 linux-firmware |

Strix Point shipped in **July 2024**, roughly five months *after* the pinned
commits. **Pinning backwards cannot supply firmware for hardware that did not
exist yet.** The pin was adopted to fix the `psp_14_0_4` error and instead
guaranteed it.

Wingo's post is about the **Ryzen 7040** Framework 13, a different generation.
The pin was correct for that machine and was carried over to hardware it had
never been validated against.

### Corroboration

On the Pop!_OS side of the same laptop, amdgpu binds and works with:

- kernel `7.0.11`
- `linux-firmware` 20260221
- `/lib/firmware/amdgpu/psp_14_0_4_toc.bin` present

### Downstream damage

The unexplained amdgpu failure was read as a generic "boot hang" and treated with
`nomodeset`, `acpi=off`, `noapic` and `nolapic` kernel arguments. Those caused
their own failures — `acpi=off` broke `xhci_hcd`, and `noapic`/`nolapic` broke the
internal i8042 keyboard. It also motivated an initrd module filter that never did
anything. See `docs/FRAMEWORK_STARTUP_HANG_FIX.md` and `docs/NVME_MODULE_FIX.md`.

**Lesson:** when a symptom looks like a hang, confirm the actual driver error
before adding kernel arguments. Every argument in that set was treating a
firmware problem with an interrupt-controller sledgehammer.

## How to move the pin forward

1. Resolve current channel heads:

   ```bash
   git ls-remote https://git.savannah.gnu.org/git/guix.git HEAD
   git ls-remote https://gitlab.com/nonguix/nonguix.git HEAD
   ```

   Cross-check guix against `https://codeberg.org/guix/guix` — the two should
   agree.

2. Confirm the candidate nonguix commit provides a new enough kernel:

   ```bash
   git clone --depth 1 https://gitlab.com/nonguix/nonguix.git
   grep -n '^(define-public linux ' nonguix/nongnu/packages/linux.scm
   ```

   `linux` must resolve to >= 6.10 for Ryzen AI 300. At the current pin it is
   `linux-7.1`, with `linux-lts` at `linux-6.18`.

3. Update the three constants in `lib/common.go`.

4. Run `lib/validate-before-deploy.sh --verbose`, then `./run-tests.sh`, then
   `./update-manifest.sh`.

5. Test the boot before trusting it — booting the real partition in QEMU is far
   cheaper than discovering the problem on reboot.

## Choosing a kernel

`nonguix` exposes both a latest and an LTS kernel:

- `(kernel linux)` — newest mainline the pin carries (currently 7.1)
- `(kernel linux-lts)` — newest LTS (currently 6.18)

Framework-dual uses `linux`. Either satisfies the >= 6.10 requirement;
`linux-lts` is the more conservative choice if a mainline release regresses on
this hardware. Because the channel is pinned, neither drifts between installs.

## Preventing future regressions

### DO NOT

- Pin to a commit older than the hardware you are installing on
- Remove the platform check in `SetupNonguixChannel()`
- Use `%default-channels` for framework-dual (it does not pin guix)
- Assume a pin validated on Ryzen 7040 transfers to Ryzen AI 300

### DO

- Keep both channels pinned to an explicit commit pair
- Record the date the pin was resolved, so staleness is visible
- Verify kernel and firmware ages against the target hardware's release date
- Document any change to the pinning strategy here

## Testing

1. Check the generated `~/channels.scm` during installation
2. Verify it contains both channels with pinned commits
3. Confirm `guix time-machine -C ~/channels.scm -- system build` produces a
   kernel and initrd

## References

- [Wingolog blog post](https://wingolog.org/archives/2024/02/16/guix-on-the-framework-13-amd) — about Ryzen 7040, not Ryzen AI 300
- [Wingolog channel analysis](./WINGOLOG_CHANNEL_ANALYSIS.md) — original analysis, kept for history
- [Framework startup hang fix](./FRAMEWORK_STARTUP_HANG_FIX.md) — the kernel arguments this mistake caused
- [NVMe module fix](./NVME_MODULE_FIX.md) — the initrd filter it also motivated

## History

- **2025-01-XX**: Observed initrd generation failures; adopted wingolog-era pinning
- **2025-01-XX**: Added this policy document to prevent regressions
- **2026-08-01**: Policy inverted. Identified the hardware as Ryzen AI 300 and the
  wingolog pin as the direct cause of the amdgpu firmware failure it was meant to
  fix. Repinned to recent commits; removed `nomodeset`/`noapic`/`nolapic`.
