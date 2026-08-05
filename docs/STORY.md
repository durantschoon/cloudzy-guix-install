# How a Laptop Refused to Boot, and What It Was Actually Telling Us

This is the narrative companion to the reference documentation. Everything here
happened; the commits and the dates are real, and every technical claim is
cross-referenced to the document that carries the detail.

It exists because the *shape* of this debugging story turned out to be more
useful than any individual fix in it. The fixes apply to one laptop. The shape
applies to everything.

> **Ratio.** This file is the one place in the repo that leads with narrative.
> Everywhere else, `docs/` holds to roughly 90% technical content and 10%
> narrative framing -- a convention set on 2025-11-25 in `cdc475e` and, until
> now, recorded only in that commit message. See `CLAUDE.md`.

---

## I. The machine that did not exist yet

The pin was the problem, and the pin was the thing nobody suspected, because
the pin came with a pedigree.

Andy Wingo had written up getting Guix onto a Framework laptop. It worked. The
channel commits from that write-up -- guix and nonguix, both from
**2024-02-16** -- went into this repo as a known-good pair, marked CRITICAL, and
were not to be touched. Pinning is how Guix keeps a build reproducible. Pinning
to commits that a respected practitioner had actually booted seemed like the
most conservative choice available.

It was the one choice that could not possibly work.

The laptop was a Framework 13 with a **Ryzen AI 300** processor. Strix Point
silicon. GPU `1002:1114`. That hardware was announced in mid-2024 and shipped
later -- *months after* the commits it was being asked to support. The amdgpu
firmware it needs (`psp_14_0_4`, `gc_11_5_*`, `dcn_3_5_*`) entered
linux-firmware around mid-2024. Kernel support for gfx11.5 landed in Linux 6.10.
The pin delivered Linux 6.6.16 and a firmware tree from February.

There is a sentence in `CLAUDE.md` now that did not exist then:

> Pinning backwards can never supply firmware for hardware that did not exist yet.

Reproducibility means *the same result every time*. It does not mean *the right
result*. A pin is a promise about consistency, not about correctness, and the
two get conflated precisely because "pinned to a known-good commit" sounds like
it settles the question. It settles a different question.

**Where the detail lives:** `docs/WINGOLOG_CHANNEL_ANALYSIS.md` for the
historical analysis and why it no longer applies;
`docs/CHANNEL_PINNING_POLICY.md` for how to move a pin forward safely.

---

## II. Four symptoms, and the temptation to fix four things

Boot the machine and you got:

1. A greeter that rendered and then ignored every keystroke.
2. No wireless. Not "wireless that failed to associate" -- no driver at all.
3. Bluetooth `wmt` timeouts scrolling past.
4. `amdgpu: Direct firmware load for amdgpu/psp_14_0_4_toc.bin failed with error -2`.

Four subsystems. Four plausible investigations. Each of them looks like its own
bug, and each has a rich literature of unrelated causes you can go read.

This is the fork in the road, and it is worth being honest that taking the wrong
branch is *reasonable*. The four symptoms have nothing obviously in common. The
keyboard is an i8042 device on IRQ 1. The wireless is a MediaTek MT7925 on PCIe.
The GPU is on-die. Nothing about "these all broke at once" points at a shared
cause unless you already suspect one.

What was actually true: **the kernel and firmware set was older than the
machine.** Every symptom was one hardware component that the running software
predated. `mt7925e` did not exist before Linux 6.7, so there was no wireless
driver -- not a broken one, an absent one. The amdgpu firmware blobs were not in
the tree yet. And so on, four times.

> **The generalization:** several unrelated-looking hardware failures appearing
> together on one boot means one cause underneath, not several. The number of
> broken subsystems is evidence about *how deep* the cause sits, not about how
> many causes there are.

---

## III. The fix that made it worse

Before the single-cause theory arrived, the dead keyboard got its own
hypothesis, and its own fix: `noapic`, `nolapic`, `nomodeset`, `acpi=off`.
These are the classic "laptop won't boot" incantations. They are all over the
forums. They went into `kernel-arguments`.

They made things worse in two distinct ways, and the second is the interesting
one.

**First**, `noapic` and `nolapic` force legacy 8259 interrupt routing, which
modern AMD platforms do not reliably provide. The internal keyboard is an
i8042 `AT Translated Set 2 keyboard` on IRQ 1. Deprive it of interrupts and it
receives nothing. The fix for the dead keyboard was *causing* a dead keyboard.
The symptom persisted, which read as "the fix didn't work" rather than "the fix
is now the cause" -- the two are indistinguishable from the outside.

**Second**, and more quietly: `kernel-arguments` in Guix *replaces*
`%default-kernel-arguments` unless you explicitly append. The default carries
`modprobe.blacklist=usbmouse,usbkbd`, and upstream blacklists `usbkbd` because
it races `usbhid` (bugs.gnu.org/35574). So the workaround silently dropped an
unrelated protection that someone else had debugged years earlier and written
down. Nothing announced this. The config simply meant less than it appeared to.

`nomodeset`, meanwhile, disabled kernel modesetting in a configuration whose
whole purpose was to load amdgpu with proprietary firmware. It cannot supply
missing firmware. It can only guarantee an unaccelerated console.

There is now a regression test in
`framework-dual/install/03-config-dual-boot_test.go` that fails if any of those
four arguments reappears, and its comment explains why rather than just
asserting. A test that says "must not contain `noapic`" teaches nobody. A test
that says "`noapic`+`nolapic` starve the i8042 keyboard of interrupts, see
`docs/FRAMEWORK_STARTUP_HANG_FIX.md`" survives the next person who thinks they
have a good reason.

**Where the detail lives:** `docs/FRAMEWORK_STARTUP_HANG_FIX.md`.

---

## IV. One change, four fixes

Repinning forward -- to a guix/nonguix pair new enough to carry Linux 7.1.5 and
a mid-2024-or-later firmware tree -- fixed all four symptoms at once. Verified
on hardware, 2026-08-02, first boot:

- keyboard works at the console
- WiFi connects via `nmtui`, which by itself proves `mt7925e` bound and its
  firmware loaded -- no `dmesg` archaeology needed
- `dmesg | grep -i amdgpu` shows no errors

The GRUB kernel line came out as `loglevel=3 modprobe.blacklist=usbmouse,usbkbd
quiet`: the append worked, and none of the four bad arguments survived.

Four symptoms, one commit. This is what a correctly identified root cause looks
like from the outside, and it is worth noticing how *anticlimactic* it is. There
was no clever fix. There was a wrong number replaced with a right one. All the
work was in the diagnosis.

---

## V. The system that could not repair itself

With the machine booting, a subtler trap sprang.

`guix system init` copies the **store closure** of the built system. It does not
copy the channel configuration that produced it. So a system installed from a
pinned guix+nonguix pair boots knowing only the `guix` channel -- and
reconfiguring *the very config that built it* fails:

```
failed to load '/etc/config.scm':
... in procedure resolve-interface: no code for module (nongnu packages linux)
```

Read that failure carefully, because it is a strange one. The only supported
mechanism for changing an installed Guix system -- including rolling a change
*back* -- is `guix system reconfigure`. A machine that cannot resolve its own
config's modules has no supported way to change itself. It boots fine and is
inert.

The fix is to put the pin *inside* the config, via `guix-configuration`'s
`channels` field. The service then writes `/etc/guix/channels.scm` at
activation, and every later `guix pull` reproduces the pin instead of jumping to
HEAD.

There is a coda. `/etc/guix/channels.scm` ends up as a **symlink into
`/gnu/store`**, so the obvious repair -- `sudo cp my-channels.scm
/etc/guix/channels.scm` -- fails with *read-only file system*, which looks like
a permissions problem and is actually the system telling you it is already
managing that file declaratively. On Guix, reaching for `cp` to fix
configuration is nearly always a sign that the declarative mechanism exists and
has not been found yet.

**Where the detail lives:** `docs/CHANNEL_PINNING_POLICY.md`.

---

## VI. Two settings that sound like one

The last trap in this sequence is the smallest and the most expensive.

Using nonguix substitutes requires **two** independent settings: authorizing the
signing key, and adding the substitute URL. Authorizing the key alone is a
complete-sounding action. It is the step with the security implications, the one
you have to think about, the one that feels like the gate.

But `%default-substitute-urls` does not include nonguix. With the key
authorized and the URL absent, Guix never *asks* the nonguix server for
anything. It does not error. It does not warn. It simply builds Linux from
source, and you discover this an hour later when the fans have not stopped.

Silence is not confirmation. A trust decision and a routing decision are
different decisions, and configuration systems are very willing to let you make
one and believe you made both.

---

## VII. What the story is for

Six sections, six failures, and only one of them was a bug in the ordinary
sense. The rest were:

- a correct-looking pin pointed at the wrong moment in time
- four symptoms read as four problems
- a workaround that became the cause, while silently deleting an unrelated fix
- an installed system with no supported path to change itself
- one of two required settings, applied confidently

None of these are Guix-specific, and that is the point. Guix's contribution is
that it makes each of them *legible after the fact* -- there is an exact commit
pair, an exact closure, an exact generation to roll back to. The `known-good/`
directory exists to preserve that legibility: it holds the configuration a
machine actually booted, captured from the machine's own record of itself,
kept apart from the generic installer precisely so that neither one quietly
becomes the other.

The reference docs tell you what to type. This one is here so that when your
own four symptoms show up together, you spend the first hour asking how deep
the cause is instead of the next four fixing four things.
