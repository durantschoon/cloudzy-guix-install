# Dormant Goals

Intentions this repo stated and then stopped acting on -- not by decision, but
by attention moving elsewhere. Recovered by reading git history on 2026-08-05.

These are **not** a backlog. Some deserve revival, some deserve a deliberate
burial, and the point of writing them down is that either outcome is better
than a third silent year. Each entry says where the intention was stated, what
evidence says it lapsed, and the actual cost of it being dormant.

Distinct from `CHECKLIST.md`'s R1-R4 roadmap, which is *live* planned work
toward making a working install generic. This file is the opposite: things that
were live once and quietly stopped.

---

## 1. The 90/10 narrative ratio

**Stated:** commit `cdc475e`, 2025-11-25 -- "Maintain 90% technical content,
10% narrative engagement," applied to `docs/INSTALLATION_KNOWLEDGE.md` along
with section titles like "The Mystery of the Missing Lock Directory."

**Evidence it lapsed:** the ratio appears in *that commit message and nowhere
else in the repository*. It was never written into `CLAUDE.md`, any guide, or
any doc. `docs/INSTALLATION_KNOWLEDGE.md` has been modified by at least ten
commits since, none referencing it.

**Cost:** a deliberate editorial decision that reads as a personal whim of one
commit. Anyone editing those docs since had no way to know a ratio existed, so
adherence has been accidental.

**Status:** partially revived 2026-08-05 -- codified in `CLAUDE.md`, and
`docs/STORY.md` created as the one place narrative leads. The convention now
has a written home.

---

## 2. Guile conversion, Phase 3

**Stated:** `docs/GUILE_CONVERSION.md` -- Phase 3 "Critical Installation
Scripts (PLANNED)", naming five targets with a "Next steps when resuming"
section.

**Evidence it lapsed:** last touched 2025-12-18. Of the five named targets,
`lib/bootstrap-installer.sh` and `lib/channel-utils.sh` are still Bash;
`clean-install.sh`, `verify-guix-install.sh` and `recovery-complete-install.sh`
no longer exist at the paths the doc names. So the plan is stale in both
directions -- unfinished *and* describing files that moved.

**Cost:** low, and possibly negative to complete. `CLAUDE.md` says Bash is
correct for "bootstrap scripts that run before Guix is available," which is
exactly what `bootstrap-installer.sh` is. The honest resolution may be to
narrow the goal rather than finish it.

**Recommendation:** re-scope. Explicitly exempt genuine bootstrap scripts,
then either convert `channel-utils.sh` or close the phase.

---

## 3. Reviewer personas at milestones

**Stated:** `CLAUDE.md:548`, "Reviewer Personas (Offer These at Milestones)" --
three personas (Mid-Level Unix User New to Guix, Seasoned Guix User, Dual-Boot
Laptop Owner), with the instruction to *offer* a review after each milestone.

**Evidence it lapsed:** the convention is live in `CLAUDE.md` and two matching
guides exist (`docs/GUIDE_SEASONED_GUIX.md`, `docs/GUIDE_DUAL_BOOT.md`), but
milestones have passed without it being exercised -- including
`docs/FRAMEWORK_DUAL_MILESTONE.md` and the successful 2026-08-02 first boot,
which is the largest milestone the project has had.

**Cost:** real and current. The Mid-Level-Unix-User persona is exactly the
reader the project is now trying to serve as it generalizes, and the
first-boot milestone was the natural moment to ask "would a newcomer have
survived this?"

**Recommendation:** offer it against the current state. This is the cheapest
item here and probably the highest value.

---

## 4. Time-tracking retrospectives

**Stated:** `docs/TIME_TRACKING_RETROSPECTIVE.md`, including estimated-vs-actual
tables and "helps identify time sinks for future improvement."

**Evidence it lapsed:** last touched 2025-11-25. No later retrospective exists,
despite months of subsequent work including the entire repin saga.

**Cost:** the repin work is precisely the material this practice was designed
to capture -- days lost to a pin that could never have worked. That data is now
only recoverable by reading commits.

**Recommendation:** either resume with one entry for the repin, or close it out
and note that `docs/STORY.md` now carries the narrative form of the same
lesson. Two half-practices are worse than one.

---

## 5. Retry statistics

**Stated:** `docs/RETRY_STATISTICS.md`, with a "Next Steps" section, added
2025-12-18 for `VerifyA`-style retry tracking.

**Evidence it lapsed:** untouched since the commit that created it.

**Cost:** unclear, which is itself the finding. It is not obvious whether the
instrumentation still runs or what would consume the numbers.

**Recommendation:** determine whether the tracking is still wired up. If not,
archive the doc rather than leave it looking active.

---

## 6. Console font verification

**Stated:** `docs/CONSOLE_FONT_TIPS.md:35` -- "**TODO: Test on next ISO boot to
confirm which fonts are available**".

**Evidence it lapsed:** several ISO boots have happened since, including a
successful full install.

**Cost:** small but now cheap to close -- there is a working machine to check
against, which is exactly what the TODO was waiting for.

**Recommendation:** run `ls /run/current-system/profile/share/consolefonts/`
on the installed system and replace the TODO with the answer.

---

## How this list was built

`git log --all --grep` across narrative/story/planned terms; a sweep for
`TODO|deferred|PLANNED|future improvement|next steps` across `docs/`,
`CHECKLIST.md` and `README.md`, excluding `archive/`; then `git log -1` per
candidate document to date its last modification. Anything whose last touch
predated 2026 and whose stated next step had not happened became a candidate,
and each was then checked against the current tree rather than trusted from the
doc.

Worth repeating periodically. The failure mode is not that goals get abandoned
-- it is that nobody notices they were, so they are neither finished nor
consciously dropped.
