---
description: Preparing a smart commit by updating documentation and manifests
---

1. **Update Documentation**
   - Review `CHECKLIST.md`:
     - Move completed items to `archive/CHECKLIST_COMPLETED.md`.
     - Update "Latest Completed Items".
     - Update "Currently Working On".
   - Review `docs/INSTALLATION_KNOWLEDGE.md`:
     - Add any "hard-won lessons" or crucial technical details discovered during the work.
   - Scan other relevant documentation (e.g., `README.md`, `docs/TROUBLESHOOTING.md`) and update if necessary to reflect code changes.

2. **Update Manifest**
   - Run the manifest update script to ensure `SOURCE_MANIFEST.txt` is current.
   // turbo
   ```bash
   ./update-manifest.sh
   ```

3. **Stage Changes**
   - Add the updated manifest file to the git stage to ensure it's included in the commit.
   // turbo
   ```bash
   git add SOURCE_MANIFEST.txt
   ```

4. **Verify State**
   - Run a status check to see what is staged and ensure the manifest is included.
   // turbo
   ```bash
   git status
   ```
