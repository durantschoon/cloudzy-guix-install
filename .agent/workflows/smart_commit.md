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

2. **Pre-Deployment Validation**
   - Run the validation script to catch syntax errors, Unicode issues, and missing parameters.
   // turbo
   ```bash
   lib/validate-before-deploy.sh --verbose
   ```

3. **Run Tests**
   - Run the test suite to ensure no regressions.
   // turbo
   ```bash
   ./run-tests.sh
   ```

4. **Update Manifest**
   - Run the manifest update script to ensure `SOURCE_MANIFEST.txt` is current.
   // turbo
   ```bash
   ./update-manifest.sh
   ```

5. **Stage Changes**
   - Add the updated manifest file and other changes to the git stage.
   // turbo
   ```bash
   git add .
   ```

6. **Verify State**
   - Run a status check to review what is staged.
   // turbo
   ```bash
   git status
   ```
