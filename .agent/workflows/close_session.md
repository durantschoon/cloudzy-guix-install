---
description: Wrap up development session, log summary, and ensure all work is saved.
---

1. Ask the user for any final notes, findings, or blocking issues from the current session that might not be in the logs yet.
2. Append a **"SESSION END: [Timestamp]"** entry to the latest `WORKLOG` file. content should include:
    - A brief summary of what was accomplished (bullet points).
    - Any open questions or next steps for the *next* session.
3. Check `git status`. If there are uncommitted changes (including the WORKLOG update), ask the user if they want to run `/smart_commit` or if you should just commit the WORKLOG.
4. Push the changes to ensuring the log is saved remotely.
5. Provide a final "Signed off" message confirming the log is safe.
