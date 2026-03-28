Read `todo.md` and `completed.md` from the project root.

## Steps

1. **Display the todo list.** Print every unchecked task from `todo.md`, ranked by priority (highest first). Annotate each task with its complexity tier (Low / Medium / High) from the section it lives in.

2. **Detect completed work.** For each unchecked `- [ ]` task, inspect the codebase (R/ files, DESCRIPTION, NAMESPACE, tests/, vignettes/, etc.) and recent git history to determine whether the task has already been implemented. Use Grep, Glob, and Read as needed — be thorough but fast.

3. **Update the files.** For every task you confirm is done:
   - In `todo.md`, change `- [ ]` to `- [x]` on that line.
   - In `completed.md`, append the task description under today's date header (create the header if it doesn't exist yet). Use the format `- [x] <description>`.

4. **Report.** Print a short summary: how many tasks remain, how many were newly checked off, and which ones.
