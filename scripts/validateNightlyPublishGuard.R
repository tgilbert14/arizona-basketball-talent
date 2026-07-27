## Regression guard for the unattended main-publish lifecycle.
## Static by design: the real orchestrator performs network, scrape, deploy,
## Task Scheduler, and Git mutations that are unsafe inside a unit test.

read_script <- function(path) {
  if (!file.exists(path)) stop("missing ", path)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

need <- function(text, pattern, label, fixed = TRUE) {
  ok <- if (fixed) grepl(pattern, text, fixed = TRUE) else grepl(pattern, text)
  if (!isTRUE(ok)) stop("nightly publish guard missing: ", label)
  invisible(TRUE)
}

nightly_path <- file.path("scripts", "nightlyRefresh.R")
runner_path <- file.path("scripts", "runNightly.ps1")
setup_path <- file.path("scripts", "setupSchedule.ps1")

invisible(parse(file = nightly_path))
nightly <- read_script(nightly_path)
runner <- read_script(runner_path)
setup <- read_script(setup_path)

need(nightly, 'Sys.getenv("GIRTH_PUBLISH_BRANCH", "main")',
     "main default publish target")
need(nightly, 'Sys.getenv("GIRTH_NIGHTLY_RUNNER", "0")',
     "dedicated runner marker")
need(nightly, 'data_commit_created <- FALSE',
     "provisional data-commit state")
need(nightly, 'if (st == 0) data_commit_created <- TRUE',
     "successful S7 commit transition")
need(nightly, 'restore_nightly_managed <- function',
     "scoped managed-file recovery")
need(nightly, '!isTRUE(dedicated_nightly_runner)',
     "runner-gated destructive recovery")
need(nightly, '"status", "--porcelain", "--untracked-files=all"',
     "post-restore untracked verification")
need(nightly, 'c("data/recruiting.db", "data/refresh-manifest.json")',
     "terminal DB and compact-manifest finalization")
need(nightly, '"www/pipeline-status.json",',
     "public status checksum target")
need(nightly, 'if (isTRUE(data_commit_created)) "data/recruiting.db"',
     "conditional terminal DB checksum")
need(nightly, 'shQuote(status_msg, type = "cmd")',
     "Windows-safe terminal commit message")
need(nightly, 'git_run("pull", "--rebase", "origin", publish_branch)',
     "explicit terminal pull target")
need(nightly, 'git_run("push", "origin", paste0("HEAD:", publish_branch))',
     "explicit terminal push target")
need(nightly, 'try(git_run("rebase", "--abort"), silent = TRUE)',
     "terminal rebase abort")
need(nightly, 'stages$status_publish',
     "observable terminal publish stage")
need(nightly, 'terminal pipeline status beacon could not be written',
     "status-write failure receipt")

guard_at <- regexpr("Refuse to publish from an arbitrary", nightly, fixed = TRUE)[1]
quick_at <- regexpr("qc <- quick_check(db_path)", nightly, fixed = TRUE)[1]
if (guard_at < 1L || quick_at < 1L || guard_at > quick_at) {
  stop("publish branch guard must run before the DB quick check")
}

need(runner, "$env:GIRTH_NIGHTLY_RUNNER = '1'",
     "wrapper runner marker")
need(runner, "$env:GIT_TERMINAL_PROMPT = '0'",
     "noninteractive Git credentials")
need(runner, "@('rebase-merge', 'rebase-apply')",
     "linked-worktree rebase recovery")
need(runner, "status --porcelain --untracked-files=all",
     "clean all-files wrapper gate")
need(runner, "rev-list --count $AheadRange",
     "clean-ahead recovery detection")
need(runner, "push origin ('HEAD:' + $PublishBranch)",
     "clean-ahead explicit main recovery push")

need(setup, "worktree prune", "stale worktree metadata pruning")
need(setup, "rev-parse --git-common-dir", "common repository identity check")
need(setup, "@('rebase-merge', 'rebase-apply')",
     "setup rebase recovery")
need(setup, "status --porcelain --untracked-files=all",
     "setup all-files cleanliness gate")
need(setup, "Register-ScheduledTask", "scheduled task registration")
need(setup, "-Settings $Settings -Force", "atomic task replacement")
if (grepl("Unregister-ScheduledTask", setup, fixed = TRUE)) {
  stop("setup must not delete the working task before replacement succeeds")
}

cat("Nightly publish guard validation passed.\n")

