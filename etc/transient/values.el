((magit-pull "--rebase")
 (magit-push "--force-with-lease")
 (magit-rebase "--committer-date-is-author-date" "--autostash"))
