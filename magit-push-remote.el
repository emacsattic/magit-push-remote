;;; magit-push-remote.el --- push remote support for Magit

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120613
;; Version: 0.1.0
;; Status: proof of concept
;; Homepage: https://github.com/tarsius/magit-push-remote
;; Keywords: convenience

;; This file is not part of GNU Emacs.
;; This file is not part of Magit.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WIP seems usable but don't complain if something goes wrong.

;; This plug-in provides support for an additional default remote
;; which when pushing is used instead of the "merge" default specified
;; by the git-config(1) option `branch.<name>.remote'.

;; Together `branch.<name>.remote' and `branch.<name>.merge' set the
;; default used by git-pull(1) and git-push(1).  Like their git
;; counterparts `magit-push' and `magit-pull' use these options. So
;; does `magit-status' which displays commits not pushed to or not
;; pulled from the default remote/branch.

;; This works nicely if commits most often flow like this:
;;
;;   +------------+            +------------+
;;   |   remote   | -- pull -> |   local    |
;;   |    repo    | <- push -- |    repo    |
;;   +------------+            +------------+

;; But it is inconventient if commits most often flow through your
;; local repository like this:
;;
;;   +------------+            +------------+            +------------+
;;   |  upstream  | -- pull -> |   local    |            |    your    |
;;   |    repo    |            |    repo    | -- push -> |   public   |
;;   |            |            +------------+            |    repo    |
;;   |            | <- merge pull reguest -------------- |            |
;;   +------------+                                      +------------+

;; This package modifies magit to automatically detect whether the
;; latter workflow is used and if so provides additional information
;; related to "personal remote" and push to that remote by default.

;; To use add this to your .emacs.el:
;;   (require 'magit-push-remote)
;;   (add-hook 'magit-mode-hook 'turn-on-magit-push-remote)
;;
;; and
;;   git config --global magit.defaultpushremote <REMOTE_NAME>
;; or
;;   git config magit.pushremote <REMOTE_NAME>


;;; TODO:
;; TODO test
;; TODO improve documentation
;; TODO do not redefine `magit-push', instead define `magit-push*'
;;      users can still use `fset'
;; TODO get bugfixes merged upstream (#440)
;; TODO are all fixes in #440 also included here?
;; TODO maybe get the whole thing merged upstream
;; TODO also determine push remote based on it's url
;; TODO also determine push remote based on push rights
;; TODO what to do if we *also* have permission to push to the "merge" remote?
;; TODO if merge == personal, don't show twice
;; TODO when user tries to push but there is no remote he has push rights for
;;      automatically add personal remote if she has a fork on github or
;;      else even offer to create it

;;; Code:

(require 'magit)

(defcustom magit-push-remote-debug nil "" :type 'boolean :group 'magit)

;; REDEFINE `magit-push' DEFINED IN `magit.el'
;;
;; * add support for the magical push remote
;; * also fix various bugs
;;   * do not push to `branch.b.merge' unless pushing to `branch.b.remote'
;;   * set the upstream remote and branch using just `--set-upstream',
;;     and only when `magit-set-upstream-on-push' calls for it
;;     * do not ignore `magit-set-upstream-on-push' by setting `branch.b.merge'
;;       when `branch.b.remote' and `remote.origin.url' are unset,
;;       and no prefix argument is used
;;     * do not ignore `magit-set-upstream-on-push' by setting `branch.b.remote'
;;       when `branch.b.remote' and `remote.origin.url' are unset
;;     * This limits setting the upstream to git >= 1.7.0 but
;;       then at least it is done correctly
(magit-define-command push ()
  "Push the current branch to a remote repository.

With a single prefix argument ask the user what branch to push
to.  With two or more prefix arguments also ask the user what
remote to push to.  Otherwise determine the remote and branch as
described below.  If the remote can not be determined ask the
user.  If the remote branch cannot be determined push without
specifing the remote branch explicitly.

When `magit-push-remote-mode' is turned on and the current
repository has a push-remote use that.  See the modes doc-string
for how the push-remote is determined.

Otherwise use the remote and branch specified by the
git-config(1) options `branch.<name>.remote' and
`branch.<name>.merge'.

This function is redefined in `magit-push-remote.el' replacing
the original definition in `magit.el'.  It's behaviour differs
even if `magit-push-remote-mode' is turned off.  These
differences are due to bugs in the original implementation being
fixed here.  See the comment before this function's definition
for more information."
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
         (push-remote (and magit-push-remote-mode
                           (magit-get-push-remote branch)))
         (pull-remote (magit-get-remote branch))
         (auto-remote (or push-remote pull-remote))
         (used-remote
          (if (or current-prefix-arg
                  (not auto-remote))
              (magit-read-remote* (format "Push %s to remote" branch)
                                  auto-remote t)
            auto-remote))
         (remote-branch
          (or (and (>= (prefix-numeric-value current-prefix-arg) 16)
                   (magit-read-remote-branch
                    used-remote (format "Push %s as branch" branch)))
              (cond ((equal used-remote push-remote)
                     (magit-get-push-remote-branch branch))
                    ((equal used-remote pull-remote)
                     (magit-get "branch" branch "merge"))))))
    (when magit-push-remote-debug
      (message "magit-push")
      (message "  branch:        %s" branch)
      (message "  push-remote:   %s" push-remote)
      (message "  pull-remote:   %s" pull-remote)
      (message "  auto-remote:   %s" auto-remote)
      (message "  used-remote:   %s" used-remote)
      (message "  remote-branch: %s" remote-branch))
    (apply 'magit-run-git-async "push" "-v" used-remote
           (if remote-branch
               (format "%s:%s" branch remote-branch)
             branch)
           (cond (remote-branch
                  magit-custom-options)
                 ((eq magit-set-upstream-on-push 'refuse)
                  (error "Not pushing since no upstream has been set."))
                 ((or (eq magit-set-upstream-on-push 'dontask)
                      (and (eq magit-set-upstream-on-push t)
                           (yes-or-no-p "Set upstream while pushing? ")))
                  (cons "--set-upstream" magit-custom-options))
                 (t
                  magit-custom-options)))))

;; REDEFINE `magit-refresh-status' DEFINED IN `magit.el'.
;;
(defun magit-refresh-status ()
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (let* ((branch (magit-get-current-branch))
             (remote (and branch (magit-get "branch" branch "remote")))
             (remote-rebase (and branch (magit-get-boolean "branch" branch "rebase")))
             (remote-branch (or (and branch (magit-remote-branch-for branch)) branch))
             (remote-string (magit-remote-string remote remote-branch remote-rebase))
             (push-remote (and magit-push-remote-mode
                               (magit-get-push-remote branch)))
             (push-remote-branch (and push-remote
                                      (magit-get-push-remote-branch branch)))
             (push-remote-string (and push-remote
                                      (magit-remote-string
                                       push-remote push-remote-branch nil)))
             (head (magit-git-string
                    "log"
                    "--max-count=1"
                    "--abbrev-commit"
                    (format "--abbrev=%s" magit-sha1-abbrev-length)
                    "--pretty=oneline"))
             (no-commit (not head)))
        (when magit-push-remote-debug
          (message "magit-refresh-status")
          (message "  mode:               %s" magit-push-remote-mode)
          (message "  branch:             %s" branch)
          (message "  remote:             %s" remote)
          (message "  remote-rebase:      %s" remote-rebase)
          (message "  remote-branch:      %s" remote-branch)
          (message "  remote-string:      %s" remote-string)
          (message "  push-remote:        %s" push-remote)
          (message "  push-remote-branch: %s" push-remote-branch)
          (message "  push-remote-string: %s" push-remote-string)
          (message "  head:               %s" head))
        (when remote-string
          (insert "Remote:   " remote-string "\n"))
        ;; If it wasn't for this we wouldn't have to patch this function.
        ;; TODO ask upstream for a mechanism to insert header lines
        ;; similar to that for sections.
        (when push-remote-string
          (insert "Personal: " push-remote-string "\n"))
        (insert (format "Local:    %s %s\n"
                        (propertize (magit--bisect-info-for-status branch)
                                    'face 'magit-branch)
                        (abbreviate-file-name default-directory)))
        (insert (format "Head:     %s\n"
                        (if no-commit "nothing commited (yet)" head)))
        (let ((merge-heads (magit-file-lines (concat (magit-git-dir)
                                                     "MERGE_HEAD"))))
          (if merge-heads
              (insert (format "Merging:   %s\n"
                              (mapconcat 'identity
                                         (mapcar 'magit-name-rev merge-heads)
                                         ", ")))))
        (let ((rebase (magit-rebase-info)))
          (if rebase
              (insert (apply 'format "Rebasing: onto %s (%s of %s); Press \"R\" to Abort, Skip, or Continue\n" rebase))))
        (insert "\n")
        (magit-git-exit-code "update-index" "--refresh")
        (magit-insert-stashes)
        (magit-insert-untracked-files)
        (magit-insert-pending-changes)
        (magit-insert-pending-commits)
        (magit-insert-unpulled-commits remote remote-branch)
        (when magit-push-remote-mode
          (magit-insert-push-remote-unpulled-commits
           push-remote push-remote-branch))
        (let ((staged (or no-commit (magit-anything-staged-p))))
          (magit-insert-unstaged-changes
           (if staged "Unstaged changes:" "Changes:"))
          (magit-insert-staged-changes staged no-commit))
        (magit-insert-unpushed-commits remote remote-branch)
        (when magit-push-remote-mode
          (magit-insert-push-remote-unpushed-commits
           push-remote push-remote-branch))
        (run-hooks 'magit-refresh-status-hook)))))

(defun magit-get-push-remote (branch)
  (let ((remote (or (magit-get "branch" branch "pushremote")
                    (car (member (magit-get "magit.defaultpushremote")
                                 (magit-git-lines "remote"))))))
    (if (string= remote "") nil remote)))

(defun magit-get-push-remote-branch (branch)
  (let ((remote-branch (magit-get "branch" branch "push")))
    (save-match-data
      (if (and remote-branch
               (string-match "^refs/heads/\\(.+\\)" remote-branch))
          (match-string 1 remote-branch)
        branch)))) ; always default to the local name

;; Like `magit-read-remote' but
;; * add REQUIRE-MATCH and
;; * deal with `completing-read's strange understanding of REQUIRE-MATCH
(defun magit-read-remote* (&optional prompt def require-match)
  (let* ((prompt (or prompt "Remote"))
         (def (or def (magit-guess-remote)))
         (remotes (magit-git-lines "remote"))

         (reply (magit-completing-read (concat prompt ": ") remotes
                                       nil require-match nil nil def)))
    (if (string= reply "")
        (if require-match
            (error "No remote selected")
          nil)
      reply)))

;; Instead we could redefine `magit-[unpulled|unpushed]-commits
;; but we have already redefined enough and once we don't have
;; to patch `magit-refresh-status' anymore it would stop working.

(magit-define-inserter push-remote-unpulled-commits (remote remote-branch)
  (when remote
    (apply #'magit-git-section
           'unpulled
           (format "Unpulled commits @ %s:" remote)
           'magit-wash-log "log"
           (append magit-git-log-options
                   (list (format "HEAD..%s/%s" remote remote-branch))))))

(magit-define-inserter push-remote-unpushed-commits (remote remote-branch)
  (when remote
    (apply #'magit-git-section
           'unpushed
           (format "Unpushed commits @ %s:" remote)
           'magit-wash-log "log"
           (append magit-git-log-options
                   (list (format "%s/%s..HEAD" remote remote-branch))))))

;;;###autoload
(define-minor-mode magit-push-remote-mode
  "Push remote support for Magit."
  :lighter "" :retire 'magit-push-remote
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  ;; We have to patch `magit-refresh-status' anyway so
  ;; we don't use these hooks which would be cleaner.
  ;; (cond (magit-push-remote-mode
  ;;        (add-hook 'magit-after-insert-unpulled-commits-hook
  ;;                  'magit-insert-push-remote-unpulled-commits nil t)
  ;;        (add-hook 'magit-after-insert-unpushed-commits-hook
  ;;                  'magit-insert-push-remote-unpushed-commits nil t))
  ;;       (t
  ;;        (remove-hook 'magit-after-insert-unpulled-commits-hook
  ;;                     'magit-insert-push-remote-unpulled-commits t)
  ;;        (remove-hook 'magit-after-insert-unpushed-commits-hook
  ;;                     'magit-insert-push-remote-unpushed-commits t)))
  )

;;;###autoload
(defun turn-on-magit-push-remote ()
  "Unconditionally turn on `magit-push-remote-mode'."
  (magit-push-remote-mode 1))

(provide 'magit-push-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; epkg.el ends here
