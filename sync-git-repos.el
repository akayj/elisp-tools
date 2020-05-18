;;; sync-git-repos.el -- sample emacs lisp script

;;; Commentary:
;; Usage in Shell:
;;   Emacs --script sync-git-repos.el

;;; Code:
(require 'subr-x)

;; colors
(defconst *color-nc* "\033[0m")
(defconst *color-red* "\033[31m")
(defconst *color-green* "\033[32m")
(defconst *color-yellow* "\033[33m")
(defconst *color-blue* "\033[34m")
(defconst *color-magenta* "\033[35m")
(defconst *color-cyan* "\033[36m")
(defconst *color-white* "\033[37m")

(defvar remote-name "gitlab-mirror")

(defun color-message (str &optional color)
  "Print colorful STR in COLOR, defualt `*color-white*'."
  (let ((used-color (if color color *color-white*)))
    (message "%s%s%s" used-color str *color-nc*)))

(defun run-shell (sh)
  "Run SH return output striped string."
  (string-trim (shell-command-to-string sh)))

(defun git-repo-p (path)
  "Check PATH whether or not a git repo."
  (let ((gitconfig-path (concat path "/.git/config")))
    (and
     (file-exists-p gitconfig-path) ;; 检查 `.git/config' 是否存在
     ;; 检查 remote repo 是否跟踪
     (not (string-blank-p
	   (run-shell (format "grep '^\\[remote \"%s\"\\]$' %s"
			      remote-name gitconfig-path)))))))

(defun sync-tags ()
  "Sync all tags to remote repo."
  (let* ((remote-tags-cmd (concat
			   "git ls-remote --tags -q " remote-name
			   " | grep -v '{}'"
			   " | awk '{print $2,$1}'"
			   " | awk -F '/' '{print $3}'"
			   " | sed 's/ /+/g'"))
	 (local-tags-cmd "git show-ref --tags")
	 (remote-tags-table (run-shell remote-tags-cmd))
	 (local-tags-table (run-shell local-tags-cmd)))

    (dolist (line (split-string remote-tags-table "\n"))
      (let* ((remote-tag (run-shell (format "echo '%s' | awk -F '+' '{print $1}'" line)))
	     (local-tag-commit
	      (run-shell (format "echo '%s' | grep /%s$ | awk '{print $1}'"
				 local-tags-table remote-tag))))
	(unless (string-blank-p remote-tag)
	  (unless (string= line (concat remote-tag "+" local-tag-commit))
	    (color-message (run-shell (concat "git push " remote-name " -d " remote-tag)))
	    )
	  )
	)
      )

    (color-message (run-shell (concat "git push " remote-name " --tags")))
    )
  )

(defun sync-repos (basedir)
  "Loop all repos in BASEDIR."
  (color-message (concat "Scanning " (expand-file-name basedir) " ...")
		 *color-magenta*)

  (dolist (f (directory-files basedir t "[^\\(\\.\\|\\.\\.\\|\\.DS_Store\\)$]"))
    (unless (not (git-repo-p f))
      (color-message (format "Syncing %-20s" (file-name-nondirectory f)) *color-green*)

      (cd f)

      ;; 删除本地tag，并拉取 origin 最新的代码
      (run-shell "git tag -l | xargs git tag -d > /dev/null && git pull")

      ;; git push && git push --tags
      (color-message (run-shell (concat "git push " remote-name)))
      (sync-tags)
      )
    )
  )

;; main
(sync-repos "~/gits/ppc")

(provide 'sync-git-repos)
;;; sync-git-repos.el ends here
