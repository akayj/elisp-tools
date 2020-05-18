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
     (not
      (string-blank-p
       (run-shell
	(format "grep '^\\[remote \"%s\"\\]$' %s"
		remote-name gitconfig-path)))))))

(defun loop-repos (basedir)
  "Loop all repos in BASEDIR."
  (color-message (concat "Scanning " (expand-file-name basedir) " ...")
		 *color-magenta*)

  (dolist (f (directory-files basedir t "[^\\(\\.\\|\\.\\.\\|\\.DS_Store\\)$]"))
    (if (not (git-repo-p f)) nil

      (color-message (format "==== Repo: %-20s ===="
			     (file-name-nondirectory f))
		     *color-green*)

      (cd f)

      (let ((output (run-shell "git tag -l | xargs git rev-parse")))
	(if (not (string-blank-p output))
	    (color-message (concat "\sTag found =>\s" output))))
      )
    )
  )

;; main
(loop-repos "~/gits/ppc")

(provide 'sync-git-repos)
;;; sync-git-repos.el ends here
