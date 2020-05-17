;;; sync-git-repos.el -- sample emacs lisp script

;;; Commentary:
;; Usage in Shell:
;;   Emacs -nw --script sync-git-repos.el

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

(defun color-string (str &optional color)
  "Colorful STR with COLOR as optional."
  (let ((used-color (if color color *color-white*)))
      (format "%s%s%s" used-color str *color-nc*)))

(defun color-message (str &optional color)
  "Print colorful STR in COLOR."
  (message (color-string str color)))

(defun git-repo-p (path)
  "Check PATH whether or not a git repo."
  (if (file-exists-p (concat path "/.git/config")) t nil))

(defvar ignored-filenames '("." ".." ".DS_Store"))

(defun loop-repos (basedir)
  "Loop all repos in BASEDIR."
  (dolist (f (directory-files basedir t))
    (if (member (file-name-nondirectory f) ignored-filenames)
	;; (color-message (format "IGNORE: %s" f))
	nil

      (if (not (git-repo-p f))
	  ;; (color-message (format "NotGit: %s" f) *color-yellow*)
	  nil

	(color-message (format "==== Git: %s ====" f) *color-green*)
	(cd f)

	(let ((output
	       (string-trim
		(shell-command-to-string "git tag -l | xargs git rev-parse"))))
	  (if (not (string-blank-p output ))
	      (color-message (concat "\sTag found =>\s" output))))
	)
      )
    )
  )

;; main
(color-message "开始同步到git.ppdaicorp.com..." *color-magenta*)
(loop-repos "~/gits")

(provide 'sync-git-repos)
;;; sync-git-repos.el ends here
