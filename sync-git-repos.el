;;; sync-git-repos.el -- sample emacs lisp script

;;; Commentary:
;; Usage in Shell:
;;   Emacs -nw --script sync-git-repos.el

;;; Code:
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
  (let ((used-color (if color color *color-red*)))
      (format "%s%s%s" used-color str *color-nc*)))

(defun color-message (str &optional color)
  "Print colorful STR in COLOR."
  (message (color-string str color)))

(defun is-git-repo (path)
  "Check PATH whether or not a git repo."
  (if (file-exists-p (concat path "/.git/config")) t nil))

(color-message "开始同步到git.ppdaicorp.com..." *color-magenta*)

(defun loop-repoes (basedir)
  "Loop all repoes in BASEDIR."
  (dolist (f (directory-files basedir t))
    (if (and
	 (not (string-equal ".DS_Store" (substring f -9)))
	 (not (string-equal "." (substring f -1)))
	 (not (string-equal ".." (substring f -2)))
	 )
	(if (is-git-repo f)
	    (color-message (format "Git: %s" f) *color-green*)
	  (color-message (format "SKIP: %s" f) *color-yellow*))

      (color-message (format "IGNORE: %s" f) *color-white*)
      )
    )
  )

(loop-repoes "~/gits")

(provide 'sync-git-repos)
;;; sync-git-repos.el ends here
