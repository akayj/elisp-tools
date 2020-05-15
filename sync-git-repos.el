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
  (if color
      (format "%s%s%s" color str *color-nc*)
    (format "%s%s%s" *color-red* str *color-nc*)))

(defun is-git-repo (path)
  "Check PATH whether or not a git repo."
  (if (file-exists-p (concat path "/.git/config")) t nil))

(message (color-string "开始同步git仓库到git.ppdaicorp.com..." *color-magenta*))

(dolist (f (directory-files "~/gits/ppc"))
  (if (and
       (file-directory-p f)
       (not (string-equal "." f))
       (not (string-equal ".." f)))

      (if (is-git-repo f)
	  (message (color-string (format "DIR: %s" f) *color-green*))
	(message (color-string (format "%s is not a git repo!" f)))
	)
    )
  )

(provide 'sync-git-repos)
;;; sync-git-repos.el ends here
