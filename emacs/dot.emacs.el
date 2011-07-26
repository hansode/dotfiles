;;  Choice setting file from emacs-version.
(defun emacs-version>= (num)
  "True if num >= emacs-version"
  (>= (string-to-int emacs-version) num))

(if (boundp 'MULE)
    (progn
      (load "~/.emacs.d/mule.el")))

(if (emacs-version>= 20)
    (progn
      (load "~/.emacs.d/emacs21.el")))
