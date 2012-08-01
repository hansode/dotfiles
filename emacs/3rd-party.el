;; accel !!!
; accel.el does not work on emacs 24.1.
;(load "~/.emacs.d/3rd-party/accel.el")


;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
