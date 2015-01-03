;; prog

;; auto-insert
(load "autoinsert" t)
(setq auto-insert-directory "~/.emacs.d/header/")
(setq auto-insert-alist
      (nconc '(
               ("\\.pm$"   . "pm")
               ("\\.pl$"   . "pl")
               ("\\.sh$"   . "sh")
               ("\\.eml$\\|\\.mail$"    . "eml")
               ) auto-insert-alist))
(add-hook 'find-file-not-found-hooks 'auto-insert)


;; ファイルの頭が #! で始まっていたら実行可能ファイルに変更
(add-hook 'after-save-hook
          '(lambda ()
             (save-restriction
               (widen)
               (if (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let* ((name (buffer-name))
                          (filename (buffer-file-name))
                          (mode (file-modes filename)))
                     (set-file-modes name
                                     (logior mode (logand (/ mode 4) 73)))
                     (message (concat "Wrote " filename " (+x)"))))
               )))

;; c-mode c++-mode setting
(autoload 'c++-mode  "c++-mode" "C++ Editing Mode"       t)
(autoload 'c-mode    "c-mode" "C Editing Mode"           t)
(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(setq auto-mode-alist
      (append '(
                ("\\.c$"   . c-mode)
                ("\\.cc$"  . c-mode)
                ("\\.cpp$" . c-mode)
                ("\\.c++$" . c-mode)
                ("\\.C$"   . c-mode)
                ("\\.h$"   . c-mode)
                ("\\.php$" . php-mode)
                ) auto-mode-alist ))

;; cperl-mode setting
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing perl script." t)
(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode)
                ("\\.cgi$" . cperl-mode)) auto-mode-alist ))

;(setq cperl-indent-parens-as-block t)
;(setq cperl-close-paren-offset -4)
;(setq cperl-highlight-variables-indiscriminately t)
;(setq cperl-indent-level 4)
;(setq cperl-continued-statement-offset 4)
;(setq cperl-brace-offset -4)
;(setq cperl-label-offset -4)
;(setq cperl-comment-column 40)

;;; cperl-mode
(setq-default indent-tabs-mode nil)
(setq fill-column 78)
(setq auto-fill-mode t)
(setq cperl-close-paren-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-indent-level 4)
(setq cperl-indent-parens-as-block t)
(setq cperl-tab-always-indent t)

(setq cperl-hairy t)
(setq cperl-highlight-variables-indiscriminately t)


;; sh-mode
(autoload 'sh-mode "sh-mode" "Shell script mode" t)
(setq auto-mode-alist
      (cons '("\\.sh$" . sh-mode) auto-mode-alist))


;; php-mode
(defun php-mode-hook ()
  (setq tab-width 4
        c-basic-offset 4
        c-hanging-comment-ender-p nil
    indent-tabs-mode
    (not
     (and (string-match "/\(PEAR\|pear\)/" (buffer-file-name))
          (string-match ".php$" (buffer-file-name)))))
  )
