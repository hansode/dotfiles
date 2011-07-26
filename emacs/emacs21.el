;; Set Language
;;;Default Language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-default-font "-misc-fixed-medium-r-normal-*-14-*-*-*-*-*-*")
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; color
(if window-system
    (progn
      (if (x-display-color-p)
          (progn
            (set-background-color "antiquewhite")    ;background
            (set-foreground-color "black")           ;foreground
            (set-face-background 'modeline "green4") ;mode-line background
            (set-face-foreground 'modeline "white")  ;mode-line foreground
            (set-mouse-color "red")                  ;mouse color
            (set-cursor-color "navy")                ;cursor color
            (set-face-foreground 'underline "black")   ;
            (set-face-background 'underline "skyblue") ;
            (set-face-background 'region "white")
            (set-face-foreground 'region "navy")
            )
        )
      )
  )

;; スクロールバーの位置。右か左か
;(set-scroll-bar-mode  'right)
;; Emacs21ではツールバーとメニューバーが邪魔。どうせ使わないし
;(tool-bar-mode nil)


;; common
(load "~/.emacs.d/setup.el")
(load "~/.emacs.d/prog.el")

;; local
(load "~/.emacs.d/hosts/colinux.el")

; find-fileで SPC で補間可能にする
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)

;; 末尾の空白を強調する
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))

;; 全角スペース、タブ等の様々な空白文字をハイライト
(defface my-face-b-1 '((t (:background "bisque"))) nil)
(defface my-face-b-2 '((t (:background "LemonChiffon2"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
