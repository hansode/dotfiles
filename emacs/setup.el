;;--------------------------------------------------
;; common basic environment setup
;;--------------------------------------------------

;; cd
(cd (getenv "HOME"))

;;
(setq load-path
      (append '(
                "~/.emacs.d/3rd-party/"
		)
	      load-path))

;; Color
(require 'font-lock)
;(fast-lock-mode)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; scroll bar
(menu-bar-mode -1)

;;$BITMQ0U$J2~9T$rKI$0(B
(setq next-line-add-newlines nil)

;;;$B%,%Y!<%8%3%l%/%7%g%s$r5/$3$j$K$/$/(B
(setq gc-cons-threshold 200000)

;;;C-k $B$G2~9T$r4^$a$F%+%C%H(B
(setq kill-whole-line t)

;;;$B%U%!%$%k$KJQ99$,2C$o$C$?$+$I$&$+$rI=<((B
(setq transient-mark-mode t)

;;;$BNsHV9f$bI=<((B
(setq column-number-mode t)

;; $B%_%K%P%C%U%!$N%j%5%$%:2D(B
(setq resize-minibuffer-mode t)

;;$B<B9T$7$?%3%^%s%I$N%-!<%P%$%s%I$r65$($F$/$l$k(B
(setq-default teach-extended-commands-p t)
(setq-default teach-extended-commands-timeout 4)

;;backup
(setq make-backup-file t)
(setq backup-by-copying nil)             ;backup by renaming $B%j%M!<%`$N;~(B
(setq backup-by-copying-when-linked t)   ;backup by copying  $B%3%T!<$N;~(B
(setq backup-by-copying-when-mismath nil);backup by renaming $B%j%M!<%`$N;~(B

;;version control
;(setq version-control nil)   ;$B$9$G$K%P%C%/%"%C%W%U%!%$%k$,$"$k;~:n@.(B
;(setq version-control t)     ;$BHV9fIU$-%P%C%/%"%C%W%U%!%$%k$r:n@.(B
;(setq version-control 'never);$BHV9fIU$-%P%C%/%"%C%W%U%!%$%k$O:n$i$J$$(B

;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(load-library "term/bobcat")
(global-set-key "\M-g" 'goto-line)

;; $B%F%-%9%H%b!<%I<+F02~9T(B
(setq text-mode-hook (function (lambda () (auto-fill-mode 1))))

;; Save abbrevs in ~/.abbrev_defs? (y or n)
(setq save-abbrevs nil)

;; 3rd-party lisps
(load "~/.emacs.d/3rd-party.el")
