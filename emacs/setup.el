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

;;不用意な改行を防ぐ
(setq next-line-add-newlines nil)

;;;ガベージコレクションを起こりにくく
(setq gc-cons-threshold 200000)

;;;C-k で改行を含めてカット
(setq kill-whole-line t)

;;;ファイルに変更が加わったかどうかを表示
(setq transient-mark-mode t)

;;;列番号も表示
(setq column-number-mode t)

;; ミニバッファのリサイズ可
(setq resize-minibuffer-mode t)

;;実行したコマンドのキーバインドを教えてくれる
(setq-default teach-extended-commands-p t)
(setq-default teach-extended-commands-timeout 4)

;;backup
(setq make-backup-file t)
(setq backup-by-copying nil)             ;backup by renaming リネームの時
(setq backup-by-copying-when-linked t)   ;backup by copying  コピーの時
(setq backup-by-copying-when-mismath nil);backup by renaming リネームの時

;;version control
;(setq version-control nil)   ;すでにバックアップファイルがある時作成
;(setq version-control t)     ;番号付きバックアップファイルを作成
;(setq version-control 'never);番号付きバックアップファイルは作らない

;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(load-library "term/bobcat")
(global-set-key "\M-g" 'goto-line)

;; テキストモード自動改行
(setq text-mode-hook (function (lambda () (auto-fill-mode 1))))

;; Save abbrevs in ~/.abbrev_defs? (y or n)
(setq save-abbrevs nil)

;; 3rd-party lisps
(load "~/.emacs.d/3rd-party.el")
