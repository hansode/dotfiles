;;; cake.el ---  CakePHP Minor Mode
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2008 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.2.5
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;; Thanks to xcezx for directory create patch.(0.0.8)
;; Thanks to custar for "search app path" idea.(0.1.6)

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'cake)

;;; Change Log
;; 0.0.3: cake-switch-to-view関数をCakePHP1.1.x.x,1.2.x.x両方の拡張子に対応
;; 0.0.4: 単数系/複数系変換用辞書ファイル実装 cake-switch-to-model関数を実装
;; 0.0.5: cake-switch-to-*関数で対象ファイルがない場合に、新規ファイルを作成するかどうかを対話的に聞くように変更
;; 0.0.6: cake-open-*-dir関数を実装
;; 0.0.7: cake-open-helpers-dir,cake-open-layouts-dir関数を実装,各regexpを修正
;; 0.0.8: id:xcezx Patch適用。cake-switch-to-viewのときにディレクトリが存在しない場合に作成するように修正
;; 0.0.9: viewファイルがxxx_xxx_xxx.ctpだったときにcake-switch-to-controllerでfunction xxxXxxXxx()も探索するように修正
;; 0.1.0: controllerのアクションがfunction xxxXxxXxx()だったときにviewファイルでxxx_xxx_xxx.ctpも探索するように修正
;; 0.1.1: cake-switch-to-javascript関数を実装。global-set-keyをコメントアウト
;; 0.1.2: cake-set-app-pathが実ファイルがないバッファのときにエラーを出していたバグを修正
;; 0.1.3: cake-tail-log関数を実装。cake-switch-to-modelの冗長なifを削除
;; 0.1.4: cake-tail-logの冗長なprognを削除。
;; 0.1.5: 任意のCakePHPプロジェクトディレクトリ名に対応
;; 0.1.6: custar Idea適用。appディレクトリの判定ロジックを変更。
;; 0.1.7: cake-switch-to-extention関数をCakePHP1.2.x.xの書き方に対応。
;; 0.1.8: views/:controller/:extension/:action.ctp上でのswitch-to-controller()関数の対応
;; 0.1.9: cl導入。anything導入。views/:controller/:extension/:action.ctp上でのswitch-to-view()関数の対応
;; 0.2.0: cake-open-*-dirをanything.el対応
;; 0.2.1: cake-switch-to-function関数を実装
;; 0.2.2: executable-findを導入
;; 0.2.3: cake-switch-to-javascript関数のregexp bug fix
;; 0.2.4: cake-open-views-dir関数のロジックの修正。cake-is-model-file bug fix
;; 0.2.5: cake-switch-to-function関数の挙動修正。cake-tail-log関数 bug fix

;;; TODO
;; リファクタリング
;; anythingのname属性を動的に変更できないか?(cake-open-dir)
;; cake-open-*-dirのファイル候補をディレクトリ再帰的に見るかどうかのフラグを作成する

;;; Code:

;; mode
(defvar cake nil
  "CakePHP minor mode.")

;;(global-set-key "\C-c\C-v" 'cake)

;;require
(require 'inflector)
(require 'cl)
(require 'anything)

(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 5000)

;; setq minor-mode-alist
(if (not (assq 'cake minor-mode-alist))
    (setq minor-mode-alist
          (cons '(cake " Cake")
                minor-mode-alist)))

;; defun cake
(defun cake (&optional arg)
  "CakePHP minor mode"
  (interactive)
  ;; mode variable settings
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq cake nil))
   (arg
    (setq cake t))
   (t
    (setq cake (not cake))))
  ;; content
  (if cake
      (progn
        (setq minor-mode-map-alist
              (cons (cons 'cake cake-key-map)
                    minor-mode-map-alist))
        )
    nil)
  )

;; key-map
(defvar cake-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cs" 'cake-switch)
    (define-key map "\C-cm" 'cake-switch-to-model)
    (define-key map "\C-cv" 'cake-switch-to-view)
    (define-key map "\C-cc" 'cake-switch-to-controller)
    (define-key map "\C-cf" 'cake-switch-to-function)
    (define-key map "\C-ce" 'cake-switch-to-element)
    (define-key map "\C-cj" 'cake-switch-to-javascript)
    (define-key map "\C-cM" 'cake-open-models-dir)
    (define-key map "\C-cV" 'cake-open-views-dir)
    (define-key map "\C-cC" 'cake-open-controllers-dir)
    (define-key map "\C-cL" 'cake-open-layouts-dir)
    (define-key map "\C-cH" 'cake-open-helpers-dir)
    (define-key map "\C-cE" 'cake-open-elements-dir)
    (define-key map "\C-cJ" 'cake-open-js-dir)
    (define-key map "\C-cS" 'cake-open-css-dir)
    (define-key map "\C-c\C-l" 'cake-tail-log)
    map))

(defvar cake-app-name "app"
  "CakePHP app directory name")

(defvar cake-app-path nil
  "CakePHP app directory path.")

(defvar cake-action-name "index"
  "CakePHP action name.")

(defvar cake-lower-camelized-action-name "index"
  "CakePHP lower camelized action name.")

(defvar cake-snake-action-name "index"
  "CakePHP snake_case action name.")

(defvar cake-view-extension "ctp"
  "CakePHP view extension.")

(defvar cake-singular-name nil
  "CakePHP current singular name.")

(defvar cake-plural-name nil
  "CakePHP current plural name.")

(defvar cake-app-path-regexp "^\\(.+/app/\\)\\(models\\|views\\|controllers\\|config\\|locale\\|plugins\\|tests\\|tmp\\|webroot\\|vendors\\|index\.php\\)$/"
  "App path regExp.")

(defvar cake-model-regexp "^\\(.+/app/\\)models/\\([^/]+\\)\.php$"
  "Model file regExp.")

(defvar cake-view-regexp "^\\(.+/app/\\)views/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"
  "View file regExp.")

(defvar cake-controller-regexp "^\\(.+/app/\\)controllers/\\([^/]+\\)_controller\.php$"
  "Contoroller file regExp.")

(defun cake-is-model-file ()
  "Check whether current file is model file."
  (cake-set-app-path)
  (if (string-match cake-model-regexp (buffer-file-name))
      (progn
        (setq cake-app-path (match-string 1 (buffer-file-name)))
        (setq cake-singular-name (match-string 2 (buffer-file-name)))
        (cake-convert-singular-to-plural cake-plural-rules)
        t)
    nil))

(defun cake-is-view-file ()
  "Check whether current file is view file."
  (cake-set-app-path)
  (if (string-match cake-view-regexp (buffer-file-name))
      (progn
        (setq cake-app-path (match-string 1 (buffer-file-name)))
        (setq cake-plural-name (match-string 2 (buffer-file-name)))
        (setq cake-action-name (match-string 4 (buffer-file-name)))
        (setq cake-view-extension (match-string 5 (buffer-file-name)))
        (setq cake-lower-camelized-action-name (cake-lower-camelize cake-action-name))
        (cake-convert-plural-to-singular cake-singular-rules)
        t)
    nil))

(defun cake-is-controller-file ()
  "Check whether current file is contoroller file."
  (cake-set-app-path)
  (if (string-match cake-controller-regexp (buffer-file-name))
      (progn
        (setq cake-app-path (match-string 1 (buffer-file-name)))
        (setq cake-plural-name (match-string 2 (buffer-file-name)))
        (save-excursion
          (if
              (not (re-search-backward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t))
              (re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)))
        (setq cake-action-name (match-string 1))
        (setq cake-snake-action-name (cake-snake cake-action-name))
        (cake-convert-plural-to-singular cake-singular-rules)
        t)
    nil))

(defun cake-get-current-line ()
  "Get current line."
  (let ((line-start (progn
                      (beginning-of-line)
                      (point)))
        (line-end (progn
                    (end-of-line)
                    (point))))
    (buffer-substring line-start line-end)))

(defun cake-set-app-path ()
  "Set app path."
  (if (cake-is-app-path)
      (if (string-match cake-app-path-regexp (buffer-file-name))
          (setq cake-app-path (match-string 1 (buffer-file-name)))
        nil)
    nil))

(defun cake-is-app-path ()
  "Check app directory name and set regExp."
  (let ((temp-root-path nil) (temp-app-name nil))
    (if (buffer-file-name)
        (if (string-match "^\\(.+/\\)\\([^/]+\\)/\\(models\\|views\\|controllers\\|config\\|locale\\|plugins\\|tests\\|tmp\\|webroot\\|vendors\\|index\.php\\)" (buffer-file-name))
            (progn
              (setq temp-root-path (match-string 1 (buffer-file-name)))
              (setq temp-app-name (match-string 2 (buffer-file-name)))
              (if (file-exists-p (concat temp-root-path temp-app-name "/config/core.php"))
                  (progn
                    (setq cake-app-name temp-app-name)
                    (setq cake-app-path-regexp (concat "^\\(.+/" cake-app-name "/\\)\\(models\\|views\\|controllers\\|config\\|locale\\|plugins\\|tests\\|tmp\\|webroot\\|vendors\\|index\.php\\)/"))
                    (setq cake-view-regexp (concat "^\\(.+/" cake-app-name "/\\)views/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"))
                    (setq cake-controller-regexp (concat "^\\(.+/" cake-app-name "/\\)controllers/\\([^/]+\\)_controller\.php"))
                    t)
                nil))
          nil)
      nil)))

(defun cake-convert-singular-to-plural (list)
  "Convert singular name To plural name."
  (if list
      (progn
        (if (string-match (nth 0 (car list)) cake-singular-name)
            (progn
              (setq cake-plural-name (replace-match (nth 1 (car list)) nil nil cake-singular-name))
              (setq list nil)))
        (cake-convert-singular-to-plural
         (cdr list)))))

(defun cake-convert-plural-to-singular (list)
  "Convert plural name To singular name."
  (if list
      (progn
        (if (string-match (nth 0 (car list)) cake-plural-name)
            (progn
              (setq cake-singular-name (replace-match (nth 1 (car list)) nil nil cake-plural-name))
              (setq list nil)))
        (cake-convert-plural-to-singular
         (cdr list)))))

(defun cake-switch-to-model ()
  "Switch to model."
  (interactive)
  (if (or (cake-is-view-file) (cake-is-controller-file))
      (if (file-exists-p (concat cake-app-path "models/" cake-singular-name ".php"))
          (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
        (if (y-or-n-p "Make new file?")
            (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
          (message (format "Can't find %s" (concat cake-app-path "models/" cake-singular-name ".php")))))
    (message "Can't find model name.")))

(defun cake-switch-to-view ()
  "Switch to view."
  (interactive)
  (let ((view-files nil))
    (if (or (cake-is-model-file) (cake-is-controller-file))
        (progn
          (if (cake-is-model-file) (cake-convert-singular-to-plural cake-plural-rules))
          (setq view-files (cake-set-view-list))
          (if view-files
              (cond
               ((= 1 (length view-files)) (find-file (concat cake-app-path "views/" cake-plural-name "/" (car view-files))))
               (t (anything
                   '(((name . "Switch to view")
                      (candidates . view-files)
                      (display-to-real . (lambda (candidate)
                                           (concat cake-app-path "views/" cake-plural-name "/" candidate)
                                           ))
                      (type . file)))
                   nil nil nil nil)
                  ))
            (if (y-or-n-p "Make new file?")
                (progn
                  (unless (file-directory-p (concat cake-app-path "views/" cake-plural-name "/"))
                    (make-directory (concat cake-app-path "views/" cake-plural-name "/")))
                  (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
              (message (format "Can't find %s" (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension))))))
      (message "Can't switch to view."))))

(defun cake-set-view-list ()
  "Set view list"
  (let ((dir (concat cake-app-path "views/" cake-plural-name))
        (view-dir nil)
        (view-files nil))
    (unless (not (file-directory-p dir))
      (setq view-dir (remove-if-not (lambda (x) (file-directory-p (concat cake-app-path "views/" cake-plural-name "/" x))) (directory-files dir)))
      (setq view-dir (remove-if (lambda (x) (equal x "..")) view-dir))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name "." cake-view-extension))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name "." cake-view-extension) y)) view-files)
                                       (push (concat x "/" cake-snake-action-name "." cake-view-extension) view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name ".thtml"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name ".thtml") y)) view-files)
                                       (push (concat x "/" cake-snake-action-name ".thtml") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name ".ctp"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name ".ctp") y)) view-files)
                                       (push (concat x "/" cake-snake-action-name ".ctp") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name "." cake-view-extension))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name "." cake-view-extension) y)) view-files)
                                       (push (concat x "/" cake-action-name "." cake-view-extension) view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name ".thtml"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name ".thtml") y)) view-files)
                                       (push (concat x "/" cake-action-name ".thtml") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name ".ctp"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name ".ctp") y)) view-files)
                                       (push (concat x "/" cake-action-name ".ctp") view-files)))))
    view-files))

(defun cake-switch-to-controller ()
  "Switch to contoroller."
  (interactive)
  (if (or (cake-is-model-file) (cake-is-view-file))
      (progn
        (if (cake-is-model-file) (cake-convert-singular-to-plural cake-plural-rules))
        (if (file-exists-p (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
            (progn
              (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
              (goto-char (point-min))
              (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
                  (progn
                    (goto-char (point-min))
                    (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t)))
              (recenter))
          (if (y-or-n-p "Make new file?")
              (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
            (message (format "Can't find %s" (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))))))
    (message "Can't switch to contoroller.")))

(defun cake-search-functions ()
  "Search function from current buffer."
  (let ((func-list nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)
        (push (match-string 1) func-list))
      func-list)))

(defun cake-switch-to-function ()
  "Switch to function."
  (interactive)
  (let ((current-func nil))
    (if (cake-is-controller-file)
        (progn
          (setq current-func (cake-search-functions))
          (anything
           '(((name . "Switch to current function")
              (candidates . current-func)
              (display-to-real . (lambda (candidate)
                                   (concat "function[ \t]*" candidate "[ \t]*\(")))
              (action
               ("Switch to Function" . (lambda (candidate)
                                         (goto-char (point-min))
                                         (re-search-forward candidate nil t)
                                         (recenter)
                                         )))))
           nil nil nil nil))
      (message "Can't switch to function."))))

(defun cake-switch-to-element ()
  "Switch to element."
  (interactive)
  (if (cake-set-app-path)
      (if (or (string-match "renderElement(['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"])" (cake-get-current-line))
              (string-match "element(['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"])" (cake-get-current-line)))
          (if (file-exists-p (concat cake-app-path "views/elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
              (find-file (concat cake-app-path "views/elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
            (if (y-or-n-p "Make new file?")
                (find-file (concat cake-app-path "views/elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
              (message (format "Can't find %s" (concat cake-app-path "views/elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension)))))
        (message "Can't find element name."))
    (message "Can't set app path.")))

(defun cake-switch-to-javascript ()
  "Switch to javascript."
  (interactive)
  (if (cake-set-app-path)
      (if (string-match "$javascript->link(['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"]" (cake-get-current-line))
          (cond
           ((file-exists-p (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line))))
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)))))
           ((file-exists-p (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js"))
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js")))
           ((y-or-n-p "Make new file?")
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js")))
           (message (format "Can't find %s" (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js"))))
        (message "Can't find javascript name."))
    (message "Can't set app path.")))

(defun cake-switch ()
  "Switch V <-> C."
  (interactive)
  (cond ((cake-is-view-file) (cake-switch-to-controller))
        ((cake-is-controller-file) (cake-switch-to-view))
        (t (message "Current buffer is neither view nor controller."))))

(defun cake-open-dir (dir)
  "Open directory."
  (interactive)
  (let ((files nil))
    (if (cake-set-app-path)
        (if (file-directory-p (concat cake-app-path dir))
            (progn
              (setq files (directory-files (concat cake-app-path dir)))
              (anything
               '(((name . "Open directory")
                  (candidates . files)
                  (display-to-real . (lambda (candidate)
                                       (concat cake-app-path dir candidate)
                                       ))
                  (type . file)))
               nil nil nil nil))
          (message (concat "Can't open " cake-app-path dir)))
      (message "Can't set app path."))))

(defun cake-open-models-dir ()
  "Open models directory."
  (interactive)
  (cake-open-dir "models/"))

(defun cake-open-views-dir ()
  "Open views directory."
  (interactive)
  (if (or (cake-is-model-file) (cake-is-controller-file) (cake-is-view-file))
      (cake-open-dir (concat "views/" cake-plural-name "/"))
    (cake-open-dir "views/")))

(defun cake-open-controllers-dir ()
  "Open contorollers directory."
  (interactive)
  (cake-open-dir "controllers/"))

(defun cake-open-helpers-dir ()
  "Open helpers directory."
  (interactive)
  (cake-open-dir "views/helpers/"))

(defun cake-open-layouts-dir ()
  "Open layouts directory."
  (interactive)
  (cake-open-dir "views/layouts/"))

(defun cake-open-elements-dir ()
  "Open elements directory."
  (interactive)
  (cake-open-dir "views/elements/"))

(defun cake-open-js-dir ()
  "Open JavaScript directory."
  (interactive)
  (cake-open-dir "webroot/js/"))

(defun cake-open-css-dir ()
  "Open css directory."
  (interactive)
  (cake-open-dir "webroot/css/"))

(defun cake-logs ()
  "Set logs list."
  (if (cake-set-app-path)
      (mapcar
       (function (lambda (el)
                   (if (listp el) el(cons el el))))
       (directory-files (concat cake-app-path "tmp/logs/") nil "\\.log$"))
    nil))

(defun cake-tail-log (log)
  "Show log by \"tail\"."
  (interactive
   (list (completing-read "tail log: " (cake-logs) nil t "debug.log")))
  (let ((logbuffer (concat "*" log "*")))
    (if (and (cake-logs) (executable-find "tail"))
        (progn
          (unless (get-buffer logbuffer)
            (get-buffer-create logbuffer)
            (set-buffer logbuffer)
            (insert-before-markers (concat "tail -f" cake-app-path "tmp/logs/" log "\n"))
            (setq buffer-read-only t)
            (start-process "tail" logbuffer "tail" "-f" (concat cake-app-path "tmp/logs/" log)))
          (switch-to-buffer logbuffer))
      (message "Can't set log."))))

(defun cake-lower-camelize (str)
  "Lower camelize."
  (let ((head-str "")(tail-str "")(default-case default-case-fold-search))
    (setq case-fold-search nil)
    (if (string-match "^\\([a-z]+_\\)\\([a-z0-9_]*\\)" (downcase str))
        (progn
          (setq head-str (match-string 1 (downcase str)))
          (setq tail-str (match-string 2 (downcase str)))
          (if (string-match "_" head-str)
              (setq head-str (replace-match "" nil nil head-str)))
          (setq tail-str (capitalize tail-str))
          (while (string-match "_" tail-str)
            (setq tail-str (replace-match "" nil nil tail-str)))
          (setq case-fold-search default-case)
          (concat head-str tail-str))
      str)))

(defun cake-snake (str)
  "Change snake_case."
  (let ((head-str "")(tail-str "")(default-case default-case-fold-search))
    (setq case-fold-search nil)
    (if (string-match "^\\([A-Z]?\\)\\([a-zA-Z0-9_]*\\)" str)
        (progn
          (setq head-str (match-string 1 str))
          (setq tail-str (match-string 2 str))
          (if (string-match "[A-Z]" head-str)
              (setq head-str (downcase head-str)))
          (while (string-match "\\([^_]\\)\\([A-Z]\\)" tail-str)
            (setq tail-str (replace-match "\\1_\\2" nil nil tail-str)))
          (setq case-fold-search default-case)
          (downcase (concat head-str tail-str)))
      str)))

;; mode provide
(provide 'cake)

;;; end
;;; cake.el ends here