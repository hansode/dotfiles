;;; anything-c-cake.el --- CakePHP Minor Mode anything.el interface

;; Copyright (C) 2008 by 101000code/101000LAB
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Version: 1.0.5
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;; Change Log
;; 1.0.0:action "Switch to Controller","Switch to View","Switch to Model"作成
;; 1.0.1:anything-c-cake-set-namesでcake-singular-nameが空白になるバグ修正
;; 1.0.2:action(function)がprivateやpublic指定されていた場合表示が崩れるバグを修正
;; 1.0.3:cake.el 0.1.5に対応
;; 1.0.4:executable-findを導入
;; 1.0.5:anything-c-cake-anything-only-source-cake関数を導入

;; TODO
;; anyhing-c-cake-switch-to-*がcake-switch-to-*に似ているのでなんとか小さくしたい

;;; Code:

(require 'anything)
(require 'cake)

(defvar anything-c-source-cake
  '((name . "Cake Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep") (executable-find "sed"))
             (call-process-shell-command
              (concat "grep function " cake-app-path "controllers/*controller.php | sed 's/.\\+\\/\\(.\\+\\)_controller\\.php:.\\+function *\\([^ ]\\+\\) *(.*).\\+/\\1 \\/ \\2/g'") nil (anything-candidate-buffer 'global))
           (call-process-shell-command nil nil (anything-candidate-buffer 'global))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names)
    (action
     ("Switch to Contoroller" . (lambda (candidate)
                                  (anything-c-cake-switch-to-controller)))
     ("Switch to View" . (lambda (candidate)
                           (anything-c-cake-switch-to-view)))
     ("Switch to Model" . (lambda (candidate)
                            (anything-c-cake-switch-to-model)))
     )))

(defun anything-c-cake-set-names (candidate)
  "Set names by display-to-real"
  (progn
    (string-match "\\(.+\\) / \\(.+\\)" candidate)
    (setq cake-plural-name (match-string 1 candidate))
    (setq cake-action-name (match-string 2 candidate))
    (cake-convert-plural-to-singular cake-singular-rules)
    (setq cake-lower-camelized-action-name cake-action-name)
    (setq cake-snake-action-name (cake-snake cake-action-name))
    ))

(defun anything-c-cake-switch-to-model ()
  "Switch to model."
  (interactive)
  (if (file-exists-p (concat cake-app-path "models/" cake-singular-name ".php"))
      (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
    (if (y-or-n-p "Make new file?")
        (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
      (message (format "Can't find %s" (concat cake-app-path "models/" cake-singular-name ".php")))))
  (message "Can't find model name."))

(defun anything-c-cake-switch-to-view ()
  "Switch to view."
  (interactive)
  (progn
    (cond ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name "." cake-view-extension))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name "." cake-view-extension)))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".thtml"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".thtml")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".ctp"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".ctp")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".thtml"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".thtml")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".ctp"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".ctp")))
          ((y-or-n-p "Make new file?")
           (unless (file-directory-p (concat cake-app-path "views/" cake-plural-name "/"))
             (make-directory (concat cake-app-path "views/" cake-plural-name "/")))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
          (t (message (format "Can't find %s" (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))))))

(defun anything-c-cake-switch-to-controller ()
  "Switch to contoroller."
  (interactive)
  (progn
    (if (file-exists-p (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (progn
          (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
          (goto-char (point-min))
          (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
              (progn
                (goto-char (point-min))
                (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t))))
      (if (y-or-n-p "Make new file?")
          (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (message (format "Can't find %s" (concat cake-app-path "controllers/" cake-plural-name "_controller.php")))))))

(defun anything-c-cake-anything-only-source-cake ()
  "anything only anything-c-source-cake."
  (interactive)
  (anything 'anything-c-source-cake nil nil nil nil))

(define-key cake-key-map "\C-cl" 'anything-c-cake-anything-only-source-cake)

(provide 'anything-c-cake)

;;; Code ends