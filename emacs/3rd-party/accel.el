;;;    Go to Google Groups Home 
;;;    Groups 
;;;    Advanced Groups Search    Groups Help
;;;    _______________________________ Google Search
   
;;;    Groups search result 1 for accel.el emacs
   
;;;                                                           Search Result 1
                                                                         
;;;    From: Kurt Partridge (kepart@cs.washington.edu)
;;;    Subject: accel.el: accelerated keyboard motion
;;;    Newsgroups: gnu.emacs.sources
   
;;;         View: (This is the only article in this thread) | Original Format
                                                                         
;;;    Date: 1996/06/06
   
;;; accel.el --- accelerate repeated emacs commands

;;; Commentary:

;; Description:
;; ------------
;;
;; I find this package to be a nifty little time-saver.  When holding down
;; a vertical cursor movement command (such as next-line) this command
;; gradually increases the argument to the function so the longer you hold
;; it down, the greater the motion.  The effect somewhat mimics the fancy
;; (all?) mouse drivers that move the pointer further when the mouse is
;; moved more quickly.  Yes, it often overshoots, but I find this less
;; annoying that having to wait for the cursor to scroll all the way
;; across a large window (of course, many will disagree with me on this
;; point).
;;
;; In addition to "accelerating" cursor motion, it accelerates window pane
;; resizing (if you prefer the keyboard to the mouse), and window
;; scrolling.  You can also finely adjust the rates of acceleration to
;; suit your tastes.  And you can also accelerate any other command
;; easily.

;; Installation:
;; -------------
;;   Add this lines in your .emacs:
;;     (load "accel" t t)
;;
;;   This automatically installs C-n and C-p to be accel-next-line and
;;   accel-previous-line, respectively.  For other accelerated commands,
;;   see the end of this file.

;; Author: Kurt Partridge <kepart@cs.washington.edu>
;; Maintainer: Kurt Partridge <kepart@cs.washington.edu>
;; Created: 06 June 1996
;; Version: 
;; Keywords:

;; LCD Archive Entry:
;; accel|Kurt Partridge|kepart@cs.washington.edu|
;; Accelerate cursor movement.|
;; 06-Jun-96|Version 1.?|

;; Copyright (C) 1996 Kurt Partridge

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

(defvar accel-level 0
  "Current accel level.  Each level defines its own prefix-argument.")

(defvar accel-times 0
  "Number of times accel functions have been successively called.")

(defmacro defun-accel-version (newname command accel-table)
  "Macro to generate an accelerated version called NEWNAME of COMMAND.
The accelerated version works by feeding the normal version succesively
larger prefix arguments as it is repeatedly invoked.  ACCEL-TABLE contains
a list of pairs; the car of each pair is the number of times a command is
repeated before the cdr of the pair becomes the next prefix argument.
Example:

(defun-accel-version accel-next-line next-line '((0 . 1) (5 . 2) (8 . 4))))

creates a new function called accel-next-line that behaves like next-line
for the first five lines, then moves two lines at a time, and after 8
invocations moves 4 lines at a time."
  (`
(defun (, newname) (accel-prefix-arg)
  (interactive "p")
  (if (not (eq accel-prefix-arg 1))
      ((, command) accel-prefix-arg)
    (if (not (eq this-command last-command))
        (setq accel-level 0
              accel-times 0))
    (let ((next-threshold (car (nth (1+ accel-level) (, accel-table))))
          prefix-arg)
      (if (and next-threshold
               (> accel-times next-threshold))
          (setq accel-level (1+ accel-level)))
      (setq prefix-arg (cdr (nth accel-level (, accel-table))))
      (or prefix-arg
          (setq prefix-arg (cddr (last (, accel-table)))))
      ((, command) prefix-arg)
      (setq accel-times (1+ accel-times))
      (if (sit-for 0.1)
          (setq this-command nil)))))))

;;
;; Standard accelerated functions.  Adjust the parameters as you wish (see
;; the documentation of defun-accel-version above for details).
;;

(defun-accel-version accel-scroll-up
        scroll-up
        '((0 . 1) (1 . 2) (4 . 4) (8 . 5)))
(defun-accel-version accel-scroll-down
        scroll-down
        '((0 . 1) (1 . 2) (4 . 4) (8 . 5)))
(defun-accel-version accel-scroll-left
        scroll-left
        '((0 . 1) (1 . 2) (6 . 4) (11 . 6)))
(defun-accel-version accel-scroll-right
        scroll-right
        '((0 . 1) (1 . 2) (6 . 4) (11 . 6)))

(defun-accel-version accel-next-line
        next-line
        '((0 . 1) (5 . 2) (8 . 4)))
(defun-accel-version accel-previous-line
        previous-line
        '((0 . 1) (5 . 2) (8 . 4)))
(defun-accel-version accel-forward-char
        forward-char
        '((0 . 1) (5 . 2) (8 . 4)))
(defun-accel-version accel-backward-char
        backward-char
        '((0 . 1) (5 . 2) (8 . 4)))

(defun-accel-version accel-shrink-window
        shrink-window
        '((0 . 1) (1 . 2) (4 . 3) (6 . 4)))
(defun-accel-version accel-enlarge-window
        enlarge-window
        '((0 . 1) (1 . 2) (4 . 3) (6 . 4)))

;;
;; Installation
;;

;; Some of these commented-out keybindings are nonstandard, and I'm not
;; sure how compatible they are with other versions of emacs.  Note that
;; accel-scroll-up and accel-scroll-down scroll lines at a time, not
;; screenfuls.
;;
;; *WARNING*: I had accel-scroll-up and accel-scroll-down bound to M-{ and
;; M-}, but the strain placed on my pinky from repetitive usage started to
;; make my hand go numb.  I move them to the bindings below and things
;; started to get better (although I wound up purchasing an alternative
;; keyboard, which really made the difference).  Anyhow, bind with care.

;(global-set-key "\e9"          'accel-scroll-up)
;(global-set-key "\e0"          'accel-scroll-down)
;(global-set-key [?\C-,]        'accel-scroll-left)
;(global-set-key [?\C-.]        'accel-scroll-right)

(global-set-key "\C-n"         'accel-next-line)
(global-set-key "\C-p"         'accel-previous-line)

;; These were just too annoying to have accelerated; but you may find
;; otherwise.
;(global-set-key "\C-f"         'accel-forward-char)
;(global-set-key "\C-b"         'accel-backward-char)

;(global-set-key [?\C-9]        'accel-enlarge-window)
;(global-set-key [?\C-0]        'accel-shrink-window)

;; accel.el ends here
