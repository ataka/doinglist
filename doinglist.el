;;; doinglist.el --- Major mode for editing Doing List
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2015 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; URL: https://github.com/ataka/doinglist
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Custom Variables

(defgroup doinglist nil
  "Doing List configuration."
  :group 'wp)

(defcustom doinglist-data-directory "~/doinglist"
  "The directory in which to store Doing List data as files."
  :type 'directory
  :group 'doinglist)

;;; Doing List mode

(define-derived-mode doinglist-mode
  text-mode "Doing-List"
  "Major mode to create, edit and manage your DoingList

\\{doinglist-mode-map}"
)

(defun doinglist ()
  "Find new DoingList"
  (interactive)
  (find-file (expand-file-name (format-time-string "doing-%Y%m%d.txt")
                               doinglist-data-directory))
  (doinglist-mode)
  (when (and (bobp) (eobp))
    (insert "[ ] ")))

;;
;; keymap
;;

(let ((map doinglist-mode-map))
  (define-key map "\r" 'doinglist-newline-and-insert-new-item)
  (define-key map "\t" 'doinglist-indent-item)
)

;;
;; Functions for doinglist-mode
;;

(defun doinglist-newline-and-insert-new-item ()
  (interactive)
  (insert "\n")
  (doinglist-insert-new-item))

(defun doinglist-insert-new-item ()
  (interactive)
  (let ((level (doinglist-get-level)))
    (insert (doinglist-new-item level))))

(defun doinglist-get-level ()
  (save-excursion
    (beginning-of-line)
    (if (and (not (bobp))
             (progn (forward-line -1)
                    (looking-at "^\\[[ x]\\]\\([[:space:]]+\\)")))
        (/ (1- (length (match-string 1))) 2)
      0)))

(defun doinglist-new-item (level &optional checked)
  (let* ((indent (* level 2)))
    (format (concat (if checked "[x]" "[ ]") " %" (number-to-string indent) "s") "")))

(defun doinglist-indent-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((level (doinglist-get-level)))
      (when (looking-at "^\\[\\([ x]\\)\\][[:space:]]+")
        (replace-match (doinglist-new-item (1+ level) (equal (match-string 1) "x")))))))

(provide 'doinglist)

;;; doinglist.el ends here
