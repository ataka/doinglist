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

(defvar doinglist-last-date)
(defconst doinglist-indent-checkbox-regexp "^\\([[:blank:]]*\\)")
(defconst doinglist-checkbox-regexp
  (concat doinglist-indent-checkbox-regexp "\\(\\[[ x]\\]\\)")
  "Regexp of Doing List checkbox")
(defconst doinglist-checked-checkbox-regexp
  (concat doinglist-indent-checkbox-regexp "\\(\\[x\\]\\)"))
(defconst doinglist-unchecked-checkbox-regexp
  (concat doinglist-indent-checkbox-regexp "\\(\\[ \\]\\)"))
(defvar doinglist-data-file ".doinglist"
  "Doing List data file")

;;; Custom Variables

(defgroup doinglist nil
  "Doing List configuration."
  :group 'wp)

(defcustom doinglist-data-directory "~/doinglist"
  "The directory in which to store Doing List data as files."
  :type 'directory
  :group 'doinglist)

(defcustom doinglist-use-last-doinglist t
  "If Non-Nil, create today's doinglist from last doinglist data"
  :type 'boolean
  :group 'doinglist)

(defcustom doinglist-template-list nil
  "List of doinglist items, inserted when find new doinglist file"
  :type '(list (repeat string))
  :group 'doinglist)

;;; Doing List mode

(add-to-list 'auto-mode-alist '("\\.doinglist\\'" . doinglist-mode))

(define-derived-mode doinglist-mode
  text-mode "Doing-List"
  "Major mode to create, edit and manage your DoingList

\\{doinglist-mode-map}"
)

(defun doinglist ()
  "Find new DoingList"
  (interactive)
  (find-file (expand-file-name (format-time-string "%Y%m%d.doinglist")
                               doinglist-data-directory))
  (when (doinglist-new-doinglist-p)
    (when doinglist-use-last-doinglist
      (doinglist-update-data)
      (let ((old-file (expand-file-name
                       (format "%s.doinglist" doinglist-last-date)
                       doinglist-data-directory)))
        (when (file-exists-p old-file)
          (insert-file-contents old-file)
          (doinglist-remove-checked-items)
          (goto-char (point-max)))))
    (doinglist-insert-new-item 0)
    (doinglist-insert-template)))

(defun doinglist-new-doinglist-p ()
  (and (bobp) (eobp)))

(defun doinglist-update-data ()
  (let ((file (expand-file-name doinglist-data-file doinglist-data-directory))
        tmp)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)        
        (setq tmp (doinglist-read))))
    (with-current-buffer (find-file-noselect file)
      (doinglist-write (format-time-string "%Y%m%d"))
      (basic-save-buffer))
    (when tmp
      (setq doinglist-last-date tmp))))

(defun doinglist-read ()
  (save-excursion
    (read (current-buffer))))

(defun doinglist-write (data)
  (save-excursion
    (delete-region (point-min) (point-max))
    (print data (current-buffer))))

(defun doinglist-remove-checked-items ()
  (save-excursion
    (doinglist-beginning-of-items)
    (while (re-search-forward doinglist-checked-checkbox-regexp nil t)
      (let ((beg (progn (forward-line 0) (point)))
            (end (progn (forward-line 1) (point))))
        (delete-region beg end)))))

(defun doinglist-insert-template ()
  (save-excursion
    (doinglist-beginning-of-items)
    (forward-line 0)
    (insert (doinglist-template-string) "\n")))

(defun doinglist-template-string ()
  (mapconcat (lambda (item)
               (concat (doinglist-new-item 0) item))
             doinglist-template-list "\n"))

;;
;; keymap
;;

(let ((map doinglist-mode-map))
  (define-key map "\r" 'doinglist-newline-and-insert-new-item)
  (define-key map "\t" 'doinglist-indent-item)
  (define-key map [(backtab)] 'doinglist-unindent-item)
  (define-key map "\C-c\C-c" 'doinglist-toggle-check)
)

;;
;; Functions for doinglist-mode
;;

(defun doinglist-newline-and-insert-new-item ()
  (interactive)
  (insert "\n")
  (doinglist-insert-new-item))

(defun doinglist-insert-new-item (&optional level)
  (interactive "P")
  (unless level (setq level (doinglist-get-level -1)))
  (insert (doinglist-new-item level)))

(defun doinglist-get-level (&optional arg)
  (save-excursion
    (beginning-of-line)
    (if (and (not (bobp))
             (progn (forward-line arg)
                    (looking-at doinglist-checkbox-regexp)))
        (/ (length (match-string 1)) 2)
      0)))

(defun doinglist-new-item (level &optional checked)
  (let ((check-box (if checked "[x]" "[ ]")))
    (concat (doinglist-indent level) check-box " ")))

(defun doinglist-indent (level)
  (let* ((indent (* level 2))
         (indent-fmt (concat "%" (number-to-string indent) "s")))
    (format indent-fmt "")))

(defun doinglist-indent-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((level (doinglist-get-level -1)))
      (when (looking-at doinglist-checkbox-regexp)
        (replace-match (concat (doinglist-indent (1+ level))
                               (match-string 2)))))))

(defun doinglist-unindent-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((level (doinglist-get-level 0)))
      (when (looking-at doinglist-checkbox-regexp)
        (replace-match (concat (doinglist-indent (1- level))
                               (match-string 2)))))))


(defun doinglist-toggle-check ()
  (interactive)
  (let ((check (save-excursion
                 (beginning-of-line)
                 (looking-at "^[[:blank:]]*\\[x\\]"))))
    (doinglist-check-item check)))

(defun doinglist-check-item (arg)
  (interactive "P")
  (apply (lambda (regexp replace)
           (save-excursion
             (beginning-of-line)
             (when (looking-at regexp))
             (replace-match replace)))
         (if arg
             '("^\\([[:blank:]]*\\)\\[x\\]" "\\1[ ]")
           '(  "^\\([[:blank:]]*\\)\\[ \\]" "\\1[x]"))))

;;
;; misc functions
;;

(defun doinglist-beginning-of-items ()
  (goto-char (point-min))
  (when (re-search-forward doinglist-checkbox-regexp nil t)
    (forward-line 0)))

(provide 'doinglist)

;;; doinglist.el ends here
