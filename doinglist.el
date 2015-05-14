
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
  (doinglist-mode))

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
