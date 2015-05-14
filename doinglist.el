
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
;; Functions for doinglist-mode
;;

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

(defun doinglist-new-item (level)
  (let* ((indent (* level 2)))
    (format (concat "[ ] %" (number-to-string indent) "s") "")))
