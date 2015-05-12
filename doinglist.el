
(define-derived-mode doinglist-mode
  text-mode "Doing-List"
  "Major mode to create, edit and manage your DoingList

\\{doinglist-mode-map}"
)

(defun doinglist ()
  "Find new DoingList"
  (interactive)
  (find-file (format-time-string "doing-%Y%m%d.txt"))
  (doinglist-mode))
