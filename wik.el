;; Color Theme
(color-theme-blackboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
	  (comment-or-uncomment-region (mark) (point))
	(comment-or-uncomment-region (point) (mark))
	)
    (comment-or-uncomment-line lines)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control \;)] 'comment-or-uncomment-region-or-line)
