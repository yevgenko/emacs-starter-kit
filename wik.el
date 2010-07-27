;; JabberEl
(add-to-list 'load-path (concat dotfiles-dir "/vendor/emacs-jabber-0.8.0"))
(require 'jabber-autoloads)

;; Re-load custom.el after all
(load custom-file 'noerror)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable assignement ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(eval-after-load 'jabber
  '(progn
     ;; Jabber Message alert hooks
     (define-jabber-alert echo "Show a message in the echo area"
       (lambda (msg)
         (unless (minibuffer-prompt)
           (message "%s" msg))))
     
     ;; Automatically highlight URLs
     (add-hook 'jabber-chat-mode-hook 'goto-address)
     ))
