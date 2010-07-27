(require 'quail)
(require 'cyrillic)

;; JabberEl
(add-to-list 'load-path (concat dotfiles-dir "/vendor/emacs-jabber-0.8.0"))
(require 'jabber-autoloads)

;; Re-load custom.el at last
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

(defun reset-flyspell-with-new-dict (dict)
  "Set new dictionary and restart flyspell"

  (unless (equal dict ispell-local-dictionary)
    (setq ispell-local-dictionary dict)
    (when flyspell-mode
      (flyspell-mode)
      (flyspell-mode)))

  (when flyspell-mode
    (save-excursion
      (flyspell-region (window-start) (window-end))))

  (message nil))

(defun toggle-specified-isearch-input-method (new-input-method)
  "Toggle specified input method in interactive search."
  (interactive)
  (let ((overriding-terminal-local-map nil)))

  (if (eq new-input-method 'default-method)
      (inactivate-input-method)
    (set-input-method new-input-method))

  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control \;)] 'comment-or-uncomment-region-or-line)

(global-set-key [(f31)]
                (lambda ()
                  (interactive)
                  (reset-flyspell-with-new-dict "american")
                  (inactivate-input-method)))

(global-set-key [(f32)]
                (lambda ()
                  (interactive)
                  (reset-flyspell-with-new-dict "russian")
                  (set-input-method 'russian-computer)))

(global-set-key [(f33)]
                (lambda ()
                  (interactive)
                  (reset-flyspell-with-new-dict "ukrainian")
                  (set-input-method 'ukrainian-computer)))

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

(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map (kbd "")
              (lambda ()
                (interactive)
                (toggle-specified-isearch-input-method 'default-method)))

            (define-key isearch-mode-map (kbd "")
              (lambda ()
                (interactive)
                (toggle-specified-isearch-input-method 'russian-computer)))

            (define-key isearch-mode-map (kbd "")
              (lambda ()
                (interactive)
                (toggle-specified-isearch-input-method 'ukrainian-computer)))))
