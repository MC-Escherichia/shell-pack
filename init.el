;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

(install-packs '(multi-term
                 exec-path-from-shell
                 smartscan))

(require 'smartscan)
(require 'term)
(require 'shell)

;; setup the path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(defun term-send-tab ()
  (interactive)
  (term-send-raw-string "\t"))

;; With this snippet, another press of C-d will kill the term buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-pack/mode-and-simple-bindings-fn ()
  "Simple binding definition and add smartscan mode"
  (local-set-key (kbd "C-c C-j") 'term-line-mode)
  (smartscan-mode))

(defun shell-pack/close-buffer-hook-fn ()
  "Add a binding that permits to kill the buffer when C-d if no input char to kill."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

 (dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook ))
  (add-hook hook 'shell-pack/mode-and-simple-bindings-fn))


 (defun last-term-buffer (l)
      "Return most recently used term buffer."
      (when l
	(if (eq 'term-mode (with-current-buffer (car l) major-mode))
	    (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
      "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
      (interactive)
      (let ((b (last-term-buffer (buffer-list))))
	(if (or (not b) (eq 'term-mode major-mode))
	    (multi-term)
	  (switch-to-buffer b))))


(defun shell-pack/fix-tab-hook ()
  (interactive)
  (local-set-key "\t" 'term-send-tab)
  (local-set-key (kbd "M-RET") 'get-term))

(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook ))
  (add-hook hook 'shell-pack/fix-tab-hook))

;;; shell-pack.el ends here
