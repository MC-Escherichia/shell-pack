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

(dolist (hook '(sh-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook ))
  (add-hook hook 'shell-pack/mode-and-simple-bindings-fn))

(defun shell-pack/close-buffer-hook-fn ()
  "Add a binding that permits to kill the buffer when C-d if no input char to kill."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook ))
  (add-hook hook 'shell-pack/close-buffer-hook-fn))

;;; shell-pack.el ends here
