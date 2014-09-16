;;; shell-pack.el --- shell configuration

;;; Commentary:

;;; Code:

;(require 'install-packages-pack)
(install-packs '(multi-term
                 exec-path-from-shell
                 smartscan))

(require 'smartscan)
(require 'term)
(require 'shell)

;; setup the path
(require 'exec-path-from-shell)

;; Add some env variables so that emacs is aware too
(eval-after-load 'exec-path-from-shell
  (lambda ()
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "LD_LIBRARY_PATH" "http_proxy" "https_proxy" "ftp_proxy" "rsync_proxy" "no_proxy"))
      (add-to-list 'exec-path-from-shell-variables var))))

(exec-path-from-shell-initialize)


(defun term-send-tab ()
  (interactive)
  (term-send-raw-string "\t"))

;; With this snippet, another press of C-d will kill the term buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "Delete ARG char or kill buffer if we hit the end of the file."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun shell-pack/mode-and-simple-bindings-fn ()
  "Simple binding definition and add smartscan mode."
  (local-set-key (kbd "C-c C-j") 'term-line-mode)
  (smartscan-mode))

(defun shell-pack/close-buffer-hook-fn ()
  "Hook function to kill the buffer given a specific binding."
  (local-set-key (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))


(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook))
  (add-hook hook 'shell-pack/close-buffer-hook-fn))


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

;(provide 'shell-pack)

;;; shell-pack.el ends here
