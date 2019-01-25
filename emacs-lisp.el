(require 'paredit)
(require 'rainbow-delimiters)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-e C-d") 'edebug-defun)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(defun helm-list-functions ()
  (interactive)
  (helm
   :sources (helm-build-in-buffer-source "All functions"
              :action (helm-make-actions "Insert" (lambda (fn-name)
                                                    (describe-function (intern fn-name))))
              :init (lambda ()
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (mapatoms
                         (lambda (x)
                           (if (fboundp x)
                               (insert (symbol-name x) "\n")))))))
   :buffer "*helm all functions*"))

(global-set-key (kbd "C-c f h") 'helm-list-functions)

