(require 'paredit)
(require 'rainbow-delimiters)

(defun chicken-initialize-scheme-mode ()
  (enable-paredit-mode)
  (rainbow-delimiters-mode))

(eval-after-load 'cmuscheme
  '(define-key scheme-mode-map (kbd "C-c C-c") 'scheme-send-definition))

(add-hook 'scheme-mode-hook #'chicken-initialize-scheme-mode)

(defvar chicken-install-prefix "/usr")

(setq scheme-program-name (concat chicken-install-prefix "/bin/csi -:d"))

(setq scheme-special-forms
      '(module if and-let* parameterize handle-exceptions
        when unless match bind-lambda bind-lambda*
        match-let match-let* match-letrec if-let if-let*))

(dolist (special-form scheme-special-forms)
  (put special-form 'scheme-indent-function 1))

