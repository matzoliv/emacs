(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))

(defun new-json-scratch ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*temp-json*"))
  (yank)
  (json-pretty-print-buffer)
  (js2-mode))

(add-hook 'json-mode-hook #'json-mode-initialize)

(global-set-key (kbd "C-c b j") 'new-json-scratch)
