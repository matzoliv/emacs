(require 'eww)
(require 'mu4e)
(require 'mu4e-contrib)

(global-set-key (kbd "C-c m m") 'mu4e)

(setq mu4e-view-prefer-html nil)
;; Make sure mu4e shows the plain text 
(setq mu4e-view-html-plaintext-ratio-heuristic 10000)

(setq gnus-inhibit-images nil)
(setq mu4e-html2text-command 'mu4e-shr2text) 

(setq send-mail-function 'sendmail-send-it)
(setq sendmail-program "msmtp")
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq smtpmail-queue-mail nil)

(setq mail-user-agent 'message-user-agent)
(setq message-kill-buffer-on-exit t)

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-get-mail-command "mbsync -a")

(setq epg-gpg-program "gpg")
;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

(defun mu4e-configure-line-wrap ()
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq-default truncate-lines nil))

(add-hook 'mu4e-compose-mode-hook #'mu4e-configure-line-wrap)
(add-hook 'mu4e-view-mode-hook  #'mu4e-configure-line-wrap)
 
