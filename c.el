(require 'xcscope)

(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-hook 'cscope-minor-mode)

