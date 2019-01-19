(require 'htmlize)
(require 'org-bullets)

(defvar org-notes-dir "")

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-archive-location
      (concat org-notes-dir
              "archives/"
              (format-time-string "%Y")
              ".org::* "
              (format-time-string "%B")))

(defun org-mode-open-notes ()
  (interactive)
  (helm-find-files-1 (concat org-notes-dir "current/")))

(global-set-key (kbd "C-c n s") #'org-mode-open-notes)

(defun org-mode-after-save-hook ()
  (interactive)
  (org-store-agenda-views))

(setq org-bullets-bullet-list '("✸" "★" "•"))

(defun org-mode-setup-font-faces ()
  (dolist (face org-level-faces)
    (set-face-attribute face nil :background nil :weight 'semi-bold :height 1.0 :box nil)))

(defun org-mode-initialize ()
  (org-bullets-mode 1)
  (org-defkey org-mode-map [(meta shift up)] 'org-move-subtree-up)
  (org-defkey org-mode-map [(meta shift down)] 'org-move-subtree-down)
  (add-hook 'after-save-hook #'org-store-agenda-views nil t))

(add-hook 'org-mode-hook #'org-mode-initialize)
(add-hook 'org-mode-hook #'org-mode-setup-font-faces)

(setq org-todo-keywords
      '((sequence "TODO(t)" "FEEDBACK(f)" "IDEA(i)" "|"
                  "DONE(d)" "HOLD(h)" "STARRED(s)" "CANCELED(c)" "SCHEDULED(a)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "#FF0000" :weight bold)
        ("FEEBACK" :foreground "#FF7400" :weight bold)
        ("IDEA" . "#EE00FF")
        ("SCHEDULED" . "green")        
        ("STARRED" . "green")
        ("HOLD" . "cyan")))

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords
 'org-mode
 '(("^ +\\([-*]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)))

(setq org-agenda-span 21)

(setq org-agenda-custom-commands
      `(("X" agenda "" nil ,(concat org-notes-dir "agenda.html"))
        ("Y" alltodo "" nil ,(concat org-notes-dir "todo.html"))))

