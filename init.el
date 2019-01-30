(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Switch window using ctrl-super-arrows
(global-set-key (kbd "C-s-<up>") 'windmove-up)
(global-set-key (kbd "C-s-<down>") 'windmove-down)
(global-set-key (kbd "C-s-<left>") 'windmove-left)
(global-set-key (kbd "C-s-<right>") 'windmove-right)

(global-set-key (kbd "C-x c c") 'comment-region)
(global-set-key (kbd "C-x c u") 'uncomment-region)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c M") 'man)

(fset 'yes-or-no-p 'y-or-n-p)

;; General UI settings
(blink-cursor-mode -1)
(global-linum-mode)
(size-indication-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)
(show-paren-mode 1)

;; General text editing settings
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(delete-selection-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(require 'material-theme)

;; Highlight current line
(global-hl-line-mode +1)

(require 'which-key)
(which-key-mode +1)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'helm)
(require 'helm-files)
(require 'helm-buffers)
(require 'helm-command)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'dash)
(eval-after-load 'dash '(dash-enable-font-lock))

;; dW vi equivalent
(defun kill-to-ws ()
  (interactive)
  (kill-region (point) (save-excursion (skip-syntax-forward "^ ") (point))))

(global-set-key (kbd "M-D") 'kill-to-ws)

(defun shell-command-to-string-stdout (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (process-file shell-file-name nil '(t nil) nil shell-command-switch command))))

(defun add-to-path (path)
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (setq exec-path (append exec-path (list path))))

(defun kill-all-buffers ()
  (interactive)
  (seq-each #'kill-buffer (-map #'buffer-name (buffer-list))))

(defun switch-to-new-temp-buffer ()
  (interactive)
  (let ((now (format-time-string "%F %T" (current-time))))
    (switch-to-buffer (generate-new-buffer-name (concat "*temp " now "*")))))

(global-set-key (kbd "C-c b b") 'switch-to-new-temp-buffer)

(defun join-lines-in-region ()
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (dotimes (i (count-lines (region-beginning) (region-end)))
      (goto-char (point-at-bol))
      (delete-char -1))))

(global-set-key (kbd "C-c C-j C-l") 'join-lines-in-region)

(defun byte-recompile-everything ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(require 's)

(defun git-get-root-or-nil ()
  (ignore-errors
    (let ((path (s-chomp (shell-command-to-string-stdout "git rev-parse --show-toplevel"))))
      (if (s-blank? path)
          nil
        path))))

(defun helm-find-files-in-git-repo ()
  (interactive)
  (let ((target-path (git-get-root-or-nil)))
    (if (not target-path)
        (message "not in a git repository")
      (helm
       :sources
       (helm-build-in-buffer-source "All files"
         :init
         (lambda ()
           (with-current-buffer (helm-candidate-buffer 'global)
             (process-file "git"
                           nil
                           (buffer-name (current-buffer))
                           nil
                           "ls-files"
                           "-co"
                           "--exclude-standard"
                           "--full-name"
                           target-path)
             (goto-char (point-min))))
         :action
         (helm-make-actions
          "Open file"
          (lambda (x)
            (find-file (concat target-path "/" x))))
         :get-line #'buffer-substring
         :filtered-candidate-transformer 'helm-ff-sort-candidates)
       :buffer "*helm all files*"))))

(global-set-key (kbd "C-c p f") 'helm-find-files-in-git-repo)

(defun grep-in-git-repo (pattern)
  (interactive
   (list (read-string "Pattern: ")))
  (let ((target-path (git-get-root-or-nil)))
    (if (not target-path)
        (message "not in a git repository")
      (grep (concat "git ls-files -co --exclude-standard --full-name"
                    " " target-path " "
                    "| xargs -n 1 -I {} grep --color -nH -e "
                    pattern " " target-path "/{}")))))

(global-set-key (kbd "C-c p g") 'grep-in-git-repo)

(load "~/.emacs.d/org-mode.el")
(load "~/.emacs.d/emacs-lisp.el")
(load "~/.emacs.d/chicken-scheme.el")
(load "~/.emacs.d/c.el")
(load "~/.emacs.d/http.el")
(load "~/.emacs.d/shell.el")
(load "~/.emacs.d/javascript.el")
(load "~/.emacs.d/mail.el")
