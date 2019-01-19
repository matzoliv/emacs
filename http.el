(require 'urlenc)
(require 'dash)
(require 'restclient)
(require 'sgml-mode)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(global-set-key (kbd "C-c b h") (lambda () (interactive) (new-http-scratch nil)))
(global-set-key (kbd "C-c b H") (lambda () (interactive) (new-http-scratch t)))

(defun restclient-prettify-json-unicode ()
  (json-pretty-print-buffer))

(defun base64-to-base64url (str)
  (setq str (replace-regexp-in-string "=+$" "" str))
  (setq str (replace-regexp-in-string "+" "-" str))
  (setq str (replace-regexp-in-string "/" "_" str)))

(defun base64url-to-base64 (str)
  (setq str (replace-regexp-in-string "-" "+" str))
  (setq str (replace-regexp-in-string "_" "/" str))
  (let ((mod (% (length str) 4)))
    (cond 
     ((= mod 1) (concat str "==="))
     ((= mod 2) (concat str "=="))
     ((= mod 3) (concat str "="))
     (t str))))

(defun base64url-encode-string (str)
  (base64-to-base64url (base64-encode-string str t)))

(defun base64url-decode-string (str)
  (base64-decode-string (base64url-to-base64 str)))

(defun jwt-decode-region ()
  (interactive)
  (let* ((jwt-str (buffer-substring-no-properties (region-beginning) (region-end)))
         (parts (split-string jwt-str "\\.")))
    (delete-region (region-beginning) (region-end))
    (insert (json-pp-string (base64url-decode-string (nth 0 parts))) "\n")

    (let ((payload (json-read-from-string (base64url-decode-string (nth 1 parts)))))
      (setf (alist-get 'exp payload)
            (format-time-string "%Y-%m-%dT%T" (seconds-to-time (alist-get 'exp payload))))
      (setf (alist-get 'iat payload)
            (format-time-string "%Y-%m-%dT%T" (seconds-to-time (alist-get 'iat payload))))
      (insert
       (let ((json-encoding-pretty-print t)
             (json-object-type 'alist))
         (json-encode-alist payload))
       "\n"))
    
    (if (> (length parts) 2)
        (insert (nth 2 parts) "\n"))))

(defun json-pp-string (s)
  (let ((json-encoding-pretty-print t)
        (json-object-type 'alist))
    (json-encode-alist (json-read-from-string s))))

(defun json-stringp (s)
  (ignore-errors (progn (json-read-from-string s) t)))

(defun convert-form-to-json (form-string)
  (let ((json-encoding-pretty-print t)
        (json-object-type 'alist))
    (json-encode-alist
     (mapcar (lambda (kvp)
               (let* ((chunks (split-string kvp "="))
                      (k (car chunks))
                      (v-parts (cdr chunks)))
                 (let ((v-as-str
                        (urlenc:decode-string (apply #'concat v-parts) 'utf-8)))
                   (let ((maybe-json
                          (ignore-errors (json-read-from-string v-as-str))))
                     (cons
                      k
                      (if maybe-json
                          (cond
                           ((and (numberp maybe-json) (= maybe-json 1.0e+INF)) v-as-str)
                           ((and (numberp maybe-json) (= maybe-json -1.0e+INF)) v-as-str)
                           (t maybe-json))
                        v-as-str))))))
             (split-string form-string "&")))))

(defun parse-curl-command-line-from-kill-ring ()  
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (search-forward-regexp "^curl '\\([^']+\\)'")
    (setq uri (match-string 1))
    (setq headers (make-hash-table :test 'equal))
    (goto-char (point-min))
    (setq
     method
     (if (search-forward-regexp "-X \"?\\([a-zA-Z]+\\)\"?" nil t)
         (upcase (match-string 1))
       "GET"))
    (goto-char (point-min))
    (while (search-forward-regexp "-H '\\([^:]+\\): \\([^']+\\)'" nil t)
      (puthash (match-string 1) (match-string 2) headers))
    (goto-char (point-min))    
    (if (search-forward-regexp "\\(--data\\|-d\\|--data-binary\\) $?'\\(.*\\)'" nil t)
        (setq body (match-string 2))
      (setq body nil)) 
    `((uri . ,uri)
      (method . ,method)
      (headers . ,headers)
      (body . ,body))))

(defun new-http-scratch (do-convert-form-to-json)
  (interactive)
  (let* ((target-buf (generate-new-buffer-name "*temp-http*"))
         (request (parse-curl-command-line-from-kill-ring))
         (body (let ((original-body (alist-get 'body request)))
                 (if original-body
                     (if (json-stringp original-body)
                         (json-pp-string original-body)
                       (if do-convert-form-to-json
                           (convert-form-to-json original-body)
                         original-body))
                   original-body))))
    (switch-to-buffer target-buf)
    (insert (alist-get 'method request nil) " "
            (alist-get 'uri request) "\n")
    (let ((headers (alist-get 'headers request)))
      (dolist (hdr '("Authorization" "Cookie" "Accept" "Origin" "Accept-Encoding" "Accept-Language"))
        (let ((hdr-val (gethash hdr headers)))
          (if hdr-val
              (insert hdr ": " hdr-val "\n")))))
    (insert "Content-Type: application/json" "\n\n")
    (if body
        (insert body)))
  (restclient-mode))

(global-set-key (kbd "C-c b x") 'new-xml-scratch)

(defun new-xml-scratch ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*temp-xml*"))
  (xml-mode)
  (yank)
  (sgml-pretty-print (point-min) (point-max)))

(defun xml-decode-region ()
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert
   (with-temp-buffer
     (save-excursion (yank))
     (xml-parse-string))))
