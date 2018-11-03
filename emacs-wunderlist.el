(defvar wl-sample-list-id "371687651") ;;emacs-wunderlist-test

(defvar wl-auth-headers
  '(("X-Access-Token" . "")
    ("X-Client-Id" . "")))

(defvar wl-task-buffer-name "*wl-task-buffer*")
(defvar wl-url-get-lists "https://a.wunderlist.com/api/v1/lists")
(defun wl-url-get-tasks-for-list (wl-list-id)
  (concat "https://a.wunderlist.com/api/v1/tasks?"
          (url-build-query-string `((list_id ,wl-list-id)))))

(defun wl-url-retrieve (url method)
  (let ((url-request-method method)
        (url-request-extra-headers wl-auth-headers))
    (url-retrieve url 'wl-display-response)))

(defun wl-display-response (response)
  (let ((json-data (wl-process-response response)))
    (if json-data
        (with-current-buffer (wl-prepare-display-buffer)
          (setq buffer-read-only nil)
          (erase-buffer)
          ;; (insert (plist-get (elt json-data 0) 'title))
          (insert (plist-get (elt json-data 0) 'title))
          ;; (insert json-data)
          ;; (insert "FOO")
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (print "NO DICE FAM"))))

(defun wl-process-response (response)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
     (when (search-forward "\n\n" nil t)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (json-read)))))

(defun wl-get-tasks-for-list (list-id)
  (wl-url-retrieve (wl-url-get-tasks-for-list list-id) "GET"))

(defun wl-prepare-display-buffer ()
  (let ((buf (get-buffer-create wl-task-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (setq buffer-read-only t))
    buf))

(wl-get-tasks-for-list wl-sample-list-id)
