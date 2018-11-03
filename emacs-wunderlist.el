;; (defvar wl-token "")
;; (defvar wl-client-id "")
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
        (print json-data)
      (print "NO DICE FAM"))))

(defun wl-process-response (response)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
     (when (search-forward "\n\n" nil t)
       (json-read))))

(defun wl-get-tasks-for-list (list-id)
  (wl-url-retrieve (wl-url-get-tasks-for-list list-id) "GET"))

(wl-get-tasks-for-list wl-sample-list-id)

       ;(let ((result (json-read)))
       ;  result)))))
       ;(json-read)))))
       ;(let ((json-object-type 'plist)
       ;      (json-key-type 'symbol)
       ;      (json-array-type 'vector))
       ;  (json-read))))))
         ;(let ((result (json-read)))
           ;;(print (plist-get (elt result 0) 'title))))))))
           ;(print (elt result 0))))))))

;(defun wl-get-lists () (wl-url-retrieve wl-url-get-lists "GET"))
;
;(defun wl-prepare-display-buffer ()
;  (let ((buf (get-buffer-create wl-task-buffer-name)))
;    (with-current-buffer buf
;      (setq buffer-read-only nil)
;      (erase-buffer)
;      (kill-all-local-variables)
;      (insert (wl-get-lists))
;      (setq buffer-read-only t))
;    buf))
;
;(wl-prepare-display-buffer)
;
;;; GOOD STUFF ABOVE
;;;
;;; STUDY MATERIALS BELOW
;(defun pbl--wl-get-lists ()
;  (let ((url-request-method "GET")
;        (url-request-extra-headers pbl--wl-headers))
;    (url-retrieve
;     "https://a.wunderlist.com/api/v1/lists"
;     (lambda (events)
;       (goto-char url-http-end-of-headers)
;       (let ((json-object-type 'plist)
;             (json-key-type 'symbol)
;             (json-array-type 'vector))
;         (let ((result (json-read)))
;           ;;(print (plist-get (elt result 0) 'title))))))))
;           (print (elt result 0))))))))
;
;;;(let ((result (json-read)))
;;;(print (plist-get (elt result 0) 'title))))))))
;;; result))))))
;(defun pbl--wl-get-tasks ()
;  (let ((url-request-method "GET")
;        (url-request-extra-headers pbl--wl-headers))
;    (url-retrieve
;     (concat "https://a.wunderlist.com/api/v1/tasks?" (url-build-query-string '(("list_id" "88434790"))))
;     (lambda (events)
;       (goto-char url-http-end-of-headers)
;       (let ((json-object-type 'plist)
;             (json-key-type 'symbol)
;             (json-array-type 'vector))
;         (json-read))))))
;
;(pbl--wl-get-lists)
;(print (pbl--wl-get-tasks))
;(print (pbl--wl-get-lists))
;
;;; wunderlist API documentation: https://developer.wunderlist.com/documentation
;
;(require 'url)
;(defun extract-response ()
;  "Extract the JSON response from the buffer returned by url-http."
;  (set-buffer-multibyte t)
;  (progn
;    (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
;        (when (search-forward "\n\n" nil t)
;          (prog1 (json-read)
;            (url-store-in-cache (current-buffer))
;            (kill-buffer)
;            ))
;      (kill-buffer))))
;
;(defun pbl--data-retrieved (status)
;  (let ((pbl--data (extract-response)))
;    (if pbl--data
;        (print pbl--data))))
;
;(url-retrieve "https://jsonplaceholder.typicode.com/todos/" 'pbl--data-retrieved)
;
;;;(switch-to-buffer (current-buffer)))
;
;;; (url-retrieve "https://jsonplaceholder.typicode.com/todos/" (lambda (status) (switch-to-buffer (current-buffer))))
;;; below stolen from @aaronbieber to study
;;(defun extract-response ()
;;  "Extract the JSON response from the buffer returned by url-http."
;;  (set-buffer-multibyte t)
;;  (progn
;;    (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
;;        (when (search-forward "\n\n" nil t)
;;          (prog1 (json-read)
;;            (url-store-in-cache (current-buffer))
;;            (kill-buffer)
;;            ))
;;(kill-buffer))))
;
;(require 'url)
;(require 'json')
;(defun extract-response ()
;  "Extract the JSON response from the buffer returned by url-http."
;  (set-buffer-multibyte t)
;  (progn
;    (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
;        (when (search-forward "\n\n" nil t)
;          (prog1 (json-read)
;            (kill-buffer)
;            ))
;      (kill-buffer))))
;
;(defun pbl--data-retrieved (status)
;  (let ((pbl--data (extract-response)))
;    (if pbl--data
;        (let ((foo1 (assoc 'title (car pbl--data))))
;          (print foo1)))))
;
;(url-retrieve "https://jsonplaceholder.typicode.com/todos/" 'pbl--data-retrieved)
;
;(require 'json)
;(with-temp-buffer
;  (url-insert-file-contents
;   "https://api.stackexchange.com/2.2/questions/12464?site=emacs"
;   (let ((json-false :false))
;     (json-read))))
