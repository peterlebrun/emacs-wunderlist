(defvar pbl--wl-headers
  '(("X-Access-Token" . "")
    ("X-Client-Id" . "")))

(defun pbl--wl-get-lists ()
  (let ((url-request-method "GET")
        (url-request-extra-headers pbl--wl-headers))
    (url-retrieve
     "https://a.wunderlist.com/api/v1/lists"
     (lambda (events)
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (let ((result (json-read)))
           ;;(print (plist-get (elt result 0) 'title))))))))
           (print (elt result 0))))))))

;;(let ((result (json-read)))
;;(print (plist-get (elt result 0) 'title))))))))
;; result))))))
(defun pbl--wl-get-tasks ()
  (let ((url-request-method "GET")
        (url-request-extra-headers pbl--wl-headers))
    (url-retrieve
     (concat "https://a.wunderlist.com/api/v1/tasks?" (url-build-query-string '(("list_id" "88434790"))))
     (lambda (events)
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (json-read))))))

(pbl--wl-get-tasks)
(print (pbl--wl-get-tasks))
(print (pbl--wl-get-lists))

;; wunderlist API documentation: https://developer.wunderlist.com/documentation

(require 'url)
(defun extract-response ()
  "Extract the JSON response from the buffer returned by url-http."
  (set-buffer-multibyte t)
  (progn
    (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
        (when (search-forward "\n\n" nil t)
          (prog1 (json-read)
            (url-store-in-cache (current-buffer))
            (kill-buffer)
            ))
      (kill-buffer))))

(defun pbl--data-retrieved (status)
  (let ((pbl--data (extract-response)))
    (if pbl--data
        (print pbl--data))))

(url-retrieve "https://jsonplaceholder.typicode.com/todos/" 'pbl--data-retrieved)

;;(switch-to-buffer (current-buffer)))

;; (url-retrieve "https://jsonplaceholder.typicode.com/todos/" (lambda (status) (switch-to-buffer (current-buffer))))
;; below stolen from @aaronbieber to study
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
;(kill-buffer))))

(require 'url)
(require 'json')
(defun extract-response ()
  "Extract the JSON response from the buffer returned by url-http."
  (set-buffer-multibyte t)
  (progn
    (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
        (when (search-forward "\n\n" nil t)
          (prog1 (json-read)
            (kill-buffer)
            ))
      (kill-buffer))))

(defun pbl--data-retrieved (status)
  (let ((pbl--data (extract-response)))
    (if pbl--data
        (let ((foo1 (assoc 'title (car pbl--data))))
          (print foo1)))))

(url-retrieve "https://jsonplaceholder.typicode.com/todos/" 'pbl--data-retrieved)

(require 'json)
(with-temp-buffer
  (url-insert-file-contents
   "https://api.stackexchange.com/2.2/questions/12464?site=emacs"
   (let ((json-false :false))
     (json-read))))
