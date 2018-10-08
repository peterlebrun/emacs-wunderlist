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
