;; TODO:
;; 1:
;; Create major mode DONE
;; set major mode in the buffer I create DONE
;; Open that DONE
;; allow "q" to close buffer/window NOT WORKING
;;
;; 2:
;; Pivot and format data to display list of tasks
;;
;; 3:
;; Propertize data so that task contains task ID and list ID
;;
;; 4
;; Read from buffer to create new task for list
;;
;; 5a:
;; Mark task done (API Call)
;; 5b:
;; Mark task done (bind to key)
;;
;; 6:
;; Delete task
;;
;; 7:
;; Move task to new list
;;
;; 8:
;; Cache responses (when appropriate) to reduce HTTP calls

;; This won't make it into the final build
;; Just making my own life easier by handling auth info separately
(load-file "./auth.el")

;; Just for testing
(defvar ewl-sample-list-id "371687651") ;;emacs-wunderlist-test

(defgroup ewl nil
  "A simple plugin to manage your wunderlist via emacs"
  :group 'tools)

(defcustom ewl-access-token ""
  "Wunderline access token for API requests."
  :group 'ewl
  :type 'string)

(defcustom ewl-client-id ""
  "Wunderline client id for API requests."
  :group 'ewl
  :type 'string)

(defcustom ewl-task-buffer-name "*ewl-task-buffer*"
  "Name for the emacs wunderlist buffer."
  :group 'ewl
  :type 'string)

(defvar ewl-auth-headers
  `(("X-Access-Token" . ,ewl-access-token)
    ("X-Client-Id" . ,ewl-client-id)))

(defvar ewl-url-get-lists "https://a.wunderlist.com/api/v1/lists")
(defun ewl-url-get-tasks-for-list (ewl-list-id)
  (concat "https://a.wunderlist.com/api/v1/tasks?"
          (url-build-query-string `((list_id ,ewl-list-id)))))

(defun ewl-url-retrieve (url method)
  (let ((url-request-method method)
        (url-request-extra-headers ewl-auth-headers))
    (url-retrieve url 'ewl-display-response)))

(defun ewl-display-response (response)
  (let ((json-data (ewl-process-response response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
          (erase-buffer)
          ;; (insert (plist-get (elt json-data 0) 'title))
          (insert (plist-get (elt json-data 0) 'title))
          ;; (insert json-data)
          ;; (insert "FOO")
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (print "NO DICE FAM"))))

(defun ewl-process-response (response)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 200 OK$" (line-end-position) t)
     (when (search-forward "\n\n" nil t)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (json-read)))))

(defun ewl-get-tasks-for-list (list-id)
  (ewl-url-retrieve (ewl-url-get-tasks-for-list list-id) "GET"))

(defun ewl-prepare-display-buffer ()
  (let ((buf (get-buffer-create ewl-task-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (ewl-mode)
      (setq buffer-read-only t))
    buf))

(ewl-get-tasks-for-list ewl-sample-list-id)

;; NOTE TO SELF: Evil Mode will override this, somehow need to
;; turn off evil mode when buffer is prepared?
(defvar ewl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" (lambda() (interactive) (quit-window t (selected-window))))
    map)
  "Get the keymap for the ewl window")

(define-derived-mode ewl-mode nil "EWL"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-mode':
\\{ewl-mode-map}"
  (setq truncate-lines t))
