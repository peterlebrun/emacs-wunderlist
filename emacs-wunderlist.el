;; TODO:
;; 1: DONE
;; Create major mode DONE
;; set major mode in the buffer I create DONE
;; Open that DONE
;; allow "q" to close buffer/window DONE
;;
;; 2:
;; Pivot and format data to display list of tasks DONE
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

;; Load auth info during development
(load-file "./setup.el")

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

(defun ewl--get-auth-headers ()
  "A nice function to return a list of auth headers."
  `(("X-Access-Token" . ,ewl-access-token)
    ("X-Client-Id" . ,ewl-client-id)))

(defvar ewl-url-get-lists "https://a.wunderlist.com/api/v1/lists")
(defun ewl-url-get-tasks-for-list (ewl-list-id)
  (concat "https://a.wunderlist.com/api/v1/tasks?"
          (url-build-query-string `((list_id ,ewl-list-id)))))

(defun ewl-url-retrieve (url method)
  (let ((url-request-method method)
        (url-request-extra-headers (ewl--get-auth-headers)))
    (url-retrieve url 'ewl-display-response)))

(defun ewl-display-response (response)
  (let ((json-data (ewl-process-response response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
          ;(insert (ewl-prepare-tasks-for-display json-data))
          (ewl-display-tasks (ewl-prepare-tasks-for-display json-data))
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

(defun ewl-prepare-tasks-for-display (task-list)
  "Pivot data into display format"
  (mapcar (lambda(val)
            (list
              (plist-get val 'id)
              (plist-get val 'title)))
          task-list))

(defun ewl-display-tasks (task-list)
  "Foobarf"
  (setq buffer-read-only nil)
  (while task-list
    (let* ((element (car task-list))
           (id (car element))
           (title (cadr element)))
      (insert (concat (number-to-string id) "\t" title "\n")))
    (setq task-list (cdr task-list)))
  (setq buffer-read-only t))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
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

;; ewl-sample-list-id comes from setup.el
(ewl-get-tasks-for-list ewl-sample-list-id)
