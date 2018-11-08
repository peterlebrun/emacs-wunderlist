;; NOTE TO SELF: You left off trying to format
;; a JSON payload to post to create a new task
;;
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
;; Propertize data so that task contains task ID and list ID DONE
;;
;; NOTE TO SELF: Both 4 & 5 will require creating
;; 4:
;; Read from buffer to create new task for list
;;
;; 5a:
;; Mark task done (API Call)
;; 5b:
;; Mark task done (bind to key)
;;
;; 6:
;; Delete task ;; I've got this request forming but HTTP request isn't working :/
;;
;; 7:
;; Move task to new list
;;
;; 8:
;; Cache responses (when appropriate) to reduce HTTP calls
;;
;; 9:
;; Add README.me to the repo

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

(defvar ewl-url-get-folders "https://a.wunderlist.com/api/v1/folders")
(defvar ewl-url-get-lists "https://a.wunderlist.com/api/v1/lists")

(defun ewl--get-url-specific-task (ewl-task-id)
  (concat "https://a.wunderlist.com/api/v1/tasks/"
          (number-to-string ewl-task-id)))

(defun ewl--get-url-tasks-for-list (ewl-list-id)
  (concat "https://a.wunderlist.com/api/v1/tasks?"
          (url-build-query-string `((list_id ,ewl-list-id)))))

(defun ewl-url-retrieve (url method cb)
  ""
  (let ((url-request-method method)
        (url-request-extra-headers (ewl--get-auth-headers)))
    (url-retrieve url cb)))

(defun ewl-url-retrieve-post (url data cb)
  ""
  (let ((url-request-data data)) (ewl-url-retrieve url "POST" cb)))

(defun ewl-display-response (response)
  (let ((json-data (ewl-process-response response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
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
  (ewl-url-retrieve
   (ewl--get-url-tasks-for-list list-id)
   "GET"
   'ewl-display-response))

(defun ewl-get-folders ()
  "Retrieve all lists"
  (ewl-url-retrieve ewl-url-get-folders "GET"))

(defun ewl-get-lists ()
  "Retrieve all lists"
  (ewl-url-retrieve ewl-url-get-lists "GET" 'ewl-display-response))

;; This does not work properly
(defun ewl-delete-task (ewl-task-id)
  "Delete a task"
  (ewl-url-retrieve (ewl--get-url-specific-task ewl-task-id) "DELETE"))

;; This seems to work
(defun ewl-get-task (ewl-task-id)
  "Delete a task"
  (ewl-url-retrieve (ewl--get-url-specific-task ewl-task-id) "GET"))

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
  (mapcar (lambda(task-data)
            (ewl-parse-task task-data))
          task-list))

;; @TODO: Make it so that ewl-get-task
;; passes off to this single task
(defun ewl-parse-task (task-data)
  "Get relevant data for a specific task."
  (let ((id (plist-get task-data 'id))
        (title (plist-get task-data 'title)))
    (propertize title 'id id)))

(defun ewl-display-tasks (task-list)
  "Foobarf"
  (setq buffer-read-only nil)
  (while task-list
    (let* ((title (car task-list)))
      (insert (concat title "\n")))
    (setq task-list (cdr task-list)))
  (setq buffer-read-only t))

(defun ewl--get-mode-map ()
  "Turn this into a function so it can refresh for dev purposes"
  (let ((map (make-sparse-keymap)))
    (define-key map "q"
      (lambda() (interactive) (quit-window t (selected-window))))
    (define-key map "\r"
      (lambda() (interactive) (ewl--get-id-from-thing-at-point)))
    map))

(defun ewl--get-id-from-thing-at-point ()
  "Get id text property of thing at point."
  (let* ((text-string (thing-at-point 'word))
         (id (get-text-property 1 'id text-string)))
    (ewl-get-tasks-for-list id)))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
(defvar ewl-mode-map (ewl--get-mode-map) "Get the keymap for the ewl window")

(define-derived-mode ewl-mode nil "EWL"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-mode':
\\{ewl-mode-map}"
  (setq truncate-lines t))

;; ewl-sample-list-id comes from setup.el
;; (ewl-get-tasks-for-list ewl-sample-list-id)
;; (ewl-get-folders)
;; (ewl-get-task 4372769545)
;; (ewl-get-lists)

(defun ewl-create-task ()
  ""
  ;; (print (concat "body="))); (json-encode-string "{'list_id': 371687651, 'title': 'foo bar baz'}"))))
  (ewl-url-retrieve-post
   "https://a.wunderlist.com/api/v1/tasks/"
   (json-encode '(("list_id" . 371687651) ("title" . "foo bar baz")))
   'ewl-display-response))

(ewl-create-task)
;(print (concat "body="))
