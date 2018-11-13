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
;; Mark task done (API Call) DONE
;; 5b:
;; Mark task done (bind to key)
;;
;; 6:
;; Delete task ;; I've got this request forming but HTTP request isn't working :/
;;
;; 7:
;; Move task to new list (Extend method `ewl-mark-task-complete` from 5a for this)
;;
;; 8:
;; Cache responses (when appropriate) to reduce HTTP calls
;;
;; 9:
;; Add README.me to the repo DONE
;;
;; 10:
;; Alter JSON response parsing to handle other-than-200 requests
;;
;; Optimal flow: command opens buffer of lists.  Select a list, and see the
;; tasks from that list.  Have a variety of keybindings available to move,
;; complete, prioritize, delete

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
    ("X-Client-Id" . ,ewl-client-id)
    ("Content-Type" . "application/json")))

(defvar ewl-url-base-api "https://a.wunderlist.com/api/v1/")
(defvar ewl-url-folders (concat ewl-url-base-api "folders"))
(defvar ewl-url-lists (concat ewl-url-base-api "lists"))
(defvar ewl-url-tasks (concat ewl-url-base-api "tasks"))

(defun ewl--url-specific-task (task-id)
  (concat ewl-url-tasks "/" (number-to-string task-id)))

(defun ewl--url-tasks-for-list (list-id)
  (concat ewl-url-tasks "?" (url-build-query-string `((list_id ,list-id)))))

(defun ewl-url-retrieve (url cb &optional method data)
  ""
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers (ewl--get-auth-headers))
        (url-request-data data))
    (url-retrieve url cb)))

(defun ewl-display-response (response)
  (debug response)
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
   'ewl-display-response))

(defun ewl-get-folders ()
  "Retrieve all lists"
  (ewl-url-retrieve ewl-url-folders 'ewl-display-response))

(defun ewl-get-lists ()
  "Retrieve all lists"
  (ewl-url-retrieve ewl-url-lists 'ewl-display-response))

;; This does not work properly
(defun ewl-delete-task (task-id)
  "Delete a task"
  (ewl-url-retrieve (ewl--url-specific-task task-id) "DELETE"))

;; This seems to work
(defun ewl-get-task (task-id)
  "Delete a task"
  (ewl-url-retrieve (ewl--url-specific-task task-id)))

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
;(ewl-get-lists)

(defun ewl-create-task (list-id task-title)
  ""
  (ewl-url-retrieve
   ewl-url-tasks
   'ewl-display-response
   "POST"
   (json-encode `((list_id . ,list-id) (title . ,task-title)))))

;(ewl-create-task)

(defun ewl-get-single-task (task-id)
  "Return plist of data representing task specified by TASK-ID."
  (ewl-url-retrieve (ewl-url-specific-task task-id) 'ewl-process-response))

(defun ewl-mark-task-complete (task-id)
  "Mark a task complete"
  (ewl-url-retrieve
   (ewl--url-specific-task task-id)
   (lambda(response)
     (let* ((task-data (ewl-process-response response))
            (task-revision (plist-get task-data 'revision))
            (task-id (plist-get task-data 'id))
            (url (ewl--url-specific-task task-id))
            (data `((revision . ,task-revision)
                    (completed . ,t))))
       (ewl-url-retrieve url 'ewl-display-response "PATCH" (json-encode data))))))

(defun ewl-update-task (task-id)
  "Update task by HTTP patch-ing data payload"
  (defun inner-method (response)
    (let* ((task-data (ewl-process-response response))
           (task-revision (plist-get task-data 'revision))
           (task-id (plist-get task-data 'id))
           (url (ewl--url-specific-task task-id))
           (data `((revision . ,task-revision)
                   (completed . ,t))))
      (ewl-url-retrieve url 'ewl-display-response "PATCH" (json-encode data))))

  (ewl-url-retrieve
   (ewl--url-specific-task task-id)
   'inner-method))

;(ewl-update-task 4372770057);  "brand new title")
(ewl-update-task 4372769545)
