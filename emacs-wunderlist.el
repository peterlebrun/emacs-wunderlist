;; @TODO: Handle auth info properly
;; @TODO: If you have the buffer open, and Inbox being shown, and you add a task, it doesn't update the buffer
;; @TODO: Add note for task
;; @TODO: Edit note for task
;; @TODO: View note for task
;; @TODO: using org-read-date opens calendar buffer on top of screen, move to bottom
;; @TODO: Get buffers to live-refresh

;; @DONE: Create major mode
;; @DONE: Set major mode in the buffer I create
;; @DONE: allow "q" to close buffer/window
;; @DONE: Pivot and format data to display list of tasks
;; @DONE: Propertize data so that task contains task ID and list ID
;; @DONE: Read from minibuffer to create new task for list
;; @DONE: Mark task done (API Call)
;; @DONE: Mark task done (bind to key)
;; @DONE: Delete task
;; @DONE: Move task to new list
;; @DONE: Add README.me to the repo
;; @DONE: Alter JSON response parsing to handle other-than-200 requests (started)
;; @DONE: Rewrite callbacks to take CB args and eliminate lexical binding
;; @DONE: Display list name as buffer header when displaying lists
;; @DONE: Get this to autoload when I start emacs
;; @DONE: Edit task title

;; @DISMISS: Cache responses (when appropriate) to reduce HTTP calls

;; Load auth info during development
(load-file (concat (file-name-directory load-file-name) "setup.el"))

(defgroup ewl nil
  "A simple plugin to manage your wunderlist via emacs"
  :group 'tools)

(defcustom ewl-access-token ""
  "Wunderlist access token for API requests."
  :group 'ewl
  :type 'string)

(defcustom ewl-client-id ""
  "Wunderlist client id for API requests."
  :group 'ewl
  :type 'string)

(defcustom ewl-list-id-inbox nil
  "ID for inbox list"
  :group 'ewl
  :type 'integer)

(defcustom ewl-list-id-priorities nil
  "ID for priorities list"
  :group 'ewl
  :type 'integer)

(defcustom ewl-list-id-backlog nil
  "ID for backlog list"
  :group 'ewl
  :type 'integer)

(defcustom ewl-task-buffer-name "*ewl-buffer*"
  "Name for the emacs wunderlist buffer."
  :group 'ewl
  :type 'string)

(defcustom ewl-list-name-inbox "gtd-inbox"
  "Name given to the inbox list."
  :group 'ewl
  :type 'string)

(defcustom ewl-list-name-priorities "gtd-priorities"
  "Name given to the priorities list."
  :group 'ewl
  :type 'string)

(defcustom ewl-list-name-backlog "gtd-backlog"
  "Name given to the backlog list."
  :group 'ewl
  :type 'string)

(defun ewl-get-auth-headers ()
  "A nice function to return a list of auth headers."
  `(("X-Access-Token" . ,ewl-access-token)
    ("X-Client-Id" . ,ewl-client-id)
    ("Content-Type" . "application/json")))

(defvar ewl-url-base-api "https://a.wunderlist.com/api/v1/")
(defvar ewl-url-lists (concat ewl-url-base-api "lists"))
(defvar ewl-url-tasks (concat ewl-url-base-api "tasks"))

(defun ewl-url-specific-task (task-id)
  "Return API url for specific task"
  (concat ewl-url-tasks "/" (number-to-string task-id)))

(defun ewl-url-tasks-for-list (list-id)
  "Return API url to get tasks for a specific list"
  (concat ewl-url-tasks "?" (url-build-query-string `((list_id ,list-id)))))

(defun ewl-url-retrieve (url cb &optional cbargs method data)
  "Wrap url-retrieve for generic use"
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers (ewl-get-auth-headers))
        (url-request-data data))
    (url-retrieve url cb (or cbargs nil))))

(defun ewl-display-response (status buffer-name)
  "Parse and display response in window"
  (let ((json-data (ewl-process-response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
          (setq header-line-format buffer-name)
          (ewl-display-items (ewl-prepare-items-for-display json-data))
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (print "Error processing API request"))))

(defun ewl-process-response () ;;status &optional cb)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 20.*$" (line-end-position) t)
     ;; 204 means no content - trying to json-read 204 response will error
     ;;(if (string-match-p "^HTTP/.+ 204.*$" (thing-at-point 'line t))
           ;;(if (fboundp cb) (funcall cb))
       (when (search-forward "\n\n" nil t)
         (prog1
             (let ((json-object-type 'plist)
                   (json-key-type 'symbol)
                   (json-array-type 'vector))
               (json-read))))))
      ;;     (if (fboundp cb) (funcall cb)))))))

(defun ewl-display-tasks-for-list (list-id list-name)
  "Display response for all tasks in a particular list"
  (ewl-url-retrieve
   (ewl-url-tasks-for-list list-id)
   'ewl-display-response '(list-name)))

(defun ewl-prepare-display-buffer ()
  "Create consistent buffer object for displaying data"
  (let ((buf (get-buffer-create ewl-task-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (ewl-mode)
      (setq buffer-read-only t))
    buf))

(defun ewl-prepare-items-for-display (item-list)
  "Pivot data into display format"
  (mapcar (lambda(item-data)
            (ewl-parse-item item-data))
          item-list))

(defun ewl-parse-item (item-data)
  "Get relevant data for a specific task."
  (let ((id (plist-get item-data 'id))
        (title (plist-get item-data 'title))
        (type (if (plist-get item-data 'type) (plist-get item-data 'type)
                (if (plist-get item-data 'list_id) "task")))
        (list-id (plist-get item-data 'list_id)))
    (propertize title 'id id 'type type 'list-id list-id)))

(defun ewl-display-items (item-list)
  "Format item data for display in buffer"
  (setq buffer-read-only nil)
  (while item-list
    (let* ((title (car item-list)))
      (insert (concat title "\n")))
    (setq item-list (cdr item-list)))
  (setq buffer-read-only t))

(defun ewl-get-mode-map ()
  "Turn this into a function so it can refresh for dev purposes"
  (let ((map (make-sparse-keymap)))
    (define-key map "t"
      (lambda() (interactive) (ewl-add-task-to-inbox)))
    (define-key map "c"
      (lambda() (interactive) (ewl-update-task-at-point t)))
    (define-key map "r"
      (lambda() (interactive) (ewl-delete-task-at-point)))
    (define-key map "p"
      (lambda() (interactive) (ewl-update-task-at-point nil ewl-list-id-priorities)))
    (define-key map "e"
      (lambda() (interactive) (ewl-update-title-for-task-at-point)))
    (define-key map "b"
      (lambda() (interactive) (ewl-update-task-at-point nil ewl-list-id-backlog)))
    (define-key map "s"
      (lambda() (interactive) (ewl-update-due-date-for-task-at-point)))
    (define-key map "q"
      (lambda() (interactive) (quit-window t (selected-window))))
    (define-key map "x"
      (lambda() (interactive) (ewl-create-note-for-task-at-point)))
    (define-key map "di"
      (lambda() (interactive) (ewl-display-inbox)))
    (define-key map "db"
      (lambda() (interactive) (ewl-display-backlog)))
    (define-key map "dp"
      (lambda() (interactive) (ewl-display-priorities)))
    map))

(defun ewl-display-inbox ()
  "Syntactic sugar to display inbox list."
  (ewl-display-tasks-for-list ewl-list-id-inbox "INBOX"))

(defun ewl-display-backlog ()
  "Syntactic sugar to display backlog list."
  (ewl-display-tasks-for-list ewl-list-id-backlog "BACKLOG"))

(defun ewl-display-priorities ()
  "Syntactic sugar to display priorities list."
  (ewl-display-tasks-for-list ewl-list-id-priorities "PRIORITIES"))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
(defvar ewl-mode-map (ewl-get-mode-map)
  "Get the keymap for the ewl window")

(define-derived-mode ewl-mode nil "EWL"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-mode':
\\{ewl-mode-map}"
  (setq truncate-lines t))

(defun ewl-get-list-id-from-thing-at-point ()
  "Get list id text property of thing at point."
  (let* ((text-string (thing-at-point 'word))
         (type (get-text-property 1 'type text-string)))
    (if (equal "list" type)
        (get-text-property 1 'id text-string)
      (get-text-property 1 'list-id text-string))))

(defun ewl-create-task (task-title list-id cb)
  "Create task from given inputs"
  (ewl-url-retrieve
   ewl-url-tasks
   cb
   nil
   "POST"
   (json-encode `((list_id . ,list-id) (title . ,task-title)))))

;; @NOTE: This works as a callback, but issue is getting
;; id of list we want to refresh
(defun ewl-refresh-current-list ()
  "Determine what list we're currently in and refresh that data"
  (pop-to-buffer ewl-task-buffer-name)
  (goto-char (point-max))
  (backward-word) ;; Go to the last place we're certain to have a list-id
  (ewl-display-tasks-for-list (ewl-get-list-id-from-thing-at-point)))

(defun ewl-update-task (task-id &optional is-complete new-list-id due-date new-title)
  "Update task by HTTP patch-ing data payload"
  (ewl-url-retrieve
   (ewl-url-specific-task task-id)
   (lambda(status task-id &optional is-complete new-list-id due-date new-title)
     (let* ((response (ewl-process-response))
            (data `((revision . ,(plist-get response 'revision)))))

       (if is-complete (nconc data `((completed . ,t))))
       (if new-list-id (nconc data `((list_id . ,new-list-id))))
       (if due-date (nconc data `((due_date . ,due-date))))
       (if new-title (nconc data `((title . ,new-title))))

       (ewl-url-retrieve
        (ewl-url-specific-task task-id)
        'ewl-noop-process-response ;;-and-refresh-list
        nil
        "PATCH"
        (json-encode data))))
   `(,task-id
     ,(or is-complete nil)
     ,(or new-list-id nil)
     ,(or due-date nil)
     ,(or new-title nil))))

(defun ewl-update-due-date-for-task-at-point ()
  ""
  (let ((due-date (org-read-date nil nil nil "Select due date:")))
    (ewl-update-task-at-point nil nil due-date)))

(defun ewl-update-title-for-task-at-point ()
  ""
  (let ((new-title (read-from-minibuffer "Enter new title: ")))
    (ewl-update-task-at-point nil nil nil new-title)))

(defun ewl-ensure-list-ids ()
  "Ensure that necessary list IDs are populated"
  (if (or
       (not ewl-list-id-inbox)
       (not ewl-list-id-priorities)
       (not ewl-list-id-backlog))
      (ewl-url-retrieve ewl-url-lists 'ewl-load-list-ids)))

;; @TODO: If list IDs are null after this
;; We need to create the lists via API
(defun ewl-load-list-ids (status)
  "Parse RESPONSE of lists API to determine inbox id"
  (let ((lists-data (ewl-process-response))
        (found-list-id-inbox nil)
        (found-list-id-priorities nil)
        (found-list-id-backlog nil)
        (i 0))
    (while (and
            (<= i (length lists-data))
            (or
             (not found-list-id-inbox)
             (not found-list-id-priorities)
             (not found-list-id-backlog)))
      (let* ((list-data (elt lists-data i))
             (list-type (plist-get list-data 'list_type))
             (list-title (plist-get list-data 'title))
             (list-id (plist-get list-data 'id)))
        (when (equal list-title ewl-list-name-inbox)
          (setq ewl-list-id-inbox list-id)
          (setq found-list-id-inbox t))
        (when (equal list-title ewl-list-name-priorities)
          (setq ewl-list-id-priorities list-id)
          (setq found-list-id-priorities t))
        (when (equal list-title ewl-list-name-backlog)
          (setq ewl-list-id-backlog list-id)
          (setq found-list-id-backlog t))
        (setq i (+ 1 i))))))

;; Ideally bind to ,-t but this would be handled by the user's config
(defun ewl-add-task-to-inbox ()
  "Add new task to inbox."
  (ewl-ensure-list-ids)
  (ewl-create-task
   (read-from-minibuffer "Enter task: ")
   ewl-list-id-inbox
   'ewl-noop-process-response))

(defun ewl-noop-process-response (status)
  (ewl-process-response))

(defun ewl-update-task-at-point (&optional is-complete new-list-id due-date new-title)
  "Update task with relevant data."
  (let* ((text-string (thing-at-point 'word))
         (task-id (get-text-property 1 'id text-string)))
    (when is-complete
      (ewl-update-task task-id t))
    (when new-list-id
      (ewl-update-task task-id nil new-list-id))
    (when due-date
      (ewl-update-task task-id nil nil due-date))
    (when new-title
      (ewl-update-task task-id nil nil nil new-title))))

;; we need the current revision in order to delete
;; This can/should probably be refactored to simplify
(defun ewl-delete-task-at-point ()
  "Get task at point and pass data to delete operation"
  (let* ((text-string (thing-at-point 'word))
         (id (get-text-property 1 'id text-string))
         (type (get-text-property 1 'type text-string)))
    (if (equal "task" type)
        (ewl-delete-task id)
      (message "delete-task requires task"))))

(defun ewl-delete-task (task-id)
  "Delete task identified by TASK-ID"
  (ewl-url-retrieve
   (ewl-url-specific-task task-id)
   (lambda(status task-id)
     (let* ((task-data (ewl-process-response))
            (revision (plist-get task-data 'revision))
            (delete-url (concat
                         (ewl-url-specific-task task-id)
                         "?"
                         (url-build-query-string `((revision ,revision))))))
       (ewl-url-retrieve
        delete-url
        'ewl-noop-process-response ;;-and-refresh-list
        nil
        "DELETE")))
   (list task-id)))

;; NOTES
(defvar ewl-url-notes (concat ewl-url-base-api "notes"))
(defun ewl-url-notes-for-task (task-id)
  "Get list of notes associated with a particular task"
  (concat ewl-url-notes "?" (url-build-query-string `((task_id , task-id)))))

;; Returns a list with no more than 1 item
(defun ewl-get-note-for-task (task-id)
  "Get notes associated with a particular task"
  )

;; Returns single item
(defun ewl-get-note (note-id)
  "Get detail associated with a particular note"
  )

;; If a note already exists, we'll get a 422 response
;; & nothing new will be created
(defun ewl-create-note-for-task-at-point ()
  (let* ((text-string (thing-at-point 'word))
         (task-id (get-text-property 1 'id text-string))
         (content (read-from-minibuffer "Enter note: ")))
    (ewl-url-retrieve
     ewl-url-notes
     'ewl-noop-process-response
     nil
     "POST"
     (json-encode `((task_id . ,task-id) (content . ,content))))))

(defun ewl-update-note (note-id content)
  "Update note by HTTP patch-ing data payload"
  (ewl-url-retrieve
   (ewl-url-specific-note note-id)
   (lambda(status note-id content)
     (let* ((response (ewl-process-response))
            (data `((revision . ,(plist-get response 'revision))
                    (content . ,content)))

       (ewl-url-retrieve
        (ewl-url-specific-note note-id)
        'ewl-noop-process-response ;;-and-refresh-list
        nil
        "PATCH"
        (json-encode data))))
   `(,note-id ,content)))

  ;;;;(defun ewl-display-note (status)
    ;;;;"Display note for task"
    ;;;;(let ((note-data
;;
;;
    ;;;; (split-window-right)
    ;;;; (pop-to-buffer)

(ewl-ensure-list-ids)
