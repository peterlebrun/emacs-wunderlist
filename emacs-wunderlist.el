;; @TODO: Handle auth info properly
;; @TODO: using org-read-date opens calendar buffer on top of screen, move to bottom
;; @TODO: gtd-scheduled list (for scheduled items)
;; @TODO: Make tasks editable in place
;; @TODO: Highlight full line while moving through tasks
;; @TODO: Provide line breaks for anything that runs off the screen
;; @TODO: Add postman tests to repo
;; @TODO: Instead of making API call to get note, read "note" property

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
;; @DONE: Add note for task
;; @DONE: Edit note for task
;; @DONE: View note for task
;; @DONE: Gracefully handle case with no note
;; @DONE: Display if task is scheduled (with a little icon of some sort)
;; @DONE: Get revision info in text properties
;; @DONE: Make notes buffer editable
;; @DONE: Get buffers to live-refresh
;; @DONE: If you have the buffer open, and Inbox being shown, and you add a task, it doesn't update the buffer
;; @DONE: Save edits in notes buffer
;; @DONE: Display if task has a note (with a little icon of some sort)
;; @DONE: Parse HTTP Response code in ewl-process-response
;; @DONE: Handle 204s in ewl-process-response

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

(defcustom ewl-task-buffer-name "*ewl-tasks*"
  "Name for the emacs wunderlist task buffer."
  :group 'ewl
  :type 'string)

(defcustom ewl-notes-buffer-name "*ewl-notes*"
  "Name for the emacs wunderlist notes buffer."
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

(defun ewl-display-list-items (status list-name list-notes)
  "Parse and display list in window"
  (let ((json-data (ewl-process-response)))
    (if json-data
        (with-current-buffer (ewl-prepare-buffer ewl-task-buffer-name 'ewl-task-mode)
          (setq buffer-read-only nil)
          (setq header-line-format list-name)
          (ewl-display-items (ewl-parse-data json-data 'ewl-parse-item list-notes))
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (message "Error processing API request"))))

(defun ewl-display-note (status)
  "Display note in split window"
  (let ((json-data (ewl-process-response)))
    (if json-data
        (let ((note-data (ewl-parse-data json-data 'ewl-parse-note)))
          (if note-data
            (with-current-buffer (ewl-prepare-buffer ewl-notes-buffer-name 'ewl-notes-mode)
              (split-window-right)
              (other-window 2) ;; Concern from pedro: Will this always work?
              ;; @TODO: Why do I need this line here?
              (pop-to-buffer ewl-notes-buffer-name)
              (setq buffer-read-only nil)
              (setq header-line-format "Notes Buffer")
              (insert (car note-data))
              (goto-char (point-min))
              (setq buffer-read-only t))
            (message "No note for this task.")))
      (message "Error processing note request"))))

(defun ewl--display-note-alt (note)
  "Display NOTE."
  )

(defun ewl--display-note-for-task-at-point-alt ()
  "Display NOTE property of task at point."
  (let ((line (thing-at-point 'line))
        (note (get-text-property 1 'note line)))
  )
  )

(defun ewl-display-note-for-task-at-point ()
  "Display note for task under cursor"
  (let* ((text-string (thing-at-point 'line))
         (task-id (get-text-property 1 'id text-string)))
    (ewl-url-retrieve
     (ewl-url-notes-for-task task-id)
     'ewl-display-note)))

(defun ewl-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min))
  (cadr (split-string (thing-at-point 'line))))

(defun ewl-parse-json ()
  ""
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'vector))
    (json-read)))

(defun ewl-process-response ()
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (let ((response-code (ewl-extract-response-code)))
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (ewl-parse-json))))

(defun ewl-display-tasks-for-list (list-id list-name)
  "Display response for all tasks in a particular list"
  (let ((list-notes (ewl-get-notes-for-list list-id)))
    (ewl-url-retrieve
     (ewl-url-tasks-for-list list-id)
     'ewl-display-list-items `(,list-name ,list-notes))))

(defun ewl-prepare-buffer (buffer-name mode-cb)
  "Create consistent buffer object for displaying list items"
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (funcall mode-cb)
      (setq buffer-read-only t))
    buf))

(defun ewl-parse-data (data cb &optional cbargs)
  "Parse DATA via CB."
  (mapcar (lambda(item)
            (if cbargs (funcall cb item cbargs)
              (funcall cb item)))
          data))

(defun ewl-pivot-note (note)
  "Pivot NOTE by task id."
  `(,(plist-get note 'task_id) ,note))

(defun ewl-parse-item (item &optional notes)
  "Get relevant data for a specific list item."
  (let* ((id (plist-get item 'id))
         (due-date (plist-get item 'due_date))
         (revision (plist-get item 'revision))
         (type (if (plist-get item 'type) (plist-get item 'type)
                 (if (plist-get item 'list_id) "task")))
         (list-id (plist-get item 'list_id))
         (note (or (assoc id notes) nil)) ;; @TODO: get note associated with this task
         (title (concat (if due-date "s" " ") (if note "n" " ") " " (plist-get item 'title))))
    (propertize title 'id id 'type type 'list-id list-id 'due-date due-date 'revision revision 'note note)))

(defun ewl-parse-note-alist (note)
  "Callback to parse note data into alist."
  (let ((task-id (plist-get item 'task_id))
        (id (plist-get item 'id))
        (content (plist-get item 'content)))
    `(,task-id ,id ,content)))

(defun ewl-parse-note (note)
  "Get relevant data for a specific note."
  (let ((id (plist-get note 'id))
        (content (plist-get note 'content)))
    (propertize content 'id id)))

(defun ewl-display-items (item-list)
  "Format item data for display in buffer"
  (setq buffer-read-only nil)
  (while item-list
    (insert (car item-list))
    (insert "\n")
    (setq item-list (cdr item-list)))
  (setq buffer-read-only t))

(defun ewl-get-task-mode-map ()
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
      (lambda() (interactive) (quit-window t)))
    (define-key map "x"
      (lambda() (interactive) (ewl-create-note-for-task-at-point)))
    (define-key map "i"
      (lambda() (interactive) (debug (text-properties-at 0 (thing-at-point 'line)))))
    (define-key map "di"
      (lambda() (interactive) (ewl-display-inbox)))
    (define-key map "db"
      (lambda() (interactive) (ewl-display-backlog)))
    (define-key map "dp"
      (lambda() (interactive) (ewl-display-priorities)))
    (define-key map "w"
      (lambda() (interactive) (ewl-display-note-for-task-at-point)))
    map))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
(defvar ewl-task-mode-map (ewl-get-task-mode-map)
  "Get the keymap for the ewl window")

(define-derived-mode ewl-task-mode nil "EWLT"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-task-mode':
\\{ewl-task-mode-map}"
  (setq truncate-lines t)
  (add-hook 'post-command-hook 'ewl--highlight-current-line nil t))

(defun ewl-get-notes-mode-map ()
  "Turn this into a function so it can refresh for dev purposes"
  (let ((map (make-sparse-keymap)))
    (define-key map "i" ;; like "insert" mode
      (lambda() (interactive) (ewl-edit-note)))
    (define-key map "q"
      (lambda() (interactive) (ewl-quit-note-buffer)))
    (define-key map (kbd "<C-return>")
      (lambda() (interactive) (ewl-save-note)))
    (define-key map (kbd "<C-escape>")
      (lambda() (interactive) (ewl-stop-edit-note)))
    map))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
(defvar ewl-notes-mode-map (ewl-get-notes-mode-map)
  "Get the keymap for the ewl notes window")

(define-derived-mode ewl-notes-mode nil "EWLN"
  "A major mode for the ewl notes buffer.
The following keys are available in `ewl-notes-mode':
\\{ewl-notes-mode-map}"
  (setq truncate-lines t))

(defun ewl-edit-note ()
  ""
  (setq buffer-read-only nil)
  (evil-insert 1))

(defun ewl-save-note ()
  ""
  (let* ((note (sentence-at-point))
        (id (get-text-property 1 'id note))
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (ewl-update-note id content))
  (setq buffer-read-only t))

(defun ewl-stop-edit-note ()
  ""
  (let ((current-header header-line-format))
    (setq buffer-read-only t)
    (evil-normal-state) ;; Disabling insert state overrides ewl-notes-mode...
    (ewl-notes-mode) ;; .. but turning ewl-notes-mode back on removes header
    (setq header-line-format current-header))) ;; so we re-set the header

(defun ewl-quit-note-buffer ()
  ""
  (interactive)
  (kill-buffer ewl-notes-buffer-name)
  (delete-window)
  (other-window 1))

(defun ewl-get-list-id-from-thing-at-point ()
  "Get list id text property of thing at point."
  (let* ((text-string (thing-at-point 'line))
         (type (get-text-property 1 'type text-string)))
    (if (equal "list" type)
        (get-text-property 1 'id text-string)
      (get-text-property 1 'list-id text-string))))

(defun ewl-display-inbox ()
  "Syntactic sugar to display inbox list."
  (ewl-display-tasks-for-list ewl-list-id-inbox "INBOX"))

(defun ewl-display-backlog ()
  "Syntactic sugar to display backlog list."
  (ewl-display-tasks-for-list ewl-list-id-backlog "BACKLOG"))

(defun ewl-display-priorities ()
  "Syntactic sugar to display priorities list."
  (ewl-display-tasks-for-list ewl-list-id-priorities "PRIORITIES"))

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
  (let ((current-header header-line-format))
    (ewl-display-tasks-for-list (ewl-get-list-id-from-thing-at-point) current-header)))

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
        'ewl-process-response-and-refresh-list
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
   'ewl-process-response-and-refresh-list))

(defun ewl-noop-process-response (status)
  (ewl-process-response))

(defun ewl-process-response-and-refresh-list (status)
  (ewl-process-response)
  (if (get-buffer ewl-task-buffer-name)
      (ewl-refresh-current-list)))

(defun ewl-update-task-at-point (&optional is-complete new-list-id due-date new-title)
  "Update task with relevant data."
  (let* ((text-string (thing-at-point 'line))
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
  (let* ((text-string (thing-at-point 'line))
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
        'ewl-process-response-and-refresh-list
        nil
        "DELETE")))
   (list task-id)))

;; NOTES
(defvar ewl-url-notes (concat ewl-url-base-api "notes"))
(defun ewl-url-notes-for-task (task-id)
  "Get list of notes associated with a particular task."
  (concat ewl-url-notes "?" (url-build-query-string `((task_id , task-id)))))

(defun ewl-url-notes-for-list (list-id)
  "Return API url to get notes for a specific list."
  (concat ewl-url-notes "?" (url-build-query-string `((list_id ,list-id)))))

(defun ewl-url-specific-note (note-id)
  "Return API url to get specific note."
  (concat ewl-url-notes "/" (number-to-string note-id)))

;; @NOTE: Using synchronous resolution here
(defun ewl-get-notes-for-list (list-id)
  "Get all notes for a particular list and return as map of task-id: note info."
  (let* ((url-request-method "GET")
         (url-request-extra-headers (ewl-get-auth-headers)))
    (with-current-buffer (url-retrieve-synchronously (ewl-url-notes-for-list list-id))
      (let ((data (ewl-process-response)))
        (ewl-parse-data data 'ewl-pivot-note)))))

;; If a note already exists, we'll get a 422 response
;; & nothing new will be created
(defun ewl-create-note-for-task-at-point ()
  (let* ((text-string (thing-at-point 'line))
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
                    (content . ,content))))
       (ewl-url-retrieve
        (ewl-url-specific-note note-id)
        'ewl-noop-process-response ;;-and-refresh-list
        nil
        "PATCH"
        (json-encode data))))
   `(,note-id ,content)))

(ewl-ensure-list-ids)

(defun ewl-init ()
  "Initialize plugin."
  ; Also check that lists exist
  (ewl-ensure-list-ids))

(ewl-init)

;; This is only for testing
;;(ewl-get-notes-for-list 380556981)

(defun ewl--highlight-current-line ()
  "Create highlight effect on current line using overlay."
  (if (ewl--thing-on-this-line-p)
      (let ((end (save-excursion
                   (forward-line 1)
                   (point))))
        (move-overlay ewl-highlight-current-line-overlay (line-beginning-position) end))
    (delete-overlay ewl-highlight-current-line-overlay)))

(defun ewl--thing-on-this-line-p ()
  "Determine whether there is a thin on this line."
  (get-text-property (line-beginning-position) 'thing))

(defface ewl-highlight-line-face
  '((((background dark)) :background "#323878")
    (((background light)) :background "#C7CAF2"))
  "Face used to highlight the active line."
  :group 'ewl)

(defvar ewl-highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting the current line.")

(overlay-put ewl-highlight-current-line-overlay
             'face 'ewl-highlight-line-face)
