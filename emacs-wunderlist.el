;;; -*- lexical-binding: t -*-

;; @TODO: Persist inbox ID to file
;; @TODO: Current task
;; Find best way to persist customization data
;; (i.e. auth tokens, list ID for inbox/starred list)

;; Another thing to do: Can I get the extra data for the task? i.e. note/comment
;;
;; TODO:
;; 1: DONE
;; Create major mode DONE ;; set major mode in the buffer I create DONE
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
;; Mark task done (bind to key) DONE
;;
;; 6:
;; Delete task ;; I've got this request forming but HTTP request isn't working :/
;;
;; 7:
;; Move task to new list (Extend method `ewl-mark-task-complete` from 5a for this) DONE
;;
;; 8:
;; Cache responses (when appropriate) to reduce HTTP calls
;;
;; 9:
;; Add README.me to the repo DONE
;;
;; 10:
;; Alter JSON response parsing to handle other-than-200 requests (started)
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

(defcustom ewl-task-buffer-name "*ewl-gtd-buffer*"
  "Name for the emacs wunderlist buffer."
  :group 'ewl
  :type 'string)

(defcustom ewl-config-file (concat user-emacs-directory "emacs-wunderlist-config.el")
  "Location to persist config information for this plugin"
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
  "Return API url for specific task"
  (concat ewl-url-tasks "/" (number-to-string task-id)))

(defun ewl--url-tasks-for-list (list-id)
  "Return API url to get tasks for a specific list"
  (concat ewl-url-tasks "?" (url-build-query-string `((list_id ,list-id)))))

(defun ewl-url-retrieve (url cb &optional method data)
  "Wrap url-retrieve for generic use"
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers (ewl--get-auth-headers))
        (url-request-data data))
    (url-retrieve url cb)))

(defun ewl-display-response (response)
  "Parse and display response in window"
  (let ((json-data (ewl-process-response response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
          (ewl-display-items (ewl-prepare-items-for-display json-data))
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (print "Error processing API request"))))

(defun ewl-process-response (response &optional cb)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 20.*$" (line-end-position) t)
     ;; 204 means no content - trying to json-read 204 response will error
     (if (string-match-p "^HTTP/.+ 204.*$" (thing-at-point 'line t))
           (if (fboundp cb) (funcall cb))
       (when (search-forward "\n\n" nil t)
         (prog1
             (let ((json-object-type 'plist)
                   (json-key-type 'symbol)
                   (json-array-type 'vector))
               (json-read))
           (if (fboundp cb) (funcall cb)))))))

(defun ewl-display-tasks-for-list (list-id)
  "Display response for all tasks in a particular list"
  (ewl-url-retrieve
   (ewl--url-tasks-for-list list-id)
   'ewl-display-response))

(defun ewl-get-folders ()
  "Retrieve all folders"
  (ewl-url-retrieve ewl-url-folders 'ewl-display-response))

(defun ewl-get-lists ()
  "Retrieve all lists"
  (ewl-url-retrieve ewl-url-lists 'ewl-display-response))

;; This seems to work
(defun ewl-get-task (task-id)
  "Get data for particular task"
  (ewl-url-retrieve (ewl--url-specific-task task-id)))

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

(defun ewl--get-mode-map ()
  "Turn this into a function so it can refresh for dev purposes"
  (let ((map (make-sparse-keymap)))
    (define-key map "t"
      (lambda() (interactive) (ewl-add-new-task-to-inbox)))
    (define-key map "c"
      (lambda() (interactive) (ewl-mark-task-at-point-complete)))
    (define-key map "d"
      (lambda() (interactive) (ewl-delete-task-at-point)))
    (define-key map "p"
      (lambda() (interactive) (ewl-prioritize-task-at-point)))
    (define-key map "b"
      (lambda() (interactive) (ewl-backlog-task-at-point)))
    (define-key map "q"
      (lambda() (interactive) (quit-window t (selected-window))))
    map))

(defun ewl--get-id-from-thing-at-point ()
  "Get id text property of thing at point."
  (let* ((text-string (thing-at-point 'word))
         (id (get-text-property 1 'id text-string))
         (type (get-text-property 1 'type text-string)))
    (if (equal type "list") (ewl-display-tasks-for-list id))
    (if (equal type "task") (ewl-mark-task-complete id))))

(defun ewl-get-list-id-from-thing-at-point ()
  "Get list id text property of thing at point."
  (let* ((text-string (thing-at-point 'word))
         (type (get-text-property 1 'type text-string)))
    (if (equal "list" type)
        (get-text-property 1 'id text-string)
      (get-text-property 1 'list-id text-string))))

;; Evil mode will override this
;; It's up to the user to handle evil mode in their configs
(defvar ewl-mode-map (ewl--get-mode-map)
  "Get the keymap for the ewl window")

(define-derived-mode ewl-mode nil "EWL"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-mode':
\\{ewl-mode-map}"
  (setq truncate-lines t))

(defun ewl-create-task (task-title list-id cb)
  "Create task from given inputs"
  (ewl-url-retrieve
   ewl-url-tasks
   cb
   "POST"
   (json-encode `((list_id . ,list-id) (title . ,task-title)))))

(defun ewl-create-task-for-list-at-point ()
  "Create a new task for the current list"
  (ewl-create-task
   (read-from-minibuffer "Enter Task: ")
   (ewl-get-list-id-from-thing-at-point)
   'ewl-process-response-and-refresh-list))

;; @NOTE: This works as a callback, but issue is getting
;; id of list we want to refresh
(defun ewl-refresh-current-list ()
  "Determine what list we're currently in and refresh that data"
  (pop-to-buffer ewl-task-buffer-name)
  (goto-char (point-max))
  (backward-word) ;; Go to the last place we're certain to have a list-id
  (ewl-display-tasks-for-list (ewl-get-list-id-from-thing-at-point)))

(defun ewl-process-response-and-refresh-list (response)
  "Handle response then refresh current list in window"
  (ewl-process-response response 'ewl-refresh-current-list))

(defun ewl-get-single-task (task-id)
  "Return plist of data representing task specified by TASK-ID."
  (ewl-url-retrieve (ewl-url-specific-task task-id) 'ewl-process-response))

;; @TODO: This is currently written to take advantage of
;; lexical binding; I would like it rewritten to pass
;; arguments to a callback and not require lexical binding
(defun ewl-update-task (task-id &optional is-complete new-title new-list-id)
  "Update task by HTTP patch-ing data payload"
  (ewl-url-retrieve
   (ewl--url-specific-task task-id)
   (lambda(response)
     (let* ((task-data (ewl-process-response response))
            (task-revision (plist-get task-data 'revision))
            (url (ewl--url-specific-task task-id))
            (data `((revision . ,task-revision))))

       (if is-complete (nconc data `((completed . ,t))))
       (if new-title (nconc data `((title . ,new-title))))
       (if new-list-id (nconc data `((list_id . ,new-list-id))))

       (ewl-url-retrieve
        url
        'ewl-process-response-and-refresh-list
        "PATCH"
        (json-encode data))))))

;; @TODO I'd like to see this be a toggle instead
(defun ewl-mark-task-complete (task-id)
  "Mark a task complete"
  (ewl-update-task task-id t))

(defun ewl-update-task-text (task-id new-text)
  "Update text of task"
  (ewl-update-task task-id nil new-text))

(defun ewl-move-task-to-new-list (task-id new-list-id)
  "Move task to a different list"
  (ewl-update-task task-id nil nil new-list-id))

(defun ewl-ensure-list-ids ()
  "Ensure that necessary list IDs are populated"
  (if (or
       (not ewl-list-id-inbox)
       (not ewl-list-id-priorities)
       (not ewl-list-id-backlog))
      (ewl-url-retrieve ewl-url-lists 'ewl-load-list-ids)))

;; @TODO: If list IDs are null after this
;; We need to create them
(defun ewl-load-list-ids (response)
  "Parse response of lists API to determine inbox ID"
  (let ((lists-data (ewl-process-response response))
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
(defun ewl-add-new-task-to-inbox ()
  "Add new task to inbox, to be sorted later."
  (ewl-ensure-list-ids)
  (ewl-create-task
   (read-from-minibuffer "Enter task: ")
   ewl-list-id-inbox
   'ewl-process-response))

;; @TODO: Make this work
(defun ewl-backlog-task-at-point ()
  ""
  )

;; @TODO: Make this work
(defun ewl-prioritize-task-at-point ()
  ""
  )

;; @TODO: Make this work
(defun ewl-mark-task-at-point-complete ()
  ""
  )

(defun ewl-display-inbox-list ()
  ""
  )

(defun ewl-display-priorities-list ()
  ""
  )

(defun ewl-display-backlog-list ()
  ""
  )

;; @TODO What the fuck is going on here?
;; j/k, we needed the current revision in order to delete
;; This can/should probably be refactored to simplify
(defun ewl-delete-task-at-point ()
  "Get task at point and pass data to delete operation"
  (let* ((text-string (thing-at-point 'word))
         (id (get-text-property 1 'id text-string))
         (type (get-text-property 1 'type text-string)))
    (if (equal "task" type)
        (ewl-delete-task id)
      (message "delete-task requires task"))))

;; @TODO: Delete works but list won't refresh
(defun ewl-delete-task (task-id)
  "Delete task identified by TASK-ID"
  (ewl-url-retrieve
   (ewl--url-specific-task task-id)
   (lambda(response)
     (let* ((task-data (ewl-process-response response))
            (revision (plist-get task-data 'revision))
            (delete-url (concat
                         (ewl--url-specific-task task-id)
                         "?"
                         (url-build-query-string `((revision ,revision))))))
       (ewl-url-retrieve
        delete-url
        'ewl-process-response-and-refresh-list
        "DELETE")))))

(defun ewl-init ()
  "Basic entry point"
  (ewl-ensure-list-ids)
  (ewl-display-tasks-for-list ewl-list-id-priorities))

(ewl-init)
