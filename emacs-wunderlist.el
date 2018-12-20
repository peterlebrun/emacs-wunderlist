;;; -*- lexical-binding: t -*-

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
  (let ((json-data (ewl-process-response response)))
    (if json-data
        (with-current-buffer (ewl-prepare-display-buffer)
          (setq buffer-read-only nil)
          (ewl-display-items (ewl-prepare-items-for-display json-data))
          (setq buffer-read-only t)
          (pop-to-buffer (current-buffer)))
      (print "NO DICE FAM"))))

(defun ewl-process-response (response &optional cb)
 "Extract the JSON response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (if (re-search-forward "^HTTP/.+ 20.*$" (line-end-position) t)
     (when (search-forward "\n\n" nil t)
       (prog1
           (let ((json-object-type 'plist)
                 (json-key-type 'symbol)
                 (json-array-type 'vector))
             (json-read))
         (if (fboundp cb) (funcall cb))))))

(defun ewl-get-tasks-for-list (list-id)
  (ewl-url-retrieve
   (ewl--url-tasks-for-list list-id)
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

(defun ewl-prepare-items-for-display (item-list)
  "Pivot data into display format"
  (mapcar (lambda(item-data)
            (ewl-parse-item item-data))
          item-list))

;; @TODO: Make it so that ewl-get-task
;; passes off to this single task
(defun ewl-parse-item (item-data)
  "Get relevant data for a specific task."
  (let ((id (plist-get item-data 'id))
        (title (plist-get item-data 'title))
        (type (if (plist-get item-data 'type) (plist-get item-data 'type)
                (if (plist-get item-data 'list_id) "task")))
        (list-id (plist-get item-data 'list_id)))
    (propertize title 'id id 'type type 'list-id list-id)))

(defun ewl-display-items (item-list)
  "Foobarf"
  (setq buffer-read-only nil)
  (while item-list
    (let* ((title (car item-list)))
      (insert (concat title "\n")))
    (setq item-list (cdr item-list)))
  (setq buffer-read-only t))

(defun ewl--get-mode-map ()
  "Turn this into a function so it can refresh for dev purposes"
  (let ((map (make-sparse-keymap)))
    (define-key map "q"
      (lambda() (interactive) (quit-window t (selected-window))))
    (define-key map "\r"
      (lambda() (interactive) (ewl--get-id-from-thing-at-point)))
    (define-key map "c"
      (lambda() (interactive) (ewl-create-task)))
    (define-key map "u"
      (lambda() (interactive) (ewl-get-lists)))
    (define-key map "z"
      (lambda() (interactive) (ewl-who-fuckin-knows)))
    ;; (define-key map "\r" mark task complete
    ;; (define-key map "n" add new task
    ;; m move to different list
    ;; p prioritize
    ;; d delete
      ;;(lambda() (interactive) (ewl--decide-what-to-do)))
    map))

(defun ewl--get-id-from-thing-at-point ()
  "Get id text property of thing at point."
  (let* ((text-string (thing-at-point 'word))
         (id (get-text-property 1 'id text-string))
         (type (get-text-property 1 'type text-string)))
    (if (equal type "list") (ewl-get-tasks-for-list id))
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
(defvar ewl-mode-map (ewl--get-mode-map) "Get the keymap for the ewl window")

(define-derived-mode ewl-mode nil "EWL"
  "A major mode for the ewl task buffer.
The following keys are available in `ewl-mode':
\\{ewl-mode-map}"
  (setq truncate-lines t))

;; @TODO: Using ewl-process-response like this works, BUT
;; we need some sort of way to refresh the buffer
(defun ewl-create-task ()
  "Create a new task for the current list"
  (let ((task-title (read-from-minibuffer "Enter Task: "))
        (list-id (ewl-get-list-id-from-thing-at-point)))
    (ewl-url-retrieve
     ewl-url-tasks
     'ewl-process-response-and-refresh-list
     "POST"
     (json-encode `((list_id . ,list-id) (title . ,task-title))))))

;; @NOTE: This works as a callback, but issue is getting
;; id of list we want to refresh
(defun ewl-refresh-current-list ()
  (pop-to-buffer ewl-task-buffer-name)
  (goto-char (point-max))
  (backward-word) ;; Go to the last place we're certain to have a list-id
  (ewl-get-tasks-for-list (ewl-get-list-id-from-thing-at-point)))

(defun ewl-process-response-and-refresh-list (response)
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

       (ewl-url-retrieve url 'ewl-display-response "PATCH" (json-encode data))))))

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

(defun ewl-init ()
  "Basic entry point"
  (ewl-get-lists))

(ewl-init)
