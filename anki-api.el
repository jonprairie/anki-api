;;; anki-api.el --- Wrapper library around AnkiConnect api -*- lexical-binding: t; -*-

;; Author: Jonathan Prairie
;; Version: 0.0.1
;; URL: https://github.com/jonprairie/anki-api
;; Package-Requires: ((emacs "28.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Wrapper library around AnkiConnect, a plugin for anki that exposes
;;  an http api for interacting with anki in various ways.
;;
;;  For this package to work, you have to setup these external dependencies:
;;  - Anki
;;  - AnkiConnect, an Anki addon that runs an RPC server over HTTP to expose
;;                 Anki functions as APIs, for installation instructions see
;;                 https://github.com/FooSoft/anki-connect#installation
;;  - curl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


(require 'cl-lib)
(require 'json)
(require 'dash)
(require 's)


;;; global vars


(defconst anki-api-version 6)

(defgroup anki-api nil
  "Customizations for anki-api."
  :group 'org)

(defcustom anki-api-host "127.0.0.1"
  "The network address AnkiConnect is listening on."
  :type 'string)

(defcustom anki-api-port "8765"
  "The port number AnkiConnect is listening on."
  :type 'string)

(defcustom anki-api-key ""
  "The api key to send to AnkiConnect for auth.
Won't be sent if empty."
  :type 'string)

(defcustom anki-api-allow-dups nil
  "If nil, will fail when adding duplicate notes"
  :type 'boolean)


;;; cURL wrapper


(cl-defun anki-api--fetch (url
                           &rest settings
                           &key
                           (type "GET")
                           data success _error
                           (parser 'buffer-string)
                           &allow-other-keys)
  "Fetch URL using curl.
The api is borrowed from request.el."
  ;; This exists because request.el's sync mode calls curl asynchronously under
  ;; the hood, which doesn't work on some machines (like mine) where the process
  ;; sentinel never gets called. After some debugging of Emacs, it seems that in
  ;; 'process.c' the pselect syscall to the file descriptor of inotify used by
  ;; 'autorevert' always returns a nonzero value and causes 'status_notify' never
  ;; being called. To determine whether it's a bug in Emacs and make a patch
  ;; requires more digging.
  (message "request: %s" data)
  (let ((tempfile (make-temp-file "emacs-anki-editor"))
        (responsebuf (generate-new-buffer " *anki-editor-curl*")))
    (when data
      (with-temp-file tempfile
        (setq buffer-file-coding-system 'utf-8)
        (set-buffer-multibyte t)
        (insert data)))
    (unwind-protect
        (with-current-buffer responsebuf
          (apply #'call-process "curl" nil t nil (list
                                                  url
                                                  "--silent"
                                                  "-X" type
                                                  "--data-binary"
                                                  (concat "@" tempfile)))

          (goto-char (point-min))
          (message "response: %s" (buffer-string))
          (when success
            (apply success (list :data (funcall parser)))))
      (kill-buffer responsebuf)
      (delete-file tempfile))))

(defun anki-api-build-request-plist (action &optional params)
  "Convert an `anki-api-request' REQUEST into a plist."
  (let* ((pl (list :action action
                   :version anki-api-version)))
    (when params
      (plist-put pl :params params))
    (unless (string= anki-api-key "")
      (plist-put pl :apiKey anki-api-key))
    pl))

(defun anki-api-call (action &optional params)
  "Send ACTION request to AnkiConnect with PARAMS.
ACTION should be a symbol corresponding to an AnkiConnect action.

PARAMS should be a plist corresponding to the json params key
required for that ACTION.

for more info see: https://foosoft.net/projects/anki-connect/index.html"
  (let* ((payload (anki-api-build-request-plist action params))
         (_request-backend 'curl)
         (json-array-type 'list)
         reply err)
    (anki-api--fetch (format "http://%s:%s"
                             anki-api-host
                             anki-api-port)
                     :type "POST"
                     :parser 'json-read
                     :data (json-encode payload)
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (setq reply data)))
                     :error (cl-function
                             (lambda (&key error-thrown &allow-other-keys)
                               (setq err (string-trim (cdr error-thrown)))))
                     :sync t)
    (when err
      (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun anki-api-dispatch (request)
  (let ((result (anki-api-call (anki-api-request-action request)
                               (anki-api-request-params request))))
    (let-alist result
      (if .error
          (funcall (anki-api-request-on-error request) .error)
        (let* ((post-process (anki-api-request-post-process request))
               (processed (if post-process (funcall post-process .result) .result))
               (on-success (anki-api-request-on-success request))
               (processed (if on-success
                              (funcall on-success
                                       processed
                                       (anki-api-request-context request))
                            processed)))
          processed)))))

(defun anki-api-dispatch-all (requests)
  (let* ((requests-plist (--map (anki-api-build-request-plist
                                 (anki-api-request-action it)
                                 (anki-api-request-params it))
                                requests))
         (request (list 'action "multi" 'version 6
                        'params (list 'actions requests-plist))))
    (anki-api-dispatch request)))


;;; AnkiConnect api wrapper


(defun anki-api-make-note (id deck model fields tags)
  (list :id (string-to-number (or id "0"))
        :deckName deck
        :modelName model
        :fields fields
        :tags (vconcat tags)
        :options (list :allowDuplicate
                       (or anki-api-allow-dups :json-false))))

(cl-defstruct anki-api-request
  "A request to the AnkiConnect api.
ACTION is a symbol corresponding to an AnkiConnect action, documented here:
https://foosoft.net/projects/anki-connect/index.html#card-actions

PARAMS is a plist representing the data that the ACTION expects. Ie if
ACTION is 'notesInfo then PARAMS should be something like
'(:notes (1502298033753))

CONTEXT is an optional plist of extra data to be passed to the ON-SUCCESS
or ON-ERROR callbacks.

ON-SUCCESS and ON-ERROR are optional functions which will be invoked when
a request is processed and returned by the AnkiConnect api. ON-SUCCESS
will be called when the action was successful and will be passed the
response data (as a list) corresponding to the `results' key in the return
JSON, as well as the CONTEXT. ON-ERROR will be called the same way when
an error response is received, but will be passed the `error' key instead
of `results'.

POST-PROCESS is a callback function which will process the response from a
successful request to AnkiConnect. The return value from POST-PROCESS will
be passed along to ON-SUCCESS instead of the raw results from the response."
  action params context on-success on-error post-process)

(defmacro anki-api-def-wrapper (action &optional parms pre-process post-process)
  (declare (indent defun))
  (let* ((action-name (substring (symbol-name action) 1))
         (action (intern (s-lower-camel-case action-name)))
         (defun-name (intern (s-concat "anki-api-" action-name)))
         (lambda-list (-non-nil parms))
         (pre-process (if pre-process
                          pre-process
                        `(list ,@(--remove (s-prefix? "&" (symbol-name it))
                                           parms)))))
    `(defun ,defun-name ,lambda-list
       (let ((req-parms ,pre-process))
         (make-anki-api-request
          :action ',action
          :params (nth 0 req-parms)
          :context (nth 1 req-parms)
          :on-success (nth 2 req-parms)
          :on-error (nth 3 req-parms)
          :post-process ,(when post-process
                           `(lambda (results) ,post-process)))))))

;; deck wrappers

(anki-api-def-wrapper :create-deck (deck-name &optional &key on-success on-error)
  (list (list :deckName deck-name) nil on-success on-error))

(anki-api-def-wrapper :change-deck (card-ids new-deck &optional &key on-success on-error)
  (list (list :cards card-ids :deck new-deck) nil on-success on-error))

;; note wrappers

(anki-api-def-wrapper :add-note (note &optional &key context on-success on-error))

(anki-api-def-wrapper :update-note (note &optional &key context on-success on-error)
  (list (list :fields
              (plist-get note :fields)
              (plist-get note :tags))
        context on-success on-error))

(anki-api-def-wrapper :notes-info (note-id &optional &key on-success on-error)
  (list (list :notes (list (string-to-number note-id)))
        nil on-success on-error)
  (car results))

;; graphical wrappers

(anki-api-def-wrapper :gui-current-card (nil nil &optional &key on-success on-error))

;; model wrappers

(anki-api-def-wrapper :model-names (nil nil &optional &key on-success on-error))
(anki-api-def-wrapper :model-field-names (model-name &optional &key on-success on-error)
  (list (list :modelName model-name) nil on-success on-error))

(provide 'anki-api)
;;; anki-api.el ends here
