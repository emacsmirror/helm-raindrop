;;; helm-raindrop.el --- Raindrop.io with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 by Takashi Masuda

;; Author: Takashi Masuda <masutaka.net@gmail.com>
;; URL: https://github.com/masutaka/emacs-helm-raindrop
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (helm "4.0.4") (request "0.3.2"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; helm-raindrop.el provides a helm interface to Raindrop.io (https://raindrop.io/).

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'json)
(require 'request)

(defgroup helm-raindrop nil
  "Raindrop.io with helm interface"
  :prefix "helm-raindrop-"
  :group 'helm)

(defcustom helm-raindrop-access-token nil
  "Your raindrop access token.
You can create on https://app.raindrop.io/settings/integrations"
  :type '(choice (const nil)
		 string)
  :group 'helm-raindrop)

(defcustom helm-raindrop-collection-ids nil
  "The IDs of the collections you want to collect.
If the collection URL is https://app.raindrop.io/my/123456, then it is 123456.
Can be either a single string or a list of strings for multiple collections.

Examples:
  \"123456\"                ;; single collection
  '(\"123456\" \"789012\")  ;; multiple collections"
  :type '(choice (const nil)
		 string
		 (repeat string))
  :group 'helm-raindrop)

(defcustom helm-raindrop-include-nested-collections t
  "Include items from nested collections when non-nil."
  :type 'boolean
  :group 'helm-raindrop)

(defcustom helm-raindrop-file
  (expand-file-name "helm-raindrop" user-emacs-directory)
  "A cache file get items with `helm-raindrop-search-query'."
  :type '(choice (const nil)
		 string)
  :group 'helm-raindrop)

(defcustom helm-raindrop-interval (* 3 60 60)
  "Number of seconds to call `helm-raindrop-http-request'."
  :type 'integer
  :group 'helm-raindrop)

(defcustom helm-raindrop-debug-mode nil
  "Debug logging level for HTTP requests.
nil: No logging
`info': Summary only
`debug': All debug messages"
  :type '(choice (const :tag "No logging" nil)
                 (const :tag "Info - Summary only" info)
                 (const :tag "Debug - All messages" debug))
  :group 'helm-raindrop)

;;; Internal Constants

(defconst helm-raindrop--work-buffer-name " *helm-raindrop-work*"
  "Working buffer name of `helm-raindrop-http-request'.")

(defconst helm-raindrop--http-status-ratelimit 429
  "HTTP status code for rate limit exceeded.")

(defconst helm-raindrop--default-retry-after 2
  "Default retry-after value in seconds when not specified in headers.")

(defconst helm-raindrop--default-ratelimit 120
  "Default API rate limit per hour when not specified in headers.")

(defconst helm-raindrop--max-retries 3
  "Maximum number of retries for rate limited requests.")

;;; Internal Variables

(defvar helm-raindrop--api-per-page 50
  "Page size of Raindrop.io API.
See https://developer.raindrop.io/v1/raindrops/multiple")

(defvar helm-raindrop--full-frame helm-full-frame)

(defvar helm-raindrop--remaining-collection-ids nil
  "Remaining collection IDs to process.")

(defvar helm-raindrop--ratelimit-remaining nil
  "Remaining number of requests before hitting rate limit.")

(defvar helm-raindrop--ratelimit-limit nil
  "Maximum number of requests allowed per hour.")

(defvar helm-raindrop--ratelimit-reset nil
  "Unix timestamp when the rate limit will reset.")

(defvar helm-raindrop--timer nil
  "Timer object for Raindrop.io caching will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar helm-raindrop--debug-start-time nil
  "Start time for debugging individual API requests.")

(defvar helm-raindrop--debug-total-start-time nil
  "Start time for debugging the entire session.")

(defvar helm-raindrop--debug-request-count 0
  "Number of API requests made in the current session.")

(defvar helm-raindrop--debug-current-collection-processed-items 0
  "Number of items processed in the current collection.")

(defvar helm-raindrop--debug-current-collection-total-items 0
  "Total number of items in the current collection.")

;;; Macro

(defmacro helm-raindrop-file-check (&rest body)
  "The BODY is evaluated only when `helm-raindrop-file' exists."
  `(if (file-exists-p helm-raindrop-file)
       ,@body
     (message "%s not found. Please wait up to %d minutes."
	      helm-raindrop-file (/ helm-raindrop-interval 60))))

;;; Helm source

(defun helm-raindrop-load ()
  "Load `helm-raindrop-file'."
  (helm-raindrop-file-check
   (with-current-buffer (helm-candidate-buffer 'global)
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents helm-raindrop-file)))))

(defvar helm-raindrop-action
  '(("Browse URL" . helm-raindrop-browse-url)
    ("Show URL" . helm-raindrop-show-url)
    ("Show NOTE" . helm-raindrop-show-note)))

(defun helm-raindrop-browse-url (candidate)
  "Action for Browse URL.
Argument CANDIDATE a line string of a raindrop."
  (string-match "\\[href:\\(.*?\\)\\]" candidate)
  (browse-url (match-string 1 candidate)))

(defun helm-raindrop-show-url (candidate)
  "Action for Show URL.
Argument CANDIDATE a line string of a raindrop."
  (string-match "\\[href:\\(.*?\\)\\]" candidate)
  (message (match-string 1 candidate)))

(defun helm-raindrop-show-note (candidate)
  "Action for Show NOTE.
Argument CANDIDATE a line string of a raindrop."
  (string-match "\\[note:\\(.*?\\)\\]" candidate)
  (message (match-string 1 candidate)))

(defvar helm-raindrop-source
  (helm-build-in-buffer-source "Raindrops"
    :init #'helm-raindrop-load
    :action 'helm-raindrop-action
    :multiline t
    :migemo t)
  "Helm source for Raindrop.io.")

;;;###autoload
(defun helm-raindrop ()
  "Search Raindrops using `helm'."
  (interactive)
  (let ((helm-full-frame helm-raindrop--full-frame))
    (helm-raindrop-file-check
     (helm :sources helm-raindrop-source
	   :prompt "Find Raindrops: "))))

;;; Process handler

(defun helm-raindrop-http-request ()
  "Initialize and start HTTP requests for creating `helm-raindrop-file'."
  (helm-raindrop-debug-session-start)
  (if (get-buffer helm-raindrop--work-buffer-name)
      (kill-buffer helm-raindrop--work-buffer-name))
  (get-buffer-create helm-raindrop--work-buffer-name)
  (helm-raindrop-init-remaining-collection-ids)
  (helm-raindrop-init-ratelimit-state)
  ;; Start the first request
  (let ((collection-id (helm-raindrop-get-current-collection-id))
        (page 0)
        (retry-count 0))
    (helm-raindrop-do-http-request collection-id page retry-count)))

(defun helm-raindrop-do-http-request (collection-id page retry-count)
  "Actually perform the HTTP request.
COLLECTION-ID is the current collection ID.
PAGE is the current page number.
RETRY-COUNT tracks the number of retry attempts."
  ;; Check rate limit before making request
  (if (helm-raindrop-ratelimit-exceeded-p)
      (helm-raindrop-wait-for-ratelimit-reset collection-id page retry-count)
    ;; Continue with the request if rate limit is not exceeded
    (helm-raindrop-debug-page-start)
    (request
      (helm-raindrop-get-url collection-id page)
      :headers `(("Authorization" . ,(concat "Bearer " helm-raindrop-access-token)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
                  (helm-raindrop-update-ratelimit-from-headers response)
		  (helm-raindrop-debug-page-finish page (request-response-url response) data)
                  (with-current-buffer (get-buffer helm-raindrop--work-buffer-name)
                    (goto-char (point-max))
                    (helm-raindrop-insert-items data)
                    (if (helm-raindrop-next-page-exist-p data)
                        (helm-raindrop-do-http-request collection-id (1+ page) 0)
                      ;; Current collection finished, check if there are more
                      (if (helm-raindrop-next-collection-exist-p)
                          ;; More collections to process
                          (helm-raindrop-process-next-collection)
                        ;; All collections processed
                        (helm-raindrop-session-finish))))))
      :error (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
                (helm-raindrop-update-ratelimit-from-headers response)
                (if (helm-raindrop-should-retry-p (request-response-status-code response) retry-count)
                    (helm-raindrop-handle-ratelimit-error response collection-id page retry-count)
                  ;; Log error and continue with next collection
                  (helm-raindrop-debug-page-error (request-response-url response) error-thrown)
                  (if (helm-raindrop-next-collection-exist-p)
                      ;; More collections to process
                      (helm-raindrop-process-next-collection)
                    ;; All collections processed
                    (helm-raindrop-session-finish))))))))

(defun helm-raindrop-init-remaining-collection-ids ()
  "Initialize remaining collection IDs and validate configuration."
  (setq helm-raindrop--remaining-collection-ids
        (helm-raindrop-normalize-collection-ids))
  (unless helm-raindrop--remaining-collection-ids
    (error "No collection IDs configured. Please set `helm-raindrop-collection-ids'.")))

(defun helm-raindrop-normalize-collection-ids ()
  "Normalize collection IDs to a list format.
Return a list of collection ID strings."
  (cond
   ((null helm-raindrop-collection-ids) nil)
   ((stringp helm-raindrop-collection-ids) (list helm-raindrop-collection-ids))
   ((listp helm-raindrop-collection-ids) helm-raindrop-collection-ids)
   (t (error "Invalid type for `helm-raindrop-collection-ids'"))))

(defun helm-raindrop-get-current-collection-id ()
  "Get the current collection ID being processed."
  (car helm-raindrop--remaining-collection-ids))

(defun helm-raindrop-next-collection-exist-p ()
  "Return non-nil if there are more collections to process."
  (cdr helm-raindrop--remaining-collection-ids))

(defun helm-raindrop-process-next-collection ()
  "Process the next collection in the list."
  (helm-raindrop-debug-collection-start)
  (setq helm-raindrop--remaining-collection-ids
        (cdr helm-raindrop--remaining-collection-ids))
  (if helm-raindrop--remaining-collection-ids
      (helm-raindrop-do-http-request (car helm-raindrop--remaining-collection-ids) 0 0)))

(defun helm-raindrop-session-finish ()
  "Called when all collections have been processed."
  (let ((work-buffer (get-buffer helm-raindrop--work-buffer-name)))
    (if (and work-buffer (> (buffer-size work-buffer) 0))
        (with-current-buffer work-buffer
          (write-region (point-min) (point-max) helm-raindrop-file))))
  (helm-raindrop-cleanup-session)
  (helm-raindrop-debug-session-finish))

(defun helm-raindrop-cleanup-session ()
  "Clean up session variables."
  (setq helm-raindrop--remaining-collection-ids nil))

(defun helm-raindrop-get-url (collection-id page)
  "Return Raindrop.io API endpoint for getting items.
COLLECTION-ID is the current collection ID.
PAGE is the current page number."
  (format "https://api.raindrop.io/rest/v1/raindrops/%s?%s"
	  collection-id
	  (url-build-query-string
	   `((page ,page)
	     (perpage ,helm-raindrop--api-per-page)
	     ,@(if helm-raindrop-include-nested-collections '((nested "true")))))))

(defun helm-raindrop-insert-items (response-body)
  "Insert Raindrop items as the format of `helm-raindrop-file'.
Argument RESPONSE-BODY is http response body as a json"
  (let ((items (helm-raindrop-items response-body))
	item title url note format-tags)
    (dotimes (i (length items))
      (setq item (aref items i)
	    title (helm-raindrop-item-title item)
	    url (helm-raindrop-item-url item)
	    note (helm-raindrop-item-note item)
	    format-tags (helm-raindrop-item-format-tags item))
      (insert
       (decode-coding-string
	(if (string-empty-p format-tags)
	    (format "%s [note:%s][href:%s]\n" title note url)
	  (format "%s %s [note:%s][href:%s]\n" format-tags title note url))
	'utf-8)))))

(defun helm-raindrop-next-page-exist-p (response-body)
  "Return whether the next page exists from RESPONSE-BODY."
  (let ((items (helm-raindrop-items response-body)))
    (> (length items) 0)))

;;; Utilities

(defun helm-raindrop-x-ratelimit-limit (response)
  "Get x-ratelimit-limit value from RESPONSE headers."
  (let ((headers (request-response-headers response)))
    (cdr (assoc 'x-ratelimit-limit headers))))

(defun helm-raindrop-x-ratelimit-remaining (response)
  "Get x-ratelimit-remaining value from RESPONSE headers."
  (let ((headers (request-response-headers response)))
    (cdr (assoc 'x-ratelimit-remaining headers))))

(defun helm-raindrop-x-ratelimit-reset (response)
  "Get x-ratelimit-reset value from RESPONSE headers."
  (let ((headers (request-response-headers response)))
    (cdr (assoc 'x-ratelimit-reset headers))))

(defun helm-raindrop-retry-after (response)
  "Get retry-after value from RESPONSE headers."
  (let ((headers (request-response-headers response)))
    (cdr (assoc 'retry-after headers))))

(defun helm-raindrop-items (response-body)
  "Return items from RESPONSE-BODY."
  (cdr (assoc 'items response-body)))

(defun helm-raindrop-item-title (item)
  "Return a name of ITEM."
  (cdr (assoc 'title item)))

(defun helm-raindrop-item-url (item)
  "Return a url of ITEM."
  (cdr (assoc 'link item)))

(defun helm-raindrop-item-note (item)
  "Return a note of ITEM."
  (cdr (assoc 'note item)))

(defun helm-raindrop-item-format-tags (item)
  "Return formatted tags of ITEM."
  (let ((result ""))
    (mapc
     (lambda (tag)
       (setq result (format "%s[%s]" result tag)))
     (helm-raindrop-item-tags item))
    (string-trim result)))

(defun helm-raindrop-item-tags (item)
  "Return tags of ITEM, as an list."
  (append (cdr (assoc 'tags item)) nil))

(defun helm-raindrop-total-count (response-body)
  "Return total count from RESPONSE-BODY."
  (cdr (assoc 'count response-body)))

;;; Rate limit handler

(defun helm-raindrop-init-ratelimit-state ()
  "Initialize rate limit state variables."
  (setq helm-raindrop--ratelimit-remaining nil
	helm-raindrop--ratelimit-limit nil
	helm-raindrop--ratelimit-reset nil))

(defun helm-raindrop-ratelimit-exceeded-p ()
  "Return t if rate limit is exceeded and we need to wait."
  (and helm-raindrop--ratelimit-remaining
       (= helm-raindrop--ratelimit-remaining 0)
       helm-raindrop--ratelimit-reset
       (< (float-time) helm-raindrop--ratelimit-reset)))

(defun helm-raindrop-wait-for-ratelimit-reset (collection-id page retry-count)
  "Wait until rate limit resets and retry the request.
COLLECTION-ID is the current collection ID.
PAGE is the current page number.
RETRY-COUNT is the current retry attempt."
  (let ((wait-seconds (max 0 (- helm-raindrop--ratelimit-reset (float-time)))))
    (helm-raindrop-debug-page-ratelimit-wait wait-seconds)
    (run-at-time wait-seconds nil #'helm-raindrop-do-http-request collection-id page retry-count)))

(defun helm-raindrop-update-ratelimit-from-headers (response)
  "Update rate limit state from RESPONSE headers."
  (if-let ((limit (helm-raindrop-x-ratelimit-limit response)))
      (setq helm-raindrop--ratelimit-limit (string-to-number limit)))
  (if-let ((remaining (helm-raindrop-x-ratelimit-remaining response)))
      (setq helm-raindrop--ratelimit-remaining (string-to-number remaining)))
  (if-let ((reset (helm-raindrop-x-ratelimit-reset response)))
      (setq helm-raindrop--ratelimit-reset (string-to-number reset))))

(defun helm-raindrop-should-retry-p (status-code retry-count)
  "Return t if we should retry the request.
STATUS-CODE is the HTTP status code.
RETRY-COUNT is the current retry attempt."
  (and (helm-raindrop-ratelimit-error-p status-code)
       (< retry-count helm-raindrop--max-retries)))

(defun helm-raindrop-ratelimit-error-p (status-code)
  "Return t if STATUS-CODE indicates a rate limit error."
  (and status-code (= status-code helm-raindrop--http-status-ratelimit)))

(defun helm-raindrop-handle-ratelimit-error (response collection-id page retry-count)
  "Handle 429 rate limit error from RESPONSE.
COLLECTION-ID is the current collection ID.
PAGE is the current page number.
RETRY-COUNT is the current retry attempt."
  (let* ((retry-after (helm-raindrop-retry-after response))
	 (wait-seconds (or (and retry-after (string-to-number retry-after))
			   helm-raindrop--default-retry-after)))
    (helm-raindrop-debug-page-ratelimit-retry wait-seconds (1+ retry-count))
    (run-at-time wait-seconds nil #'helm-raindrop-do-http-request collection-id page (1+ retry-count))))

;;; Debug

(defun helm-raindrop-debug-session-start ()
  "Initialize debug information for the session."
  (setq helm-raindrop--debug-total-start-time (current-time)
	helm-raindrop--debug-request-count 0)
  (helm-raindrop-debug-collection-start))

(defun helm-raindrop-debug-collection-start ()
  "Initialize debug information for a collection."
  (setq helm-raindrop--debug-current-collection-processed-items 0
        helm-raindrop--debug-current-collection-total-items 0))

(defun helm-raindrop-debug-page-start ()
  "Initialize debug information for a page request."
  (cl-incf helm-raindrop--debug-request-count)
  (setq helm-raindrop--debug-start-time (current-time)))

(defun helm-raindrop-debug-page-finish (page url response-body)
  "Record successful page request information.
PAGE is the current page number.
URL is the request URL.
RESPONSE-BODY is the parsed JSON response."
  (if (eq page 0)
      (setq helm-raindrop--debug-current-collection-total-items
	    (helm-raindrop-total-count response-body))
    (cl-incf helm-raindrop--debug-current-collection-processed-items
             (length (helm-raindrop-items response-body)))
    (if (eq helm-raindrop-debug-mode 'debug)
	(let ((total-collections (length (helm-raindrop-normalize-collection-ids)))
              (remaining-collections (length helm-raindrop--remaining-collection-ids)))
          (message "[Raindrop] Succeed to GET %s [collections: %d/%d] [items: %d/%d] (%0.1fsec) [rate limit: %d/%d] at %s."
	           url
	           (1+ (- total-collections remaining-collections))
	           total-collections
                   helm-raindrop--debug-current-collection-processed-items
                   helm-raindrop--debug-current-collection-total-items
	           (time-to-seconds
		    (time-subtract (current-time)
			           helm-raindrop--debug-start-time))
	           (or helm-raindrop--ratelimit-remaining 0)
	           (or helm-raindrop--ratelimit-limit helm-raindrop--default-ratelimit)
	           (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))))

(defun helm-raindrop-debug-page-error (url error-thrown)
  "Record failed page request information.
URL is the request URL.
ERROR-THROWN is (ERROR-SYMBOL . DATA), or nil."
  (if (eq helm-raindrop-debug-mode 'debug)
      (message "[Raindrop] Fail %S to GET %s (%0.1fsec) at %s."
	       error-thrown
	       url
	       (time-to-seconds
		(time-subtract (current-time)
			       helm-raindrop--debug-start-time))
	       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

(defun helm-raindrop-debug-page-ratelimit-wait (wait-seconds)
  "Record rate limit wait information.
WAIT-SECONDS is the seconds to wait."
  (if (memq helm-raindrop-debug-mode '(info debug))
      (message "[Raindrop] Rate limit reached. Waiting %0.1f seconds..."
	       wait-seconds)))

(defun helm-raindrop-debug-page-ratelimit-retry (wait-seconds retry-count)
  "Record rate limit retry information.
WAIT-SECONDS is the seconds to wait.
RETRY-COUNT is the current retry attempt."
  (if (memq helm-raindrop-debug-mode '(info debug))
      (message "[Raindrop] Rate limit error (429). Retrying in %d seconds... (attempt %d/%d)"
	       wait-seconds retry-count helm-raindrop--max-retries)))

(defun helm-raindrop-debug-session-finish ()
  "Record session completion information."
  (if (memq helm-raindrop-debug-mode '(info debug))
      (message "[Raindrop] Total: %d requests completed in %0.1fsec at %s."
	       helm-raindrop--debug-request-count
	       (time-to-seconds
		(time-subtract (current-time)
			       helm-raindrop--debug-total-start-time))
	       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

;;; Timer

(defun helm-raindrop-set-timer ()
  "Set timer."
  (setq helm-raindrop--timer
	(run-at-time "0 sec"
		     helm-raindrop-interval
		     #'helm-raindrop-http-request)))

(defun helm-raindrop-cancel-timer ()
  "Cancel timer."
  (when helm-raindrop--timer
    (cancel-timer helm-raindrop--timer)
    (setq helm-raindrop--timer nil)))

;;;###autoload
(defun helm-raindrop-initialize ()
  "Initialize `helm-raindrop'."
  (unless helm-raindrop-access-token
    (error "Variable `helm-raindrop-access-token' is nil"))
  (unless helm-raindrop-collection-ids
    (error "Variable `helm-raindrop-collection-ids' is nil"))
  (helm-raindrop-set-timer))

(provide 'helm-raindrop)

;;; helm-raindrop.el ends here
