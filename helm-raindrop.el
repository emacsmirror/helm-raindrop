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

(defcustom helm-raindrop-collection-id nil
  "The ID of the collection you want to collect.
If the collection URL is https://app.raindrop.io/my/123456, then it is 123456."
  :type '(choice (const nil)
		 string)
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
  "Enable debug mode for HTTP requests."
  :type 'boolean
  :group 'helm-raindrop)

;;; Internal Variables

(defvar helm-raindrop-api-per-page 50
  "Page size of Raindrop.io API.
See https://developer.raindrop.io/v1/raindrops/multiple")

(defconst helm-raindrop-work-buffer-name " *helm-raindrop-work*"
  "Working buffer name of `helm-raindrop-http-request'.")

(defvar helm-raindrop-full-frame helm-full-frame)

(defvar helm-raindrop-timer nil
  "Timer object for Raindrop.io caching will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar helm-raindrop-debug-start-time nil)
(defvar helm-raindrop-debug-total-start-time nil)
(defvar helm-raindrop-debug-request-count 0)

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
  (string-match "\\[href:\\(.+\\)\\]" candidate)
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
  (let ((helm-full-frame helm-raindrop-full-frame))
    (helm-raindrop-file-check
     (helm :sources helm-raindrop-source
	   :prompt "Find Raindrops: "))))

;;; Process handler

(defun helm-raindrop-http-request (&optional page)
  "Make a new HTTP request for create `helm-raindrop-file'.
It is the first page if PAGE is nil."
  (setq page (or page 0))
  (when (eq page 0)
    (if (get-buffer helm-raindrop-work-buffer-name)
	(kill-buffer helm-raindrop-work-buffer-name))
    (get-buffer-create helm-raindrop-work-buffer-name)
    (helm-raindrop-debug-init-session))
  (helm-raindrop-debug-start-request)
  (request
    (helm-raindrop-get-url page)
    :headers `(("Authorization" . ,(concat "Bearer " helm-raindrop-access-token)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data response &allow-other-keys)
		(helm-raindrop-debug-log-request-success (request-response-url response))
		(with-current-buffer (get-buffer helm-raindrop-work-buffer-name)
		  (goto-char (point-max))
		  (helm-raindrop-insert-items data)
		  (if (helm-raindrop-next-page-exist-p data)
		      (helm-raindrop-http-request (1+ page))
		    (write-region (point-min) (point-max) helm-raindrop-file)
		    (helm-raindrop-debug-log-session-summary)))))
    :error (cl-function
	    (lambda (&key error-thrown response &allow-other-keys)
	      (helm-raindrop-debug-log-request-error (request-response-url response) error-thrown)))))

(defun helm-raindrop-get-url (page)
  "Return Raindrop.io API endpoint for getting items.
PAGE is a natural number.  If it doesn't set, it equal to 0."
  (format "https://api.raindrop.io/rest/v1/raindrops/%s?%s"
	  helm-raindrop-collection-id
	  (url-build-query-string `((page ,page) (perpage ,helm-raindrop-api-per-page)))))

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

;;; Debug

(defun helm-raindrop-debug-init-session ()
  "Initialize debug session for batch requests."
  (setq helm-raindrop-debug-total-start-time (current-time)
	helm-raindrop-debug-request-count 0))

(defun helm-raindrop-debug-start-request ()
  "Start timing for individual request and increment counter."
  (cl-incf helm-raindrop-debug-request-count)
  (setq helm-raindrop-debug-start-time (current-time)))

(defun helm-raindrop-debug-log-request-success (url)
  "Log successful completion of request.
URL is the request URL."
  (if helm-raindrop-debug-mode
      (message "[Raindrop] Succeed to GET %s (%0.1fsec) at %s."
	       url
	       (time-to-seconds
		(time-subtract (current-time)
			       helm-raindrop-debug-start-time))
	       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

(defun helm-raindrop-debug-log-request-error (url error-thrown)
  "Log error for failed request.
URL is the request URL.
ERROR-THROWN is (ERROR-SYMBOL . DATA), or nil."
  (if helm-raindrop-debug-mode
      (message "[Raindrop] Fail %S to GET %s (%0.1fsec) at %s."
	       error-thrown
	       url
	       (time-to-seconds
		(time-subtract (current-time)
			       helm-raindrop-debug-start-time))
	       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

(defun helm-raindrop-debug-log-session-summary ()
  "Log summary of all requests in the session."
  (when helm-raindrop-debug-mode
    (message "[Raindrop] Total: %d requests completed in %0.1fsec at %s."
	     helm-raindrop-debug-request-count
	     (time-to-seconds
	      (time-subtract (current-time)
			     helm-raindrop-debug-total-start-time))
	     (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

;;; Timer

(defun helm-raindrop-set-timer ()
  "Set timer."
  (setq helm-raindrop-timer
	(run-at-time "0 sec"
		     helm-raindrop-interval
		     #'helm-raindrop-http-request)))

(defun helm-raindrop-cancel-timer ()
  "Cancel timer."
  (when helm-raindrop-timer
    (cancel-timer helm-raindrop-timer)
    (setq helm-raindrop-timer nil)))

;;;###autoload
(defun helm-raindrop-initialize ()
  "Initialize `helm-raindrop'."
  (unless helm-raindrop-access-token
    (error "Variable `helm-raindrop-access-token' is nil"))
  (unless helm-raindrop-collection-id
    (error "Variable `helm-raindrop-collection-id' is nil"))
  (helm-raindrop-set-timer))

(provide 'helm-raindrop)

;;; helm-raindrop.el ends here
