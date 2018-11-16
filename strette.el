;; strette.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2018 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Version 0.1.0-SNAPSHOT
;; Package-Requires: ((emacs "25"))

;; Commentary:

;; Code:

(defconst strette-version "0.0.3")

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; 240000 chars
;; ~ 4-5mb ??
(defcustom strette-batch-size 2400000
  "Maximum batch size when processing large files. This is a number of characters"
  :type 'integer
  :group 'strette)

(defcustom strette-logs-limit 100
  "The maximum number of displayed logs"
  :type 'integer
  :group 'strette)

(defvar strette--proc nil)
(defvar strette--proc-stderr nil)
(defvar strette--last-parsed-log nil)

(defun strette--make-logs (logs-size)
  (if (null logs-size)
      '()
    (make-ring logs-size)))

(defun strette--logs-length (logs)
  (if (ring-p logs)
      (ring-length logs)
    (length logs)))

(defmacro strette--insert-log (logs l)
  `(if (ring-p ,logs)
       (ring-insert ,logs ,l)
     (push ,l ,logs)))

(defmacro strette--init-insert-log (logs l)
  `(if (ring-p ,logs)
       (ring-insert-at-beginning ,logs ,l)
     (push ,l ,logs)))

(defun strette--log-to-remove (logs)
  (when (ring-p logs)
    (when (equal (ring-size logs) (ring-length logs))
      (ring-ref logs (- (ring-size logs) 1)))))

(defmacro strette--logs-init-elements (logs)
  (let ((logs-copy-sym (make-symbol "logs-copy")))
    `(if (ring-p ,logs)
         (nreverse (ring-elements ,logs))
       (let ((,logs-copy-sym (copy-tree ,logs)))
         (setq logs (nreverse ,logs))
         ,logs-copy-sym))))

(defun strette--logs-first (logs)
  (if (ring-p logs)
      (if (ring-empty-p logs)
          nil
        (ring-ref logs 0))
    (car logs)))

(defmacro strette--logs-replace-first (logs new-first)
  `(if (ring-p ,logs)
       (if (ring-empty-p ,logs)
           ,logs
         (progn (ring-remove ,logs 0)
                (ring-insert ,logs ,new-first)))
     (setq ,logs (cdr ,logs))))

(defun strette--count-characters (f-name)
  (with-temp-buffer
    (let ((status (call-process "wc" nil t nil "-c" f-name)))
      (if (equal status 0)
          (let ((wc-string (buffer-substring-no-properties (point-min) (point-max))))
            (save-match-data
              (string-match "\\([0-9]+\\)" wc-string)
              (string-to-number (match-string-no-properties 1 wc-string))))
        (error (buffer-substring-no-properties (point-min) (point-max)))))))

(defun strette--make-log (log-keys match-list message)
  (let ((l (cddr match-list))
        (log-keys log-keys)
        (log `((:message . ,message))))
    (while (and (car l) (car log-keys))
      (push `(,(car log-keys) . ,(buffer-substring-no-properties (car l) (cadr l))) log)
      (setq l (cddr l))
      (setq log-keys (cdr log-keys)))
    log))

(defun strette--insert-file-contents (f-name beg end)
  (cadr (insert-file-contents f-name nil beg end nil)))

(defun strette--batch-start (c-count)
  (max 0 (- c-count strette-batch-size)))

(defun strette--init-logs (f-name log-regexp log-keys log-filter logs-limit)
  (save-match-data
    (let* ((c-count (strette--count-characters f-name))
           (logs (strette--make-logs logs-limit))
           (match-list '(nil nil))
           (match-search-start-point nil))
      (strette--insert-file-contents
       f-name (strette--batch-start c-count) c-count)
      (goto-char (point-max))
      (goto-char (line-beginning-position))
      (skip-chars-backward "\n")
      (setq match-search-start-point (point))
      (while (and (or (null logs-limit) (< (strette--logs-length logs) logs-limit))
                  (> c-count 0))
        (while (and (or (null logs-limit) (< (strette--logs-length logs) logs-limit))
                    (re-search-backward log-regexp nil t))
          (match-data nil match-list nil)
          (let ((message (buffer-substring-no-properties
                          (cadr match-list) match-search-start-point)))
            (when-let (log (strette--make-log log-keys match-list message))
              (setq-local strette--last-parsed-log log)
              (when-let (filtered-log (funcall log-filter log))
                (strette--init-insert-log logs log)))
            (skip-chars-backward "\n")
            (delete-region (point) match-search-start-point)
            (setq match-search-start-point (point))))
        (setq c-count (strette--batch-start c-count))
        (goto-char (point-min))
        (setq match-search-start-point
              (+ match-search-start-point
                 (strette--insert-file-contents
                  f-name (strette--batch-start c-count) c-count)))
        (goto-char match-search-start-point))
      (delete-region (point-min) match-search-start-point)
      logs)))

(defun strette-alist-get (alist k &optional default)
  "Return the value associated with KEY in ALIST. If KEY is not found in ALIST, return DEFAULT. keys are compared using `equal`."
  (cond ((null alist) default)
        ((equal (caar alist) k) (cdar alist))
        (t (strette-alist-get (cdr alist) k default))))

(defun strette-alist-set (alist k v)
  "Non destructively associate the value V with the key K in the alist ALIST. If ALIST already contains K, then its value (the first one if there are multiple K) is changed to V. If ALIST does not contain a key K, then the pair (K . V) is cons-ed to ALIST. Keys are compared using `equal`."
  (cond ((null alist) `((,k . ,v)))
        ((equal (caar alist) k) (cons `(,k . ,v) (cdr alist)))
        (t (cons (car alist) (strette-alist-set (cdr alist) k v)))))

(defun strette--remove-log-text (buffer log)
  (when log
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (let* ((maybe-log-start (when (eq (get-text-property (point-min) 'strette-log) log)
                                    (point-min)))
                 (property-change-pos (next-single-property-change (point-min) 'strette-log))
                 (maybe-log-start (if maybe-log-start
                                      maybe-log-start
                                    (when (and property-change-pos
                                               (eq
                                                (get-text-property property-change-pos 'strette-log)
                                                log))
                                      property-change-pos)))
                 (maybe-log-end (when maybe-log-start
                                  (next-single-property-change maybe-log-start 'strette-log))))
            (when (and maybe-log-start maybe-log-end)
              (goto-char maybe-log-end)
              (skip-chars-forward "\n")
              (delete-region maybe-log-start (point)))))
        (restore-buffer-modified-p nil)))))

(defun strette--remove-log-text-backward (buffer log)
  (when log
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (let* ((p-max (max 1 (- (point-max) 1)))
                 (maybe-log-end (when (eq (get-text-property p-max 'strette-log) log)
                                  (+ p-max 1)))
                 (property-change-pos (previous-single-property-change p-max 'strette-log))
                 (maybe-log-end (if maybe-log-end
                                    maybe-log-end
                                  (when (and property-change-pos
                                             (eq
                                              (get-text-property property-change-pos 'strette-log)
                                              log))
                                    property-change-pos)))
                 (maybe-log-start (when maybe-log-end
                                    (or
                                     (previous-single-property-change maybe-log-end 'strette-log)
                                     (point-min)))))
            (when (and maybe-log-start maybe-log-end)
              (goto-char maybe-log-end)
              (skip-chars-forward "\n")
              (delete-region maybe-log-start (point)))))
        (restore-buffer-modified-p nil)))))

(defmacro strette--save-excursion-or-move-forward (&rest body)
  (let ((result-sym (make-symbol "result")))
    `(progn
       (if (equal (point) (point-max))
           (let ((,result-sym (progn ,@body)))
             (goto-char (point-max))
             (when-let (w (get-buffer-window nil t))
               (set-window-point w (point-max)))
             ,result-sym)
         (save-excursion
           ,@body)))))

(defun strette--append-message (buffer logs log-formatter log-filter message-tail)
  (when strette--last-parsed-log
    (let* ((last-displayed-log (strette--logs-first logs))
           (log-message (alist-get :message strette--last-parsed-log))
           (updated-log (strette-alist-set strette--last-parsed-log
                                           :message (concat log-message message-tail)))
           (filtered-log (funcall log-filter updated-log)))
      (setq-local strette--last-parsed-log updated-log)
      (with-current-buffer buffer
        (strette--save-excursion-or-move-forward
         (let* ((inhibit-read-only t))
           (when (eq strette--last-parsed-log last-displayed-log)
             (strette--remove-log-text-backward buffer last-displayed-log))
           (when filtered-log
             (strette--logs-replace-first logs updated-log)
             (goto-char (point-max))
             (insert (propertize (funcall log-formatter filtered-log)
                                 'strette-log updated-log)))
           (restore-buffer-modified-p nil)))))))

(defun strette--parse-new-logs
    (working-buffer buffer logs log-regexp log-keys
                    log-formatter log-filter string)
  (with-current-buffer working-buffer
    (goto-char (point-max))
    (insert string)
    (goto-char (point-min))
    (let ((match-list '(nil nil)))
      ;; initial match - update the last log and remove the trailing message
      (if (re-search-forward log-regexp nil t)
          (progn
            (match-data nil match-list nil)
            (goto-char (car match-list))
            (skip-chars-backward "\n")
            (delete-region (point) (car match-list))
            (when (not (equal (point-min) (point)))
              (strette--append-message buffer logs log-formatter log-filter
                                      (buffer-substring-no-properties (point-min) (point)))
              (delete-region (point-min) (point)))
            ;; intermediate matches
            (let ((log-start (car match-list))
                  (message-start (cadr match-list)))
              (goto-char message-start)
              (while (re-search-forward log-regexp nil t)
                (let ((log-without-message (strette--make-log log-keys match-list nil)))
                  (match-data nil match-list nil)
                  (goto-char (car match-list))
                  (skip-chars-backward "\n")
                  (let* ((message (buffer-substring-no-properties message-start (point)))
                         (log (when log-without-message (strette-alist-set log-without-message
                                                                           :message message)))
                         (filtered-log (when log (funcall log-filter log))))
                    (delete-region log-start (car match-list))
                    (when log (setq-local strette--last-parsed-log log))
                    (when filtered-log
                      (strette--remove-log-text buffer (strette--log-to-remove logs))
                      (strette--insert-log logs log)
                      (with-current-buffer buffer
                        (strette--save-excursion-or-move-forward
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (insert "\n")
                            (insert (propertize (funcall log-formatter filtered-log)
                                                'strette-log log))
                            (restore-buffer-modified-p nil)))))
                    (setq log-start (car match-list))
                    (setq message-start (cadr match-list))
                    (goto-char message-start))))
              ;; last match - the message may be partial
              (goto-char (point-max))
              (skip-chars-backward "\n")
              (let* ((message (buffer-substring-no-properties message-start (point)))
                     (log (strette--make-log log-keys match-list message))
                     (filtered-log (when log (funcall log-filter log))))
                (delete-region log-start (point))
                (when log (setq-local strette--last-parsed-log log))
                (when filtered-log
                  (strette--remove-log-text buffer (strette--log-to-remove logs))
                  (strette--insert-log logs log)
                  (with-current-buffer buffer
                    (strette--save-excursion-or-move-forward
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert "\n")
                        (insert (propertize (funcall log-formatter filtered-log) 'strette-log log))
                        (restore-buffer-modified-p nil))))))))
        ;; no match at all - update the last log and remove the trailing message
        (progn
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (when (not (equal (point-min) (point-max)))
            (strette--append-message buffer logs log-formatter log-filter
                                     (buffer-substring-no-properties (point-min) (point)))
            (delete-region (point-min) (point))))))))

(defun strette--proc-filter (buffer logs log-regexp log-keys
                                   log-formatter log-filter proc string)
  (condition-case err
      (let ((working-buffer (process-buffer proc)))
        (when (and (buffer-live-p working-buffer)
                   (buffer-live-p buffer))
          (strette--parse-new-logs
           working-buffer buffer logs log-regexp log-keys
           log-formatter log-filter string)))
    (error
     (when (process-live-p proc)
       (kill-process proc))
     (signal (car err) (cdr err)))))

(defun strette--kill-working-buffer (working-buffer)
  (when-let (proc (get-buffer-process working-buffer))
    (when (process-live-p proc)
      (kill-process proc))
    (set-process-buffer proc nil))
  (when (buffer-live-p working-buffer)
    (kill-buffer working-buffer)))

(defun strette--kill-buffer-hook ()
  (when strette--proc-stderr 
    (let ((buffer (process-buffer strette--proc-stderr)))
      (when (process-live-p strette--proc-stderr)
        (delete-process strette--proc-stderr))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer))))
  (when strette--proc
    (when-let (working-buff (process-buffer strette--proc))
      (strette--kill-working-buffer working-buff))))

(defun strette--proc-sentinel (buffer process event)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (strette--kill-buffer-hook)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n\n%s" event))
        (restore-buffer-modified-p nil)))))

(defun strette-start (f-name buffer-name log-regexp log-keys log-formatter log-filter
                             &optional logs-limit)
  "Watch the log file F-NAME and display its content in a buffer named BUFFER-NAME.

If BUFFER-NAME already exists and is already a strette buffer, then it is cleared and reused. If BUFFER-NAME already exists and is not a strette buffer, an error is thrown. If BUFFER-NAME does not exist, it is created.

Each line of the F-NAME file is matched against the LOG-REGEXP regexp and matching lines are reified into an association list with the following format:
`((key1 . match-group1)
  (key2 . match-group2)
  ...
  (keyN . match-groupN)
  (:message . message-text))
where the keys are the elements of the LOG-KEYS list and the match-groups are the groups defined in the LOG-REGEXP. The reified log always contains a :message entry which value is the text that didn't match the regexp, including the lines found between two regexp match.

LOG-FORMATTER is a function which only argument is a log. LOG-FORMATTER must return a string which is the serialized representation of the log.

LOG-FILTER is a function which only argument is a log. LOG-FILTER must return a log which will be output into the BUFFER-NAME buffer. LOG-FILTER can return nil to exclude a log from the output buffer. LOG-FILTER is allowed to modify the values associated with the log keys.

LOGS-LIMIT is an optional parameter representing the maximum number of logs written to the output buffer. Its default value is 100. Passing nil as a LOGS-LIMIT will limit the number of logs to 100. The LOGS-LIMIT can be disabled by passing the value :no-limit.

Additional considerations:
 - Every log entry in the F-NAME log file is expected to start after a newline character.
 - The LOG-FILTER can be called multiple times with a same log parameter and as such, must be free of side effects. The LOG-FILTER function can be called with a log parameter which :message value is truncated but the LOG-FILTER is guaranteed to be called at least once with its full :message value. Strette will take care of writing a same log only once in the output buffer."
  (add-hook 'kill-buffer-hook 'strette--kill-buffer-hook)
  (let* ((f-name (expand-file-name f-name))
         (logs-limit (if (equal logs-limit :no-limit)
                         nil
                       (or logs-limit strette-logs-limit)))
         (buffer (get-buffer buffer-name))
         (proc-name (format "strette*%s" f-name))
         (proc-name-stderr (format " strette*stderr*%s" f-name))
         (display-window nil))
    (when (and buffer (buffer-live-p buffer)) 
      (with-current-buffer buffer
        (if (null strette--proc)
            (user-error "buffer %s already exists" (buffer-name buffer))))
      (setq display-window (get-buffer-window buffer))
      (kill-buffer buffer))
    (let ((buffer (generate-new-buffer buffer-name))
          (working-buffer (generate-new-buffer (concat " " proc-name))))
      (condition-case err
          (let ((logs (with-current-buffer working-buffer
                        (strette--init-logs
                         f-name log-regexp log-keys log-filter logs-limit))))
            (with-current-buffer buffer
              (buffer-disable-undo)
              (read-only-mode 1)
              (font-lock-mode 1)
              (let ((inhibit-read-only t)
                    (first-log t))
                (dolist (log (strette--logs-init-elements logs))
                  (if first-log
                      (setq first-log nil)
                    (insert "\n"))
                  (let* ((filtered-log (funcall log-filter log))
                         (log-string (funcall log-formatter filtered-log)))
                    (insert (propertize log-string 'strette-log log)))))
              (restore-buffer-modified-p nil))
            ;; Update the logs with the line skipped in the init function
            (strette--parse-new-logs working-buffer buffer logs log-regexp log-keys
                                     log-formatter log-filter "")
            (with-current-buffer buffer
              (let* ((proc-stderr (make-pipe-process :name proc-name-stderr
                                                     :buffer nil
                                                     :sentinel (apply-partially
                                                                'strette--proc-sentinel
                                                                buffer)
                                                     :filter (apply-partially
                                                              'strette--proc-sentinel
                                                              buffer)))
                     (proc (make-process :name proc-name
                                         :buffer working-buffer
                                         :command (list "tail" "-n0" "-f" f-name)
                                         :filter (apply-partially
                                                  'strette--proc-filter
                                                  buffer logs log-regexp log-keys
                                                  log-formatter log-filter)
                                         :sentinel (apply-partially 'strette--proc-sentinel buffer)
                                         :stderr proc-stderr)))
                (setq-local strette--proc proc)
                (setq-local strette--proc-stderr proc-stderr))
              (goto-char (point-max)))
            (if display-window
                (with-selected-window display-window
                  (switch-to-buffer buffer))
              (pop-to-buffer buffer))
            buffer)
        (error
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (strette--kill-working-buffer working-buffer)
         (signal (car err) (cdr err)))))))

(comment
 (file-name-nondirectory (expand-file-name "~/strette.el/test.log"))
 
 (generate-new-buffer "*strette-test*")

 (defvar strette-batch-size 6)

 (let ((f-name (expand-file-name "~/strette.el/test.log"))
       (logs-limit 1)
       (log-regexp "^\\([0-9]\\{4\\}\\) ")
       (log-keys '(:time))
       (log-formatter (lambda (log)
                        (format "%s %s"
                                (alist-get :time log)
                                (alist-get :message log))))
       (log-filter (lambda (log) log)))

   (strette-start f-name "*test.log*" log-regexp log-keys log-formatter log-filter))

 (list-processes)

 )

(provide 'strette)

;; match-data -> buffer markers vs integer -> marker positions are updated when deleting
;; content from the buffer. reseat -> markers in the previous match list are invalidated
;; (not really what we want)

;; We expect every log message to start after a newline character

;; strette--save-excursion-or-move-forward - invisible frames

;; byte-recompile to check warnings ----  C-u 0 M-x byte-recompile-directory
