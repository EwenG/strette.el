;; strette.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2021 Ewen Grosjean

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

;; Version 0.4.0-SNAPSHOT
;; Package-Requires: ((emacs "27"))

;; Commentary:

;; Code:

(defconst strette-version "0.0.4-SNAPSHOT")

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
(defvar strette--last-displayed-log-id nil)

;; When passing a buffer to strette-start, this must be a buffer local
;; which value is the name of the log file. This is what makes a buffer
;; a so called "strette buffer"
(defvar strette-f-name nil)
;; The number of logs displayed in a strette buffer
(defvar strette-logs-count 0)
;; buffer local, set in the working buffer
(defvar strette-logs-ids-counter 0)

(defun strette--make-logs (logs-size)
  (if (null logs-size)
      '()
    (make-ring logs-size)))


(defun strette--logs-length (buffer)
  (with-current-buffer buffer
    strette-logs-count))


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

(defun strette--make-log (log-id log-keys match-list message)
  (let ((l (cddr match-list))
        (log-keys log-keys)
        (log `()))
    (while (and (car l) (car log-keys))
      (push `(,(car log-keys) . ,(buffer-substring-no-properties (car l) (cadr l))) log)
      (setq l (cddr l))
      (setq log-keys (cdr log-keys)))
    (push `(:message . ,message) log)
    (push `(:log-id . ,log-id) log)
    log))

(defun strette--insert-file-contents (f-name beg end)
  (cadr (insert-file-contents f-name nil beg end nil)))

(defun strette--batch-start (c-count)
  (max 0 (- c-count strette-batch-size)))

(defun strette--insert-log-init (buffer logs-limit log-id formatted-log)
  (with-current-buffer buffer
    (when (or (null logs-limit) (< strette-logs-count logs-limit))
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert (propertize formatted-log 'strette-log-id log-id))
        (insert (propertize "\n" 'strette-log-id log-id))
        (setq-local strette-logs-count (+ 1 strette-logs-count))
        (when (null strette--last-displayed-log-id)
          (setq-local strette--last-displayed-log-id log-id))
        (restore-buffer-modified-p nil)))))

(defun strette--insert-log (logs-limit log-id formatted-log)
  (while (and logs-limit (> strette-logs-count 0) (>= strette-logs-count logs-limit))
    (strette--remove-first-log-text))
  (when (or (null logs-limit) (< strette-logs-count logs-limit))
    (goto-char (point-max))
    (insert (propertize formatted-log 'strette-log-id log-id))
    (insert (propertize "\n" 'strette-log-id log-id))
    (setq-local strette-logs-count (+ 1 strette-logs-count))
    (setq-local strette--last-displayed-log-id log-id)))

;; Search backward since we don't want to search past the number of log-entries limit
(defun strette--init-logs (f-name buffer log-regexp log-keys log-formatter logs-limit)
  (save-match-data
    (let* ((c-count (strette--count-characters f-name))
           (match-list '(nil nil))
           (match-search-start-point nil))
      (strette--insert-file-contents
       f-name (strette--batch-start c-count) c-count)
      (goto-char (point-max))
      ;; Skip the last line since it may be partial
      (goto-char (line-beginning-position))
      (skip-chars-backward "\n")
      (setq match-search-start-point (point))
      (while (and (or (null logs-limit) (< (strette--logs-length buffer) logs-limit))
                  (> c-count 0))
        (while (and (or (null logs-limit) (< (strette--logs-length buffer) logs-limit))
                    (re-search-backward log-regexp nil t))
          (match-data nil match-list nil)
          (let* ((message (buffer-substring-no-properties
                           (cadr match-list) match-search-start-point))
                 (log (strette--make-log strette-logs-ids-counter log-keys match-list message)))
            (setq-local strette-logs-ids-counter (- strette-logs-ids-counter 1))
            (when log 
              (when (null strette--last-parsed-log)
                (setq-local strette--last-parsed-log log))
              (when-let (log-str (funcall log-formatter log))
                (strette--insert-log-init buffer logs-limit (alist-get :log-id log) log-str)))
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
      (setq-local strette-logs-ids-counter 1)
      (delete-region (point-min) match-search-start-point)
      (current-buffer))))

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

(defun strette--remove-first-log-text ()
  (let* ((maybe-log-start (if (get-text-property (point-min) 'strette-log-id)
                              (point-min)
                            (next-single-property-change (point-min) 'strette-log-id)))
         (maybe-log-end (when maybe-log-start
                          (next-single-property-change maybe-log-start 'strette-log-id))))
    (when (and maybe-log-start maybe-log-end)
      (goto-char maybe-log-end)
      (skip-chars-forward "\n")
      (delete-region maybe-log-start (point))
      (setq strette-logs-count (- strette-logs-count 1)))))

(defun strette--remove-log-text-backward (log-id)
  (when log-id
    (let* ((log-end (max 1 (- (point-max) 1)))
           (maybe-log-start (or (previous-single-property-change log-end 'strette-log-id)
                                (point-min)))
           (maybe-log-start (when (and maybe-log-start
                                       (eq (get-text-property maybe-log-start 'strette-log-id)
                                           log-id))
                              maybe-log-start)))
      (when maybe-log-start
        (goto-char log-end)
        (skip-chars-forward "\n")
        (delete-region maybe-log-start (point))
        (setq strette-logs-count (- strette-logs-count 1))))))

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

(defun strette--append-message (message-tail)
  (when (and strette--last-parsed-log message-tail (not (equal message-tail "")))
    (let* ((log-message (alist-get :message strette--last-parsed-log))
           (log-id (alist-get :log-id strette--last-parsed-log))
           (updated-log (strette-alist-set strette--last-parsed-log
                                           :message (concat log-message message-tail))))
      (setq-local strette--last-parsed-log updated-log)
      t)))

(defun strette--insert-last-parsed-log (buffer logs-limit log-formatter message-tail)
  (when strette--last-parsed-log
    (let* ((last-parsed-log-updated-p (strette--append-message message-tail))
           (last-parsed-log strette--last-parsed-log)
           (log-id (alist-get :log-id last-parsed-log)))
      (with-current-buffer buffer
        (strette--save-excursion-or-move-forward
         (let* ((inhibit-read-only t)
                (match-ids-p (equal strette--last-displayed-log-id log-id)))
           (when (and match-ids-p last-parsed-log-updated-p)
             (strette--remove-log-text-backward log-id))
           (when (or (null match-ids-p)
                     (and match-ids-p last-parsed-log-updated-p))
             (let* ((log-str (funcall log-formatter last-parsed-log)))
               (when log-str
                 (strette--insert-log logs-limit log-id log-str))))
           (restore-buffer-modified-p nil)))))))

(defun strette--parse-new-logs-insert (buffer logs-limit log-formatter point-start)
  (setq point-start (+ point-start 0))
  (goto-char point-start)
  (let ((skiped-backward (skip-chars-backward "\n")))
    (delete-region (point) point-start)
    (setq point-start (+ point-start skiped-backward)))
  (strette--insert-last-parsed-log
   buffer logs-limit log-formatter
   (buffer-substring-no-properties (point-min) point-start))
  (delete-region (point-min) point-start))

(defun strette--parse-new-logs
    (working-buffer buffer logs-limit log-regexp log-keys
                    log-formatter string)
  (with-current-buffer working-buffer
    (goto-char (point-max))
    (insert string)
    (goto-char (point-min))
    (while (null (equal (line-end-position) (point-max)))
      (let* ((match-list '(nil nil)))
        (if (re-search-forward log-regexp (line-end-position) t)
            (progn
              (match-data nil match-list nil)
              (let* ((log-start (car match-list))
                     (message-start (cadr match-list)))
                (strette--parse-new-logs-insert buffer logs-limit log-formatter log-start)
                (let ((log-without-message (strette--make-log
                                            strette-logs-ids-counter log-keys match-list nil)))
                  (setq-local strette-logs-ids-counter (+ 1 strette-logs-ids-counter))
                  (setq-local strette--last-parsed-log log-without-message)
                  (delete-region (point-min) message-start))))
          (forward-line 1))))
    (strette--parse-new-logs-insert buffer logs-limit log-formatter
                                    (line-beginning-position))))

(defun strette--proc-filter (buffer logs-limit log-regexp log-keys
                                    log-formatter proc string)
  (condition-case err
      (let ((working-buffer (process-buffer proc)))
        (when (and (buffer-live-p working-buffer)
                   (buffer-live-p buffer))
          (strette--parse-new-logs
           working-buffer buffer logs-limit log-regexp log-keys
           log-formatter string)))
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
      (strette--kill-working-buffer working-buff)))
  (setq-local strette--proc nil)
  (setq-local strette--proc-stderr nil)
  (setq-local strette--last-parsed-log nil))

(defun strette--proc-sentinel (buffer process event)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (null (process-live-p strette--proc))
        (strette--kill-buffer-hook)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n\n%s" event))
          (restore-buffer-modified-p nil))))))

(defun strette-start (f-name-or-buffer log-regexp log-keys log-formatter &optional logs-limit)
  "If F-NAME-OR-BUFFER is the path to a log file, then watch the log file F-NAME-OR-BUFFER and display its content in a buffer.
The buffer name is generated from F-NAME-OR-BUFFER.

If F-NAME-OR-BUFFER is a strette buffer (typically a buffer returned by the strette API), then it is cleared and reused. If F-NAME-OR-BUFFER is a buffer but is not a strette buffer, then an error is thrown.

Each line of the log file is matched against the LOG-REGEXP regexp and matching lines are parsed into an association list with the following format:
`((key1 . match-group1)
  (key2 . match-group2)
  ...
  (keyN . match-groupN)
  (:message . message-text)
  (:log-id . log-id))
where the keys are the elements of the LOG-KEYS list and the match-groups are the groups defined in the LOG-REGEXP. The parsed logs always contains a :message entry which value is the rest of the text that didn't match the regexp. The :log-id is generated by strette.

LOG-FORMATTER is a function which only argument is a log. LOG-FORMATTER must return a string which is the serialized representation of the log. LOG-FORMATTER can return nil to exclude a log from the output buffer.

LOGS-LIMIT is an optional parameter representing the maximum number of logs written to the output buffer. Its default value is 100. Passing nil as a LOGS-LIMIT will limit the number of logs to 100. The LOGS-LIMIT can be disabled by passing the value :no-limit.

Additional considerations:
 - Every log entry in the log file is expected to end by a newline character.
 - The LOG-FORMATTER can be called multiple times with a same log parameter and as such, must be free of side effects. The LOG-FORMATTER function can be called with a log parameter which :message value is truncated but the LOG-FORMATTER is guaranteed to be called at least once with its full :message value. Strette will take care of writing a same log only once in the output buffer."
  (add-hook 'kill-buffer-hook 'strette--kill-buffer-hook)
  (when (null f-name-or-buffer)
    (user-error "f-name-or-buffer is nil"))
  (let* ((f-name (if (buffer-live-p f-name-or-buffer)
                     (with-current-buffer f-name-or-buffer
                       (if (null strette-f-name)
                           (user-error "strette-f-name is nil")
                         (expand-file-name strette-f-name)))
                   (expand-file-name f-name-or-buffer)))
         (buffer (if (buffer-live-p f-name-or-buffer)
                     f-name-or-buffer
                   (let* ((b-name (concat "*" f-name-or-buffer "*")))
                     (get-buffer b-name))))
         (b-name (if (buffer-live-p f-name-or-buffer)
                     (buffer-name buffer)
                   (concat "*" f-name-or-buffer "*")))
         (logs-limit (if (equal logs-limit :no-limit)
                         nil
                       (or logs-limit strette-logs-limit)))
         (proc-name (format "strette*%s" f-name))
         (proc-name-stderr (format " strette*stderr*%s" f-name))
         (display-window nil))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (if (null strette-f-name)
            (user-error "buffer %s already exists" b-name)))
      (setq display-window (get-buffer-window buffer))
      (with-current-buffer buffer
        (strette--kill-buffer-hook)))
    (let* ((buffer (if (buffer-live-p buffer)
                       buffer
                     (generate-new-buffer b-name)))
           (working-buffer (generate-new-buffer (concat " " proc-name))))
      (with-current-buffer buffer
        (setq-local strette-f-name f-name)
        (setq-local strette-logs-count 0)
        (buffer-disable-undo)
        (read-only-mode 1)
        (font-lock-mode 1)
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (condition-case err
          (progn (with-current-buffer working-buffer
                   (setq-local strette-logs-ids-counter 0)
                   (strette--init-logs
                    f-name buffer log-regexp log-keys log-formatter logs-limit))
                 ;; Update the logs with the line skipped in the init function
                 (strette--parse-new-logs working-buffer buffer logs-limit
                                          log-regexp log-keys log-formatter "")
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
                                                       buffer logs-limit log-regexp log-keys log-formatter)
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
                                (alist-get :message log)))))

   (strette-start f-name "*test.log*" log-regexp log-keys log-formatter))

 (list-processes)

 )

(provide 'strette)

;; match-data -> buffer markers vs integer -> marker positions are updated when deleting
;; content from the buffer. reseat -> markers in the previous match list are invalidated
;; (not really what we want)

;; We expect every log message to end by a newline character

;; strette--save-excursion-or-move-forward - invisible frames

;; byte-recompile to check warnings ----  C-u 0 M-x byte-recompile-directory
