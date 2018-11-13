(require 'strette)

;; The association list representing a log will contains the four keys defined here, plus an
;; additional :message key.
(defvar sample-log-keys '(:time :thread :level :logger))

;; The Regexp used to parse log entries - the regexp matches the log pattern defined in the
;; sample-logback.xml file. Note the usage of four regexp groups which are used to match the
;; values of the four sample-log-keys defined above.
(defvar sample-log-regexp "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}\\) \\[\\([^\]]*\\)\\] \\([^\s]+\\)\s+\\([^\(?: -\)]+\\) - ")

;; The background color of the :level entry
(defun sample-log-level-background (level)
  (cond ((equal level "ERROR") "red")
        ((equal level "WARN") "yellow")
        ((equal level "INFO") "light gray")
        ((equal level "DEBUG") "white")
        (t nil)))

;; Format a single log entry. Note the use of font locks.
(defun sample-log-formatter (log)
  (format "%s [%s] %s %s - %s"
          (propertize (alist-get :time log) 'font-lock-face '(:foreground "dark blue" :weight semi-bold))
          (propertize (alist-get :thread log) 'font-lock-face '(:foreground "dark green" :weight semi-bold))
          (propertize (alist-get :level log) 'font-lock-face `(:underline t :background ,(sample-log-level-background (alist-get :level log))))
          (propertize (alist-get :logger log) 'font-lock-face '(:foreground "saddle brown" :weight semi-bold))
          (alist-get :message log)))

;; This function can filter out logs entries by returning nil. This function is also allowed to
;; transorm the logs.
(defun sample-log-filter (log)
  ;; Filter out logs which do not have an ERROR level
  (when (equal (alist-get :level log) "ERROR")
    ;; Modify the message of ERROR logs
    (strette-alist-set log :message "Modified message")))

(comment
 
 ;; Call strette-start again to refresh the strette buffer after one of the
 ;; parsing / filtering / formatting functions have been redefined
 
 (strette-start (expand-file-name "sample.log")
                "*sample.log*" sample-log-regexp sample-log-keys
                'sample-log-formatter 'sample-log-filter)

 (strette-start (expand-file-name "sample.log")
                "*sample.log*" sample-log-regexp sample-log-keys
                'sample-log-formatter 'sample-log-filter
                ;; Display at most 10 log entries (the default is 100)
                10)

 (strette-start (expand-file-name "sample.log")
                "*sample.log*" sample-log-regexp sample-log-keys
                'sample-log-formatter 'sample-log-filter
                ;; Don't limit the number of displayed log entries
                :no-limit)
 )
