;; -*- Gerbil -*-
;;; © ober
;;; Pagerduty client library

(import
  :gerbil/gambit
  :ober/oberlib
  :std/crypto/cipher
  :std/error
  :std/format
  :std/generic/dispatch
  :std/iter
  :std/logger
  :std/misc/list
  :std/net/websocket
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/yaml)

(export #t)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(def version "0.11")

(declare (not optimize-dead-definitions))

(def config-file "~/.pagerduty.yaml")
(def program-name "pagerduty")

(def (load-config)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    config))

(def (default-headers token)
  [["Accept" :: "application/vnd.pagerduty+json;version=2" ]
   ["Content-type" :: "application/json"]
   ["Authorization" :: (format "Token token=~a" token) ]])

(def (incidents-last-secs secs-ago)
  "Fetch incidents from (now - secs) to now."
  (let* ((secs (any->int secs-ago))
         (start (date->string (epoch->date (float->int (- (time->seconds (builtin-current-time)) secs))) "~Y-~m-~dT~H:~M:~S~z"))
         (end (date->string (epoch->date (float->int (time->seconds (builtin-current-time)))) "~Y-~m-~dT~H:~M:~S~z")))
    (incidents-range start end)))

(def (incidents-last-day)
  "Fetch incidents for last 24 hours"
  (incidents-last-secs (* 24 3600)))

(def (incidents-last-hour)
  "Fetch incidents for last hour"
  (incidents-last-secs 3600))

(def (incidents-last-week)
  "Fetch incidents for last hour"
  (incidents-last-secs (* 7 24 3600)))

(def (incidents-range begin end)
  "Fetch all incidents between begin and end.
   Date/time format as 2018-09-10T10:12:14 or partial"
  (let-hash (load-config)
    (process-incidents (format "~a/incidents?since=~a&until=~a&time_zone=~a&limit=100" .url begin end .time_zone))))

(def (incidents)
  "Fetch all active incidents"
  (let-hash (load-config)
    (process-incidents (format "~a/incidents?time_zone=UTC&statuses[]=resolved" .url))))

(def (process-incidents url)
  (let-hash (load-config)
    (let ((outs [[ "Incident" "Created At" "Title" "Team" "Url" ]]))
      (let lp ((offset 0))
        (let (offset-url (format "~a&offset=~a" url offset))
          (with ([ status body ] (rest-call 'get offset-url (default-headers .token)))
            (unless status
              (error body))
            (if (table? body)
              (let-hash body
                (when (and .?incidents
                           (list? .?incidents))
                  (for (incident .incidents)
                    (let-hash incident
                      (set! outs (cons [ .?incident_number
                                         .?created_at
                                         (lines-to-spaces .?title)
                                         (if (table? .?escalation_policy) (let-hash .escalation_policy .summary) #f)
                                         .?html_url ]
                                       outs))))
                  (when .?more
                    (lp (+ offset 100)))))
              (begin(
                (set! outs "")
                (pi body))
              ))))
      (style-output outs))))

(def (incident id)
  (let-hash (load-config)
    (process-incidents (format "~a/incidents/~a?time_zone=UTC" .url id))))

(def (users)
  (let-hash (load-config)
    (let ((outs [[ "Name" "Email" "Role" "Team" ]]))
      (let lp ((offset 0))
        (let (url (format "~a/users?offset=~a&limit=100" .url offset))
          (with ([ status body ] (rest-call 'get url (default-headers .token)))
            (unless status
              (error body))
            (when (table? body)
              (let-hash body
                (for (user .users)
                  (let-hash user
                    (let ((teams (collect-string .?teams 'summary)))
                      (set! outs (cons [ .?name
                                         .?email
                                         .?role
                                         teams
                                         ] outs)))))
                (when .?more
                  (lp (+ offset 100))))))))
      (style-output outs))))

(def (collect-string items element)
  (when (pair? items)
    (let ((sum ""))
      (for (t items)
        (when (table? t)
          (let-hash t
            (let ((value (hash-ref t element))
                  (pre (if (> (string-length sum) 0)
                         ", "
                         "")))
              (when value
                (set! sum (string-append sum (format "~a~a"  pre value))))))))
      (if sum
        (pregexp-replace* ", $" sum "")
        "None"))))

(def (create-user email full-name)
  (let-hash (load-config)
    (let ((user (json-object->string
                 (hash
                  ("name" full-name)
                  ("email" email)
                  ("password" "lala1234"))))
          (url (format "~a/users" .url)))
      (with ([ status body ] (rest-call 'post url (default-headers .token)))
        (unless status
          (error body))
        (present-item body)))))

;; (def (converge-template template metas project)
;;   (if (not (table? template))
;;     (error-print "Not a table")
;;     (hash
;;      (assignee (interpol-from-env (hash-get template "assignee")))
;;      (description (interpol-from-env (hash-get template "description")))
;;      (duedate (interpol-from-env (hash-get template "duedate")))
;;      (issuetype (get-issuetype-id (interpol-from-env (hash-get template "issuetype")) metas))
;;      (labels [(interpol-from-env (hash-get template "labels"))])
;;      (originalestimate (interpol-from-env (hash-get template "estimate")))
;;      (priority (interpol-from-env (hash-get template "priority")))
;;      (project (interpol-from-env (hash-get template "project")))
;;      (summary (interpol-from-env (hash-get template "summary"))))))

;; (def (execute-template template metas project)
;;   (if (not (table? template))
;;     (begin
;;       (displayln "Error: execute-template passed non-table :"  template)
;;       (exit 2)))
;;   (let ((converged (converge-template template metas project)))
;;     (let-hash converged
;;       (let ((parent2 (create-issue (get-project-id .project metas)
;;                                    .summary
;;                                    .issuetype
;;                                    .assignee
;;                                    .priority
;;                                    .labels
;;                                    .originalestimate
;;                                    .description
;;                                    .duedate
;;                                    parent))
;;             (subtasks (hash-get template "subtasks")))
;;         (when subtasks
;;           (for (subtask subtasks)
;;             (execute-template subtask metas projects parent2)))
;;         (unless parent
;;           (displayln "Primary issue: " parent2))))))

;; (def (report creation)
;;   (let-hash (load-config)
;;     (if .?reports
;;       (let ((report (hash-get .reports report)))
;;         (if report
;;           (execute-template report metas .project-key #f)
;;           (begin
;;             (displayln "Error: could not find an entry for " report " in your ~/.pagerduty.yaml under the reports block")
;;             (exit 2))))
;;       (displayln "Error: no create templates defined in ~/.pagerduty.yaml under creations"))))
