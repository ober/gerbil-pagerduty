;; -*- Gerbil -*-
;;; © ober
;;; Pagerduty client library

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :std/xml/ssax
  :ober/oberlib)

(export #t)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(def version "0.10")

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
    (let ((outs [[ "Incident" "Created At" "Title" "Description" "Team" "Url" ]]))
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
                                         .?title
                                         .?description
                                         (if (table? .?escalation_policy) (let-hash .escalation_policy .summary) #f)
                                         .?html_url ]
                                       outs))))
                  (when .?more
                    (lp (+ offset 100)))))
              (set! outs "")))))
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
                    (set! outs (cons [ .?name
                                       .?email
                                       .?role
                                       (when (pair? .?teams)
                                         (let ((sum []))
                                           (for (t .teams)
                                             (when (table? t)
                                               (let-hash t
                                                 (when .?summary
                                                   (set! sum (cons .summary sum))))))
                                           (if sum
                                             (string-join sum ", "))))
                                       ] outs))))
                (when .?more
                  (lp (+ offset 100))))))))
      (style-output outs))))

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
