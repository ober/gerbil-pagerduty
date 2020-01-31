;;; -*- Gerbil -*-
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
(def version "0.03")

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

(def (incidents-range begin end)
  "Fetch all incidents between begin and end.
   Date/time format as 2018-09-10T10:12:14 or partial"
  (let-hash (load-config)
    (process-incidents (format "~a/incidents?since=~a&until=~a&time_zone=~a&limit=100" .url begin end .time_zone)) ))

(def (incidents)
  "Fetch all active incidents"
  (let-hash (load-config)
    (process-incidents (format "~a/incidents?time_zone=UTC&statuses[]=resolved" .url))))

(def (process-incidents url)
  (let-hash (load-config)
    (let ((outs [[ "Incident" "Created At" "Title" "Description" "Team" "Url" ]]))
      (let lp ((offset 0))
        (let* ((offset-url (format "~a&offset=~a" url offset))
               (results (do-get-generic offset-url (default-headers .token)))
               (data (from-json results)))
          (dp results)
          (let-hash data
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
              (lp (+ offset 100))))))
      (style-output outs))))

(def (incident id)
  (let-hash (load-config)
    (process-incidents (format "~a/incidents/~a?time_zone=UTC" .url id))))

(def (users)
  (let-hash (load-config)
    (let ((outs [[ "Name" "Email" "Role" "Team" ]]))
      (let lp ((offset 0))
        (let* ((url (format "~a/users?offset=~a&limit=100" .url offset))
               (results (do-get-generic url (default-headers .token)))
               (data (from-json results)))
          (let-hash data
            (for (user .users)
              (dp (hash->list user))
              (let-hash user
                   (set! outs (cons [ .?name
                                      .?email
                                      .?role
                                      (when (table? .?teams)
                                        (let-hash .teams
                                          .summary))
                                      ] outs))))
            (when .?more
              (lp (+ offset 100))))))
      (style-output outs))))

(def (create-user email full-name)
  (let-hash (load-config)
    (let* ((user (json-object->string
                  (hash
                   ("name" full-name)
                   ("email" email)
                   ("password" "lala1234"))))
           (url (format "~a/users" .url))
           (results (do-post url (default-headers .token) user)))
      (displayln results))))
