;;; -*- Gerbil -*-
;;; © ober
;;; Pagerduty client library

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
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

(declare (not optimize-dead-definitions))
(def config-file "~/.pagerduty.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
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
    (let* ((url (format "~a/incidents?since=~a&until=~a&time_zone=~a&limit=200" .url begin end .time_zone))
           (results (do-get-generic url (default-headers .token)))
           (data (from-json results))
           (outs [[ "Incident" "Created At" "Title" "Description" "Team" "Url" ]]))
      (let-hash data
        (for (incident .incidents)
             (let-hash incident
               (set! outs (cons [ .?incident_number
                                  .?created_at
                                  .?title
                                  .?description
                                  (if (table? .?escalation_policy) (let-hash .escalation_policy .summary) #f)
                                  .?html_url ] outs)))))
      (style-output outs))))

(def (incidents)
  (let-hash (load-config)
    (let* ((url (format "~a/incidents?time_zone=UTC&statuses[]=resolved" .url))
	   (results (do-get-generic url (default-headers .token))))
      (displayln results))))

(def (incident id)
  (let-hash (load-config)
    (let* ((url (format "~a/incidents/~a?time_zone=UTC" .url id))
	   (results (do-get-generic url (default-headers .token))))
      (displayln results))))
