;; -*- Gerbil -*-
package: pagerduty
namespace: pagerduty
(export main)

(declare (not optimize-dead-definitions))
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
  )

(def config-file "~/.pagerduty.yaml")
(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(def program-name "pagerduty")
(def DEBUG (getenv "DEBUG" #f))
(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def interactives
  (hash
   ("create" (hash (description: "Return information on incident") (usage: "create <title> <message>") (count: 2)))
   ("incident" (hash (description: "Return information on incident") (usage: "incident <incident number>") (count: 1)))
   ("incidents" (hash (description: "Show all open incidents") (usage: "incidents") (count: 0)))
   ("incidents-range" (hash (description: "Show all open incidents for range") (usage: "incidents-range <begin date/time> <end date/time>") (count: 2)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "pagerduty#" verb))) args2))))

(def (load-config)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    config))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Usage: pagerduty <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (let* ((reply (http-post uri
			   headers: headers
			   data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      text
      (format "Failure on post. Status:~a Text:~a~%" status text))))

(def (do-get uri)
  (let* ((reply (http-get uri))
	 (status (request-status reply))
	 (text (request-text reply)))
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (from-json json)
  ;;(try
  (with-input-from-string json read-json))
;;   (catch (e)
;;     (displayln "error parsing json " e))))

(def (hash->str h)
  (let ((results '()))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      ;;        (pregexp-replace "\n" (append-strings results) "\t"))
      "N/A")))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))


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
           (data (from-json results)))
      (displayln "| Incident | Created At| Title | Description | Team | Url |")
      (displayln "|----------|")
      (let-hash data
        (for (incident .incidents)
             (let-hash incident
               (displayln "|" .?incident_number
                          "|" .?created_at
                          "|" .?title
                          "|" .?description
                          "|" (if (table? .?escalation_policy) (let-hash .escalation_policy .summary) #f)
                          "|" .?html_url)))))))

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

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
	(do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
	(if (string=? "" data)
	  (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
	  (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
	(displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
	(displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "delete")
	(displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' ~a" heads data uri)))
       (else
	(displayln "unknown format " type))))))
