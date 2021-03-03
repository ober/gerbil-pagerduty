;;; -*- Gerbil -*-
;;; © ober
;;; Pagerduty client binary

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
  :ober/oberlib
  :ober/pagerduty/client)

(export main)

(def interactives
  (hash
   ("create-user" (hash (description: "Create New User") (usage: "create-user <email-address> <full name>") (count: 2)))
   ("incident" (hash (description: "Return information on incident") (usage: "incident <incident number>") (count: 1)))
   ("incidents" (hash (description: "Show all open incidents") (usage: "incidents") (count: 0)))
   ("incidents-last-day" (hash (description: "List incidents from last 24 hours") (usage: "incident-last-day") (count: 0)))
   ("incidents-last-hour" (hash (description: "List incidents from last hour") (usage: "incident-last-hour") (count: 0)))
   ("incidents-last-secs" (hash (description: "Return information on incidents during last secs") (usage: "incident-last-secs <seconds>") (count: 1)))
   ("incidents-last-week" (hash (description: "Return information on incidents for last week") (usage: "incident-last-week <seconds>") (count: 0)))
   ("incidents-range" (hash (description: "Show all open incidents for range") (usage: "incidents-range <begin YYYYMMDDT00> <end YYYYMMDDT00> ex: pagerduty incidents-range 20200301T00 20200308T00") (count: 2)))
   ("report" (hash (description: "Generate Incidents report from definition in ~/.pagerduty.yaml") (usage: "report <name>") (count: 1)))
   ("users" (hash (description: "List all users in your Pagerduty Account") (usage: "users") (count: 0)))
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
      (apply (eval (string->symbol (string-append "ober/pagerduty/client#" verb))) args2))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln (format "Pagerduty: version ~a" version))
  (displayln "Usage: pagerduty <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))
