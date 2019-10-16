;;; -*- Gerbil -*-
;;; © ober
;;; Pagerduty client binary


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
  :ober/oberlib
  :ober/pagerduty/client)

(export main)

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
