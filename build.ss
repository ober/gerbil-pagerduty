#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("pagerduty/client"
    (exe:
     "pagerduty/pagerduty"
     "-ld-options"
     "-lyaml"
     )))
