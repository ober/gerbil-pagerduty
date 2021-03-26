#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("pagerduty/client"
    (static-exe:
     "pagerduty/pagerduty"
     "-ld-options"
     "-lyaml -lssl -lz -L/usr/local/opt/openssl/lib/ -L/usr/local/lib"
     "-cc-options"
     "-I/usr/local/opt/openssl/include -I/usr/local/include")))
