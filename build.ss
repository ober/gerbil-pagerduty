#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("pagerduty/client"
    (static-exe:
     "pagerduty/pagerduty"
     "-cc-options"
     "-I/usr/pkg/include"
     "-ld-options"
     "-lpthread -lyaml -ldl -lssl -lz -L/usr/lib64 -L/usr/pkg/lib")))
