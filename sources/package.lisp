;;;; -*- mode: lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; package.lisp

(defpackage #:convex-hull
  (:nicknames #:cvh)
  (:use #:cl)
  (:export #:main
           #:perimeter
           #:convex-hull))
