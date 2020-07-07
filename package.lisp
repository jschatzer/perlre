;;;; package.lisp

(defpackage #:perlre
  (:nicknames pre)
  (:use #:cl)
  (:export |#~-reader| ifmatch whenmatch match))

