;;;; perlre.asd

(asdf:defsystem #:perlre
  :version "0.5"
  :description "s///, m//, d// - regular expression API for CL-PPCRE and CL-INTERPOL"
  :author "<schatzer.johann@gmail> using idea and code from LET-OVER-LAMBDA"
  :license "BSD Simplified --- the same as let-over-lambda"
  :depends-on (cl-ppcre cl-interpol let-over-lambda prove trivia trivia.ppcre)
  :serial t
  :components ((:file "package")
               (:file "perlre")))

