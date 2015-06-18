;;;; perlre.asd

(asdf:defsystem #:perlre
  :version "0.2"
  :description "perl regular expression api - m// and s/// - for CL-PPCRE with CL-INTERPOL support"
  :author "<schatzer.johann@gmail> using idea and code from LET-OVER-LAMBDA"
  :license "BSD Simplified --- the same as let-over-lambda"
  :depends-on (cl-ppcre cl-interpol let-over-lambda prove)
  :serial t
  :components ((:file "package")
               (:file "perlre")))

