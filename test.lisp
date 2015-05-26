(defpackage test 
  (:use cl prove perlre))

;adapt tests
;eval vs pandoric-eval

(in-package test)
;(named-readtables:in-readtable lol:lol-syntax)


#|
=============================================================================================
Rules:
=============================================================================================
m/r/   str
s/r/s/ str
=============================================================================================
1) - str can always be any lisp form, which returns a string 
   - if there is are quoting delimiters, e.g. s'''
     r and s are interpreted as literal strings 
   - if there are non-quoting delimiters, r and s may be any lisp form, which returns a string 
     e.g. - "abc" 
          - "ab${variable}12"
          - (let ((x "abc")) (#~s/x/"123"/ abc))
          - (#~m/(princ-to-string 'hello)/ (string-upcase "hello"))

2) if-match -- scope
3) quoting delimiter should be not global and possibly integrated
4) interpol varialbles should not need enabling
=============================================================================================

ad tests
(let ((x "abc")) (#~s/x/"123"/ x)) ;"123" T
(#~m/(princ-to-string 'hello)/ (string-upcase "hello"))  ; "HELLO" #()

|#

;------------
;normal usage
;------------
(plan 6)

(is (#~s'(A)'*\1*'i "hanna") "h*a*nna")
(is (#~s'(A)'*\1*'ig "hanna") "h*a*nn*a*")

(is (#~s/"(a)"/"*\\1*"/ "hanna") "h*a*nna")
(is (#~s%"(a)"%"*\\1*"%g "hanna") "h*a*nn*a*")
(is (ifmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") (list $\` $& $\' $1 $2 $3 $4)) '("a" "bcde" "f" "b" "c" "d" "e"))

(is (whenmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") 
  (print $\`) 
  (print $2) 
  (print $4)
  (print $\')) "f")

(finalize)

;------------
;variable or function
;------------
(plan 3)

(is 
  (let ((x "an")
        (y "AN"))
    (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna")) 
  "hANna")

(is 
  (let ((x "an")) 
    (ifmatch (#~m/x/ "hanna") $&)) 
  "an")

(is 
  (let ((x "a(n)n")) 
    (ifmatch (#~m/x/ "hanna") (list $\` $& $\' $1))) 
  '("h" "ann" "a" "n"))

(finalize)

;------------
;interpolation with cl-interpol
;------------
(plan 3)

(cl-interpol:enable-interpol-syntax)

(is 
  (let ((x "(n)(n)"))
    (#~s/#?"${x}"/#?"\\1 \n\n \\2"/ "hanna"))
"han 

 na")

(is 
  (let ((x "(n)(n)")
        (y "HANNA"))
    (#~s/#?"${x}"/#?"\\1 \n ${y} \n \\2"/ "hanna"))
"han 
 HANNA 
 na")

(is 
  (let ((x "an")) 
    (pre:ifmatch (#~m/#?"(${x})"/ "hanna") 
                 (list $& $1)))
  '("an" "an"))

(cl-interpol:disable-interpol-syntax)

(finalize)

;------------
;with another quoting delimiter
;------------
(plan 1)

(setf perlre::qd #\§)

(is (#~s§(A)§'\1'§i "hanna") "h'a'nna")

(setf perlre::qd #\') ; reset to default 

(finalize)


;; bug fix 19.5.15;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1  stg  mit ifmach
;; 2  $1   mit stg und ifmatch

(plan 2)

(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)

(is-values (let ((stg "a")) (#~s'a'b' stg)) '("b" t))

;The variable STG is unbound
;(let ((stg "a")) (pre:ifmatch (#~m'a' stg) "hello"))

(finalize)


(plan 4)
; (m/r/ s)   s may be a string, variable, function
(is-values (#~m'a' "a") '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)
(is-values (#~m'a' (format nil "a")) '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' (format nil "~a" stg))) '("a" #()) :test #'equalp)
;geht nicht
;(is-values (let ((stg (format nil "~a" "a"))) (ifmatch (#~m'(a)' stg) $1)) '("a" #()) :test #'equalp)

(finalize)

(plan 1)
; string can be any lisp form returning a string
(is (#~m'abc' (#~m'abc' "abc")) "abc")

(finalize)

(plan 1)
;scope of $1 etc
(is
(let ((x "an"))
  (pre:whenmatch (#~m/x/ "hanna") 
    (#~s/$&/(string-upcase $&)/ "anna")))
"ANna")



(finalize)
