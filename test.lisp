;test.lisp
(defpackage test (:use cl prove perlre))
(in-package test)
#|===========================================================================================
Rules:
=============================================================================================
m/r/   str
s/r/s/ str
=============================================================================================
1) - str can always be any lisp form, which returns a string 
   - if there are quoting delimiters, e.g. s'''
     r and s are interpreted as literal strings 
   - if there are non-quoting delimiters, r and s may be any lisp form, which returns a string 
     e.g. - "abc" 
          - "ab${variable}12"
          - (let ((x "abc")) (#~s/x/"123"/ abc))
          - (#~m/(princ-to-string 'hello)/ (string-upcase "hello"))

2) if-match -- scope
3) quoting delimiter should be not global and possibly integrated
4) interpolating variables should not need explicit enabling
===========================================================================================|#

;----------------------------------------------
; 1) simple m// or s///
;----------------------------------------------
(plan 17)

;normal usage
(is (#~s'(A)'*\1*'i "hanna") "h*a*nna")
(is (#~s'(A)'*\1*'ig "hanna") "h*a*nn*a*")
(is (#~s/"(a)"/"*\\1*"/ "hanna") "h*a*nna")
(is (#~s%"(a)"%"*\\1*"%g "hanna") "h*a*nn*a*")

;variable or function
(is-values (#~m/(princ-to-string 'hello)/ (string-upcase "hello")) '("HELLO" #()) :test #'equalp) 
(is-values (let ((x "abc")) (#~s/x/"123"/ x)) '("123" T) :test #'equalp)

(is 
  (let ((x "an")
        (y "AN"))
    (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna")) 
  "hANna")

;interpolation with cl-interpol
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
(cl-interpol:disable-interpol-syntax)

;with another quoting delimiter
(setf perlre::qd #\§)
(is (#~s§(A)§'\1'§i "hanna") "h'a'nna")
(setf perlre::qd #\') ; reset to default 


(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~s'a'b' stg)) '("b" t))

; (m/r/ s)   s may be a string, variable, function
(is-values (#~m'a' "a") '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)
(is-values (#~m'a' (format nil "a")) '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' (format nil "~a" stg))) '("a" #()) :test #'equalp)
; string can be any lisp form returning a string
(is (#~m'abc' (#~m'abc' "abc")) "abc")

(finalize)

;----------------------------------------------
; 2) simple if, when
;----------------------------------------------
(plan 2)

(is (if (#~m's' "s") "hello") "hello")
(is (if (#~s's'x' "s") "hello") "hello")

(finalize)

;----------------------------------------------
; 3) ifmatch whenmatch, these are ok
;----------------------------------------------
(plan 11)

(is (ifmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") (list $\` $& $\' $1 $2 $3 $4)) '("a" "bcde" "f" "b" "c" "d" "e"))
(is (ifmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") "hello") "hello")
(is (ifmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") (+ 3 4)) 7)

(is (whenmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") 
  (list $\`) 
  (list $2) 
  (list $4)
  (list $\')) '("f"))

;variable or function
(is 
  (let ((x "an")) 
    (ifmatch (#~m/x/ "hanna") $&)) 
  "an")

(is 
  (let ((x "a(n)n")) 
    (ifmatch (#~m/x/ "hanna") (list $\` $& $\' $1))) 
  '("h" "ann" "a" "n"))

;interpolation with cl-interpol
(cl-interpol:enable-interpol-syntax)
(is 
  (let ((x "an")) 
    (pre:ifmatch (#~m/#?"(${x})"/ "hanna") 
                 (list $& $1)))
  '("an" "an"))
(cl-interpol:disable-interpol-syntax)

;scope of $1 etc
(is
(let ((x "an"))
  (whenmatch (#~m/x/ "hanna") 
    (#~s/$&/(string-upcase $&)/ "anna")))
"ANna")

(is (let ((stg (format nil "~a" "a"))) (ifmatch (#~m'(a)' stg) $1)) "a")
(is (let ((stg "a")) (ifmatch (#~m'a' stg) "hello")) "hello")

(is
(let ((stg "a1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    "hello"))
'("a" CHAR))

(finalize)

;----------------------------------------------
; 4) ifmatch whenmatch - gehen mit warning, undefined variable: $1
;----------------------------------------------
(plan 3)

(is
(let ((stg "b1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    "hello"))
"hello")

(is
(let ((stg "b1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    (+ 3 4)))
7)

(is
(let ((stg "b1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    stg))
"b1")
(finalize)

;----------------------------------------------
; 5) ifmatch whenmatch, these are NOT ok, make them go
;----------------------------------------------
#|
(plan 1)

; example from abc  <------

(is
(let ((stg "b1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    (ifmatch (#~m'(1)' stg)
      (list $1 'number))))   
'("1" NUMBER))

;nesting geht nicht
; error, illegal function call
(is
(let ((stg "b1"))
  (ifmatch (#~m'(x)' stg)
    (list $1 'char)
    (ifmatch (#~m'(.)(1)' stg)
      (list $2 'number))))   
'("1" NUMBER))

(finalize)

(plan 4)

(is
(let ((stg "b1"))
  (ifmatch (#~m'(a)' stg)
    (list $1 'char)
    (ifmatch (#~m'(1)' stg)
      (list $1 'number))))   
'("1" NUMBER))

;nesting is ok
(is
  (let ((stg "b1"))
    (ifmatch (#~m'(a)' stg)
      (list $1 'char)
      (ifmatch (#~m'(c)' stg)
        (list $1 'char2)
        (ifmatch (#~m'(1)' stg)
          (list $1 'number)))))   
    '("1" NUMBER))

(is (ifmatch (#~s's'x' "stg") "hello") "hello")

(is (let ((stg (format nil "~a" "a"))) (ifmatch (#~m'(a)' stg) $1)) "a")

(finalize)
|#


;----------------------------------------------
; 6) for nesting use lol:if-match lol:when-match ###########################
;----------------------------------------------
(plan 4)
(named-readtables:in-readtable lol:lol-syntax)

(is
(let ((stg "b1"))
  (lol:if-match (#~m'(a)' stg)
    (list $1 'char)
    (lol:if-match (#~m'(1)' stg)
      (list $1 'number))))   
'("1" NUMBER))

;nesting is ok
(is
  (let ((stg "b1"))
    (lol:if-match (#~m'(a)' stg)
      (list $1 'char)
      (lol:if-match (#~m'(c)' stg)
        (list $1 'char2)
        (lol:if-match (#~m'(1)' stg)
          (list $1 'number)))))   
    '("1" NUMBER))

(is (lol:if-match (#~s's'x' "stg") "hello") "hello")

(is (let ((stg (format nil "~a" "a"))) (lol:if-match (#~m'(a)' stg) $1)) "a")

(finalize)
