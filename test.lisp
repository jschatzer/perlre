;test.lisp
(defpackage test (:use cl prove perlre))
(in-package test)

#|============================================================================================
Rules:
1) - use // delimiters to have the content evaluated
2) - for unevaluated content use any other character as delimiter, e.g. (#~ma3a "1234") -> "3"
3) - (cl-interpol:enable-interpol-syntax) to get variable- and backslash interpolation 
============================================================================================|#

;----------------------------------------------
; 1) simple m// or s///
;----------------------------------------------
(plan 19)

;normal usage
(is (#~s'(A)'*\1*'i "hanna") "h*a*nna")
(is (#~s'(A)'*\1*'ig "hanna") "h*a*nn*a*")
(is (#~s/"(a)"/"*\\1*"/ "hanna") "h*a*nna")

;variable or function
(is-values (#~m/(princ-to-string 'hello)/ (string-upcase "hello")) '("HELLO" #()) :test #'equalp) 
(is-values (let ((x "abc")) (#~s/x/"123"/ x)) '("123" T) :test #'equalp)

(is 
  (let ((x "an")
        (y "AN"))
    (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna")) 
  "hANna")

(is 
  (let ((x "an")
        (y "AN"))
    (#~s/x/y/ "hanna")) 
  "hANna")

(is 
  (let ((x "an")
        (y "AN")
        (z "hanna"))
    (#~s/x/y/ z)) 
  "hANna")

(is 
  (let ((x "an")
        (y "AN")
        (z "hanna"))
    (#~s/(format nil "~a" x)/(format nil "~a" y)/ (format nil "~a" z))) 
  "hANna")


;----------------------------------------------
; 2) interpolation with cl-interpol
;----------------------------------------------
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


;----------------------------------------------
; 3) other quoting delimiter
;----------------------------------------------
(is (#~s§(A)§'\1'§i "hanna") "h'a'nna")


;----------------------------------------------
; 4) with // delimiters regex, substitution and string can be any lisp form returning a string
;----------------------------------------------
(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~s'a'b' stg)) '("b" t))
(is-values (#~m'a' "a") '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' stg)) '("a" #()) :test #'equalp)
(is-values (#~m'a' (format nil "a")) '("a" #()) :test #'equalp)
(is-values (let ((stg "a")) (#~m'a' (format nil "~a" stg))) '("a" #()) :test #'equalp)
(is (#~m'abc' (#~m'abc' "abc")) "abc")

(finalize)

;----------------------------------------------
; 5) simple if, when
;----------------------------------------------
(plan 2)

(is (if (#~m's' "s") "hello") "hello")
(is (if (#~s's'x' "s") "hello") "hello")

(finalize)

;----------------------------------------------
; 6) ifmatch whenmatch, these are ok
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
    (ifmatch (#~m/#?"(${x})"/ "hanna") 
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
; 7) ifmatch whenmatch - warning, undefined variable: $1, solved
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
; 8) ifmatch whenmatch, nesting problem, solved
;----------------------------------------------
(plan 1)
; example from abc  <------
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


(plan 6)

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(b)' stg)
               $1)) "b")

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(b)' stg)
             (#~s/$1/"a"/ stg))) "a1")

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(b)' stg)
      (ifmatch (#~m/(format nil "(~a)" $1)/ stg)
               $1))) "b")

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(^\w)' stg)
      (ifmatch (#~m/(format nil "(~a)" $1)/ stg)
               $1))) "b")

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(^\w)' stg)
      (ifmatch (#~m/(format nil "(~a1)" $1)/ stg)
               $1))) "b1")

(is
  (let ((stg "b1"))
    (ifmatch (#~m'(^\w)' stg)
      (ifmatch (#~m/(format nil "(~a\\d)" $1)/ stg)
               $1))) "b1")

(finalize)


;look arounds
(plan 1)

(is
(let ((s "a1
         bc
         ef"))
  (#~s'(?<!\d)\n\s*''g s)) 
"a1
         bcef")

(finalize)



(plan 3)
; using variables or functions in regex if there are modifiers
; ok
(is
(let ((y "A"))
  (#~s/"a"/(format nil "~a" y)/g "hanna")) "hAnnA")

;fail, 9.7.15, ok 13.7.15

(is
(let ((x "a")
      (y "A"))
  (#~s/(format nil "~a" x)/(format nil "~a" y)/g "hanna")) "hAnnA")

(is
(let ((x "a")
      (y "A"))
  (#~s/x/y/g "hanna")) "hAnnA")

(finalize)


(plan 9)
;s///e

(defun pad (s r)
  (declare (ignore s))
  (format nil "~2,,,'0@a" r))

(is 
	(#~s/"^(\\d{1,2})\\.$"/'pad/e "4.")
	"04")

(is 
	(let ((re '("^(\\d{1,2})\\.$" . pad)))
		(#~s/(car re)/(cdr re)/e "2."))
	"02")

(let ((stg "2."))
	(is 
		(let ((re '("^(\\d{1,2})\\.$" . pad)))
			(#~s/(car re)/(cdr re)/e stg))
		(ifmatch (#~m/"^(\\d{1,2})\\.$"/ stg) 
						 (pad stg $1))))


(defun fn (s r1 r2) 
	(declare (ignore s))
	(format nil "~a : ~a " (string-upcase r1) (+ 3 (read-from-string r2))))

(is
	(ppcre:regex-replace "a(bc)(3)" "abc3e" 'fn :simple-calls t)
	"BC : 6 e")
(is
	(ppcre:regex-replace "a(bc)(3)" "abc3e" #'fn :simple-calls t)
	"BC : 6 e")
(is
	(ppcre:regex-replace "a(bc)(3)" "abc3e" (function fn) :simple-calls t)
	"BC : 6 e")

(is
	(#~s/"a(bc)(3)"/'fn/e "abc3e")
	"BC : 6 e")
(is
	(#~s/"a(bc)(3)"/#'fn/e "abc3e")
	"BC : 6 e")
(is
	(#~s/"a(bc)(3)"/(function fn)/e "abc3e")
	"BC : 6 e")

(finalize)



(plan 2)

(is (#~s/"'"/"'"/g "a'b'c") "a'b'c")
(is (#~s/(code-char 8217)/"'"/g "a'b'c") "a'b'c")

(finalize)

(plan 3)

(is (ppcre:all-matches-as-strings "ab" "1ab2ab3ab4") ; ("ab" "ab" "ab")
    (#~m'ab'g "1ab2ab3ab4"))

(is (#~m'aB'gi "1ab2ab3ab4") '("ab" "ab" "ab"))

;make a better test
(is (#~m'aB'gims "1ab2
     ab3ab4") '("ab" "ab" "ab"))
;???
;(#~m'(ab)'g "1ab2ab3ab4"))
;(#~m'a(b)'g "1ab2ab3ab4"))

(finalize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 19.12.2016
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(plan 5)

;replace all  ’  with '
(is (#~s/"’"/"'"/g "a’b’c") ; "a'b'c" 
    "a'b'c")

;(char-code #\’) ; 8217
(is (#~s/(code-char 8217)/"'"/g "a’b’c")
    "a'b'c")

;from man perlre
(is-values (#~m/"fi"/i "LATIN SMALL LIGATURE FI") '("FI" #()) :test #'equalp)

;# swap first two words, from man perlre
(is (#~s'^([^ ]*) *([^ ]*)'\2 \1' "Abc Bca Cde")
    "Bca Abc Cde")

; x modifier
;from https://code.tutsplus.com/tutorials/advanced-regular-expression-tips-and-techniques--net-11011
;match us phone number
(is-values
  (#~m'^         # beginn of string
   (1[-\s.])?    # optional "1-", "1." or "1"   
   (\()?         # optional opening parenthesis
   \d{3}         # the area code
   (?(2)\))      # if there was opening parenthesis, close it
   [-\s.]?       # followed by "-" or "." or space
   \d{3}         # first 3 digits
   [-\s.]?       # followed by "-" or "." or space
   \d{4}         # last 4 digits
   $'x           
   ; "1-541-754-3010")
   "(541) 754-3010")
  '("(541) 754-3010" #(NIL "(")) :test #'equalp)




(finalize)

#|
ev test, anschauen
;;; in icd diagnosi
(ppcre:regex-replace (ppcre:quote-meta-chars rmtxt) item "") 

;; 1.10.15    alternat delimiter example   <-------
The value 8217 is not of type (UNSIGNED-BYTE 8)
replace all  ’  with '       <----------------

    right single-quote — &rsquo; — ’
		left single-quote — &lsquo; — ‘
		right double-quote — &rdquo; — ”
		left double-quote — &ldquo; — “

(code-char #\’)   ; error

(char-code #\’) ; 8217

(#~s'’'x'g "a’b’c")    <--- ad test perlre

(#~s'’'x'g "a’b’c")    <--- ad test perlre
|#

; 24.9.2017
;split/divide test
(plan 3)
(is (#~d'b' "abc") '("a" "c") :test #'equalp)
(is (#~d/#\|/ "a|b") '("a" "b") :test #'equalp)
(is (destructuring-bind (x y) (#~d/#\|/ "a|b") (list y x)) '("b" "a") :test #'equalp)
(finalize)

; 28.3.18
;(ppcre:split "a" "cab")  ; ("c" "b")
;(ppcre:split "(a)" "cab" :with-registers-p t) ;  ("c" "a" "b")

;30.3.2018 split with limit and registers
(plan 11)
(is (#~d'b' "abc") '("a" "c") :test #'equalp)
(is (#~d'(b)' "abc") '("a" "c") :test #'equalp)
(is (#~d'B'i "abc") '("a" "c") :test #'equalp)
(is (#~d'(b)'r "abc") '("a" "b" "c") :test #'equalp)
(is (#~d'(B)'ir "abc") '("a" "b" "c") :test #'equalp)
(is (ppcre:split "b" "abcbd" :limit 2) '("a" "cbd") :test #'equalp)
(is (ppcre:split "b" "abcbd" :limit (+ 1 1)) '("a" "cbd") :test #'equalp)
(is (#~d'b'2 "abcbd") '("a" "cbd") :test #'equalp)
(is (#~d'(b)'r2 "abcbd") '("a" "b" "cbd") :test #'equalp)
(is (ppcre:split "(b)" "abcbd" :limit 3 :with-registers-p t) '("a" "b" "c" "b" "d") :test #'equalp)
(is (#~d'(b)'r3 "abcbd") '("a" "b" "c" "b" "d") :test #'equalp)
(finalize)

(plan 1)
; example in README
; output of both is "h*a*nn*a*" 
(let ((stg "hanna")
      (reg "(A)")
      (sub "*\\1*"))
  (is
    ; perlre
    (#~s/reg/sub/ig stg)
    ; cl-ppcre
    (ppcre:regex-replace-all (ppcre:create-scanner reg :case-insensitive-mode t) stg sub)))
(finalize)

;;vergleicht optima match, ev include in test
;;;-geht--------------------------
;;;nicht ad perlre test, da muß auch optima.ppcre geladen werden
;;(h:ql :optima.ppcre)
;;
;;(optima:match "1234"
;;  ((optima.ppcre:ppcre "(3)" r) r))
;;
;;(#~ma3a "1234")
;;;beide geben "3"
;;;-------------------------------

(plan 8)
(is (match "abc"
      (#~m'(a)' $1))
    "a")

(is (match "abc"
      (#~m'(e)' $1)
      (#~m'd' 'no)
      (#~m'a(b)c' 'yes $1))
    "b")

(is (pre:match "abc"
       (#~m'(e)' $1)
       (#~m'd' 'no)
       (#~m'a(d)c' 'yes $1))
    NIL)         ; 'NIL geht auch

(is 
(match "2012-11-04" 
  (#~m'^(\d+)-(\d+)-(\d+)$' (list $1 $2 $3)))
    '("2012" "11" "04") :test #'equalp)

;vs trivia
(is (trivia:match "2012-11-04"
      ((trivia.ppcre:ppcre "^(\\d+)-(\\d+)-(\\d+)$" year month day)
       (list year month day)))
    '("2012" "11" "04") :test #'equalp)
 
(is (pre:ifmatch (#~m'^(\d+)-(\d+)-(\d+)$' "2012-11-04")
      (list $1 $2 $3))
    '("2012" "11" "04") :test #'equalp)
 
(is (pre:whenmatch (#~m'^(\d+)-(\d+)-(\d+)$' "2012-11-04")
      (list $1 $2 $3))
    '("2012" "11" "04") :test #'equalp)

; (t for otherweise 15.9.18
(is (match "2012-11-04" 
  (#~m'abc' 1)
  (t 2)) 2)


(finalize)



