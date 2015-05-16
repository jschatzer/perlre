(defpackage test 
  (:use cl prove perlre))

(in-package test)
(named-readtables:in-readtable lol:lol-syntax)

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
 (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna")) "hANna")

(is 
(let ((x "an")) 
 (pre:ifmatch (#~m/x/ "hanna") $&)) "an")

(is 
(let ((x "a(n)n")) 
 (pre:ifmatch (#~m/x/ "hanna") 
	(list $\` $& $\' $1))) '("h" "ann" "a" "n"))

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
