perlre
======

*Regular expression API using CL-PPCRE and CL-INTERPOL with operators known from sed or perl,*
__m//__ and __s///__ plus a similar one, __d//__ for split.

Can be useful for code brevity in regex heavy programs.

#### idea and code from Doug Hoyte's book [Let Over Lambda](http://letoverlambda.com) and quicklisp-package let-over-lambda

### Synopsis:
```
(#~s/regex/substitution/imsxge string)
(#~m/regex/[modifier] string)
(#~d/regex/[modifier] string) 

(ifmatch test then else)
(whenmatch test conseq*)
(match string clause*) ; a clause (test conseq) e.g (#~m/(regex)/ (list $1))

  Some examples:

  ; output of both is "h*a*nn*a*" 
  (let ((stg "hanna")
        (reg "(A)")
        (sub "*\\1*"))

    ; perlre
    (#~s/reg/sub/ig stg)

    ; cl-ppcre
    (ppcre:regex-replace-all (ppcre:create-scanner reg :case-insensitive-mode t) stg sub))
  



  ; -> 2
  (match "2012-11-04" 
    (#~m'abc' 1)
    (t 2))

  ;    HELLO
  ; -> 2016
  (match "2012-11-04" 
    (#~m'abc' 1)
    (#~m§(2012)-11-(04)§ (print 'hello) (+ (parse-integer $1) (parse-integer $2)))
    (t 2))

  ;"04/2012"
  (pre:match "2012-11-04"
    (#~m'abc' 1)
    (#~m§(\d{4})-.+-(..)§ (format nil "~a/~a" $2 $1))
    (t 2))

```
See test.lisp for other examples.
