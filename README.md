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


  (let ((stg "hanna")
        (reg "(A)")
        (sub "*\\1*"))

   ; output of both is "h*a*nn*a*" 

   ; perlre
   (#~s/reg/sub/ig stg)

   ; cl-ppcre
   (ppcre:regex-replace-all (ppcre:create-scanner reg :case-insensitive-mode t) stg sub))
  ```
See test.lisp for other examples.
