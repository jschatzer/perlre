perlre
======

perl regular expression api - m// and s/// - for CL-PPCRE with CL-INTERPOL support


With LET-OVER-LAMBDA you can do:
(#~m/regex/imsx string) or (#~s!regex!substitution!imsx string)
It supports perl's imsx modifiers and arbitrary delimiters, 
but for now it does not support "string-interpolation" in regex or substitution.


PERLRE supports: 
- perl's g modifier
- string- and backslash-interpolation with cl-interpol
- normal variable substitution or function calls like format
- suppressing interpolation, using single quotes - m'' or s''' - as in perl


For now interpolation comes with a cost, here are some restrictions:
- regular expressions and substitutions must be explicit strings, i.e. double quoted or cl-interpol strings
- need of 2 backslashes, \\& \\` \\1 \\2 ... as cl-ppcre
- you must enable/disable cl-interpol-syntax

------------
Examples:
------------

(#~s'(A)'*\1*'i "hanna")

(#~s'(A)'*\1*'ig "hanna")

(#~s!"(a)"!"*\\1*"! "hanna")

(#~s%"(a)"%"*\\1*"%g "hanna")

; variable or function call

(let ((x "an")) 
  (#~m/x/ "hanna"))

(let ((x "an")
      (y "AN"))
 (#~s/(format nil "~a" x)/(format nil "~a" y)/ "hanna"))


(cl-interpol:enable-interpol-syntax)

(let ((x "(n)(n)"))
 (#~s/#?"${x}"/#?"\\1 \n\n \\2"/ "hanna"))

(let ((x "(n)(n)")
      (y "HANNA"))
 (#~s/#?"${x}"/#?"\\1 \n ${y} \n \\2"/ "hanna"))

(cl-interpol:disable-interpol-syntax)

