;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------
(in-package #:perlre)

; 24.11.2014
;;; http://code.activestate.com/lists/perl5-porters/182367/   ; /n  to not interpolate  <--------
;;; m//n or mq// or ... ? to preclude interpolation, (q for quote)
(defun segment-reader (strm ch n)
  "with m'' or s''' supress string interpolation, camel192"
  (if (> n 0)
    (let (chars)
      (do ((curr #1=(read-char strm) #1#))
        ((char= ch curr))
        (push curr chars))
      (if (char= ch #\')
        (cons (coerce (nreverse chars) 'string) (segment-reader strm ch (1- n)))
        (cons (with-input-from-string (s (coerce (nreverse chars) 'string)) (read s)) 
              (segment-reader strm ch (1- n)))))))

(define-symbol-macro 
  regex
  `(if (zerop (length ,g!mods))
     (car ,g!args)
     (format nil "(?~a)~a" (remove #\g ,g!mods) (car ,g!args))))

(lol:defmacro! subst-mode-ppcre-lambda-form (o!args o!mods)
  ``(lambda (,',g!str)
      (if (find #\g ,,g!mods)
        (ppcre:regex-replace-all ,,regex ,',g!str ,(cadr ,g!args))
        (ppcre:regex-replace ,,regex ,',g!str ,(cadr ,g!args)))))

(lol:defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
  ``(lambda (,',g!str)
      (ppcre:scan-to-strings ,,regex ,',g!str)))

(defun mods (stm)
  "imsxg modifiers"
  (coerce (loop for c = (read-char stm)
                while (alpha-char-p c) collect c
                finally (unread-char c stm))
          'string))

(set-dispatch-macro-character #\# #\~
  (lambda (stm c n) (declare (ignore c n))
    "dispatch function for #~"
    (let ((mode-char (read-char stm)))
      (case mode-char 
        (#\m (match-mode-ppcre-lambda-form (segment-reader stm (read-char stm) 1) (mods stm)))
        (#\s (subst-mode-ppcre-lambda-form (segment-reader stm (read-char stm) 2) (mods stm)))
        (t (error "Unknown #~~ mode character"))))))

(lol:defmacro! ifmatch ((test str) conseq &optional altern)
  #"(ifmatch (#~m/"(b(c)d)e"/ "abcdef") (list |$`| $& |$`| $1 $2))"#
  (let ((dollars (remove-duplicates
                   (remove-if-not #'lol:dollar-symbol-p
                                  (lol:flatten (lol:prune-if-match-bodies-from-sub-lexical-scope conseq))))))
    (let ((top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))      ; la for length array, ev rename top
      `(let ((,g!s ,str)) ; s for string
         (multiple-value-bind (,g!ms ,g!ra) (,test ,g!s) (declare (ignorable ,g!ra)) ; ms match-string, ra register-array
           (if (plusp (length ,g!ms)) ; ppcre:scan-to-strings returns "" if there is no match
             (destructuring-bind ($\` $& $\') (ppcre:split (format nil "(~a)" ,g!ms) ,g!s :with-registers-p t :limit 3)
               (declare (ignorable $\` $& $\'))
               (let ,#1=(mapcar #`(,(lol:symb "$" a1) (aref ,g!ra ,(1- a1))) (loop for i from 1 to top collect i))
                 (declare (ignorable ,@(mapcar #'car #1#)))
                 ,conseq))
             ,altern))))))

(defmacro whenmatch ((test str) conseq &rest more-conseq)
  #"(whenmatch (#~m/"(b)(c)(d)(e)"/ "abcdef") 
    (print |$`|) 
    (print $2) 
    (print $4))"#
  `(ifmatch (,test ,str)
     (progn ,conseq ,@more-conseq)))
