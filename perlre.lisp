;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------

; 29.4.2015 named-readtables because of new let-over-lambda version
; thanks to EuAndreh, https://github.com/jschatzer/perlre/pull/2  

; 15.5.2015 alternate quoting delimiter, a first try.

(in-package #:perlre)
(named-readtables:in-readtable lol:lol-syntax)

(defvar qd #\' "quoting-delimiter") 

(defun segment-reader (s c n)
  "to supress string interpolation use single-quote delimiters, #~s''', #~m'', as in perl, see camelbook page 192,
  or use an alternate quoting-delimiter doing e.g.
  (setf perlre::qd #\!)"
  (if (plusp n)
    (let (chars)
      (do ((curr #1=(read-char s) #1#)) ((char= c curr)) (push curr chars))
      (if (char= c qd)
        (cons (coerce (nreverse chars) 'string) (segment-reader s c (1- n)))
        (cons (with-input-from-string (x (coerce (nreverse chars) 'string)) (read x)) (segment-reader s c (1- n)))))))

(define-symbol-macro regex
  `(if (zerop (length ,g!m))
     (car ,g!a)
     (format nil "(?~a)~a" (remove #\g ,g!m) (car ,g!a))))

(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s)
      (if (find #\g ,,g!m)
        (ppcre:regex-replace-all ,,regex ,',g!s ,(cadr ,g!a))
        (ppcre:regex-replace     ,,regex ,',g!s ,(cadr ,g!a)))))

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (ppcre:scan-to-strings ,,regex ,',g!s)))

(defun mods (s)
  "imsxg modifiers"
  (coerce 
    (loop for c = (read-char s) while (alpha-char-p c) collect c finally (unread-char c s)) 
    'string))

(set-dispatch-macro-character #\# #\~
  (lambda (s c n) (declare (ignore c n))
    "dispatch function for #~"
    (let ((mode-char (read-char s)))
      (case mode-char 
        (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
        (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
        (t (error "Unknown #~~ mode character"))))))

(defun xx (l i) (case i (\` (first l)) (& (second l)) (\' (third l))))

; for now without gensyms, 15.12.14
(lol:defmacro! ifmatch ((test s) conseq &optional altern)
  `(multiple-value-bind (m a) (,test ,s) ; m match, a array
     (eval `(if (plusp (length ,m))
              (let ((ml (ppcre:split (format nil "(~a)" ,m) ,',s :with-registers-p t :limit 3))) ; ml match-list
                (let ,#1=(append 
                           (mapcar #`(,(lol:symb "$" a1) (xx ml ',a1)) '(\` & \'))
                           (mapcar #`(,(lol:symb "$" a1) (aref ,a ,(1- a1))) (loop for i from 1 to (length a) collect i)))
                  (declare (ignorable ,@(mapcar #'car #1#)))
                  ,',conseq))
              ,',altern))))

(defmacro whenmatch ((test s) &rest conseq) `(ifmatch (,test ,s) (progn ,@conseq)))
