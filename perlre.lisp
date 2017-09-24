;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------

(in-package #:perlre)
;(named-readtables:in-readtable lol:lol-syntax)

(defvar qd #\' "quoting-delimiter") 

(defun segment-reader (s c n)
  "to supress string interpolation use single-quote delimiters, #~s''', #~m'', as in perl, see camelbook page 192,
  or use an alternate quoting-delimiter doing e.g. (setf perlre::qd #\!)"
  (if (plusp n)
    (symbol-macrolet ((bar (coerce (nreverse chars) 'string)) (baz (segment-reader s c (1- n))))
      (let (chars)
        (do ((curr #1=(read-char s) #1#)) ((char= c curr)) (push curr chars))
        (if (char= c qd) (cons bar baz) (cons (with-input-from-string (x bar) (read x)) baz))))))

(defun mods (s)
  "imsxg modifiers"
  (coerce (loop for c = (read-char s) while (alpha-char-p c) collect c finally (unread-char c s)) 'string))

(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1)))
        (let ((a1 ,(car ,g!a)) (a2 ,(cadr ,g!a)) (m ,,g!m)) ; ev gensym problems??
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              (if (find #\e m)
                (ppcre:regex-replace-all reg ,',g!s a2 :simple-calls t)
                (ppcre:regex-replace-all reg ,',g!s a2))
              (if (find #\e m)
                (ppcre:regex-replace reg ,',g!s a2 :simple-calls t)
                (ppcre:regex-replace reg ,',g!s a2))))))))

; ist in bzw siehe perlre.1.12.15.lisp
;--------------------

;---- without /e modifier
#|
(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1))
                        (sc (a2 :simple-calls t)))
        (let ((a1 ,(car ,g!a)) (a2 ,(cadr ,g!a)) (m ,,g!m)) ; ev gensym problems??
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              (ppcre:regex-replace-all reg ,',g!s (if (functionp a2) sc a2))
              (ppcre:regex-replace reg ,',g!s (if (functionp a2) sc a2))))))))

;könnte prinzipiell gehen, problems with splice in
(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1))
                        (rep `(if (functionp ,a2) '(a2 :simple-calls t) '(a2))))   ; splice this in
        (let ((a1 ,(car ,g!a)) (a2 ,(cadr ,g!a)) (m ,,g!m)) ; ev gensym problems??
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              (ppcre:regex-replace-all reg ,',g!s ,@rep)
              (ppcre:regex-replace reg ,',g!s ,@rep)))))))

(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1)))
        (let* ((a1 ,(car ,g!a)) 
              (a2 ,(cadr ,g!a)) 
              (m ,,g!m)
              (rep (if (functionp a2) '(a2 :simple-calls t) '(a2))))
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              `(ppcre:regex-replace-all ,reg ,,',g!s ,@rep)
              `(ppcre:regex-replace ,reg ,,',g!s ,@rep)))))))

(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1)))
        (let* ((a1 ,(car ,g!a)) 
              (a2 ,(cadr ,g!a)) 
              (m ,,g!m)
              (rep (if (functionp a2) `(,a2 :simple-calls t) `(,a2))))
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              `(ppcre:regex-replace-all ,reg ,,',g!s ,@rep)
              `(ppcre:regex-replace ,reg ,,',g!s ,@rep)))))))

;geht nicht
(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
      (symbol-macrolet ((reg (format nil "(?~a)~a" (remove #\e (remove #\g m)) a1)))
        (let* ((a1 ,(car ,g!a)) 
              (a2 ,(cadr ,g!a)) 
              (m ,,g!m)
              (rep (if (functionp a2) `(,a2 :simple-calls t) `(,a2))))
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              (ppcre:regex-replace-all reg ,',g!s `,@rep)
              (ppcre:regex-replace reg ,',g!s `,@rep)))))))

|#
;-------------------------------------------------------------

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (cond 
        ((string= "" ,,g!m) (ppcre:scan-to-strings ,(car ,g!a) ,',g!s))
        ((find "g" ,,g!m :test 'string=) (ppcre:all-matches-as-strings (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s)) 
        (t (ppcre:scan-to-strings (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(set-dispatch-macro-character #\# #\~
  (lambda (s c n)
    (case (read-char s)
      (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
      (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
      (t (error "Unknown #~~ mode character")))))
|#

;27.2.2017  to use it in onlisp <---
; ev wieder mit named readtables???
(defun xxx (s c n)
    (case (read-char s)
      (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
      (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
      (t (error "Unknown #~~ mode character"))))
(set-dispatch-macro-character #\# #\~ 'xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|;geht
(cl-anonfun:enable-fn-syntax)

(set-dispatch-macro-character #\# #\~
    #%3(case (read-char %1)
      (#\m (mat (segment-reader %1 (read-char %1) 1) (mods %1)))
      (#\s (sub (segment-reader %1 (read-char %1) 2) (mods %1)))
      (t (error "Unknown #~~ mode character"))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.9.2017 split test
;options zu überlegen, 
; i case-insensitiv <---
; limit             <---
;  limit   with-registers-p omit-unmatched-p sharedp

;(lol:defmacro! split (o!a o!m)
(lol:defmacro! div (o!a o!m)
 ``(lambda (,',g!s)
     (cond 
       ;((string= "" ,,g!m) (ppcre:scan-to-strings ,(car ,g!a) ,',g!s))
       ;((find "g" ,,g!m :test 'string=) (ppcre:all-matches-as-strings (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s))
       ;(t (ppcre:scan-to-strings (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s)))))
       ((string= "" ,,g!m) (ppcre:split ,(car ,g!a) ,',g!s))
       ; g not relevant
       (t (ppcre:split (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s)))))

(defun xxx (s c n)
  (case (read-char s)
    (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
    (#\d (div (segment-reader s (read-char s) 1) (mods s))) ; divide for split
    (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
    (t (error "Unknown #~~ mode character"))))
(set-dispatch-macro-character #\# #\~ 'xxx)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;


(lol:defmacro! ifmatch ((test o!s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(multiple-value-bind (m a) (,test ,g!s)
       (declare (ignorable a))
       (if m
         (let ((ml (ppcre:split (format nil "(~a)" m) ,g!s :with-registers-p t :limit 3)))
           (let ,#1=(append (mapcar (lambda (a1) `(,(lol:symb "$" a1) (optima:match ml ((list a b c) (case ',a1 (\` a) (& b) (\' c)))))) '(\` & \'))
                            (mapcar (lambda (a1) `(,(lol:symb "$" a1) (aref a (1- ,a1)))) (loop for i from 1 to top collect i)))
             (declare (ignorable ,@(mapcar #'car #1#)))
             ,conseq))
         ,altern))))

#|geht nicht, warum nicht? 2.12.15     bugreport in cl-anonfun  --email 11.5.17  <-----
(cl-anonfun:enable-fn-syntax)

(lol:defmacro! ifmatch ((test o!s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(multiple-value-bind (m a) (,test ,g!s)
       (declare (ignorable a))
       (if m
         (let ((ml (ppcre:split (format nil "(~a)" m) ,g!s :with-registers-p t :limit 3)))
           (let ,#1=(append (mapcar #%`(,(lol:symb "$" %) (optima:match ml ((list a b c) (case ',% (\` a) (& b) (\' c))))) '(\` & \'))
                            (mapcar #%`(,(lol:symb "$" %) (aref a (1- ,%))) (loop for i from 1 to top collect i)))
             (declare (ignorable ,@(mapcar #'car #1#)))
             ,conseq))
         ,altern))))
|#

(defmacro whenmatch ((test s) &rest conseq) `(ifmatch (,test ,s) (progn ,@conseq)))
