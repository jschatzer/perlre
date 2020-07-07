;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------

(in-package #:perlre)

(defun segment-reader (s c n)
  "evaluation with / / delimiters, no evaluation with any other corresponding delimiter"
  (if (plusp n)
    (symbol-macrolet ((bar (coerce (nreverse chars) 'string)) (baz (segment-reader s c (1- n))))
      (let (chars)
        (do ((curr #1=(read-char s) #1#)) ((char= c curr)) (push curr chars))
        (if (char= c #\/) (cons (with-input-from-string (x bar) (read x)) baz) (cons bar baz))))))

(defun mods (s)
  "modifiers: i m s x g e r and digits"
  (coerce (loop for c = (read-char s) while (alphanumericp c) collect c finally (unread-char c s)) 'string))

;a1 reg regex, a2 rpl replacement, m modifiers: ev rename  s/a1/a2/m
(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s) 
        (let ((a1 ,(car ,g!a)) (a2 ,(cadr ,g!a)) (m ,,g!m))
          (if (string= "" m)
            (ppcre:regex-replace a1 ,',g!s a2)
            (if (find #\g m)
              (ppcre:regex-replace-all #1=(format nil "(?~a)~a" (remove #\e (remove #\g m)) a1) ,',g!s a2 
                                       :simple-calls #2=(find-if (lambda (x) (char= x #\e)) m))
              (ppcre:regex-replace #1# ,',g!s a2 :simple-calls #2#))))))

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (cond 
        ((string= "" ,,g!m) (ppcre:scan-to-strings ,(car ,g!a) ,',g!s))
        ((find #\g ,,g!m) (ppcre:all-matches-as-strings (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s)) 
        (t (ppcre:scan-to-strings (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s)))))

(lol:defmacro! div (o!a o!m)
  ``(lambda (,',g!s)
      (cond 
        ((string= "" ,,g!m) (ppcre:split ,(car ,g!a) ,',g!s))
        ((or #1=(find #\r ,,g!m) #2=(find-if 'digit-char-p ,,g!m))
         (ppcre:split (format nil "(?~a)~a" (remove-if (lambda (x) (or (char= x #\r) (digit-char-p x))) ,,g!m) ,(car ,g!a))
                      ,',g!s :with-registers-p #1# :limit (lol:aif #2# (digit-char-p lol:it))))
        (t (ppcre:split (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s)))))

(defun |#~-reader| (s c n)
  (declare (ignore c n))
  (case (read-char s)
    (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
    (#\d (div (segment-reader s (read-char s) 1) (mods s))) ; divide for split
    (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
    (t (error "Unknown #~~ mode character"))))

;----------- o -------------------- o --------------------- o ------------------------

(set-dispatch-macro-character #\# #\~ '|#~-reader|)

;orig
;(time (dotimes (x 100000) (pre:ifmatch (#~m'a' "a") 1 2))) ; 1.5 seconds of real time
(lol:defmacro! ifmatch ((test o!s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(multiple-value-bind (m a) (,test ,g!s)
       (declare (ignorable a))
       (if m
         (let ((ml (ppcre:split (format nil "(~a)" m) ,g!s :with-registers-p t :limit 3)))
           (let ,#1=(append (mapcar (lambda (a1) `(,(lol:symb "$" a1) (trivia:match ml ((list a b c) (case ',a1 (\` a) (& b) (\' c))))))
                                    '(\` & \'))
                            (mapcar (lambda (a1) `(,(lol:symb "$" a1) (aref a (1- ,a1)))) (loop for i from 1 to top collect i)))
             (declare (ignorable ,@(mapcar #'car #1#)))
             ,conseq))
         ,altern))))

(defmacro whenmatch ((test s) &rest conseq) `(ifmatch (,test ,s) (progn ,@conseq)))

;(ifsplit $1 $2 ..?

(defmacro match (stg &body clauses)
  `(cond
     ,@(loop for (regex . conseq) in clauses
             collect (if (eq regex t)
                       `(t ,@conseq)
                       `((whenmatch (,regex ,stg) ,@conseq))))))

;; END ;;
#|
;; error: no dispatch function defined for #\`

;(time (dotimes (x 100000) (pre:ifmatch (#~m'a' "a") 1 2))) ; 2.0 seconds of real time
(lol:defmacro! ifmatch ((test o!s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(multiple-value-bind (m a) (,test ,g!s)
       (declare (ignorable a))
       (if m
         (let* ((ml (#~d/(format nil "(~a)" m)/r3 ,g!s))
                ,@#1=(append (mapcar #`,`(,(lol:symb "$" a1) (trivia:match ml ((list a b c) (case ',a1 (\` a) (& b) (\' c))))) '(\` & \'))
                             (mapcar #`,`(,(lol:symb "$" a1) (aref a (1- ,a1))) (loop for i from 1 to top collect i))))
           (declare (ignorable ,@(mapcar #'car #1#)))
           ,conseq)
         ,altern))))

;(time (dotimes (x 100000) (pre:ifmatch (#~m'a' "a") 1 2))) ; 5.5 seconds of real time
;ev keep this?, performance probably plays no role
(lol:defmacro! ifmatch ((test o!s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(multiple-value-bind (m a) (,test ,g!s)
       (declare (ignorable a))
       (if m
         (let (,@#1=(append (mapcar #`,`(,(lol:symb "$" a1) (trivia:match (#~d/(format nil "(~a)" m)/r3 ,g!s) ((list a b c) (case ',a1 (\` a) (& b) (\' c))))) '(\` & \'))
                            (mapcar #`,`(,(lol:symb "$" a1) (aref a (1- ,a1))) (loop for i from 1 to top collect i))))
           (declare (ignorable ,@(mapcar #'car #1#)))
           ,conseq)
         ,altern))))
|#


