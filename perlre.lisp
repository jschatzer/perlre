;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------

(in-package #:perlre)

(defvar qd #\' "quoting-delimiter") 
(define-symbol-macro bar (coerce (nreverse chars) 'string))
(define-symbol-macro baz (segment-reader s c (1- n)))
(define-symbol-macro regex `(if (zerop (length ,g!m)) (car ,g!a) (format nil "(?~a)~a" (remove #\g ,g!m) (car ,g!a))))
(defun xx (l i) (case i (\` (first l)) (& (second l)) (\' (third l))))

(defun segment-reader (s c n)
  "to supress string interpolation use single-quote delimiters, #~s''', #~m'', as in perl, see camelbook page 192,
  or use an alternate quoting-delimiter doing e.g. (setf perlre::qd #\!)"
  (if (plusp n)
    (let (chars)
      (do ((curr #1=(read-char s) #1#)) ((char= c curr)) (push curr chars))
      (if (char= c qd) (cons bar baz) (cons (with-input-from-string (x bar) (read x)) baz)))))

(defun mods (s)
  "imsxg modifiers"
  (coerce (loop for c = (read-char s) while (alpha-char-p c) collect c finally (unread-char c s)) 'string))

(lol:defmacro! sub (o!a o!m)
  ``(lambda (,',g!s)
      (if (find #\g ,,g!m)
        (ppcre:regex-replace-all ,,regex ,',g!s ,(cadr ,g!a))
        (ppcre:regex-replace     ,,regex ,',g!s ,(cadr ,g!a)))))

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (ppcre:scan-to-strings ,,regex ,',g!s)))

(set-dispatch-macro-character #\# #\~
  (lambda (s c n)
    (case (read-char s)
      (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
      (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
      (t (error "Unknown #~~ mode character")))))

(defmacro ifmatch ((test stg) conseq &optional altern)
  `(let ((s ,stg))
     (multiple-value-bind (m a) (,test s)
       (lol:pandoric-eval (a)
         `(if (plusp (length ,m))
            (let ((ml (ppcre:split (format nil "(~a)" ,m) ,s :with-registers-p t :limit 3)))
              (let ,#1=(append 
                         (mapcar (lambda (a1) `(,(lol:symb "$" a1) (xx ml ',a1))) '(\` & \'))
                         (mapcar (lambda (a1) `(,(lol:symb "$" a1) (aref ,a (1- ,a1)))) (loop for i from 1 to (length a) collect i)))
                (declare (ignorable ,@(mapcar #'car #1#)))
                ,',conseq))
            ,',altern)))))

(defmacro whenmatch ((test stg) &rest conseq) `(ifmatch (,test ,stg) (progn ,@conseq)))
