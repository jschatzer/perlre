;;;; perlre.lisp
;-----------------------------------------------------------------------------
;;; idea and code from Doug Hoyte's book and quicklisp-package Let-Over-Lambda
;;; http://letoverlambda.com 
;-----------------------------------------------------------------------------

(in-package #:perlre)

(defvar qd #\' "quoting-delimiter") 
(define-symbol-macro bar (coerce (nreverse chars) 'string))
(define-symbol-macro baz (segment-reader s c (1- n)))
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
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
				 (if (find #\e ,,g!m)
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\e (remove #\g ,,g!m)) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a)))
				 (if (find #\e ,,g!m)
           (ppcre:regex-replace (format nil "(?~a)~a" (remove #\e ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s ,(cadr ,g!a)))))))

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (if (string= "" ,,g!m)
        (ppcre:scan-to-strings ,(car ,g!a) ,',g!s)
        (ppcre:scan-to-strings (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s))))

(set-dispatch-macro-character #\# #\~
  (lambda (s c n)
    (case (read-char s)
      (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
      (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
      (t (error "Unknown #~~ mode character")))))

(lol:defmacro! ifmatch ((test s) conseq &optional altern)
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten conseq))))
         (top (or (car (sort (mapcar #'lol:dollar-symbol-p dollars) #'>)) 0)))
    `(let ((,g!s ,s))
       (multiple-value-bind (m a) (,test ,g!s)
         (declare (ignorable a))
         (if m
           (let ((ml (ppcre:split (format nil "(~a)" m) ,g!s :with-registers-p t :limit 3)))
             (let ,#1=(append 
                        (mapcar (lambda (a1) `(,(lol:symb "$" a1) (xx ml ',a1))) '(\` & \'))
                        (mapcar (lambda (a1) `(,(lol:symb "$" a1) (aref a (1- ,a1)))) (loop for i from 1 to top collect i)))
               (declare (ignorable ,@(mapcar #'car #1#)))
               ,conseq))
           ,altern)))))

(defmacro whenmatch ((test s) &rest conseq) `(ifmatch (,test ,s) (progn ,@conseq)))
