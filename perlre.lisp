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
  "imsxge modifiers"
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

;;;;;; geht auch
(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
				 (or
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\e (remove #\g ,,g!m)) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a)))
				 (or
           (ppcre:regex-replace (format nil "(?~a)~a" (remove #\e ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s ,(cadr ,g!a)))))))

;factor out
(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
				 (or
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\e (remove #\g ,,g!m)) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace-all (format nil "(?~a)~a"             (remove #\g ,,g!m)  ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls nil))
				 (or
           (ppcre:regex-replace (format nil "(?~a)~a" (remove #\e ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace (format nil "(?~a)~a"             ,,g!m  ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls nil))))))


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ziel ---------------------------------

;(defun rm-ch (stg chs) (ppcre:regex-replace-all (format nil "[~a]" chs) stg ""))
(defun rm-ch (chs stg) (ppcre:regex-replace-all (format nil "[~a]" chs) stg ""))


(defmacro xxx (ch sc)
;	(ppcre:regex-replace-all (format nil "(?~a)~a" (rm-ch ch ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls sc))        ; orig
``(ppcre:regex-replace-all (format nil "(?~a)~a" (rm-ch ch ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls sc)) 
;	``(ppcre:regex-replace-all (format nil "(?~a)~a" (rm-ch ,,ch ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,,sc))  ; ladet ok, geht aber nicht
;	`(ppcre:regex-replace-all (format nil "(?~a)~a" (rm-ch ,ch ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,sc)) 


(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
				 (or (xxx "eg" t) (xxx "g" nil))
				 (or (xxx "e" t) (xxx "" nil))))))


;---------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#


#|
(defun rem-chars (stg chs) (ppcre:regex-replace-all (format nil "[~a]" chs) stg ""))

#|
(defun rem-chars (chs) (ppcre:regex-replace-all (format nil "[~a]" chs) ,,g!m ""))

(rc ,,g!m "eg")
(rc ,,g!m "")

(defun rc (s c) (ppcre:regex-replace-all (format nil "[~a]" c) s ""))
|#

(defmacro xxx (chs-to-remove simple-call)
	``(ppcre:regex-replace-all (format nil "(?~a)~a" 
																	 ;																	(remove #\e (remove #\g ,,g!m))
																	 (rem-chars ,,g!m chs-to-remove)
																	 ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls simple-call))
;(rem-chars "abedgh" "ge")
|#





;(defmacro rep (x y &optional (z ppcre:regex-replace-all zp))
;	(let ((z (if zp 'ppcre:regex-replac ,z)))

#;(defmacro x (&rest y)
	`(if ,y 
			`(,@y ,,g!m) 
			,g!m))

#|
;das scheint zu gehen
(defmacro x ()
````,,g!m)

(defmacro x ()
``g!m)

(defmacro x (&rest y)
(if y 
	``(,,@y g!m)
``g!m))



(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
				 (or
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\e (remove #\g ,,g!m)) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace-all (format nil "(?~a)~a" (remove #\g ,,g!m) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls nil))
				 (or
           (ppcre:regex-replace (format nil "(?~a)~a" (x remove #\e) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls t)
					 (ppcre:regex-replace (format nil "(?~a)~a" (x) ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls nil))))))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro rep (x y z)
	`(,x (format nil "(?~a)~a" ,y ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,z))

(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
					 (or (rep ppcre:regex-replace-all (remove #\e (remove #\g ,,g!m)) t) (rep ppcre:regex-replace-all (remove #\g ,,g!m) nil))
           (or (rep ppcre:regex-replace (remove #\e ,,g!m) t) (rep ppcre:regex-replace ,,g!m nil))))))
;--------

(defmacro rep (x y &optional (z ppcre:regex-replace-all zp))
	(let ((z (or z ppcre:regex-replace)))
	`(,z (format nil "(?~a)~a" ,x ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,y)))


#;(defmacro rep (x y &optional (z 'ppcre:regex-replace-all zp))
	`(let ((z (if ,zp ppcre:regex-replace ,z)))
	``(,,z (format nil "(?~a)~a" ,,x ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,,y)))

#;(defmacro rep (x y &optional (z 'ppcre:regex-replace-all zp))
	`(let ((z (if (not ,zp) ppcre:regex-replace ,z)))
	``(,,,z (format nil "(?~a)~a" ,,,x ,(car ,g!a)) ,',g!s ,(cadr ,g!a) :simple-calls ,,,y)))

;  ,,g!m
#;(defmacro rep (x)
``(ppcre:regex-replace (format nil "(?~a)~a" ,,x ,(car ,g!a)) ,',g!s ,(cadr ,g!a))

#;(lol:defmacro! sub (o!a o!m)
 ``(lambda (,',g!s)
		 (if (string= "" ,,g!m)
			 (ppcre:regex-replace ,(car ,g!a) ,',g!s ,(cadr ,g!a))
			 (if (find #\g ,,g!m)
					 (or (rep (remove #\e (remove #\g ,,g!m)) t) (rep (remove #\g ,,g!m) nil))
           (or (rep (remove #\e ,,g!m) t t) (rep ,,g!m nil t))))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lol:defmacro! mat (o!a o!m)
  ``(lambda (,',g!s)
      (if (string= "" ,,g!m)
        (ppcre:scan-to-strings ,(car ,g!a) ,',g!s)
        (ppcre:scan-to-strings (format nil "(?~a)~a" ,,g!m ,(car ,g!a)) ,',g!s))))

;(ql:quickload :perlre :verbose t)   ; 2 style warnings <--------
;; The variable C is defined but never used.
;; The variable N is defined but never used.
;SET-DISPATCH-MACRO-CHARACTER
; so ok
(set-dispatch-macro-character #\# #\~
  (lambda (s c n) (declare (ignore c n))
    (case (read-char s)
      (#\m (mat (segment-reader s (read-char s) 1) (mods s)))
      (#\s (sub (segment-reader s (read-char s) 2) (mods s)))
      (t (error "Unknown #~~ mode character")))))

; 22.10.15 -----------------
; prune-if-match-bodies-from-sub-lexical-scope seems to be removed from let-over-lambda,
; this is from a previous version
#;(defun prune-if-match-bodies-from-sub-lexical-scope (tree)
  (if (consp tree)
    (if (or (eq (car tree) 'ifmatch) (eq (car tree) 'whenmatch))
      (cddr tree)
      (cons (prune-if-match-bodies-from-sub-lexical-scope (car tree))
            (prune-if-match-bodies-from-sub-lexical-scope (cdr tree))))
    tree))
;--------------------------- 

#|
(lol:defmacro! ifmatch ((test s) conseq &optional altern)
;  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten (lol:prune-if-match-bodies-from-sub-lexical-scope conseq)))))
  (let* ((dollars (remove-duplicates (remove-if-not #'lol:dollar-symbol-p (lol:flatten (prune-if-match-bodies-from-sub-lexical-scope conseq)))))
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
|#

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



#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 22.10.15 new lol
;;----
;;;;;; NEW CODE FOR ANTIWEB
#+cl-ppcre
(defun dollar-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 1)
       (string= (symbol-name s)
                "$"
                :start1 0
                :end1 1)
       (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))

(defmacro! if-match ((match-regex str) then &optional else)
  (let* ((dollars (remove-duplicates
                   (remove-if-not #'dollar-symbol-p
                                  (flatten then))))
         (top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>))
                  0)))
    `(multiple-value-bind (,g!matches ,g!captures) (,match-regex ,str)
       (declare (ignorable ,g!matches ,g!captures))
       (let ((,g!captures-len (length ,g!captures)))
         (declare (ignorable ,g!captures-len))
         (symbol-macrolet ,(mapcar #`(,(symb "$" a1)
                                       (if (< ,g!captures-len ,a1)
                                           (error "Too few matchs: ~a unbound." ,(mkstr "$" a1))
                                           (aref ,g!captures ,(1- a1))))
                                   (loop for i from 1 to top collect i))
           (if ,g!matches
               ,then
               ,else))))))


(defmacro when-match ((match-regex str) &body forms)
  `(if-match (,match-regex ,str)
     (progn ,@forms)))

;

|#
