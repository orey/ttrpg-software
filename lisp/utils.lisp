;;;;=============================HEADER====================================
;;;; utils.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Utilities for dice rolling
;;;;=======================================================================


;;;---------------------------------------UTILS

(defun string-to-list (str)
  "Transforms a string in a list of strings of one char"
  (if (or (not (stringp str)) (= 0 (length str)))
      NIL
      (if (= 1 (length str))
          (cons (subseq str 0 1) nil)
          (cons (subseq str 0 1) (string-to-list (subseq str 1 (length str)))))))


(defun list-to-string (lst)
  "Transforms a list of strings in a big string"
  (if (null lst)
      NIL
      (if (or (not (listp lst)) (not (stringp (car lst))))
          NIL
          (concatenate 'string (car lst) (list-to-string (cdr lst))))))


(defun make-list-variable (size f &rest args)
  "Creates a list with eval of f and args for each member"
  (let ((lst (make-list size)))
    (dotimes (i (length lst))
      (setf (nth i lst) (apply f args)))
    lst))


(defun replace-elem-by-list (source index lst)
  "Replaces elem in source list at index by another list (index start at 0)"
  (let ((outlst NIL))
    (do  ((i (- (length source) 1) (- i 1)))
         ((= -1 i))
      (if (= i index)
          (do ((j (- (length lst) 1) (- j 1)))
              ((= -1 j))
            (setf outlst (cons (nth j lst) outlst)))
          (setf outlst (cons (nth i source) outlst))))
    outlst))
 

;;;---------------------------------------PREDICATES

(defun plus-p (char)
  (if (string-equal char "+") T NIL))

(defun minus-p (char)
  (if (string-equal char "-") T NIL))

(defun letterd-p (char)
  (if (string-equal (string-upcase char) "D") T NIL))

