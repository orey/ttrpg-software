;;;;=============================HEADER====================================
;;;; dice.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Utilities for dice rolling
;;;;=======================================================================

;(defpackage "DICE-UTILS"
;            (:use "COMMON-LISP")
;            (:nicknames "DICE")
;            (:export "rollDie" "test-rollDie"))

;(in-package dice-utils)

(load "utils.lisp")

(defparameter *NB* 10000)

;;;---------------------------------------DICE

(defun roll-die (faces)
  "Returns a random value corresponding to the number of faces chosen for the die"
  (+ 1 (random faces)))


(defun parse-combi (combi)
  "Parses a combination XdY+Z or XDY-Z and return a list of X, Y and Z (possibly negative)"
  (if (or (not (stringp combi)) (< (length combi) 2)) ; we accept d6 as a combi
      NIL
      (let ((lst (string-to-list combi))
            (xdone NIL) (ydone NIL)
            (xstr "") (ystr "") (zstr "") 
            (zsign 1)
            (x 0) (y 0) (z 0))
        (dotimes (i (length lst))
          (cond
            ;; case of d or D
            ((letterd-p (nth i lst))
             (if (string-equal xstr "") ; X is void so X=1
                 (setf xdone T
                       x 1)
                 (setf xdone T
                       x (parse-integer xstr))))
             ;; case of + or -
             ((or (plus-p (nth i lst)) (minus-p (nth i lst)))
              (progn
                (if (string-equal ystr "")
                    (return-from parse-combi NIL))
                (setf ydone T
                      y (parse-integer ystr))
                (if (minus-p (nth i lst))
                    (setf zsign -1))))
             ;; general case of parsing a char which is an int
             (T
              (if ydone
                  (setf zstr (concatenate 'string zstr (nth i lst)))
                  (if xdone
                      (setf ystr (concatenate 'string ystr (nth i lst)))
                      (setf xstr (concatenate 'string xstr (nth i lst))))))))
        (cond
          ((null xdone)
           (return-from parse-combi NIL))
          ((null ydone)
           (progn
             (if (string-equal ystr "")
                 (return-from parse-combi NIL))
             ;; there is no +/-Z
             (setf ydone T
                   y (parse-integer ystr))))
          ((if (string-equal zstr "")
               (setf z 0)
               (setf z (* zsign (parse-integer zstr))))))
        (list x y z))))


(defun roll-combi (str)
  "Launch dice to roll combis"
  (let* ((lst (parse-combi str))
         (rolls (make-list-variable (car lst) #'roll-die (cadr lst)))
         (newlist (append rolls (cddr lst))))
    (reduce #'+ newlist)))


(defun roll-four-keep-three(&optional verbose)
  "roll 4d6 and keep the best 3d6"
  (let* ((lst (make-list-variable 4 #'roll-die 6))
         (mymin (reduce #'min lst))
         (acc 0)
         (one-elem-removed nil))
    (if verbose (print lst))
    (if verbose (print mymin))
    (dotimes (i (length lst))
      (let ((elem (nth i lst)))
        (if (and (= elem mymin) (not one-elem-removed))
            (setf one-elem-removed T)
            (setf acc (+ acc elem)))))
    acc))
        
              
           
    
      
    


