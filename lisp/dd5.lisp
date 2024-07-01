;;;;=============================HEADER====================================
;;;; dd5.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Simplest PC group simulator
;;;;=======================================================================

(load "dice.lisp")

(defstruct abilities FOR DEX CON INT WIS CHA)

(defun ability-modifier (ab)
  (if (or (not (integerp ab)) (< ab 1))
      nil
      (floor (/ (- ab 10) 2))))

(defun roll-abilities(mode)
  (let ((lst nil))
    (cond
      ((eql mode '3D6)
       (setf lst (make-list-variable 6 #'roll-combi "3d6")))
       ((eql mode '4D6)
       (setf lst (make-list-variable 6 #'roll-four-keep-three)))
       (t (print "Keep on implementing cases!")))
    lst))

  





