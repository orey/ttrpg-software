;;;;=============================HEADER====================================
;;;; dd5.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Simplest PC group simulator
;;;;=======================================================================

(load "dice.lisp")

;;; Abilities are lists of FOR DEX CON INT WIS CHA


(defun ability-modifier (ab)
  "Defines the ability modifiers"
  (if (or (not (integerp ab)) (< ab 1))
      nil
      (floor (/ (- ab 10) 2))))


(defun roll-abilities(mode)
  "Roll abilities with various strategies"
  (let ((lst nil))
    (cond
      ((eql mode '3D6)
       (setf lst (make-list-variable 6 #'roll-combi "3d6")))
      ((eql mode '4D6)
       (setf lst (make-list-variable 6 #'roll-four-keep-three)))
      (t
       (print "Keeping the default ability rolls")
       (setf lst '(15 14 13 12 10 8))))
    (sort lst #'>)))


(defun apply-race(abs race subrace)
  "takes an ability struct and applies ability bonuses"
  (let ((newlst nil))
    (cond
      ((eq race 'human)
       (setf newlst (mapcar #'+ '(1 1 1 1 1 1) abs)))
      ((eq race 'dwarf)
       (cond ((eql subrace 'mountain)
              (setf newlst (mapcar #'+ '(2 0 2 0 0 0) abs)))
             ((eql subrace 'hill)
              (setf newlst (mapcar #'+ '(0 0 2 0 1 0) abs)))))
      ((eq race 'elf)
       (cond ((eql subrace 'high)
              (setf newlst (mapcar #'+ '(0 2 0 1 0 0) abs)))
             ((eql sbrace 'wood)
              (setf newlst (mapcar #'+ '(0 2 0 0 1 0) abs)))))
      ((eql race 'halfling)
       (cond ((eql subrace 'stout)
              (setf newlst (mapcar #'+ '(0 2 1 0 0 0) abs)))
             ((eql subrace lightfoot)
              (setf newlst (mapcar #'+ '(0 2 0 0 0 1) abs)))))
      (t
       (format t "Unrecognized option~%")))
    newlst))

(defun get-racial-traits (race)
  " get racial traits:
    age, alignment, size, speed, language, subrace"
  (msg "ongoin"))




