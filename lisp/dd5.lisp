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


(defun roll-abilities(&optional mode)
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


(defun apply-race(abs race)
  "takes an ability struct and applies ability bonuses"
  (let ((newlst))
    (cond
      ((eq race 'human)
       (setf newlst (mapcar #'+ '(1 1 1 1 1 1) abs)))
      ((eq race 'mountain-dwarf)
       (setf newlst (mapcar #'+ '(2 0 2 0 0 0) abs)))
      ((eq race 'hill-dwarf)
       (setf newlst (mapcar #'+ '(0 0 2 0 1 0) abs)))
      ((eq race 'high-elf)
       (setf newlst (mapcar #'+ '(0 2 0 1 0 0) abs)))
      ((eq race 'wood-elf)
       (setf newlst (mapcar #'+ '(0 2 0 0 1 0) abs)))
      ((eq race 'stout-halfling)
       (setf newlst (mapcar #'+ '(0 2 1 0 0 0) abs)))
      ((eq race 'lightfoot-halfling)
       (setf newlst (mapcar #'+ '(0 2 0 0 0 1) abs)))
      (t
       (format t "Try another option~%")))
    newlst))


(defun dwarf-p (x)
  (or (eql x 'mountain-dwarf) (eql x 'hill-dwarf) (eql x 'dwarf)))

(defun elf-p (x)
  (or (eql x 'high-elf) (eql x 'wood-elf) (eql x 'elf)))

(defun halfling-p (x)
  (or (eql x 'stout-halfling) (eql x 'lightfoot-halfling) (eql x 'halfling)))

(defun human-p (x)
  (eql x 'human))


(defun get-racial-traits (race)
  " get racial traits:
    age, alignment, size, speed, language"
  (let ((res nil))
    (cond ((dwarf-p race)
           (setf res (list (draw-between 50 100) 'lawful-good 1.5 'medium '(common dwarwish))))
          (t
           (print "reprendre ici")))
    res))




