;;;;=============================HEADER====================================
;;;; dd5-tests.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Simplest PC group simulator
;;;;=======================================================================

(load "dd5.lisp")

(let ((pc (make-abilities :FOR 18 :DEX 12 :CON 15 :INT 10 :WIS 11 :CHA 9)))
  (print pc)
  (print (abilities-for pc)))

(dotimes (i 31)
  (format t "Ability: ~d - Modifier ~d~%" i (ability-modifier i)))

(print "--------------------------------3D6 strategy")
(print (roll-abilities '3D6))
(print (roll-abilities '3D6))
(print (roll-abilities '3D6))
(print (roll-abilities '3D6))

(print "--------------------------------4D6 strategy")
(print (roll-abilities '4D6))
(print (roll-abilities '4D6))
(print (roll-abilities '4D6))
(print (roll-abilities '4D6))

(print "--------------------------------default strategy")
(print (roll-abilities 'POLLUX))
(print (roll-abilities 'POLLUX))
(print (roll-abilities 'TOTO))
(print (roll-abilities 12))

(print "--------------------------------Apply race")
(defparameter *pc* '(10 10 10 10 10 10))
(mapcar #'(lambda (x) (print x) (print (apply-race *pc* x)))
        '(human mountain-dwarf hill-dwarf high-elf wood-elf stout-halfling lightfoot-halfling))
