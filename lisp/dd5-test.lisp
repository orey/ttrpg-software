;;;;=============================HEADER====================================
;;;; dd5-tests.lisp
;;;; Copyleft Olivier Rey 2024
;;;; Simplest PC group simulator
;;;;=======================================================================

(load "dd5.lisp")

(print "--------------------------------ability modifiers")
(dotimes (i 31)
  (format t "Ability: ~d - Modifier ~d~%" i (ability-modifier i)))

(print "--------------------------------roll-abilities 3D6 strategy")
(dotimes (i 4)
  (print (roll-abilities '3D6)))

(print "--------------------------------roll-abilities 4D6 strategy")
(dotimes (i 4)
  (print (roll-abilities '4D6)))

(print "--------------------------------roll-abilities default strategy")
(print (roll-abilities 'POLLUX))
(print (roll-abilities "blah"))
(print (roll-abilities 12))
(print (roll-abilities))

(print "--------------------------------Apply race")
(defparameter *pc* '(10 10 10 10 10 10))
(mapcar #'(lambda (x) (print x) (print (apply-race *pc* x)))
        '(human mountain-dwarf hill-dwarf high-elf wood-elf stout-halfling lightfoot-halfling))

(print "--------------------------------test-predicates")
(print "Expecting T T T NIL several times")
(print (dwarf-p 'mountain-dwarf))
(print (dwarf-p 'hill-dwarf))
(print (dwarf-p 'dwarf))
(print (dwarf-p 'elf))

(print (elf-p 'elf))
(print (elf-p 'high-elf))
(print (elf-p 'wood-elf))
(print (elf-p 'halfling))

(print (halfling-p 'halfling))
(print (halfling-p 'stout-halfling))
(print (halfling-p 'lightfoot-halfling))
(print (halfling-p 'dwarf))

(print (human-p 'human))

(print "--------------------------------racial traits")
(print (get-racial-traits 'mountain-dwarf))
(print (get-racial-traits 'hill-dwarf))
(print (get-racial-traits 'dwarf))
(print (get-racial-traits 'elf))

       



