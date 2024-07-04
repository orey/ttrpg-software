;=============================HEADER====================================
; dice-test.lisp
; Copyleft Olivier Rey 2024
; Utilities for dice rolling
;=======================================================================

(load "dice.lisp")


;;;---------------------------------------test-roll-die
(defun test-roll-die (dice)
  "Throws NB times a dice with 'dice' faces"
  (let ((value 0))
    (dotimes (i *NB*)
      (setf value (+ value (roll-die dice))))
    (coerce (/ value *NB*) 'float)))


; Test on common dice
(let ((list-dice '(4 6 8 10 12 20 30 100)))
  (print "---------------------------test-roll-die")
  (print (mapcar #'test-roll-die list-dice)))


;;;---------------------------------------test-parse-combi
(defun test-parse-combi ()
  (print "---------------------------test-parse-combi")
  (print "Test with void chain")
  (print (parse-combi ""))
  (print "Test with 2")
  (print (parse-combi 2))
  (mapcar #'(lambda (str)
              (print (concatenate 'string "Test with " str))
              (print (parse-combi str)))
          '("22+1" "22d" "d20" "D20+4" "d20-2" "3d10+44" "2d8-1" "6d6")))

(test-parse-combi)


;;;---------------------------------------test-roll-combi
(defun test-roll-combi ()
  (print "---------------------------test-roll-combi")
  (mapcar #'(lambda (str)
              (print (concatenate 'string "Test with " str))
              (print (parse-combi str))
              (print (roll-combi str)))
          '("d20" "D20+4" "d20-2" "3d10+44" "2d8-1" "6d6")))

(test-roll-combi)


;;;---------------------------------------test-roll-four-keep-three
(defun test-roll-four-keep-three ()
  (print "---------------------------test-roll-four-keep-three")
  (print (roll-four-keep-three T)))

(dotimes (i 6)
  (test-roll-four-keep-three))

;;;---------------------------------------test-draw-between
(defparameter *min* 50)
(defparameter *max* 100)
(defparameter *nb* 1000)
(let ((average (/ (+ *min* *max*) 2))
      (temp 0)
      (acc 0.0))
  (format t "~%---------------------------test-draw-between~%")
  (dotimes (i *nb*)
    (progn
      (setf temp (draw-between *min* *max*))
      (format t "~d|" temp)
      (setf acc (+ acc temp))))
  (format t "~%Theoretical average: ~d~%" average)
  (format t "Average: ~d~%" (/ acc *nb*)))
