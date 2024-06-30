;=============================HEADER====================================
; dice-test.lisp
; Copyleft Olivier Rey 2024
; Utilities for dice rolling
;=======================================================================

(load "dice.lisp")

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


(defun test-string-to-list ()
  (print "---------------------------test-string-to-list")
  (print "Test with 2")
  (print (string-to-list 2))
  (print "Test with nil")
  (print (string-to-list nil))
  (print "Test with empy string")
  (print (string-to-list ""))
  (print "Test with a")
  (print (string-to-list "a"))
  (print "Test with ab")
  (print (string-to-list "ab"))
  (print "Test with abc")
  (print (string-to-list "abc"))
  (print "Test with abcd")
  (print (string-to-list "abcd")))

(test-string-to-list)

(defun test-list-to-string ()
  (print "---------------------------test-list-to-string")
  (print "Test with nil")
  (print (list-to-string nil))
  (print "Test with 2")
  (print (list-to-string 2))
  (print "Test with (2)")
  (print (list-to-string '(2)))
  (mapcar #'(lambda (lst)
              (print "Test with")
              (print lst)
              (print (list-to-string lst)))
          '(("") ("a") ("a" "b") ("a" "b" "c") ("a" "b" "c" "def"))))
  
(test-list-to-string)


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


(defun test-roll-combi ()
  (print "---------------------------test-roll-combi")
  (mapcar #'(lambda (str)
              (print (concatenate 'string "Test with " str))
              (print (parse-combi str))
              (print (roll-combi str)))
          '("d20" "D20+4" "d20-2" "3d10+44" "2d8-1" "6d6")))

(test-roll-combi)

(defun test-replace-elem-by-list ()
  (print "---------------------------test-replace-elem-by-list")
  (print "Test with '(0 1 2 3 4 5 6) 3 '(a b c)")
  (print (replace-elem-by-list '(0 1 2 3 4 5 6) 3 '(a b c)))
  (print "Test with '(0 1 2 3 4 5 6) 0 '(a b c)")
  (print (replace-elem-by-list '(0 1 2 3 4 5 6) 0 '(a b c)))
  (print "Test with '(0 1 2 3 4 5 6) 6 '(a b c)")
  (print (replace-elem-by-list '(0 1 2 3 4 5 6) 6 '(a b c)))
  (print "Test with '(0 1 2 3 4 5 6) 18 '(a b c)")
  (print (replace-elem-by-list '(0 1 2 3 4 5 6) 18 '(a b c))))
      
(test-replace-elem-by-list)
        

