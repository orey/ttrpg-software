;=============================HEADER====================================
; utils-test.lisp
; Copyleft Olivier Rey 2024
; Utilities for dice rolling
;=======================================================================

(load "utils.lisp")

;;;---------------------------------------test-string-to-list
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


;;;---------------------------------------test-list-to-string
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

;;;---------------------------------------test-replace-elem-by-list
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
        
