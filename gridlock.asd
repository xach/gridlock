;;;; gridlock.asd

(asdf:defsystem #:gridlock
  :depends-on (#:flexi-streams
               #:grout
               #:cxml
               #:cxml-stp)
  :components ((:file "gridlock")))
