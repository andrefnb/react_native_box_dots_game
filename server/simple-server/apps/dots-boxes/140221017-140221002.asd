;;;; dots-boxes.asd

(asdf:defsystem #:140221017-140221002
  :serial t
  :depends-on ("simple-server" "cl-json" "drakma")
  :components ((:file "dots-boxes")(:file "alfabeta")))

