;;;; simple-server.asd

(asdf:defsystem #:simple-server
  :serial t
  :components ((:file "simple-server")
               (:file 
                #+allegro "aserve-server"
                #+lispworks "aserve-server"
                #+sbcl "hunchentoot-server")))

