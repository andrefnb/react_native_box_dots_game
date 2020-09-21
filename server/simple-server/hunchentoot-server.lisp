;;; hunchentoot-server.lisp

;;; Define the SIMPLE-SERVER API: DEFROUTE, START-SERVER, STOP-SERVER


(defpackage #:hunchentoot-server
  (:use #:common-lisp #:hunchentoot #:simple-server))

(in-package :hunchentoot-server)


;;; Update history:
;;;
;;; 09-30-2014 Changed to port-based server API [CKR]
;;; 09-19-2014 Fixed method arg to get-response-function [CKR]
;;; 09-11-2014 Created file [CKR]


(defun start-server (&key (port 8000))
  (hunchentoot:start
   (set-server port 'new-server 'init-server 'end-server)))

(defun new-server (port)                    
  (make-instance 'hunchentoot:easy-acceptor 
    :port port 
    :document-root *root-dir*))

(defun init-server (server) server)

(defun end-server (server)
  (hunchentoot:stop server))

(defmacro defroute (method url fn)
  (let ((url-var '#:url))
    `(let ((,url-var ,url))
       (set-response-function ,method ,url-var ,fn)
       (hunchentoot:define-easy-handler 
        (,(gensym) :uri ,url-var) ()
        (funcall
           (get-response-function (request-method*) ,url-var) 
           (hunchentoot:raw-post-data :force-text t)
           (hunchentoot:get-parameters*))))))

(defun json-response (text)
  (let ((callback (get-parameter "callback")))
    (cond (callback 
           (set-content-type "application/javascript")
           (concatenate 'string callback "(" text ")"))
          (t 
           (set-content-type "application/json")
           text))))


;;; Add PUT to the methods for which Hunchentoot should collect body data
;;; [Needed? Note: Many browser use POST when you say PUT so test carefully
;;; before removing.]

(pushnew :put hunchentoot:*methods-for-post-parameters*)
