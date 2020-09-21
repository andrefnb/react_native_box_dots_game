;;; simple-server.lisp
;;;
;;; Base for the EECS 325 Simple Server
;;;
;;; Defines STOP-SERVER and PARAM-VALUE
;;; Needs server-specific implementations of DEFROUTE and START-SERVER

;;; Update history:
;;;
;;; 09-30-2014 Enable multiple servers on different ports [CKR]
;;; 09-15-2014 Created file [CKR]

-
(defpackage #:simple-server
  (:use #:common-lisp)
  (:export 
   ;; defined for webapps to use
   #:param-value #:stop-server
   
   ;; defined for server implementations to use
   #:get-response-function #:set-response-function #:set-server
   #:*port-servers* #:*root-dir*
      
   ;; must be defined by server implementations
   #:defroute #:start-server
   ))

(in-package :simple-server)

;;; PUBLIC
;;; for server implementations

;;; list of (port server end-server-fn) triples

(defvar *port-servers* nil)

(defun stop-server (&key port)
  (if (null port)
      (mapc 'end-server *port-servers*)
    (end-server (get-server port))))

(defun get-server (port)
  (entry-server (assoc port *port-servers*)))

(defun set-server (port make-fn init-fn end-fn)
  (let ((server (or (get-server port)
                    (add-server port (funcall make-fn port) end-fn))))
    (funcall init-fn server)
    server))

;;; PRIVATE

(defun entry-port (entry) (car entry))
(defun entry-server (entry) (cadr entry))
(defun entry-end-fn (entry) (caddr entry))

(defun end-server (entry)
  (funcall (entry-end-fn entry) (entry-server entry)))

(defun add-server (port server end-fn)
  (push (list port server end-fn) *port-servers*)
  server)


;;; Where "/" maps to for static HTML assets. Optional.

(setq *root-dir* (asdf:system-relative-pathname "simple-server" "apps/"))


;;; A table of method + url => function mappings

(defvar *route-map* (make-hash-table :test 'equal))

(defun get-response-function (method url)
  (let ((fn (gethash (list method url) *route-map*)))
    (assert fn (fn) "No response function for ~S ~S" method url)
    fn))

(defun set-response-function (method url fn)
  (setf (gethash (list method url) *route-map*) fn))

(defun param-entry (param alist)
  (assoc param alist :test #'string-equal))

(defun param-value (param alist &optional default)
  (let ((entry (param-entry param alist)))
    (if entry (cdr entry) default)))


;;; SIMPLE SERVER API
;;;
;;; Specific servers need to implement the following.

;;; (START-SERVER [:port 8000] [:root *root-dir*]) => server
;;;   Starts a web server on the given port. 
;;; (STOP-SERVER [server])
;;;   Shuts down the server. 
;;;
;;; "/" should map to assets at root, if no get-response-function match is found

;;; (DEFROUTE method url function)
;;;    Assocates the method and URL with the function. Function should
;;;    evaluate to a function or function name that
;;;      - takes a string containing data, e.g., JSON, and an alist of request
;;;        query parameters
;;;      - returns a string

;;; The server implementation should make sure that
;;;   - the function is called when the given HTTP method and url are received
;;;   - the data argument is set to the request body and the parameter alist 
;;;     to any query parameters
;;;   - SIMPLE-SERVER:PARAM-VALUE should correctly retrieve values from
;;;     the parameter alist for a GET HTTP call
;;;   - a JSON parser should get the correct request body for a PUT or POST
;;;     from an AJAX client call
;;;   - if a JSONP callback is passed, the JSON object should be wrapped in 
;;;     callback(obj) and the content-type should be "application/javascript", otherwise
;;;     the content-type should be "application/json" 