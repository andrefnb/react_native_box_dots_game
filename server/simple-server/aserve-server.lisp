;;; aserve-server.lisp

;;; Define the SIMPLE-SERVER API: DEFROUTE, START-SERVER, STOP-SERVER


(defpackage #:aserve-server
  (:use #:common-lisp #:net.aserve #:simple-server))

(in-package :aserve-server)


;;; Update history:
;;;
;;; 09-30-2014 Changed to port-based server API [CKR]
;;; 09-19-2014 Fixed method arg to get-response-function [CKR]
;;; 09-15-2014 Created file [CKR]

(defun start-server (&key (port 8000))
  (start :port port 
         :server (set-server port 'new-server 'init-server 'end-server)))

;;; use allegoserve's pre-initialized server the first time
;;; otherwise create a new one and make it the default

(defun new-server (port)
  (if (and (null *port-servers*) *wserver*)
      *wserver*
    (setq *wserver* (make-instance 'wserver))))

;;; do I need to clear old locator?
(defun init-server (server)
  (publish-directory :server server :prefix "/" 
                     :destination (namestring *root-dir*)))

(defun end-server (server)
  (shutdown :server server))

;;; Macro because HUNCHENTOOT needs that

(defmacro defroute (method url fn)
  (let ((url-var '#:url))
    `(let ((,url-var ,url))
       (set-response-function ,method ,url-var ,fn)
       (publish :path ,url-var :content-type "application/json" 
                :function (lambda (req resp)
                            (let ((callback (get-callback req)))
                              (with-http-response (req resp :content-type (json-content-type callback))
                                (with-http-body (req resp)
                                  (write-string 
                                   (json-text (funcall (get-response-function (request-method req) ,url-var)
                                                       (get-request-body req) 
                                                       (request-query req :post nil))
                                              callback)
                                   (request-reply-stream req))))))))))

(defun get-callback (req)
  (request-query-value "callback" req))

(defun json-content-type (callback)
  (if callback "application/javascript" "application/json"))

(defun json-text (text callback)
  (if callback (concatenate 'string callback "(" text ")") text))

