;;; Neo4j requests and request handlers for them. Also database stuff is handled here.

(in-package :cl-neo4j.utils)

(defmacro with-neo4j-database ((host port user pass) &rest body)
  `(let ((*neo4j-host* ,host)
         (*neo4j-port* ,port)
         (*neo4j-user* ,user)
         (*neo4j-pass* ,pass))
     ,@body))

(defmacro with-request-handler (handler &body body)
  `(let ((*default-request-handler* (apply ,(car handler) ,(cdr handler))))
     ,@body
     (close-handler *default-request-handler*)))

(defvar *default-request-handler* 'basic-handler)

;; Requests and handlers

(defmacro def-neo4j-fun (name lambda-list method &rest args)
  `(progn
     (defun ,name (&key ,@lambda-list)
       (let ((uri ,(cadr (assoc :uri-spec args)))
             (json (encode-neo4j-json-payload ,@(aif (assoc :encode args)
                                                     (cdr it)
                                                     (list '() :string)))))
         (make-neo4j-request ,method uri json
                             (list ,@(mapcar (lambda (handler)
                                               `(list ,(car handler) (lambda (body uri json)
                                                                  ,(cadr handler))))
                                             (cdr (assoc :status-handlers args)))))))))

(defstruct (neo4j-request (:constructor %make-neo4j-request)
                          (:conc-name request-))
  method
  uri
  payload)

(defun make-neo4j-request (method uri payload error-handlers &key (request-handler *default-request-handler*))
  (handle-request request-handler (%make-neo4j-request :method method
                                                       :uri uri
                                                       :payload payload)
                   error-handlers))

(defgeneric send-request (handler request)
  (:documentation "Governs how handler sends the request."))

(defgeneric handle-request (handler request error-handlers)
  (:documentation "Main interface for the handlers, make-neo4j-request uses it.")
  (:method ((handler symbol) request error-handlers)
    (handle-request (funcall handler) request error-handlers)))

(defgeneric close-handler (handler)
  (:documentation "Closes the handler. Handler should do finalization operarions - batch handler sends the request at this point."))

(defclass basic-handler ()
  ((host :initarg :host :accessor handler-host)
   (port :initarg :port :accessor handler-port)
   (user :initarg :user :accessor handler-user)
   (pass :initarg :pass :accessor handler-pass)
   (database :initarg :db-name :accessor handler-db))
  (:documentation "Basic handler that just sends request to the database."))

(defun basic-handler (&key (host *neo4j-host*) (port *neo4j-port*)
                        (user *neo4j-user*) (pass *neo4j-pass*)
                        (db *neo4j-database*))
  (make-instance 'basic-handler
                 :host host
                 :port port
                 :user user
                 :pass pass
                 :db-name db))

(defmethod send-request ((handler basic-handler) request)
  (with-accessors ((method request-method) (uri request-uri) (payload request-payload))
      request
    (multiple-value-bind (body status)
        (http-request (format-neo4j-query (handler-host handler)
                                          (handler-port handler)
                                          uri
                                          :db-postfix (format nil "db/~A/" (handler-db handler)))
                      :method method
                      :basic-authorization (list (handler-user handler)
                                                 (handler-pass handler))
                      :content payload
                      :content-type (if payload "application/json")
                      :accept "application/json")
      (values status body))))

(defmethod handle-request ((handler basic-handler) request error-handlers)
  (multiple-value-bind (status body)
      (send-request handler request)
    (aif (assoc status error-handlers)
         (funcall (second it)
                  body
                  (request-uri request)
                  (request-payload request))
         (error 'unknown-return-type-error
                :uri (request-uri request)
                :status status))))

(defmethod close-handler ((handler basic-handler))
  (declare (ignore handler))
  t)
