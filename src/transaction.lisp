(in-package #:cl-neo4j.transaction)

;; {"statements": [{"statement":<cypher-query>, "parameters": {}}]}
(defun make-cypher-transaction (&rest statements)
  (make-instance 'cl-neo4j.utils:cypher-transaction :statements statements))

(defun make-cypher-query (statement &optional parameters)
  (let ((res (make-instance 'cl-neo4j.utils:cypher-query :statement statement)))
    (when parameters
      (setf (properties res) parameters))
    res))

(defun create-node-query (node-name node-type node-properties)
  (format nil "CREATE (~A:~A {~{~A:\"~A\"~^,~}})"
          node-name
          node-type
          node-properties))

(defun get-node-by-id-query (node-id)
  (format nil "MATCH (x) WHERE ID(x) = ~a RETURN x;" node-id))

(defun delete-node-query (node-id)
  (format nil "MATCH (x) WHERE ID(x) = ~a DETACH DELETE x;" node-id))

(defun create-relationship-query (src-node src-node-type src-node-props dst-node dst-node-type dst-node-props relationship-type properties)
  (format nil "MATCH (x:~a) WHERE x0MATCH (y:~a {~{~a:\"~a\"~^,~}}) MERGE (~a)-[~a {~{~a:\"~a\"~^,~}}]->(~a);"
          src-node-type
          src-node-props
          dst-node
          dst-node-type
          dst-node-props
          relationship-type
          properties))

(cl-neo4j.utils:def-neo4j-fun tx-create-node (node-id node-type node-properties)
  :post
  (:uri-spec (format nil "tx/commit"))
  (:encode (make-cypher-transaction
            (make-cypher-query (create-node-query node-id node-type node-properties))) :string)
  (:status-handlers
   (200 (cl-neo4j::decode-neo4j-json-output cl-neo4j::body))
   (404 (error 'node-not-found-error :uri cl-neo4j::uri))))

(cl-neo4j.utils:def-neo4j-fun tx-get-node (node-id)
  :post
  (:uri-spec (format nil "tx/commit"))
  (:encode (make-cypher-transaction
            (make-cypher-query (get-node-by-id-query node-id))) :string)
  (:status-handlers
   (200 (cl-neo4j::decode-neo4j-json-output cl-neo4j::body))
   (404 (error 'node-not-found-error :uri cl-neo4j::uri))))

(cl-neo4j.utils:def-neo4j-fun tx-delete-node (node-id)
  :post
  (:uri-spec (format nil "tx/commit"))
  (:encode (make-cypher-transaction
            (make-cypher-query (delete-node-query node-id))) :string)
  (:status-handlers
   (200 (cl-neo4j::decode-neo4j-json-output cl-neo4j::body))
   (404 (error 'node-not-found-error :uri cl-neo4j::uri))))
