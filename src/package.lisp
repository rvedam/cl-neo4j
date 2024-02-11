(in-package #:cl-user)

(defpackage #:cl-neo4j.utils
  (:use #:cl
        #:alexandria
        #:anaphora
        #:json
        #:json-rpc
        #:drakma)
  (:export
   ;; Conditions
   #:unknown-return-type-error
   #:invalid-data-sent-error
   #:node-not-found-error
   #:property-not-found-error
   #:unable-to-delete-node-error
   #:relationship-not-found-error
   #:index-not-found-error
   #:index-entry-not-found-error
   #:path-not-found-error
   ;; Macros
   #:def-neo4j-fun
   ;; query data structures
   #:cypher-query
   #:cypher-transaction
   ;; Vars
   #:*neo4j-host*
   #:*neo4j-port*
   #:*neo4j-user*
   #:*neo4j-pass*
   #:*neo4j-database*))

(defpackage #:cl-neo4j.deprecated
  (:use #:cl
        #:alexandria
        #:cl-neo4j.utils))

(defpackage #:cl-neo4j
  (:use
   #:cl
   #:cl-neo4j.utils
   #:alexandria
   #:anaphora
   #:json
   #:json-rpc
   #:drakma)
  (:export
   #:get-node
   #:create-node
   #:delete-node
   #:set-node-properties
   #:get-node-properties
   #:del-node-properties
   #:set-node-property
   #:get-node-property
   #:del-node-property
   #:get-relationship
   #:create-relationship
   #:set-relationship-properties
   #:get-relationship-properties
   #:del-relationship-properties
   #:set-relationship-property
   #:get-relationship-property
   #:del-relationship-property
   #:delete-relationship
   #:get-node-relationships
   #:get-relationships-types
   #:create-index
   #:delete-index
   #:add-to-index
   #:remove-from-index
   #:lookup-index
   #:query-index
   #:traverse
   #:get-path
   #:get-paths))

(defpackage #:cl-neo4j-wrapper
  (:use #:cl
        #:alexandria
        #:anaphora
        #:split-sequence
        #:cl-neo4j)
  (:export #:node-create
           #:node-get-by-id
           #:node-delete
           #:node-properties
           #:node-property
           #:node-relationships
           #:node-add-to-index
           #:node-remove-from-index
           #:node-query-index
           #:node-traverse

           #:relationship-create
           #:relationship-get-by-id
           #:relationship-delete
           #:relationship-start
           #:relationship-end
           #:relationship-type
           #:relationship-properties
           #:relationship-property
           #:relationship-add-to-index
           #:relationship-remove-from-index
           #:relationship-query-index
           #:relationship-traverse

           #:standard-node
           #:standard-relationship

           #:node-id
           #:relationship-id
           ;: Vars
           #:*default-node-constructor*
           #:*default-relationship-constructor*))

(defpackage #:cl-neo4j.transaction
  (:use #:cl
        #:alexandria
        #:anaphora)
  (:export
   ;; transaction
   #:tx-create-node
   #:tx-get-node
   #:tx-delete-node
   #:tx-relationship-create
   #:tx-relationship-get-by-id
   #:tx-relationship-delete))
