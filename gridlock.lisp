;;;; gridlock.lisp

(defpackage #:gridlock
  (:use #:cl
        #:stp)
  (:export #:rows
           #:save-updated-row
           #:column-value)
  (:shadowing-import-from #:cxml
                          #:with-xml-output
                          #:with-namespace
                          #:with-element
                          #:with-element*
                          #:attribute
                          #:attribute*
                          #:text))

(in-package #:gridlock)

(defparameter *spreadsheet-list-url*
  "http://spreadsheets.google.com/feeds/spreadsheets/private/full")

(defun spreadsheet-list-xml ()
  (grout:authenticated-request *spreadsheet-list-url*))

(defclass row ()
  ((id
    :initarg :id
    :accessor id)
   (title
    :initarg :title
    :accessor title)
   (edit-url
    :initarg :edit-url
    :accessor edit-url)
   (etag
    :initarg :etag
    :accessor etag)
   (data
    :initarg :data
    :accessor data)))

(defmethod print-object ((row row) stream)
  (print-unreadable-object (row stream :type t)
    (format stream "~A" (title row))))

(defun map-matches (pred fun source)
  (let ((result '()))
    (map-recursively (lambda (node)
                       (when (funcall pred node)
                         (push (funcall fun node) result)))
                     source)
    (nreverse result)))

(defun column-value (row column)
  (cdr (assoc column (data row) :test #'string=)))

(defun (setf column-value) (new-value row column)
  (let ((cons (assoc column (data row) :test #'string=))
        (value (princ-to-string new-value)))
    (if cons
        (setf (cdr cons) value)
        (push (cons column value)
              (data row)))
    new-value))

(defun attribute-matches (&key name (ns "") value)
  (if value
      (lambda (node)
        (equal (attribute-value node name ns) value))
      (lambda (node)
        (not (null (attribute-value node name ns))))))

(defun all-attributes-match (attributes)
  (let ((funs (mapcar (lambda (attributes)
                        (destructuring-bind (name &optional value (ns ""))
                            attributes
                          (attribute-matches :name name :value value
                                             :ns ns)))
                      attributes)))
    (lambda (node)
      (every (lambda (fun) (funcall fun node)) funs))))


(defun node-matches (&key name (ns "") attributes)
  (let ((attributes-fun (if attributes
                            (all-attributes-match attributes)
                            (constantly t))))
    (lambda (node)
      (and (typep node 'element)
           (or (not name) (equal (local-name node) name))
           (equal (namespace-uri node) ns)
           (funcall attributes-fun node)))))

(defun node-text (node)
  (with-output-to-string (stream)
    (do-children (child node)
      (write-string (data child) stream))))

(defun first-text (pred source)
  (map-recursively (lambda (node)
                     (when (funcall pred node)
                       (return-from first-text (node-text node))))
                   source))

(defun first-attribute (pred source attribute-name &optional (attribute-ns ""))
  (map-recursively (lambda (node)
                     (when (funcall pred node)
                       (return-from first-attribute
                         (attribute-value node attribute-name attribute-ns))))
                   source))


(defvar *googledata-ns* "http://schemas.google.com/g/2005")
(defvar *atom-ns* "http://www.w3.org/2005/Atom")
(defvar *gsx-ns* "http://schemas.google.com/spreadsheets/2006/extended")

(defun entry-row (source)
  (let ((etag (stp:attribute-value source "etag" *googledata-ns*))
        (edit-url (first-attribute (node-matches :name "link"
                                                 :ns *atom-ns*
                                                 :attributes '(("rel" "edit")))
                                   source
                                   "href"))
        (id (first-text (node-matches :name "id"
                                      :ns *atom-ns*)
                        source))
        (title (first-text (node-matches :name "title"
                                         :ns *atom-ns*)
                           source))
        (data (map-matches (node-matches :ns *gsx-ns*)
                              (lambda (node)
                                (cons (local-name node)
                                      (node-text node)))
                              source)))
    (make-instance 'row
                   :id id
                   :title title
                   :edit-url edit-url
                   :etag etag
                   :data data)))


(defun source-rows (source)
  (map-matches (node-matches :name "entry" :ns *atom-ns*)
               (lambda (node)
                 (entry-row node))
               source))

(defun serialize (row)
  (with-xml-output (cxml:make-string-sink)
    (with-namespace ("" *atom-ns*)
      (with-namespace ("gsx" *gsx-ns*)
        (with-namespace ("gd" *googledata-ns*) 
          (with-element "entry"
            (attribute* "gd" "etag" (etag row))
            (with-element "id"
              (text (id row)))
            (loop for (name . value) in (data row)
                  do
                  (with-element* ("gsx" name)
                    (text value)))))))))

(defun save-updated-row (row)
  (grout:authenticated-request (edit-url row)
                               :method :put
                               :content (flexi-streams:string-to-octets
                                         (serialize row)
                                         :external-format :utf8)
                               :content-type "application/atom+xml"))

(defun rows (uri)
  (let* ((data (grout:authenticated-request uri))
         (source (cxml:parse data (make-builder))))
    (source-rows source)))

(defun make-source (thing)
  (cxml:parse thing (make-builder)))

(defun refresh (row)
  (let ((source (make-source (grout:authenticated-request (edit-url row)))))
    (entry-row (find-child-if (node-matches :name "entry"
                                            :ns *atom-ns*)
                              source))))

(defun first-entry (source)
  (first (filter-recursively (node-matches :name "entry"
                                           :ns *atom-ns*)
                             source)))
