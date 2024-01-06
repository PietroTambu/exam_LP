;;;; -*- Mode: Lisp -*-

;;;; ool.lisp
;;;;
;;;; Tamburini Pietro 894628
;;;;

;;; Global variables
(defparameter *classes-specs* (make-hash-table))
(defvar *instance-id-counter* 0)
(defvar *instance-map* (make-hash-table))
(defvar *method-map* (make-hash-table :test #'equal))

;;; Add key:value to *classes-specs*
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

;;; Retrieve value from *classes-specs*
(defun class-spec (name)
  (gethash name *classes-specs*))

;;; Add method to key:value *method-map*
(defun define-method (class-name method-name method-lambda)
  (setf (gethash (list class-name method-name) *method-map*) method-lambda))

;;; Add a value to *instance-map*
(defun add-instance (instance)
  (incf *instance-id-counter*)
  (setf (gethash *instance-id-counter* *instance-map*) instance)
  *instance-id-counter*)

;;; Retrieve instance from *instance-map*
(defun get-instance (id)
  (gethash id *instance-map*))

;;; Checks the type compatibility of each field in a list of fields.
;;; Raises an error if a field's value does not match its declared type.
(defun check-fields-types (fields)
  (let ((errors (mapcan (lambda (field)
                          (let ((field-name (first field))
                                (field-value (second field))
                                (field-type (third field)))
                            (when (and (> (length field) 2)
                                       (not (or (class-spec field-type)
                                                (ignore-errors (typep field-value field-type)))))
                              (list (format nil "Error: field value ~A (~A) doesn't match type (~A)"
                                            field-name field-value field-type)))))
                        fields)))
    (unless (null errors)
      (error (format nil "~{~A~^, ~}" errors)))))

;;; Retrieves the attributes of a defined class
(defun get-attributes (class-name)
    (let ((attributes (third (class-spec class-name))))
        attributes))

;;; Retrieves the methods of a defined class
(defun get-methods (class-name)
    (let ((methods (fourth (class-spec class-name))))
        methods))

;;; Retrieves all parent classes of a given class, including indirect ancestors,
;;; by recursively traversing the class hierarchy. (left-handed)
(defun get-parents (class-name)
  (let ((class-specification (class-spec class-name)))
    (when class-specification
      (let ((parents (second class-specification)))
        (nconc (copy-list parents)
               (mapcan #'get-parents parents))))))

;;; Generates a sorted list of unique parent classes for a list of classes, 
;;; removing any duplicates.
(defun get-parents-list-sorted (classes-name)
  (remove-duplicates (mapcan #'get-parents classes-name)
                     :from-end t))

;;; Ensures each field in a list has three elements: name, value, and a type. 
;;; Adds default values and types if none are provided.
(defun ensure-field-types (fields)
  (mapcar (lambda (field)
            (cond ((= (length field) 1) (append field '(NIL T)))
                  ((= (length field) 2) (append field '(T)))
                  (t field)))
          fields))

;;; The pair-up function transforms a flat list into a list of pairs.
;;; It recursively pairs every two consecutive elements of the input list.
(defun pair-up (lst)
  (if (null lst)
      nil
      (cons (list (car lst) (cadr lst))
            (pair-up (cddr lst)))))  

;;; Validates that field types in a class are subtypes of the same fields in
;;; its parent classes, raising an error for any type mismatches.
(defun check-subtype-integrity (parents fields)
  (mapc (lambda (parent)
          (mapc (lambda (attribute)
                  (let ((matching-attribute 
                         (find (first attribute) fields :key #'first)))
                    (unless (or (not matching-attribute)
                                (subtypep (third matching-attribute)
                                          (third attribute)))
                      (error "Error: type of slot ~S is a supertype of 
                              inherited type" (first attribute)))))
                (get-attributes parent)))
        (append parents (get-parents-list-sorted parents))))

;;; Links a method to its implementation based on class and method names,
;;; handling method invocation or signaling an error if the method is not found.
(defun method-linker (method-name)
  (setf (fdefinition method-name)
        (lambda (&rest params)
          (let ((class-name (first (get-instance (first params)))))
            (let ((method-lambda 
                   (gethash (list class-name method-name) *method-map*)))
              (if method-lambda
                  (apply method-lambda params)
                  (error "Error: Method ~A not found in class ~A" 
                         method-name class-name)))))))

;;; Collects attributes from parent classes not present in 'parts', using a 
;;; specified function to retrieve attributes from each parent.
(defun get-missing-parts (get-parts-function parents parts)
  (mapcan (lambda (parent)
            (mapcan (lambda (attribute)
                      (unless (find (first attribute) parts :key #'first)
                        (list attribute)))
                    (funcall get-parts-function parent)))
          (nconc parents (get-parents-list-sorted parents))))

;;; (def-class <class-name> <parents> <part>*) => <class-name | error>
;;;
;;; Defines a new class with fields and methods, including inherited ones,
;;; and performs type checks and integrity validations.
(defun def-class (class-name parents &rest parts)
  (let* ((fields (mapcan (lambda (part)
                           (when (eq (first part) 'fields)
                             (rest part)))
                         parts))
         (methods (mapcan (lambda (part)
                            (when (eq (first part) 'methods)
                              (rest part)))
                          parts))
         (final-fields (ensure-field-types
                        (append fields
                                (get-missing-parts 'get-attributes parents
                                                   fields))))
         (final-methods (append methods
                                (get-missing-parts 'get-methods parents
                                                   methods))))

    (check-subtype-integrity parents fields)
    (check-fields-types fields)
    (add-class-spec class-name `(,class-name ,parents 
                                 ,final-fields ,final-methods))
    class-name))

;;; (make <class-name> [<field-name> <value>]*) => <instance | error>
;;;
;;; Creates an instance of a specified class, handling field initialization, 
;;; method linking, and validation of field types and attributes.
(defun make (class-name &rest parts)
  (unless (class-spec class-name)
    (error "Error: class ~A doesn't exist" class-name))

  (let* ( (classes-chain (append (get-parents-list-sorted (list class-name))
                                (list class-name)))
          (class-fields (get-attributes class-name))
          (fields (pair-up parts))
          (missing-fields (remove-duplicates
                            (mapcan (lambda (class)
                              (mapcan (lambda (attribute)
                                (unless (find (first attribute)
                                              fields :key #'first)
                                        (list (list (first attribute)
                                                    (second attribute)))))
                                      (get-attributes class)))
                              classes-chain)
                            :key #'first :test #'equal)))
        
        (mapc (lambda (field)
                (let ((class-field (find (first field)
                                          class-fields :key #'first)))
                  (unless class-field
                    (error "Error: Illegal attribute ~A found" (first field)))
                  (unless (typep (second field) (third class-field))
                    (error "Error: value ~S for field ~S is not of type ~S."
                          (second field) (first class-field)
                          (third class-field)))))
              fields)

        (mapc (lambda (missing-field)
                (unless (second missing-field)
                  (error "Error: You must provide a value for field ~A." 
                        missing-field)))
              missing-fields)

        (mapc (lambda (method)
                (let* ((method-name (first method))
                      (method-params (cons 'this (second method)))
                      (method-body (third method)))
                  (define-method class-name method-name
                    (eval `(lambda ,method-params ,method-body)))
                  (method-linker method-name)))
              (get-methods class-name))

        (add-instance (list class-name (append fields missing-fields)
                            (get-methods class-name)))))

;;; (is-class <class-name>) => T/NIL
;;;
;;; Checks if a given class name corresponds to a defined class.
(defun is-class (class-name)
  (not (null (class-spec class-name))))

;;; (is-instance <value> [<class-name>]) => T/NIL
;;; 
;;; Determines if a value is an instance of given parent class/es.
(defun is-instance (instance &optional (class-names 'T))
  (and (not (null (get-instance instance)))
       (or (eq class-names 'T)
           (let ((parents (get-parents (first (get-instance instance)))))
             (if (listp class-names)
                 (every (lambda (class-name) 
                          (member class-name parents :test #'eql))
                        class-names)
                 (member class-names parents :test #'eql))))))


;;; (field <instance> <field-name>) => <field-value | error>
;;;
;;; Retrieves the value of a specified field from an instance,
;;; raising an error if the field is not found.
(defun field (instance field-name)
  ;; Valuta instance se non è un numero
  (let ((real-instance (if (numberp instance)
                           instance
                           (symbol-value instance))))
    (let ((field-found (find field-name
                             (second (get-instance real-instance))
                             :key #'first)))
      (unless field-found
        (error "Error: unknown field."))
      (second field-found))))


;;; (field* <instance> <field-name>+) => <field-value | error>
;;; Extended version of field.
;;; Allows accessing nested fields within an instance
;;; or its related instances.
(defun field* (instance field-names)
  (if field-names
      (if (= (length field-names) 1)
          (field instance (first field-names))
          (field* (field instance (first field-names)) (rest field-names)))
      (error "Error: no field names provided")))


(def-class 'person nil 
    '(fields (name "Eve") (age 21 integer))
)

(def-class 'student '(person)
    '(fields ('name "Eva Lu Ator") (university "Berkeley" string))
    '(methods
        (talk (&optional (out *standard-output*)) 
            (format out "My name is ~A~%My age is ~D~%"
                (field this 'name)
                (field this 'age)
            )
        )
    )
)

(def-class 'studente_bicocca '(student)
    '(fields (university "UNIMIB" string))
    '(methods
        (talk (word)
            (format *STANDARD-OUTPUT* "PARAMS: ~A~%My age is ~D~%"
                word
                (field this 'age)
            )
        )    
    )
)

; (defparameter p (make 'person 'name "person name"))
; (defparameter sb (make 'studente_bicocca 'name "studente bicocca name"))

; (defparameter obj1 (make 'person 'name 'obj2))

; (defparameter obj2 (make 'person 'name 'obj3))

; (defparameter obj3 (make 'person 'age 99))

; (maphash (lambda (key value)
;            (format t "~A -> ~A~%" key value))
;          *classes-specs*)
