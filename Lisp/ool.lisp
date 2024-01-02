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
  (dolist (field fields)
    (let ((field-name (first field))
          (field-value (second field))
          (field-type (third field)))
      (when (and (> (length field) 2)
                 (not (or (class-spec field-type)
                          (ignore-errors (typep field-value field-type)))))
        (error "Error: field value ~A (~A) doesn't match type (~A)"
               field-name field-value field-type)))))

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
               (loop for parent in parents
                     nconc (get-parents parent)))))))

;;; Generates a sorted list of unique parent classes for a list of classes, 
;;; removing any duplicates.
(defun get-parents-list-sorted (classes-name)
  (remove-duplicates (loop for class-name in classes-name
                           nconc (get-parents class-name))
                     :from-end t))

;;; Ensures each field in a list has three elements: name, value, and a type. 
;;; Adds default values and types if none are provided.
(defun ensure-field-types (fields)
  (loop for field in fields
        collect (cond ((= (length field) 1) (append field '(NIL T)))
                      ((= (length field) 2) (append field '(T)))
                      (t field))))

;;; Validates that field types in a class are subtypes of the same fields in
;;; its parent classes, raising an error for any type mismatches.
(defun check-subtype-integrity (parents fields)
  (loop for parent in (append parents (get-parents-list-sorted parents))
    do (loop for attribute in (get-attributes parent)
      do (let ((matching-attribute 
                (find (first attribute) fields :key #'first)))
           (unless (or (not matching-attribute)
                       (subtypep (third matching-attribute)
                                 (third attribute)))
             (error "Error: type of slot ~S is a supertype of 
                     inherited type" (first attribute)))))))

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
  (loop for parent in 
        (nconc parents 
                (get-parents-list-sorted parents))
        nconc (loop for attribute in 
                    (funcall get-parts-function parent)
                    unless (find (first attribute)
                                  parts 
                                  :key #'first)
                    collect attribute)))

;;; (def-class <class-name> <parents> <part>*) => <class-name | error>
;;;
;;; Defines a new class with fields and methods, including inherited ones,
;;; and performs type checks and integrity validations.
(defun def-class (class-name parents &rest parts)
  (let* ((fields (loop for part in parts
                       when (eq (first part) 'fields)
                       nconc (rest part)))
         (methods (loop for part in parts
                        when (eq (first part) 'methods)
                        nconc (rest part)))
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
          (fields (loop for (key value) on parts by #'cddr
                      collect (list key value)))
          (missing-fields (remove-duplicates
                            (loop for class in classes-chain nconc
                                  (loop for attribute in (get-attributes class)
                                        unless (find (first attribute) fields
                                                      :key #'first)
                                        collect (list (first attribute)
                                                      (second attribute))))
                            :key #'first :test #'equal)))
        
        (loop for field in fields
              do (let ((class-field (find (first field) class-fields
                                          :key #'first)))
                    (unless class-field
                      (error "Error: Illegal attribute ~A found"
                              (first field)))
                    (unless (typep (second field) (third class-field))
                      (error "Error: value ~S for field ~S is not of type ~S."
                              (second field) (first class-field)
                              (third class-field)))))

        (loop for missing-field in missing-fields
              unless (second missing-field)
              do (error "Error: You must provide a value for NIL default
                        field ~A." missing-field))

        (loop for method in (get-methods class-name)
              do (let* ((method-name (first method))
                        (method-params (cons 'this (second method)))
                        (method-body (third method)))
                    (define-method class-name method-name
                      (eval `(lambda ,method-params ,method-body)))
                    (method-linker method-name)))
        
        (add-instance (list class-name (append fields missing-fields)
                            (get-methods class-name)))))

;;; (is-class <class-name>) => T/NIL
;;;
;;; Checks if a given class name corresponds to a defined class.
(defun is-class (class-name)
  (not (null (class-spec class-name))))

;;; (is-instance <value> [<class-name>]) => T/NIL
;;; 
;;; Determines if an object is an instance of a given class or any of its parent 
;;; classes, with an option to check against any class.
(defun is-instance (instance &optional (class-name T))
  (and (not( null (get-instance instance )))
       (or (eq class-name T)
           (not (null (member class-name
                              (get-parents (first (get-instance instance)))
                              :test #'eql))))))

;;; (field <instance> <field-name>) => <field-value | error>
;;;
;;; Retrieves the value of a specified field from an instance,
;;; raising an error if the field is not found.
(defun field (instance field-name)
  (let ((field-found (find field-name
                           (second (get-instance instance))
                           :key #'first)))
    (unless field-found
      (error "Error: unknown field."))
    (second field-found)))

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


;;
; (def-class 'person nil 
;     '(fields (name "Eve") (age 21 integer))
; )

; (def-class 'student '(person)
;     '(fields ('name "Eva Lu Ator") (university "Berkeley" string))
;     '(methods
;         (talk (&optional (out *standard-output*)) 
;             (format out "My name is ~A~%My age is ~D~%"
;                 (field this 'name)
;                 (field this 'age)
;             )
;         )
;     )
; )

; (def-class 'studente-bicocca '(student)
;     '(methods 
;         (talk ()
;             (format t "Mi chiamo ~A, e studio alla Bicocca~%"
;             (field this 'name))
;         )
;     )
;     '(fields
;         (university "UNIMIB")
;     )
; )

; (def-class 'studente_bicocca '(student)
;     '(fields (university "UNIMIB" string))
;     '(methods
;         (talk (word)
;             (format *STANDARD-OUTPUT* "PARAMS: ~A~%My age is ~D~%"
;                 word
;                 (field this 'age)
;             )
;         )    
;     )
; )

; (defparameter p (make 'person 'name "person name"))
; (defparameter sb (make 'studente_bicocca 'name "studente bicocca name"))

; (defparameter obj1 (make 'person 'age 4))

; (defparameter obj2 (make 'person 'age 5))

; (defparameter obj3 (make 'person 'age 99))

; (maphash (lambda (key value)
;            (format t "~A -> ~A~%" key value))
;          *classes-specs*)
