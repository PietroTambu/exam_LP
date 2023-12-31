(defparameter *classes-specs* (make-hash-table))
(defvar *instance-id-counter* 0)
(defvar *instance-map* (make-hash-table))
(defvar *method-map* (make-hash-table :test #'equal))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

(defun define-method (class-name method-name method-lambda)
  (setf (gethash (list class-name method-name) *method-map*) method-lambda))


(defun add-instance (instance)
  (incf *instance-id-counter*)
  (setf (gethash *instance-id-counter* *instance-map*) instance)
  *instance-id-counter*)

(defun instance-exist (id)
  (not (null (gethash id *instance-map*))))

(defun get-instance (id)
  (gethash id *instance-map*))

(defun check-fields-types (fields)
  (dolist (field fields)
    (let ((field-name (first field))
          (field-value (second field))
          (field-type (third field)))
      (when (and (> (length field) 2)
                 (not (or (gethash field-type *classes-specs*)
                          (ignore-errors (typep field-value field-type)))))
        (error "Error: field value ~A (~A) doesn't match type (~A)"
               field-name field-value field-type)))))


(defun get-attributes (class-name)
    (let ((attributes (third (class-spec class-name))))
        attributes))

(defun get-methods (class-name)
    (let ((methods (fourth (class-spec class-name))))
        methods))


(defun ensure-in-list (nested-list list)
    (let
        ((key-name (first list)))
        (if
            (find key-name nested-list :key #'first)
            nested-list
            (cons list nested-list)
        )
    )
)

(defun get-parents (class-name)
  (let ((class-specification (class-spec class-name)))
    (when class-specification
      (let ((parents (second class-specification)))
        (nconc (copy-list parents)
               (loop for parent in parents
                     nconc (get-parents parent)))))))


(defun get-parents-list-sorted (classes-name)
  (remove-duplicates (loop for class-name in classes-name
                           nconc (get-parents class-name))
                     :from-end t))

(defun ensure-field-types (fields)
  (loop for field in fields
        collect (cond ((= (length field) 1) (append field '(NIL T)))
                      ((= (length field) 2) (append field '(T)))
                      (t field))))

(defun check-subtype-integrity (parents fields)
    (loop for parent in (append parents (get-parents-list-sorted parents))
        do (loop for attribute in (get-attributes parent)
                 do (let
                        (
                            (matching-attribute (find (first attribute) fields :key #'first))
                        )
                        (unless
                            (or (not matching-attribute)
                                (subtypep (third matching-attribute) (third attribute))
                            )
                            (error
                                "Error: type of slot ~S is a supertype of inherited type"
                                (first attribute)
                            )
                        )
                    )
            )
    ))

(defun def-class (class-name parents &rest parts)
    (let*
        (
            (fields (loop for part in parts
                    when (eq (first part) 'fields)
                    nconc (rest part))
            )
            (methods (loop for part in parts
                    when (eq (first part) 'methods)
                    nconc (rest part))
            )
            (final-fields
                (ensure-field-types
                    (append fields 
                        (loop for parent in (nconc parents (get-parents-list-sorted parents))
                            nconc (loop for attribute in (get-attributes parent)
                                unless (find (first attribute) fields :key #'first)
                                collect attribute
                            )
                        )
                    )
                )
            )
            (final-methods
                (append methods 
                    (loop for parent in (nconc parents (get-parents-list-sorted parents))
                        nconc (loop for method in (get-methods parent)
                            unless (find (first method) methods :key #'first)
                            collect method
                        )
                    )
                )
            )
        )
        (check-subtype-integrity parents fields)
        (check-fields-types fields)
        (add-class-spec class-name `(,class-name ,parents ,final-fields ,final-methods))
        class-name
    ))



(defun method-dispatcher (method-name)
    (setf (fdefinition method-name)
        (lambda (&rest params)
            (let (
                    (class-name (first (get-instance (first params))))
                )
                (let (
                        (method-lambda (gethash (list class-name method-name) *method-map*))
                    )
                    (if method-lambda
                        (apply method-lambda params)
                        (error "Error: Method ~A not found in class ~A" method-name class-name))
                    )
            )
        )
    )
)

(defun make (class-name &rest parts)

    (unless (class-spec class-name)
    (error "Error: class ~A doesn't exist" class-name))

    (let*
        (
            (classes-chain
                (append (get-parents-list-sorted (list class-name)) (list class-name))
            )
            (class-fields
                (get-attributes class-name)
            )
            (fields (loop for (key value) on parts by #'cddr
                      collect (list key value)))
            (missing-fields
                (remove-duplicates (loop for class in classes-chain
                    nconc (loop for attribute in (get-attributes class)
                        unless (find (first attribute) fields :key #'first)
                        collect (list (first attribute) (second attribute))
                    )
                ) :key #'first :test #'equal)
            )
        )
        (loop for field in fields
            do (let ((class-field (find (first field) class-fields :key #'first)))
                (unless class-field
                    (error "Error: Illegal attribute ~A found" (first field)))
                    (unless (typep (second field) (third class-field))
                        (error "Error: value ~S for field ~S is not of type ~S." (second field) (first class-field) (third class-field)))))

        ; check missing fields value not null ?
        (loop for missing-field in missing-fields
            unless (second missing-field)
            do (error "Error: You must provide a value for NIL default field ~A." missing-field)
        )

        (loop for method in (get-methods class-name)
            do (let*
                    (
                        (method-name (first method))
                        (method-params (cons 'this (second method)))
                        (method-body (third method))
                    )
                    (define-method class-name method-name
                        (eval `(lambda ,method-params ,method-body))
                    )
                    (method-dispatcher method-name)
                )
        )
        
        ; ritorno una lista da salvare con deparameter.
        (add-instance (list class-name (append fields missing-fields) (get-methods class-name)))
    )
)


(defun is-class (class-name)
  (not (null (class-spec class-name))))

(defun is-instance (instance &optional (class-name T))
  (and (instance-exist instance)
       (or (eq class-name T)
           (not (null (member class-name (get-parents (first (get-instance instance))) :test #'eql))))))



(defun field (instance field-name)
  (let ((field-found (find field-name (second (get-instance instance)) :key #'first)))
    (unless field-found
      (error "Error: unknown field."))
    (second field-found)))


(defun field* (instance field-names)
  (if field-names
      (if (= (length field-names) 1)
          (field instance (first field-names))
          (field* (field instance (first field-names)) (rest field-names)))
      (error "Error: no field names provided")))


(def-class 'using-integers () '(fields (x 42 integer)))


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
