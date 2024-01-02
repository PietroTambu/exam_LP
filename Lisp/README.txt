Common Lisp project E1P

Author:
    Tamburini Pietro 894628

OOΛ in Common Lisp

documentation:

def-class:
 -  def-class is a macro used for defining classes.
    It allows the creation of new object with specific properties and methods.

    usage:
        (def-class <class-name> <parents> <part>*)

    example:
        (def-class 'person nil '(fields (name "person_name") (age 18 integer)))

        (def-class 'student '(person)
            '(fields
                ('name "student_name")
                (university "university_name" string))
            '(methods
                (talk (&optional (out *standard-output*)) 
                    (format out "My name is ~A~%My age is ~D~%"
                    (field this 'name)
                    (field this 'age)))))

make:
 -  make is a function used to instantiate a new object of a defined class,
    allowing to set initial values for its fields.
 -  fields of the class that are not specified will be set with the
    default value.
 -  A field type check will be performed during instantiation.
 -  Invalid field names will throw an error.

    usage:
        (make <class-name> [<field-name> <value>]*)

    example:
        (make 'person 'name "person name")
        (make 'student 'name "student_name" 'age 20)


is-class:
 -  is-class is a predicate function used to check if a given symbol corresponds
    to a defined class.

    usage:
        (is-class <class-name>)

    example:
        (is-class 'person)                                      # return T
        (is-class 'doesntexist)                                 # return NIL


is-instance:
 -  is-instance is a predicate function used to check if a given value is
    an instance.
 -  The predicate verifies that the instance has [<class-name>]
    as the superclass(es).

    usage:
        (is-instance <value> [<class-name>])

    example:
        (defparameter studente (make 'student 'name "student_name" 'age 20))

        (is-instance studente '(person))                        # return T
        (is-instance studente '(student person))                # return NIL


field:
    field is a function for accessing the value of a specific field
    in an instance of a class.

    usage:
        (field <instance> <field-name>)

    example:
        (defparameter studente (make 'student 'name "student_name" 'age 20))
        (field studente 'age)                                   # return 20


field*:
    field* is an extended version of the field function that supports navigating
    through linked instances.

    usage:
        (field* <instance> [<field-name>])

    example:
        (defparameter p1 (make 'person 'name 'p2))
        (defparameter p2 (make 'person 'name 'p3))
        (defparameter p3 (make 'person 'name "p3_name"))
        (field* p1 '(name name name))